use syn;
use util;

use std::iter;
use std::vec;

//==============================================================================
// Public API
//==============================================================================

#[derive(Clone, Debug)]
pub struct ParsedAttributes {
    pub skip:     PSkip,
    pub weight:   PWeight,
    pub params:   ParamsMode,
    pub strategy: PStrategy,
}

#[derive(Clone, Debug)]
pub enum ParamsMode {
    Passthrough,
    Default,
    Specified(syn::Ty),
}

impl ParamsMode {
    pub fn is_set(&self) -> bool {
        match *self {
            ParamsMode::Passthrough => false,
            _ => true,
        }
    }
}

pub fn parse_attributes(attrs: Vec<syn::Attribute>) -> ParsedAttributes {
    let (skip, weight, no_params, ty_params, strategy)
      = attrs.into_iter()
             .filter(is_proptest_attr)
             .flat_map(extract_modifiers)
             .fold(init_parse_state(), dispatch_attribute);

    let params
      = parse_params_mode(no_params, ty_params);

    ParsedAttributes { skip, weight, params, strategy, }
}

//==============================================================================
// Internals: Initialization
//==============================================================================

type PSkip     = Option<()>;
type PWeight   = Option<u32>;
type PNoParams = Option<()>;
type PTyParams = Option<syn::Ty>;
type PStrategy = Option<syn::Expr>;
type PAll      = (PSkip, PWeight, PNoParams, PTyParams, PStrategy);

fn init_parse_state() -> PAll { (None, None, None, None, None) }

//==============================================================================
// Internals: Extraction & Filtering
//==============================================================================

fn is_proptest_attr(attr: &syn::Attribute) -> bool {
    attr.name() == "proptest"
}

fn extract_modifiers(attr: syn::Attribute)
    // TODO: use impl Trait when stable:
    -> iter::Map<vec::IntoIter<syn::NestedMetaItem>,
           fn(syn::NestedMetaItem) -> syn::MetaItem>
{
    use syn::MetaItem::{Word, NameValue, List};
    use syn::NestedMetaItem::{MetaItem, Literal};

    if !util::is_outer_attr(&attr) { error::outer_attr(); }
    match attr.value {
        Word(_)          => error::bare_proptest_attr(),
        NameValue(_, _)  => error::literal_set_proptest(),
        List(_, nested)  => nested.into_iter().map(|nmi| match nmi {
            Literal(_)   => error::immediate_literals(),
            MetaItem(mi) => mi.clone(),
        }),
    }
}

//==============================================================================
// Internals: Dispatch
//==============================================================================

fn dispatch_attribute(mut acc: PAll, meta: syn::MetaItem) -> PAll {
    // TODO: revisit when we have NLL.

    // Dispatch table for attributes:
    match meta.name() {
        "skip"          => parse_skip(&mut acc.0, &meta),
        "w"             => parse_weight(&mut acc.1, &meta),
        "weight"        => parse_weight(&mut acc.1, &meta),
        "no_params"     => parse_no_params(&mut acc.2, &meta),
        "params"        => parse_params(&mut acc.3, &meta),
        "strategy"      => parse_strategy(&mut acc.4, &meta),
        "weights"       => error::did_you_mean("weight",    "weights"),
        "strat"         => error::did_you_mean("strategy",  "strat"),
        "strategies"    => error::did_you_mean("strategy",  "strategies"),
        "param"         => error::did_you_mean("params",    "param"),
        "parameters"    => error::did_you_mean("params",    "parameters"),
        "no_param"      => error::did_you_mean("no_params", "no_param"),
        "no_parameters" => error::did_you_mean("no_params", "no_parameters"),
        modifier        => error::unkown_modifier(modifier),
    }
    acc
}

fn parse_skip(set: &mut PSkip, meta: &syn::MetaItem) {
    if set.is_some() { error::set_again("skip"); }
    else if let &syn::MetaItem::Word(_) = meta { *set = Some(()); }
    else { error::skip_malformed(); }
}

fn parse_weight(set: &mut PWeight, meta: &syn::MetaItem) {
    use std::u32;
    use syn::IntTy::{I8, I32, I16, I64};
    use syn::Lit;

    let name = meta.name();

    if set.is_some() {
        error::set_again(name);
    } else if let &syn::MetaItem::NameValue(_, Lit::Int(val, ty)) = meta {
        if val <= u32::MAX as u64 && match ty {
            I8 | I16 | I32 | I64 => false,
            _ => true
        } {
            *set = Some(val as u32);  
        } else {
            error::weight_malformed(name);
        }
    } else {
        error::weight_malformed(name);
    }
}

fn parse_strategy(set: &mut PStrategy, meta: &syn::MetaItem) {
    use syn::MetaItem::NameValue;
    use syn::Lit;
    use syn::StrStyle::Cooked;

    if set.is_some() {
        error::set_again("strategy");
    } else if let &NameValue(_, Lit::Str(ref lit, Cooked)) = meta {
        if let Ok(expr) = syn::parse_expr(lit.as_ref()) {
            *set = Some(expr);
        } else {
            error::strategy_malformed_expr();
        }
    } else {
        error::strategy_malformed();
    }
}

//==============================================================================
// Internals: Parameters
//==============================================================================

fn parse_params_mode(no_params: PNoParams, ty_params: PTyParams) -> ParamsMode {
    match (no_params, ty_params) {
        (Some(_), Some(_) ) => error::overspecified_param(),
        (None,    None    ) => ParamsMode::Passthrough,
        (Some(_), None    ) => ParamsMode::Default,
        (None,    Some(ty)) => ParamsMode::Specified(ty),
    }
}

fn parse_params(set: &mut PTyParams, meta: &syn::MetaItem) {
    use syn::MetaItem::{NameValue, Word, List};
    use syn::NestedMetaItem::MetaItem;
    use syn::Lit;
    use syn::StrStyle::Cooked;

    if set.is_some() {
        error::set_again("params");
    } else if let &NameValue(_, Lit::Str(ref lit, Cooked)) = meta {
        if let Ok(ty) = syn::parse_type(lit.as_ref()) {
            *set = Some(ty);
        } else {
            error::param_malformed_type();
        }
    } else if let &List(_, ref list) = meta {
        let mut iter = list.iter().filter_map(|nmi|
            if let MetaItem(Word(ref i)) = *nmi { Some(i) } else { None });

        if let (Some(ident), None) = (iter.next(), iter.next()) {
            *set = Some(syn::Ty::Path(None, ident.clone().into()));
        } else {
            error::param_malformed();
        }
    } else {
        error::param_malformed();
    }
}

fn parse_no_params(set: &mut PNoParams, meta: &syn::MetaItem) {
    if set.is_some() { error::set_again("no_params"); }
    else if let &syn::MetaItem::Word(_) = meta { *set = Some(()); }
    else { error::no_params_malformed(); }
}

//==============================================================================
// Internals: Errors
//==============================================================================

mod error {
    pub fn overspecified_param() -> ! {
        panic!("Can not set #[proptest(no_params)] and \
                #[proptest(params(<type>))] simultaneously in \
                #[derive(Arbitrary)] from proptest_derive. \
                Please pick one of those attributes.");
    }

    pub fn outer_attr() {
        panic!("Outer attributes #![proptest(..)] are not currently \
                supported by #[derive(Arbitrary)] from proptest_derive.");
    }

    pub fn bare_proptest_attr() -> ! {
        panic!("Bare #[proptest] attribute in #[derive(Arbitrary)] \
                from proptest_derive is not allowed.");
    }

    pub fn literal_set_proptest() -> ! {
        panic!("The attribute form #[proptest = <literal>] in \
                #[derive(Arbitrary)] from proptest_derive is not allowed.");
    }

    pub fn immediate_literals() -> ! {
        panic!("Literals immediately inside #[proptest(..)] as in \
                #[proptest(<lit>, ..)] in #[derive(Arbitrary)] from \
                proptest_derive are not allowed.");
    }

    pub fn set_again(modifier: &str) -> ! {
        panic!("The attribute modifier \"{}\" inside #[proptest(..)] in \
                #[derive(Arbitrary)] from proptest_derive has already been \
                set. To fix the error, please remove at least one such \
                modifier."
              , modifier);
    }

    pub fn did_you_mean(expected: &str, found: &str) -> ! {
        panic!("Unknown attribute modifier {} inside #[proptest(..)] in \
                #[derive(Arbitrary)] from proptest_derive is not allowed. \
                Did you mean to write {} instead?"
            , found
            , expected);
    }

    pub fn unkown_modifier(modifier: &str) -> ! {
        panic!("Unknown attribute modifier {} inside #[proptest(..)] in \
                #[derive(Arbitrary)] from proptest_derive is not allowed."
            , modifier);
    }

    pub fn no_params_malformed() -> ! {
        panic!("The attribute modifier \"no_params\" inside #[proptest(..)] \
                in #[derive(Arbitrary)] from proptest_derive does not support \
                any further configuration and must be a plain modifier as in \
                #[proptest(no_params)].");
    }

    pub fn skip_malformed() -> ! {
        panic!("The attribute modifier \"skip\" inside #[proptest(..)] in \
                #[derive(Arbitrary)] from proptest_derive does not support \
                any further configuration and must be a plain modifier as in \
                #[proptest(skip)].");
    }

    pub fn weight_malformed(attr_name: &str) -> ! {
        panic!("The attribute modifier {0:?} inside #[proptest(..)] in \
                #[derive(Arbitrary)] from proptest_derive must have the \
                format #[proptest({0} = <integer>)] where <integer> is an \
                integer that fits within a u32. \
                An example: #[proptest({0} = 2)] to set a \
                relative weight of 2."
              , attr_name);
    }

    pub fn param_malformed() -> ! {
        panic!("The attribute modifier \"params\" inside #[proptest(..)] in \
                #[derive(Arbitrary)] from proptest_derive must have the \
                format #[proptest(params = \"<type>\")] where <type> is a \
                valid type in Rust. \
                An example: #[proptest(params = ComplexType<Foo>)].");
    }

    pub fn param_malformed_type() -> ! {
        panic!("The attribute modifier \"params\" inside #[proptest(..)] in \
                #[derive(Arbitrary)] from proptest_derive is not assigned a \
                valid Rust type. A valid example: \
                #[proptest(params = ComplexType<Foo>)].");
    }

    pub fn strategy_malformed() -> ! {
        panic!("The attribute modifier \"strategy\" inside #[proptest(..)] \
                in #[derive(Arbitrary)] from proptest_derive must have the \
                format #[proptest(strategy = \"<expr>\")] where <expr> is a \
                valid Rust expression.");
    }

    pub fn strategy_malformed_expr() -> ! {
        panic!("The attribute modifier \"param\" inside #[proptest(..)] \
                in #[derive(Arbitrary)] from proptest_derive is not assigned \
                a valid Rust expression.");
    }
}