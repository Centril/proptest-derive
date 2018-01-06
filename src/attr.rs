use syn;
use util;
use error;

use std::iter;
use std::vec;

//==============================================================================
// Public API
//==============================================================================

#[derive(Clone, Debug)]
pub struct ParsedAttributes {
    pub skip:     bool,
    pub weight:   PWeight,
    pub params:   ParamsMode,
    pub strategy: StratMode,
}

#[derive(Clone, Debug)]
pub enum StratMode {
    Arbitrary,
    Value(syn::Expr),
    Strategy(syn::Expr),
}

#[derive(Clone, Debug)]
pub enum ParamsMode {
    Passthrough,
    Default,
    Specified(syn::Ty),
}

impl ParamsMode {
    pub fn is_set(&self) -> bool {
        if let ParamsMode::Passthrough = *self { false } else { true }
    }

    pub fn to_option(self) -> Option<Option<syn::Ty>> {
        use self::ParamsMode::*;
        match self {
            Passthrough => None,
            Specified(ty) => Some(Some(ty)),
            Default => Some(None),
        }
    }
}

impl StratMode {
    pub fn is_set(&self) -> bool {
        if let StratMode::Arbitrary = *self { false } else { true }
    }
}

pub fn parse_attributes(attrs: Vec<syn::Attribute>) -> ParsedAttributes {
    let (skip, weight, no_params, ty_params, strategy, value)
      = attrs.into_iter()
             .filter(is_proptest_attr)
             .flat_map(extract_modifiers)
             .fold(init_parse_state(), dispatch_attribute);

    let params = parse_params_mode(no_params, ty_params);
    let strategy = parse_strat_mode(strategy, value);
    let skip = skip.is_some();

    ParsedAttributes { skip, weight, params, strategy }
}

//==============================================================================
// Internals: Initialization
//==============================================================================

type PSkip     = Option<()>;
type PWeight   = Option<u32>;
type PNoParams = Option<()>;
type PTyParams = Option<syn::Ty>;
type PStrategy = Option<syn::Expr>;
type PAll      = (PSkip, PWeight,
                  PNoParams, PTyParams,
                  PStrategy, PStrategy);

fn init_parse_state() -> PAll { (None, None, None, None, None, None) }

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

    error_if_inner_attr(&attr);

    match attr.value {
        Word(_)          => error::bare_proptest_attr(),
        NameValue(_, _)  => error::literal_set_proptest(),
        List(_, nested)  => nested.into_iter().map(|nmi| match nmi {
            Literal(_)   => error::immediate_literals(),
            MetaItem(mi) => mi.clone(),
        }),
    }
}

fn error_if_inner_attr(attr: &syn::Attribute) {
    if !util::is_outer_attr(&attr) { error::outer_attr(); }
}

//==============================================================================
// Internals: Dispatch
//==============================================================================

fn dispatch_attribute(mut acc: PAll, meta: syn::MetaItem) -> PAll {
    // TODO: revisit when we have NLL.

    // Dispatch table for attributes:
    let parser = {
        let name = meta.name();
        match name {
            "skip"          => parse_skip,
            "w" | "weight"  => parse_weight,
            "no_params"     => parse_no_params,
            "params"        => parse_params,
            "strategy"      => parse_strategy,
            "value"         => parse_value,
            "weights" |
            "weighted"      => error::did_you_mean(name, "weight"),
            "strat" |
            "strategies"    => error::did_you_mean(name, "strategy"),
            "values" |
            "valued" |
            "fix" |
            "fixed"         => error::did_you_mean(name, "value"),
            "param" |
            "parameters"    => error::did_you_mean(name, "params"),
            "no_param" |
            "no_parameters" => error::did_you_mean(name, "no_params"),
            modifier        => error::unkown_modifier(modifier),
        }
    };
    parser(&mut acc, meta);

    acc
}

//==============================================================================
// Internals: Skip
//==============================================================================

fn parse_skip(set: &mut PAll, meta: syn::MetaItem) {
    error_if_set(&set.0, "skip");
    if let syn::MetaItem::Word(_) = meta { set.0 = Some(()); }
    else { error::skip_malformed(); }
}

//==============================================================================
// Internals: Weight
//==============================================================================

fn parse_weight(set: &mut PAll, meta: syn::MetaItem) {
    use std::u32;
    use syn::Lit;

    let name = meta.name();

    error_if_set(&set.1, name);

    if let syn::MetaItem::NameValue(_, Lit::Int(val, ty)) = meta {
        if val <= u32::MAX as u64 && is_int_ty_unsigned(ty) {
            set.1 = Some(val as u32);  
        } else {
            error::weight_malformed(name);
        }
    } else {
        error::weight_malformed(name);
    }
}

fn is_int_ty_unsigned(ty: syn::IntTy) -> bool {
    use syn::IntTy::{I8, I32, I16, I64};
    match ty {
        I8 | I16 | I32 | I64 => false,
        _ => true
    }
}

//==============================================================================
// Internals: Strategy
//==============================================================================

fn parse_value(set: &mut PAll, meta: syn::MetaItem) {
    parse_strategy_base(&mut set.5, meta);
}

fn parse_strategy(set: &mut PAll, meta: syn::MetaItem) {
    parse_strategy_base(&mut set.4, meta);
}

fn parse_strategy_base(set: &mut PStrategy, meta: syn::MetaItem) {
    use syn::MetaItem::NameValue;
    use syn::Lit;
    use syn::StrStyle::Cooked;

    let name = meta.name();
    error_if_set(&set, name);

    if let NameValue(_, Lit::Str(ref lit, Cooked)) = meta {
        if let Ok(expr) = syn::parse_expr(lit.as_ref()) {
            *set = Some(expr);
        } else {
            error::strategy_malformed_expr(name);
        }
    } else {
        error::strategy_malformed(name);
    }
}

fn parse_strat_mode(strat: PStrategy, value: PStrategy) -> StratMode {
    match (strat, value) {
        (None,     None    ) => StratMode::Arbitrary,
        (None,     Some(ty)) => StratMode::Value(ty),
        (Some(ty), None    ) => StratMode::Strategy(ty),
        (Some(_), Some(_) )  => error::overspecified_strat(),
    }
}

//==============================================================================
// Internals: Parameters
//==============================================================================

fn parse_params_mode(no_params: PNoParams, ty_params: PTyParams) -> ParamsMode {
    match (no_params, ty_params) {
        (None,    None    ) => ParamsMode::Passthrough,
        (None,    Some(ty)) => ParamsMode::Specified(ty),
        (Some(_), None    ) => ParamsMode::Default,
        (Some(_), Some(_) ) => error::overspecified_param(),
    }
}

fn parse_params(set: &mut PAll, meta: syn::MetaItem) {
    use syn::MetaItem::{NameValue, Word, List};
    use syn::NestedMetaItem::MetaItem;
    use syn::Lit;
    use syn::StrStyle::Cooked;

    let set = &mut set.3;

    error_if_set(&set, "params");

    if let NameValue(_, Lit::Str(lit, Cooked)) = meta {
        if let Ok(ty) = syn::parse_type(lit.as_ref()) {
            *set = Some(ty);
        } else {
            error::param_malformed_type();
        }
    } else if let List(_, list) = meta {
        let mut iter = list.into_iter().filter_map(|nmi|
            if let MetaItem(Word(i)) = nmi { Some(i) } else { None });

        if let (Some(ident), None) = (iter.next(), iter.next()) {
            *set = Some(syn::Ty::Path(None, ident.into()));
        } else {
            error::param_malformed();
        }
    } else {
        error::param_malformed();
    }
}

fn parse_no_params(set: &mut PAll, meta: syn::MetaItem) {
    error_if_set(&set.2, "no_params");
    if let syn::MetaItem::Word(_) = meta { set.2 = Some(()); }
    else { error::no_params_malformed(); }
}

//==============================================================================
// Internals: Utilities
//==============================================================================

fn error_if_set<T>(set: &Option<T>, attr: &str) {
    if set.is_some() { error::set_again(attr); }
}