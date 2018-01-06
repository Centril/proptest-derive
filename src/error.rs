use syn;
use void::IsUninhabited;
use attr;

//==============================================================================
// Item descriptions
//==============================================================================

pub const STRUCT: &'static str = "struct";
pub const STRUCT_FIELD: &'static str = "struct field";
pub const ENUM: &'static str = "enum";
pub const ENUM_VARIANT: &'static str = "enum variant";
pub const ENUM_VARIANT_FIELD: &'static str = "enum variant field";

//==============================================================================
// Checkers
//==============================================================================

pub fn if_uninhabited_fields(vd: &Vec<syn::Field>) {
    // Not a complete or exhaustive check.
    if vd.as_slice().is_uninhabited() { uninhabited_struct() }
}

pub fn if_anything_specified(attrs: &attr::ParsedAttributes, item: &str) {
    if_enum_attrs_present(attrs, item);
    if_strategy_present(attrs, item);
    if_specified_params(attrs, item);
}

pub fn if_enum_attrs_present(attrs: &attr::ParsedAttributes, item: &str) {
    if_skip_present(attrs, item);
    if_weight_present(attrs, item);
}

pub fn if_specified_params(attrs: &attr::ParsedAttributes, item: &str) {
    if attrs.params.is_set() { parent_has_param(item) }
}

pub fn if_strategy_present(attrs: &attr::ParsedAttributes, item: &str) {
    use attr::StratMode::*;
    match attrs.strategy {
        Arbitrary   => {},
        Strategy(_) => illegal_strategy("strategy", item),
        Value(_)    => illegal_strategy("value", item),
    }
}

pub fn if_strategy_present_on_unit_variant(attrs: &attr::ParsedAttributes) {
    use attr::StratMode::*;
    match attrs.strategy {
        Arbitrary   => {},
        Strategy(_) => strategy_on_unit_variant("strategy"),
        Value(_)    => strategy_on_unit_variant("value"),
    }
}

pub fn if_params_present_on_unit_variant(attrs: &attr::ParsedAttributes) {
    if attrs.params.is_set() { params_on_unit_variant() }
}

pub fn if_params_present_on_unit_struct(attrs: &attr::ParsedAttributes) {
    if attrs.params.is_set() { params_on_unit_struct() }
}

pub fn if_skip_present(attrs: &attr::ParsedAttributes, item: &str) {
    if attrs.skip { illegal_skip(item) }
}

pub fn if_weight_present(attrs: &attr::ParsedAttributes, item: &str) {
    if attrs.weight.is_some() { illegal_weight(item) }
}

/// Ensures that the type is not parametric over lifetimes.
pub fn if_has_lifetimes(ast: &syn::DeriveInput) {
    if !ast.generics.lifetimes.is_empty() { has_lifetimes() }
}

//==============================================================================
// Messages
//==============================================================================

macro_rules! error {
    ($code: ident, $msg: expr) => {
        panic!(
            concat!("[proptest_derive, ", stringify!($code),
                    "] during #[derive(Arbitrary)]: ",
                    $msg)
        );
    };
    ($code: ident, $msg: expr, $($fmt: tt)+) => {
        panic!(
            concat!("[proptest_derive, ", stringify!($code),
                    "] during #[derive(Arbitrary)]: ",
                    $msg),
            $($fmt)+
        );
    };
}

pub fn has_lifetimes() -> ! {
    error!(E0001,
        "Deriving on types that are parametric over lifetimes, such as: \
        `struct Foo<'a> { bar: &'a str }` are currently not supported since \
        proptest can not define strategies for such types.");
}

pub fn not_struct_or_enum() -> ! {
    // Overspecified atm, to catch future support in syn for unions.
    error!(E0002,
        "Deriving is only possible and defined for structs and enums. \
         It is currently not defined unions.");
}

pub fn uninhabited_struct() -> ! {    
    error!(E0003,
        "The struct you are deriving `Arbitrary` for is uninhabited since one \
         of its fields is uninhabited. An uninhabited type is by definition \
         impossible to generate.")
}

pub fn uninhabited_enum_with_no_variants() -> ! {    
    error!(E0004,
        "The enum you are deriving `Arbitrary` for is uninhabited since it has \
         no variants. An example of such an `enum` is: `enum Foo {}`. \
         An uninhabited type is by definition impossible to generate.");
}

pub fn uninhabited_enum_variants_uninhabited() -> ! {    
    error!(E0005,
        "The enum you are deriving `Arbitrary` for is uninhabited since all \
         its variants are uninhabited. \
         An uninhabited type is by definition impossible to generate.");
}

pub fn uninhabited_enum_because_of_skipped_variants() -> ! {    
    error!(E0006,
        "The enum you are deriving `Arbitrary` for is uninhabited for all \
         intents and purposes since you have `#[proptest(skip)]`ed all \
         inhabited variants. \
         An uninhabited type is by definition impossible to generate.");
}

pub fn illegal_strategy(attr: &str, item: &str) -> ! {
    error!(E0007,
        "`#[proptest({0} = \"<expr>\")]` is not allowed on {1}. \
         Only struct fields, enum variants and fields inside those can use an \
         explicit {0}."
        , attr, item);
}

pub fn illegal_skip(item: &str) -> ! {
    error!(E0008,
        "A {} can't be `#[proptest(skip)]`ed, only enum variants can be skipped."
        , item);
}

pub fn illegal_weight(item: &str) -> ! {
    error!(E0009,
        "`#[proptest(weight = <integer>)]` is not allowed on {} as it is \
         meaningless. Only enum variants can be assigned weights."
        , item);
}

pub fn parent_has_param(item: &str) -> ! {
    error!(E0010,
        "Can not set the associated type Parameters with either \
         `#[proptest(no_params)]` or `#[proptest(params(<type>)]` on {} \
         since it was set on the parent."
        , item);
}

pub fn cant_set_param_but_not_strat(self_ty: &syn::Ty, item: &str) -> ! {
    error!(E0011,
        "Can not set `#[proptest(param = <type>)]` on {0} while not \
         providing a strategy for the {0} to use it since \
         `<{1} as Arbitrary<'a>>::Strategy` may require a different \
         type than the one provided in `<type>`."
        , item, quote! { #self_ty });
}

pub fn cant_set_param_and_value(item: &str) -> ! {
    error!(E0012,
        "Can not set `#[proptest(param = <type>)]` on {0} and set a value via \
         `#[proptest(value = <expr>)]` since `move || <expr>` closures can not \
         be coerced into function pointers. This is most likely a temporary \
         restriction while `-> impl Trait` is not yet stable."
        , item);
}

pub fn outer_attr() {
    error!(E0013,
        "Outer attributes `#![proptest(..)]` are not currently supported.");
}

pub fn bare_proptest_attr() -> ! {
    error!(E0014, "Bare `#[proptest]` attributes are not allowed.");
}

pub fn literal_set_proptest() -> ! {
    error!(E0015,
        "The attribute form `#[proptest = <literal>]` is not \
         allowed.");
}

pub fn immediate_literals() -> ! {
    error!(E0016,
        "Literals immediately inside `#[proptest(..)]` as in \
         `#[proptest(<lit>, ..)]` are not allowed.");
}

pub fn set_again(modifier: &str) -> ! {
    error!(E0017,
        "The attribute modifier `{}` inside `#[proptest(..)]` has already been \
         set. To fix the error, please remove at least one such modifier."
        , modifier);
}

pub fn did_you_mean(found: &str, expected: &str) -> ! {
    error!(E0018,
        "Unknown attribute modifier `{}` inside #[proptest(..)] is not \
         allowed. Did you mean to use `{}` instead?"
        , found, expected);
}

pub fn unkown_modifier(modifier: &str) -> ! {
    error!(E0018,
        "Unknown attribute modifier `{}` inside `#[proptest(..)]` is not \
         allowed."
        , modifier);
}

pub fn no_params_malformed() -> ! {
    error!(E0019,
        "The attribute modifier `no_params` inside `#[proptest(..)]` does not \
         support any further configuration and must be a plain modifier as in \
         `#[proptest(no_params)]`.");
}

pub fn skip_malformed() -> ! {
    error!(E0020,
        "The attribute modifier `skip` inside `#[proptest(..)]` does not \
         support any further configuration and must be a plain modifier as in \
         `#[proptest(skip)]`.");
}

pub fn weight_malformed(attr_name: &str) -> ! {
    error!(E0021,
        "The attribute modifier `{0}` inside `#[proptest(..)]` must have the \
         format `#[proptest({0} = <integer>)]` where `<integer>` is an integer \
         that fits within a `u32`. An example: `#[proptest({0} = 2)]` to set \
         a relative weight of 2."
        , attr_name);
}

pub fn overspecified_param() -> ! {
    error!(E0022,
        "Can not set `#[proptest(no_params)]` as well as \
         `#[proptest(params(<type>))]` simultaneously. \
         Please pick one of those attributes.");
}

pub fn param_malformed() -> ! {
    error!(E0023,
        "The attribute modifier `params` inside #[proptest(..)] must have the \
         format `#[proptest(params = \"<type>\")]` where `<type>` is a valid \
         type in Rust. An example: `#[proptest(params = ComplexType<Foo>)]`.");
}

pub fn param_malformed_type() -> ! {
    error!(E0024,
        "The attribute modifier `params` inside `#[proptest(..)]` is not \
         assigned a valid Rust type. A valid example: \
         `#[proptest(params = ComplexType<Foo>)]`.");
}

pub fn overspecified_strat() -> ! {
    error!(E0025,
        "Can not set `#[proptest(value = \"<expr>\")]` as well as \
        `#[proptest(params(strategy = \"<expr>\"))]` simultaneously. \
         Please pick one of those attributes.");
}

pub fn strategy_malformed(item: &str) -> ! {
    error!(E0026,
        "The attribute modifier `{0}` inside `#[proptest(..)]` must have the \
         format `#[proptest({0} = \"<expr>\")]` where `<expr>` is a \
         valid Rust expression."
        , item);
}

pub fn strategy_malformed_expr(item: &str) -> ! {
    error!(E0027,
        "The attribute modifier `{}` inside `#[proptest(..)]` is not assigned \
         a valid Rust expression."
        , item);
}

pub fn skipped_variant_has_weight(item: &str) -> ! {
    error!(E0028,
        "A variant has been skipped. Setting `#[proptest(weight = <value>)]` \
         on the {} is meaningless and is not allowed."
        , item);
}

pub fn skipped_variant_has_param(item: &str) -> ! {
    error!(E0028,
        "A variant has been skipped. Setting `#[proptest(no_param)]` or \
         `#[proptest(param(<type>))]` on the {} is meaningless and \
         is not allowed."
        , item);
}

pub fn skipped_variant_has_strat(item: &str) -> ! {
    error!(E0028,
        "A variant has been skipped. Setting `#[proptest(value = \"<expr>\")]` \
         or `#[proptest(strategy = \"<expr>\")]` on the {} is meaningless and \
         is not allowed."
        , item);
}

pub fn strategy_on_unit_variant(what: &str) -> ! {
    error!(E0029,
        "Setting `#[proptest({0} = \"<expr>\")]` on a unit variant has \
         no effect and is redundant because there is nothing to configure."
        , what);
}

pub fn params_on_unit_variant() -> ! {
    error!(E0029,
        "Setting `#[proptest(params = \"<type>\")]` on a unit variant has \
         no effect and is redundant because there is nothing to configure.");
}

pub fn params_on_unit_struct() -> ! {
    error!(E0030,
        "Setting `#[proptest(params = \"<type>\")]` on a unit struct has \
         no effect and is redundant because there is nothing to configure.");
}