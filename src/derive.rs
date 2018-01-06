use syn;
use quote::Tokens;

use util::*;
use error;
use attr::*;
use use_tracking::*;
use ast::*;

// TODO: Handle tuples > 10 and arrays > 32.

//==============================================================================
// API
//==============================================================================

pub fn impl_proptest_arbitrary(ast: syn::DeriveInput) -> Tokens {
    error::if_has_lifetimes(&ast);

    let tracker = UseTracker::new(ast.generics);

    #[allow(unreachable_patterns)]
    let q = match ast.body {
        syn::Body::Struct(vd) => derive_struct(DeriveInput {
            tracker,
            ident: ast.ident,
            attrs: ast.attrs,
            body: variant_data_to_fields(vd),
        }),
        syn::Body::Enum(variants) => derive_enum(DeriveInput {
            tracker,
            ident: ast.ident,
            attrs: ast.attrs,
            body: variants
        }),
        _ => { error::not_struct_or_enum() }
    }.to_tokens();

    println!("{}\n", q.as_ref());
    q
}

//==============================================================================
// Struct
//==============================================================================

fn derive_struct(mut ast: DeriveInput<Vec<syn::Field>>) -> Impl {
    error::if_uninhabited_fields(&ast.body);
    let t_attrs = parse_attributes(ast.attrs);
    error::if_enum_attrs_present(&t_attrs, error::STRUCT);
    error::if_strategy_present(&t_attrs, error::STRUCT);

    let v_path = (&ast.ident).into();
    let parts = if ast.body.is_empty() {
        error::if_params_present_on_unit_struct(&t_attrs);
        let (strat, ctor) = pair_unit_self(v_path);
        (Params::empty(), strat, ctor)
    } else {
        let closure = map_closure(v_path, &ast.body);
        if let Some(sty) = t_attrs.params.to_option() {
            derive_struct_has_params(&mut ast.tracker, closure, ast.body, sty,
                error::STRUCT_FIELD)
        } else {
            derive_struct_no_params_base(&mut ast.tracker, ast.body)
                .finish(closure)
        }
    };
    Impl::new(ast.ident, ast.tracker, parts)
}

fn derive_struct_has_params(
    ut: &mut UseTracker, closure: MapClosure, fields: Vec<syn::Field>,
    param_ty: Option<syn::Ty>, item: &str)
    -> ImplParts
{
    add_top_params(param_ty,
        derive_struct_has_params_base(ut, item, closure, fields))
}

fn add_top_params(param_ty: Option<syn::Ty>, (strat, ctor): StratPair)
    -> ImplParts
{
    let params = Params::empty();
    if let Some(params_ty) = param_ty {
        (params + params_ty, strat, extract_api(ctor, FromReg::Top))
    } else {
        (params, strat, ctor)
    }
}

fn derive_struct_has_params_base(
    ut: &mut UseTracker, item: &str, closure: MapClosure, fields: Vec<syn::Field>)
    -> StratPair
{
    let len = fields.len();
    fields.into_iter().fold(StratAcc::new(len), |acc, field| {
        let attrs = parse_attributes(field.attrs);
        error::if_enum_attrs_present(&attrs, item);
        error::if_specified_params(&attrs, item);
        acc.add(handle_default_params(ut, field.ty, attrs.strategy))
    }).finish(closure)
}

fn handle_default_params
    (ut: &mut UseTracker, ty: syn::Ty, strategy: StratMode) -> StratPair {
    match strategy {
        StratMode::Strategy(strat) => pair_existential(ty, strat),
        StratMode::Value(value) => pair_value(ty, value),
        StratMode::Arbitrary => { ty.mark_uses(ut); pair_any(ty) },
    }
}

fn derive_struct_no_params_base(ut: &mut UseTracker, fields: Vec<syn::Field>)
    -> ImplState<Ctor>
{
    let acc = ImplState::new(fields.len());
    fields.into_iter().fold(acc, |mut acc, field| {
        let attrs = parse_attributes(field.attrs);
        error::if_enum_attrs_present(&attrs, error::STRUCT_FIELD);

        let ty = field.ty;
        let strat = match attrs.params {
            ParamsMode::Passthrough => match attrs.strategy {
                StratMode::Strategy(strat) => pair_existential(ty, strat),
                StratMode::Value(value) => pair_value(ty, value),
                StratMode::Arbitrary => {
                    ty.mark_uses(ut);
                    let pref = acc.add_param(arbitrary_param(ty.clone()));
                    pair_any_with(ty, pref)
                },
            },
            ParamsMode::Default => handle_default_params(ut, ty, attrs.strategy),
            ParamsMode::Specified(p_ty) => match attrs.strategy {
                StratMode::Strategy(strat) => (
                    self::Strategy::Existential(ty),
                    extract_api(Ctor::Existential(strat),
                        FromReg::Num(acc.add_param(p_ty)))
                ),
                StratMode::Value(_) =>
                    error::cant_set_param_and_value(error::STRUCT_FIELD),
                StratMode::Arbitrary =>
                    error::cant_set_param_but_not_strat(&ty, error::STRUCT_FIELD),
            },
        };
        acc.add_strat(strat)
    })
}

//==============================================================================
// Enum
//==============================================================================

fn derive_enum(mut ast: DeriveInput<Vec<syn::Variant>>) -> Impl {
    use void::IsUninhabited;

    if ast.body.is_empty() {
        error::uninhabited_enum_with_no_variants();
    }

    if ast.body.as_slice().is_uninhabited() {
        error::uninhabited_enum_variants_uninhabited();
    }

    let t_attrs = parse_attributes(ast.attrs);
    error::if_skip_present(&t_attrs, error::ENUM);
    error::if_weight_present(&t_attrs, error::ENUM);
    error::if_strategy_present(&t_attrs, error::ENUM);

    let parts = if let Some(sty) = t_attrs.params.to_option() {
        derive_enum_has_params(&mut ast.tracker, &ast.ident, ast.body, sty)
    } else {
        derive_enum_no_params(&mut ast.tracker, &ast.ident, ast.body)
    };
    Impl::new(ast.ident, ast.tracker, parts)
}

fn derive_enum_no_params(
    ut: &mut UseTracker, _self: &syn::Ident, variants: Vec<syn::Variant>)
    -> ImplParts
{
    let acc = ImplState::new(variants.len());
    let inhabited = variants.into_iter().filter_map(|var|
        keep_inhabited_variant(_self, var)
    );
    let acc = inhabited.fold(acc, |mut acc, (weight, ident, fields, v_attrs)| {
        let v_path = (_self, ident).into();
        let (strat, ctor) = if fields.is_empty() {
            pair_unit_variant(&v_attrs, v_path)
        } else {
            handle_variant_with_fields(ut, v_path, v_attrs, fields, &mut acc)
        };
        acc.add_strat((strat, (weight, ctor)))
    });

    if acc.strats.is_empty() {
        error::uninhabited_enum_because_of_skipped_variants();
    }

    acc.finish()
}

fn handle_variant_with_fields<C>
    (ut: &mut UseTracker, v_path: VariantPath, attrs: ParsedAttributes,
     fields: Vec<syn::Field>, acc: &mut ImplState<C>)
    -> StratPair
{
    match attrs.params {
        ParamsMode::Passthrough => match attrs.strategy {
            StratMode::Strategy(strat) => {
                deny_all_attrs_on_fields(fields);
                pair_existential_self(strat)
            },
            StratMode::Value(value) => {
                deny_all_attrs_on_fields(fields);
                pair_value_self(value)
            },
            StratMode::Arbitrary => {
                let closure = map_closure(v_path, &fields);
                let fields_acc = derive_struct_no_params_base(ut, fields);
                let (params, count) = fields_acc.params.consume();
                let (strat, ctor) = fields_acc.strats.finish(closure);

                let params_ty = params.into();
                (strat, if is_unit_type(&params_ty) { ctor } else {
                    let pref = acc.add_param(params_ty);
                    if pref + 1 == count {
                        ctor
                    } else {
                        extract_all(ctor, count, FromReg::Num(pref))
                    }
                })
            },
        },
        ParamsMode::Default => match attrs.strategy {
            StratMode::Strategy(strat) => {
                deny_all_attrs_on_fields(fields);
                pair_existential_self(strat)
            },
            StratMode::Value(value) => {
                deny_all_attrs_on_fields(fields);
                pair_value_self(value)
            },
            StratMode::Arbitrary =>
                derive_struct_has_params_base(ut, error::ENUM_VARIANT_FIELD,
                    map_closure(v_path, &fields), fields),
        },
        ParamsMode::Specified(p_ty) => match attrs.strategy {
            StratMode::Strategy(strat) => {
                deny_all_attrs_on_fields(fields);
                let (strat, ctor) = pair_existential_self(strat);
                (strat,
                 extract_api(ctor, FromReg::Num(acc.add_param(p_ty))))
            },
            StratMode::Value(_) =>
                error::cant_set_param_and_value(error::ENUM_VARIANT),
            StratMode::Arbitrary => {
                let ty = self_ty();
                error::cant_set_param_but_not_strat(&ty, error::ENUM_VARIANT)
            },
        },
    }
}

fn deny_all_attrs_on_fields(fields: Vec<syn::Field>) {
    fields.into_iter().for_each(|field| {
        let f_attr = parse_attributes(field.attrs);
        error::if_anything_specified(&f_attr, error::ENUM_VARIANT_FIELD);
    });
}

fn derive_enum_has_params(
    ut: &mut UseTracker, _self: &syn::Ident, variants: Vec<syn::Variant>,
    sty: Option<syn::Ty>)
    -> ImplParts
{
    let acc = StratAcc::new(variants.len());
    let inhabited = variants.into_iter().filter_map(|var|
        keep_inhabited_variant(_self, var)
    );
    let acc = inhabited.fold(acc, |acc, (weight, ident, fields, v_attrs)| {
        let v_path = (_self, ident).into();
        let (strat, ctor) = if fields.is_empty() {
            pair_unit_variant(&v_attrs, v_path)
        } else {
            match v_attrs.strategy {
                StratMode::Arbitrary =>
                    derive_struct_has_params_base(ut, error::ENUM_VARIANT_FIELD,
                        map_closure(v_path, &fields), fields),
                StratMode::Value(val) => pair_value_self(val),
                StratMode::Strategy(strat) => pair_existential_self(strat),
            }
        };
        acc.add((strat, (weight, ctor)))
    });

    if acc.is_empty() {
        error::uninhabited_enum_because_of_skipped_variants();
    }

    add_top_params(sty, acc.finish())
}

fn keep_inhabited_variant(_self: &syn::Ident, variant: syn::Variant)
    -> Option<(u32, syn::Ident, Vec<syn::Field>, ParsedAttributes)>
{
    use void::IsUninhabited;

    let v_attrs = parse_attributes(variant.attrs);
    let fields = variant_data_to_fields(variant.data);

    if v_attrs.skip {
        handle_skip_variant(v_attrs, fields);
        return None
    }
    if (&*fields).is_uninhabited() { return None }

    let weight = v_attrs.weight.unwrap_or(1);
    Some((weight, variant.ident, fields, v_attrs))
}

fn handle_skip_variant(v_attrs: ParsedAttributes, fields: Vec<syn::Field>) {
    // We've been ordered to skip this variant!
    // Check that all other attributes are not set.
    check_skipped_other_attrs(v_attrs, error::ENUM_VARIANT);
    fields.into_iter().for_each(|field| {
        let f_attrs = parse_attributes(field.attrs);
        error::if_skip_present(&f_attrs, error::ENUM_VARIANT_FIELD);
        check_skipped_other_attrs(f_attrs, error::ENUM_VARIANT_FIELD);
    });
}

fn check_skipped_other_attrs(attrs: ParsedAttributes, item: &str) {
    if attrs.params.is_set() {
        error::skipped_variant_has_param(item);
    }
    if attrs.strategy.is_set() {
        error::skipped_variant_has_strat(item);
    }
    if attrs.weight.is_some() {
        error::skipped_variant_has_weight(item);
    }
}

fn pair_unit_variant
    (attrs: &ParsedAttributes, v_path: VariantPath) -> StratPair {
    error::if_strategy_present_on_unit_variant(attrs);
    error::if_params_present_on_unit_variant(attrs);
    pair_unit_self(v_path)
}

//==============================================================================
// Param accumulator
//==============================================================================

struct ImplState<C> {
    params: ParamAcc,
    strats: StratAcc<C>,
}

impl<C> ImplState<C> {
    fn new(n_fields: usize) -> Self {
        Self {
            params: ParamAcc::empty(),
            strats: StratAcc::new(n_fields),
        }
    }

    fn add_strat(self, pair: (Strategy, C)) -> Self {
        Self {
            strats: self.strats.add(pair),
            params: self.params
        }
    }

    fn add_param(&mut self, ty: syn::Ty) -> usize {
        self.params.add(ty)
    }
}

impl ImplState<Ctor> {
    fn finish(self, closure: MapClosure) -> ImplParts {
        let (params, count) = self.params.consume();
        let (strat, ctor) = self.strats.finish(closure);
        (params, strat, extract_all(ctor, count, FromReg::Top))
    }
}

impl ImplState<(u32, Ctor)> {
    fn finish(self) -> ImplParts {
        let (params, count) = self.params.consume();
        let (strat, ctor) = self.strats.finish();
        (params, strat, extract_all(ctor, count, FromReg::Top))
    }
}

//==============================================================================
// Param accumulator
//==============================================================================

struct ParamAcc {
    types: Params,
    count: usize,
}

impl ParamAcc {
    fn empty() -> Self {
        Self { types: Params::empty(), count: 0, }
    }

    fn add(&mut self, ty: syn::Ty) -> usize {
        self.types += ty;
        let var = self.count;
        self.count += 1;
        var
    }

    fn consume(self) -> (Params, usize) {
        (self.types, self.count)
    }
}

//==============================================================================
// Strategy accumulator
//==============================================================================

struct StratAcc<C> {
    types: Vec<Strategy>,
    ctors: Vec<C>,
}

impl<C> StratAcc<C> {
    fn new(n_fields: usize) -> Self {
        Self {
            types: Vec::with_capacity(n_fields),
            ctors: Vec::with_capacity(n_fields),
        }
    }

    fn add(mut self, (strat, ctor): (Strategy, C)) -> Self {
        self.types.push(strat);
        self.ctors.push(ctor);
        self
    }

    fn consume(self) -> (Vec<Strategy>, Vec<C>) {
        (self.types, self.ctors)
    }

    fn is_empty(&self) -> bool {
        self.types.is_empty()
    }
}

impl StratAcc<Ctor> {
    fn finish(self, closure: MapClosure) -> StratPair {
        pair_map(self.consume(), closure)
    }
}

impl StratAcc<(u32, Ctor)> {
    fn finish(self) -> StratPair {
        pair_oneof(self.consume())
    }
}