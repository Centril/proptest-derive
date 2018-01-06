use syn;
use quote::{ToTokens, Tokens};

use util::*;
use use_tracking::*;

use std::ops::{Add, AddAssign};
use std::mem;

//==============================================================================
// AST Root
//==============================================================================

pub struct Impl {
    typ: syn::Ident,
    tracker: UseTracker,
    parts: ImplParts
}

pub type ImplParts = (Params, Strategy, Ctor);

impl Impl {
    pub fn new(typ: syn::Ident, tracker: UseTracker, parts: ImplParts) -> Self {
        Self { typ, tracker, parts }
    }

    pub fn to_tokens(self) -> Tokens {
        let Impl { typ, mut tracker, parts: (params, strategy, ctor) } = self;

        fn debug_bound() -> syn::TyParamBound {
            make_path_bound(global_path(
                idents_to_path(&["std", "fmt", "Debug"])))
        }

        fn arbitrary_bound() -> syn::TyParamBound {
            make_path_bound(global_path(
                vec!["proptest_arbitrary".into(), arbitrary_seg()]))
        }

        tracker.add_bounds(arbitrary_bound(), Some(debug_bound()));
        let generics = tracker.consume();

        let lt_arb = syn::LifetimeDef::new("'a");
        let (impl_generics, ty_generics, where_clause)
          = split_for_impl(from_ref(&lt_arb), &generics);

        quote! {
            impl #impl_generics ::proptest_arbitrary::Arbitrary<#lt_arb>
            for #typ #ty_generics #where_clause {
                type ValueTree =
                    <Self::Strategy as ::proptest::strategy::Strategy>::Value;

                type Parameters = #params;

                type Strategy = #strategy;

                fn arbitrary_with(_top: Self::Parameters) -> Self::Strategy {
                    #ctor
                }
            }
        }
    }
}

//==============================================================================
// Smart construcors, StratPair
//==============================================================================

pub type StratPair = (Strategy, Ctor);

pub fn pair_any(ty: syn::Ty) -> StratPair {
    let q = Ctor::Arbitrary(ty.clone());
    (Strategy::Arbitrary(ty), q)
}

pub fn pair_any_with(ty: syn::Ty, var: usize) -> StratPair {
    let q = Ctor::ArbitraryWith(ty.clone(), var);
    (Strategy::Arbitrary(ty), q)
}

pub fn pair_existential(ty: syn::Ty, strat: syn::Expr) -> StratPair {
    (Strategy::Existential(ty), Ctor::Existential(strat))
}

pub fn pair_value(ty: syn::Ty, val: syn::Expr) -> StratPair {
    (Strategy::Value(ty), Ctor::Value(val))
}

pub fn pair_existential_self(strat: syn::Expr) -> StratPair {
    pair_existential(self_ty(), strat)
}

pub fn pair_value_self(val: syn::Expr) -> StratPair {
    pair_value(self_ty(), val)
}

pub fn pair_unit_self(path: VariantPath) -> StratPair {
    (Strategy::Value(self_ty()), Ctor::UnitSelf(path))
}

pub fn pair_map((strats, ctors): (Vec<Strategy>, Vec<Ctor>), closure: MapClosure)
    -> StratPair
{
    (Strategy::Map(strats.into()), Ctor::Map(ctors.into(), closure))
}

pub fn pair_oneof((strats, ctors): (Vec<Strategy>, Vec<(u32, Ctor)>))
    -> StratPair
{
    (Strategy::Union(strats.into()), Ctor::Union(ctors.into()))
}

//==============================================================================
// Parameters
//==============================================================================

pub enum Params {
    Single(syn::Ty),
    Many(Vec<syn::Ty>)
}

impl Params {
    pub fn empty() -> Self {
        Params::Many(Vec::new())
    }
}

impl From<Params> for syn::Ty {
    fn from(x: Params) -> Self {
        match x {
            Params::Single(ty) => ty,
            Params::Many(tys) => syn::Ty::Tup(tys)
        }
    }
}

impl Add<syn::Ty> for Params {
    type Output = Params;

    fn add(self, rhs: syn::Ty) -> Self::Output {
        if is_unit_type(&rhs) {
            self
        } else { match self {
            Params::Single(typ) => Params::Many(vec![typ, rhs]),
            Params::Many(mut types) => if types.is_empty() {
                Params::Single(rhs)
            } else {
                types.push(rhs);
                Params::Many(types)
            }
        } }
    }
}

impl AddAssign<syn::Ty> for Params {
    fn add_assign(&mut self, rhs: syn::Ty) {
        *self = mem::replace(self, Params::empty()) + rhs;
    }
}

impl ToTokens for Params {
    fn to_tokens(&self, tokens: &mut Tokens) {
        match *self {
            Params::Single(ref typ) => typ.to_tokens(tokens),
            Params::Many(ref types) => tuple_to_tokens(tokens, types),
        }
    }
}

pub fn arbitrary_param(ty: syn::Ty) -> syn::Ty {
    fn arbitrary_item(ty: syn::Ty, item: &str) -> syn::Ty {
        let segs = vec!["proptest_arbitrary".into(),
                        arbitrary_seg(), item.into()];
        let qself = syn::QSelf { position: segs.len() - 1, ty: ty.into(), };
        syn::Ty::Path(Some(qself), global_path(segs))
    }

    arbitrary_item(ty, "Parameters")
}

fn arbitrary_seg() -> syn::PathSegment {
    parametric_path_segment("Arbitrary", param_lf(syn::Lifetime::new("'a")))
}

//==============================================================================
// Strategy
//==============================================================================

pub enum Strategy {
    Arbitrary(syn::Ty),
    Existential(syn::Ty),
    Value(syn::Ty),
    Map(Box<[Strategy]>),
    Union(Box<[Strategy]>),
}

impl ToTokens for Strategy {
    fn to_tokens(&self, tokens: &mut Tokens) {
        use self::Strategy::*;

        struct WStrategy<'a>(&'a Strategy);

        impl<'a> ToTokens for WStrategy<'a> {
            fn to_tokens(&self, tokens: &mut Tokens) {
                tokens.append("(u32, ");
                self.0.to_tokens(tokens);
                tokens.append(")");
            }
        }

        match *self {
            Arbitrary(ref ty) => {
                tokens.append("<");
                ty.to_tokens(tokens);
                tokens.append(" as ::proptest_arbitrary::Arbitrary<'a>>::Strategy");
            },
            Existential(ref ty) => {
                tokens.append("::proptest::strategy::BoxedStrategy<");
                ty.to_tokens(tokens);
                tokens.append(">");
            },
            Value(ref ty) => {
                tokens.append("::proptest_arbitrary::FnGenerator<");
                ty.to_tokens(tokens);
                tokens.append(">");
            },
            Map(ref strats) => {
                tokens.append("::proptest::strategy::Map<");
                tuple_to_tokens(tokens, strats.iter());
                tokens.append(", fn(::proptest::strategy::ValueFor<");
                tuple_to_tokens(tokens, strats.iter());
                tokens.append(">) -> Self>");
            },
            Union(ref strats) => {
                if let Some(strat) = match_singleton(&strats) {
                    strat.to_tokens(tokens);
                } else {
                    tokens.append("::proptest::strategy::TupleUnion<");
                    tuple_to_tokens(tokens, strats.iter().map(WStrategy));
                    tokens.append(">");
                }
            },
        }
    }
}

//==============================================================================
// Constructor
//==============================================================================

pub enum FromReg {
    Top,
    Num(usize),
}

pub enum ToReg {
    Range(usize),
    API,
}

pub enum Ctor {
    Extract(Box<Ctor>, ToReg, FromReg),
    Existential(syn::Expr),
    Value(syn::Expr),
    UnitSelf(VariantPath),
    Arbitrary(syn::Ty),
    ArbitraryWith(syn::Ty, usize),
    Map(Box<[Ctor]>, MapClosure),
    Union(Box<[(u32, Ctor)]>),
}

pub fn extract_all(c: Ctor, to: usize, from: FromReg) -> Ctor {
    extract(c, ToReg::Range(to), from)
}

pub fn extract_api(c: Ctor, from: FromReg) -> Ctor {
    extract(c, ToReg::API, from)
}

impl ToTokens for FromReg {
    fn to_tokens(&self, tokens: &mut Tokens) {
        match *self {
            FromReg::Top => tokens.append("_top"),
            FromReg::Num(reg) => param(reg).to_tokens(tokens),
        }
    }
}

impl ToTokens for ToReg {
    fn to_tokens(&self, tokens: &mut Tokens) {
        match *self {
            ToReg::Range(to) if to == 1 => param(0).to_tokens(tokens),
            ToReg::Range(to) => tuple_to_tokens(tokens, (0..to).map(param)),
            ToReg::API => tokens.append("params"),
        }
    }
}

impl ToTokens for Ctor {
    fn to_tokens(&self, tokens: &mut Tokens) {
        use self::Ctor::*;
        match *self {
            Extract(ref ctor, ref to, ref from) => {
                tokens.append("{ let");
                to.to_tokens(tokens);
                tokens.append("=");
                from.to_tokens(tokens);
                tokens.append(";");
                ctor.to_tokens(tokens);
                tokens.append("}");
            },
            ArbitraryWith(ref ty, fv) => {
                tokens.append("::proptest_arbitrary::any_with::<");
                ty.to_tokens(tokens);
                tokens.append(">(");
                param(fv).to_tokens(tokens);
                tokens.append(")");
            },
            Arbitrary(ref ty) => {
                tokens.append("::proptest_arbitrary::any::<");
                ty.to_tokens(tokens);
                tokens.append(">()");
            },
            Existential(ref expr) => {
                tokens.append("::proptest::strategy::Strategy::boxed(");
                expr.to_tokens(tokens);
                tokens.append(")");
            },
            Value(ref expr) => {
                tokens.append("::proptest_arbitrary::FnGenerator::new(||");
                expr.to_tokens(tokens);
                tokens.append(")");
            },
            UnitSelf(ref path) => {
                tokens.append("::proptest_arbitrary::FnGenerator::new(||");
                path.to_tokens(tokens);
                tokens.append("{})");
            },
            Map(ref ctors, ref closure) => {
                tokens.append("::proptest::strategy::Strategy::prop_map(");
                tuple_to_tokens(tokens, ctors.iter());
                tokens.append(",");
                closure.to_tokens(tokens);
                tokens.append(")");
            },
            Union(ref ctors) => {
                if let Some(&(_, ref ctor)) = match_singleton(&ctors) {
                    ctor.to_tokens(tokens);
                } else {
                    tokens.append("::proptest::strategy::TupleUnion::new(");
                    tuple_to_tokens(tokens, ctors.iter());
                    tokens.append(")");
                }
            },
        }
    }
}

fn extract(c: Ctor, to: ToReg, from: FromReg) -> Ctor {
    Ctor::Extract(Box::new(c), to, from)
}

fn param<'a>(fv: usize) -> FreshVar<'a> {
    fresh_var("param", fv)
}

//==============================================================================
// MapClosure
//==============================================================================

pub fn map_closure(path: VariantPath, fs: &Vec<syn::Field>) -> MapClosure {
    let ids = fs.iter().filter_map(|field| field.ident.as_ref())
                .cloned().collect::<Vec<_>>();

    if ids.is_empty() {
        MapClosure::Tuple(path, fs.len())
    } else {
        MapClosure::Named(path, ids)
    }
}

pub enum MapClosure {
    Tuple(VariantPath, usize),
    Named(VariantPath, Vec<syn::Ident>),
}

impl ToTokens for MapClosure {
    fn to_tokens(&self, tokens: &mut Tokens) {
        fn tmp_var<'a>(idx: usize) -> FreshVar<'a> {
            fresh_var("tmp", idx)
        }

        fn map_closure_base<T, I>
            (tokens: &mut Tokens, path: &VariantPath, count: usize, iter: I)
        where
            T: ToTokens,
            I: Iterator<Item = T>,
        {    
            tokens.append("|");
            if count == 1 {
                tmp_var(0).to_tokens(tokens);
            } else {
                tuple_to_tokens(tokens, (0..count).map(tmp_var));
            }
            tokens.append("|");

            path.to_tokens(tokens);

            tokens.append("{");
            tokens.append_terminated(iter, ",");
            tokens.append("}");
        }

        struct FieldInit<T: ToTokens>((usize, T));
        struct FieldIndex(pub usize);

        impl<T: ToTokens> ToTokens for FieldInit<T> {
            fn to_tokens(&self, tokens: &mut Tokens) {
                let (count, ref name) = self.0;
                name.to_tokens(tokens);
                tokens.append(":");
                tmp_var(count).to_tokens(tokens);
            }
        }

        impl<'a> ToTokens for FieldIndex {
            fn to_tokens(&self, tokens: &mut Tokens) {
                // TODO: Suboptimal.. optimize later.
                tokens.append(format!("{}", self.0))
            }
        }

        match *self {
            MapClosure::Tuple(ref path, ref count) =>
                map_closure_base(tokens, path, *count,
                    (0..*count).map(|idx| FieldInit((idx, FieldIndex(idx))))),
            MapClosure::Named(ref path, ref ids) =>
                map_closure_base(tokens, path, ids.len(),
                    ids.iter().enumerate().map(FieldInit)),
        }
    }
}

//==============================================================================
// VariantPath
//==============================================================================

#[derive(Clone, Debug)]
pub struct VariantPath {
    typ: syn::Ident,
    var: Option<syn::Ident>,
}

impl<'a> From<&'a syn::Ident> for VariantPath {
    fn from(typ: &'a syn::Ident) -> Self {
        VariantPath { typ: typ.clone(), var: None }
    }
}

impl<'a> From<(&'a syn::Ident, syn::Ident)> for VariantPath {
    fn from((typ, var): (&'a syn::Ident, syn::Ident)) -> Self {
        VariantPath { typ: typ.clone(), var: Some(var) }
    }
}

impl ToTokens for VariantPath {
    fn to_tokens(&self, tokens: &mut Tokens) {
        self.typ.to_tokens(tokens);
        if let Some(ref var) = self.var {
            tokens.append("::");
            var.to_tokens(tokens);
        }
    }
}

//==============================================================================
// FreshVar
//==============================================================================

struct FreshVar<'a> {
    prefix: &'a str,
    count: usize
}

fn fresh_var(prefix: &str, count: usize) -> FreshVar {
    FreshVar { prefix, count }
}

impl<'a> ToTokens for FreshVar<'a> {
    fn to_tokens(&self, tokens: &mut Tokens) {
        // TODO: Suboptimal.. optimize later.
        tokens.append(format!("{}_{}", self.prefix, self.count))
    }
}

//==============================================================================
// Util
//==============================================================================

fn match_singleton<T>(slice: &[T]) -> Option<&T> {
    let mut it = slice.into_iter();
    if let (Some(x), None) = (it.next(), it.next()) { Some(x) } else { None }
}

fn tuple_to_tokens<T, I>(tokens: &mut Tokens, tuple: I)
where
    T: ToTokens,
    I: IntoIterator<Item = T>,
{
    tokens.append("(");
    tokens.append_separated(tuple, ",");
    tokens.append(")");
}