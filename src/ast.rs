// Copyright 2018 Mazdak Farrokhzad
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! High level IR and abstract syntax tree (AST) of impls.
//!
//! We compile to this AST and then linearise that to Rust code.

use syn;
use quote::{ToTokens, Tokens};

use util::*;
use use_tracking::*;

use std::ops::{Add, AddAssign};
use std::mem;

//==============================================================================
// Config
//==============================================================================

/// The `MAX - 1` number of strategies that `TupleUnion` supports.
/// Increase this if the behaviour is changed in `proptest`.
/// Keeping this lower than what `proptest` supports will also work
/// but for optimality this should follow what `proptest` supports.
const UNION_CHUNK_SIZE: usize = 9;

/// The name of the top parameter variable name given in `arbitrary_with`.
/// Changing this is not a breaking change because a user is expected not
/// to rely on this (and the user shouldn't be able to..).
const TOP_PARAM_NAME: &'static str = "_top";

/// The name of the variable name used for user facing parameter types
/// specified in a `#[proptest(params = "<type>"]` attribute.
///
/// Changing the value of this constant constitutes a breaking change!
const API_PARAM_NAME: &'static str = "params";

//==============================================================================
// AST Root
//==============================================================================

/// Top level AST and everything required to implement `Arbitrary` for any
/// given type. Linearizing this AST gives you the impl wrt. Rust code.
pub struct Impl {
    /// Name of the type.
    typ: syn::Ident,
    /// Tracker for uses of Arbitrary trait for a generic type. 
    tracker: UseTracker,
    /// The three main parts, see description of `ImplParts` for details.
    parts: ImplParts,
}

/// The three main parts to deriving `Arbitrary` for a type.
/// That is: the associated items `Parameters` (`Params`),
/// `Strategy` (`Strategy`) as well as the construction of the
/// strategy itself (`Ctor`).
pub type ImplParts = (Params, Strategy, Ctor);

impl Impl {
    /// Constructs a new `Impl` from the parts as described on the type.
    pub fn new(typ: syn::Ident, tracker: UseTracker, parts: ImplParts) -> Self {
        Self { typ, tracker, parts }
    }

    /// Linearises the impl into a sequence of tokens.
    /// This produces the actual Rust code for the impl.
    pub fn to_tokens(self) -> Tokens {
        let Impl { typ, mut tracker, parts: (params, strategy, ctor) } = self;

        /// A `Debug` bound on a type variable.
        fn debug_bound() -> syn::TyParamBound {
            make_path_bound(global_path(
                idents_to_path(&["std", "fmt", "Debug"])))
        }

        /// An `Arbitrary` bound on a type variable.
        fn arbitrary_bound() -> syn::TyParamBound {
            make_path_bound(global_path(
                idents_to_path(&["proptest", "arbitrary", "Arbitrary"])))
        }

        // Add bounds and get generics for the impl.
        tracker.add_bounds(arbitrary_bound(), Some(debug_bound()));
        let generics = tracker.consume();
        let (impl_generics, ty_generics, where_clause)
          = generics.split_for_impl();

        let _top = syn::Ident::from(TOP_PARAM_NAME);

        // Linearise everything. We're done after this.
        quote! {
            impl #impl_generics ::proptest::arbitrary::Arbitrary
            for #typ #ty_generics #where_clause {
                type ValueTree =
                    <Self::Strategy as ::proptest::strategy::Strategy>::Value;

                type Parameters = #params;

                type Strategy = #strategy;

                fn arbitrary_with(#_top: Self::Parameters) -> Self::Strategy {
                    #ctor
                }
            }
        }
    }
}

//==============================================================================
// Smart construcors, StratPair
//==============================================================================

/// A pair of `Strategy` and `Ctor`. These always come in pairs.
pub type StratPair = (Strategy, Ctor);

/// The type and constructor for `any::<Type>()`.
pub fn pair_any(ty: syn::Ty) -> StratPair {
    let q = Ctor::Arbitrary(ty.clone(), None);
    (Strategy::Arbitrary(ty), q)
}

/// The type and constructor for `any_with::<Type>(parameters)`.
pub fn pair_any_with(ty: syn::Ty, var: usize) -> StratPair {
    let q = Ctor::Arbitrary(ty.clone(), Some(var));
    (Strategy::Arbitrary(ty), q)
}

/// The type and constructor for a specific strategy value
/// constructed by the given expression. Currently, the type
/// is erased and a `BoxedStrategy<Type>` is given back instead.
///
/// This is a temporary restriction. Once `impl Trait` is
/// stabilized, the boxing and dynamic dispatch can be replaced
/// with a statically dispatched anonymous type instead.
pub fn pair_existential(ty: syn::Ty, strat: syn::Expr) -> StratPair {
    (Strategy::Existential(ty), Ctor::Existential(strat))
}

/// The type and constructor for a strategy that always returns
/// the value provided in the expression `val`.
/// This is statically dispatched since no erasure is needed or used.
pub fn pair_value(ty: syn::Ty, val: syn::Expr) -> StratPair {
    (Strategy::Value(ty), Ctor::Value(val))
}

/// Same as `pair_existential` for the `Self` type.
pub fn pair_existential_self(strat: syn::Expr) -> StratPair {
    pair_existential(self_ty(), strat)
}

/// Same as `pair_value` for the `Self` type.
pub fn pair_value_self(val: syn::Expr) -> StratPair {
    pair_value(self_ty(), val)
}

/// Same as `pair_value` but for a unit variant or unit struct.
pub fn pair_unit_self(path: VariantPath) -> StratPair {
    pair_value_self(syn::Expr {
        attrs: vec![],
        node: syn::ExprKind::Struct(path.into(), vec![], None),
    })
}

/// The type and constructor for .prop_map:ing a set of strategies
/// into the type we are implementing for. The closure for the
/// `.prop_map(<closure>)` must also be given.
pub fn pair_map((strats, ctors): (Vec<Strategy>, Vec<Ctor>), closure: MapClosure)
    -> StratPair
{
    (Strategy::Map(strats.into()), Ctor::Map(ctors.into(), closure))
}

/// The type and constructor for a union of strategies which produces
/// a new strategy that used the given strategies with probabilities
/// based on the assigned relative weights for each strategy.
pub fn pair_oneof((strats, ctors): (Vec<Strategy>, Vec<(u32, Ctor)>))
    -> StratPair
{
    (Strategy::Union(strats.into()), Ctor::Union(ctors.into()))
}

//==============================================================================
// Parameters
//==============================================================================

/// Represents the associated item of `Parameters` of an `Arbitrary` impl.
pub enum Params {
    /// A parameter made up of a single type logically.
    /// This mostly happens when dealing with a user specified type or
    /// for a simple type.
    ///
    /// We represent a singleton in this way to avoid heap allocation
    /// in the case of a single type which is common.
    Single(syn::Ty),
    /// Zero or Two+ parameters.
    Many(Vec<syn::Ty>)
}

impl Params {
    /// Construct an `empty` list of parameters.
    /// This is equivalent to the unit type `()`.
    pub fn empty() -> Self {
        Params::Many(Vec::new())
    }

    /// Computes and returns the number of parameter types.
    pub fn len(&self) -> usize {
        match *self {
            Params::Single(_) => 1,
            Params::Many(ref tys) => tys.len()
        }
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
        /*
        if is_unit_type(&rhs) {
            // We've been given a unit type - for ergonomics,
            // we skip the type so as to not force the user to
            // specify a tuple of units just to please the compiler.
            self
        } else {
        */
            // Logic is straight forward here.. We just add one type.
            match self {
                Params::Single(typ) => Params::Many(vec![typ, rhs]),
                Params::Many(mut types) => if types.is_empty() {
                    Params::Single(rhs)
                } else {
                    types.push(rhs);
                    Params::Many(types)
                }
            }
        //}
    }
}

impl AddAssign<syn::Ty> for Params {
    fn add_assign(&mut self, rhs: syn::Ty) {
        // We have to do a swap first.
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

/// Returns for a given type `ty` the associated item `Parameters` of the
/// type's `Arbitrary` implementation.
pub fn arbitrary_param(ty: syn::Ty) -> syn::Ty {
    /// Returns for a given type `ty` the associated `item` of the
    /// type's `Arbitrary` implementation.
    fn arbitrary_item(ty: syn::Ty, item: &str) -> syn::Ty {
        let segs = idents_to_path(&["proptest", "arbitrary", "Arbitrary",
                        item.into()]);
        let qself = syn::QSelf { position: segs.len() - 1, ty: ty.into(), };
        syn::Ty::Path(Some(qself), global_path(segs))
    }

    arbitrary_item(ty, "Parameters")
}

//==============================================================================
// Strategy
//==============================================================================

/// The type of a given `Strategy`.
pub enum Strategy {
    /// Assuming the metavariable `$ty` for a given type, this models
    /// the strategy type `<$ty as Arbitrary>::Strategy`.
    Arbitrary(syn::Ty),
    /// Assuming the metavariable `$ty` for a given type, this models
    /// the strategy type `BoxedStrategy<$ty>`, i.e: an existentially
    /// typed strategy.
    ///
    /// The dynamic dispatch used here is an implementation
    /// detail that may be changed. Such a change does not count as
    /// a breakage semver wise.
    Existential(syn::Ty),
    /// Assuming the metavariable `$ty` for a given type, this models
    /// a non-shrinking strategy that simply always returns a value
    /// of the given type.
    Value(syn::Ty),
    /// Assuming a sequence of strategies, this models a mapping from
    /// that sequence to `Self`.
    Map(Box<[Strategy]>),
    /// Assuming a sequence of relative-weighted strategies, this
    /// models a weighted choice of those strategies. The resultant
    /// strategy will in other words randomly pick one strategy with
    /// probabilities based on the specified weights.
    Union(Box<[Strategy]>),
}

impl ToTokens for Strategy {
    fn to_tokens(&self, tokens: &mut Tokens) {
        // The logic of each of these are pretty straight forward
        // save for union which is described separately.
        use self::Strategy::*;
        match *self {
            Arbitrary(ref ty) => {
                tokens.append("<");
                ty.to_tokens(tokens);
                tokens.append(" as ::proptest::arbitrary::Arbitrary>::Strategy");
            },
            Existential(ref ty) => {
                tokens.append("::proptest::strategy::BoxedStrategy<");
                ty.to_tokens(tokens);
                tokens.append(">");
            },
            Value(ref ty) => {
                tokens.append("::proptest::strategy::LazyJustFn<");
                ty.to_tokens(tokens);
                tokens.append(">");
            },
            Map(ref strats) => {
                tokens.append("::proptest::strategy::Map<");
                tuple_to_tokens2(tokens, &strats);
                tokens.append(", fn(::proptest::strategy::ValueFor<");
                tuple_to_tokens2(tokens, &strats);
                tokens.append(">) -> Self>");
            },
            Union(ref strats) => union_strat_to_tokens(tokens, strats),
        }
    }
}

//==============================================================================
// Constructor
//==============================================================================

/// The right hand side (RHS) of a let binding of parameters.
pub enum FromReg {
    /// Denotes a move from the top parameter given in
    /// the arguments of `arbitrary_with`.
    Top,
    /// Denotes a move from a variable `params_<x>`
    /// where `<x>` is the given number.
    Num(usize),
}

/// The left hand side (LHS) of a let binding of parameters.
pub enum ToReg {
    /// Denotes a move and declaration to a sequence
    /// of variables from `params_0` to `params_x`.
    Range(usize),
    /// Denotes a move and declaration of a special variable `params`
    /// that is user facing and is ALWAYS named `params`.
    ///
    /// To change the name this linearises to is considered
    /// a breaking change wrt. semver.
    API,
}

/// Models an expression that generates a proptest `Strategy`.
pub enum Ctor {
    /// A strategy generated by using the `Arbitrary` impl
    /// for the given `Ty´. If `Some(idx)` is specified, then
    /// a parameter at `params_<idx>` is used and provided
    /// to `any_with::<Ty>(params_<idx>)`.
    Arbitrary(syn::Ty, Option<usize>),
    /// An exact strategy value given by the expression.
    Existential(syn::Expr),
    /// A strategy that always produces the given expression.
    Value(syn::Expr),
    /// A strategy that maps from a sequence of strategies
    /// into `Self`.
    Map(Box<[Ctor]>, MapClosure),
    /// A strategy that randomly selects one of the given
    /// relative-weighted strategies.
    Union(Box<[(u32, Ctor)]>),
    /// A let binding that moves to and declares the ToReg
    /// from the FromReg as well as the strategy that uses
    /// the `ToReg`.
    Extract(Box<Ctor>, ToReg, FromReg),
}

/// Wraps the given strategy producing expression with a move
/// into `params_<to>` from `FromReg`. This is used when the
/// given `c` expects `params_<to>` to be there.
pub fn extract_all(c: Ctor, to: usize, from: FromReg) -> Ctor {
    extract(c, ToReg::Range(to), from)
}

/// Wraps the given strategy producing expression with a move
/// into `params` (literally named like that) from `FromReg`.
/// This is used when the given `c` expects `params` to be there.
pub fn extract_api(c: Ctor, from: FromReg) -> Ctor {
    extract(c, ToReg::API, from)
}

impl ToTokens for FromReg {
    fn to_tokens(&self, tokens: &mut Tokens) {
        match *self {
            FromReg::Top => tokens.append(TOP_PARAM_NAME),
            FromReg::Num(reg) => param(reg).to_tokens(tokens),
        }
    }
}

impl ToTokens for ToReg {
    fn to_tokens(&self, tokens: &mut Tokens) {
        match *self {
            ToReg::Range(to) if to == 1 => param(0).to_tokens(tokens),
            ToReg::Range(to) => tuple_to_tokens(tokens, (0..to).map(param)),
            ToReg::API => tokens.append(API_PARAM_NAME),
        }
    }
}

impl ToTokens for Ctor {
    fn to_tokens(&self, tokens: &mut Tokens) {
        // The logic of each of these are pretty straight forward
        // save for union which is described separately.
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
            Arbitrary(ref ty, fv) => if let Some(fv) = fv {                    
                tokens.append("::proptest::arbitrary::any_with::<");
                ty.to_tokens(tokens);
                tokens.append(">(");
                param(fv).to_tokens(tokens);
                tokens.append(")");
            } else {
                tokens.append("::proptest::arbitrary::any::<");
                ty.to_tokens(tokens);
                tokens.append(">()");
            },
            Existential(ref expr) => {
                tokens.append("::proptest::strategy::Strategy::boxed(");
                expr.to_tokens(tokens);
                tokens.append(")");
            },
            Value(ref expr) => {
                tokens.append("::proptest::strategy::LazyJustFn::new(||");
                expr.to_tokens(tokens);
                tokens.append(")");
            },
            Map(ref ctors, ref closure) => {
                tokens.append("::proptest::strategy::Strategy::prop_map(");
                tuple_to_tokens2(tokens, &ctors);
                tokens.append(",");
                closure.to_tokens(tokens);
                tokens.append(")");
            },
            Union(ref ctors) => union_ctor_to_tokens(tokens, ctors),
        }
    }
}

/// Tokenizes a weighted list of `Ctor`.
///
/// The logic is that the output should be as linear as possible while
/// still supporting enums with an unbounded number of variants without
/// any boxing (erasure) or dynamic dispatch.
///
/// As `TupleUnion` is (currently) limited to 10 summands in the coproduct
/// we can't just emit the entire thing linearly as this will fail on the
/// 11:th variant.
///
/// A naive approach to solve might be to simply use a cons-list like so:
///
/// ```ignore
/// TupleUnion::new(
///     (w_1, s_1),
///     (w_2 + w_3 + w_4 + w_5,
///      TupleUnion::new(
///         (w_2, s_2),
///         (w_3 + w_4 + w_5,
///          TupleUnion::new(
///             (w_3, s_3),
///             (w_4 + w_5,
///              TupleUnion::new(
///                 (w_4, s_4),
///                 (w_5, s_5),
///             ))
///         ))
///     ))
/// )
/// ```
///
/// However, we can do better by being linear for the `10 - 1` first
/// strategies and then switch to nesting like so:
///
/// ```ignore
/// (1, 2, 3, 4, 5, 6, 7, 8, 9,
///     (10, 11, 12, 13, 14, 15, 16, 17, 18,
///         (19, ..)))
/// ```
fn union_ctor_to_tokens(tokens: &mut Tokens, ctors: &[(u32, Ctor)]) {
    // Runtime complexity [T(n) ~= 2n ] \in O(n)
    // Space complexity: constant (or linear if we count the space for ctors)

    if let Some(&(_, ref ctor)) = match_singleton(&ctors) {
        // This is not a union at all - user provided an enum with one variant.
        ctor.to_tokens(tokens);
        return;
    }

    // Compute total weight:
    let mut right_weight: u32 = ctors.iter().map(|&(w, _)| w).sum();

    // Keep track of how many left parens we've added:
    let mut left = 0;

    for chunk in ctors.chunks(UNION_CHUNK_SIZE) {
        if let Some(w_ctor) = match_singleton(&chunk) {
            // Only one element left - no need to nest.
            w_ctor.to_tokens(tokens);
            right_weight -= w_ctor.0;
        } else {
            if left > 0 {
                left += 1;
                tokens.append("(");
                right_weight.to_tokens(tokens);
                tokens.append(",");
            }

            // Nest and note that we added 2 left parens.
            left += 2;
            tokens.append("::proptest::strategy::TupleUnion::new( (");

            // Lay out the chunk linearly.
            for w_ctor in chunk {
                // Remove our weight from the total sum.
                right_weight -= w_ctor.0;

                // Dump one strategy:
                w_ctor.to_tokens(tokens);
                tokens.append(",");
            }
        }
    }

    // Balance out left parens.
    for _ in 0..left {
        tokens.append(")");
    }

    assert_eq!(right_weight, 0, "INTERNAL LOGIC ERROR");
}

/// Tokenizes a weighted list of `Strategy`.
/// For details, see `union_ctor_to_tokens`.
fn union_strat_to_tokens(tokens: &mut Tokens, strats: &[Strategy]) {
    if let Some(strat) = match_singleton(&strats) {
        // This is not a union at all - user provided an enum with one variant.
        strat.to_tokens(tokens);
        return;
    }

    struct WStrategy<'a>(&'a Strategy);

    impl<'a> ToTokens for WStrategy<'a> {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append("( u32, ");
            self.0.to_tokens(tokens);
            tokens.append(")");
            tokens.append(",");
        }
    }

    // Keep track of brackets:
    let mut left = 0;

    for chunk in strats.chunks(UNION_CHUNK_SIZE) {
        if let Some(strat) = match_singleton(&chunk) {
            // Only one element left - no need to nest.
            WStrategy(strat).to_tokens(tokens);
        } else {
            if left > 0 {
                left += 1;
                tokens.append("( u32, ");
            }

            // Nest and note that we added 2 left parens.
            left += 2;
            tokens.append("::proptest::strategy::TupleUnion<(");

            // Lay out the chunk linearly.
            for strat in chunk {
                // Dump one strategy:
                WStrategy(strat).to_tokens(tokens);
            }
        }
    }

    // Balance out brackets.
    for x in 0..left {
        tokens.append(if x % 3 == 1 { ">" } else { ")" });
    }
}

/// Wraps a `Ctor` that expects the `to` "register" to be filled with
/// contents of the `from` register. The correctness of this wrt. the
/// generated Rust code has to be verified externally by checking the
/// construction of the particular `Ctor`.
fn extract(c: Ctor, to: ToReg, from: FromReg) -> Ctor {
    Ctor::Extract(Box::new(c), to, from)
}

/// Construct a `FreshVar` prefixed by `param_`.
fn param<'a>(fv: usize) -> FreshVar<'a> {
    fresh_var("param", fv)
}

//==============================================================================
// MapClosure
//==============================================================================

/// Constructs a `MapClosure` for the given `VariantPath` and a list of fields.
pub fn map_closure(path: VariantPath, fs: &Vec<syn::Field>) -> MapClosure {
    let ids = fs.iter().filter_map(|field| field.ident.as_ref())
                .cloned().collect::<Vec<_>>();

    if ids.is_empty() {
        MapClosure::Tuple(path, fs.len())
    } else {
        MapClosure::Named(path, ids)
    }
}

/// A `MapClosure` models the closure part inside a `.prop_map(..)` call.
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

/// A `VariantPath` models a path to a variant or just a normal identifier.
#[derive(Clone, Debug)]
pub struct VariantPath {
    typ: syn::Ident,
    var: Option<syn::Ident>,
}

impl From<VariantPath> for syn::Path {
    fn from(path: VariantPath) -> Self {
        syn::Path {
            global: false,
            segments: if let Some(variant) = path.var {
                vec![path.typ.into(), variant.into()]
            } else {
                vec![path.typ.into()]
            },
        }
    }
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

/// A `FreshVar` is an internal implementation detail and models a temporary
/// variable on the stack.
struct FreshVar<'a> {
    prefix: &'a str,
    count: usize
}

/// Construct a `FreshVar` with the given `prefix` and the number it has in the
/// count of temporaries for that prefix.
fn fresh_var(prefix: &str, count: usize) -> FreshVar {
    FreshVar { prefix, count }
}

impl<'a> ToTokens for FreshVar<'a> {
    fn to_tokens(&self, tokens: &mut Tokens) {
        tokens.append(format!("{}_{}", self.prefix, self.count))
    }
}

//==============================================================================
// Util
//==============================================================================

/// Append a comma separated tuple to a token stream.
fn tuple_to_tokens2<T>(tokens: &mut Tokens, tuple: &[T])
where
    T: ToTokens,
{
    if tuple.len() == 1 {
        tuple[0].to_tokens(tokens);
    } else {
        tokens.append("(");
        tokens.append_separated(tuple, ",");
        tokens.append(")");
    }
}

/// Append a comma separated tuple to a token stream.
fn tuple_to_tokens<T, I>(tokens: &mut Tokens, tuple: I)
where
    T: ToTokens,
    I: IntoIterator<Item = T>,
{
    tokens.append("(");
    tokens.append_separated(tuple, ",");
    tokens.append(")");
}