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
        fn debug_bound() -> syn::TypeParamBound {
            parse_quote!( ::std::fmt::Debug )
        }

        /// An `Arbitrary` bound on a type variable.
        fn arbitrary_bound() -> syn::TypeParamBound {
            parse_quote!( crate_proptest::arbitrary::Arbitrary )
        }

        // Add bounds and get generics for the impl.
        tracker.add_bounds(arbitrary_bound(), Some(debug_bound()));
        let generics = tracker.consume();
        let (impl_generics, ty_generics, where_clause)
          = generics.split_for_impl();

        let _top = syn::Ident::from(TOP_PARAM_NAME);

        let _const = const_ident(typ);

        // Linearise everything. We're done after this.
        quote! {
            #[allow(non_upper_case_globals)]
            const #_const: () = {
            extern crate proptest as crate_proptest;

            impl #impl_generics crate_proptest::arbitrary::Arbitrary
            for #typ #ty_generics #where_clause {
                type ValueTree =
                    <Self::Strategy as crate_proptest::strategy::Strategy>::Value;

                type Parameters = #params;

                type Strategy = #strategy;

                fn arbitrary_with(#_top: Self::Parameters) -> Self::Strategy {
                    #ctor
                }
            }

            };
        }
    }
}

fn const_ident(typ: syn::Ident) -> syn::Ident {
    syn::Ident::from(format!("IMPL_PROPTEST_ARBITRARY_FOR_{}", typ))
}

//==============================================================================
// Smart construcors, StratPair
//==============================================================================

/// A pair of `Strategy` and `Ctor`. These always come in pairs.
pub type StratPair = (Strategy, Ctor);

/// The type and constructor for `any::<Type>()`.
pub fn pair_any(ty: syn::Type) -> StratPair {
    let q = Ctor::Arbitrary(ty.clone(), None);
    (Strategy::Arbitrary(ty), q)
}

/// The type and constructor for `any_with::<Type>(parameters)`.
pub fn pair_any_with(ty: syn::Type, var: usize) -> StratPair {
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
pub fn pair_existential(ty: syn::Type, strat: syn::Expr) -> StratPair {
    (Strategy::Existential(ty), Ctor::Existential(strat))
}

/// The type and constructor for a strategy that always returns
/// the value provided in the expression `val`.
/// This is statically dispatched since no erasure is needed or used.
pub fn pair_value(ty: syn::Type, val: syn::Expr) -> StratPair {
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
pub fn pair_unit_self(path: syn::Path) -> StratPair {
    pair_value_self(parse_quote!( #path {} ))
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
pub struct Params(Vec<syn::Type>);

impl Params {
    /// Construct an `empty` list of parameters.
    /// This is equivalent to the unit type `()`.
    pub fn empty() -> Self {
        Params(Vec::new())
    }

    /// Computes and returns the number of parameter types.
    pub fn len(&self) -> usize {
        self.0.len()
    }
}

impl From<Params> for syn::Type {
    fn from(x: Params) -> Self {
        let tys = x.0;
        parse_quote!( (#(#tys),*) )
    }
}

impl Add<syn::Type> for Params {
    type Output = Params;

    fn add(mut self, rhs: syn::Type) -> Self::Output {
        self.0.push(rhs);
        self
    }
}

impl AddAssign<syn::Type> for Params {
    fn add_assign(&mut self, rhs: syn::Type) {
        self.0.push(rhs);
    }
}

impl ToTokens for Params {
    fn to_tokens(&self, tokens: &mut Tokens) {
        Tuple2(self.0.as_slice()).to_tokens(tokens)
    }
}

/// Returns for a given type `ty` the associated item `Parameters` of the
/// type's `Arbitrary` implementation.
pub fn arbitrary_param(ty: syn::Type) -> syn::Type {
    parse_quote!(<#ty as crate_proptest::arbitrary::Arbitrary>::Parameters)
}

//==============================================================================
// Strategy
//==============================================================================

/// The type of a given `Strategy`.
pub enum Strategy {
    /// Assuming the metavariable `$ty` for a given type, this models
    /// the strategy type `<$ty as Arbitrary>::Strategy`.
    Arbitrary(syn::Type),
    /// Assuming the metavariable `$ty` for a given type, this models
    /// the strategy type `BoxedStrategy<$ty>`, i.e: an existentially
    /// typed strategy.
    ///
    /// The dynamic dispatch used here is an implementation
    /// detail that may be changed. Such a change does not count as
    /// a breakage semver wise.
    Existential(syn::Type),
    /// Assuming the metavariable `$ty` for a given type, this models
    /// a non-shrinking strategy that simply always returns a value
    /// of the given type.
    Value(syn::Type),
    /// Assuming a sequence of strategies, this models a mapping from
    /// that sequence to `Self`.
    Map(Box<[Strategy]>),
    /// Assuming a sequence of relative-weighted strategies, this
    /// models a weighted choice of those strategies. The resultant
    /// strategy will in other words randomly pick one strategy with
    /// probabilities based on the specified weights.
    Union(Box<[Strategy]>),
}

macro_rules! quote_append {
    ($tokens: expr, $($quasi: tt)*) => {
        $tokens.append_all(quote!($($quasi)*))
    };
}

impl ToTokens for Strategy {
    fn to_tokens(&self, tokens: &mut Tokens) {
        // The logic of each of these are pretty straight forward
        // save for union which is described separately.
        use self::Strategy::*;
        match *self {
            Arbitrary(ref ty) => quote_append!(tokens,
                <#ty as crate_proptest::arbitrary::Arbitrary>::Strategy>
            ),
            Existential(ref ty) => quote_append!(tokens,
                crate_proptest::strategy::BoxedStrategy<#ty>
            ),
            Value(ref ty) => quote_append!(tokens,
                crate_proptest::strategy::LazyJustFn<#ty>
            ),
            Map(ref strats) => {
                let tuple = Tuple(strats.iter());
                quote_append!(tokens,
                    crate_proptest::strategy::Map<#tuple,
                        fn(crate_proptest::strategy::ValueFor<#tuple>) -> Self>
                )
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
    Arbitrary(syn::Type, Option<usize>),
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
            FromReg::Top => syn::Ident::from(TOP_PARAM_NAME).to_tokens(tokens),
            FromReg::Num(reg) => param(reg).to_tokens(tokens),
        }
    }
}

impl ToTokens for ToReg {
    fn to_tokens(&self, tokens: &mut Tokens) {
        match *self {
            ToReg::Range(to) if to == 1 => param(0).to_tokens(tokens),
            ToReg::Range(to) => Tuple((0..to).map(param)).to_tokens(tokens),
            ToReg::API => syn::Ident::from(API_PARAM_NAME).to_tokens(tokens),
        }
    }
}

impl ToTokens for Ctor {
    fn to_tokens(&self, tokens: &mut Tokens) {
        // The logic of each of these are pretty straight forward
        // save for union which is described separately.
        use self::Ctor::*;
        match *self {
            Extract(ref ctor, ref to, ref from) => quote_append!(tokens, {
                let #to = #from; #ctor
            }),
            Arbitrary(ref ty, fv) => if let Some(fv) = fv {
                let args = param(fv);
                quote_append!(tokens,
                    crate_proptest::arbitrary::any_with::<#ty>(#args)
                )
            } else {
                quote_append!(tokens, crate_proptest::arbitrary::any::<#ty>() )
            },
            Existential(ref expr) => quote_append!(tokens,
                crate_proptest::strategy::Strategy::boxed( #expr ) ),
            Value(ref expr) => quote_append!(tokens,
                crate_proptest::strategy::LazyJustFn::new(|| #expr ) ),
            Map(ref ctors, ref closure) => {
                let ctors = Tuple2(ctors.as_ref());
                quote_append!(tokens,
                    crate_proptest::strategy::Strategy::prop_map(#ctors, #closure)
                );
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
    if ctors.is_empty() { return; }

    if let Some(&(_, ref ctor)) = match_singleton(ctors) {
        // This is not a union at all - user provided an enum with one variant.
        ctor.to_tokens(tokens);
        return;
    }

    let mut chunks = ctors.chunks(UNION_CHUNK_SIZE);
    let chunk = chunks.next().unwrap();
    let head = chunk.iter().map(|&(w, ref c)| quote!( (#w, #c) ));
    let tail = Recurse(weight_sum(ctors) - weight_sum(chunk), chunks);

    quote_append!(tokens,
        crate_proptest::strategy::TupleUnion::new(( #(#head,)* #tail ))
    );

    struct Recurse<'a>(u32, ::std::slice::Chunks<'a, (u32, Ctor)>);

    impl<'a> ToTokens for Recurse<'a> {
        fn to_tokens(&self, tokens: &mut Tokens) {
            let (tweight, mut chunks) = (self.0, self.1.clone());

            if let Some(chunk) = chunks.next() {
                if let Some(&(w, ref c)) = match_singleton(chunk) {
                    // Only one element left - no need to nest.
                    quote_append!(tokens, (#w, #c) );
                } else {
                    let head = chunk.iter().map(|&(w, ref c)| quote!( (#w, #c) ));
                    let tail = Recurse(tweight - weight_sum(chunk), chunks);
                    quote_append!(tokens,
                        (#tweight, crate_proptest::strategy::TupleUnion::new((
                            #(#head,)* #tail
                        )))
                    );
                }
            }
        }
    }

    fn weight_sum(ctors: &[(u32, Ctor)]) -> u32 {
        ctors.iter().map(|&(w, _)| w).sum()
    }
}

/// Tokenizes a weighted list of `Strategy`.
/// For details, see `union_ctor_to_tokens`.
fn union_strat_to_tokens(tokens: &mut Tokens, strats: &[Strategy]) {
    if strats.is_empty() { return; }

    if let Some(strat) = match_singleton(strats) {
        // This is not a union at all - user provided an enum with one variant.
        strat.to_tokens(tokens);
        return;
    }

    let mut chunks = strats.chunks(UNION_CHUNK_SIZE);
    let chunk = chunks.next().unwrap();
    let head = chunk.iter().map(|s| quote!( (u32, #s) ));
    let tail = Recurse(chunks);

    quote_append!(tokens,
        crate_proptest::strategy::TupleUnion<( #(#head,)* #tail )>
    );

    struct Recurse<'a>(::std::slice::Chunks<'a, Strategy>);

    impl<'a> ToTokens for Recurse<'a> {
        fn to_tokens(&self, tokens: &mut Tokens) {
            let mut chunks = self.0.clone();

            if let Some(chunk) = chunks.next() {
                if let Some(ref s) = match_singleton(chunk) {
                    // Only one element left - no need to nest.
                    quote_append!(tokens, (u32, #s) );
                } else {
                    let head = chunk.iter().map(|s| quote!( (u32, #s) ));
                    let tail = Recurse(chunks);
                    quote_append!(tokens,
                        (u32, crate_proptest::strategy::TupleUnion<(
                            #(#head,)* #tail
                        )>)
                    );
                }
            }
        }
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

/// Constructs a `MapClosure` for the given `path` and a list of fields.
pub fn map_closure(path: syn::Path, fs: &Vec<syn::Field>) -> MapClosure {
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
    Tuple(syn::Path, usize),
    Named(syn::Path, Vec<syn::Ident>),
}

impl ToTokens for MapClosure {
    fn to_tokens(&self, tokens: &mut Tokens) {
        fn tmp_var<'a>(idx: usize) -> FreshVar<'a> {
            fresh_var("tmp", idx)
        }

        fn map_closure_base<T, I>
            (tokens: &mut Tokens, path: &syn::Path, count: usize, iter: I)
        where
            T: ToTokens,
            I: Iterator<Item = T>,
        {
            tokens.append_all(if count == 1 {
                let tmp = tmp_var(0);
                quote!(|#tmp|)
            } else {
                let tmps = (0..count).map(tmp_var);
                quote!(|(#(#tmps),*)|)
            });

            path.to_tokens(tokens);
            syn::token::Brace::default().surround(tokens, |tokens|
                tokens.append_terminated(iter, <Token![,]>::default())
            );
        }

        struct FieldInit<T: ToTokens>((usize, T));

        impl<T: ToTokens> ToTokens for FieldInit<T> {
            fn to_tokens(&self, tokens: &mut Tokens) {
                let (count, ref name) = self.0;
                name.to_tokens(tokens);
                <Token![:]>::default().to_tokens(tokens);
                tmp_var(count).to_tokens(tokens);
            }
        }

        match *self {
            MapClosure::Tuple(ref path, ref count) =>
                map_closure_base(tokens, path, *count,
                    (0..*count).map(|idx| FieldInit((idx,
                        syn::Member::Unnamed(syn::Index::from(idx))
                    )))),
            MapClosure::Named(ref path, ref ids) =>
                map_closure_base(tokens, path, ids.len(),
                    ids.iter().enumerate().map(FieldInit)),
        }
    }
}

/*
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
        let VariantPath { typ, var } = path;
        if let Some(variant) = var {
            parse_quote!(#typ::#variant)
        } else {
            typ.into()
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
        let p: syn::Path = self.clone().into();
        p.to_tokens(tokens);
    }
}
*/

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
        let ident = format!("{}_{}", self.prefix, self.count);
        syn::Ident::from(ident).to_tokens(tokens)
    }
}

//==============================================================================
// Util
//==============================================================================

/// A comma separated tuple to a token stream when more than 1, or just flat
/// when 1.
#[derive(Copy, Clone)]
struct Tuple2<S>(S);

impl<'a, T: ToTokens> ToTokens for Tuple2<&'a [T]> {
    fn to_tokens(&self, tokens: &mut Tokens) {
        if let Some(ref x) = match_singleton(self.0) {
            x.to_tokens(tokens);
        } else {
            Tuple(self.0).to_tokens(tokens);
        }
    }
}

/// Append a comma separated tuple to a token stream.
struct Tuple<I>(I);

impl<T: ToTokens, I: Clone + IntoIterator<Item = T>> ToTokens for Tuple<I> {
    fn to_tokens(&self, tokens: &mut Tokens) {
        syn::token::Paren::default().surround(tokens, |tokens|
            tokens.append_separated(self.0.clone(), <Token![,]>::default()))
    }
}