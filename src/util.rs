// Copyright 2018 Mazdak Farrokhzad
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Mostly useful utilities for syn used in the crate.

use syn;
use quote::{Tokens, ToTokens};

use use_tracking;

use std::borrow::Borrow;

//==============================================================================
// split_for_impl
//==============================================================================

/// This is a bit unfortunate that we have to duplicate this effort...
/// However, syn provides no method at the moment for playing tyvars and
/// lifetime variables on the trait, so we have to do this.
pub fn split_for_impl<'a>
    (trait_ls: &'a [syn::LifetimeDef], generics: &'a syn::Generics)
    -> (ImplGenerics<'a>, syn::TyGenerics<'a>, &'a syn::WhereClause)
{
    let (_, ty_generics, where_clause) = generics.split_for_impl();
    let impl_generics = ImplGenerics {
        generics: generics,
        trait_ls: trait_ls,
    };
    (impl_generics, ty_generics, where_clause)
}

pub struct ImplGenerics<'a> {
    trait_ls: &'a [syn::LifetimeDef],
    generics: &'a syn::Generics,
}

impl<'a> ToTokens for ImplGenerics<'a> {
    fn to_tokens(&self, tokens: &mut Tokens) {
        // Logic derived almost entirely from the syn crate.
        // Therefore some bits of this is owned (copyrighted) by David Tolnay.
        let t_lifetimes = self.trait_ls;
        let g_lifetimes = &self.generics.lifetimes;
        let g_ty_params = &self.generics.ty_params;

        let has_lifetimes = !t_lifetimes.is_empty() || !g_lifetimes.is_empty();
        let has_ty_params = !g_ty_params.is_empty();

        if has_lifetimes || has_ty_params {
            tokens.append("<");
            tokens.append_separated(t_lifetimes, ",");
            tokens.append_separated(g_lifetimes, ",");
            // Leave off the type parameter defaults
            for (i, ty_param) in self.generics
                    .ty_params
                    .iter()
                    .enumerate() {
                if i > 0 || has_lifetimes {
                    tokens.append(",");
                }
                tokens.append_all(
                    ty_param.attrs.iter().filter(|attr| is_outer_attr(attr)));
                ty_param.ident.to_tokens(tokens);
                if !ty_param.bounds.is_empty() {
                    tokens.append(":");
                    tokens.append_separated(&ty_param.bounds, "+");
                }
            }
            tokens.append(">");
        }
    }
}

//==============================================================================
// General AST manipulation and types
//==============================================================================

/// Simplified version of `DeriveInput` from syn letting us be generic over
/// the body.
pub struct DeriveInput<B> {
    pub ident: syn::Ident,
    pub attrs: Vec<syn::Attribute>,
    pub tracker: use_tracking::UseTracker,
    pub body: B
}

/// Extract the list of fields from a `VariantData` from syn.
/// We don't care about the style, we always and uniformly use {} in
/// struct literal syntax for making struct and enum variant values.
pub fn variant_data_to_fields(vd: syn::VariantData) -> Vec<syn::Field> {
    use syn::VariantData::*;
    match vd {
        Struct(fs) => fs,
        Tuple(fs)  => fs,
        Unit       => vec![]
    }
}

/// Returns true iff the given attribute is an outer one, i.e: `#[<attr>]`.
/// An inner attribute is the other possibility and has the syntax `#![<attr>]`.
/// Note that `<attr>` is a meta-variable for the contents inside.
pub fn is_outer_attr(attr: &syn::Attribute) -> bool {
    attr.style == syn::AttrStyle::Outer
}

/// Constructs a list of `syn::PathSegment` given a list of
/// identifiers as string slices. Useful for quickly making
/// a `syn::Path` out of a specific known path.
pub fn idents_to_path(idents: &[&str]) -> Vec<syn::PathSegment> {
    idents.iter().map(|&id| id.into()).collect()
}

/// Helper for constructing a path segment with possible parameters.
pub fn parametric_path_segment
    (ident: &str, param_data: syn::AngleBracketedParameterData)
    -> syn::PathSegment
{
    syn::PathSegment {
        ident: ident.into(),
        parameters: syn::PathParameters::AngleBracketed(param_data)
    }
}

/// Constructs parameter data for the given lifetime.
/// This is useful for specifying the `<'a>` in `Arbitrary<'a>`.
pub fn param_lf(lf: syn::Lifetime) -> syn::AngleBracketedParameterData {
    parameters(vec![lf], vec![], vec![])
}

/// A shorthand for struct literal syntax for `syn::AngleBracketedParameterData`.
pub fn parameters
    ( lifetimes: Vec<syn::Lifetime>
    , types: Vec<syn::Ty>
    , bindings: Vec<syn::TypeBinding>)
    -> syn::AngleBracketedParameterData {
    syn::AngleBracketedParameterData { lifetimes, types, bindings }
}

/// Returns a global (prefixed by `::`) path from the given path segments.
pub fn global_path(segments: Vec<syn::PathSegment>) -> syn::Path {
    syn::Path { global: true, segments }
}

/// Returns true iff the given path segments matches any of given
/// path segments specified as string slices.
pub fn match_pathsegs(segs: &[syn::PathSegment], against: &[&[&str]]) -> bool {
    against.iter().any(|kups|
        kups.len() == segs.len() &&
        segs.iter().zip(kups.iter())
            .all(|(x, y)| x.parameters.is_empty() && x.ident == y)
    )
}

/// Returns true iff the given type is of the form `PhantomData<TY>` where
/// `TY` can be substituted for any type, including type variables.
pub fn is_phantom_data(path: &syn::Path) -> bool {
    let segs = &path.segments;
    if segs.is_empty() { return false }
    let last = segs.len() - 1;
    let lseg = &segs[last];

    &lseg.ident == "PhantomData" &&
    pp_has_single_tyvar(&lseg.parameters) &&
    match_pathsegs(&segs[..last], &[
        // We hedge a bet that user will never declare
        // their own type named PhantomData.
        // This may give errors, but is worth it usability-wise.
        &[],
        &["marker"],
        &["std", "marker"],
        &["core", "marker"]
    ])
}

/// Extracts a simple non-global path of length 1.
pub fn extract_simple_path<'a>(path: &'a syn::Path) -> Option<&'a syn::Ident> {
    match_singleton(&path.segments).and_then(|f|
        if !path.global { Some(&f.ident) } else { None })
}

/// Returns true iff the given `PathParameters` is one that has one type
/// applied to it.
pub fn pp_has_single_tyvar(pp: &syn::PathParameters) -> bool {
    if let syn::PathParameters::AngleBracketed(ref x) = *pp {
        x.lifetimes.is_empty() && x.bindings.is_empty() && x.types.len() == 1
    } else {
        false
    }
}

/// Constructs a bound like `::std::fmt::Debug` that can be used as:
/// `T: ::std::fmt::Debug`.
pub fn make_path_bound(path: syn::Path) -> syn::TyParamBound {
    let ptr = syn::PolyTraitRef { trait_ref: path, bound_lifetimes: vec![] };
    syn::TyParamBound::Trait(ptr, syn::TraitBoundModifier::None)
}

/// Returns the `Self` type (in the literal syntactic sense).
pub fn self_ty() -> syn::Ty {
    syn::Ty::Path(None, "Self".into())
}

/// Returns true iff the given type is the literal unit type `()`.
/// This is treated the same way by `syn` as a 0-tuple.
pub fn is_unit_type<T: Borrow<syn::Ty>>(ty: T) -> bool {
    if let &syn::Ty::Tup(ref vec) = ty.borrow() {
        vec.is_empty()
    } else {
        false
    }
}

//==============================================================================
// General Rust utilities:
//==============================================================================

/// Returns `Some(x)` iff the slice is singleton and otherwise None.
pub fn match_singleton<T>(slice: &[T]) -> Option<&T> {
    let mut it = slice.into_iter();
    if let (Some(x), None) = (it.next(), it.next()) { Some(x) } else { None }
}

/// From libcore. TODO: Replace with libcore once stable.
pub fn from_ref<T>(s: &T) -> &[T] {
    unsafe {
        ::std::slice::from_raw_parts(s, 1)
    }
}