// Copyright 2018 Mazdak Farrokhzad
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Mostly useful utilities for syn used in the crate.

use syn;

use use_tracking;

use std::borrow::Borrow;

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