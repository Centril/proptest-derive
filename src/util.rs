// Copyright 2018 Mazdak Farrokhzad
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Mostly useful utilities for syn used in the crate.

use std::borrow::Borrow;

use syn;
use syn::punctuated::Punctuated;
use syn::PathSegment;

use use_tracking;

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

/// Extract the list of fields from a `Fields` from syn.
/// We don't care about the style, we always and uniformly use {} in
/// struct literal syntax for making struct and enum variant values.
pub fn fields_to_vec(fields: syn::Fields) -> Vec<syn::Field> {
    use syn::Fields::*;
    match fields {
        Named(fields) => fields.named.into_iter().collect(),
        Unnamed(fields) => fields.unnamed.into_iter().collect(),
        Unit       => vec![]
    }
}

/// Returns true iff the given type is the literal unit type `()`.
/// This is treated the same way by `syn` as a 0-tuple.
pub fn is_unit_type<T: Borrow<syn::Type>>(ty: T) -> bool {
    ty.borrow() == &parse_quote!(())
}

/// Returns the `Self` type (in the literal syntactic sense).
pub fn self_ty() -> syn::Type {
    parse_quote!(Self)
}

//==============================================================================
// Paths:
//==============================================================================

type CommaPS = Punctuated<PathSegment, Token![::]>;

/// Returns true iff the path is simple, i.e:
/// just a :: separated list of identifiers.
fn is_path_simple(path: &syn::Path) -> bool {
    path.segments.iter().all(|ps| ps.arguments.is_empty())
}

/// Returns true iff lhs matches the rhs.
fn eq_simple_pathseg(lhs: &str, rhs: &CommaPS) -> bool {
    lhs.split("::").eq(rhs.iter().map(|ps| ps.ident.as_ref()))
}

/// Returns true iff lhs matches the given simple Path.
pub fn eq_simple_path(mut lhs: &str, rhs: &syn::Path) -> bool {
    if !is_path_simple(rhs) { return false }

    if rhs.leading_colon.is_some() {
        if !lhs.starts_with("::") { return false }
        lhs = &lhs[2..];
    }

    eq_simple_pathseg(lhs, &rhs.segments)
}

/// Returns true iff the given path matches any of given
/// paths specified as string slices.
pub fn match_pathsegs(path: &syn::Path, against: &[&str]) -> bool {
    against.iter().any(|needle| eq_simple_path(needle, path))
}

/// Returns true iff the given `PathArguments` is one that has one type
/// applied to it.
pub fn pseg_has_single_tyvar(pp: &syn::PathSegment) -> bool {
    use syn::GenericArgument::Type;
    use syn::PathArguments::AngleBracketed;
    if let AngleBracketed(ref ab) = pp.arguments {
        if let Some(&Type(_)) = match_singleton(ab.args.iter()) {
            return true;
        }
    }
    false
}

/// Returns true iff the given type is of the form `PhantomData<TY>` where
/// `TY` can be substituted for any type, including type variables.
pub fn is_phantom_data(path: &syn::Path) -> bool {
    let segs = &path.segments;
    if segs.is_empty() { return false }

    let mut path = path.clone();
    let lseg = path.segments.pop().unwrap().into_value();

    &lseg.ident == "PhantomData" &&
    pseg_has_single_tyvar(&lseg) &&
    match_pathsegs(&path, &[
        // We hedge a bet that user will never declare
        // their own type named PhantomData.
        // This may give errors, but is worth it usability-wise.
        "",
        "marker",
        "std::marker",
        "core::marker",
        "::std::marker",
        "::core::marker",
    ])
}

/// Extracts a simple non-global path of length 1.
pub fn extract_simple_path<'a>(path: &'a syn::Path) -> Option<&'a syn::Ident> {
    filter(match_singleton(&path.segments), |_| !path.global())
        .map(|f| &f.ident)
}

//==============================================================================
// General Rust utilities:
//==============================================================================

/// Returns `Some(x)` iff the iterable is singleton and otherwise None.
pub fn match_singleton<T, I>(it: I) -> Option<T>
where
    I: IntoIterator<Item = T>,
{
    let mut it = it.into_iter();
    filter(it.next(), |_| it.next().is_none())
}

/// A temporary solution until Option::filter is stabilized.
/// TODO: replace once stabilized!
fn filter<T, P: FnOnce(&T) -> bool>
    (option: Option<T>, predicate: P) -> Option<T> {
    option.and_then(|x| if predicate(&x) { Some(x) } else { None })
}