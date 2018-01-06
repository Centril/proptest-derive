use syn;
use quote::{Tokens, ToTokens};

use use_tracking;

use std::borrow::Borrow;

//==============================================================================
// split_for_impl
//==============================================================================

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

pub struct DeriveInput<B> {
    pub ident: syn::Ident,
    pub attrs: Vec<syn::Attribute>,
    pub tracker: use_tracking::UseTracker,
    pub body: B
}

pub fn variant_data_to_fields(vd: syn::VariantData) -> Vec<syn::Field> {
    use syn::VariantData::*;
    match vd {
        Struct(fs) => fs,
        Tuple(fs)  => fs,
        Unit       => vec![]
    }
}

pub fn is_outer_attr(attr: &syn::Attribute) -> bool {
    attr.style == syn::AttrStyle::Outer
}

pub fn idents_to_path(idents: &[&str]) -> Vec<syn::PathSegment> {
    idents.iter().map(|&id| id.into()).collect()
}

pub fn parametric_path_segment
    (ident: &str, param_data: syn::AngleBracketedParameterData)
    -> syn::PathSegment
{
    syn::PathSegment {
        ident: ident.into(),
        parameters: syn::PathParameters::AngleBracketed(param_data)
    }
}

pub fn param_lf(lf: syn::Lifetime) -> syn::AngleBracketedParameterData {
    parameters(vec![lf], vec![], vec![])
}

pub fn parameters
    ( lifetimes: Vec<syn::Lifetime>
    , types: Vec<syn::Ty>
    , bindings: Vec<syn::TypeBinding>)
    -> syn::AngleBracketedParameterData {
    syn::AngleBracketedParameterData { lifetimes, types, bindings }
}

pub fn global_path(segments: Vec<syn::PathSegment>) -> syn::Path {
    syn::Path { global: true, segments }
}

pub fn match_pathsegs(segs: &[syn::PathSegment], against: &[&[&str]]) -> bool {
    against.iter().any(|kups|
        kups.len() == segs.len() &&
        segs.iter().zip(kups.iter())
            .all(|(x, y)| x.parameters.is_empty() && x.ident == y)
    )
}

pub fn is_phantom_data(qp: &Option<syn::QSelf>, path: &syn::Path) -> bool {
    let segs = &path.segments;
    if qp.is_some() || segs.is_empty() { return false }
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

pub fn extract_simple_path<'a>(qp: &Option<syn::QSelf>, path: &'a syn::Path)
    -> Option<&'a syn::Ident>
{
    match_singleton(&path.segments).and_then(|f|
        if qp.is_none() && !path.global { Some(&f.ident) } else { None })
}

pub fn pp_has_single_tyvar(pp: &syn::PathParameters) -> bool {
    if let syn::PathParameters::AngleBracketed(ref x) = *pp {
        x.lifetimes.is_empty() && x.bindings.is_empty() && x.types.len() == 1
    } else {
        false
    }
}

pub fn make_path_bound(path: syn::Path) -> syn::TyParamBound {
    let ptr = syn::PolyTraitRef { trait_ref: path, bound_lifetimes: vec![] };
    syn::TyParamBound::Trait(ptr, syn::TraitBoundModifier::None)
}

pub fn self_ty() -> syn::Ty {
    syn::Ty::Path(None, "Self".into())
}

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