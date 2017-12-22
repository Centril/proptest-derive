use syn;
use quote;

//==============================================================================
// FreshVar
//==============================================================================

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FreshVar<'a> {
    prefix: &'a str,
    count: usize
}

pub fn fresh_var(prefix: &str, count: usize) -> FreshVar {
    FreshVar { prefix, count }
}

impl<'a> From<FreshVar<'a>> for syn::Ident {
    fn from(fv: FreshVar<'a>) -> Self {
        // TODO: Suboptimal.. optimize later.
        format!("{}_{}", fv.prefix, fv.count).into()
    }
}

impl<'a> quote::ToTokens for FreshVar<'a> {
    fn to_tokens(&self, tokens: &mut quote::Tokens) {
        syn::Ident::from(*self).to_tokens(tokens)
    }
}

//==============================================================================
// split_for_impl
//==============================================================================

/// From libcore. TODO: Replace with libcore once stable.
pub fn from_ref<T>(s: &T) -> &[T] {
    unsafe {
        ::std::slice::from_raw_parts(s, 1)
    }
}

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

impl<'a> quote::ToTokens for ImplGenerics<'a> {
    fn to_tokens(&self, tokens: &mut quote::Tokens) {
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

pub enum StructStyle { Unit, Named, Tuple, }

pub struct DeriveInput<B> {
    pub ident: syn::Ident,
    pub attrs: Vec<syn::Attribute>,
    pub generics: syn::Generics,
    pub body: B
}

pub fn variant_data_to_fields(vd: syn::VariantData)
    -> (StructStyle, Vec<syn::Field>)
{
    use syn::VariantData::*;
    match vd {
        Struct(fs) => (StructStyle::Named, fs),
        Tuple(fs)  => (StructStyle::Tuple, fs),
        Unit       => (StructStyle::Unit , Vec::new())
    }
}

pub fn is_outer_attr(attr: &syn::Attribute) -> bool {
    attr.style == syn::AttrStyle::Outer
}

pub fn plain_lifetime_def(lifetime: syn::Lifetime) -> syn::LifetimeDef {
    syn::LifetimeDef { attrs: Vec::new(), bounds: Vec::new(), lifetime }
}