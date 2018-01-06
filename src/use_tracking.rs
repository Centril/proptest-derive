use syn;

// Perhaps ordermap would be better, but our maps are so small that it doesn't
// matter either way. We need to preserve insertion order in any case.
use std::collections::BTreeMap;

use util;

//==============================================================================
// API: Type variable use tracking
//==============================================================================

#[derive(Debug)]
pub struct UseTracker {
    used_map: BTreeMap<Box<str>, bool>,
    generics: syn::Generics,
}

pub trait UseMarkable {
    fn mark_uses(&self, tracker: &mut UseTracker);
}

impl UseTracker {
    pub fn new(generics: syn::Generics) -> Self {
        let used_map = generics.ty_params.iter()
            .map(|v| (v.ident.as_ref().into(), false))
            .collect();
        Self { generics, used_map }
    }

    pub fn mark_used<S: AsRef<str>>(&mut self, ty: S) {
        self.used_map.get_mut(ty.as_ref()).map(|used| { *used = true; });
    }

    pub fn add_bounds(&mut self,
        for_used: syn::TyParamBound, for_not: Option<syn::TyParamBound>) {
        let iter = self.used_map.values().zip(self.generics.ty_params.iter_mut());
        if let Some(for_not) = for_not {
            iter.for_each(|(&used, tv)| {
                let bound = if used { &for_used } else { &for_not };
                tv.bounds.push(bound.clone());
            });
        } else {
            iter.for_each(|(&used, tv)|
                if used { tv.bounds.push(for_used.clone()) }
            )
        }
    }

    pub fn consume(self) -> syn::Generics {
        self.generics
    }
}

//==============================================================================
// Impls
//==============================================================================

impl UseMarkable for syn::Ty {
    fn mark_uses(&self, ut: &mut UseTracker) {
        use syn::Ty::*;
        match *self {
            Paren(ref ty) | Slice(ref ty) | Array(ref ty, _)
                => ty.mark_uses(ut),
            Ptr(ref ty) | Rptr(_, ref ty) => ty.mark_uses(ut),
            Tup(ref tys) => tys.mark_uses(ut),
            BareFn(ref ty) => ty.mark_uses(ut),
            Path(ref qself, ref path) => {
                if let Some(ident) = util::extract_simple_path(qself, path) {
                    ut.mark_used(ident);
                } else if !util::is_phantom_data(&qself, path) {
                    // If path is PhantomData do not mark innards.
                    qself.mark_uses(ut);
                    path.mark_uses(ut);
                }
            },
            // For the rest it never applies:
            _ => {}
        }
    }
}

fn mark_uses_iter<'a, T, I>(iter: I, ut: &mut UseTracker)
where
    T: 'a + UseMarkable,
    I: IntoIterator<Item = &'a T>
{
    iter.into_iter().for_each(|x| x.mark_uses(ut))
}

impl<T: UseMarkable> UseMarkable for Box<T> {
    fn mark_uses(&self, ut: &mut UseTracker) {
        self.as_ref().mark_uses(ut)
    }
}

impl<T: UseMarkable> UseMarkable for Option<T> {
    fn mark_uses(&self, ut: &mut UseTracker) {
        mark_uses_iter(self, ut)
    }
}

impl<T: UseMarkable> UseMarkable for Vec<T> {
    fn mark_uses(&self, ut: &mut UseTracker) {
        mark_uses_iter(self, ut)
    }
}

impl UseMarkable for syn::MutTy {
    fn mark_uses(&self, ut: &mut UseTracker) {
        self.ty.mark_uses(ut)
    }
}

impl UseMarkable for syn::BareFnTy {
    fn mark_uses(&self, ut: &mut UseTracker) {
        // TODO: consider this more.
        mark_uses_iter(self.inputs.iter().map(|bfa| &bfa.ty), ut);
        if let syn::FunctionRetTy::Ty(ref ret) = self.output {
            ret.mark_uses(ut)
        }
    }
}

impl UseMarkable for syn::ParenthesizedParameterData {
    fn mark_uses(&self, ut: &mut UseTracker) {
        // TODO: consider this more.
        self.inputs.mark_uses(ut);
        self.output.mark_uses(ut);
    }
}

impl UseMarkable for syn::AngleBracketedParameterData {
    fn mark_uses(&self, ut: &mut UseTracker) {
        self.types.mark_uses(ut)
    }
}

impl UseMarkable for syn::PathParameters {
    fn mark_uses(&self, ut: &mut UseTracker) {
        use syn::PathParameters::*;
        match *self {
            AngleBracketed(ref abpd) => abpd.mark_uses(ut),
            Parenthesized(ref ppd) => ppd.mark_uses(ut)
        }
    }
}

impl UseMarkable for syn::PathSegment {
    fn mark_uses(&self, ut: &mut UseTracker) {
        self.parameters.mark_uses(ut)
    }
}

impl UseMarkable for syn::Path {
    fn mark_uses(&self, ut: &mut UseTracker) {
        self.segments.mark_uses(ut)
    }
}

impl UseMarkable for syn::QSelf {
    fn mark_uses(&self, ut: &mut UseTracker) {
        self.ty.mark_uses(ut)
    }
}