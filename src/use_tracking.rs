//! Provides `UseTracker` as well as `UseMarkable` which is used to
//! track uses of type variables that need `Arbitrary` bounds in our
//! impls.

use syn;

use util;

// Perhaps ordermap would be better, but our maps are so small that we care
// much more about the increased compile times incured by including ordermap.
// We need to preserve insertion order in any case, so HashMap is not useful.
use std::collections::BTreeMap;

//==============================================================================
// API: Type variable use tracking
//==============================================================================

/// `UseTracker` tracks what type variables that have used in
/// `any_with::<Type>` or similar and thus needs an `Arbitrary<'a>`
/// bound added to them.
#[derive(Debug)]
pub struct UseTracker {
    /// Tracks 'usage' of a type variable name.
    /// Allocation of this map will happen at once and no further
    /// allocation will happen after that. Only potential updates
    /// will happen after initial allocation.
    used_map: BTreeMap<Box<str>, bool>,
    /// The generics that we are doing this for.
    /// This what we will modify later once we're done.
    generics: syn::Generics,
}

/// Models a thing that may have type variables in it that
/// can be marked as 'used' as defined by `UseTracker`.
pub trait UseMarkable {
    fn mark_uses(&self, tracker: &mut UseTracker);
}

impl UseTracker {
    /// Constructs the tracker for the given `generics`.
    pub fn new(generics: syn::Generics) -> Self {
        // Construct the map by setting all type variables as being unused
        // initially. This is the only time we will allocate for the map.
        let used_map = generics.ty_params.iter()
            .map(|v| (v.ident.as_ref().into(), false))
            .collect();
        Self { generics, used_map }
    }

    /// Mark the _potential_ type variable `ty_var` as used.
    /// If the tracker does not know about the name, it is not
    /// a type variable and this call has no effect.
    pub fn mark_used<S: AsRef<str>>(&mut self, ty_var: S) {
        self.used_map.get_mut(ty_var.as_ref()).map(|used| { *used = true; });
    }

    /// Adds the bound in `for_used` on used type variables and
    /// the bound in `for_not` (`if .is_some()`) on unused type variables.
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

    /// Consumes the (potentially) modified generics that the
    /// tracker was originally constructed with and returns it.
    pub fn consume(self) -> syn::Generics {
        self.generics
    }
}

//==============================================================================
// Impls
//==============================================================================

// It would be really nice to use SYB programming here, but these are not our
// types, wherefore using scrapmetal would result in orphan impls.

impl UseMarkable for syn::Ty {
    fn mark_uses(&self, ut: &mut UseTracker) {
        use syn::Ty::*;
        match *self {
            // For these variants, we simply delegate down:
            Paren(ref ty) | Slice(ref ty) | Array(ref ty, _)
                => ty.mark_uses(ut),
            Ptr(ref ty) | Rptr(_, ref ty) => ty.mark_uses(ut),
            Tup(ref tys) => tys.mark_uses(ut),
            BareFn(ref ty) => ty.mark_uses(ut),
            // Interesting parts:
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

/// Potentially marks an iterator of `UseMarkable` things in the given tracker.
fn mark_uses_iter<'a, T, I>(iter: I, ut: &mut UseTracker)
where
    T: 'a + UseMarkable,
    I: IntoIterator<Item = &'a T>
{
    iter.into_iter().for_each(|x| x.mark_uses(ut))
}

// Super boring delegation:

impl<T: UseMarkable> UseMarkable for Box<T> {
    fn mark_uses(&self, ut: &mut UseTracker) { self.as_ref().mark_uses(ut) }
}

impl<T: UseMarkable> UseMarkable for Option<T> {
    fn mark_uses(&self, ut: &mut UseTracker) { mark_uses_iter(self, ut) }
}

impl<T: UseMarkable> UseMarkable for Vec<T> {
    fn mark_uses(&self, ut: &mut UseTracker) { mark_uses_iter(self, ut) }
}

impl UseMarkable for syn::MutTy {
    fn mark_uses(&self, ut: &mut UseTracker) { self.ty.mark_uses(ut) }
}

impl UseMarkable for syn::AngleBracketedParameterData {
    fn mark_uses(&self, ut: &mut UseTracker) { self.types.mark_uses(ut) }
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
    fn mark_uses(&self, ut: &mut UseTracker) { self.parameters.mark_uses(ut) }
}

impl UseMarkable for syn::Path {
    fn mark_uses(&self, ut: &mut UseTracker) { self.segments.mark_uses(ut) }
}

impl UseMarkable for syn::QSelf {
    fn mark_uses(&self, ut: &mut UseTracker) { self.ty.mark_uses(ut) }
}

// TODO: consider these two impls more:

impl UseMarkable for syn::BareFnTy {
    fn mark_uses(&self, ut: &mut UseTracker) {
        mark_uses_iter(self.inputs.iter().map(|bfa| &bfa.ty), ut);
        if let syn::FunctionRetTy::Ty(ref ret) = self.output {
            ret.mark_uses(ut)
        }
    }
}

impl UseMarkable for syn::ParenthesizedParameterData {
    fn mark_uses(&self, ut: &mut UseTracker) {
        self.inputs.mark_uses(ut);
        self.output.mark_uses(ut);
    }
}
