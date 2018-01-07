//! Provides the `IsUninhabited` trait. See the trait for more information.

use syn;
use util;

/// A trait for types for which it is possible to check if the modelled
/// object is uninhabited or not. A `false` answer means that we can not
/// tell for sure that the thing is uninhabited, not that we are 100%
/// certain that it is inhabited.
pub trait IsUninhabited {
    /// Returns true if the given type is known to be uninhabited.
    /// There may be more scenarios under which the type is uninhabited.
    /// Thus, this is not a complete and exhaustive check.
    fn is_uninhabited(&self) -> bool;
}

impl<T: IsUninhabited> IsUninhabited for Box<T> {
    fn is_uninhabited(&self) -> bool {
        self.as_ref().is_uninhabited()
    }
}

impl<'a> IsUninhabited for &'a [syn::Variant] {
    fn is_uninhabited(&self) -> bool {
        self.iter().all(IsUninhabited::is_uninhabited)
    }
}

impl IsUninhabited for syn::Variant {
    fn is_uninhabited(&self) -> bool {
        self.data.is_uninhabited()
    }
}

impl IsUninhabited for syn::VariantData {
    fn is_uninhabited(&self) -> bool {
        self.fields().is_uninhabited()
    }
}

impl<'a> IsUninhabited for &'a [syn::Field] {
    fn is_uninhabited(&self) -> bool {
        self.iter().any(IsUninhabited::is_uninhabited)
    }
}

impl IsUninhabited for syn::Field {
    fn is_uninhabited(&self) -> bool {
        self.ty.is_uninhabited()
    }
}

impl IsUninhabited for syn::MutTy {
    fn is_uninhabited(&self) -> bool {
        self.ty.is_uninhabited()
    }
}

impl IsUninhabited for syn::Ty {
    fn is_uninhabited(&self) -> bool {
        use syn::Ty::*;
        match *self {
            Never => true,
            Path(None, ref p) => match_uninhabited_pathsegs(&p.segments),
            // Even if `T` in `<T as Trait>::Item` is uninhabited, the
            // associated item may be inhabited, so we can't say for sure
            // that it is uninhabited.
            Path(_, _) => false,
            Paren(ref box_ty)    => box_ty.is_uninhabited(),
            Slice(ref box_ty)    => box_ty.is_uninhabited(),
            Array(ref box_ty, _) => box_ty.is_uninhabited(),
            Tup(ref vec_ty)      => vec_ty.iter().any(syn::Ty::is_uninhabited),
            Rptr(_, ref mut_ty)  => mut_ty.is_uninhabited(),
            // We are more strict than Rust is.
            // Our notion of uninhabited is if the type is generatable or not.
            // The second a type like *const ! is dereferenced you have UB.
            Ptr(ref mut_ty)      => mut_ty.is_uninhabited(),
            BareFn(_) => false,
            Mac(_) => false,
            // Could be, but we can't tell:
            Infer => false,
            // Both of these could be, but type is anonymous:
            TraitObject(_) => false,
            ImplTrait(_) => false,
        }
    }
}

/// Returns true iff the path segements matches one that is known to be
/// uninhabited.
fn match_uninhabited_pathsegs(segs: &[syn::PathSegment]) -> bool {
    util::match_pathsegs(segs, &[
        &["Infallible"],
        &["convert", "Infallible"],
        &["std", "convert", "Infallible"],
        &["core", "convert", "Infallible"]
    ])
}