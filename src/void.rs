use syn;

pub (crate) trait IsUninhabited {
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
            Path(None, ref p) => if match_uninhabited_pathsegs(&p.segments) {
                true
            } else {
                false
            },
            Path(_, _) => {
                // TODO
                false
            },
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

const KNOWN_UNINHABITED_PATH_SEGMENTS : &[&[&str]] = &[
    &["std", "convert", "Infallible"],
    &["core", "convert", "Infallible"]
];

fn match_uninhabited_pathsegs(segs: &[syn::PathSegment]) -> bool {
    KNOWN_UNINHABITED_PATH_SEGMENTS.iter().any(|kups|
        kups.len() == segs.len() &&
        segs.iter().map(|seg| &seg.ident).zip(kups.iter())
            .all(|(x, y)| x == y)
    )
}