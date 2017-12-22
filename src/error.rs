pub fn has_lifetimes() -> ! {    
    panic!("#[derive(Arbitrary)] from proptest_derive does not support types \
            that are parametric over lifetimes.");
}

pub fn not_struct_or_enum() -> ! {
    // Overspecified atm, to catch future support in syn for unions.
    panic!("#[derive(Arbitrary)] from proptest_derive is only defined for \
            structs and enums.");
}

pub fn uninhabited_struct() -> ! {    
    panic!("#[derive(Arbitrary)] from proptest_derive is only supported \
            for inhabited types. The given struct is not inhabited \
            since one of the fields in uninhabited.")
}

pub fn uninhabited_enum() -> ! {    
    panic!("#[derive(Arbitrary)] from proptest_derive is only supported \
            for inhabited types. The given enum is not inhabited since \
            all of the variants are uninhabited.");
}

pub fn illegal_strat(item: &str) -> ! {
    panic!("#[proptest(strategy = \"<strategy>\")] is not allowed on {} \
            at the moment. Only struct fields, enum variants and fields inside \
            those can use an explicit strategy in #[derive(Arbitrary)] from\
            proptest_derive."
          , item);
}

pub fn illegal_skip(item: &str) -> ! {
    panic!("{} can not be #[proptest(skip)]ed, only enum variants can in \
            #[derive(Arbitrary)] from proptest_derive."
          , item);
}

pub fn illegal_weight(item: &str) -> ! {
    panic!("#[proptest(weight = <integer>)] is not allowed on {} as it is \
            meaningless. Only enum variants can assign weights in \
            #[derive(Arbitrary)] from proptest_derive."
          , item);
}

pub fn parent_has_strat(item: &str) -> ! {
    panic!("Can not set strategy on {} since it was set on the parent \
            in #[derive(Arbitrary)] from proptest_derive."
          , item);
}

pub fn parent_has_param(item: &str) -> ! {
    panic!("Can not set parameters type on {} since it was set on the \
            parent in #[derive(Arbitrary)] from proptest_derive."
          , item);
}
