//! This module provides integration tests that test the expansion
//! of the derive macro.

extern crate proc_macro2;

// Borrowed from:
// https://docs.rs/synstructure/0.7.0/src/synstructure/macros.rs.html#104-135
macro_rules! test_derive {
    ($name:path { $($i:tt)* } expands to { $($o:tt)* }) => {
        {
            #[allow(dead_code)]
            fn ensure_compiles() {
                $($i)*
                $($o)*
            }

            test_derive!($name { $($i)* } expands to { $($o)* } no_build);
        }
    };
    ($name:path { $($i:tt)* } expands to { $($o:tt)* } no_build) => {
        {
            let expected = stringify!( $($o)* )
                .parse::<$crate::tests::proc_macro2::TokenStream>()
                .expect("output should be a valid TokenStream");
            let mut expected_toks = $crate::quote::Tokens::new();
            expected_toks.append_all(expected);

            let i = stringify!( $($i)* );
            let parsed = $crate::syn::parse_str::<$crate::syn::DeriveInput>(i).expect(
                concat!("Failed to parse input to `#[derive(",
                    stringify!($name),
                ")]`")
            );
            let res = $name(parsed);
            assert_eq!(res, expected_toks)
        }
    };
}

macro_rules! test {
    ($test_name:ident { $($i:tt)* } expands to { $($o:tt)* }) => {
        #[test]
        fn $test_name() {
            test_derive!(
                $crate::derive::impl_proptest_arbitrary { $($i)* }
                expands to { $($o)* }
            );
        }
    };
}

test! {
    struct_unit_unit {
        #[derive(Debug)]
        struct MyUnitStruct;
    } expands to {
        #[allow(non_upper_case_globals)]
        const IMPL_PROPTEST_ARBITRARY_FOR_MyUnitStruct : () = {
            extern crate proptest as crate_proptest;
        impl crate_proptest::arbitrary::Arbitrary for MyUnitStruct {
            type ValueTree =
                <Self::Strategy as crate_proptest::strategy::Strategy>::Value;

            type Parameters = ();
            type Strategy = crate_proptest::strategy::LazyJustFn<Self>;

            fn arbitrary_with(_top: Self::Parameters) -> Self::Strategy {
                crate_proptest::strategy::LazyJustFn::new(|| MyUnitStruct {} )
            }
        }
        };
    }
}

test! {
    struct_unit_tuple {
        #[derive(Debug)]
        struct MyTupleUnitStruct();
    } expands to {
        #[allow(non_upper_case_globals)]
        const IMPL_PROPTEST_ARBITRARY_FOR_MyTupleUnitStruct : () = {
            extern crate proptest as crate_proptest;
        impl crate_proptest::arbitrary::Arbitrary for MyTupleUnitStruct {
            type ValueTree =
                <Self::Strategy as crate_proptest::strategy::Strategy>::Value;

            type Parameters = ();
            type Strategy = crate_proptest::strategy::LazyJustFn<Self>;

            fn arbitrary_with(_top: Self::Parameters) -> Self::Strategy {
                crate_proptest::strategy::LazyJustFn::new(|| MyTupleUnitStruct {} )
            }
        }
        };
    }
}

test! {
    struct_unit_named {
        #[derive(Debug)]
        struct MyNamedUnitStruct {}
    } expands to {
        #[allow(non_upper_case_globals)]
        const IMPL_PROPTEST_ARBITRARY_FOR_MyNamedUnitStruct : () = {
            extern crate proptest as crate_proptest;
        impl crate_proptest::arbitrary::Arbitrary for MyNamedUnitStruct {
            type ValueTree =
                <Self::Strategy as crate_proptest::strategy::Strategy>::Value;

            type Parameters = ();
            type Strategy = crate_proptest::strategy::LazyJustFn<Self>;

            fn arbitrary_with(_top: Self::Parameters) -> Self::Strategy {
                crate_proptest::strategy::LazyJustFn::new(|| MyNamedUnitStruct {} )
            }
        }
        };
    }
}