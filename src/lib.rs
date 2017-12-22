#![recursion_limit="128"]

extern crate proc_macro;

extern crate syn;
#[macro_use] extern crate quote;
//#[macro_use] extern crate darling;

//extern crate synstructure;

use proc_macro::TokenStream;

mod util;
mod void;
mod error;
mod attr;
mod derive;

use derive::impl_proptest_arbitrary;

#[proc_macro_derive(Arbitrary, attributes(proptest))]
pub fn derive_proptest_arbitrary(input: TokenStream) -> TokenStream {
    // Bootstrap!
    // This function just delegates to impl_proptest_arbitrary.

    // Get the concrete syntax of the type definition:
    let s = input.to_string();

    // Parse concrete syntax into AST:
    let ast = syn::parse_derive_input(&s).unwrap();

    // Build impl:
    let gen = impl_proptest_arbitrary(ast);

    // Pretty print the generated tokens into the impl:
    gen.parse().unwrap()
}