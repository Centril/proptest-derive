extern crate proc_macro;
use proc_macro::TokenStream;

extern crate syn;
#[macro_use]
extern crate quote;

mod util;
mod void;
mod error;
mod use_tracking;
mod attr;
mod ast;
mod derive;

#[proc_macro_derive(Arbitrary, attributes(proptest))]
pub fn derive_proptest_arbitrary(input: TokenStream) -> TokenStream {
    // Bootstrap!
    // This function just delegates to impl_proptest_arbitrary.
    derive::impl_proptest_arbitrary(
        syn::parse_derive_input(&input.to_string()).unwrap()
    ).parse().unwrap()
}