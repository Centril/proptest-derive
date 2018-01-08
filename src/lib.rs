// Copyright 2018 Mazdak Farrokhzad
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! TODO

#![recursion_limit="128"]

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
mod array;
mod derive;

/// TODO
#[proc_macro_derive(Arbitrary, attributes(proptest))]
pub fn derive_proptest_arbitrary(input: TokenStream) -> TokenStream {
    // Bootstrap!
    // This function just delegates to impl_proptest_arbitrary.
    derive::impl_proptest_arbitrary(
        syn::parse_derive_input(&input.to_string()).unwrap()
    ).parse().unwrap()
}