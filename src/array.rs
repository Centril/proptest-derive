// Copyright 2018 Mazdak Farrokhzad
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Provides a tracker for array sizes > 32 used in fields.
//! <TEMPORARY FIX, REMOVE WITH CONST GENERICS!>

const PROPTEST_ARRAY_MAX_SIZE: usize = 32;

use syn;
use quote::{Tokens, ToTokens};

// We use a BTreeSet to preserve insertion order so that we
// are not in the way of incremental compilation and such.
use std::collections::btree_set::BTreeSet;

use std::rc::Rc;

/// Special parts needed in particular for arrays to construct
/// the typename for the UniformArrayStrategy.
pub type UASParts = (usize, Rc<syn::Ident>);

/// `ArraySizeTracker` tracks uses of `any_with::<[?; <size>]>`
/// or similar where `<size> > 32` so that necessary strategies
/// can be emitted.
#[derive(Debug)]
pub struct ArraySizeTracker {
    /// A record of all the array sizes that need strategies.
    sizes: BTreeSet<usize>,
    /// The name of the type we are doing this for.
    ident: Rc<syn::Ident>,
}

/// The coproduct A + B.
pub enum Either<A, B> { Left(A), Right(B) }

impl ArraySizeTracker {
    /// Constructs a new tracker that starts of having tracked nothing.
    pub fn new(ident: syn::Ident) -> Self {
        Self {
            sizes: BTreeSet::new(),
            ident: Rc::new(ident),
        }
    }

    /// Track the array size of the given type is an
    /// immediate array and then return the inner type.
    pub fn track_type(&mut self, ty: syn::Ty)
        -> Either<syn::Ty, (syn::Ty, UASParts)>
    {
        Either::Left(match ty {
            syn::Ty::Array(ty, size_expr) => {
                if let Some(size) = eval_expr(&size_expr) {
                    if size > PROPTEST_ARRAY_MAX_SIZE {
                        self.sizes.insert(size);
                        return Either::Right(
                            (*ty, (size, Rc::clone(&self.ident)))
                        )
                    }
                }
                syn::Ty::Array(ty, size_expr)
            },
            ty => ty,
        })
    }
}

impl ToTokens for ArraySizeTracker {
    fn to_tokens(&self, tokens: &mut Tokens) {
        if self.sizes.is_empty() { return; }

        let uas = uniform_array_strategy_ident(&self.ident);
        let avt = array_value_tree_ident(&self.ident);

        tokens.append(uas_type_tokens(&uas, &avt));

        self.sizes.iter().for_each(|&size| {
            tokens.append(uas_impl_tokens(&uas, &avt, size));
        })
    }
}

/// Linearises the type declaration of the UAS and AVT.
fn uas_type_tokens(uas: &syn::Ident, avt: &syn::Ident) -> Tokens {
    quote! {
        #[allow(non_camel_case_types)]
        #[derive(Clone, Copy, Debug)]
        pub struct #uas<S, T> {
            strategy: S,
            _marker: ::std::marker::PhantomData<T>,
        }

        impl<S, T> #uas<S, T> {
            pub fn new(strategy: S) -> Self {
                Self { strategy, _marker: ::std::marker::PhantomData, }
            }
        }

        #[allow(non_camel_case_types)]
        #[derive(Clone, Copy, Debug)]
        pub struct #avt<T> {
            tree: T,
            shrinker: usize,
            last_shrinker: Option<usize>,
        }
    }
}

/// Linearises the impl for one UAS and AVT for the given array size.
fn uas_impl_tokens(uas: &syn::Ident, avt: &syn::Ident, size: usize) -> Tokens {
    #[derive(Copy, Clone)]
    struct NewValue;

    struct Current(usize);

    impl ToTokens for NewValue {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append("self.strategy.new_value(runner)?");
        }
    }

    impl ToTokens for Current {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append("self.tree[");
            self.0.to_tokens(tokens);
            tokens.append("].current()");
        }
    }

    let new_values = ::std::iter::repeat(NewValue).take(size);
    let current_ix = (0..size).map(Current);

    quote! {
        impl<S: ::proptest::strategy::Strategy>
            ::proptest::strategy::Strategy
        for #uas<S, [::proptest::strategy::ValueFor<S>; #size]> {
            type Value = #avt<[S::Value; #size]>;

            fn new_value(&self, runner: &mut ::proptest::test_runner::TestRunner)
                         -> Result<Self::Value, String> {
                Ok(#avt {
                    tree: [#(#new_values,)*],
                    shrinker: 0,
                    last_shrinker: None,
                })
            }
        }

        impl<T: ::proptest::strategy::ValueTree>
            ::proptest::strategy::ValueTree
        for #avt<[T; #size]> {
            type Value = [T::Value; #size];

            fn current(&self) -> [T::Value; #size] { [#(#current_ix,)*] }

            fn simplify(&mut self) -> bool {
                while self.shrinker < #size {
                    if self.tree[self.shrinker].simplify() {
                        self.last_shrinker = Some(self.shrinker);
                        return true;
                    } else {
                        self.shrinker += 1;
                    }
                }

                false
            }

            fn complicate(&mut self) -> bool {
                if let Some(shrinker) = self.last_shrinker {
                    self.shrinker = shrinker;
                    if self.tree[shrinker].complicate() {
                        true
                    } else {
                        self.last_shrinker = None;
                        false
                    }
                } else {
                    false
                }
            }
        }
    }
}

/// The special `UniformArrayStrategy` typename for the derived type.
pub fn uniform_array_strategy_ident(impl_id: &syn::Ident) -> syn::Ident {
    format!("__proptest_internal_UniformArrayStrategy_for_{}", impl_id.as_ref())
        .into()
}

/// The special `ArrayValueTree` typename for the derived type.
fn array_value_tree_ident(impl_id: &syn::Ident) -> syn::Ident {
    format!("__proptest_internal_ArrayValueTree_for_{}", impl_id.as_ref())
        .into()
}

/// A very simple interpreter of a subset of const exprs
/// where only +, -, *, /, parenthesis and integer literals are allowed.
///
/// Simple recursion is used as it is highly unlikely that we
/// will blow the stack.
///
/// If overflow or underflow happens we will happily panic.
fn eval_expr(expr: &syn::ConstExpr) -> Option<usize> {
    use syn::ConstExpr::*;
    use syn::BinOp::{Add, Sub, Mul, Div};
    use syn::Lit::Int;
    use syn::IntTy::{Usize, Unsuffixed};

    match *expr {
        Paren(ref expr) => eval_expr(expr),
        Binary(Mul, ref l, ref r) => Some(eval_expr(l)? * eval_expr(r)?),
        Binary(Div, ref l, ref r) => Some(eval_expr(l)? / eval_expr(r)?),
        Binary(Add, ref l, ref r) => Some(eval_expr(l)? + eval_expr(r)?),
        Binary(Sub, ref l, ref r) => Some(eval_expr(l)? - eval_expr(r)?),
        Lit(Int(val, Usize)) | Lit(Int(val, Unsuffixed)) => Some(val as usize),
        _ => None,
    }
}