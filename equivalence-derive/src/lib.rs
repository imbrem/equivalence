use fnv::FnvHashMap as HashMap;
use std::borrow::Cow;

use darling::{
    ast::Data,
    util::{Flag, Override, SpannedValue},
    FromDeriveInput, FromField, FromMeta, FromVariant,
};
use proc_macro::{self, TokenStream};
use proc_macro_error::*;
use quote::quote;
use syn::{
    parse_macro_input, punctuated::Punctuated, spanned::Spanned, token::Where, Expr, Ident, Type,
    WhereClause,
};

use proc_macro2::{Span, TokenStream as TokenStream2};

mod derivation;
mod parser;

use derivation::*;
use parser::*;

pub(crate) type IdentMap = HashMap<Ident, Ident>;

#[proc_macro_error]
#[proc_macro_derive(Equivalence, attributes(equiv, fwd))]
pub fn derive_equivalence(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input);

    // Generate derivation
    let mut derivation = match EquivalenceDerivation::from_derive_input(&input) {
        Ok(opts) => opts,
        Err(err) => abort! {
            err.span(),
            "failed to parse Equivalence derivation: {}",
            err
        },
    };

    // Synthesize and return output
    let mut result = quote! {};
    derivation.synthesize(&mut result);

    result.into()
}
