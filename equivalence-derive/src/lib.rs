use std::{borrow::Cow, collections::HashMap};

use darling::{
    ast::Data,
    util::{Flag, SpannedValue, Override},
    FromDeriveInput, FromField, FromMeta, FromVariant, ToTokens,
};
use proc_macro::{self, TokenStream};
use proc_macro_error::*;
use quote::quote;
use syn::{
    parse_macro_input, punctuated::Punctuated, spanned::Spanned, token::Where, Expr, Ident, Type,
    WhereClause,
};

use proc_macro2::{Span, TokenStream as TokenStream2};

mod clauses;
mod derivation;
mod syntax;

use clauses::*;
use derivation::*;
use syntax::*;

#[proc_macro_error]
#[proc_macro_derive(Equivalence, attributes(equiv, fwd))]
pub fn derive_equivalence(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input);
    let opts = match EquivalenceOptsParser::from_derive_input(&input) {
        Ok(opts) => opts,
        Err(err) => abort! {
            err.span(),
            "failed to derive Equivalence: {}",
            err
        },
    };

    // Compute base equivalence traits
    let base_traits = match EquivalenceTraits::compute_base_traits(
        opts.full,
        opts.partial_eq.clone(),
        opts.eq.clone(),
        opts.partial_ord.clone(),
        opts.ord.clone(),
        opts.hash.clone(),
        true,
    ) {
        Ok(traits) => traits,
        Err(err) => abort! {
            err.span(),
            "failed to compute set of Equivalence traits to derive: {}",
            err
        },
    };

    // Compute base forwardings
    let base_fwds = Fwds::new(opts.fwd.clone());

    let mut result = quote! {};

    // If no relations specified, insert default relation
    if opts.rel.is_empty() {
        abort!(Span::call_site(), "default relation not yet supported")
    } else if let Some(ctx) = &opts.ctx {
        abort!(
            ctx.span(),
            "setting the context variable name is not defined if a relation is given"
        )
    }

    // For each relation, synthesize code for that relation and push
    for rel in &opts.rel {
        let derivation = match ContextDerivation::construct_derivation(
            &rel.0,
            &opts,
            base_traits.as_ref(),
            &base_fwds,
            rel.span(),
        ) {
            Ok(traits) => traits,
            Err(err) => abort! {
                err.span(),
                "failed to compute derivation for relation {}: {}",
                rel.0.name(),
                err
            },
        };
        derivation.synthesize(&mut result);
    }

    result.into()
}
