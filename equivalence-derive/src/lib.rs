use std::{borrow::Cow, collections::HashMap};

use darling::{
    ast::Data,
    util::{Flag, SpannedValue},
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

use clauses::*;
use derivation::*;

#[derive(Debug, FromDeriveInput)]
#[darling(attributes(equiv))]
pub(crate) struct EquivalenceOpts {
    #[darling(multiple)]
    rel: Vec<SpannedValue<RelOpts>>,
    full: Option<SpannedValue<SetFlag>>,
    partial_eq: Option<WhereFlag>,
    eq: Option<WhereFlag>,
    partial_ord: Option<WhereFlag>,
    ord: Option<WhereFlag>,
    hash: Option<WhereFlag>,
    ctx: Option<Ident>,
    #[darling(multiple)]
    fwd: Vec<FwdOpts>,
    #[darling(rename = "where", default)]
    where_: Option<WhereClause>,
    data: Data<VariantOpts, FieldOpts>,
    generics: syn::Generics,
    ident: Ident,
}

#[derive(Debug, FromMeta, Default)]
pub(crate) struct RelOptsInner {
    name: Option<SpannedValue<String>>,
    ty: Option<Type>,
    full: Flag,
    partial_eq: Option<WhereFlag>,
    eq: Option<WhereFlag>,
    partial_ord: Option<WhereFlag>,
    ord: Option<WhereFlag>,
    hash: Option<WhereFlag>,
    #[darling(multiple)]
    param: Vec<Ident>,
    #[darling(rename = "where")]
    where_: Option<WhereClause>,
}

impl RelOptsInner {
    fn name(&self) -> String {
        if let Some(name) = &self.name {
            (**name).clone()
        } else if let Some(ty) = &self.ty {
            ty.to_token_stream().to_string()
        } else {
            "unnamed".to_string()
        }
    }
}

#[derive(Debug)]
struct RelOpts(RelOptsInner);

impl FromMeta for RelOpts {
    fn from_list(items: &[syn::NestedMeta]) -> darling::Result<Self> {
        RelOptsInner::from_list(items).map(RelOpts)
    }

    fn from_value(value: &syn::Lit) -> darling::Result<Self> {
        (match *value {
            syn::Lit::Bool(ref b) => Self::from_bool(b.value),
            syn::Lit::Str(ref s) => Ok(RelOpts(RelOptsInner {
                name: Some(SpannedValue::new(s.value().into(), s.span())),
                ..Default::default()
            })),
            syn::Lit::Char(ref ch) => Self::from_char(ch.value()),
            _ => Err(darling::Error::unexpected_lit_type(value)),
        })
        .map_err(|e| e.with_span(value))
    }

    fn from_string(value: &str) -> darling::Result<Self> {
        Ok(RelOpts(RelOptsInner {
            name: Some(SpannedValue::new(value.into(), Span::call_site())),
            ..Default::default()
        }))
    }
}

#[derive(Debug, Clone, FromMeta, Default)]
struct FwdOptsInner {
    name: Option<String>,
    full: Flag,
    eq: Flag,
    partial_ord: Flag,
    ord: Flag,
    hash: Flag,
    map: Option<Expr>,
    map_eq: Option<Expr>,
    map_partial_cmp: Option<Expr>,
    map_cmp: Option<Expr>,
    map_hash: Option<Expr>,
    eq_with: Option<Expr>,
    cmp_with: Option<Expr>,
    partial_cmp_with: Option<Expr>,
    hash_with: Option<Expr>,
    ignore: Flag,
    delegate: Flag,
    rec: Flag,
    ignore_eq: Flag,
    delegate_eq: Flag,
    rec_eq: Flag,
    ignore_partial_ord: Flag,
    delegate_partial_ord: Flag,
    rec_partial_ord: Flag,
    ignore_ord: Flag,
    delegate_ord: Flag,
    rec_ord: Flag,
    ignore_hash: Flag,
    delegate_hash: Flag,
    rec_hash: Flag,
}

#[derive(Debug, Clone, Default)]
struct FwdOpts(FwdOptsInner);

impl FromMeta for FwdOpts {
    fn from_word() -> darling::Result<Self> {
        Ok(FwdOpts::default())
    }

    fn from_list(items: &[syn::NestedMeta]) -> darling::Result<Self> {
        FwdOptsInner::from_list(items).map(FwdOpts)
    }

    fn from_string(value: &str) -> darling::Result<Self> {
        Ok(FwdOpts(FwdOptsInner {
            name: Some(value.into()),
            ..Default::default()
        }))
    }

    // fn from_bool(value: bool) -> darling::Result<Self> {
    //     Ok(FwdOpts(FwdOptsInner {
    //         full: Some(SpannedValue::new(SetFlag(value), Span::call_site())),
    //         ..Default::default()
    //     }))
    // }
}

#[derive(Debug, Clone, FromField)]
#[darling(attributes(equiv))]
struct FieldOptsParser {
    #[darling(multiple)]
    fwd: Vec<FwdOpts>,
    ident: Option<Ident>,
}

#[derive(Debug, Clone)]
struct Fwds {
    default_fwd: Option<FwdDesc>,
    fwd: HashMap<String, FwdDesc>,
}

impl Fwds {
    fn new(fwds: Vec<FwdOpts>) -> Fwds {
        let mut fwd = HashMap::with_capacity(fwds.len());
        let mut default_fwd = None;
        for f in fwds {
            let f = f.0;
            if let Some(name) = f.name.clone() {
                match fwd.entry(name) {
                    std::collections::hash_map::Entry::Occupied(mut o) => {
                        let o = o.get_mut();
                        let mut old = FwdDesc::default();
                        std::mem::swap(o, &mut old);
                        *o = FwdDesc::next_desc(
                            Cow::Owned(f.parse_to_desc()),
                            Cow::Owned(old),
                            true,
                        )
                        .into_owned();
                    }
                    std::collections::hash_map::Entry::Vacant(v) => {
                        v.insert(f.parse_to_desc());
                    }
                }
            } else {
                let new = FwdDesc::next_desc(
                    Cow::Owned(f.parse_to_desc()),
                    Cow::Owned(default_fwd.unwrap_or(FwdDesc::default())),
                    true,
                )
                .into_owned();
                default_fwd = Some(new)
            }
        }
        Fwds { fwd, default_fwd }
    }
}

#[derive(Debug, Clone)]
struct FieldOpts {
    fwds: Fwds,
    ident: Option<Ident>,
}

impl FromField for FieldOpts {
    fn from_field(field: &syn::Field) -> darling::Result<Self> {
        let parsed = FieldOptsParser::from_field(field)?;
        let fwds = Fwds::new(parsed.fwd);
        Ok(FieldOpts {
            fwds,
            ident: parsed.ident,
        })
    }
}

#[derive(Debug, Clone, FromVariant)]
#[darling(attributes(fwd))]
struct VariantOptsParser {
    ident: Ident,
    #[darling(multiple)]
    fwd: Vec<FwdOpts>,
    fields: darling::ast::Fields<FieldOpts>,
}

#[derive(Debug, Clone)]
struct VariantOpts {
    ident: Ident,
    fwds: Fwds,
    fields: darling::ast::Fields<FieldOpts>,
}

impl FromVariant for VariantOpts {
    fn from_variant(variant: &syn::Variant) -> darling::Result<Self> {
        let parsed = VariantOptsParser::from_variant(variant)?;
        let fwds = Fwds::new(parsed.fwd);
        Ok(VariantOpts {
            fwds,
            fields: parsed.fields,
            ident: parsed.ident,
        })
    }
}

#[derive(Debug, Clone)]
enum WhereFlag {
    Clause(WhereClause),
    Set(SpannedValue<bool>),
}

#[derive(Debug, Copy, Clone)]
struct SetFlag(bool);

impl FromMeta for SetFlag {
    fn from_word() -> darling::Result<Self> {
        Ok(SetFlag(true))
    }

    fn from_bool(value: bool) -> darling::Result<Self> {
        Ok(SetFlag(value))
    }
}

impl FromMeta for WhereFlag {
    fn from_word() -> darling::Result<Self> {
        Ok(WhereFlag::Set(SpannedValue::new(true, Span::call_site())))
    }

    fn from_string(value: &str) -> darling::Result<Self> {
        WhereClause::from_string(value).map(WhereFlag::Clause)
    }

    fn from_bool(value: bool) -> darling::Result<Self> {
        Ok(WhereFlag::Set(SpannedValue::new(value, Span::call_site())))
    }

    fn from_meta(item: &syn::Meta) -> darling::Result<Self> {
        let mut result = (match *item {
            syn::Meta::Path(_) => Self::from_word(),
            syn::Meta::List(ref value) => Self::from_list(
                &value
                    .nested
                    .iter()
                    .cloned()
                    .collect::<Vec<syn::NestedMeta>>()[..],
            ),
            syn::Meta::NameValue(ref value) => Self::from_value(&value.lit),
        })
        .map_err(|e| e.with_span(item))?;
        if let WhereFlag::Set(value) = &mut result {
            *value = SpannedValue::new(*value.as_ref(), item.span())
        }
        Ok(result)
    }
}

fn true_where_clause(span: Span) -> WhereClause {
    WhereClause {
        where_token: Where { span },
        predicates: Punctuated::default(),
    }
}

trait FlagSet: Sized {
    fn flag_set(&self) -> Option<bool>;

    fn where_span(&self) -> Span;

    fn is_set_true(&self) -> bool {
        self.flag_set() == Some(true)
    }

    fn into_where_flag(self) -> Option<WhereFlag> {
        self.flag_set()
            .map(|v| WhereFlag::Set(SpannedValue::new(v, Span::call_site())))
    }

    fn into_where_clause(self) -> Option<WhereClause> {
        self.into_where_flag()?.into_where_clause()
    }
}

impl FlagSet for Flag {
    fn flag_set(&self) -> Option<bool> {
        if self.is_present() {
            Some(true)
        } else {
            None
        }
    }

    fn where_span(&self) -> Span {
        self.span()
    }
}

impl FlagSet for WhereFlag {
    fn flag_set(&self) -> Option<bool> {
        match self {
            WhereFlag::Clause(_) => Some(true),
            WhereFlag::Set(set) => Some(*set.as_ref()),
        }
    }

    fn where_span(&self) -> Span {
        match self {
            WhereFlag::Clause(c) => c.span(),
            WhereFlag::Set(_) => Span::call_site(),
        }
    }

    fn is_set_true(&self) -> bool {
        match self {
            WhereFlag::Clause(_) => false,
            WhereFlag::Set(set) => *set.as_ref(),
        }
    }

    fn into_where_flag(self) -> Option<WhereFlag> {
        Some(self)
    }

    fn into_where_clause(self) -> Option<WhereClause> {
        match self {
            WhereFlag::Clause(clause) => Some(clause),
            WhereFlag::Set(set) => {
                if *set {
                    Some(true_where_clause(set.span()))
                } else {
                    None
                }
            }
        }
    }
}

impl<T> FlagSet for Option<T>
where
    T: FlagSet,
{
    fn flag_set(&self) -> Option<bool> {
        self.as_ref()?.flag_set()
    }

    fn where_span(&self) -> Span {
        if let Some(this) = self {
            this.where_span()
        } else {
            Span::call_site()
        }
    }

    fn is_set_true(&self) -> bool {
        if let Some(this) = self {
            this.is_set_true()
        } else {
            false
        }
    }

    fn into_where_flag(self) -> Option<WhereFlag> {
        self?.into_where_flag()
    }
}

impl FlagSet for SetFlag {
    fn flag_set(&self) -> Option<bool> {
        Some(self.0)
    }

    fn where_span(&self) -> Span {
        Span::call_site()
    }
}

impl FlagSet for SpannedValue<SetFlag> {
    fn flag_set(&self) -> Option<bool> {
        Some(self.0)
    }

    fn where_span(&self) -> Span {
        self.span()
    }
}

#[proc_macro_error]
#[proc_macro_derive(Equivalence, attributes(equiv, fwd))]
pub fn derive_equivalence(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input);
    let opts = match EquivalenceOpts::from_derive_input(&input) {
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
        let derivation = match EquivalenceDerivation::construct_derivation(
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
