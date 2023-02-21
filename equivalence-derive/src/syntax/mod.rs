use super::*;

mod utils;
pub(crate) use utils::*;

#[derive(Debug, FromDeriveInput)]
#[darling(attributes(equiv))]
pub(crate) struct EquivalenceOptsParser {
    #[darling(multiple)]
    pub(crate) rel: Vec<SpannedValue<RelOpts>>,
    pub(crate) full: Flag,
    pub(crate) partial_eq: Option<WhereFlag>,
    pub(crate) eq: Option<WhereFlag>,
    pub(crate) partial_ord: Option<WhereFlag>,
    pub(crate) ord: Option<WhereFlag>,
    pub(crate) hash: Option<WhereFlag>,
    pub(crate) ctx: Option<Ident>,
    #[darling(multiple)]
    pub(crate) fwd: Vec<FwdOpts>,
    #[darling(rename = "where", default)]
    pub(crate) where_: Option<WhereClause>,
    pub(crate) data: Data<VariantOpts, FieldOpts>,
    pub(crate) generics: syn::Generics,
    pub(crate) ident: Ident,
}

#[derive(Debug, FromMeta, Default)]
pub(crate) struct RelOptsInner {
    pub(crate) name: Option<SpannedValue<String>>,
    pub(crate) ty: Option<Type>,
    pub(crate) full: Flag,
    pub(crate) partial_eq: Option<WhereFlag>,
    pub(crate) eq: Option<WhereFlag>,
    pub(crate) partial_ord: Option<WhereFlag>,
    pub(crate) ord: Option<WhereFlag>,
    pub(crate) hash: Option<WhereFlag>,
    #[darling(multiple)]
    pub(crate) param: Vec<Ident>,
    #[darling(rename = "where")]
    pub(crate) where_: Option<WhereClause>,
}

impl RelOptsInner {
    pub(crate) fn name(&self) -> String {
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
pub(crate) struct RelOpts(pub(crate) RelOptsInner);

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
pub(crate) struct FwdOptsInner {
    pub(crate) name: Option<String>,
    pub(crate) full: Flag,
    pub(crate) eq: Option<Override<FwdMethod>>,
    pub(crate) partial_ord: Option<Override<FwdMethod>>,
    pub(crate) ord: Option<Override<FwdMethod>>,
    pub(crate) hash: Option<Override<FwdMethod>>,
    pub(crate) map: Option<Expr>,
    pub(crate) ignore: Flag,
    pub(crate) delegate: Flag,
    pub(crate) rec: Flag,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct FwdOpts(pub(crate) FwdOptsInner);

impl FromMeta for FwdOpts {
    fn from_word() -> darling::Result<Self> {
        Ok(FwdOpts::default())
    }

    fn from_list(items: &[syn::NestedMeta]) -> darling::Result<Self> {
        FwdOptsInner::from_list(items).map(FwdOpts)
    }

    // fn from_string(value: &str) -> darling::Result<Self> {
    //     Ok(FwdOpts(FwdOptsInner {
    //         name: Some(value.into()),
    //         ..Default::default()
    //     }))
    // }

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
pub(crate) struct FieldOpts {
    pub(crate) fwds: Fwds,
    pub(crate) ident: Option<Ident>,
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
pub(crate) struct VariantOpts {
    pub(crate) ident: Ident,
    pub(crate) fwds: Fwds,
    pub(crate) fields: darling::ast::Fields<FieldOpts>,
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