use super::*;

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
pub(crate) struct RelOptsParser {
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

impl RelOptsParser {
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
pub(crate) struct RelOpts(pub(crate) RelOptsParser);

impl FromMeta for RelOpts {
    fn from_list(items: &[syn::NestedMeta]) -> darling::Result<Self> {
        RelOptsParser::from_list(items).map(RelOpts)
    }

    fn from_value(value: &syn::Lit) -> darling::Result<Self> {
        (match *value {
            syn::Lit::Bool(ref b) => Self::from_bool(b.value),
            syn::Lit::Str(ref s) => Ok(RelOpts(RelOptsParser {
                name: Some(SpannedValue::new(s.value().into(), s.span())),
                ..Default::default()
            })),
            syn::Lit::Char(ref ch) => Self::from_char(ch.value()),
            _ => Err(darling::Error::unexpected_lit_type(value)),
        })
        .map_err(|e| e.with_span(value))
    }

    fn from_string(value: &str) -> darling::Result<Self> {
        Ok(RelOpts(RelOptsParser {
            name: Some(SpannedValue::new(value.into(), Span::call_site())),
            ..Default::default()
        }))
    }
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

#[derive(Debug, Clone)]
pub(crate) struct Fwds {
    pub(crate) default_fwd: Option<FwdMethods>,
    pub(crate) fwd: HashMap<String, FwdMethods>,
}

impl Fwds {
    pub(crate) fn new(fwds: Vec<FwdOpts>) -> Fwds {
        let mut fwd = HashMap::with_capacity(fwds.len());
        let mut default_fwd = None;
        for f in fwds {
            if let Some(name) = f.name {
                match fwd.entry(name) {
                    std::collections::hash_map::Entry::Occupied(mut o) => {
                        let o = o.get_mut();
                        let mut old = FwdMethods::default();
                        std::mem::swap(o, &mut old);
                        *o = FwdMethods::next_desc(Cow::Owned(f.methods), Cow::Owned(old), true)
                            .into_owned();
                    }
                    std::collections::hash_map::Entry::Vacant(v) => {
                        v.insert(f.methods);
                    }
                }
            } else {
                let new = FwdMethods::next_desc(
                    Cow::Owned(f.methods),
                    Cow::Owned(default_fwd.unwrap_or(FwdMethods::default())),
                    true,
                )
                .into_owned();
                default_fwd = Some(new)
            }
        }
        Fwds { fwd, default_fwd }
    }
}

#[derive(Debug, Clone, FromMeta, Default)]
struct FwdOptsParser {
    name: Option<String>,
    by: Option<SpannedValue<FwdMethod>>,
    full: Flag,
    eq: Option<Override<FwdMethod>>,
    partial_ord: Option<Override<FwdMethod>>,
    ord: Option<Override<FwdMethod>>,
    hash: Option<Override<FwdMethod>>,
    map: Option<Expr>,
    ignore: Flag,
    delegate: Flag,
    rec: Flag,
}

impl From<FwdOptsParser> for FwdOpts {
    fn from(value: FwdOptsParser) -> FwdOpts {
        let has_specification = value.eq.is_some()
            || value.partial_ord.is_some()
            || value.ord.is_some()
            || value.hash.is_some();

        let has_specific_method = matches!(value.eq, Some(Override::Explicit(_)))
            || matches!(value.partial_ord, Some(Override::Explicit(_)))
            || matches!(value.ord, Some(Override::Explicit(_)))
            || matches!(value.hash, Some(Override::Explicit(_)));

        let all_specified = matches!(value.eq, Some(Override::Explicit(_)))
            && matches!(value.partial_ord, Some(Override::Explicit(_)))
            && matches!(value.ord, Some(Override::Explicit(_)))
            && matches!(value.hash, Some(Override::Explicit(_)));

        let override_method = if let Some(by) = value.by {
            if value.map.is_some() {
                abort!(
                    value.map.span(),
                    "`by` specification conflicts with `map` specification"
                )
            }
            if value.ignore.is_present() {
                abort!(
                    value.ignore.span(),
                    "`by` specification conflicts with `ignore` flag"
                )
            }
            if value.delegate.is_present() {
                abort!(
                    value.delegate.span(),
                    "`by` specification conflicts with `delegate` flag"
                )
            }
            if value.rec.is_present() {
                abort!(
                    value.rec.span(),
                    "`by` specification conflicts with `rec` flag"
                )
            }
            if matches!(*by, FwdMethod::WithFn(_)) {
                abort!(by.span(), "`by` specification cannot use `with`")
            }
            //NOTE: can't just move out of a SpannedValue because darling is weird.
            //      go make a pull request about this or smt...
            (*by).clone()
        } else if let Some(map) = value.map {
            if value.ignore.is_present() {
                abort!(
                    map.span(),
                    "`map` specification conflicts with `ignore` flag"
                )
            }
            let map_span = map.span();
            let result = if value.rec.is_present() {
                if value.delegate.is_present() {
                    abort!(
                        value.delegate.span(),
                        "`delegate` flag conflicts with `rec` flag"
                    )
                }
                FwdMethod::MapRec(map)
            } else {
                FwdMethod::Map(map)
            };
            if all_specified {
                emit_warning!(map_span, "`map` specification redundant since forwarding is specified for all Equivalence traits");
            }
            result
        } else if value.ignore.is_present() {
            if value.delegate.is_present() {
                abort!(
                    value.ignore.span(),
                    "`ignore` flag conflicts with `delegate` flag"
                )
            }
            if value.rec.is_present() {
                abort!(
                    value.ignore.span(),
                    "`ignore` flag conflicts with `delegate` flag"
                )
            }
            if all_specified {
                emit_warning!(value.ignore.span(), "`ignore` flag redundant since forwarding is specified for all Equivalence traits");
            }
            FwdMethod::Ignore
        } else if value.delegate.is_present() {
            if value.rec.is_present() {
                abort!(
                    value.delegate.span(),
                    "`delegate` flag conflicts with `rec` flag"
                )
            }
            if all_specified {
                emit_warning!(value.delegate.span(), "`delegate` flag redundant since forwarding is specified for all Equivalence traits");
            }
            FwdMethod::Delegate
        } else {
            if all_specified && value.rec.is_present() {
                emit_warning!(
                    value.ignore.span(),
                    "`rec` flag redundant since forwarding is specified for all Equivalence traits"
                );
            }
            FwdMethod::Rec
        };

        if value.full.is_present() && has_specification && !has_specific_method {
            let info = if let Some(name) = &value.name {
                format!("in forward block for {name:?}")
            } else {
                format!("in default forward block")
            };
            emit_call_site_warning!("flags `eq`, `partial_ord`, `ord`, `hash` are redundant if `full` flag is specified"; info = "{}", info);
        }

        let eq = match value.eq {
            Some(Override::Explicit(eq)) => Some(eq),
            None if has_specification => None,
            _ => Some(override_method.clone()),
        };

        let partial_cmp = match value.partial_ord {
            Some(Override::Explicit(partial_cmp)) => Some(partial_cmp),
            None if has_specification => None,
            _ => Some(override_method.clone()),
        };

        let cmp = match value.ord {
            Some(Override::Explicit(cmp)) => Some(cmp),
            None if has_specification => None,
            _ => Some(override_method.clone()),
        };

        let hash = match value.hash {
            Some(Override::Explicit(hash)) => Some(hash),
            None if has_specification => None,
            _ => Some(override_method.clone()),
        };

        let methods = FwdMethods {
            eq,
            partial_cmp,
            cmp,
            hash,
        };

        FwdOpts {
            name: value.name,
            methods,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub(crate) struct FwdOpts {
    pub(crate) name: Option<String>,
    pub(crate) methods: FwdMethods,
}

impl FromMeta for FwdOpts {
    fn from_word() -> darling::Result<Self> {
        Ok(FwdOpts {
            name: None,
            methods: FwdMethods::all_rec(),
        })
    }

    fn from_list(items: &[syn::NestedMeta]) -> darling::Result<Self> {
        FwdOptsParser::from_list(items).map(From::from)
    }

    fn from_string(value: &str) -> darling::Result<Self> {
        let fwd_method = FwdMethod::from_string(value)?;
        if matches!(fwd_method, FwdMethod::WithFn(_)) {
            return Err(darling::Error::custom(
                "cannot use `with` to forward all `Equivalence` traits",
            ));
        }
        Ok(FwdOpts {
            name: None,
            methods: FwdMethods {
                eq: Some(fwd_method.clone()),
                partial_cmp: Some(fwd_method.clone()),
                cmp: Some(fwd_method.clone()),
                hash: Some(fwd_method),
            },
        })
    }

    fn from_bool(value: bool) -> darling::Result<Self> {
        let methods = if value {
            FwdMethods::all_rec()
        } else {
            FwdMethods::all_delegate()
        };
        Ok(FwdOpts {
            name: None,
            methods,
        })
    }
}

impl FromMeta for FwdMethod {
    fn from_string(value: &str) -> darling::Result<Self> {
        let mut tokens = value.split(" ");
        if let Some(first) = tokens.next() {
            //NOTE: this is a hack since `SplitWhitespace::as_str` is not yet stabilized
            let rest = &value[first.len()..];
            let any_left = tokens.next().is_some();
            match first {
                "map" => syn::parse_str::<Expr>(rest)
                    .map(FwdMethod::Map)
                    .map_err(|e| darling::Error::custom(format!("{e}"))),
                "map_rec" => syn::parse_str::<Expr>(rest)
                    .map(FwdMethod::MapRec)
                    .map_err(|e| darling::Error::custom(format!("{e}"))),
                "with" => syn::parse_str::<Expr>(rest)
                    .map(FwdMethod::WithFn)
                    .map_err(|e| darling::Error::custom(format!("{e}"))),
                "ignore" if !any_left => Ok(FwdMethod::Ignore),
                "delegate" if !any_left => Ok(FwdMethod::Delegate),
                "rec" if !any_left => Ok(FwdMethod::Rec),
                method => Err(darling::Error::custom(format!(
                    "unsupported forwarding method {method:?}"
                ))),
            }
        } else {
            Err(darling::Error::custom(
                "cannot parse FwdMethod from empty string",
            ))
        }
    }

    fn from_bool(value: bool) -> darling::Result<Self> {
        if value {
            Ok(FwdMethod::Rec)
        } else {
            Ok(FwdMethod::Delegate)
        }
    }
}
