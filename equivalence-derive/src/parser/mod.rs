use super::*;

mod utils;
pub(crate) use utils::*;

#[derive(Debug, FromDeriveInput)]
#[darling(attributes(equiv))]
pub(crate) struct EquivalenceOptsParser {
    #[darling(multiple)]
    rel: Vec<SpannedValue<RelOpts>>,
    full: Flag,
    partial_eq: Option<WhereFlag>,
    eq: Option<WhereFlag>,
    partial_ord: Option<WhereFlag>,
    ord: Option<WhereFlag>,
    hash: Option<WhereFlag>,
    ctx: Option<Ident>,
    #[darling(multiple)]
    fwd: Vec<FwdOptsParser>,
    #[darling(rename = "where", default)]
    where_: Option<WhereClause>,
    data: Data<VariantOpts, FieldOpts>,
    generics: syn::Generics,
    ident: Ident,
}

impl FromDeriveInput for EquivalenceDerivation {
    fn from_derive_input(input: &syn::DeriveInput) -> darling::Result<Self> {
        let opts = EquivalenceOptsParser::from_derive_input(input)?;
        let base_traits = EquivalenceBounds::compute_base_traits(
            opts.full,
            opts.partial_eq.clone(),
            opts.eq.clone(),
            opts.partial_ord.clone(),
            opts.ord.clone(),
            opts.hash.clone(),
            true,
        )?;
        let base_fwds = Fwds::new(opts.fwd.clone());

        // If no relations specified, insert default relation
        if opts.rel.is_empty() {
            abort!(Span::call_site(), "default relation not yet supported")
        } else if let Some(ctx) = &opts.ctx {
            abort!(
                ctx.span(),
                "setting the context type variable name is not defined if a relation is given"
            )
        }

        if opts.where_.is_some() {
            //TODO: fix this
            abort!(Span::call_site(), "where clauses not yet supported")
        }

        let mut rels = HashMap::with_capacity(opts.rel.len());

        for rel in opts.rel {
            let (name, ctx) = match (&rel.0.name, &rel.0.ty) {
                (None, Some(ty)) => (format!("{:?}", ty), ty.clone()),
                (Some(name), None) => match syn::parse_str::<Type>(name) {
                    Ok(ty) => (name.to_string(), ty),
                    Err(err) => abort!(
                        name.span(),
                        "no type specified, and name {:?} is not a valid type",
                        **name;
                        help = err
                    ),
                },
                (Some(name), Some(ty)) => (name.to_string(), ty.clone()),
                (None, None) => abort!(
                    rel.span(),
                    "one of relation name or relation type must be specified"
                ),
            };

            let ty = if opts.generics.params.is_empty() && rel.0.param.is_empty() {
                syn::parse_str::<Type>(&opts.ident.to_string())
                    .expect("a struct's ident should always be a valid type...")
            } else {
                //TODO: fix this
                abort!(Span::call_site(), "generics not yet supported")
            };

            if rel.0.where_.is_some() {
                //TODO: fix this
                abort!(
                    Span::call_site(),
                    "relation-specific where clauses not yet supported"
                )
            }

            let bounds = EquivalenceBounds::extend_base_traits(
                base_traits.as_ref(),
                rel.0.full.clone(),
                rel.0.partial_eq.clone(),
                rel.0.eq.clone(),
                rel.0.partial_ord.clone(),
                rel.0.ord.clone(),
                rel.0.hash.clone(),
                true,
            )?;

            match rels.entry(name) {
                std::collections::hash_map::Entry::Occupied(o) => {
                    abort! {
                        rel.span(),
                        "duplicate relation name {:?}", o.get()
                    }
                }
                std::collections::hash_map::Entry::Vacant(v) => {
                    v.insert(RelDesc { ctx, ty, bounds });
                }
            }
        }

        Ok(EquivalenceDerivation {
            rels,
            // ctx: opts.ctx,
            base_fwds,
            data: opts.data,
            ident: opts.ident,
        })
    }
}

#[derive(Debug, Clone, FromMeta, Default)]
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

// impl RelOptsParser {
//     pub(crate) fn name(&self) -> String {
//         if let Some(name) = &self.name {
//             (**name).clone()
//         } else if let Some(ty) = &self.ty {
//             ty.to_token_stream().to_string()
//         } else {
//             "unnamed".to_string()
//         }
//     }
// }

#[derive(Debug, Clone)]
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

impl EquivalenceBounds {
    pub(crate) fn compute_base_traits(
        full: impl FlagSet,
        partial_eq: impl FlagSet,
        eq: impl FlagSet,
        partial_ord: impl FlagSet,
        ord: impl FlagSet,
        hash: impl FlagSet,
        warn_redundant: bool,
    ) -> Result<Option<EquivalenceBounds>, darling::Error> {
        let all_unset = partial_eq.flag_set() == None
            && eq.flag_set() == None
            && partial_ord.flag_set() == None
            && ord.flag_set() == None
            && hash.flag_set() == None;
        let full_set = full.flag_set();
        match full_set {
            Some(true) => {
                let anyone_false = partial_eq.flag_set() == Some(false)
                    || eq.flag_set() == Some(false)
                    || partial_ord.flag_set() == Some(false)
                    || ord.flag_set() == Some(false)
                    || hash.flag_set() == Some(false);
                if anyone_false {
                    return Err(darling::Error::custom(
                        "`full` flag set to `true`, but another `equiv` flag is set to `false`",
                    )
                    .with_span(&SpannedValue::new((), full.where_span())));
                } else {
                    let warn_redundant = warn_redundant
                        && (partial_eq.is_set_true()
                            || eq.is_set_true()
                            || partial_ord.is_set_true()
                            || ord.is_set_true()
                            || hash.is_set_true());
                    if warn_redundant {
                        //TODO: consider whether this warning is useful
                        emit_warning!(
                            full.where_span(),
                            "if `full` flag is set to `true`, setting other `equiv` flags to `true` (vs a `where` clause) is redundant"
                        )
                    }
                    Ok(Some(EquivalenceBounds {
                        partial_eq: Some(
                            partial_eq
                                .into_where_clause()
                                .unwrap_or(true_where_clause(full.where_span())),
                        ),
                        eq: Some(
                            eq.into_where_clause()
                                .unwrap_or(true_where_clause(full.where_span())),
                        ),
                        partial_ord: Some(
                            partial_ord
                                .into_where_clause()
                                .unwrap_or(true_where_clause(full.where_span())),
                        ),
                        ord: Some(
                            ord.into_where_clause()
                                .unwrap_or(true_where_clause(full.where_span())),
                        ),
                        hash: Some(
                            hash.into_where_clause()
                                .unwrap_or(true_where_clause(full.where_span())),
                        ),
                    }))
                }
            }
            Some(false) => {
                return Err(darling::Error::custom(
                    "explicitly disabling `full` is currently undefined",
                )
                .with_span(&SpannedValue::new((), full.where_span())))
            }
            None if all_unset => Ok(None),
            None => {
                let eq = eq.into_where_clause();
                let ord = ord.into_where_clause();
                let hash = hash.into_where_clause();
                let partial_eq = partial_eq.into_where_clause();
                let partial_ord = partial_ord.into_where_clause();
                Ok(Some(EquivalenceBounds {
                    partial_eq,
                    eq,
                    partial_ord,
                    ord,
                    hash,
                }))
            }
        }
    }

    pub(crate) fn extend_base_traits(
        base_traits: Option<&Self>,
        full: impl FlagSet,
        partial_eq: impl FlagSet,
        eq: impl FlagSet,
        partial_ord: impl FlagSet,
        ord: impl FlagSet,
        hash: impl FlagSet,
        warn_redundant: bool,
    ) -> Result<EquivalenceBounds, darling::Error> {
        if let Some(base_traits) = base_traits {
            if full.flag_set() != None
                || partial_eq.flag_set() != None
                || eq.flag_set() != None
                || partial_ord.flag_set() != None
                || ord.flag_set() != None
                || hash.flag_set() != None
            {
                //TODO: fix this
                abort! {
                    Span::call_site(),
                    "per-relation implementation not yet supported"
                }
            }
            Ok(base_traits.clone())
        } else {
            Ok(Self::compute_base_traits(
                full,
                partial_eq,
                eq,
                partial_ord,
                ord,
                hash,
                warn_redundant,
            )?
            .unwrap_or_else(|| EquivalenceBounds::all_empty(Span::call_site())))
        }
    }
}

#[derive(Debug, Clone, FromField)]
#[darling(attributes(equiv))]
struct FieldOptsParser {
    #[darling(multiple)]
    fwd: Vec<FwdOptsParser>,
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
    fwd: Vec<FwdOptsParser>,
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

impl Fwds {
    fn new(fwds: Vec<FwdOptsParser>) -> Fwds {
        let mut fwd = HashMap::with_capacity(fwds.len());
        let mut default_fwd = None;
        for f in fwds {
            if let Some(name) = f.name {
                match fwd.entry(name) {
                    std::collections::hash_map::Entry::Occupied(mut o) => {
                        let o = o.get_mut();
                        let mut old = FwdMethods::default();
                        std::mem::swap(o, &mut old);
                        *o = FwdMethods::compute_element_desc(Cow::Owned(f.methods), Cow::Owned(old), true)
                            .into_owned();
                    }
                    std::collections::hash_map::Entry::Vacant(v) => {
                        v.insert(f.methods);
                    }
                }
            } else {
                let new = FwdMethods::compute_element_desc(
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
struct FwdOptsInnerParser {
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

impl From<FwdOptsInnerParser> for FwdOptsParser {
    fn from(value: FwdOptsInnerParser) -> FwdOptsParser {
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

        FwdOptsParser {
            name: value.name,
            methods,
        }
    }
}

#[derive(Debug, Clone, Default)]
struct FwdOptsParser {
    name: Option<String>,
    methods: FwdMethods,
}

impl FromMeta for FwdOptsParser {
    fn from_word() -> darling::Result<Self> {
        Ok(FwdOptsParser {
            name: None,
            methods: FwdMethods::all_rec(),
        })
    }

    fn from_list(items: &[syn::NestedMeta]) -> darling::Result<Self> {
        FwdOptsInnerParser::from_list(items).map(From::from)
    }

    fn from_string(value: &str) -> darling::Result<Self> {
        let fwd_method = FwdMethod::from_string(value)?;
        if matches!(fwd_method, FwdMethod::WithFn(_)) {
            return Err(darling::Error::custom(
                "cannot use `with` to forward all `Equivalence` traits",
            ));
        }
        Ok(FwdOptsParser {
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
        Ok(FwdOptsParser {
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
