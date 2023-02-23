use std::cell::Cell;

use super::*;

use syn::{Generics, TypeParam};

#[derive(Debug, FromDeriveInput)]
#[darling(attributes(equiv))]
pub(crate) struct EquivalenceOptsParser {
    #[darling(multiple)]
    rel: Vec<SpannedValue<RelOpts>>,
    full: Option<SpannedValue<Override<bool>>>,
    partial_eq: Option<Override<WhereFlag>>,
    eq: Option<Override<WhereFlag>>,
    partial_ord: Option<Override<WhereFlag>>,
    ord: Option<Override<WhereFlag>>,
    hash: Option<Override<WhereFlag>>,
    ctx: Option<Ident>,
    #[darling(multiple)]
    fwd: Vec<FwdOptsParser>,
    #[darling(rename = "where", default)]
    where_: Option<WhereClause>,
    data: Data<VariantOpts, FieldOpts>,
    generics: syn::Generics,
    ident: Ident,
    infer_generics: Option<SpannedValue<Override<bool>>>,
}

impl FromDeriveInput for EquivalenceDerivation {
    fn from_derive_input(input: &syn::DeriveInput) -> darling::Result<Self> {
        let opts = EquivalenceOptsParser::from_derive_input(input)?;
        let base_traits = EquivalenceBounds::compute_base_traits(
            opts.full,
            opts.infer_generics,
            opts.partial_eq.clone(),
            opts.eq.clone(),
            opts.partial_ord.clone(),
            opts.ord.clone(),
            opts.hash.clone(),
            opts.where_.clone(),
            &opts.generics,
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

            let bounds = base_traits.specify(
                rel.0.full.clone(),
                rel.0.infer_generics.clone(),
                rel.0.partial_eq.clone(),
                rel.0.eq.clone(),
                rel.0.partial_ord.clone(),
                rel.0.ord.clone(),
                rel.0.hash.clone(),
                rel.0.where_.clone(),
                &opts.generics,
                false,
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
struct RelOptsParser {
    name: Option<SpannedValue<String>>,
    ty: Option<Type>,
    full: Option<SpannedValue<Override<bool>>>,
    partial_eq: Option<Override<WhereFlag>>,
    eq: Option<Override<WhereFlag>>,
    partial_ord: Option<Override<WhereFlag>>,
    ord: Option<Override<WhereFlag>>,
    hash: Option<Override<WhereFlag>>,
    #[darling(multiple)]
    param: Vec<TypeParam>,
    #[darling(rename = "where")]
    where_: Option<WhereClause>,
    infer_generics: Option<SpannedValue<Override<bool>>>,
}

#[derive(Debug, Clone)]
struct RelOpts(RelOptsParser);

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
    fn compute_base_traits(
        full: Option<SpannedValue<Override<bool>>>,
        infer_generics: Option<SpannedValue<Override<bool>>>,
        partial_eq: Option<Override<WhereFlag>>,
        eq: Option<Override<WhereFlag>>,
        partial_ord: Option<Override<WhereFlag>>,
        ord: Option<Override<WhereFlag>>,
        hash: Option<Override<WhereFlag>>,
        where_: Option<WhereClause>,
        generics: &Generics,
        warn_redundant: bool,
    ) -> Result<EquivalenceBounds, darling::Error> {
        EquivalenceBounds {
            partial_eq: None,
            eq: None,
            partial_ord: None,
            ord: None,
            hash: None,
        }
        .specify(
            full,
            infer_generics,
            partial_eq,
            eq,
            partial_ord,
            ord,
            hash,
            where_,
            generics,
            true,
            warn_redundant,
        )
    }

    fn specify(
        &self,
        full: Option<SpannedValue<Override<bool>>>,
        infer_generics: Option<SpannedValue<Override<bool>>>,
        partial_eq: Option<Override<WhereFlag>>,
        eq: Option<Override<WhereFlag>>,
        partial_ord: Option<Override<WhereFlag>>,
        ord: Option<Override<WhereFlag>>,
        hash: Option<Override<WhereFlag>>,
        where_: Option<WhereClause>,
        generics: &Generics,
        strong_full: bool,
        warn_redundant: bool,
    ) -> Result<EquivalenceBounds, darling::Error> {
        let none_specified = partial_eq.is_none()
            && eq.is_none()
            && partial_ord.is_none()
            && ord.is_none()
            && hash.is_none();

        let full_span = full.as_ref().map(Spanned::span);
        let full = match &full {
            Some(full) => match **full {
                Override::Explicit(b) => Some(b),
                Override::Inherit => Some(true),
            },
            None => None,
        };

        let where_span = where_.as_ref().map(Spanned::span);

        let infer_generics_used = Cell::new(infer_generics.is_none());
        let infer_generics_span = infer_generics.as_ref().map(Spanned::span);
        let infer_generics_helper = infer_generics.as_deref();

        let get_infer_generics = |original: Option<&ImplBounds>| {
            infer_generics_used.set(true);
            match infer_generics_helper {
                Some(infer_generics) => infer_generics.clone().unwrap_or(true),
                None => original.map(|original| original.infer).unwrap_or(true),
            }
        };

        let override_clause_used = Cell::new(where_.is_none());
        let get_override_clause = |original: Option<&ImplBounds>| {
            override_clause_used.set(true);
            where_
                .clone()
                .or_else(|| original.map(|original| original.clause.clone()))
                .unwrap_or_else(|| true_where_clause(full_span.unwrap_or(Span::call_site())))
        };

        let get_override = |original: Option<&ImplBounds>| ImplBounds {
            clause: get_override_clause(original),
            infer: get_infer_generics(original),
            generics: generics.clone(),
        };

        let full_override_used = Cell::new(full.is_none());
        let get_full_override = |original: Option<&ImplBounds>| {
            full_override_used.set(true);
            if full == Some(true)
                || (full == None && (where_span.is_some() || (strong_full && none_specified)))
            {
                Some(get_override(original))
            } else if full == None {
                original.cloned()
            } else {
                None
            }
        };

        let get_strategy_bounds = |strategy: WhereStrategy, original: Option<&ImplBounds>| {
            let clause = strategy
                .where_
                .unwrap_or_else(|| get_override_clause(original));
            let infer = strategy
                .infer
                .map(|infer| infer.unwrap_or(true))
                .unwrap_or_else(|| get_infer_generics(original));
            ImplBounds {
                clause,
                infer,
                generics: generics.clone(),
            }
        };

        let get_where_flag_bounds =
            |flag: Option<Override<WhereFlag>>, original: Option<&ImplBounds>| {
                let res = match flag {
                    Some(Override::Inherit) => Some(get_override(original)),
                    Some(Override::Explicit(WhereFlag::Disabled)) => {
                        if let Some(blame) = full {
                            return Err(darling::Error::custom(
                            "`partial_eq` cannot be set to `false` while `full` is set to `true`",
                        )
                        .with_span(&blame));
                        } else {
                            None
                        }
                    }
                    Some(Override::Explicit(WhereFlag::Enabled(strategy))) => {
                        Some(get_strategy_bounds(strategy, original))
                    }
                    None => get_full_override(original),
                };
                Ok(res)
            };

        let partial_eq = get_where_flag_bounds(partial_eq, self.partial_eq.as_ref())?;
        let eq = get_where_flag_bounds(eq, self.eq.as_ref())?;
        let partial_ord = get_where_flag_bounds(partial_ord, self.partial_ord.as_ref())?;
        let ord = get_where_flag_bounds(ord, self.ord.as_ref())?;
        let hash = get_where_flag_bounds(hash, self.hash.as_ref())?;

        if warn_redundant {
            if let Some(where_span) = where_span {
                if !override_clause_used.get() {
                    emit_warning!(where_span, "`where` redundant since all Equivalence traits have manually specified bounds")
                }
            }
            if let Some(full_span) = full {
                if !full_override_used.get() {
                    emit_warning!(
                        full_span,
                        "`full` flag redundant since all Equivalence traits are enabled manually"
                    )
                }
            }
            if let Some(infer_generics_span) = infer_generics_span {
                if !infer_generics_used.get() {
                    emit_warning!(infer_generics_span, "`infer_generics` flag redundant since generic inference has been set manually for all traits")
                }
            }
        }

        Ok(EquivalenceBounds {
            partial_eq,
            eq,
            partial_ord,
            ord,
            hash,
        })
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
                        *o = FwdMethods::compute_element_desc(
                            Cow::Owned(f.methods),
                            Cow::Owned(old),
                            true,
                        )
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

#[derive(Debug, Clone)]
enum WhereFlag {
    Enabled(WhereStrategy),
    Disabled,
}

impl FromMeta for WhereFlag {
    fn from_list(items: &[syn::NestedMeta]) -> darling::Result<Self> {
        WhereStrategy::from_list(items).map(WhereFlag::Enabled)
    }

    fn from_string(value: &str) -> darling::Result<Self> {
        Ok(WhereFlag::Enabled(WhereStrategy {
            where_: Override::Explicit(WhereClause::from_string(value)?),
            infer: None,
        }))
    }

    fn from_bool(value: bool) -> darling::Result<Self> {
        if value {
            Ok(WhereFlag::Enabled(WhereStrategy {
                where_: Override::Inherit,
                infer: None,
            }))
        } else {
            Ok(WhereFlag::Disabled)
        }
    }
}

#[derive(Debug, Clone, FromMeta)]
struct WhereStrategy {
    where_: Override<WhereClause>,
    infer: Option<Override<bool>>,
}
