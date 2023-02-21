use syn::{Index, Member};

use super::*;

mod synthesis;

#[derive(Debug)]
pub(crate) struct EquivalenceDerivation {
    ctx: Type,
    this: Type,
    ident: Ident,
    traits: EquivalenceTraits,
    data: Data<VariantDesc, FieldDesc>,
    _generics: syn::Generics,
}

impl EquivalenceTraits {
    pub fn all_empty(span: Span) -> EquivalenceTraits {
        let clause = Some(true_where_clause(span));
        EquivalenceTraits {
            partial_eq: clause.clone(),
            eq: clause.clone(),
            partial_ord: clause.clone(),
            ord: clause.clone(),
            hash: clause,
        }
    }
}

impl EquivalenceDerivation {
    /// Construct the derivation given a relation and a set of options, and an input span
    pub(crate) fn construct_derivation(
        rel: &RelOptsInner,
        opts: &EquivalenceOpts,
        base_traits: Option<&EquivalenceTraits>,
        base_fwds: &Fwds,
        rel_span: Span,
    ) -> Result<EquivalenceDerivation, darling::Error> {
        let (name, ctx) = match (&rel.name, &rel.ty) {
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
                rel_span,
                "one of relation name or relation type must be specified"
            ),
        };

        let curr_desc = base_fwds
            .parse_to_desc(&name, &FwdDesc::all_delegate())
            .into_owned();

        let this = if opts.generics.params.is_empty() && rel.param.is_empty() {
            syn::parse_str::<Type>(&opts.ident.to_string())
                .expect("a struct's ident should always be a valid type...")
        } else {
            //TODO: fix this
            abort!(Span::call_site(), "generics not yet supported")
        };

        if opts.where_.is_some() {
            //TODO: fix this
            abort!(Span::call_site(), "where clauses not yet supported")
        }
        if rel.where_.is_some() {
            //TODO: fix this
            abort!(
                Span::call_site(),
                "relation-specific where clauses not yet supported"
            )
        }

        //TODO: add generic clause synthesis...
        let data = opts
            .data
            .as_ref()
            .map_enum_variants(|v| v.parse_to_desc(&name, &curr_desc))
            .map_struct_fields(|f| f.parse_to_desc(&name, &curr_desc));

        let traits = EquivalenceTraits::extend_base_traits(
            base_traits,
            rel.full.clone(),
            rel.partial_eq.clone(),
            rel.eq.clone(),
            rel.partial_ord.clone(),
            rel.ord.clone(),
            rel.hash.clone(),
            true,
        )?;

        Ok(EquivalenceDerivation {
            ctx,
            this,
            ident: opts.ident.clone(),
            traits,
            data,
            _generics: opts.generics.clone(),
        })
    }
}

#[derive(Debug, Clone)]
struct FieldDesc {
    ident: Option<Ident>,
    fwd: FwdDesc,
}

impl FieldOpts {
    /// Parse this to a field descriptor, given the current forwarding descriptor and name
    fn parse_to_desc(&self, name: &str, curr_desc: &FwdDesc) -> FieldDesc {
        FieldDesc {
            ident: self.ident.clone(),
            fwd: self.fwds.parse_to_desc(name, curr_desc).into_owned(),
        }
    }
}

#[derive(Debug, Clone)]
struct VariantDesc {
    ident: Ident,
    fields: darling::ast::Fields<FieldDesc>,
}

impl VariantOpts {
    /// Parse this to a variant descriptor, given the current forwarding descriptor and name
    fn parse_to_desc(&self, name: &str, curr_desc: &FwdDesc) -> VariantDesc {
        let curr_desc = self.fwds.parse_to_desc(name, curr_desc);
        VariantDesc {
            ident: self.ident.clone(),
            fields: self
                .fields
                .as_ref()
                .map(|f| f.parse_to_desc(name, &curr_desc)),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub(crate) struct FwdDesc {
    eq: Option<FwdMethod>,
    partial_cmp: Option<FwdMethod>,
    cmp: Option<FwdMethod>,
    hash: Option<FwdMethod>,
}

impl FwdDesc {
    /// Get the next forwarding descriptor, given a the current descriptor and a refinement
    ///
    /// If `strict` is true, then crash if an override is attempted.
    pub(crate) fn next_desc<'a>(
        mut this: Cow<'a, Self>,
        refinement: Cow<FwdDesc>,
        strict: bool,
    ) -> Cow<'a, FwdDesc> {
        let refinement = &*refinement;
        if this.eq != refinement.eq {
            if refinement.partial_cmp.is_some() {
                if this.eq.is_some() && strict {
                    abort_call_site!("ambiguous forwarding rule for PartialEqWith")
                }
                this.to_mut().eq = refinement.eq.clone()
            }
        }
        if this.partial_cmp != refinement.partial_cmp {
            if refinement.partial_cmp.is_some() {
                if this.partial_cmp.is_some() && strict {
                    abort_call_site!("ambiguous forwarding rule for PartialOrdWith")
                }
                this.to_mut().partial_cmp = refinement.partial_cmp.clone()
            }
        }
        if this.cmp != refinement.cmp {
            if refinement.cmp.is_some() {
                if this.cmp.is_some() && strict {
                    abort_call_site!("ambiguous forwarding rule for OrdWith")
                }
                this.to_mut().cmp = refinement.cmp.clone()
            }
        }
        if this.hash != refinement.hash {
            if refinement.hash.is_some() {
                if this.hash.is_some() && strict {
                    abort_call_site!("ambiguous forwarding rule for HashWith")
                }
                this.to_mut().hash = refinement.hash.clone()
            }
        }
        this
    }

    fn all_delegate() -> FwdDesc {
        FwdDesc {
            eq: Some(FwdMethod::Delegate),
            partial_cmp: Some(FwdMethod::Delegate),
            cmp: Some(FwdMethod::Delegate),
            hash: Some(FwdMethod::Delegate),
        }
    }

    //TODO: fix this...
}

impl FwdOptsInner {
    pub(crate) fn parse_to_desc(self) -> FwdDesc {
        let full = self.full.is_present();
        let eq_set = self.eq.is_present();
        let partial_ord_set = self.partial_ord.is_present();
        let ord_set = self.ord.is_present();
        let hash_set = self.hash.is_present();
        let anyone_set = eq_set || partial_ord_set || ord_set || hash_set;
        if anyone_set && full {
            emit_warning!(self.full.span(), "if `full` flag is set to `true`, setting other `equiv` flags to `true` is redundant")
        }
        let set_all = full | !anyone_set;

        if self.ignore.is_present() {
            if self.delegate.is_present() || self.rec.is_present() {
                abort!(
                    self.ignore.span(),
                    "`ignore` flag contradicts `delegate` and `rec` flags"
                )
            }
        } else if self.delegate.is_present() {
            if self.rec.is_present() {
                abort!(
                    self.delegate.span(),
                    "`delegate` flag contradicts `rec` flag"
                )
            }
        }

        let compute_method = |with_fn,
                              map,
                              ignore: &Flag,
                              delegate: &Flag,
                              rec: &Flag,
                              with_fn_name: &str,
                              map_fn_name: &str,
                              ignore_name: &str,
                              delegate_name: &str| {
            let mut has_spec = true;
            let result = match (
                with_fn,
                map,
                ignore.is_present(),
                delegate.is_present(),
                rec.is_present(),
            ) {
                (Some(eq_with), None, false, false, false) => Some(FwdMethod::WithFn(eq_with)),
                (None, Some(map_eq), false, false, rec) => {
                    if rec || self.rec.is_present() || self.map_ctx.is_some() {
                        Some(FwdMethod::MapRec(map_eq, self.map_ctx.clone()))
                    } else {
                        Some(FwdMethod::Map(map_eq))
                    }
                }
                (None, Some(map_eq), false, true, false) => Some(FwdMethod::Map(map_eq)),
                (None, None, true, false, false) => Some(FwdMethod::Ignore),
                (None, None, false, true, false) => {
                    if let Some(map) = &self.map {
                        Some(FwdMethod::Map(map.clone()))
                    } else {
                        Some(FwdMethod::Delegate)
                    }
                }
                (None, None, false, false, true) => {
                    if let Some(map) = &self.map {
                        Some(FwdMethod::MapRec(map.clone(), self.map_ctx.clone()))
                    } else {
                        Some(FwdMethod::Rec(self.map_ctx.clone()))
                    }
                }
                (None, None, false, false, false) => {
                    if let Some(map) = &self.map {
                        if self.rec.is_present() || self.map_ctx.is_some() {
                            Some(FwdMethod::MapRec(map.clone(), self.map_ctx.clone()))
                        } else {
                            Some(FwdMethod::Map(map.clone()))
                        }
                    } else {
                        if set_all || eq_set {
                            has_spec = eq_set;
                            if self.ignore.is_present() {
                                Some(FwdMethod::Ignore)
                            } else if self.delegate.is_present() {
                                Some(FwdMethod::Delegate)
                            } else {
                                Some(FwdMethod::Rec(self.map_ctx.clone()))
                            }
                        } else {
                            None
                        }
                    }
                }
                (Some(x), _, _, _, _) => abort!(
                    x.span(),
                    "{} contradicts later directives for PartialEqWith derivation",
                    with_fn_name
                ),
                (_, Some(x), _, _, _) => abort!(
                    x.span(),
                    "{} contradicts later directives for PartialEqWith derivation",
                    map_fn_name
                ),
                (_, _, true, _, _) => abort!(
                    ignore.span(),
                    "{} contradicts later directives for PartialEqWith derivation",
                    ignore_name
                ),
                (_, _, _, true, _) => abort!(
                    delegate.span(),
                    "{} contradicts later directives for PartialEqWith derivation",
                    delegate_name
                ),
            };
            (has_spec, result)
        };

        let eq = compute_method(
            self.eq_with,
            self.map_eq,
            &self.ignore_eq,
            &self.delegate_eq,
            &self.rec_eq,
            "eq_with",
            "map_eq",
            "ignore_eq",
            "delegate_eq",
        )
        .1;
        let (_has_partial_cmp_spec, partial_cmp) = compute_method(
            self.partial_cmp_with,
            self.map_partial_cmp,
            &self.ignore_partial_ord,
            &self.delegate_partial_ord,
            &self.rec_partial_ord,
            "partial_cmp_with",
            "map_partial_cmp",
            "ignore_partial_ord",
            "delegate_partial_ord",
        );
        //TODO: deal with this later...
        // let partial_cmp = if has_partial_cmp_spec {
        //     partial_cmp
        // } else {
        //     None
        // };
        let cmp = compute_method(
            self.cmp_with,
            self.map_cmp,
            &self.ignore_ord,
            &self.delegate_ord,
            &self.rec_ord,
            "cmp_with",
            "map_cmp",
            "ignore_ord",
            "delegate_ord",
        )
        .1;
        let hash = compute_method(
            self.hash_with,
            self.map_hash,
            &self.ignore_hash,
            &self.delegate_hash,
            &self.rec_hash,
            "hash_with",
            "map_hash",
            "ignore_hash",
            "delegate_hash",
        )
        .1; //TODO: fix this
        FwdDesc {
            eq,
            partial_cmp,
            cmp,
            hash,
        }
    }
}

impl Fwds {
    /// Get the next forwarding descriptor, given the current forwarding descriptor and name
    fn parse_to_desc<'a>(&self, name: &str, curr_desc: &'a FwdDesc) -> Cow<'a, FwdDesc> {
        let mut result = Cow::Borrowed(curr_desc);
        if let Some(default) = &self.default_fwd {
            result = FwdDesc::next_desc(result, Cow::Borrowed(default), false);
        }
        if let Some(named) = self.fwd.get(name) {
            result = FwdDesc::next_desc(result, Cow::Borrowed(named), false);
        }
        result
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
enum FwdMethod {
    Ignore,
    #[default]
    Delegate,
    Rec(Option<Expr>),
    Map(Expr),
    MapRec(Expr, Option<Expr>),
    WithFn(Expr),
}
