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
        let has_specification = self.eq.is_some()
            || self.partial_ord.is_some()
            || self.ord.is_some()
            || self.hash.is_some();

        let has_specific_method = matches!(self.eq, Some(Override::Explicit(_)))
            || matches!(self.partial_ord, Some(Override::Explicit(_)))
            || matches!(self.ord, Some(Override::Explicit(_)))
            || matches!(self.hash, Some(Override::Explicit(_)));

        let all_specified = matches!(self.eq, Some(Override::Explicit(_)))
            && matches!(self.partial_ord, Some(Override::Explicit(_)))
            && matches!(self.ord, Some(Override::Explicit(_)))
            && matches!(self.hash, Some(Override::Explicit(_)));

        let override_method = if let Some(map) = self.map {
            if self.ignore.is_present() {
                abort!(
                    map.span(),
                    "`map` specification conflicts with `ignore` flag"
                )
            }
            let map_span = map.span();
            let result = if self.rec.is_present() {
                if self.delegate.is_present() {
                    abort!(
                        self.delegate.span(),
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
        } else if self.ignore.is_present() {
            if self.delegate.is_present() {
                abort!(
                    self.ignore.span(),
                    "`ignore` flag conflicts with `delegate` flag"
                )
            }
            if self.rec.is_present() {
                abort!(
                    self.ignore.span(),
                    "`ignore` flag conflicts with `delegate` flag"
                )
            }
            if all_specified {
                emit_warning!(self.ignore.span(), "`ignore` flag redundant since forwarding is specified for all Equivalence traits");
            }
            FwdMethod::Ignore
        } else if self.delegate.is_present() {
            if self.rec.is_present() {
                abort!(
                    self.delegate.span(),
                    "`delegate` flag conflicts with `rec` flag"
                )
            }
            if all_specified {
                emit_warning!(self.delegate.span(), "`delegate` flag redundant since forwarding is specified for all Equivalence traits");
            }
            FwdMethod::Delegate
        } else {
            if all_specified && self.rec.is_present() {
                emit_warning!(
                    self.ignore.span(),
                    "`rec` flag redundant since forwarding is specified for all Equivalence traits"
                );
            }
            FwdMethod::Rec
        };

        if self.full.is_present() && has_specification && !has_specific_method {
            let info = if let Some(name) = &self.name {
                format!("in forward block for {name:?}")
            } else {
                format!("in default forward block")
            };
            emit_call_site_warning!("flags `eq`, `partial_ord`, `ord`, `hash` are redundant if `full` flag is specified"; info = "{}", info);
        }

        let eq = match self.eq {
            Some(Override::Explicit(eq)) => Some(eq),
            None if has_specification => None,
            _ => Some(override_method.clone()),
        };

        let partial_cmp = match self.partial_ord {
            Some(Override::Explicit(partial_cmp)) => Some(partial_cmp),
            None if has_specification => None,
            _ => Some(override_method.clone()),
        };

        let cmp = match self.ord {
            Some(Override::Explicit(cmp)) => Some(cmp),
            None if has_specification => None,
            _ => Some(override_method.clone()),
        };

        let hash = match self.hash {
            Some(Override::Explicit(hash)) => Some(hash),
            None if has_specification => None,
            _ => Some(override_method.clone()),
        };

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
pub(crate) enum FwdMethod {
    Ignore,
    #[default]
    Delegate,
    Rec,
    Map(Expr),
    MapRec(Expr),
    WithFn(Expr),
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
