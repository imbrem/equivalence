use syn::{Index, Member};

use super::*;

mod synthesis;

#[derive(Debug)]
pub(crate) struct ContextDerivation {
    ctx: Type,
    this: Type,
    ident: Ident,
    traits: EquivalenceTraits,
    data: Data<VariantDesc, FieldDesc>,
    _generics: syn::Generics,
}

impl ContextDerivation {
    /// Construct the derivation given a relation and a set of options, and an input span
    pub(crate) fn construct_derivation(
        rel: &RelOptsParser,
        opts: &EquivalenceOptsParser,
        base_traits: Option<&EquivalenceTraits>,
        base_fwds: &Fwds,
        rel_span: Span,
    ) -> Result<ContextDerivation, darling::Error> {
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
            .parse_to_desc(&name, &FwdMethods::all_delegate())
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

        Ok(ContextDerivation {
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
    fwd: FwdMethods,
}

impl FieldOpts {
    /// Parse this to a field descriptor, given the current forwarding descriptor and name
    fn parse_to_desc(&self, name: &str, curr_desc: &FwdMethods) -> FieldDesc {
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
    fn parse_to_desc(&self, name: &str, curr_desc: &FwdMethods) -> VariantDesc {
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
pub(crate) struct FwdMethods {
    pub(crate) eq: Option<FwdMethod>,
    pub(crate) partial_cmp: Option<FwdMethod>,
    pub(crate) cmp: Option<FwdMethod>,
    pub(crate) hash: Option<FwdMethod>,
}

impl FwdMethods {
    pub(crate) fn all_rec() -> FwdMethods {
        FwdMethods {
            eq: Some(FwdMethod::Rec),
            partial_cmp: Some(FwdMethod::Rec),
            cmp: Some(FwdMethod::Rec),
            hash: Some(FwdMethod::Rec),
        }
    }

    pub(crate) fn all_delegate() -> FwdMethods {
        FwdMethods {
            eq: Some(FwdMethod::Delegate),
            partial_cmp: Some(FwdMethod::Delegate),
            cmp: Some(FwdMethod::Delegate),
            hash: Some(FwdMethod::Delegate),
        }
    }

    /// Get the next forwarding descriptor, given a the current descriptor and a refinement
    ///
    /// If `strict` is true, then crash if an override is attempted.
    pub(crate) fn next_desc<'a>(
        mut this: Cow<'a, Self>,
        refinement: Cow<FwdMethods>,
        strict: bool,
    ) -> Cow<'a, FwdMethods> {
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

    //TODO: fix this...
}

impl Fwds {
    /// Get the next forwarding descriptor, given the current forwarding descriptor and name
    fn parse_to_desc<'a>(&self, name: &str, curr_desc: &'a FwdMethods) -> Cow<'a, FwdMethods> {
        //NOTE: named overrides default overrides current
        let mut result = Cow::Borrowed(curr_desc);
        if let Some(default) = &self.default_fwd {
            result = FwdMethods::next_desc(result, Cow::Borrowed(default), false);
        }
        if let Some(named) = self.fwd.get(name) {
            result = FwdMethods::next_desc(result, Cow::Borrowed(named), false);
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
