use syn::{Index, Member};

use super::*;

mod synthesis;

#[derive(Debug, Clone)]
pub(crate) struct EquivalenceDerivation {
    pub(crate) rels: HashMap<String, RelDesc>,
    // pub(crate) ctx: Option<Ident>,
    pub(crate) base_fwds: Fwds,
    pub(crate) data: Data<VariantOpts, FieldOpts>,
    pub(crate) ident: Ident,
}

impl EquivalenceDerivation {
    fn context_derivations(&mut self) -> impl Iterator<Item = ContextDerivation> + '_ {
        self.rels.drain().map(|(name, rel)| {
            let curr_desc = self
                .base_fwds
                .element_desc(&name, &FwdMethods::all_delegate())
                .into_owned();

            //TODO: add generic clause synthesis...
            let data = self
                .data
                .as_ref()
                .map_enum_variants(|v| v.parse_to_desc(&name, &curr_desc))
                .map_struct_fields(|f| f.parse_to_desc(&name, &curr_desc));

            ContextDerivation {
                ident: self.ident.clone(),
                rel,
                data,
            }
        })
    }

    pub(crate) fn synthesize(&mut self, result: &mut TokenStream2) {
        for derivation in self.context_derivations() {
            derivation.synthesize(result);
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct RelDesc {
    pub(crate) ctx: Type,
    pub(crate) ty: Type,
    pub(crate) bounds: EquivalenceBounds,
}

#[derive(Debug, Clone)]
pub(crate) struct EquivalenceBounds {
    pub(crate) partial_eq: Option<WhereClause>,
    pub(crate) eq: Option<WhereClause>,
    pub(crate) partial_ord: Option<WhereClause>,
    pub(crate) ord: Option<WhereClause>,
    pub(crate) hash: Option<WhereClause>,
}

impl EquivalenceBounds {
    pub(crate) fn all_empty(span: Span) -> EquivalenceBounds {
        let clause = Some(true_where_clause(span));
        EquivalenceBounds {
            partial_eq: clause.clone(),
            eq: clause.clone(),
            partial_ord: clause.clone(),
            ord: clause.clone(),
            hash: clause,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Fwds {
    pub(crate) default_fwd: Option<FwdMethods>,
    pub(crate) fwd: HashMap<String, FwdMethods>,
}

#[derive(Debug)]
pub(crate) struct ContextDerivation {
    ident: Ident,
    rel: RelDesc,
    data: Data<VariantDesc, FieldDesc>,
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
            fwd: self.fwds.element_desc(name, curr_desc).into_owned(),
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
        let curr_desc = self.fwds.element_desc(name, curr_desc);
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

    /// Given the current descriptor and a descriptor for the desired element, get the element descriptor
    ///
    /// If `strict` is true, then crash if a forwarding method override is attempted
    pub(crate) fn compute_element_desc<'a>(
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
    /// Given the current forwarding descriptor, get the forwarding descriptor for the given named element
    fn element_desc<'a>(&self, name: &str, curr_desc: &'a FwdMethods) -> Cow<'a, FwdMethods> {
        //NOTE: named overrides default overrides current
        let mut result = Cow::Borrowed(curr_desc);
        if let Some(default) = &self.default_fwd {
            result = FwdMethods::compute_element_desc(result, Cow::Borrowed(default), false);
        }
        if let Some(named) = self.fwd.get(name) {
            result = FwdMethods::compute_element_desc(result, Cow::Borrowed(named), false);
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
