use darling::{
    usage::{IdentSet, Purpose, UsesTypeParams},
    ToTokens,
};
use syn::{token::Comma, Generics, Index, Member, TypeParam};

use super::*;

mod synthesis;

#[derive(Debug, Clone)]
pub(crate) struct EquivalenceDerivation {
    pub(crate) rels: HashMap<String, RelDesc>,
    // pub(crate) ctx: Option<Ident>,
    pub(crate) base_fwds: Fwds,
    pub(crate) data: Data<VariantOpts, FieldOpts>,
    pub(crate) ident: Ident,
    pub(crate) generics: Generics,
    pub(crate) type_set: IdentSet,
}

impl EquivalenceDerivation {
    fn context_derivations(&mut self) -> impl Iterator<Item = ContextDerivation> + '_ {
        self.rels.drain().map(|(name, mut rel)| {
            let curr_desc = self
                .base_fwds
                .element_desc(&name, &FwdMethods::all_delegate())
                .into_owned();

            let data = self
                .data
                .as_ref()
                .map_enum_variants(|v| {
                    let desc = v.parse_to_desc(&name, &curr_desc);
                    desc.infer_bounds(&mut rel.bounds, &self.type_set, &rel.ctx);
                    desc
                })
                .map_struct_fields(|f| {
                    let desc = f.parse_to_desc(&name, &curr_desc);
                    desc.infer_bounds(&mut rel.bounds, &self.type_set, &rel.ctx);
                    desc
                });

            let mut generics = self.generics.clone();
            let (_, ty_generics, _) = generics.split_for_impl();
            let ident = self.ident.clone();
            let ty = syn::parse_quote!(#ident #ty_generics);

            for param in &rel.params {
                generics.params.push(param.clone().into())
            }

            ContextDerivation {
                ident,
                ty,
                rel,
                data,
                generics,
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
    pub(crate) bounds: EquivalenceBounds,
    pub(crate) params: Punctuated<TypeParam, Comma>,
}

#[derive(Debug, Clone)]
pub(crate) struct EquivalenceBounds {
    pub(crate) partial_eq: Option<ImplBounds>,
    pub(crate) eq: Option<ImplBounds>,
    pub(crate) partial_ord: Option<ImplBounds>,
    pub(crate) ord: Option<ImplBounds>,
    pub(crate) hash: Option<ImplBounds>,
}

#[derive(Debug, Clone)]
pub(crate) struct ImplBounds {
    pub(crate) clause: WhereClause,
    pub(crate) infer: bool,
}

#[derive(Debug, Clone)]
pub(crate) struct Fwds {
    pub(crate) default_fwd: Option<FwdMethods>,
    pub(crate) fwd: HashMap<String, FwdMethods>,
}

#[derive(Debug)]
pub(crate) struct ContextDerivation {
    ident: Ident,
    ty: Type,
    rel: RelDesc,
    data: Data<VariantDesc, FieldDesc>,
    generics: Generics,
}

#[derive(Debug, Clone)]
struct FieldDesc {
    ident: Option<Ident>,
    ty: Type,
    fwd: FwdMethods,
}

impl FieldOpts {
    /// Parse this to a field descriptor, given the current forwarding descriptor and name
    fn parse_to_desc(&self, name: &str, curr_desc: &FwdMethods) -> FieldDesc {
        FieldDesc {
            ident: self.ident.clone(),
            fwd: self.fwds.element_desc(name, curr_desc).into_owned(),
            ty: self.ty.clone(),
        }
    }
}

impl FieldDesc {
    fn infer_bounds(&self, bounds: &mut EquivalenceBounds, type_set: &IdentSet, ctx: &Type) {
        self.fwd.infer_bounds(&self.ty, bounds, type_set, ctx)
    }
}

#[derive(Debug, Clone)]
struct VariantDesc {
    ident: Ident,
    fields: darling::ast::Fields<FieldDesc>,
}

impl VariantDesc {
    fn infer_bounds(&self, bounds: &mut EquivalenceBounds, type_set: &IdentSet, ctx: &Type) {
        for field in self.fields.iter() {
            field.infer_bounds(bounds, type_set, ctx)
        }
    }
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
    fn infer_bounds(
        &self,
        ty: &Type,
        bounds: &mut EquivalenceBounds,
        type_set: &IdentSet,
        ctx: &Type,
    ) {
        let methods = [
            (
                EquivalenceTrait::PartialEqWith,
                self.eq.as_ref(),
                bounds.partial_eq.as_mut(),
            ),
            (
                EquivalenceTrait::EqWith,
                self.eq.as_ref(),
                bounds.eq.as_mut(),
            ),
            (
                EquivalenceTrait::PartialOrdWith,
                self.partial_cmp.as_ref(),
                bounds.partial_ord.as_mut(),
            ),
            (
                EquivalenceTrait::OrdWith,
                self.cmp.as_ref(),
                bounds.ord.as_mut(),
            ),
            (
                EquivalenceTrait::HashWith,
                self.hash.as_ref(),
                bounds.hash.as_mut(),
            ),
        ];
        for (tr, method, bounds) in methods.into_iter() {
            method.map(|eq| bounds.map(|bounds| eq.infer_bounds(tr, ty, bounds, type_set, ctx)));
        }
    }
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

#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) enum EquivalenceTrait {
    PartialEqWith,
    EqWith,
    PartialOrdWith,
    OrdWith,
    HashWith,
}

impl ToTokens for EquivalenceTrait {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            EquivalenceTrait::PartialEqWith => {
                tokens.extend(quote! { ::equivalence::PartialEqWith })
            }
            EquivalenceTrait::EqWith => tokens.extend(quote! { ::equivalence::EqWith }),
            EquivalenceTrait::PartialOrdWith => {
                tokens.extend(quote! { ::equivalence::PartialOrdWith })
            }
            EquivalenceTrait::OrdWith => tokens.extend(quote! { ::equivalence::OrdWith }),
            EquivalenceTrait::HashWith => tokens.extend(quote! { ::equivalence::HashWith }),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
struct AssociatedTrait(EquivalenceTrait);

impl ToTokens for AssociatedTrait {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self.0 {
            EquivalenceTrait::PartialEqWith => tokens.extend(quote! { PartialEq }),
            EquivalenceTrait::EqWith => tokens.extend(quote! { Eq }),
            EquivalenceTrait::PartialOrdWith => tokens.extend(quote! { PartialOrd }),
            EquivalenceTrait::OrdWith => tokens.extend(quote! { Ord }),
            EquivalenceTrait::HashWith => tokens.extend(quote! { ::core::hash::Hash }),
        }
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

impl FwdMethod {
    fn infer_bounds(
        &self,
        tr: EquivalenceTrait,
        ty: &Type,
        bounds: &mut ImplBounds,
        type_set: &IdentSet,
        ctx: &Type,
    ) {
        if !bounds.infer {
            return;
        }
        // NOTE: we do not infer for map/maprec by default!
        match self {
            FwdMethod::Rec => {
                for ty in ty.uses_type_params(&Purpose::BoundImpl.into(), type_set) {
                    bounds
                        .clause
                        .predicates
                        .push(syn::parse_quote!(#ty: #tr<#ctx>))
                }
            }
            FwdMethod::Delegate => {
                let tr = AssociatedTrait(tr);
                for ty in ty.uses_type_params(&Purpose::BoundImpl.into(), type_set) {
                    bounds.clause.predicates.push(syn::parse_quote!(#ty: #tr))
                }
            }
            _ => {}
        }
    }
}

pub(crate) fn true_where_clause(span: Span) -> WhereClause {
    WhereClause {
        where_token: Where { span },
        predicates: Punctuated::default(),
    }
}
