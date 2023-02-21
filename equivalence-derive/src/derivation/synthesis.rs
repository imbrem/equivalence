use super::*;

impl ContextDerivation {
    /// Synthesize code for a derivation
    pub(crate) fn synthesize(&self, result: &mut TokenStream2) {
        if let Some(clause) = &self.traits.partial_eq {
            self.synthesize_partial_eq(clause, result)
        }
        if let Some(clause) = &self.traits.eq {
            self.synthesize_eq(clause, result)
        }
        let delegate_partial = if let Some(ord_clause) = &self.traits.ord {
            //TODO: fix this once has_partial_spec is added
            let delegate_partial = /*if let Some(partial_ord_clause) = &self.traits.partial_ord {
            ord_clause == partial_ord_clause
        } else */{
            false
        };
            self.synthesize_cmp(ord_clause, delegate_partial, result);
            delegate_partial
        } else {
            false
        };
        if !delegate_partial {
            if let Some(clause) = &self.traits.partial_ord {
                self.synthesize_partial_cmp(clause, result);
            }
        }
        if let Some(clause) = &self.traits.hash {
            self.synthesize_hash(clause, result)
        }
    }

    /// Synthesize code for partial equality, given a where clause
    fn synthesize_partial_eq(&self, clause: &WhereClause, result: &mut TokenStream2) {
        let ty = &self.this;
        let ctx = &self.ctx;
        let imp = self.synthesize_binary_trait_impl(BinaryTrait::PartialEq);
        result.extend(quote! {
            impl ::equivalence::PartialEqWith<#ctx> for #ty #clause {
                fn eq_with(&self, other: &Self, ctx: &#ctx) -> bool {
                    #imp
                }
            }
        })
    }

    /// Synthesize code for equality, given a where clause
    fn synthesize_eq(&self, clause: &WhereClause, result: &mut TokenStream2) {
        let ty = &self.this;
        let ctx = &self.ctx;
        result.extend(quote! {
            impl ::equivalence::EqWith<#ctx> for #ty #clause {}
        })
    }

    /// Synthesize code for partial comparison, given a where clause
    fn synthesize_partial_cmp(&self, clause: &WhereClause, result: &mut TokenStream2) {
        let ty = &self.this;
        let ctx = &self.ctx;
        let imp = self.synthesize_binary_trait_impl(BinaryTrait::PartialOrd);
        result.extend(quote! {
        impl ::equivalence::PartialOrdWith<#ctx> for #ty #clause {
            fn partial_cmp_with(&self, other: &Self, ctx: &#ctx) -> Option<::core::cmp::Ordering> {
                #imp
            }
        }
    })
    }

    /// Synthesize code for comparison, given a where clause
    ///
    /// If `delegate_partial` is true, generates partial comparison code with the same where clause as well
    fn synthesize_cmp(
        &self,
        clause: &WhereClause,
        delegate_partial: bool,
        result: &mut TokenStream2,
    ) {
        if delegate_partial {
            //TODO: fix this
            abort! {
                Span::call_site(),
                "partial delegation not yet implemented"
            }
        }
        let ty = &self.this;
        let ctx = &self.ctx;
        let imp = self.synthesize_binary_trait_impl(BinaryTrait::Ord);
        result.extend(quote! {
            impl ::equivalence::OrdWith<#ctx> for #ty #clause {
                fn cmp_with(&self, other: &Self, ctx: &#ctx) -> ::core::cmp::Ordering {
                    #imp
                }
            }
        })
    }

    /// Synthesize code for hashing, given a where clause
    fn synthesize_hash(&self, clause: &WhereClause, result: &mut TokenStream2) {
        let ty = &self.this;
        let ctx = &self.ctx;
        let imp = self.synthesize_hash_impl();
        result.extend(quote! {
            impl ::equivalence::HashWith<#ctx> for #ty #clause {
                fn hash_with<H: ::core::hash::Hasher>(&self, hasher: &mut H, ctx: &#ctx) {
                    use ::core::hash::Hash;
                    #imp
                }
            }
        })
    }

    fn synthesize_binary_trait_impl(&self, tr: BinaryTrait) -> TokenStream2 {
        let mut result = quote! {};
        let this = quote! { self };
        let other = quote! { other };
        let ctx = quote! { ctx };
        let ident = &self.ident;
        match &self.data {
            Data::Enum(e) => {
                let mut match_block = quote! {};
                for v in e {
                    let variant = &v.ident;
                    let mut left_args = quote! {};
                    let mut right_args = quote! {};
                    let fields = v.fields.iter().enumerate().map(|(i, f)| {
                        let (left_arg_name, right_arg_name) = if let Some(ident) = &f.ident {
                            (
                                ident.clone(),
                                Ident::new(&(ident.to_string() + "_"), Span::call_site()),
                            )
                        } else {
                            (
                                Ident::new(&format!("x{i}"), Span::call_site()),
                                Ident::new(&format!("y{i}"), Span::call_site()),
                            )
                        };
                        left_args.extend(quote! { #left_arg_name, });
                        if let Some(ident) = &f.ident {
                            right_args.extend(quote! { #ident: #right_arg_name, })
                        } else {
                            right_args.extend(quote! { #right_arg_name, })
                        }

                        let field = f.fwd.synthesize_binary_trait(
                            quote! { #left_arg_name },
                            quote! { #right_arg_name },
                            &ctx,
                            tr,
                        );
                        field
                    });

                    let br_result = tr.fuse(fields);

                    match v.fields.style {
                    darling::ast::Style::Unit => {
                        match_block.extend(quote! {(#ident::#variant, #ident::#variant) => #br_result,})
                    }
                    darling::ast::Style::Struct => match_block.extend(quote! {
                        (#ident::#variant{#left_args}, #ident::#variant{#right_args}) => #br_result,
                    }),
                    darling::ast::Style::Tuple => match_block.extend(quote! {
                        (#ident::#variant(#left_args), #ident::#variant(#right_args)) => #br_result,
                    }),
                }
                }
                let post_match = tr.synthesize_post_match(ident, e);
                result.extend(quote! { match (self, other) { #match_block #post_match } })
            }
            Data::Struct(s) => {
                let fields = s.iter().enumerate().map(|(i, f)| {
                    f.synthesize_binary_trait(this.clone(), other.clone(), &ctx, i as u32, tr)
                });
                result.extend(tr.fuse(fields));
            }
        }
        result
    }

    fn synthesize_hash_impl(&self) -> TokenStream2 {
        let mut result = quote! {};
        let this = quote! { self };
        let hasher = quote! { hasher };
        let ctx = quote! { ctx };
        let ident = &self.ident;
        match &self.data {
            Data::Enum(e) => {
                let mut match_block = quote! {};
                for v in e {
                    let variant = &v.ident;
                    let mut args = quote! {};
                    let fields = v.fields.iter().enumerate().map(|(i, f)| {
                        let arg_name = if let Some(ident) = &f.ident {
                            ident.clone()
                        } else {
                            Ident::new(&format!("x{i}"), Span::call_site())
                        };
                        args.extend(quote! { #arg_name, });

                        let field = f.fwd.synthesize_hash(quote! { #arg_name }, &hasher, &ctx);
                        field
                    });

                    let br_result = fuse_hash(fields);

                    match v.fields.style {
                        darling::ast::Style::Unit => {
                            match_block.extend(quote! {#ident::#variant => #br_result,})
                        }
                        darling::ast::Style::Struct => match_block.extend(quote! {
                            #ident::#variant{#args} => #br_result,
                        }),
                        darling::ast::Style::Tuple => match_block.extend(quote! {
                            #ident::#variant(#args) => #br_result,
                        }),
                    }
                }
                result.extend(quote! {
                    ::core::mem::discriminant(self).hash(hasher); match self { #match_block }
                })
            }
            Data::Struct(s) => {
                let fields = s
                    .iter()
                    .enumerate()
                    .map(|(i, f)| f.synthesize_hash(this.clone(), &hasher, &ctx, i as u32));
                result.extend(fuse_hash(fields));
            }
        }
        result
    }
}

impl FieldDesc {
    fn synthesize_binary_trait(
        &self,
        this: TokenStream2,
        other: TokenStream2,
        ctx: &TokenStream2,
        index: u32,
        tr: BinaryTrait,
    ) -> TokenStream2 {
        let member = self
            .ident
            .clone()
            .map(Member::Named)
            .unwrap_or(Member::Unnamed(Index {
                index,
                span: Span::call_site(),
            }));
        self.fwd.synthesize_binary_trait(
            quote! { #this.#member },
            quote! { #other.#member },
            ctx,
            tr,
        )
    }

    fn synthesize_hash(
        &self,
        this: TokenStream2,
        hasher: &TokenStream2,
        ctx: &TokenStream2,
        index: u32,
    ) -> TokenStream2 {
        let member = self
            .ident
            .clone()
            .map(Member::Named)
            .unwrap_or(Member::Unnamed(Index {
                index,
                span: Span::call_site(),
            }));
        self.fwd
            .synthesize_hash(quote! { #this.#member }, hasher, ctx)
    }
}

impl FwdDesc {
    fn synthesize_binary_trait(
        &self,
        this: TokenStream2,
        other: TokenStream2,
        ctx: &TokenStream2,
        tr: BinaryTrait,
    ) -> TokenStream2 {
        match tr {
            BinaryTrait::PartialEq => self.synthesize_partial_eq(this, other, ctx),
            BinaryTrait::PartialOrd => self.synthesize_partial_cmp(this, other, ctx),
            BinaryTrait::Ord => self.synthesize_cmp(this, other, ctx),
        }
    }

    fn synthesize_partial_eq(
        &self,
        this: TokenStream2,
        other: TokenStream2,
        ctx: &TokenStream2,
    ) -> TokenStream2 {
        match &self.eq {
            Some(FwdMethod::Ignore) => quote! { true },
            Some(FwdMethod::Delegate) => quote! { #this == #other },
            Some(FwdMethod::Rec) => {
                quote! { #this.eq_with(&#other, #ctx) }
            }
            Some(FwdMethod::Map(map)) => {
                quote! { (#map)(&#this, #ctx) == (#map)(&#other, #ctx) }
            }
            Some(FwdMethod::MapRec(map)) => {
                quote! { (#map)(&#this, #ctx).eq_with(&(#map)(&#other, #ctx), #ctx) }
            }
            Some(FwdMethod::WithFn(eq)) => {
                quote! { (#eq)(&#this, &#other, #ctx) }
            }
            // Some(EqMethod::WithPartialCmp(cmp)) => {
            //     quote! { && (#cmp)(&#this, &#other, #ctx) == Some(::core::cmp::Ordering::Equal)s }
            // }
            // Some(EqMethod::WithCmp(cmp)) => {
            //     quote! { && (#cmp)(&#this, &#other, #ctx) == ::core::cmp::Ordering::Equal }
            // }
            _ => abort!(
                Span::call_site(),
                "internal error: unable to determine method to use for PartialEqWith implementation. This is a bug and should be reported."
            ),
        }
    }

    fn synthesize_partial_cmp(
        &self,
        this: TokenStream2,
        other: TokenStream2,
        ctx: &TokenStream2,
    ) -> TokenStream2 {
        match &self.partial_cmp {
            Some(FwdMethod::Ignore) => quote! { Some(::core::cmp::Ordering::Equal) },
            Some(FwdMethod::Delegate) => quote! { #this.partial_cmp(&#other) },
            Some(FwdMethod::Rec) => {
                quote! { #this.partial_cmp_with(&#other, #ctx) }
            }
            Some(FwdMethod::Map(map)) => {
                quote! {(#map)(&#this, #ctx).partial_cmp(&(#map)(&#other, #ctx)) }
            }
            Some(FwdMethod::MapRec(map)) => {
                quote! { (#map)(&#this, #ctx).partial_cmp_with(&(#map)(&#other, #ctx), #ctx) }
            }
            Some(FwdMethod::WithFn(cmp)) => {
                quote! { (#cmp)(&#this, &#other, #ctx) }
            }
            _ => abort!(
                Span::call_site(),
                "internal error: unable to determine method to use for PartialOrdWith implementation. This is a bug and should be reported."
            ),
        }
    }

    fn synthesize_cmp(
        &self,
        this: TokenStream2,
        other: TokenStream2,
        ctx: &TokenStream2,
    ) -> TokenStream2 {
        match &self.cmp {
            Some(FwdMethod::Ignore) => quote! { ::core::cmp::Ordering::Equal },
            Some(FwdMethod::Delegate) => quote! { #this.cmp(&#other) },
            Some(FwdMethod::Rec) => {
                quote! { #this.cmp_with(&#other, #ctx) }
            }
            Some(FwdMethod::Map(map)) => {
                quote! {(#map)(&#this, #ctx).cmp(&(#map)(&#other, #ctx)) }
            }
            Some(FwdMethod::MapRec(map)) => {
                quote! { (#map)(&#this, #ctx).cmp_with(&(#map)(&#other, #ctx), #ctx) }
            }
            Some(FwdMethod::WithFn(cmp)) => {
                quote! { (#cmp)(&#this, &#other, #ctx) }
            }
            _ => abort!(
                Span::call_site(),
                "internal error: unable to determine method to use for OrdWith implementation. This is a bug and should be reported."
            ),
        }
    }

    fn synthesize_hash(
        &self,
        this: TokenStream2,
        hasher: &TokenStream2,
        ctx: &TokenStream2,
    ) -> TokenStream2 {
        match &self.hash {
            Some(FwdMethod::Ignore) => quote! {},
            Some(FwdMethod::Delegate) => quote! { #this.hash(#hasher) },
            Some(FwdMethod::Rec) => {
                quote! { #this.hash_with(#hasher, #ctx) }
            }
            Some(FwdMethod::Map(map)) => {
                quote! {(#map)(&#this, #ctx).hash(#hasher) }
            }
            Some(FwdMethod::MapRec(map)) => {
                quote! { (#map)(&#this, #ctx).hash_with(#hasher, #ctx) }
            }
            Some(FwdMethod::WithFn(hash)) => {
                quote! { (#hash)(&#this, #hasher, #ctx) }
            }
            _ => abort!(
                Span::call_site(),
                "internal error: unable to determine method to use for HashWith implementation. This is a bug and should be reported."
            ),
        }
    }
}

#[derive(Debug, Copy, Clone)]
enum BinaryTrait {
    PartialEq,
    PartialOrd,
    Ord,
}

impl BinaryTrait {
    fn fuse(&self, fields: impl IntoIterator<Item = TokenStream2>) -> TokenStream2 {
        match self {
            BinaryTrait::PartialEq => Self::fuse_partial_eq(fields),
            BinaryTrait::PartialOrd => Self::fuse_partial_cmp(fields),
            BinaryTrait::Ord => Self::fuse_cmp(fields),
        }
    }

    fn fuse_partial_eq(fields: impl IntoIterator<Item = TokenStream2>) -> TokenStream2 {
        let mut fields = fields.into_iter();
        if let Some(mut result) = fields.next() {
            for term in fields {
                result.extend(quote! { && #term })
            }
            result
        } else {
            quote! { true }
        }
    }

    fn fuse_partial_cmp(fields: impl IntoIterator<Item = TokenStream2>) -> TokenStream2 {
        let mut result = quote! {};
        for field in fields {
            result.extend(quote! {
                let result = #field;
                if result != Some(::core::cmp::Ordering::Equal) {
                    return result
                }
            });
        }
        quote! { { #result; Some(::core::cmp::Ordering::Equal) } }
    }

    fn fuse_cmp(fields: impl IntoIterator<Item = TokenStream2>) -> TokenStream2 {
        let mut result = quote! {};
        for field in fields {
            result.extend(quote! {
                let result = #field;
                if result != ::core::cmp::Ordering::Equal {
                    return result
                }
            });
        }
        quote! { { #result; ::core::cmp::Ordering::Equal } }
    }

    fn synthesize_post_match(&self, ident: &Ident, variants: &[VariantDesc]) -> TokenStream2 {
        match self {
            BinaryTrait::PartialEq => Self::synthesize_post_match_partial_eq(ident, variants),
            BinaryTrait::PartialOrd => Self::synthesize_post_match_partial_cmp(ident, variants),
            BinaryTrait::Ord => Self::synthesize_post_match_cmp(ident, variants),
        }
    }

    fn synthesize_post_match_partial_eq(_ident: &Ident, _variants: &[VariantDesc]) -> TokenStream2 {
        quote! { _ => false }
    }

    fn synthesize_post_match_partial_cmp(ident: &Ident, variants: &[VariantDesc]) -> TokenStream2 {
        let mut result = quote! {};
        for variant in variants {
            let variant_name = &variant.ident;
            match variant.fields.style {
                darling::ast::Style::Tuple => result.extend(quote! {
                    (#ident::#variant_name(..), _) => Some(::core::cmp::Ordering::Less),
                    (_, #ident::#variant_name(..)) => Some(::core::cmp::Ordering::Greater),
                }),
                darling::ast::Style::Struct => result.extend(quote! {
                    (#ident::#variant_name{..}, _) => Some(::core::cmp::Ordering::Less),
                    (_, #ident::#variant_name{..}) => Some(::core::cmp::Ordering::Greater),
                }),
                darling::ast::Style::Unit => result.extend(quote! {
                    (#ident::#variant_name, _) => Some(::core::cmp::Ordering::Less),
                    (_, #ident::#variant_name) => Some(::core::cmp::Ordering::Greater),
                }),
            }
        }
        result
    }

    fn synthesize_post_match_cmp(ident: &Ident, variants: &[VariantDesc]) -> TokenStream2 {
        let mut result = quote! {};
        for variant in variants {
            let variant_name = &variant.ident;
            match variant.fields.style {
                darling::ast::Style::Tuple => result.extend(quote! {
                    (#ident::#variant_name(..), _) => ::core::cmp::Ordering::Less,
                    (_, #ident::#variant_name(..)) => ::core::cmp::Ordering::Greater,
                }),
                darling::ast::Style::Struct => result.extend(quote! {
                    (#ident::#variant_name{..}, _) => ::core::cmp::Ordering::Less,
                    (_, #ident::#variant_name{..}) => ::core::cmp::Ordering::Greater,
                }),
                darling::ast::Style::Unit => result.extend(quote! {
                    (#ident::#variant_name, _) => ::core::cmp::Ordering::Less,
                    (_, #ident::#variant_name) => ::core::cmp::Ordering::Greater,
                }),
            }
        }
        result
    }
}

fn fuse_hash(fields: impl IntoIterator<Item = TokenStream2>) -> TokenStream2 {
    let mut inner = quote! {};
    for field in fields {
        inner.extend(quote! { #field; })
    }
    quote! { { #inner } }
}
