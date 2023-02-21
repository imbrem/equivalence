use super::*;

//TODO: fix this to be actually clause-based

#[derive(Debug, Clone)]
pub(crate) struct EquivalenceTraits {
    pub(crate) partial_eq: Option<WhereClause>,
    pub(crate) eq: Option<WhereClause>,
    pub(crate) partial_ord: Option<WhereClause>,
    pub(crate) ord: Option<WhereClause>,
    pub(crate) hash: Option<WhereClause>,
}

impl EquivalenceTraits {
    pub(crate) fn compute_base_traits(
        full: impl FlagSet,
        partial_eq: impl FlagSet,
        eq: impl FlagSet,
        partial_ord: impl FlagSet,
        ord: impl FlagSet,
        hash: impl FlagSet,
        warn_redundant: bool,
    ) -> Result<Option<EquivalenceTraits>, darling::Error> {
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
                    Ok(Some(EquivalenceTraits {
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
                Ok(Some(EquivalenceTraits {
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
    ) -> Result<EquivalenceTraits, darling::Error> {
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
            .unwrap_or_else(|| EquivalenceTraits::all_empty(Span::call_site())))
        }
    }
}
