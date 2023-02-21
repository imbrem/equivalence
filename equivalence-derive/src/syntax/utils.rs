use super::*;

#[derive(Debug, Clone)]
pub(crate) enum WhereFlag {
    Clause(WhereClause),
    Set(bool, Span),
}

#[derive(Debug, Copy, Clone)]
pub(crate) struct SetFlag(bool);

impl FromMeta for SetFlag {
    fn from_word() -> darling::Result<Self> {
        Ok(SetFlag(true))
    }

    fn from_bool(value: bool) -> darling::Result<Self> {
        Ok(SetFlag(value))
    }
}

impl FromMeta for WhereFlag {
    fn from_word() -> darling::Result<Self> {
        Ok(WhereFlag::Set(true, Span::call_site()))
    }

    fn from_string(value: &str) -> darling::Result<Self> {
        WhereClause::from_string(value).map(WhereFlag::Clause)
    }

    fn from_bool(value: bool) -> darling::Result<Self> {
        Ok(WhereFlag::Set(value, Span::call_site()))
    }

    fn from_meta(item: &syn::Meta) -> darling::Result<Self> {
        let mut result = (match *item {
            syn::Meta::Path(_) => Self::from_word(),
            syn::Meta::List(ref value) => Self::from_list(
                &value
                    .nested
                    .iter()
                    .cloned()
                    .collect::<Vec<syn::NestedMeta>>()[..],
            ),
            syn::Meta::NameValue(ref value) => Self::from_value(&value.lit),
        })
        .map_err(|e| e.with_span(item))?;
        if let WhereFlag::Set(_, span) = &mut result {
            *span = item.span()
        }
        Ok(result)
    }
}

pub(crate) fn true_where_clause(span: Span) -> WhereClause {
    WhereClause {
        where_token: Where { span },
        predicates: Punctuated::default(),
    }
}

pub(crate) trait FlagSet: Sized {
    fn flag_set(&self) -> Option<bool>;

    fn where_span(&self) -> Span;

    fn is_set_true(&self) -> bool {
        self.flag_set() == Some(true)
    }

    fn into_where_flag(self) -> Option<WhereFlag> {
        self.flag_set()
            .map(|v| WhereFlag::Set(v, Span::call_site()))
    }

    fn into_where_clause(self) -> Option<WhereClause> {
        self.into_where_flag()?.into_where_clause()
    }
}

impl FlagSet for Flag {
    fn flag_set(&self) -> Option<bool> {
        if self.is_present() {
            Some(true)
        } else {
            None
        }
    }

    fn where_span(&self) -> Span {
        self.span()
    }
}

impl FlagSet for WhereFlag {
    fn flag_set(&self) -> Option<bool> {
        match self {
            WhereFlag::Clause(_) => Some(true),
            WhereFlag::Set(set, _) => Some(*set),
        }
    }

    fn where_span(&self) -> Span {
        match self {
            WhereFlag::Clause(c) => c.span(),
            WhereFlag::Set(_, span) => *span,
        }
    }

    fn is_set_true(&self) -> bool {
        match self {
            WhereFlag::Clause(_) => false,
            WhereFlag::Set(set, _) => *set,
        }
    }

    fn into_where_flag(self) -> Option<WhereFlag> {
        Some(self)
    }

    fn into_where_clause(self) -> Option<WhereClause> {
        match self {
            WhereFlag::Clause(clause) => Some(clause),
            WhereFlag::Set(set, span) => {
                if set {
                    Some(true_where_clause(span))
                } else {
                    None
                }
            }
        }
    }
}

impl<T> FlagSet for Option<T>
where
    T: FlagSet,
{
    fn flag_set(&self) -> Option<bool> {
        self.as_ref()?.flag_set()
    }

    fn where_span(&self) -> Span {
        if let Some(this) = self {
            this.where_span()
        } else {
            Span::call_site()
        }
    }

    fn is_set_true(&self) -> bool {
        if let Some(this) = self {
            this.is_set_true()
        } else {
            false
        }
    }

    fn into_where_flag(self) -> Option<WhereFlag> {
        self?.into_where_flag()
    }
}

impl FlagSet for SetFlag {
    fn flag_set(&self) -> Option<bool> {
        Some(self.0)
    }

    fn where_span(&self) -> Span {
        Span::call_site()
    }
}

impl FlagSet for SpannedValue<SetFlag> {
    fn flag_set(&self) -> Option<bool> {
        Some(self.0)
    }

    fn where_span(&self) -> Span {
        self.span()
    }
}
