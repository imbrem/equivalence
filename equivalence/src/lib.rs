use std::borrow::Cow;
use std::hash::Hash;
use std::num::{
    NonZeroI128, NonZeroI16, NonZeroI32, NonZeroI64, NonZeroI8, NonZeroIsize, NonZeroU128,
    NonZeroU16, NonZeroU32, NonZeroU64, NonZeroU8, NonZeroUsize,
};
// extern crate self as equivalence;
// use equivalence_derive::Equivalence;d
use std::rc::Rc;
use std::sync::Arc;
use std::{cmp::Ordering, hash::Hasher};

use either::{for_both, Either};

pub use equivalence_derive::Equivalence;

#[macro_export]
macro_rules! pod_partial_eq_with {
    ($ty:ty) => {
        impl<C: ?Sized, T: ?Sized> PartialEqWith<C, T> for $ty
        where
            $ty: PartialEq<T>,
        {
            #[inline(always)]
            fn eq_with(&self, other: &T, _ctx: &C) -> bool {
                *self == *other
            }
        }
    };
}

#[macro_export]
macro_rules! pod_eq_with {
    ($ty:ty) => {
        $crate::pod_partial_eq_with!($ty);
        impl<C: ?Sized> EqWith<C> for $ty where $ty: Eq {}
    };
}

#[macro_export]
macro_rules! pod_partial_ord_with {
    ($ty:ty) => {
        impl<C: ?Sized, T: ?Sized> PartialOrdWith<C, T> for $ty
        where
            $ty: PartialOrd<T>,
        {
            #[inline(always)]
            fn partial_cmp_with(&self, other: &T, _ctx: &C) -> Option<Ordering> {
                self.partial_cmp(other)
            }
        }
    };
}

#[macro_export]
macro_rules! pod_ord_with {
    ($ty:ty) => {
        $crate::pod_partial_ord_with!($ty);

        impl<C: ?Sized> OrdWith<C> for $ty
        where
            $ty: Ord,
        {
            #[inline(always)]
            fn cmp_with(&self, other: &$ty, _ctx: &C) -> Ordering {
                self.cmp(other)
            }
        }
    };
}

#[macro_export]
macro_rules! pod_hash_with {
    ($ty:ty) => {
        impl<C: ?Sized> HashWith<C> for $ty
        where
            $ty: std::hash::Hash,
        {
            #[inline(always)]
            fn hash_with<H: std::hash::Hasher>(&self, hasher: &mut H, _ctx: &C) {
                use std::hash::Hash;
                self.hash(hasher)
            }
        }
    };
}

#[macro_export]
macro_rules! pod_equiv {
    ($ty:ty) => {
        $crate::pod_eq_with!($ty);
        $crate::pod_ord_with!($ty);
        $crate::pod_hash_with!($ty);
    };
}

/// Values equipped with a partial equivalence relation modulo a context of type `C`
pub trait PartialEqWith<C: ?Sized, T: ?Sized = Self> {
    /// Check whether `self` and `other` are equal modulo the context `ctx`
    fn eq_with(&self, other: &T, ctx: &C) -> bool;
    /// Check whether `self` and `other` are disequal modulo the context `ctx`
    fn ne_with(&self, other: &T, ctx: &C) -> bool {
        !self.eq_with(other, ctx)
    }
}

/// Values equipped with an equivalence relation modulo contexts of type `C`
pub trait EqWith<C: ?Sized>: PartialEqWith<C> {}

/// Values equipped with a partial ordering modulo a context of type `C`
pub trait PartialOrdWith<C: ?Sized, T: ?Sized = Self>: PartialEqWith<C, T> {
    /// Compare `self` and `other` modulo the context `ctx`
    fn partial_cmp_with(&self, other: &T, ctx: &C) -> Option<Ordering>;
}

/// Values equipped with an ordering modulo contexts of type `C`
pub trait OrdWith<C: ?Sized>: PartialOrdWith<C, Self> + EqWith<C> {
    /// Compare `self` and `other` modulo the context `ctx`
    fn cmp_with(&self, other: &Self, ctx: &C) -> Ordering;
}

/// Values which can be hashed modulo a context of type `C`
///
/// If `PartialEqWith<C, T>` is implemented, it is expected that equivalent values have the same hashing behaviour.
pub trait HashWith<C: ?Sized, T: ?Sized = Self> {
    /// Hash `self` modulo the context `ctx`
    fn hash_with<H: Hasher>(&self, hasher: &mut H, ctx: &C);
}

impl<A, B, C> PartialEqWith<C, &B> for &A
where
    A: PartialEqWith<C, B>,
{
    #[inline(always)]
    fn eq_with(&self, other: &&B, ctx: &C) -> bool {
        (*self).eq_with(*other, ctx)
    }
}

impl<A, C> EqWith<C> for &A where A: EqWith<C> {}

impl<A, B, C> PartialOrdWith<C, &B> for &A
where
    A: PartialOrdWith<C, B>,
{
    #[inline(always)]
    fn partial_cmp_with(&self, other: &&B, ctx: &C) -> Option<Ordering> {
        (*self).partial_cmp_with(*other, ctx)
    }
}

impl<A, C> OrdWith<C> for &A
where
    A: OrdWith<C>,
{
    #[inline(always)]
    fn cmp_with(&self, other: &&A, ctx: &C) -> Ordering {
        (*self).cmp_with(*other, ctx)
    }
}

impl<A, C> HashWith<C> for &A
where
    A: HashWith<C>,
{
    #[inline(always)]
    fn hash_with<H: Hasher>(&self, hasher: &mut H, ctx: &C) {
        (*self).hash_with(hasher, ctx)
    }
}

impl<A, B, C> PartialEqWith<C, &mut B> for &mut A
where
    A: PartialEqWith<C, B>,
{
    #[inline(always)]
    fn eq_with(&self, other: &&mut B, ctx: &C) -> bool {
        (**self).eq_with(*other, ctx)
    }
}

impl<A, C> EqWith<C> for &mut A where A: EqWith<C> {}

impl<A, B, C> PartialOrdWith<C, &mut B> for &mut A
where
    A: PartialOrdWith<C, B>,
{
    #[inline(always)]
    fn partial_cmp_with(&self, other: &&mut B, ctx: &C) -> Option<Ordering> {
        (**self).partial_cmp_with(*other, ctx)
    }
}

impl<A, C> OrdWith<C> for &mut A
where
    A: OrdWith<C>,
{
    #[inline(always)]
    fn cmp_with(&self, other: &&mut A, ctx: &C) -> Ordering {
        (**self).cmp_with(*other, ctx)
    }
}

impl<A, C> HashWith<C> for &mut A
where
    A: HashWith<C>,
{
    #[inline(always)]
    fn hash_with<H: Hasher>(&self, hasher: &mut H, ctx: &C) {
        (**self).hash_with(hasher, ctx)
    }
}

impl<A, B, C> PartialEqWith<C, Box<B>> for Box<A>
where
    A: PartialEqWith<C, B>,
{
    #[inline(always)]
    fn eq_with(&self, other: &Box<B>, ctx: &C) -> bool {
        (**self).eq_with(&**other, ctx)
    }
}

impl<A, C> EqWith<C> for Box<A> where A: EqWith<C> {}

impl<A, B, C> PartialOrdWith<C, Box<B>> for Box<A>
where
    A: PartialOrdWith<C, B>,
{
    #[inline(always)]
    fn partial_cmp_with(&self, other: &Box<B>, ctx: &C) -> Option<Ordering> {
        (**self).partial_cmp_with(&**other, ctx)
    }
}

impl<A, C> OrdWith<C> for Box<A>
where
    A: OrdWith<C>,
{
    #[inline(always)]
    fn cmp_with(&self, other: &Box<A>, ctx: &C) -> Ordering {
        (**self).cmp_with(&**other, ctx)
    }
}

impl<A, C> HashWith<C> for Box<A>
where
    A: HashWith<C>,
{
    #[inline(always)]
    fn hash_with<H: Hasher>(&self, hasher: &mut H, ctx: &C) {
        (**self).hash_with(hasher, ctx)
    }
}

impl<A, B, C> PartialEqWith<C, [B]> for [A]
where
    A: PartialEqWith<C, B>,
{
    fn eq_with(&self, other: &[B], ctx: &C) -> bool {
        self.len() == other.len()
            && self
                .iter()
                .zip(other.iter())
                .all(|(a, b)| a.eq_with(b, ctx))
    }
}

impl<A, C> EqWith<C> for [A] where A: EqWith<C> {}

impl<A, B, C> PartialOrdWith<C, [B]> for [A]
where
    A: PartialOrdWith<C, B>,
{
    fn partial_cmp_with(&self, other: &[B], ctx: &C) -> Option<Ordering> {
        for (a, b) in self.iter().zip(other.iter()) {
            match a.partial_cmp_with(b, ctx) {
                Some(Ordering::Equal) => {}
                o => return o,
            }
        }
        Some(self.len().cmp(&other.len()))
    }
}

impl<A, C> OrdWith<C> for [A]
where
    A: OrdWith<C>,
{
    fn cmp_with(&self, other: &[A], ctx: &C) -> Ordering {
        for (a, b) in self.iter().zip(other.iter()) {
            match a.cmp_with(b, ctx) {
                Ordering::Equal => {}
                o => return o,
            }
        }
        self.len().cmp(&other.len())
    }
}

impl<A, C> HashWith<C> for [A]
where
    A: HashWith<C>,
{
    fn hash_with<H: Hasher>(&self, hasher: &mut H, ctx: &C) {
        for a in self.iter() {
            a.hash_with(hasher, ctx)
        }
    }
}

impl<A, B, C> PartialEqWith<C, Vec<B>> for Vec<A>
where
    A: PartialEqWith<C, B>,
{
    #[inline(always)]
    fn eq_with(&self, other: &Vec<B>, ctx: &C) -> bool {
        self[..].eq_with(&other[..], ctx)
    }
}

impl<A, C> EqWith<C> for Vec<A> where A: EqWith<C> {}

impl<A, B, C> PartialOrdWith<C, Vec<B>> for Vec<A>
where
    A: PartialOrdWith<C, B>,
{
    #[inline(always)]
    fn partial_cmp_with(&self, other: &Vec<B>, ctx: &C) -> Option<Ordering> {
        self[..].partial_cmp_with(&other[..], ctx)
    }
}

impl<A, C> OrdWith<C> for Vec<A>
where
    A: OrdWith<C>,
{
    #[inline(always)]
    fn cmp_with(&self, other: &Vec<A>, ctx: &C) -> Ordering {
        self[..].cmp_with(&other[..], ctx)
    }
}

impl<A, C> HashWith<C> for Vec<A>
where
    A: HashWith<C>,
{
    fn hash_with<H: Hasher>(&self, hasher: &mut H, ctx: &C) {
        self[..].hash_with(hasher, ctx)
    }
}

impl<const N: usize, A, B, C> PartialEqWith<C, [B; N]> for [A; N]
where
    A: PartialEqWith<C, B>,
{
    #[inline(always)]
    fn eq_with(&self, other: &[B; N], ctx: &C) -> bool {
        self[..].eq_with(&other[..], ctx)
    }
}

impl<const N: usize, A, C> EqWith<C> for [A; N] where A: EqWith<C> {}

impl<const N: usize, A, B, C> PartialOrdWith<C, [B; N]> for [A; N]
where
    A: PartialOrdWith<C, B>,
{
    #[inline(always)]
    fn partial_cmp_with(&self, other: &[B; N], ctx: &C) -> Option<Ordering> {
        self[..].partial_cmp_with(&other[..], ctx)
    }
}

impl<const N: usize, A, C> OrdWith<C> for [A; N]
where
    A: OrdWith<C>,
{
    #[inline(always)]
    fn cmp_with(&self, other: &[A; N], ctx: &C) -> Ordering {
        self[..].cmp_with(&other[..], ctx)
    }
}

impl<const N: usize, A, C> HashWith<C> for [A; N]
where
    A: HashWith<C>,
{
    fn hash_with<H: Hasher>(&self, hasher: &mut H, ctx: &C) {
        self[..].hash_with(hasher, ctx)
    }
}

impl<A, B, C> PartialEqWith<C, Rc<B>> for Rc<A>
where
    A: PartialEqWith<C, B>,
{
    #[inline(always)]
    fn eq_with(&self, other: &Rc<B>, ctx: &C) -> bool {
        (**self).eq_with(&**other, ctx)
    }
}

impl<A, C> EqWith<C> for Rc<A> where A: EqWith<C> {}

impl<A, B, C> PartialOrdWith<C, Rc<B>> for Rc<A>
where
    A: PartialOrdWith<C, B>,
{
    #[inline(always)]
    fn partial_cmp_with(&self, other: &Rc<B>, ctx: &C) -> Option<Ordering> {
        (**self).partial_cmp_with(&**other, ctx)
    }
}

impl<A, C> OrdWith<C> for Rc<A>
where
    A: OrdWith<C>,
{
    #[inline(always)]
    fn cmp_with(&self, other: &Rc<A>, ctx: &C) -> Ordering {
        (**self).cmp_with(&**other, ctx)
    }
}

impl<A, C> HashWith<C> for Rc<A>
where
    A: HashWith<C>,
{
    #[inline(always)]
    fn hash_with<H: Hasher>(&self, hasher: &mut H, ctx: &C) {
        (**self).hash_with(hasher, ctx)
    }
}

impl<A, B, C> PartialEqWith<C, Arc<B>> for Arc<A>
where
    A: PartialEqWith<C, B>,
{
    #[inline(always)]
    fn eq_with(&self, other: &Arc<B>, ctx: &C) -> bool {
        (**self).eq_with(&**other, ctx)
    }
}

impl<A, C> EqWith<C> for Arc<A> where A: EqWith<C> {}

impl<A, B, C> PartialOrdWith<C, Arc<B>> for Arc<A>
where
    A: PartialOrdWith<C, B>,
{
    #[inline(always)]
    fn partial_cmp_with(&self, other: &Arc<B>, ctx: &C) -> Option<Ordering> {
        (**self).partial_cmp_with(&**other, ctx)
    }
}

impl<A, C> OrdWith<C> for Arc<A>
where
    A: OrdWith<C>,
{
    #[inline(always)]
    fn cmp_with(&self, other: &Arc<A>, ctx: &C) -> Ordering {
        (**self).cmp_with(&**other, ctx)
    }
}

impl<A, C> HashWith<C> for Arc<A>
where
    A: HashWith<C>,
{
    #[inline(always)]
    fn hash_with<H: Hasher>(&self, hasher: &mut H, ctx: &C) {
        (**self).hash_with(hasher, ctx)
    }
}

impl<A, B, C> PartialEqWith<C, Cow<'_, B>> for Cow<'_, A>
where
    A: PartialEqWith<C, B> + ToOwned,
    B: ToOwned,
{
    #[inline(always)]
    fn eq_with(&self, other: &Cow<B>, ctx: &C) -> bool {
        (**self).eq_with(&**other, ctx)
    }
}

impl<A, C> EqWith<C> for Cow<'_, A> where A: EqWith<C> + ToOwned {}

impl<A, B, C> PartialOrdWith<C, Cow<'_, B>> for Cow<'_, A>
where
    A: PartialOrdWith<C, B> + ToOwned,
    B: ToOwned,
{
    #[inline(always)]
    fn partial_cmp_with(&self, other: &Cow<B>, ctx: &C) -> Option<Ordering> {
        (**self).partial_cmp_with(&**other, ctx)
    }
}

impl<A, C> OrdWith<C> for Cow<'_, A>
where
    A: OrdWith<C> + ToOwned,
{
    #[inline(always)]
    fn cmp_with(&self, other: &Cow<A>, ctx: &C) -> Ordering {
        (**self).cmp_with(&**other, ctx)
    }
}

impl<A, C> HashWith<C> for Cow<'_, A>
where
    A: HashWith<C> + ToOwned,
{
    #[inline(always)]
    fn hash_with<H: Hasher>(&self, hasher: &mut H, ctx: &C) {
        (**self).hash_with(hasher, ctx)
    }
}

impl<A, B, C> PartialEqWith<C, Option<B>> for Option<A>
where
    A: PartialEqWith<C, B>,
{
    #[inline]
    fn eq_with(&self, other: &Option<B>, ctx: &C) -> bool {
        match (self, other) {
            (Some(this), Some(other)) => this.eq_with(other, ctx),
            (None, None) => true,
            _ => false,
        }
    }
}

impl<A, C> EqWith<C> for Option<A> where A: EqWith<C> {}

impl<A, B, C> PartialOrdWith<C, Option<B>> for Option<A>
where
    A: PartialOrdWith<C, B>,
{
    #[inline]
    fn partial_cmp_with(&self, other: &Option<B>, ctx: &C) -> Option<Ordering> {
        match (self, other) {
            (Some(this), Some(other)) => this.partial_cmp_with(other, ctx),
            (None, Some(_)) => Some(Ordering::Less),
            (Some(_), None) => Some(Ordering::Greater),
            (None, None) => Some(Ordering::Equal),
        }
    }
}

impl<A, C> OrdWith<C> for Option<A>
where
    A: OrdWith<C>,
{
    #[inline]
    fn cmp_with(&self, other: &Option<A>, ctx: &C) -> Ordering {
        match (self, other) {
            (Some(this), Some(other)) => this.cmp_with(other, ctx),
            (None, Some(_)) => Ordering::Less,
            (Some(_), None) => Ordering::Greater,
            (None, None) => Ordering::Equal,
        }
    }
}

impl<A, C> HashWith<C> for Option<A>
where
    A: HashWith<C>,
{
    #[inline]
    fn hash_with<H: Hasher>(&self, hasher: &mut H, ctx: &C) {
        //TODO: try to make this consistent with regular Option hashing?
        std::mem::discriminant(self).hash(hasher);
        if let Some(this) = self {
            this.hash_with(hasher, ctx)
        }
    }
}

impl<A1, B1, A2, B2, C> PartialEqWith<C, Either<A2, B2>> for Either<A1, B1>
where
    A1: PartialEqWith<C, A2>,
    B1: PartialEqWith<C, B2>,
{
    #[inline]
    fn eq_with(&self, other: &Either<A2, B2>, ctx: &C) -> bool {
        match (self, other) {
            (Either::Left(this), Either::Left(other)) => this.eq_with(other, ctx),
            (Either::Right(this), Either::Right(other)) => this.eq_with(other, ctx),
            _ => false,
        }
    }
}

impl<A, B, C> EqWith<C> for Either<A, B>
where
    A: EqWith<C>,
    B: EqWith<C>,
{
}

impl<A1, B1, A2, B2, C> PartialOrdWith<C, Either<A2, B2>> for Either<A1, B1>
where
    A1: PartialOrdWith<C, A2>,
    B1: PartialOrdWith<C, B2>,
{
    #[inline]
    fn partial_cmp_with(&self, other: &Either<A2, B2>, ctx: &C) -> Option<Ordering> {
        match (self, other) {
            (Either::Left(this), Either::Left(other)) => this.partial_cmp_with(other, ctx),
            (Either::Right(this), Either::Right(other)) => this.partial_cmp_with(other, ctx),
            (Either::Left(_), Either::Right(_)) => Some(Ordering::Less),
            (Either::Right(_), Either::Left(_)) => Some(Ordering::Greater),
        }
    }
}

impl<A, B, C> OrdWith<C> for Either<A, B>
where
    A: OrdWith<C>,
    B: OrdWith<C>,
{
    #[inline]
    fn cmp_with(&self, other: &Either<A, B>, ctx: &C) -> Ordering {
        match (self, other) {
            (Either::Left(this), Either::Left(other)) => this.cmp_with(other, ctx),
            (Either::Right(this), Either::Right(other)) => this.cmp_with(other, ctx),
            (Either::Left(_), Either::Right(_)) => Ordering::Less,
            (Either::Right(_), Either::Left(_)) => Ordering::Greater,
        }
    }
}

impl<A, B, C> HashWith<C> for Either<A, B>
where
    A: HashWith<C>,
    B: HashWith<C>,
{
    #[inline]
    fn hash_with<H: Hasher>(&self, hasher: &mut H, ctx: &C) {
        //TODO: try to make this consistent with regular Either hashing?
        std::mem::discriminant(self).hash(hasher);
        for_both!(self, x => x.hash_with(hasher, ctx))
    }
}

impl<A1, B1, A2, B2, C> PartialEqWith<C, (A2, B2)> for (A1, B1)
where
    A1: PartialEqWith<C, A2>,
    B1: PartialEqWith<C, B2>,
{
    #[inline]
    fn eq_with(&self, other: &(A2, B2), ctx: &C) -> bool {
        self.0.eq_with(&other.0, ctx) && self.1.eq_with(&other.1, ctx)
    }
}

impl<A, B, C> EqWith<C> for (A, B)
where
    A: EqWith<C>,
    B: EqWith<C>,
{
}

impl<A1, B1, A2, B2, C> PartialOrdWith<C, (A2, B2)> for (A1, B1)
where
    A1: PartialOrdWith<C, A2>,
    B1: PartialOrdWith<C, B2>,
{
    #[inline]
    fn partial_cmp_with(&self, other: &(A2, B2), ctx: &C) -> Option<Ordering> {
        match self.0.partial_cmp_with(&other.0, ctx) {
            Some(Ordering::Equal) => self.1.partial_cmp_with(&other.1, ctx),
            o => o,
        }
    }
}

impl<A, B, C> OrdWith<C> for (A, B)
where
    A: OrdWith<C>,
    B: OrdWith<C>,
{
    #[inline]
    fn cmp_with(&self, other: &(A, B), ctx: &C) -> Ordering {
        self.0
            .cmp_with(&other.0, ctx)
            .then_with(|| self.1.cmp_with(&other.1, ctx))
    }
}

impl<A, B, C> HashWith<C> for (A, B)
where
    A: HashWith<C>,
    B: HashWith<C>,
{
    #[inline]
    fn hash_with<H: Hasher>(&self, hasher: &mut H, ctx: &C) {
        self.0.hash_with(hasher, ctx);
        self.1.hash_with(hasher, ctx);
    }
}

pod_equiv!(());
pod_equiv!(u8);
pod_equiv!(u16);
pod_equiv!(u32);
pod_equiv!(u64);
pod_equiv!(u128);
pod_equiv!(usize);
pod_equiv!(i8);
pod_equiv!(i16);
pod_equiv!(i32);
pod_equiv!(i64);
pod_equiv!(i128);
pod_equiv!(isize);
pod_equiv!(NonZeroU8);
pod_equiv!(NonZeroU16);
pod_equiv!(NonZeroU32);
pod_equiv!(NonZeroU64);
pod_equiv!(NonZeroU128);
pod_equiv!(NonZeroUsize);
pod_equiv!(NonZeroI8);
pod_equiv!(NonZeroI16);
pod_equiv!(NonZeroI32);
pod_equiv!(NonZeroI64);
pod_equiv!(NonZeroI128);
pod_equiv!(NonZeroIsize);
pod_equiv!(String);
pod_equiv!(str);
pod_partial_eq_with!(f32);
pod_partial_ord_with!(f32);
pod_partial_eq_with!(f64);
pod_partial_ord_with!(f64);
