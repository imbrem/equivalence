#![doc = include_str!("../README.md")]
#![forbid(unsafe_code, missing_docs, missing_debug_implementations)]

use std::borrow::Cow;
use std::hash::Hash;
use std::rc::Rc;
use std::sync::Arc;
use std::{cmp::Ordering, hash::Hasher};

use either::{for_both, Either};

#[cfg(feature = "derive")]
pub use equivalence_derive::Equivalence;

mod macros;
pub use macros::*;

/// Trait for equality comparisons modulo a context of type `C`
///
/// # Example
/// ```
/// # use equivalence::*;
/// struct Person<'a> {
///     name: &'a str,
///     age: u8,
/// }
///
/// struct ByAge;
/// struct ByName;
///
/// impl<'a> PartialEqWith<ByAge, Self> for Person<'a> {
///     fn eq_with(&self, other: &Self, _: &ByAge) -> bool {
///         self.age == other.age
///     }
/// }
///
/// impl<'a> PartialEqWith<ByName, Self> for Person<'a> {
///     fn eq_with(&self, other: &Self, _: &ByName) -> bool {
///         self.name == other.name
///     }
/// }
///
/// let alice = Person { name: "Alice", age: 25 };
/// let bob = Person { name: "Bob", age: 25 };
/// assert!(alice.eq_with(&bob, &ByAge));
/// assert!(!alice.ne_with(&bob, &ByAge));
/// assert!(!alice.eq_with(&bob, &ByName));
/// assert!(alice.ne_with(&bob, &ByName));
/// ```
pub trait PartialEqWith<C: ?Sized, T: ?Sized = Self> {
    /// Check whether `self` and `other` are equal modulo the context `ctx`.
    ///
    /// Returns a `bool` indicating whether `self` and `other` are equal modulo the context `ctx`.
    fn eq_with(&self, other: &T, ctx: &C) -> bool;
    /// Check whether `self` and `other` are disequal modulo the context `ctx`.
    ///
    /// Returns a `bool` indicating whether `self` and `other` are disequal modulo the context `ctx`.
    ///
    /// This method is equivalent to `!self.eq_with(other, ctx)`. In general, there is no need to
    /// manually implement this method, as it can always be derived from `eq_with`.
    #[inline(always)]
    fn ne_with(&self, other: &T, ctx: &C) -> bool {
        !self.eq_with(other, ctx)
    }
}

/// Trait for equality comparisons modulo a context of type `C` which are equivalence relations.
///
/// This means that for any fixed `ctx: &C`, for all `a, b, c`, the `eq_with` relation is
/// - reflexive: `a.eq_with(a, ctx)`
/// - symmetric: `a.eq_with(b, ctx)` implies `b.eq_with(a, ctx)`
/// - transitive: `a.eq_with(b, ctx)` and `b.eq_with(c, ctx)` implies `a.eq_with(c, ctx)`
pub trait EqWith<C: ?Sized>: PartialEqWith<C> {}

/// Trait for types that form a partial order modulo a context of type `C`
///
/// Example
/// ```
/// # use equivalence::*;
/// # use std::cmp::Ordering;
/// /// A simple struct representing a set of boolean flags.
/// struct Flags(u8);
///
/// impl PartialOrdWith<Mask> for Flags {
///     /// Compare `self` and `other` modulo the context `ctx`.
///     /// One object is less than another if it has strictly less flags set;
///     /// two objects are incomparable if they have different flags set.
///     fn partial_cmp_with(&self, other: &Self, ctx: &Mask) -> Option<Ordering> {
///         // Get the masked flags for both self and other.
///         let self_masked = self.0 & ctx.0;
///         let other_masked = other.0 & ctx.0;
///
///         // Get the flags that are in self but not in other, and vice versa.
///         let self_not_other = self_masked & !other_masked;
///         let other_not_self = other_masked & !self_masked;
///         // Compare the sets of flags.
///         if self_not_other == 0 {
///             if other_not_self == 0 {
///                 // If the sets of flags are equal, the objects are equal.
///                 Some(Ordering::Equal)
///             } else {
///                 // If other has more flags set than self, self is less than other.
///                 Some(Ordering::Less)
///             }
///         } else if other_not_self == 0 {
///             // If self has more flags set than other, self is greater than other.
///             Some(Ordering::Greater)
///         } else {
///             // If self and other have different flags set, they are incomparable.
///             None
///         }
///     }
/// }
///
/// impl PartialEqWith<Mask> for Flags {
///     fn eq_with(&self, other: &Self, ctx: &Mask) -> bool {
///         (self.0 & ctx.0) == (other.0 & ctx.0)
///     }
/// }
///
/// /// A simple struct representing a bitmask.
/// struct Mask(u8);
///
/// let a = Flags(0b001);
/// let b = Flags(0b010);
/// let c = Flags(0b011);
/// let d = Flags(0b100);
/// let ctx = Mask(0b011);
///
/// assert_eq!(a.partial_cmp_with(&b, &ctx), None);
/// assert_eq!(a.partial_cmp_with(&c, &ctx), Some(Ordering::Less));
/// assert_eq!(c.partial_cmp_with(&a, &ctx), Some(Ordering::Greater));
/// assert_eq!(a.partial_cmp_with(&d, &ctx), Some(Ordering::Greater));
/// assert_eq!(d.partial_cmp_with(&a, &ctx), Some(Ordering::Less));
/// ```
pub trait PartialOrdWith<C: ?Sized, T: ?Sized = Self>: PartialEqWith<C, T> {
    /// Compare `self` and `other` modulo the context `ctx`
    ///
    /// Returns `Some(Ordering)` if `self` is less than, equal to, or greater than `other`, and
    /// `None` if they are incomparable.
    fn partial_cmp_with(&self, other: &T, ctx: &C) -> Option<Ordering>;

    /// Returns `true` if `self` is greater than or equal to `other` modulo the context `ctx`.
    ///
    /// This must be equivalent to `self.partial_cmp_with(other, ctx) >= Some(Ordering::Equal)`.
    #[inline(always)]
    fn ge_with(&self, other: &T, ctx: &C) -> bool {
        matches!(
            self.partial_cmp_with(other, ctx),
            Some(Ordering::Greater) | Some(Ordering::Equal)
        )
    }

    /// Returns `true` if `self` is strictly greater than `other` modulo the context `ctx`.
    ///
    /// This must be equivalent to `self.partial_cmp_with(other, ctx) == Some(Ordering::Greater)`
    #[inline(always)]
    fn gt_with(&self, other: &T, ctx: &C) -> bool {
        self.partial_cmp_with(other, ctx) == Some(Ordering::Greater)
    }

    /// Returns `true` if `self` is less than or equal to `other` modulo the context `ctx`.
    ///
    /// This must be equivalent to `self.partial_cmp_with(other, ctx) <= Some(Ordering::Equal)`
    #[inline(always)]
    fn le_with(&self, other: &T, ctx: &C) -> bool {
        matches!(
            self.partial_cmp_with(other, ctx),
            Some(Ordering::Less) | Some(Ordering::Equal)
        )
    }

    /// Returns `true` if `self` is strictly less than `other` modulo the context `ctx`.
    ///
    /// This must be equivalent to `self.partial_cmp_with(other, ctx) == Some(Ordering::Less)`
    #[inline(always)]
    fn lt_with(&self, other: &T, ctx: &C) -> bool {
        self.partial_cmp_with(other, ctx) == Some(Ordering::Less)
    }
}

/// Trait for types that form a total order modulo a context of type `C`
///
/// # Examples
///
/// ```
/// # use equivalence::*;
/// # use std::cmp::Ordering;
/// struct Person {
///     name: String,
///     age: u32,
/// }
///
/// enum PersonField { Name, Age }
///
/// impl OrdWith<PersonField> for Person {
///     fn cmp_with(&self, other: &Self, ctx: &PersonField) -> Ordering {
///         match ctx {
///             PersonField::Name => self.name.cmp(&other.name),
///             PersonField::Age => self.age.cmp(&other.age),
///         }
///     }
/// }
///
/// impl PartialOrdWith<PersonField> for Person {
///     fn partial_cmp_with(&self, other: &Self, ctx: &PersonField) -> Option<Ordering> {
///         Some(self.cmp_with(other, ctx))
///     }
/// }
///
/// impl PartialEqWith<PersonField> for Person {
///     fn eq_with(&self, other: &Self, ctx: &PersonField) -> bool {
///         match ctx {
///             PersonField::Name => self.name == other.name,
///             PersonField::Age => self.age == other.age,
///         }
///     }
/// }
///
/// impl EqWith<PersonField> for Person {}
///
/// let alice = Person { age: 30, name: "Alice".to_string() };
/// let bob = Person { age: 25, name: "Bob".to_string() };
/// assert_eq!(alice.cmp_with(&bob, &PersonField::Name), Ordering::Less);
/// assert_eq!(alice.cmp_with(&bob, &PersonField::Age), Ordering::Greater);
/// ```
pub trait OrdWith<C: ?Sized>: PartialOrdWith<C, Self> + EqWith<C> {
    /// Compare `self` and `other` modulo the context `ctx`.
    ///
    /// Returns `Ordering::Less` if `self` is less than `other`, `Ordering::Equal` if they are
    /// equal, and `Ordering::Greater` if `self` is greater than `other`.
    ///
    /// This must be equivalent to calling `self.partial_cmp_with(other, ctx).unwrap()`
    fn cmp_with(&self, other: &Self, ctx: &C) -> Ordering;

    #[inline]
    /// Returns the maximum of `self` and `other` modulo the context `ctx`.
    fn max_with(self, other: Self, ctx: &C) -> Self
    where
        Self: Sized,
    {
        if self.ge_with(&other, ctx) {
            self
        } else {
            other
        }
    }

    #[inline]
    /// Returns the minimum of `self` and `other` modulo the context `ctx`.
    fn min_with(self, other: Self, ctx: &C) -> Self
    where
        Self: Sized,
    {
        if self.le_with(&other, ctx) {
            self
        } else {
            other
        }
    }

    /// Clamps `self` to the interval `[min, max]` modulo the context `ctx`.
    ///
    /// If `self` is less than `min`, this returns `min`. If `self` is greater than `max`, this
    /// returns `max`. Otherwise, this returns `self`.
    ///
    /// The result value is unspecified if `min > max`; this is in contrast to the standard
    /// library's [`Ord::clamp`], which is guaranteed to panic.
    #[inline]
    fn clamp_with(self, min: Self, max: Self, ctx: &C) -> Self
    where
        Self: Sized,
    {
        if self.le_with(&max, ctx) {
            if self.ge_with(&min, ctx) {
                self
            } else {
                min
            }
        } else {
            max
        }
    }
}

/// A type which can be hashed modulo a context of type `C`
///
/// If `PartialEqWith<C, T>` is implemented, it is expected that equivalent values have the same hashing behaviour.
///
/// # Example
/// ```
/// # use equivalence::{HashWith, EqWith};
/// # use std::collections::hash_map::DefaultHasher;
/// # use std::hash::{Hash, Hasher};
///
/// #[derive(Debug)]
/// struct Person {
///     name: String,
///     age: u32,
/// }
///
/// enum PersonField {
///     Name,
///     Age,
/// }
///
/// impl HashWith<PersonField> for Person {
///     fn hash_with<H: Hasher>(&self, hasher: &mut H, ctx: &PersonField) {
///         match ctx {
///             PersonField::Name => self.name.hash(hasher),
///             PersonField::Age => self.age.hash(hasher),
///         }
///     }
/// }
///
/// let alice1 = Person { age: 30, name: "Alice".to_string() };
/// let alice2 = Person { age: 35, name: "Alice".to_string() };
/// let bob = Person { age: 30, name: "Bob".to_string() };
///
/// let mut hasher = DefaultHasher::new();
/// alice1.hash_with(&mut hasher, &PersonField::Name);
/// let alice1_name = hasher.finish();
///
/// let mut hasher = DefaultHasher::new();
/// alice2.hash_with(&mut hasher, &PersonField::Name);
/// let alice2_name = hasher.finish();
///
/// let mut hasher = DefaultHasher::new();
/// bob.hash_with(&mut hasher, &PersonField::Name);
/// let bob_name = hasher.finish();
///
/// assert_eq!(alice1_name, alice2_name);
/// assert_ne!(alice1_name, bob_name);
///
/// let mut hasher = DefaultHasher::new();
/// alice1.hash_with(&mut hasher, &PersonField::Age);
/// let alice1_age = hasher.finish();
///
/// let mut hasher = DefaultHasher::new();
/// alice2.hash_with(&mut hasher, &PersonField::Age);
/// let alice2_age = hasher.finish();
///
/// let mut hasher = DefaultHasher::new();
/// bob.hash_with(&mut hasher, &PersonField::Age);
/// let bob_age = hasher.finish();
///
/// assert_ne!(alice1_age, alice2_age);
/// assert_eq!(alice1_age, bob_age);
/// ```
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

impl<A1, B1, C1, A2, B2, C2, X> PartialEqWith<X, (A2, B2, C2)> for (A1, B1, C1)
where
    A1: PartialEqWith<X, A2>,
    B1: PartialEqWith<X, B2>,
    C1: PartialEqWith<X, C2>,
{
    #[inline]
    fn eq_with(&self, other: &(A2, B2, C2), ctx: &X) -> bool {
        self.0.eq_with(&other.0, ctx)
            && self.1.eq_with(&other.1, ctx)
            && self.2.eq_with(&other.2, ctx)
    }
}

impl<A, B, C, X> EqWith<X> for (A, B, C)
where
    A: EqWith<X>,
    B: EqWith<X>,
    C: EqWith<X>,
{
}

impl<A1, B1, C1, A2, B2, C2, X> PartialOrdWith<X, (A2, B2, C2)> for (A1, B1, C1)
where
    A1: PartialOrdWith<X, A2>,
    B1: PartialOrdWith<X, B2>,
    C1: PartialOrdWith<X, C2>,
{
    #[inline]
    fn partial_cmp_with(&self, other: &(A2, B2, C2), ctx: &X) -> Option<Ordering> {
        match self.0.partial_cmp_with(&other.0, ctx) {
            Some(Ordering::Equal) => {}
            o => return o,
        }
        self.1.partial_cmp_with(&other.1, ctx)
    }
}

impl<A, B, C, X> OrdWith<X> for (A, B, C)
where
    A: OrdWith<X>,
    B: OrdWith<X>,
    C: OrdWith<X>,
{
    #[inline]
    fn cmp_with(&self, other: &(A, B, C), ctx: &X) -> Ordering {
        self.0
            .cmp_with(&other.0, ctx)
            .then_with(|| self.1.cmp_with(&other.1, ctx))
            .then_with(|| self.2.cmp_with(&other.2, ctx))
    }
}

impl<A, B, C, X> HashWith<X> for (A, B, C)
where
    A: HashWith<X>,
    B: HashWith<X>,
    C: HashWith<X>,
{
    #[inline]
    fn hash_with<H: Hasher>(&self, hasher: &mut H, ctx: &X) {
        self.0.hash_with(hasher, ctx);
        self.1.hash_with(hasher, ctx);
        self.2.hash_with(hasher, ctx);
    }
}

impl<A1, B1, C1, D1, A2, B2, C2, D2, X> PartialEqWith<X, (A2, B2, C2, D2)> for (A1, B1, C1, D1)
where
    A1: PartialEqWith<X, A2>,
    B1: PartialEqWith<X, B2>,
    C1: PartialEqWith<X, C2>,
    D1: PartialEqWith<X, D2>,
{
    #[inline]
    fn eq_with(&self, other: &(A2, B2, C2, D2), ctx: &X) -> bool {
        self.0.eq_with(&other.0, ctx)
            && self.1.eq_with(&other.1, ctx)
            && self.2.eq_with(&other.2, ctx)
            && self.3.eq_with(&other.3, ctx)
    }
}

impl<A, B, C, D, X> EqWith<X> for (A, B, C, D)
where
    A: EqWith<X>,
    B: EqWith<X>,
    C: EqWith<X>,
    D: EqWith<X>,
{
}

impl<A1, B1, C1, D1, A2, B2, C2, D2, X> PartialOrdWith<X, (A2, B2, C2, D2)> for (A1, B1, C1, D1)
where
    A1: PartialOrdWith<X, A2>,
    B1: PartialOrdWith<X, B2>,
    C1: PartialOrdWith<X, C2>,
    D1: PartialOrdWith<X, D2>,
{
    #[inline]
    fn partial_cmp_with(&self, other: &(A2, B2, C2, D2), ctx: &X) -> Option<Ordering> {
        match self.0.partial_cmp_with(&other.0, ctx) {
            Some(Ordering::Equal) => {}
            o => return o,
        }
        match self.1.partial_cmp_with(&other.1, ctx) {
            Some(Ordering::Equal) => {}
            o => return o,
        }
        match self.2.partial_cmp_with(&other.2, ctx) {
            Some(Ordering::Equal) => {}
            o => return o,
        }
        self.3.partial_cmp_with(&other.3, ctx)
    }
}

impl<A, B, C, D, X> OrdWith<X> for (A, B, C, D)
where
    A: OrdWith<X>,
    B: OrdWith<X>,
    C: OrdWith<X>,
    D: OrdWith<X>,
{
    #[inline]
    fn cmp_with(&self, other: &(A, B, C, D), ctx: &X) -> Ordering {
        self.0
            .cmp_with(&other.0, ctx)
            .then_with(|| self.1.cmp_with(&other.1, ctx))
            .then_with(|| self.2.cmp_with(&other.2, ctx))
            .then_with(|| self.3.cmp_with(&other.3, ctx))
    }
}

impl<A, B, C, D, X> HashWith<X> for (A, B, C, D)
where
    A: HashWith<X>,
    B: HashWith<X>,
    C: HashWith<X>,
    D: HashWith<X>,
{
    #[inline]
    fn hash_with<H: Hasher>(&self, hasher: &mut H, ctx: &X) {
        self.0.hash_with(hasher, ctx);
        self.1.hash_with(hasher, ctx);
        self.2.hash_with(hasher, ctx);
        self.3.hash_with(hasher, ctx);
    }
}
