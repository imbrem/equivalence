/*!
Macros for quickly implementing `Equivalence` traits by delegating to `std`
*/

/// In contexts of type `$ctx`, delegate `PartialEqWith<$s>` to `PartialEq<$s>` for `$t`
///
/// If `$s` is not specified, the delegation is for *all* types `S` such that `$t: PartialEq<S>`.
/// 
/// Specifying `$s` as `self` is equivalent to setting `$s = $t`.
///
/// # Examples
/// ```rust
/// # use equivalence::*;
/// struct CompareU32;
///
/// delegate_partial_eq_with!(CompareU32, u32);
///
/// struct MyU32(u32);
///
/// impl PartialEq<MyU32> for u32 {
///     fn eq(&self, other: &MyU32) -> bool {
///         *self == other.0
///     }
/// }
///
/// impl PartialEqWith<CompareU32> for u64 {
///     fn eq_with(&self, other: &u64, _: &CompareU32) -> bool {
///         *self as u32 == *other as u32
///     }
/// }
///
/// assert!(5u32.eq_with(&5, &CompareU32));
/// assert!(5u32.eq_with(&MyU32(5), &CompareU32));
/// assert!((5u64 + (1 << 35)).eq_with(&5u64, &CompareU32));
/// assert!(!6u32.eq_with(&5, &CompareU32));
/// assert!(!6u32.eq_with(&MyU32(5), &CompareU32));
/// assert!(!(6u64 + (1 << 35)).eq_with(&5u64, &CompareU32));
/// ```
/// ```compile_fail
/// # use equivalence::*;
/// struct CompareU32;
///
/// // NOTE: unlike the above example, this *only* delegates the `PartialEqWith`
/// // implementation for `u32` (but *not* the one for `MyU32`, hence the error).
/// delegate_partial_eq_with!(CompareU32, u32, u32);
///
/// struct MyU32(u32);
///
/// impl PartialEq<MyU32> for u32 {
///     fn eq(&self, other: &MyU32) -> bool {
///         *self == other.0
///     }
/// }
///
/// assert!(5u32.eq_with(&MyU32(5), &CompareU32));
/// ```
/// ```rust
/// # use equivalence::*;
/// struct CompareU32;
///
/// delegate_partial_eq_with!(CompareU32, u32, u32);
///
/// assert!(5u32.eq_with(&5, &CompareU32));
/// ```
/// ```rust
/// # use equivalence::*;
/// struct CompareU32;
///
/// delegate_partial_eq_with!(CompareU32, u32, self);
///
/// assert!(5u32.eq_with(&5, &CompareU32));
/// ```
#[macro_export]
macro_rules! delegate_partial_eq_with {
    ($ctx:ty, $t:ty, self) => {
        $crate::delegate_partial_eq_with!($ctx, $t, $t);
    };
    ($ctx:ty, $t:ty) => {
        impl<S> $crate::PartialEqWith<$ctx, S> for $t
        where
            $t: PartialEq<S>,
        {
            #[inline(always)]
            fn eq_with(&self, other: &S, _ctx: &$ctx) -> bool {
                self.eq(other)
            }
        }
    };
    ($ctx:ty, $t:ty, $s:ty) => {
        impl $crate::PartialEqWith<$ctx, $s> for $t {
            #[inline(always)]
            fn eq_with(&self, other: &$s, _ctx: &$ctx) -> bool {
                self.eq(other)
            }
        }
    };
}

/// In contexts of type `$ctx`, implement `EqWith` for `$t`
/// 
/// Exactly equivalent `impl EqWith<$ctx> for $t {}`
#[macro_export]
macro_rules! delegate_eq_with {
    ($ctx:ty, $t:ty) => {
        impl $crate::EqWith<$ctx> for $t {}
    };
}

/// In contexts of type `$ctx`, delegate `PartialOrdWith<$s>` to `PartialOrd<$s>`
///
/// If `$s` is not specified, the delegation is for *all* types `S` such that `$t: PartialOrd<S>`.
/// 
/// Specifying `$s` as `self` is equivalent to setting `$s = $t`.
///
/// # Examples
/// ```rust
/// # use equivalence::*;
/// # use core::cmp::Ordering;
/// # use Ordering::*;
/// struct CompareU32;
///
/// delegate_partial_ord_with!(CompareU32, u32);
/// delegate_partial_eq_with!(CompareU32, u32);
///
/// struct MyU32(u32);
///
/// impl PartialEq<MyU32> for u32 {
///     fn eq(&self, other: &MyU32) -> bool {
///         *self == other.0
///     }
/// }
///
/// impl PartialOrd<MyU32> for u32 {
///     fn partial_cmp(&self, other: &MyU32) -> Option<Ordering> {
///         self.partial_cmp(&other.0)
///     }
/// }
///
/// assert_eq!(5u32.partial_cmp_with(&5, &CompareU32), Some(Equal));
/// assert_eq!(5u32.partial_cmp_with(&MyU32(5), &CompareU32), Some(Equal));
/// assert_eq!(6u32.partial_cmp_with(&5, &CompareU32), Some(Greater));
/// assert_eq!(4u32.partial_cmp_with(&MyU32(5), &CompareU32), Some(Less));
/// ```
/// ```compile_fail
/// # use equivalence::*;
/// # use core::cmp::Ordering;
/// # use Ordering::*;
/// struct CompareU32;
///
/// // NOTE: unlike the above example, this *only* delegates the `PartialOrdWith`
/// // implementation for `u32` (but *not* the one for `MyU32`, hence the error).
/// delegate_partial_ord_with!(CompareU32, u32, u32);
/// delegate_partial_eq_with!(CompareU32, u32, u32);
///
/// struct MyU32(u32);
///
/// impl PartialEq<MyU32> for u32 {
///     fn eq(&self, other: &MyU32) -> bool {
///         *self == other.0
///     }
/// }
///
/// impl PartialOrd<MyU32> for u32 {
///     fn partial_cmp(&self, other: &MyU32) -> Option<Ordering> {
///         self.partial_cmp(&other.0)
///     }
/// }
///
/// assert_eq!(5u32.partial_cmp_with(&MyU32(5), &CompareU32), Some(Equal));
/// ```
/// ```rust
/// # use equivalence::*;
/// # use core::cmp::Ordering;
/// # use Ordering::*;
/// struct CompareU32;
/// 
/// delegate_partial_ord_with!(CompareU32, u32, u32);
/// delegate_partial_eq_with!(CompareU32, u32, u32);
///
/// assert_eq!(5u32.partial_cmp_with(&5, &CompareU32), Some(Equal));
/// ```
/// ```rust
/// # use equivalence::*;
/// # use core::cmp::Ordering;
/// # use Ordering::*;
/// struct CompareU32;
/// 
/// delegate_partial_ord_with!(CompareU32, u32, self);
/// delegate_partial_eq_with!(CompareU32, u32, self);
///
/// assert_eq!(5u32.partial_cmp_with(&5, &CompareU32), Some(Equal));
/// ```
#[macro_export]
macro_rules! delegate_partial_ord_with {
    ($ctx:ty, $t:ty, self) => {
        $crate::delegate_partial_ord_with!($ctx, $t, $t);
    };
    ($ctx:ty, $t:ty) => {
        impl<S> $crate::PartialOrdWith<$ctx, S> for $t
        where
            $t: PartialOrd<S>,
        {
            #[inline(always)]
            fn partial_cmp_with(&self, other: &S, _ctx: &$ctx) -> Option<core::cmp::Ordering> {
                self.partial_cmp(other)
            }
        }
    };
    ($ctx:ty, $t:ty, $s:ty) => {
        impl $crate::PartialOrdWith<$ctx, $s> for $t {
            #[inline(always)]
            fn partial_cmp_with(&self, other: &$s, _ctx: &$ctx) -> Option<core::cmp::Ordering> {
                self.partial_cmp(other)
            }
        }
    };
}

/// In contexts of type `$ctx`, delegate `OrdWith` to `Ord` for `$t`
/// 
/// # Examples
/// ```rust
/// # use equivalence::*;
/// # use core::cmp::Ordering;
/// # use Ordering::*;
/// struct CompareU32;
/// 
/// delegate_ord_with!(CompareU32, u32);
/// delegate_partial_ord_with!(CompareU32, u32, u32);
/// delegate_eq_with!(CompareU32, u32);
/// delegate_partial_eq_with!(CompareU32, u32, u32);
///
/// assert_eq!(5u32.cmp_with(&5, &CompareU32), Equal);
/// ```
#[macro_export]
macro_rules! delegate_ord_with {
    ($ctx:ty, $t:ty) => {
        impl $crate::OrdWith<$ctx> for $t {
            #[inline(always)]
            fn cmp_with(&self, other: &$t, _ctx: &$ctx) -> core::cmp::Ordering {
                self.cmp(other)
            }
        }
    };
}

/// In contexts of type `$ctx`, delegate `HashWith` to `Hash` for `$t`
/// 
/// # Examples
/// ```rust
/// # use equivalence::*;
/// # use core::cmp::Ordering;
/// # use Ordering::*;
/// # use std::collections::hash_map::DefaultHasher;
/// # use std::hash::{Hash, Hasher};
/// struct CompareU32;
/// 
/// delegate_hash_with!(CompareU32, u32);
/// 
/// let mut hasher = DefaultHasher::new();
/// 5u32.hash(&mut hasher);
/// 
/// let mut hasher2 = DefaultHasher::new();
/// 5u32.hash_with(&mut hasher2, &CompareU32);
/// 
/// assert_eq!(hasher.finish(), hasher2.finish());
/// ```
#[macro_export]
macro_rules! delegate_hash_with {
    ($ctx:ty, $t:ty) => {
        impl $crate::HashWith<$ctx> for $t {
            #[inline(always)]
            fn hash_with<H: core::hash::Hasher>(&self, hasher: &mut H, _ctx: &$ctx) {
                core::hash::Hash::hash(self, hasher)
            }
        }
    };
}

/// Delegate all `Equivalence` traits to their corresponding `core` trait for `$t`
/// 
/// If `$s` is not provided, delegates `PartialEqWith<S>` to `PartialEq<S>`, `PartialOrdWith<S>` 
/// to `PartialOrd<S>` (for all valid `S`), `OrdWith` to `Ord`, `HashWith` to `Hash`, and 
/// implements `EqWith`.
/// 
/// If `$s` is provided and is a valid type, delegates `PartialEqWith<$s>` to `PartialEq<$s>` and 
/// `PartialOrdWith<$s>` to `PartialOrd<$s>`.
/// 
/// If `$s` is provided and is `self`, `PartialEqWith` to `PartialEq`, `PartialOrdWith` 
/// to `PartialOrd`, `OrdWith` to `Ord`, `HashWith` to `Hash`, and implements `EqWith`.
/// 
/// # Examples
/// ```rust
/// # use equivalence::*;
/// # use core::cmp::Ordering;
/// # use Ordering::*;
/// struct CompareU32;
///
/// delegate_equiv!(CompareU32, u32);
///
/// struct MyU32(u32);
///
/// impl PartialEq<MyU32> for u32 {
///     fn eq(&self, other: &MyU32) -> bool {
///         *self == other.0
///     }
/// }
///
/// impl PartialOrd<MyU32> for u32 {
///     fn partial_cmp(&self, other: &MyU32) -> Option<Ordering> {
///         self.partial_cmp(&other.0)
///     }
/// }
///
/// assert_eq!(5u32.partial_cmp_with(&5, &CompareU32), Some(Equal));
/// assert_eq!(5u32.partial_cmp_with(&MyU32(5), &CompareU32), Some(Equal));
/// assert_eq!(6u32.partial_cmp_with(&5, &CompareU32), Some(Greater));
/// assert_eq!(4u32.partial_cmp_with(&MyU32(5), &CompareU32), Some(Less));
/// ```
/// ```compile_fail
/// # use equivalence::*;
/// # use core::cmp::Ordering;
/// # use Ordering::*;
/// struct CompareU32;
///
/// // NOTE: unlike the above example, this *only* delegates the `PartialOrdWith`
/// // implementation for `u32` (but *not* the one for `MyU32`, hence the error).
/// delegate_equiv!(CompareU32, u32, self);
///
/// struct MyU32(u32);
///
/// impl PartialEq<MyU32> for u32 {
///     fn eq(&self, other: &MyU32) -> bool {
///         *self == other.0
///     }
/// }
///
/// impl PartialOrd<MyU32> for u32 {
///     fn partial_cmp(&self, other: &MyU32) -> Option<Ordering> {
///         self.partial_cmp(&other.0)
///     }
/// }
///
/// assert_eq!(5u32.partial_cmp_with(&MyU32(5), &CompareU32), Some(Equal));
/// ```
/// ```rust
/// # use equivalence::*;
/// # use core::cmp::Ordering;
/// # use Ordering::*;
/// struct CompareU32;
/// 
/// delegate_equiv!(CompareU32, u32, self);
///
/// assert_eq!(5u32.partial_cmp_with(&5, &CompareU32), Some(Equal));
/// ```
/// ```rust
/// # use equivalence::*;
/// # use core::cmp::Ordering;
/// # use Ordering::*;
/// struct CompareU32;
/// 
/// delegate_equiv!(CompareU32, u32, u32);
///
/// assert_eq!(5u32.partial_cmp_with(&5, &CompareU32), Some(Equal));
/// ```
#[macro_export]
macro_rules! delegate_equiv {
    ($ctx:ty, $t:ty, self) => {
        $crate::delegate_eq_with!($ctx, $t);
        $crate::delegate_hash_with!($ctx, $t);
        $crate::delegate_ord_with!($ctx, $t);
        $crate::delegate_partial_eq_with!($ctx, $t, $t);
        $crate::delegate_partial_ord_with!($ctx, $t, $t);
    };
    ($ctx:ty, $t:ty, $s:ty) => {
        $crate::delegate_partial_eq_with!($ctx, $t, $s);
        $crate::delegate_partial_ord_with!($ctx, $t, $s);
    };
    ($ctx:ty, $t:ty) => {
        $crate::delegate_eq_with!($ctx, $t);
        $crate::delegate_hash_with!($ctx, $t);
        $crate::delegate_ord_with!($ctx, $t);
        $crate::delegate_partial_eq_with!($ctx, $t);
        $crate::delegate_partial_ord_with!($ctx, $t);
    };
}

/// Delegate all `Equivalence` traits to their corresponding `core` trait for primitive integer types
/// 
/// If the second argument is not provided or `false`, delegates `PartialEqWith<S>` to `PartialEq<S>`, 
/// `PartialOrdWith<S>` to `PartialOrd<S>` (for all valid `S`), `OrdWith` to `Ord`, `HashWith` to `Hash`, 
/// and  implements `EqWith` for all primitive integer types.
/// 
/// If the second argument is `true`,  delegates `PartialEqWith` to `PartialEq`,  `PartialOrdWith` to 
/// `PartialOrd`, `OrdWith` to `Ord`, `HashWith` to `Hash`, and  implements `EqWith` for all primitive 
/// integer types.
/// 
/// # Examples
/// ```rust
/// # use equivalence::*;
/// # use core::cmp::Ordering;
/// # use Ordering::*;
/// struct CompareInt;
/// delegate_int_equiv!(CompareInt);
///
/// struct MyU32(u32);
///
/// impl PartialEq<MyU32> for u32 {
///     fn eq(&self, other: &MyU32) -> bool {
///         *self == other.0
///     }
/// }
///
/// impl PartialOrd<MyU32> for u32 {
///     fn partial_cmp(&self, other: &MyU32) -> Option<Ordering> {
///         self.partial_cmp(&other.0)
///     }
/// }
///
/// assert_eq!(5u32.partial_cmp_with(&5, &CompareInt), Some(Equal));
/// assert_eq!(5u32.partial_cmp_with(&MyU32(5), &CompareInt), Some(Equal));
/// assert_eq!(6u32.partial_cmp_with(&5, &CompareInt), Some(Greater));
/// assert_eq!(4u32.partial_cmp_with(&MyU32(5), &CompareInt), Some(Less));
/// ```
/// ```compile_fail
/// # use equivalence::*;
/// # use core::cmp::Ordering;
/// # use Ordering::*;
/// // NOTE: unlike the above example, this *only* delegates the `PartialOrdWith`
/// // implementation for `u32` (but *not* the one for `MyU32`, hence the error).
/// struct CompareInt;
/// delegate_int_equiv!(CompareInt, true);
///
/// struct MyU32(u32);
///
/// impl PartialEq<MyU32> for u32 {
///     fn eq(&self, other: &MyU32) -> bool {
///         *self == other.0
///     }
/// }
///
/// impl PartialOrd<MyU32> for u32 {
///     fn partial_cmp(&self, other: &MyU32) -> Option<Ordering> {
///         self.partial_cmp(&other.0)
///     }
/// }
///
/// assert_eq!(5u32.partial_cmp_with(&MyU32(5), &CompareInt), Some(Equal));
/// ```
/// ```rust
/// # use equivalence::*;
/// # use core::cmp::Ordering;
/// # use Ordering::*;
/// struct CompareInt;
/// delegate_int_equiv!(CompareInt, true);
///
/// assert_eq!(5u32.partial_cmp_with(&5, &CompareInt), Some(Equal));
/// ```
#[macro_export]
macro_rules! delegate_int_equiv {
    ($ctx:ty, true) => {
        $crate::delegate_equiv!($ctx, u8, self);
        $crate::delegate_equiv!($ctx, u16, self);
        $crate::delegate_equiv!($ctx, u32, self);
        $crate::delegate_equiv!($ctx, u64, self);
        $crate::delegate_equiv!($ctx, u128, self);
        $crate::delegate_equiv!($ctx, i8, self);
        $crate::delegate_equiv!($ctx, i16, self);
        $crate::delegate_equiv!($ctx, i32, self);
        $crate::delegate_equiv!($ctx, i64, self);
        $crate::delegate_equiv!($ctx, i128, self);
    };
    ($ctx:ty, false) => {
        $crate::delegate_equiv!($ctx, u8);
        $crate::delegate_equiv!($ctx, u16);
        $crate::delegate_equiv!($ctx, u32);
        $crate::delegate_equiv!($ctx, u64);
        $crate::delegate_equiv!($ctx, u128);
        $crate::delegate_equiv!($ctx, i8);
        $crate::delegate_equiv!($ctx, i16);
        $crate::delegate_equiv!($ctx, i32);
        $crate::delegate_equiv!($ctx, i64);
        $crate::delegate_equiv!($ctx, i128);
    };
    ($ctx:ty) => {
        $crate::delegate_int_equiv!($ctx, false);
    };
}