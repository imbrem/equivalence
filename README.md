# equivalence

![Version](https://img.shields.io/crates/v/equivalence)
![Documentation](https://img.shields.io/docsrs/equivalence)
![Build](https://img.shields.io/github/actions/workflow/status/imbrem/equivalence/rust.yml)
![License](https://img.shields.io/crates/l/equivalence/0.1.0)
![Downloads](https://img.shields.io/crates/d/equivalence)

This crate provides traits for comparing and hashing values modulo an equivalence relation specified by a context of user-defined type `C`.
The `Equivalence` derive macro allows the user to easily implement `Equivalence` for custom types.

# Example
```rust
# use equivalence::*;
# use std::cmp::Ordering;
# use std::hash::{Hash, Hasher};
/// The equivalence relation mod n over 64-bit unsigned integers
struct ModN(u64);

impl PartialEqWith<ModN> for u64 {
    fn eq_with(&self, other: &u64, ctx: &ModN) -> bool {
        (*self % ctx.0) == (*other % ctx.0)
    }
}

impl EqWith<ModN> for u64 {}

impl OrdWith<ModN> for u64 {
    fn cmp_with(&self, other: &u64, ctx: &ModN) -> Ordering {
        (*self % ctx.0).cmp(&(*other % ctx.0))
    }
}

impl PartialOrdWith<ModN> for u64 {
    fn partial_cmp_with(&self, other: &u64, ctx: &ModN) -> Option<Ordering> {
        Some(self.cmp_with(other, ctx))
    }
}

impl HashWith<ModN> for u64 {
    fn hash_with<H: Hasher>(&self, hasher: &mut H, ctx: &ModN) {
        (*self % ctx.0).hash(hasher)
    }
}

// Containers can be conveniently compared and hashed modulo a given equivalence context:
assert!([1, 2, 3].eq_with(&[4, 5, 6], &ModN(3)));
assert!([1, 2, 3].ne_with(&[4, 5, 6], &ModN(2)));

// The `Equivalence` derive macro can be used to derive `Equivalence` for custom containers
#[derive(Equivalence)]
struct MyPair<T> {
    #[equiv(fwd)]
    left: T, // self.left and other.left are compared using `PartialEqWith`, since they are specified as forwarded
    right: u32 // self.right and other.left are compared using `PartialEq`, since it is not forwarded
}

assert!(MyPair { left: 5u64, right: 7 }.eq_with(&MyPair { left: 6u64, right: 7 }, &ModN(1)));
assert!(MyPair { left: 5u64, right: 7 }.ne_with(&MyPair { left: 5u64, right: 8 }, &ModN(1)));

// We may also use the macro to derive `Equivalence` for a particular context only, with custom logic
#[derive(Equivalence)]
#[equiv(rel = "ModN")]
struct U32Pair {
    #[equiv(fwd = "map_rec |x: &u32, _| *x as u64")]
    left: u32, // self.left and other.left are first cast to u64, and then compared using `PartialEqWith`
    #[equiv(fwd = "map |x: &u32, ctx: &ModN| (*x as u64) % ctx.0")]
    right: u32, // right is mapped to right % ctx.0, which is then compared using `PartialEq`; this has the same result as the above
}

assert!(U32Pair { left: 3, right: 5 }.eq_with(&U32Pair { left: 5, right: 7 }, &ModN(2)));
assert!(U32Pair { left: 3, right: 5 }.ne_with(&U32Pair { left: 5, right: 7 }, &ModN(3)));
```