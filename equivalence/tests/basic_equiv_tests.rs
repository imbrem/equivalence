use std::cmp::Ordering;

use equivalence::*;

pub struct CtxA;
pub struct CtxB;

#[derive(Equivalence)]
#[equiv(rel(name = "CtxA"))]
#[equiv(rel = "CtxB")]

pub struct ModS {
    a: u32,
    #[equiv(fwd(delegate))]
    b: u32,
    #[equiv(fwd(name = "CtxA", map = "|x, _| x % 2"))]
    #[equiv(fwd(name = "CtxB", map = "|x, _| x % 3"))]
    c: u32,
}

#[derive(Equivalence)]
#[equiv(rel(name = "CtxA"))]
#[equiv(rel(name = "CtxB"))]

pub struct ModS2(
    u32,
    #[equiv(fwd(name = "CtxA", map = "|x, _| x % 2"))]
    #[equiv(fwd(name = "CtxB", map = "|x, _| x % 3"))]
    u32,
);

#[test]
fn mod_s() {
    let m111 = ModS { a: 1, b: 1, c: 1 };
    let m113 = ModS { a: 1, b: 1, c: 3 };
    let m114 = ModS { a: 1, b: 1, c: 4 };
    let m124 = ModS { a: 1, b: 2, c: 4 };

    assert!(m111.eq_with(&m111, &CtxA));
    assert!(m111.eq_with(&m113, &CtxA));
    assert!(!m111.eq_with(&m114, &CtxA));
    assert!(!m111.eq_with(&m124, &CtxA));
    assert!(m111.eq_with(&m111, &CtxB));
    assert!(!m111.eq_with(&m113, &CtxB));
    assert!(m111.eq_with(&m114, &CtxB));
    assert!(!m111.eq_with(&m124, &CtxB));

    assert!(m113.eq_with(&m111, &CtxA));
    assert!(m113.eq_with(&m113, &CtxA));
    assert!(!m113.eq_with(&m114, &CtxA));
    assert!(!m113.eq_with(&m124, &CtxA));
    assert!(!m113.eq_with(&m111, &CtxB));
    assert!(m113.eq_with(&m113, &CtxB));
    assert!(!m113.eq_with(&m114, &CtxB));
    assert!(!m113.eq_with(&m124, &CtxB));

    assert!(!m114.eq_with(&m111, &CtxA));
    assert!(!m114.eq_with(&m113, &CtxA));
    assert!(m114.eq_with(&m114, &CtxA));
    assert!(!m114.eq_with(&m124, &CtxA));
    assert!(m114.eq_with(&m111, &CtxB));
    assert!(!m114.eq_with(&m113, &CtxB));
    assert!(m114.eq_with(&m114, &CtxB));
    assert!(!m114.eq_with(&m124, &CtxB));

    assert!(!m124.eq_with(&m111, &CtxA));
    assert!(!m124.eq_with(&m113, &CtxA));
    assert!(!m124.eq_with(&m114, &CtxA));
    assert!(m124.eq_with(&m124, &CtxA));
    assert!(!m124.eq_with(&m111, &CtxB));
    assert!(!m124.eq_with(&m113, &CtxB));
    assert!(!m124.eq_with(&m114, &CtxB));
    assert!(m124.eq_with(&m124, &CtxB));

    let m111 = ModS2(1, 1);
    let m113 = ModS2(1, 3);
    let m114 = ModS2(1, 4);
    let m124 = ModS2(2, 4);

    assert!(m111.eq_with(&m111, &CtxA));
    assert!(m111.eq_with(&m113, &CtxA));
    assert!(!m111.eq_with(&m114, &CtxA));
    assert!(!m111.eq_with(&m124, &CtxA));
    assert!(m111.eq_with(&m111, &CtxB));
    assert!(!m111.eq_with(&m113, &CtxB));
    assert!(m111.eq_with(&m114, &CtxB));
    assert!(!m111.eq_with(&m124, &CtxB));

    assert!(m113.eq_with(&m111, &CtxA));
    assert!(m113.eq_with(&m113, &CtxA));
    assert!(!m113.eq_with(&m114, &CtxA));
    assert!(!m113.eq_with(&m124, &CtxA));
    assert!(!m113.eq_with(&m111, &CtxB));
    assert!(m113.eq_with(&m113, &CtxB));
    assert!(!m113.eq_with(&m114, &CtxB));
    assert!(!m113.eq_with(&m124, &CtxB));

    assert!(!m114.eq_with(&m111, &CtxA));
    assert!(!m114.eq_with(&m113, &CtxA));
    assert!(m114.eq_with(&m114, &CtxA));
    assert!(!m114.eq_with(&m124, &CtxA));
    assert!(m114.eq_with(&m111, &CtxB));
    assert!(!m114.eq_with(&m113, &CtxB));
    assert!(m114.eq_with(&m114, &CtxB));
    assert!(!m114.eq_with(&m124, &CtxB));

    assert!(!m124.eq_with(&m111, &CtxA));
    assert!(!m124.eq_with(&m113, &CtxA));
    assert!(!m124.eq_with(&m114, &CtxA));
    assert!(m124.eq_with(&m124, &CtxA));
    assert!(!m124.eq_with(&m111, &CtxB));
    assert!(!m124.eq_with(&m113, &CtxB));
    assert!(!m124.eq_with(&m114, &CtxB));
    assert!(m124.eq_with(&m124, &CtxB));
}

pub struct CtxN(u32);

#[derive(Equivalence)]
#[equiv(rel(name = "mod_n", ty = "CtxN"))]
pub enum MList {
    Nil,
    Cons(
        //TODO: why does this need a type annotation?
        #[equiv(fwd(map = "|x: &u32, c: &CtxN| *x % c.0"))] u32,
        #[equiv(fwd)] Box<MList>,
    ),
}

impl From<&[u32]> for MList {
    fn from(value: &[u32]) -> Self {
        if value.len() == 0 {
            MList::Nil
        } else {
            MList::Cons(value[0], Box::new((&value[1..]).into()))
        }
    }
}

#[test]
fn m_list() {
    let n = MList::Nil;
    let x = MList::from(&[1, 2, 3, 4, 5][..]);
    let y = MList::from(&[6, 7, 8, 9, 10][..]);

    assert_eq!(n.cmp_with(&x, &CtxN(5)), Ordering::Less);
    assert!(x.eq_with(&y, &CtxN(5)));
    assert!(!x.eq_with(&y, &CtxN(2)));
    assert!(!x.eq_with(&y, &CtxN(3)));
    assert!(!x.eq_with(&y, &CtxN(6)));
}
