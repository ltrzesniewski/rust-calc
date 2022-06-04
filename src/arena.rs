use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;

pub struct Arena<'a, T> {
    vec: RefCell<Vec<Vec<T>>>,
    _p: PhantomData<&'a T>,
}

impl<'a, T> Arena<'a, T> {
    const ALLOC_QUANTA: usize = 32;

    pub fn new() -> Arena<'a, T> {
        Arena {
            vec: vec![].into(),
            _p: PhantomData,
        }
    }

    pub fn alloc(&self, item: T) -> &'a T {
        self.alloc_mut(item)
    }

    pub fn alloc_mut(&self, item: T) -> &'a mut T {
        let mut outer = self.vec.borrow_mut();

        let mut inner = outer.last_mut();
        if inner.is_none() {
            outer.push(Vec::with_capacity(Self::ALLOC_QUANTA));
            inner = outer.last_mut();
        }

        let mut inner = inner.unwrap();

        if inner.len() == Self::ALLOC_QUANTA {
            outer.push(Vec::with_capacity(Self::ALLOC_QUANTA));
            inner = outer.last_mut().unwrap();
        }

        let idx = inner.len();
        inner.push(item);
        unsafe { &mut *inner.as_mut_ptr().add(idx) }
    }

    pub fn len(&self) -> usize {
        let outer = self.vec.borrow();
        if let Some(inner) = outer.last() {
            Self::ALLOC_QUANTA * (outer.len() - 1) + inner.len()
        } else {
            0
        }
    }
}

impl<'a, T> Debug for Arena<'a, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Arena of {}", self.len())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_stores_values() {
        let arena = Arena::new();
        assert_eq!(arena.len(), 0);

        for i in 1..=(Arena::<usize>::ALLOC_QUANTA * 10) {
            let result = arena.alloc(i);
            assert_eq!(*result, i);
            assert_eq!(arena.len(), i)
        }
    }
}
