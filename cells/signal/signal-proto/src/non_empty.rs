//! Non-empty collection — structurally guarantees at least one element.
//!
//! `NonEmptyVec<T>` stores the first element separately from the rest,
//! so `len() >= 1` is enforced by construction.

/// A `Vec` that always contains at least one element.
///
/// Used for domain invariants like:
/// - An engine must have at least one layer
/// - A rig must have at least one engine
#[derive(Debug, Clone, PartialEq, Eq, Hash, ::facet::Facet)]
pub struct NonEmptyVec<T: 'static> {
    first: T,
    rest: Vec<T>,
}

impl<T> NonEmptyVec<T> {
    /// Create a `NonEmptyVec` with a single element.
    pub fn new(first: T) -> Self {
        Self {
            first,
            rest: Vec::new(),
        }
    }

    /// Try to create from a `Vec`. Returns `None` if the vec is empty.
    pub fn from_vec(mut vec: Vec<T>) -> Option<Self> {
        if vec.is_empty() {
            return None;
        }
        let rest = vec.split_off(1);
        // SAFETY: we checked non-empty above
        let first = vec.into_iter().next()?;
        Some(Self { first, rest })
    }

    /// Create from a first element and remaining elements.
    pub fn from_parts(first: T, rest: Vec<T>) -> Self {
        Self { first, rest }
    }

    /// Get the first element.
    pub fn first(&self) -> &T {
        &self.first
    }

    /// Get a mutable reference to the first element.
    pub fn first_mut(&mut self) -> &mut T {
        &mut self.first
    }

    /// Get the last element.
    pub fn last(&self) -> &T {
        self.rest.last().unwrap_or(&self.first)
    }

    /// Get the number of elements. Always >= 1.
    pub fn len(&self) -> usize {
        1 + self.rest.len()
    }

    /// Always returns `false` — a `NonEmptyVec` is never empty by construction.
    pub fn is_empty(&self) -> bool {
        false
    }

    /// Push an element to the end.
    pub fn push(&mut self, item: T) {
        self.rest.push(item);
    }

    /// Get an element by index.
    pub fn get(&self, index: usize) -> Option<&T> {
        if index == 0 {
            Some(&self.first)
        } else {
            self.rest.get(index - 1)
        }
    }

    /// Get a mutable element by index.
    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        if index == 0 {
            Some(&mut self.first)
        } else {
            self.rest.get_mut(index - 1)
        }
    }

    /// Iterate over all elements.
    pub fn iter(&self) -> Iter<'_, T> {
        Iter {
            first: Some(&self.first),
            rest: self.rest.iter(),
        }
    }

    /// Iterate mutably over all elements.
    pub fn iter_mut(&mut self) -> IterMut<'_, T> {
        IterMut {
            first: Some(&mut self.first),
            rest: self.rest.iter_mut(),
        }
    }

    /// Convert into a standard `Vec<T>`.
    pub fn into_vec(self) -> Vec<T> {
        let mut v = Vec::with_capacity(1 + self.rest.len());
        v.push(self.first);
        v.extend(self.rest);
        v
    }

    /// Convert into the underlying parts: (first, rest).
    pub fn into_parts(self) -> (T, Vec<T>) {
        (self.first, self.rest)
    }

    /// Map each element, producing a new `NonEmptyVec`.
    pub fn map<U, F: FnMut(T) -> U>(self, mut f: F) -> NonEmptyVec<U> {
        NonEmptyVec {
            first: f(self.first),
            rest: self.rest.into_iter().map(f).collect(),
        }
    }
}

impl<T> NonEmptyVec<T>
where
    T: PartialEq,
{
    /// Check if the collection contains an element.
    pub fn contains(&self, item: &T) -> bool {
        self.first == *item || self.rest.contains(item)
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Iterator types
// ─────────────────────────────────────────────────────────────────────────────

/// Iterator over `&T` elements of a `NonEmptyVec`.
pub struct Iter<'a, T> {
    first: Option<&'a T>,
    rest: std::slice::Iter<'a, T>,
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.first.take().or_else(|| self.rest.next())
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = usize::from(self.first.is_some()) + self.rest.len();
        (remaining, Some(remaining))
    }
}

impl<T> ExactSizeIterator for Iter<'_, T> {}

/// Mutable iterator over `&mut T` elements of a `NonEmptyVec`.
pub struct IterMut<'a, T> {
    first: Option<&'a mut T>,
    rest: std::slice::IterMut<'a, T>,
}

impl<'a, T> Iterator for IterMut<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        self.first.take().or_else(|| self.rest.next())
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = usize::from(self.first.is_some()) + self.rest.len();
        (remaining, Some(remaining))
    }
}

impl<T> ExactSizeIterator for IterMut<'_, T> {}

/// Owned iterator over elements of a `NonEmptyVec`.
pub struct IntoIter<T> {
    first: Option<T>,
    rest: std::vec::IntoIter<T>,
}

impl<T> Iterator for IntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.first.take().or_else(|| self.rest.next())
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = usize::from(self.first.is_some()) + self.rest.len();
        (remaining, Some(remaining))
    }
}

impl<T> ExactSizeIterator for IntoIter<T> {}

impl<T> IntoIterator for NonEmptyVec<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            first: Some(self.first),
            rest: self.rest.into_iter(),
        }
    }
}

impl<'a, T> IntoIterator for &'a NonEmptyVec<T> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, T> IntoIterator for &'a mut NonEmptyVec<T> {
    type Item = &'a mut T;
    type IntoIter = IterMut<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_element() {
        let v = NonEmptyVec::new(42);
        assert_eq!(v.len(), 1);
        assert_eq!(v.first(), &42);
        assert_eq!(v.last(), &42);
    }

    #[test]
    fn from_vec_empty_returns_none() {
        let result: Option<NonEmptyVec<i32>> = NonEmptyVec::from_vec(vec![]);
        assert!(result.is_none());
    }

    #[test]
    fn from_vec_nonempty() {
        let v = NonEmptyVec::from_vec(vec![1, 2, 3]).unwrap();
        assert_eq!(v.len(), 3);
        assert_eq!(v.first(), &1);
        assert_eq!(v.last(), &3);
    }

    #[test]
    fn push_increases_len() {
        let mut v = NonEmptyVec::new(1);
        assert_eq!(v.len(), 1);
        v.push(2);
        assert_eq!(v.len(), 2);
        v.push(3);
        assert_eq!(v.len(), 3);
    }

    #[test]
    fn get_by_index() {
        let v = NonEmptyVec::from_vec(vec![10, 20, 30]).unwrap();
        assert_eq!(v.get(0), Some(&10));
        assert_eq!(v.get(1), Some(&20));
        assert_eq!(v.get(2), Some(&30));
        assert_eq!(v.get(3), None);
    }

    #[test]
    fn iter_yields_all() {
        let v = NonEmptyVec::from_vec(vec![1, 2, 3]).unwrap();
        let collected: Vec<_> = v.iter().copied().collect();
        assert_eq!(collected, vec![1, 2, 3]);
    }

    #[test]
    fn iter_exact_size() {
        let v = NonEmptyVec::from_vec(vec![1, 2, 3]).unwrap();
        let iter = v.iter();
        assert_eq!(iter.len(), 3);
    }

    #[test]
    fn into_iter_consumes() {
        let v = NonEmptyVec::from_vec(vec![1, 2, 3]).unwrap();
        let collected: Vec<_> = v.into_iter().collect();
        assert_eq!(collected, vec![1, 2, 3]);
    }

    #[test]
    fn into_vec_roundtrip() {
        let original = vec![1, 2, 3];
        let nev = NonEmptyVec::from_vec(original.clone()).unwrap();
        assert_eq!(nev.into_vec(), original);
    }

    #[test]
    fn map_transforms() {
        let v = NonEmptyVec::from_vec(vec![1, 2, 3]).unwrap();
        let doubled = v.map(|x| x * 2);
        assert_eq!(doubled.into_vec(), vec![2, 4, 6]);
    }

    #[test]
    fn contains_element() {
        let v = NonEmptyVec::from_vec(vec![1, 2, 3]).unwrap();
        assert!(v.contains(&1));
        assert!(v.contains(&3));
        assert!(!v.contains(&4));
    }

    #[test]
    fn from_parts() {
        let v = NonEmptyVec::from_parts(10, vec![20, 30]);
        assert_eq!(v.len(), 3);
        assert_eq!(v.first(), &10);
    }

    #[test]
    fn into_parts_roundtrip() {
        let v = NonEmptyVec::from_parts(10, vec![20, 30]);
        let (first, rest) = v.into_parts();
        assert_eq!(first, 10);
        assert_eq!(rest, vec![20, 30]);
    }

    #[test]
    fn get_mut_modifies() {
        let mut v = NonEmptyVec::from_vec(vec![1, 2, 3]).unwrap();
        *v.get_mut(0).unwrap() = 100;
        *v.get_mut(2).unwrap() = 300;
        assert_eq!(v.get(0), Some(&100));
        assert_eq!(v.get(2), Some(&300));
    }

    #[test]
    fn iter_mut_modifies_all() {
        let mut v = NonEmptyVec::from_vec(vec![1, 2, 3]).unwrap();
        for item in v.iter_mut() {
            *item *= 10;
        }
        assert_eq!(v.into_vec(), vec![10, 20, 30]);
    }
}
