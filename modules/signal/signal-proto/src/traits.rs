//! Core traits for the collection/variant architecture.
//!
//! - [`Variant`]: A named configuration within a collection.
//! - [`DefaultVariant`]: Factory for creating a default variant with a given name.
//! - [`Collection`]: An ordered group of [`Variant`]s with a designated default.
//! - [`HasMetadata`]: Provides access to [`Metadata`](crate::metadata::Metadata) for tagging/description.

use crate::metadata::{Metadata, Tags};

// ─── Variant ────────────────────────────────────────────────────

/// A named state snapshot within a collection.
///
/// Variants are the leaves of the signal hierarchy — each one captures
/// a concrete configuration (parameters, block choices, overrides, etc.).
pub trait Variant {
    type Id: PartialEq;

    fn variant_id(&self) -> &Self::Id;
    fn variant_name(&self) -> &str;
}

// ─── DefaultVariant ─────────────────────────────────────────────

/// Factory for producing a sensible default variant.
pub trait DefaultVariant: Variant {
    fn default_named(name: &str) -> Self;
}

// ─── Collection ─────────────────────────────────────────────────

/// An ordered group of variants with a designated default.
///
/// # Default Normalization Contract
///
/// After any mutation the caller should invoke [`normalize_default`](Collection::normalize_default).
/// The rules are:
///
/// 1. **Empty collection** → inject a default variant via [`DefaultVariant::default_named`].
/// 2. **Non-empty but default missing** → promote the first variant.
/// 3. **Valid** → no-op.
pub trait Collection {
    type Variant: Variant;

    fn variants(&self) -> &[Self::Variant];
    fn variants_mut(&mut self) -> &mut Vec<Self::Variant>;

    fn default_variant_id(&self) -> &<Self::Variant as Variant>::Id;
    fn set_default_variant_id(&mut self, id: <Self::Variant as Variant>::Id);

    /// Look up the designated default variant.
    fn default_variant(&self) -> Option<&Self::Variant> {
        let default_id = self.default_variant_id();
        self.variants()
            .iter()
            .find(|v| v.variant_id() == default_id)
    }

    /// Ensure the collection satisfies the normalization contract.
    fn normalize_default(&mut self)
    where
        Self::Variant: DefaultVariant,
        <Self::Variant as Variant>::Id: Clone,
    {
        if self.variants().is_empty() {
            let fallback = Self::Variant::default_named("Default");
            let id = fallback.variant_id().clone();
            self.variants_mut().push(fallback);
            self.set_default_variant_id(id);
            return;
        }

        let default_id = self.default_variant_id();
        let found = self.variants().iter().any(|v| v.variant_id() == default_id);
        if !found {
            if let Some(first) = self.variants().first() {
                let id = first.variant_id().clone();
                self.set_default_variant_id(id);
            }
        }
    }
}

// ─── HasMetadata ────────────────────────────────────────────────

/// Provides access to metadata (tags, description, notes).
pub trait HasMetadata {
    fn metadata(&self) -> &Metadata;
    fn metadata_mut(&mut self) -> &mut Metadata;
}

/// Convenience trait for items that are tagged.
pub trait Tagged: HasMetadata {
    fn tags(&self) -> &Tags {
        &self.metadata().tags
    }
}

/// Convenience trait for items that carry a description.
pub trait Described: HasMetadata {
    fn description(&self) -> Option<&str> {
        self.metadata().description.as_deref()
    }
}

// Blanket impls
impl<T: HasMetadata> Tagged for T {}
impl<T: HasMetadata> Described for T {}

#[cfg(test)]
mod tests {
    use super::*;

    // Minimal test types

    #[derive(Debug, Clone, PartialEq)]
    struct TestId(String);

    #[derive(Debug, Clone)]
    struct TestVariant {
        id: TestId,
        name: String,
    }

    impl Variant for TestVariant {
        type Id = TestId;
        fn variant_id(&self) -> &TestId {
            &self.id
        }
        fn variant_name(&self) -> &str {
            &self.name
        }
    }

    impl DefaultVariant for TestVariant {
        fn default_named(name: &str) -> Self {
            Self {
                id: TestId(format!("{}-default", name.to_lowercase())),
                name: name.to_string(),
            }
        }
    }

    struct TestCollection {
        variants: Vec<TestVariant>,
        default_id: TestId,
    }

    impl Collection for TestCollection {
        type Variant = TestVariant;

        fn variants(&self) -> &[TestVariant] {
            &self.variants
        }
        fn variants_mut(&mut self) -> &mut Vec<TestVariant> {
            &mut self.variants
        }
        fn default_variant_id(&self) -> &TestId {
            &self.default_id
        }
        fn set_default_variant_id(&mut self, id: TestId) {
            self.default_id = id;
        }
    }

    #[test]
    fn normalize_injects_default_when_empty() {
        let mut coll = TestCollection {
            variants: vec![],
            default_id: TestId("gone".into()),
        };
        coll.normalize_default();
        assert_eq!(coll.variants().len(), 1);
        assert_eq!(coll.variants()[0].variant_name(), "Default");
        assert_eq!(coll.default_variant_id(), coll.variants()[0].variant_id());
    }

    #[test]
    fn normalize_promotes_first_when_default_missing() {
        let a = TestVariant {
            id: TestId("a".into()),
            name: "Alpha".into(),
        };
        let b = TestVariant {
            id: TestId("b".into()),
            name: "Beta".into(),
        };
        let mut coll = TestCollection {
            variants: vec![a, b],
            default_id: TestId("gone".into()),
        };
        coll.normalize_default();
        assert_eq!(coll.default_variant_id(), &TestId("a".into()));
    }

    #[test]
    fn normalize_noop_when_valid() {
        let a = TestVariant {
            id: TestId("a".into()),
            name: "Alpha".into(),
        };
        let mut coll = TestCollection {
            variants: vec![a],
            default_id: TestId("a".into()),
        };
        coll.normalize_default();
        assert_eq!(coll.variants().len(), 1);
        assert_eq!(coll.default_variant_id(), &TestId("a".into()));
    }
}
