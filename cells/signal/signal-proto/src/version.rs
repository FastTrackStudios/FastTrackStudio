//! Versioning — timestamped history with typestate enforcement.
//!
//! **`Version<T, State>`**: A versioned wrapper around any data. Drafts can be mutated;
//! committed versions are frozen. You cannot create a `VersionedRef` to a draft.
//!
//! **`VersionedRef<Id, Mode>`**: A reference to a versioned entity. Auto-update mode
//! tracks `saved_at_version` and resolves to latest externally. Pinned mode freezes
//! to a specific version number.
//!
//! **`LayerIndex`**: A 1-based layer index. Cannot be constructed with 0.
//!
//! # Typestate Proof: Cannot create VersionedRef from a Draft
//!
//! ```compile_fail
//! use signal_proto::version::{Version, Draft};
//!
//! let draft = Version::<String, Draft>::new("hello".into(), None);
//! let _ = draft.to_ref::<signal_proto::id::VersionId>(); // ERROR: no method on Draft
//! ```
//!
//! # Typestate Proof: Cannot get pinned_version on AutoUpdate ref
//!
//! ```compile_fail
//! use signal_proto::version::VersionedRef;
//! use signal_proto::id::BlockSnapshotId;
//!
//! let auto_ref = VersionedRef::<BlockSnapshotId>::new(BlockSnapshotId::new(), 1);
//! let _ = auto_ref.pinned_version(); // ERROR: no method on AutoUpdate
//! ```
//!
//! # Typestate Proof: LayerIndex(0) panics
//!
//! ```should_panic
//! use signal_proto::version::LayerIndex;
//! let _ = LayerIndex::new(0); // PANIC: LayerIndex must be >= 1
//! ```

use std::marker::PhantomData;

use crate::id::VersionId;

// ─── Version state markers ───────────────────────────────────────

/// Version is a work-in-progress, mutable draft.
#[derive(..Marker)]
pub struct Draft;

/// Version is frozen and immutable.
#[derive(..Marker)]
pub struct Committed;

// ─── Version<T, State> ──────────────────────────────────────────

/// A versioned wrapper around data `T`.
///
/// - `Draft`: data can be mutated, message can be set.
/// - `Committed`: data is frozen. Can produce `VersionedRef`s.
#[derive(Debug, Clone)]
pub struct Version<T, State = Draft> {
    id: VersionId,
    version_number: Option<u32>,
    created_at: Option<String>,
    data: T,
    parent_version_id: Option<VersionId>,
    message: Option<String>,
    _state: PhantomData<State>,
}

impl<T> Version<T, Draft> {
    /// Create a new draft version.
    pub fn new(data: T, parent: Option<VersionId>) -> Self {
        Self {
            id: VersionId::new(),
            version_number: None,
            created_at: None,
            data,
            parent_version_id: parent,
            message: None,
            _state: PhantomData,
        }
    }

    /// Set a commit message on this draft.
    pub fn set_message(&mut self, msg: impl Into<String>) {
        self.message = Some(msg.into());
    }

    /// Mutate the data in this draft.
    pub fn data_mut(&mut self) -> &mut T {
        &mut self.data
    }

    /// Read the data (available on both Draft and Committed).
    pub fn data(&self) -> &T {
        &self.data
    }

    /// Commit this draft, freezing it with a version number and timestamp.
    pub fn commit(
        self,
        version_number: u32,
        timestamp: impl Into<String>,
    ) -> Version<T, Committed> {
        Version {
            id: self.id,
            version_number: Some(version_number),
            created_at: Some(timestamp.into()),
            data: self.data,
            parent_version_id: self.parent_version_id,
            message: self.message,
            _state: PhantomData,
        }
    }
}

impl<T> Version<T, Committed> {
    /// The frozen data.
    pub fn data(&self) -> &T {
        &self.data
    }

    /// The version number assigned at commit time.
    pub fn version_number(&self) -> u32 {
        self.version_number
            .expect("committed version always has a number")
    }

    /// When this version was committed.
    pub fn created_at(&self) -> &str {
        self.created_at
            .as_deref()
            .expect("committed version always has a timestamp")
    }

    /// The version ID.
    pub fn id(&self) -> VersionId {
        self.id
    }

    /// The parent version this was derived from, if any.
    pub fn parent_version_id(&self) -> Option<VersionId> {
        self.parent_version_id
    }

    /// The commit message, if any.
    pub fn message(&self) -> Option<&str> {
        self.message.as_deref()
    }

    /// Create a `VersionedRef` pointing to this committed version.
    pub fn to_ref<Id: From<VersionId> + Clone + PartialEq>(&self) -> VersionedRef<Id> {
        VersionedRef::new(Id::from(self.id), self.version_number())
    }
}

// ─── VersionedRef mode markers ───────────────────────────────────

/// Reference auto-updates to the latest version.
#[derive(..Marker)]
pub struct AutoUpdate;

/// Reference is pinned to a specific version number.
#[derive(..Marker)]
pub struct Pinned;

// ─── VersionedRef<Id, Mode> ─────────────────────────────────────

/// A reference to a versioned entity.
///
/// - `AutoUpdate`: resolves to the latest version externally; tracks when it was saved.
/// - `Pinned`: frozen to a specific version number.
#[derive(Debug, Clone, PartialEq, ::facet::Facet)]
pub struct VersionedRef<Id: PartialEq, Mode = AutoUpdate> {
    target_id: Id,
    saved_at_version: u32,
    pinned_version: Option<u32>,
    #[facet(skip)]
    _mode: PhantomData<Mode>,
}

impl<Id: Clone + PartialEq> VersionedRef<Id, AutoUpdate> {
    /// Create a new auto-updating reference.
    pub fn new(target_id: Id, saved_at_version: u32) -> Self {
        Self {
            target_id,
            saved_at_version,
            pinned_version: None,
            _mode: PhantomData,
        }
    }

    /// The version this reference was saved at.
    pub fn saved_at_version(&self) -> u32 {
        self.saved_at_version
    }

    /// Pin to a specific version, stopping auto-update.
    pub fn pin(self, version: u32) -> VersionedRef<Id, Pinned> {
        VersionedRef {
            target_id: self.target_id,
            saved_at_version: self.saved_at_version,
            pinned_version: Some(version),
            _mode: PhantomData,
        }
    }
}

impl<Id: Clone + PartialEq> VersionedRef<Id, Pinned> {
    /// The pinned version number.
    pub fn pinned_version(&self) -> u32 {
        self.pinned_version
            .expect("pinned ref always has a version")
    }

    /// Unpin — resume auto-updating from the current saved_at_version.
    pub fn unpin(self) -> VersionedRef<Id, AutoUpdate> {
        VersionedRef {
            target_id: self.target_id,
            saved_at_version: self.saved_at_version,
            pinned_version: None,
            _mode: PhantomData,
        }
    }
}

/// Shared accessors available in any mode.
impl<Id: Clone + PartialEq, Mode> VersionedRef<Id, Mode> {
    /// The target entity this reference points to.
    pub fn target_id(&self) -> &Id {
        &self.target_id
    }
}

// ─── LayerIndex ──────────────────────────────────────────────────

/// A 1-based layer index. Cannot be constructed with 0.
///
/// Layers in an engine are always 1-indexed (Layer 1, Layer 2, Layer 3),
/// matching the physical UX (like Nord's Layer A=1, B=2, C=3).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, ::facet::Facet)]
pub struct LayerIndex(u8);

impl LayerIndex {
    /// Create a 1-based layer index. Panics if index is 0.
    pub fn new(index: u8) -> Self {
        assert!(index >= 1, "LayerIndex must be >= 1, got {index}");
        Self(index)
    }

    /// Try to create a layer index. Returns `None` if index is 0.
    pub fn try_new(index: u8) -> Option<Self> {
        if index >= 1 {
            Some(Self(index))
        } else {
            None
        }
    }

    /// Get the 1-based index value.
    pub fn get(self) -> u8 {
        self.0
    }

    /// Convert to 0-based index for `Vec` access.
    pub fn to_zero_based(self) -> usize {
        (self.0 - 1) as usize
    }
}

impl std::fmt::Display for LayerIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ─── LayerIndex tests ────────────────────────────────────────

    #[test]
    fn layer_index_is_one_based() {
        let idx = LayerIndex::new(1);
        assert_eq!(idx.get(), 1);
        assert_eq!(idx.to_zero_based(), 0);

        let idx3 = LayerIndex::new(3);
        assert_eq!(idx3.get(), 3);
        assert_eq!(idx3.to_zero_based(), 2);
    }

    #[test]
    fn layer_index_try_new() {
        assert!(LayerIndex::try_new(0).is_none());
        assert!(LayerIndex::try_new(1).is_some());
        assert!(LayerIndex::try_new(255).is_some());
    }

    #[test]
    #[should_panic(expected = "LayerIndex must be >= 1")]
    fn layer_index_zero_panics() {
        LayerIndex::new(0);
    }

    #[test]
    fn layer_index_ordering() {
        let a = LayerIndex::new(1);
        let b = LayerIndex::new(2);
        assert!(a < b);
    }

    // ─── Version typestate tests ─────────────────────────────────

    #[test]
    fn draft_can_be_mutated() {
        let mut draft = Version::new(42u32, None);
        assert_eq!(*draft.data(), 42);
        *draft.data_mut() = 99;
        assert_eq!(*draft.data(), 99);
    }

    #[test]
    fn commit_freezes_version() {
        let draft = Version::new("hello".to_string(), None);
        let committed = draft.commit(1, "2026-02-11T00:00:00Z");

        assert_eq!(committed.version_number(), 1);
        assert_eq!(committed.created_at(), "2026-02-11T00:00:00Z");
        assert_eq!(committed.data(), "hello");
    }

    #[test]
    fn draft_with_message() {
        let mut draft = Version::new(0u8, None);
        draft.set_message("initial commit");
        let committed = draft.commit(1, "now");
        assert_eq!(committed.message(), Some("initial commit"));
    }

    #[test]
    fn version_chain() {
        let v1 = Version::new("v1", None).commit(1, "t1");
        let v1_id = v1.id();

        let v2 = Version::new("v2", Some(v1_id)).commit(2, "t2");
        assert_eq!(v2.parent_version_id(), Some(v1_id));
        assert_eq!(v2.version_number(), 2);
    }

    // ─── VersionedRef tests ──────────────────────────────────────

    #[test]
    fn auto_update_ref() {
        let r = VersionedRef::<crate::id::BlockSnapshotId, AutoUpdate>::new(
            crate::id::BlockSnapshotId::new(),
            3,
        );
        assert_eq!(r.saved_at_version(), 3);
    }

    #[test]
    fn pin_and_unpin() {
        let auto_ref = VersionedRef::new(crate::id::BlockSnapshotId::new(), 3);
        let pinned = auto_ref.pin(3);
        assert_eq!(pinned.pinned_version(), 3);

        let unpinned = pinned.unpin();
        assert_eq!(unpinned.saved_at_version(), 3);
    }

    #[test]
    fn committed_version_produces_ref() {
        let committed = Version::new(42u32, None).commit(5, "t5");
        let vref: VersionedRef<VersionId> = committed.to_ref();
        assert_eq!(vref.saved_at_version(), 5);
    }
}
