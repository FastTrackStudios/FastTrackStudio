//! Provider-agnostic parameter handle.
//!
//! Audio widgets read and write a normalized `f32` in `[0.0, 1.0]`. They do
//! not know about nih_plug, vizia, or any specific param system — consumers
//! construct a [`ParamHandle`] from whatever backend they use.
//!
//! Each handle bundles the closures the widgets need:
//! - read the current normalized value
//! - bracket an edit gesture (`begin_edit` / `end_edit`) for host automation
//! - set the normalized value mid-gesture
//! - format the current value for display
//! - parse a user-typed string back to a normalized value
//!
//! Two handles compare equal iff they share the same `id`. Construct fresh
//! handles via [`ParamHandle::new`] (auto-assigns a unique id) so Dioxus's
//! prop memoization works correctly.

use std::sync::{
    Arc,
    atomic::{AtomicU64, Ordering},
};

static NEXT_ID: AtomicU64 = AtomicU64::new(1);

type Getter<T> = Arc<dyn Fn() -> T + Send + Sync>;
type Action = Arc<dyn Fn() + Send + Sync>;
type Setter = Arc<dyn Fn(f32) + Send + Sync>;
type Parser = Arc<dyn Fn(&str) -> Option<f32> + Send + Sync>;

/// A handle that lets audio widgets drive a single parameter.
#[derive(Clone)]
pub struct ParamHandle {
    id: u64,
    pub(crate) normalized: Getter<f32>,
    pub(crate) begin_edit: Action,
    pub(crate) set_normalized: Setter,
    pub(crate) end_edit: Action,
    pub(crate) display_value: Getter<String>,
    pub(crate) name: Getter<String>,
    pub(crate) string_to_normalized: Parser,
}

impl ParamHandle {
    /// Build a handle from raw closures. Assigns a fresh equality id.
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        normalized: impl Fn() -> f32 + Send + Sync + 'static,
        begin_edit: impl Fn() + Send + Sync + 'static,
        set_normalized: impl Fn(f32) + Send + Sync + 'static,
        end_edit: impl Fn() + Send + Sync + 'static,
        display_value: impl Fn() -> String + Send + Sync + 'static,
        name: impl Fn() -> String + Send + Sync + 'static,
        string_to_normalized: impl Fn(&str) -> Option<f32> + Send + Sync + 'static,
    ) -> Self {
        Self {
            id: NEXT_ID.fetch_add(1, Ordering::Relaxed),
            normalized: Arc::new(normalized),
            begin_edit: Arc::new(begin_edit),
            set_normalized: Arc::new(set_normalized),
            end_edit: Arc::new(end_edit),
            display_value: Arc::new(display_value),
            name: Arc::new(name),
            string_to_normalized: Arc::new(string_to_normalized),
        }
    }

    pub fn id(&self) -> u64 {
        self.id
    }

    pub fn normalized(&self) -> f32 {
        (self.normalized)()
    }
    pub fn begin_edit(&self) {
        (self.begin_edit)()
    }
    pub fn set_normalized(&self, v: f32) {
        (self.set_normalized)(v.clamp(0.0, 1.0))
    }
    pub fn end_edit(&self) {
        (self.end_edit)()
    }
    pub fn display_value(&self) -> String {
        (self.display_value)()
    }
    pub fn name(&self) -> String {
        (self.name)()
    }
    pub fn string_to_normalized(&self, s: &str) -> Option<f32> {
        (self.string_to_normalized)(s)
    }
}

impl PartialEq for ParamHandle {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl std::fmt::Debug for ParamHandle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ParamHandle")
            .field("id", &self.id)
            .field("name", &self.name())
            .finish()
    }
}
