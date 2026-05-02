//! Dioxus-aware runtime for `fts-story`.
//!
//! Re-exports `fts-story-core` and `fts-story-macros` so consumers only need
//! one dep, and provides:
//!
//! - The concrete render-thunk signature stored in
//!   [`Story::render`](fts_story_core::Story::render). The opaque
//!   `*const ()` in core is cast back to [`RenderFn`] by callers.
//! - [`KnobSource`] — runtime trait the macro-generated thunk uses to
//!   read knob values out of the shell or VRT runner.
//! - Helpers for executing [`Interaction`](fts_story_core::Interaction)
//!   steps against a `dioxus_core::VirtualDom` (and, in
//!   `fts-story-snapshots`, a `DioxusDocument`).
//!
//! This crate intentionally has no Blitz / wgpu dep. It works in any
//! Dioxus 0.7 context — desktop, web, native — so the interactive
//! shell can run anywhere.

pub use fts_story_core::*;
pub use fts_story_macros::{states, story, story_test};

use dioxus::prelude::Element;

/// Concrete signature stored in [`Story::render`].
///
/// `knobs` is provided by the caller (the shell's knob editor, the VRT
/// runner's matrix iterator, the fuzz harness's mutator). The macro
/// generates a thunk that reads each declared knob out of it via
/// [`KnobSource::get`] and invokes the user-written component fn.
pub type RenderFn = fn(&dyn KnobSource) -> Element;

/// Source of typed knob values for a single render.
///
/// The shell implements this against a `Signal<HashMap<&'static str, Knob
/// Value>>`; the VRT runner implements it against a fixed
/// `&[(&'static str, KnobValue)]` from the state matrix.
pub trait KnobSource {
    fn get(&self, name: &'static str) -> Option<&KnobValue>;
}

/// Convenience — read a knob with a fallback default.
pub fn knob<'a>(src: &'a dyn KnobSource, name: &'static str, fallback: &'a KnobValue) -> &'a KnobValue {
    src.get(name).unwrap_or(fallback)
}

/// Cast a `Story::render` opaque pointer back to a callable.
///
/// # Safety
/// `story.render` must have been produced by the `#[story]` macro (or a
/// hand-written story that points at a `RenderFn`). Mismatched signatures
/// are UB.
#[inline]
pub unsafe fn render_fn(story: &Story) -> RenderFn {
    unsafe { std::mem::transmute::<*const (), RenderFn>(story.render) }
}
