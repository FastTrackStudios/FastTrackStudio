//! Proc-macros for `fts-visual-testing`.
//!
//! Currently a stub. The MVP target is:
//!
//! ```ignore
//! /// Standard CTA button.
//! #[story(category = "Buttons")]
//! fn button(
//!     /// Visual style.
//!     #[knob(default = ButtonVariant::Primary)]
//!     variant: ButtonVariant,
//!     /// Disable interaction.
//!     #[knob(default = false)]
//!     disabled: bool,
//!     /// Label text.
//!     #[knob(default = "Click me")]
//!     label: &str,
//! ) -> Element {
//!     rsx! { Button { variant, disabled, "{label}" } }
//! }
//! ```
//!
//! Expansion plan (MVP):
//!
//! 1. Parse the fn signature, pull each arg's `#[knob(default = …)]` attr
//!    and rustdoc comment.
//! 2. Infer `KnobKind` from the arg type (`bool`, primitive number, `&str`,
//!    `String`, anything else → `Opaque` until enum/derive support lands).
//! 3. Emit:
//!    - the original `fn button(...)`, unchanged.
//!    - a `pub static BUTTON_STORY_KNOBS: [KnobSpec; N]`.
//!    - a `pub static BUTTON_STORY: Story` with `render` set to a
//!      generated thunk that reads knobs out of `&dyn KnobSource` and
//!      calls the user fn.
//!    - `#[linkme::distributed_slice(STORIES)] static __BUTTON_REG: &Story
//!      = &BUTTON_STORY;`.
//!
//! Until the macro lands, stories can be declared by hand against
//! `fts-vt-core::Story` for early bring-up of the shell + VRT.

use proc_macro::TokenStream;

#[proc_macro_attribute]
pub fn story(_args: TokenStream, input: TokenStream) -> TokenStream {
    // TODO: implement. For now, leave the user fn untouched so the crate
    // builds. This lets us scaffold the shell + VRT against hand-rolled
    // `Story` values first.
    input
}

#[proc_macro_attribute]
pub fn story_test(_args: TokenStream, input: TokenStream) -> TokenStream {
    // TODO: parse a returned `InteractionScript` and register via
    // `#[distributed_slice(INTERACTION_SCRIPTS)]`.
    input
}

#[proc_macro_attribute]
pub fn states(_args: TokenStream, input: TokenStream) -> TokenStream {
    // TODO: emit a `&'static [StateAssignment]` and wire it into the
    // sibling story's `auto_states` field.
    input
}
