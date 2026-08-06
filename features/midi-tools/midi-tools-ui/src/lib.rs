//! The MIDI velocity panel — Dioxus.
//!
//! The GUI half of midi-tools. All the arithmetic lives in `midi-tools`;
//! this crate is widgets and layout, and holds a `velocity::Session` as
//! its entire model so the preview is a pure function of the controls.
//!
//! ## Rendering constraints
//!
//! This panel must look the same standalone, as a plugin editor, and
//! embedded in REAPER, so it follows the signal UI rules:
//!
//! - **Inline styles only.** Blitz does not load external CSS reliably,
//!   so every layout-critical value is an inline `style="..."`, and the
//!   theme comes in through `document::Style { {CSS_STR} }` from an
//!   `include_str!` — never `document::Stylesheet { href }`, never
//!   `asset!()`.
//! - **No Tailwind dependency.** Classes would be additive polish at
//!   most; nothing here needs a stylesheet to lay out correctly.
//! - **Propless root.** [`VelocityPanel`] takes no props and reads its
//!   sink from context, so the same component works standalone and as a
//!   hosted panel.
//! - **No `dioxus::desktop::LaunchBuilder` in the crate.** The desktop
//!   (WebView) renderer is the iteration harness only — it lives in
//!   `examples/panel.rs`, never in the shipping path.
//!
//! Colours are `var(--token, fallback)` throughout: the fts theme drives
//! them when it's loaded, and the fallback keeps the panel legible when
//! it isn't.

mod arp_panel;
mod curve_editor;
mod drag;
mod panel;

pub use curve_editor::CurveEditor;
pub use arp_panel::{ArpPanel, ArpSinkHandle};
pub use panel::{SinkHandle, VelocityPanel};
