//! Static class mappings for [`ColorTag`].
//!
//! Tailwind v4 only inlines classes it sees as *literal* strings in
//! source. Constructing `bg-violet-500/90` via `format!()` would
//! silently drop the rule. We map each [`ColorTag`] to a hard-coded
//! class set so every variant the calendar can emit is literally
//! present in this file — and therefore in the compiled CSS.
//!
//! Visuals follow Google Calendar / Apple Calendar conventions:
//! solid colored body, brighter left accent bar, hover ring.

use crate::types::ColorTag;

/// Class set used for time-grid event chips (week/day) and month-view
/// chips:
///
/// - `body`  — fill + text color + accent border
/// - `hover` — hover-only emphasis (ring + brighten)
#[derive(Clone, Copy, Debug)]
pub struct ChipPalette {
    pub body: &'static str,
    pub hover: &'static str,
}

#[must_use]
pub fn chip_palette(color: ColorTag) -> ChipPalette {
    match color {
        ColorTag::Neutral => ChipPalette {
            body: "bg-slate-600/90 text-white border-l-4 border-slate-300",
            hover: "hover:bg-slate-600 hover:ring-1 hover:ring-slate-300/60",
        },
        ColorTag::Primary => ChipPalette {
            body: "bg-violet-600/90 text-white border-l-4 border-violet-300",
            hover: "hover:bg-violet-600 hover:ring-1 hover:ring-violet-300/60",
        },
        ColorTag::Success => ChipPalette {
            body: "bg-emerald-600/90 text-white border-l-4 border-emerald-300",
            hover: "hover:bg-emerald-600 hover:ring-1 hover:ring-emerald-300/60",
        },
        ColorTag::Warning => ChipPalette {
            body: "bg-amber-500/90 text-amber-50 border-l-4 border-amber-200",
            hover: "hover:bg-amber-500 hover:ring-1 hover:ring-amber-200/60",
        },
        ColorTag::Danger => ChipPalette {
            body: "bg-rose-600/90 text-white border-l-4 border-rose-300",
            hover: "hover:bg-rose-600 hover:ring-1 hover:ring-rose-300/60",
        },
        ColorTag::Info => ChipPalette {
            body: "bg-sky-600/90 text-white border-l-4 border-sky-300",
            hover: "hover:bg-sky-600 hover:ring-1 hover:ring-sky-300/60",
        },
    }
}

/// Faint dashed-outline class set for a day-plan template "ghost"
/// block — a dim placement guide that sits behind real events. Same
/// literal-class requirement as [`chip_palette`]: every variant is
/// spelled out so Tailwind keeps the rules.
#[must_use]
pub fn template_palette(color: ColorTag) -> &'static str {
    match color {
        ColorTag::Neutral => "border-slate-400/40 bg-slate-400/[0.07] text-slate-300/80",
        ColorTag::Primary => "border-violet-400/40 bg-violet-400/[0.07] text-violet-200/80",
        ColorTag::Success => "border-emerald-400/40 bg-emerald-400/[0.07] text-emerald-200/80",
        ColorTag::Warning => "border-amber-400/40 bg-amber-400/[0.08] text-amber-100/80",
        ColorTag::Danger => "border-rose-400/40 bg-rose-400/[0.07] text-rose-200/80",
        ColorTag::Info => "border-sky-400/40 bg-sky-400/[0.07] text-sky-200/80",
    }
}
