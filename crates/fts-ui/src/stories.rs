//! `fts-story` registrations for fts-ui components.
//!
//! Each `#[story]` block expands to a static `Story` value plus a
//! `linkme` registration into the global `STORIES` slice. The shell
//! enumerates them; the snapshot harness iterates them × their auto
//! state matrix.
//!
//! Stories assume an enclosing `ThemeProvider` is supplied by the host
//! app (the Lookbook shell wraps everything in one global
//! `ThemeProvider`, which lets the host pick the active preset / mode
//! from the top bar). Story render bodies should NOT instantiate their
//! own provider; doing so breaks the global theme switcher.

use crate::prelude::*;
use dioxus::prelude::*;
use fts_story_runtime::story;

// ── Buttons ──────────────────────────────────────────────────────────────────

/// Single button rendered with the active theme preset.
#[story(
    category = "Buttons",
    name = "primary",
    knobs(
        label = "Click me",
        disabled = false,
    ),
)]
pub fn button_primary(label: &str, disabled: bool) -> Element {
    rsx! {
        div { class: "p-6 bg-background text-foreground",
            Button { variant: ButtonVariant::Primary, disabled, "{label}" }
        }
    }
}

/// All `ButtonVariant` values laid out side by side.
#[story(
    category = "Buttons",
    name = "variants",
    knobs(_marker = false),
)]
pub fn button_variants(_marker: bool) -> Element {
    rsx! {
        div { class: "p-6 bg-background text-foreground flex flex-wrap gap-2",
            Button { variant: ButtonVariant::Primary, "Primary" }
            Button { variant: ButtonVariant::Secondary, "Secondary" }
            Button { variant: ButtonVariant::Outline, "Outline" }
            Button { variant: ButtonVariant::Ghost, "Ghost" }
            Button { variant: ButtonVariant::Destructive, "Destructive" }
            Button { variant: ButtonVariant::Link, "Link" }
        }
    }
}

/// Matrix of every `ButtonVariant` × every state — Enabled, Disabled,
/// Loading. Designed to surface cross-renderer styling drift in one
/// snapshot: rows are variants, columns are states. Each cell is
/// labelled so you can map a divergence in the composite back to the
/// exact (variant, state) pair without counting.
#[story(
    category = "Buttons",
    name = "matrix",
    knobs(_marker = false),
)]
pub fn button_matrix(_marker: bool) -> Element {
    rsx! {
        div { class: "p-6 bg-background text-foreground",
            table { class: "border-collapse",
                thead {
                    tr { class: "text-xs uppercase tracking-wider text-muted-foreground",
                        th { class: "px-3 py-2 text-left", "Variant" }
                        th { class: "px-3 py-2 text-left", "Enabled" }
                        th { class: "px-3 py-2 text-left", "Disabled" }
                        th { class: "px-3 py-2 text-left", "Loading" }
                    }
                }
                tbody {
                    ButtonRow { label: "Primary", variant: ButtonVariant::Primary }
                    ButtonRow { label: "Secondary", variant: ButtonVariant::Secondary }
                    ButtonRow { label: "Outline", variant: ButtonVariant::Outline }
                    ButtonRow { label: "Ghost", variant: ButtonVariant::Ghost }
                    ButtonRow { label: "Destructive", variant: ButtonVariant::Destructive }
                    ButtonRow { label: "Link", variant: ButtonVariant::Link }
                }
            }
        }
    }
}

#[component]
fn ButtonRow(label: String, variant: ButtonVariant) -> Element {
    rsx! {
        tr {
            td { class: "px-3 py-2 text-sm text-muted-foreground", "{label}" }
            td { class: "px-3 py-2",
                Button { variant: variant.clone(), "Click me" }
            }
            td { class: "px-3 py-2",
                Button { variant: variant.clone(), disabled: true, "Click me" }
            }
            td { class: "px-3 py-2",
                Button { variant: variant.clone(), loading: true, "Click me" }
            }
        }
    }
}

/// All button sizes for the Primary variant. Catches per-renderer
/// drift in font scaling, padding, and intrinsic sizing.
#[story(
    category = "Buttons",
    name = "sizes",
    knobs(_marker = false),
)]
pub fn button_sizes(_marker: bool) -> Element {
    rsx! {
        div { class: "p-6 bg-background text-foreground flex flex-wrap items-center gap-3",
            Button { size: ButtonSize::Small, "Small" }
            Button { size: ButtonSize::Medium, "Medium" }
            Button { size: ButtonSize::Large, "Large" }
        }
    }
}

// ── Badges ───────────────────────────────────────────────────────────────────

/// All `BadgeVariant` values rendered with a configurable label.
#[story(
    category = "Badges",
    name = "variants",
    knobs(label = "Badge"),
)]
pub fn badge_variants(label: &str) -> Element {
    rsx! {
        div { class: "p-6 bg-background text-foreground flex flex-wrap gap-2",
            Badge { variant: BadgeVariant::Default, "{label}" }
            Badge { variant: BadgeVariant::Secondary, "{label}" }
            Badge { variant: BadgeVariant::Destructive, "{label}" }
            Badge { variant: BadgeVariant::Outline, "{label}" }
        }
    }
}

// ── Cards ────────────────────────────────────────────────────────────────────

/// Card with header, content, and footer regions.
#[story(
    category = "Cards",
    name = "basic",
    knobs(
        title = "Project Alpha",
        description = "A sample project card.",
    ),
)]
pub fn card_basic(title: &str, description: &str) -> Element {
    rsx! {
        div { class: "p-6 bg-background text-foreground",
            Card {
                CardHeader {
                    CardTitle { "{title}" }
                    CardDescription { "{description}" }
                }
                CardContent {
                    p { class: "text-sm text-muted-foreground", "Card body content." }
                }
                CardFooter {
                    Button { size: ButtonSize::Small, variant: ButtonVariant::Primary, "Action" }
                    Button { size: ButtonSize::Small, variant: ButtonVariant::Ghost, "Cancel" }
                }
            }
        }
    }
}

// ── Diagnostics ─────────────────────────────────────────────────────────────

/// Minimal SVG smoke test — used to isolate which Blitz SVG behaviour
/// is responsible for the missing spinner in the Button matrix.
/// Renders four progressively-simpler variants of the same arc the
/// Button uses so we can see exactly where Blitz diverges.
#[story(
    category = "Diagnostics",
    name = "svg-smoke",
    knobs(_marker = false),
)]
pub fn diag_svg_smoke(_marker: bool) -> Element {
    rsx! {
        div { class: "p-6 bg-background text-foreground flex flex-col gap-6",
            // 1. Bare SVG with explicit width/height + viewBox.
            //    No Tailwind, no currentColor. If this renders an arc,
            //    Blitz can paint paths.
            div { class: "flex items-center gap-3",
                span { class: "w-48 text-sm text-muted-foreground", "explicit w/h, white stroke" }
                svg {
                    width: "32",
                    height: "32",
                    view_box: "0 0 24 24",
                    fill: "none",
                    stroke: "white",
                    stroke_width: "2",
                    path { d: "M21 12a9 9 0 1 1-6.219-8.56" }
                }
            }
            // 2. currentColor stroke — relies on Blitz inheriting the
            //    parent's `color` for the SVG painter.
            div { class: "flex items-center gap-3 text-foreground",
                span { class: "w-48 text-sm text-muted-foreground", "currentColor stroke" }
                svg {
                    width: "32",
                    height: "32",
                    view_box: "0 0 24 24",
                    fill: "none",
                    stroke: "currentColor",
                    stroke_width: "2",
                    path { d: "M21 12a9 9 0 1 1-6.219-8.56" }
                }
            }
            // 3. Tailwind `size-8` instead of explicit width/height.
            //    If this renders nothing, the issue is `size-*` on SVG.
            div { class: "flex items-center gap-3",
                span { class: "w-48 text-sm text-muted-foreground", "size-8 utility" }
                svg {
                    class: "size-8",
                    view_box: "0 0 24 24",
                    fill: "none",
                    stroke: "white",
                    stroke_width: "2",
                    path { d: "M21 12a9 9 0 1 1-6.219-8.56" }
                }
            }
            // 4. Exact spinner from Button: size-4 + currentColor.
            div { class: "flex items-center gap-3 text-foreground",
                span { class: "w-48 text-sm text-muted-foreground", "size-4 + currentColor (Button spinner)" }
                svg {
                    class: "size-4 animate-spin",
                    view_box: "0 0 24 24",
                    fill: "none",
                    stroke: "currentColor",
                    stroke_width: "2",
                    path { d: "M21 12a9 9 0 1 1-6.219-8.56" }
                }
            }
            // 5. Explicit width/height at 16px (no Tailwind). If this
            //    renders an arc but row 4 doesn't, the issue is with
            //    Blitz applying the `size-4` utility to SVG elements
            //    rather than path painting at 16px.
            div { class: "flex items-center gap-3",
                span { class: "w-48 text-sm text-muted-foreground", "explicit 16x16, white stroke" }
                svg {
                    width: "16",
                    height: "16",
                    view_box: "0 0 24 24",
                    fill: "none",
                    stroke: "white",
                    stroke_width: "2",
                    path { d: "M21 12a9 9 0 1 1-6.219-8.56" }
                }
            }
            // 6. Tailwind w-4 + h-4 split into two utilities (instead
            //    of `size-4`). If this renders, the bug is the
            //    `size-*` shorthand.
            div { class: "flex items-center gap-3",
                span { class: "w-48 text-sm text-muted-foreground", "w-4 h-4 split utility" }
                svg {
                    class: "w-4 h-4",
                    view_box: "0 0 24 24",
                    fill: "none",
                    stroke: "white",
                    stroke_width: "2",
                    path { d: "M21 12a9 9 0 1 1-6.219-8.56" }
                }
            }
            // 7. size-4 WITHOUT animate-spin. Isolates whether the
            //    issue is the rotation or the size class itself.
            div { class: "flex items-center gap-3",
                span { class: "w-48 text-sm text-muted-foreground", "size-4 (no animate-spin)" }
                svg {
                    class: "size-4",
                    view_box: "0 0 24 24",
                    fill: "none",
                    stroke: "white",
                    stroke_width: "2",
                    path { d: "M21 12a9 9 0 1 1-6.219-8.56" }
                }
            }
            // 8. size-8 + animate-spin. Confirms the spin animation
            //    works at larger sizes.
            div { class: "flex items-center gap-3",
                span { class: "w-48 text-sm text-muted-foreground", "size-8 + animate-spin" }
                svg {
                    class: "size-8 animate-spin",
                    view_box: "0 0 24 24",
                    fill: "none",
                    stroke: "white",
                    stroke_width: "2",
                    path { d: "M21 12a9 9 0 1 1-6.219-8.56" }
                }
            }
            // 9. 16x16 SVG with a STATIC `transform: rotate(45deg)`.
            //    No animation. If this renders, the bug is the
            //    keyframes / animation system, not transforms in
            //    general at small sizes.
            div { class: "flex items-center gap-3",
                span { class: "w-48 text-sm text-muted-foreground", "16x16 + static rotate(45deg)" }
                svg {
                    width: "16",
                    height: "16",
                    style: "transform: rotate(45deg);",
                    view_box: "0 0 24 24",
                    fill: "none",
                    stroke: "white",
                    stroke_width: "2",
                    path { d: "M21 12a9 9 0 1 1-6.219-8.56" }
                }
            }
            // 10. 16x16 SVG wrapped in a div with the spin animation.
            //     Isolates whether the bug is "animation on SVG" vs
            //     "animation on small element regardless of type".
            div { class: "flex items-center gap-3",
                span { class: "w-48 text-sm text-muted-foreground", "div size-4 + animate-spin (no SVG)" }
                div {
                    class: "size-4 animate-spin",
                    style: "background: white;",
                }
            }
            // 11. size-4 + animate-spin + stroke="white" (no
            //     currentColor). If this renders, the bug is
            //     specifically `currentColor` at small sizes — usvg
            //     resolves currentColor to black when the SVG markup
            //     has no `color` attribute, and Blitz doesn't inject
            //     the computed color before parsing. If this still
            //     vanishes, the bug is animate-spin on small SVGs
            //     regardless of stroke source.
            div { class: "flex items-center gap-3",
                span { class: "w-48 text-sm text-muted-foreground", "size-4 + animate-spin + white stroke" }
                svg {
                    class: "size-4 animate-spin",
                    view_box: "0 0 24 24",
                    fill: "none",
                    stroke: "white",
                    stroke_width: "2",
                    path { d: "M21 12a9 9 0 1 1-6.219-8.56" }
                }
            }
            // 12. size-4 + stroke="currentColor" WITHOUT animate-spin.
            //     If this vanishes (or renders black-on-dark and is
            //     functionally invisible), the bug is `currentColor`
            //     at small sizes independent of animation. Pairs with
            //     test 2 (currentColor at 32px ✓) — if 12 fails and 2
            //     passes, the threshold is size, not currentColor in
            //     general.
            div { class: "flex items-center gap-3 text-foreground",
                span { class: "w-48 text-sm text-muted-foreground", "size-4 + currentColor (no animate-spin)" }
                svg {
                    class: "size-4",
                    view_box: "0 0 24 24",
                    fill: "none",
                    stroke: "currentColor",
                    stroke_width: "2",
                    path { d: "M21 12a9 9 0 1 1-6.219-8.56" }
                }
            }
        }
    }
}

/// Regression test for cross-renderer layout parity. Renders 8 rows
/// of `flex items-center gap-3` (text label + 32×32 box) inside
/// `flex-col gap-6`. Originally a bisection probe that pinned a 1 px
/// per-row vertical drift between Blitz and wry to a single root
/// cause: Xvfb's default DPI (75) made webkit scale every CSS-pixel
/// value by ~96/75, while Blitz's CPU rasteriser uses CSS-px ==
/// device-px at scale 1.0. Pinning Xvfb to `-dpi 96` brought parity
/// from `dssim ≈ 0.085` to `≈ 0.003`. Keep this story so any future
/// regression in flex / gap / line-height / DPI handling shows up
/// here before the heavier Button-matrix tests fail.
#[story(
    category = "Diagnostics",
    name = "layout-stack",
    knobs(_marker = false),
)]
pub fn diag_layout_stack(_marker: bool) -> Element {
    rsx! {
        div { class: "p-6 bg-background text-foreground flex flex-col gap-6",
            for i in 0..8 {
                div { class: "flex items-center gap-3",
                    span { class: "w-48 text-sm text-muted-foreground", "row {i} text-sm label" }
                    div {
                        style: "width: 32px; height: 32px; background: white;",
                    }
                }
            }
        }
    }
}

/// Force-link helper — referenced from the binary's `main` so LTO
/// can't drop the static registrations. Each `#[story]` macro emits
/// a registration as a `static` item; without a code path that
/// touches it, the linker may strip it from the final binary.
pub fn force_link() {
    let _ = (
        &BUTTON_PRIMARY_STORY,
        &BUTTON_VARIANTS_STORY,
        &BUTTON_MATRIX_STORY,
        &BUTTON_SIZES_STORY,
        &BADGE_VARIANTS_STORY,
        &CARD_BASIC_STORY,
        &DIAG_SVG_SMOKE_STORY,
        &DIAG_LAYOUT_STACK_STORY,
    );
}
