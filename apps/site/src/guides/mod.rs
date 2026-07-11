//! Guides — narrative, step-by-step walkthroughs of FastTrackStudio
//! workflows, built from a small structured data model so guides render
//! consistently (prose, numbered steps, shortcut caps) and stay testable
//! against the real embedded keybind profiles.
//!
//! A guide is multiple pages: `/guides/<id>` is the overview (intro +
//! section cards), `/guides/<id>/<section>` renders one section per page
//! with a sticky cross-page TOC and prev/next navigation.

mod reaper;

pub use reaper::reaper_guide;

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{ArrowLeft, ArrowRight, BookOpen, Keyboard, Lightbulb, Wrench};
use fts_ui::prelude::*;

use crate::Route;
use crate::components::colors::category_color;
use crate::components::input_tutorial::pretty_keys;

// ---------------------------------------------------------------------------
// Data model
// ---------------------------------------------------------------------------

/// One guide: a titled document made of typed sections.
#[derive(Clone, PartialEq)]
pub struct Guide {
    pub id: &'static str,
    pub title: &'static str,
    pub intro: &'static str,
    pub sections: Vec<GuideSection>,
}

/// What kind of material a section covers — each kind gets a distinct
/// icon + accent color in the section header and table of contents.
#[derive(Clone, Copy, PartialEq)]
pub enum SectionKind {
    /// Background / mental model.
    Concept,
    /// Keyboard-driven workflow walkthroughs.
    Input,
    /// Installation / configuration.
    Setup,
}

impl SectionKind {
    pub fn label(self) -> &'static str {
        match self {
            SectionKind::Concept => "Concept",
            SectionKind::Input => "Input",
            SectionKind::Setup => "Setup",
        }
    }

    pub fn accent(self) -> &'static str {
        match self {
            SectionKind::Concept => "#a78bfa",
            SectionKind::Input => "#38bdf8",
            SectionKind::Setup => "#10b981",
        }
    }

    pub fn icon(self) -> Element {
        match self {
            SectionKind::Concept => rsx! { Lightbulb { class: "w-4 h-4" } },
            SectionKind::Input => rsx! { Keyboard { class: "w-4 h-4" } },
            SectionKind::Setup => rsx! { Wrench { class: "w-4 h-4" } },
        }
    }
}

/// One section of a guide — its own page under `/guides/<guide>/<id>`.
#[derive(Clone, PartialEq)]
pub struct GuideSection {
    pub kind: SectionKind,
    pub id: &'static str,
    pub title: &'static str,
    pub body: Vec<GuideBlock>,
}

impl GuideSection {
    /// The section accent: Input sections whose id matches a workflow
    /// category (transport, tracks, …) take that category's color, so the
    /// guide speaks the same color language as /input; other kinds use
    /// their kind accent.
    pub fn accent(&self) -> &'static str {
        match self.kind {
            SectionKind::Input => category_color(self.id),
            kind => kind.accent(),
        }
    }
}

/// A content block. Kept deliberately small — prose paragraphs, numbered
/// steps, shortcut rows (rendered with the same key caps as /input), and
/// a "see everything" link chip into the /input reference.
#[derive(Clone, PartialEq)]
pub enum GuideBlock {
    /// Markdown-lite prose: blank lines split paragraphs.
    Prose(String),
    /// A keybinding, `keys` in the profile's chord syntax (`"<C-t>"`, `"n d"`).
    Shortcut { keys: String, desc: String },
    /// One numbered step (numbering is automatic and per-section).
    Step(String),
    /// Link chip to /input preselecting `category`.
    SeeAll { category: String, label: String },
}

/// Shorthand constructors keeping guide content declarations readable.
pub fn prose(text: &str) -> GuideBlock {
    GuideBlock::Prose(text.to_string())
}

pub fn shortcut(keys: &str, desc: &str) -> GuideBlock {
    GuideBlock::Shortcut { keys: keys.to_string(), desc: desc.to_string() }
}

pub fn step(text: &str) -> GuideBlock {
    GuideBlock::Step(text.to_string())
}

pub fn see_all(category: &str, label: &str) -> GuideBlock {
    GuideBlock::SeeAll { category: category.to_string(), label: label.to_string() }
}

/// Route to a guide's section page. Only the REAPER guide exists today —
/// dispatch on `guide_id` as more guides land.
fn section_route(_guide_id: &str, section_id: &str) -> Route {
    Route::ReaperGuideSectionPage { section: section_id.to_string() }
}

/// Route to a guide's overview page.
fn overview_route(_guide_id: &str) -> Route {
    Route::ReaperGuidePage {}
}

// ---------------------------------------------------------------------------
// Landing page
// ---------------------------------------------------------------------------

/// `/guides` — cards for every available guide.
#[component]
pub fn GuidesLanding() -> Element {
    rsx! {
        div { class: "max-w-7xl mx-auto px-4 lg:px-8 py-10",
            div { class: "mb-10",
                h1 { class: "text-3xl font-bold tracking-tight", "Guides" }
                p { class: "mt-2 text-muted-foreground max-w-3xl",
                    "Hands-on walkthroughs of FastTrackStudio workflows — the "
                    "narrative companion to the reference pages. Start here, then "
                    "keep the "
                    Link { to: Route::InputPage { category: String::new() }, class: "text-primary hover:underline", "shortcut reference" }
                    " open while you work."
                }
            }

            div { class: "grid gap-5 md:grid-cols-2 lg:grid-cols-3",

                // REAPER guide card
                Link {
                    to: Route::ReaperGuidePage {},
                    class: "group relative overflow-hidden rounded-xl border border-border/40 bg-gradient-to-br from-card/50 to-card/10 p-6 transition-all duration-300 hover:-translate-y-1 hover:border-primary/50",
                    div { class: "mb-4 inline-flex h-10 w-10 items-center justify-center rounded-lg bg-primary/10 text-primary",
                        BookOpen { class: "w-5 h-5" }
                    }
                    h3 { class: "text-lg font-semibold mb-1 group-hover:text-primary transition-colors",
                        "REAPER Guide"
                    }
                    p { class: "text-sm text-muted-foreground leading-relaxed",
                        "Drive REAPER the FastTrackStudio way: the input layer, "
                        "transport essentials, and the track workflows — with the "
                        "real shortcuts from the fasttrackstudio profile."
                    }
                    div { class: "mt-4 inline-flex items-center gap-1 text-sm text-primary",
                        "Read the guide"
                        ArrowRight { class: "w-4 h-4 transition-transform group-hover:translate-x-0.5" }
                    }
                }

                // Placeholder — more coming
                div {
                    class: "rounded-xl border border-dashed border-border/50 p-6 flex flex-col items-start justify-center text-muted-foreground/70",
                    div { class: "mb-4 inline-flex h-10 w-10 items-center justify-center rounded-lg bg-muted/30",
                        BookOpen { class: "w-5 h-5" }
                    }
                    h3 { class: "text-lg font-semibold mb-1", "More guides coming" }
                    p { class: "text-sm leading-relaxed",
                        "Session setlists, the Signal rig, and Keyflow charting are next."
                    }
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Guide overview page
// ---------------------------------------------------------------------------

/// `/guides/<id>` — the guide overview: intro + one card per section.
#[component]
pub fn GuideOverviewView(guide: Guide) -> Element {
    rsx! {
        div { class: "max-w-7xl mx-auto px-4 lg:px-8 py-10",

            div { class: "mb-8",
                Link {
                    to: Route::GuidesPage {},
                    class: "text-sm text-muted-foreground hover:text-foreground transition-colors",
                    "\u{2190} All guides"
                }
                h1 { class: "mt-2 text-3xl font-bold tracking-tight", "{guide.title}" }
                p { class: "mt-2 text-muted-foreground max-w-3xl", "{guide.intro}" }
            }

            div { class: "grid gap-5 md:grid-cols-2 lg:grid-cols-3",
                for (i, s) in guide.sections.iter().enumerate() {
                    {
                        let accent = s.accent();
                        let excerpt = section_excerpt(s);
                        rsx! {
                            Link {
                                key: "{s.id}",
                                to: section_route(guide.id, s.id),
                                class: "group relative overflow-hidden rounded-xl border border-border/40 bg-gradient-to-br from-card/50 to-card/10 p-6 transition-all duration-300 hover:-translate-y-1",
                                style: "--gs-accent: {accent};",

                                div { class: "mb-4 flex items-center justify-between",
                                    span {
                                        class: "inline-flex h-10 w-10 items-center justify-center rounded-lg",
                                        style: "color: {accent}; background-color: {accent}1a; border: 1px solid {accent}40;",
                                        {s.kind.icon()}
                                    }
                                    span { class: "font-mono text-xs text-muted-foreground/50 tracking-[0.2em]",
                                        {format!("{:02}", i + 1)}
                                    }
                                }
                                div {
                                    class: "text-[0.65rem] font-mono uppercase tracking-[0.2em] mb-1",
                                    style: "color: {accent};",
                                    {s.kind.label()}
                                }
                                h3 { class: "text-lg font-semibold mb-1 transition-colors group-hover:[color:var(--gs-accent)]",
                                    "{s.title}"
                                }
                                p { class: "text-sm text-muted-foreground leading-relaxed line-clamp-3",
                                    "{excerpt}"
                                }
                                div {
                                    class: "mt-4 inline-flex items-center gap-1 text-sm",
                                    style: "color: {accent};",
                                    "Read section"
                                    ArrowRight { class: "w-4 h-4 transition-transform group-hover:translate-x-0.5" }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// First prose paragraph of a section, for overview cards.
fn section_excerpt(section: &GuideSection) -> String {
    section
        .body
        .iter()
        .find_map(|b| match b {
            GuideBlock::Prose(t) => t.split("\n\n").next().map(|p| p.trim().to_string()),
            _ => None,
        })
        .unwrap_or_default()
}

// ---------------------------------------------------------------------------
// Guide section page
// ---------------------------------------------------------------------------

/// `/guides/<id>/<section>` — one section per page: sticky cross-page TOC
/// (current section highlighted), the section content, prev/next nav.
#[component]
pub fn GuideSectionPageView(guide: Guide, section: String) -> Element {
    let Some(idx) = guide.sections.iter().position(|s| s.id == section) else {
        return rsx! {
            div { class: "max-w-7xl mx-auto px-4 lg:px-8 py-16 text-center",
                p { class: "text-muted-foreground mb-4", "That section doesn't exist." }
                Link {
                    to: overview_route(guide.id),
                    class: "text-primary hover:underline",
                    "Back to {guide.title}"
                }
            }
        };
    };
    let current = &guide.sections[idx];
    let prev = idx.checked_sub(1).and_then(|i| guide.sections.get(i));
    let next = guide.sections.get(idx + 1);

    rsx! {
        div { class: "max-w-7xl mx-auto px-4 lg:px-8 py-10",

            div { class: "mb-8",
                Link {
                    to: overview_route(guide.id),
                    class: "text-sm text-muted-foreground hover:text-foreground transition-colors",
                    "\u{2190} {guide.title}"
                }
            }

            div { class: "flex gap-8 items-start",

                // Cross-page TOC — links between section pages, current
                // one highlighted; clicking the current entry re-anchors
                // to the top of its content.
                nav { class: "hidden lg:block w-56 shrink-0 sticky top-24",
                    div { class: "text-xs uppercase tracking-wider text-muted-foreground mb-2", "In this guide" }
                    for s in guide.sections.iter() {
                        TocEntry {
                            key: "{s.id}",
                            guide_id: guide.id,
                            section: s.clone(),
                            active: s.id == current.id,
                        }
                    }
                }

                // The section
                div { class: "flex-1 min-w-0",
                    GuideSectionView { section: current.clone() }

                    // Prev / next navigation
                    div { class: "mt-12 pt-6 border-t border-border/40 flex items-stretch justify-between gap-4",
                        if let Some(p) = prev {
                            Link {
                                to: section_route(guide.id, p.id),
                                class: "group flex-1 max-w-xs rounded-lg border border-border/50 bg-card/40 px-4 py-3 hover:border-border transition-colors",
                                div { class: "flex items-center gap-1 text-xs text-muted-foreground mb-1",
                                    ArrowLeft { class: "w-3.5 h-3.5" }
                                    "Previous"
                                }
                                div { class: "text-sm font-medium group-hover:text-primary transition-colors", "{p.title}" }
                            }
                        } else {
                            div { class: "flex-1 max-w-xs" }
                        }
                        if let Some(n) = next {
                            Link {
                                to: section_route(guide.id, n.id),
                                class: "group flex-1 max-w-xs rounded-lg border border-border/50 bg-card/40 px-4 py-3 text-right hover:border-border transition-colors",
                                div { class: "flex items-center justify-end gap-1 text-xs text-muted-foreground mb-1",
                                    "Next"
                                    ArrowRight { class: "w-3.5 h-3.5" }
                                }
                                div { class: "text-sm font-medium group-hover:text-primary transition-colors", "{n.title}" }
                            }
                        } else {
                            div { class: "flex-1 max-w-xs" }
                        }
                    }
                }
            }
        }
    }
}

/// One TOC entry: a route link to the section's page. On the current page
/// it also smooth-scrolls back to the section anchor (long sections).
#[component]
fn TocEntry(guide_id: &'static str, section: GuideSection, active: bool) -> Element {
    let accent = section.accent();
    let id = section.id;
    rsx! {
        Link {
            to: section_route(guide_id, section.id),
            class: if active {
                "flex w-full items-center gap-2 px-2 py-1.5 rounded text-sm bg-accent/60 text-foreground"
            } else {
                "flex w-full items-center gap-2 px-2 py-1.5 rounded text-sm text-muted-foreground hover:text-foreground hover:bg-accent/30 transition-colors"
            },
            onclick: move |_| {
                // Same-page click: re-anchor to the section heading.
                let js = format!(
                    "document.getElementById('{id}')?.scrollIntoView({{behavior:'smooth',block:'start'}});"
                );
                let _ = dioxus::document::eval(&js);
            },
            span {
                class: "inline-flex h-6 w-6 shrink-0 items-center justify-center rounded",
                style: "color: {accent}; background-color: {accent}1a;",
                {section.kind.icon()}
            }
            "{section.title}"
        }
    }
}

/// Render one section's header + blocks (anchored by the section id so
/// in-page TOC clicks keep working on long sections).
#[component]
fn GuideSectionView(section: GuideSection) -> Element {
    let accent = section.accent();
    let mut step_no = 0usize;

    rsx! {
        section { id: "{section.id}", class: "scroll-mt-24",

            // Header: kind icon + kind label + title, tinted by the
            // section accent (Input sections use their category color).
            div { class: "flex items-center gap-3 mb-4",
                span {
                    class: "inline-flex h-9 w-9 shrink-0 items-center justify-center rounded-lg",
                    style: "color: {accent}; background-color: {accent}1a; border: 1px solid {accent}40;",
                    {section.kind.icon()}
                }
                div {
                    div {
                        class: "text-[0.65rem] font-mono uppercase tracking-[0.2em]",
                        style: "color: {accent};",
                        {section.kind.label()}
                    }
                    h2 { class: "text-2xl font-semibold leading-tight", "{section.title}" }
                }
            }

            div { class: "space-y-3",
                for (i, block) in section.body.iter().enumerate() {
                    match block {
                        GuideBlock::Prose(text) => rsx! {
                            for (pi, para) in text.split("\n\n").filter(|p| !p.trim().is_empty()).enumerate() {
                                p { key: "{i}-{pi}", class: "text-sm leading-relaxed text-muted-foreground max-w-prose",
                                    "{para.trim()}"
                                }
                            }
                        },
                        GuideBlock::Step(text) => {
                            step_no += 1;
                            let n = step_no;
                            rsx! {
                                div { key: "{i}", class: "flex items-start gap-3 max-w-prose",
                                    span {
                                        class: "mt-0.5 inline-flex h-5 w-5 shrink-0 items-center justify-center rounded-full text-[0.65rem] font-semibold",
                                        style: "color: {accent}; background-color: {accent}1a; border: 1px solid {accent}40;",
                                        "{n}"
                                    }
                                    p { class: "text-sm leading-relaxed text-foreground", "{text}" }
                                }
                            }
                        }
                        GuideBlock::Shortcut { keys, desc } => rsx! {
                            div { key: "{i}",
                                class: "flex items-center gap-4 rounded-lg border border-border/50 bg-card/40 px-4 py-2 max-w-prose",
                                div { class: "w-48 shrink-0 flex items-center gap-1.5 flex-wrap",
                                    for (ci, chord) in pretty_keys(keys).iter().enumerate() {
                                        if ci > 0 {
                                            span { class: "text-muted-foreground/60 text-xs", "then" }
                                        }
                                        span { class: "inline-flex items-center gap-0.5",
                                            for (kii, k) in chord.iter().enumerate() {
                                                if kii > 0 {
                                                    span { class: "text-muted-foreground/60 text-xs", "+" }
                                                }
                                                Kbd { "{k}" }
                                            }
                                        }
                                    }
                                }
                                div { class: "flex-1 text-sm", "{desc}" }
                            }
                        },
                        GuideBlock::SeeAll { category, label } => rsx! {
                            Link {
                                key: "{i}",
                                to: Route::InputPage { category: category.clone() },
                                class: "inline-flex items-center gap-1.5 mt-1 px-3 py-1.5 rounded-full text-xs font-medium border transition-colors",
                                style: "color: {accent}; border-color: {accent}40; background-color: {accent}0d;",
                                "{label}"
                                ArrowRight { class: "w-3.5 h-3.5" }
                            }
                        },
                    }
                }
            }
        }
    }
}
