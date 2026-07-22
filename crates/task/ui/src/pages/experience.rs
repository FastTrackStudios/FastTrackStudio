//! **Experiences** — a note can declare an immersive, full-screen view of
//! itself (distinct from the vault's editing "views"). A `type: setlist`
//! note, or any note with an `experience:` frontmatter key, opens straight
//! into its Experience; `Esc` (or the close control) drops back to the
//! normal vault + sidebars.
//!
//! [`FullscreenExperience`] is the chrome: a fixed, viewport-covering
//! overlay (so it escapes the pane/sidebar layout regardless of nesting)
//! with a slim top bar and an `Esc`-to-exit affordance. The experience's
//! own content — the setlist player today, more to come — renders inside.
//!
//! An experience can be configured to auto-open full-screen or stay
//! embedded; the setlist experience auto-opens.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::Minimize2;

/// Which immersive experience a note declares. Parsed from the note's
/// `type:` / `experience:` frontmatter by [`experience_of`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ExperienceKind {
    /// The setlist player — navigator + transport + chart/mixer.
    Setlist,
    /// Inbox triage — process captured items one at a time.
    Inbox,
}

impl ExperienceKind {
    /// Whether this experience auto-opens full-screen on note open.
    pub fn auto_fullscreen(self) -> bool {
        match self {
            // Setlist notes open on the NOTE (the embedded streaming
            // player); the fullscreen rehearsal Experience is entered
            // deliberately via the Setlist button.
            ExperienceKind::Setlist => false,
            // Inbox is entered deliberately (a "Process" button), not on
            // every note open.
            ExperienceKind::Inbox => false,
        }
    }
}

/// Resolve a note's Experience from its `type` and an optional
/// `experience:` frontmatter value. `type: setlist` implies the setlist
/// experience; an explicit `experience: <name>` selects one for a note
/// that isn't typed as one.
pub fn experience_of(note_type: Option<&str>, experience: Option<&str>) -> Option<ExperienceKind> {
    let want = experience.map(str::trim).filter(|s| !s.is_empty());
    match (note_type, want) {
        (Some("setlist"), _) | (_, Some("setlist")) => Some(ExperienceKind::Setlist),
        (_, Some("inbox")) => Some(ExperienceKind::Inbox),
        _ => None,
    }
}

/// Full-screen Experience chrome: a viewport overlay with a top bar
/// (title + exit) that renders `children` beneath. `on_exit` fires on
/// `Esc` or the close control.
#[component]
pub fn FullscreenExperience(
    title: String,
    on_exit: EventHandler<()>,
    /// Handle `Esc` at the chrome level. Set false when the inner content
    /// already binds `Esc` (e.g. the inbox deck) so exit fires once.
    #[props(default = true)]
    handle_esc: bool,
    children: Element,
) -> Element {
    // Double-Esc to exit: the experience can host a vim editor whose own Esc
    // leaves insert mode — a single Esc must NOT drop the whole experience.
    // The first Esc arms; a second within the window exits (the arm auto-clears
    // so a stray Esc leaving insert mode never accumulates into an exit).
    let mut esc_armed = use_signal(|| false);
    rsx! {
        div {
            // `fixed inset-0` escapes the pane/sidebar layout and covers the
            // whole viewport. `tabindex` + autofocus so the Esc keydown lands
            // here immediately (keydown bubbles up from focused children).
            class: "fixed inset-0 z-50 flex flex-col bg-background outline-none",
            tabindex: "0",
            autofocus: true,
            onkeydown: move |e| {
                if handle_esc && e.key() == Key::Escape {
                    if *esc_armed.peek() {
                        e.prevent_default();
                        esc_armed.set(false);
                        on_exit.call(());
                    } else {
                        esc_armed.set(true);
                        spawn(async move {
                            #[cfg(target_arch = "wasm32")]
                            gloo_timers::future::TimeoutFuture::new(900).await;
                            esc_armed.set(false);
                        });
                    }
                }
            },
            // Slim top bar: title on the left, Esc-to-exit on the right.
            div { class: "flex shrink-0 items-center justify-between border-b border-border px-4 py-2",
                span { class: "truncate text-sm font-semibold text-foreground", "{title}" }
                button {
                    class: "flex items-center gap-1.5 rounded-md border border-border px-2 py-1 text-xs text-muted-foreground hover:bg-accent hover:text-foreground",
                    title: "Press Esc twice to exit",
                    onclick: move |_| on_exit.call(()),
                    Minimize2 { class: "size-3.5" }
                    span { class: "font-medium",
                        if esc_armed() { "Esc again to exit" } else { "Esc Esc" }
                    }
                }
            }
            // Experience content fills the rest of the viewport.
            div { class: "flex min-h-0 flex-1 flex-col overflow-hidden", {children} }
        }
    }
}

/// Compact control to (re-)enter the full-screen experience from the
/// embedded fallback view.
#[component]
pub fn EnterExperienceButton(label: String, on_enter: EventHandler<()>) -> Element {
    rsx! {
        div { class: "flex items-center justify-between border-b border-border/60 px-4 py-2",
            span { class: "text-xs font-medium uppercase tracking-wide text-muted-foreground", "{label}" }
            button {
                class: "rounded-md bg-primary px-2.5 py-1 text-xs font-semibold text-primary-foreground hover:bg-primary/90",
                onclick: move |_| on_enter.call(()),
                "Open full-screen"
            }
        }
    }
}
