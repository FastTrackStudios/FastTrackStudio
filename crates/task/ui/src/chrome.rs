//! Persistent app chrome — the desktop top bar plus the quick-capture
//! and timer widgets that live in it (and in the sidebar / bottom bar).
//!
//! Three pieces, all org-scoped off the shared [`OrgSelection`] context:
//!
//! - **Fleeting capture** — a single global modal ([`FleetingModal`])
//!   toggled by a [`FleetingOpen`] context signal, so a lightweight
//!   [`FleetingButton`] anywhere (top bar, sidebar, bottom bar) opens
//!   the same capture form. Captures land in the inbox via
//!   `upsert_inbox_item` (the FLAP "capture" step).
//! - **Timer widget** — [`TimerWidget`]: a compact live clock with
//!   start / stop over the org's `TimerService`, mirroring `/timer`.
//! - **Top bar** — [`TopBar`]: desktop-only sticky header, right
//!   aligned, with at-a-glance stat chips + the two widgets.
//!
//! All three read/write the shared optimistic stores
//! ([`crate::stores`]), so a capture or a timer start made *anywhere*
//! (these widgets, `/inbox`, `/timer`) updates the chips and the clock
//! instantly — no refresh counter, no extra round-trips.

use chrono::Utc;
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Feather, Inbox as InboxIcon, Play, Square};
use fts_ui::prelude::*;
use uuid::Uuid;

use crate::orgs::{OrgMeta, OrgSelection};
use crate::routes::Route;

// ── shared context ──────────────────────────────────────────────────

/// Visibility of the single global fleeting-capture modal. Provided
/// once by the app shell; flipped by any [`FleetingButton`].
#[derive(Clone, Copy)]
pub struct FleetingOpen(pub Signal<bool>);

/// Install the chrome contexts. Call once in the app shell.
pub fn provide_chrome_contexts() {
    use_context_provider(|| FleetingOpen(Signal::new(false)));
}

/// Shell panel state: the vault explorer (left) — toggled from the
/// top bar, Obsidian-style.
#[derive(Clone, Copy, PartialEq)]
pub struct ExplorerOpen(pub bool);

/// Shell panel state: the right (backlinks/context) panel.
#[derive(Clone, Copy, PartialEq)]
pub struct RightPanelOpen(pub bool);

pub(crate) fn use_fleeting_open() -> Signal<bool> {
    use_context::<FleetingOpen>().0
}

// ── top bar ─────────────────────────────────────────────────────────

/// Desktop-only sticky top bar: right-aligned stat chips + the
/// fleeting-capture button + the timer widget. Hidden on mobile, where
/// the bottom bar carries the fleeting button instead.
#[component]
pub fn TopBar() -> Element {
    let selection = use_context::<Signal<OrgSelection>>();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();

    let target = use_memo(move || resolve_org(&selection.read(), &org_list.read()));

    // Both chips derive from the shared stores, so a capture or a
    // stopped timer made anywhere updates them instantly (the widgets
    // below mutate the same stores).
    let inbox_rows = crate::stores::use_inbox_list();
    let session_rows = crate::stores::use_session_list();

    // Open inbox items still to review (excludes processed / archived
    // and not-yet-due snoozes), for the at-a-glance chip.
    let today = Utc::now().date_naive().to_string();
    let inbox_count = inbox_rows.value().map_or(0, |rows| {
        rows.iter()
            .filter(|(_, it)| {
                it.is_open()
                    && it
                        .resurface_on
                        .as_deref()
                        .is_none_or(|d| d <= today.as_str())
            })
            .count()
    });

    // Time logged today (completed sessions only — the running one is
    // shown live in the timer widget), scoped to the active org+owner.
    let today_date = Utc::now().date_naive();
    let logged: i64 = match (target(), session_rows.value()) {
        (Some((slug, org_id)), Some(rows)) => {
            let owner = owner_id(org_id);
            rows.iter()
                .map(|(_, r)| r)
                .filter(|r| {
                    r.slug == slug
                        && r.session.user_id == owner
                        && r.session.start_time.date_naive() == today_date
                })
                .filter_map(|r| {
                    r.session
                        .end_time
                        .map(|e| (e - r.session.start_time).num_seconds())
                })
                .sum()
        }
        _ => 0,
    };

    // Obsidian-shaped window bar: sidebar toggle at the far left,
    // the tab strip in the middle (one live "tab" today — the open
    // view; a real tab system arrives with vault-views slice 4),
    // actions + the right-panel toggle at the far right.
    let mut explorer = use_context::<Signal<ExplorerOpen>>();
    let mut right_panel = use_context::<Signal<RightPanelOpen>>();
    let route = use_route::<Route>();
    let title = crate::nav::route_title(&route);

    rsx! {
        div {
            class: "z-20 hidden h-10 shrink-0 items-center gap-2 border-b border-border bg-card/60 px-2 md:flex",
            button {
                r#type: "button",
                class: "flex h-7 w-7 items-center justify-center rounded-md text-muted-foreground hover:bg-accent/50 hover:text-foreground",
                title: "Toggle vault sidebar",
                onclick: move |_| {
                    let cur = explorer.peek().0;
                    explorer.set(ExplorerOpen(!cur));
                },
                fts_ui::lucide_dioxus::PanelLeft { size: 15 }
            }
            // The tab strip region — today the single open view.
            div { class: "flex min-w-0 flex-1 items-center gap-1 px-1",
                div { class: "flex max-w-56 items-center gap-2 rounded-md bg-accent/60 px-3 py-1 text-xs text-foreground",
                    span { class: "truncate", "{title}" }
                }
            }

            StatChip {
                icon: rsx! { InboxIcon { size: 14 } },
                label: "{inbox_count} to review",
                route: Route::InboxRoute {},
            }
            StatChip {
                icon: rsx! { fts_ui::lucide_dioxus::Clock { size: 14 } },
                label: "Today {fmt_hms(logged)}",
                route: Route::TimerRoute {},
            }

            div { class: "mx-1 h-5 w-px bg-border" }

            FleetingButton { compact: true }
            TimerWidget {}

            // Who's here — avatar group opening the full roster.
            crate::presence::PresenceAvatarBar {}

            button {
                r#type: "button",
                class: "ml-1 flex h-7 w-7 items-center justify-center rounded-md text-muted-foreground hover:bg-accent/50 hover:text-foreground",
                title: "Toggle right panel",
                onclick: move |_| {
                    let cur = right_panel.peek().0;
                    right_panel.set(RightPanelOpen(!cur));
                },
                fts_ui::lucide_dioxus::PanelRight { size: 15 }
            }
        }
    }
}

/// A small clickable stat pill that navigates to `route`.
#[component]
fn StatChip(icon: Element, label: String, route: Route) -> Element {
    rsx! {
        Link {
            to: route,
            class: "flex items-center gap-1.5 rounded-full border border-border bg-card/40 px-2.5 py-1 text-xs text-muted-foreground transition-colors hover:text-foreground",
            span { class: "flex h-3.5 w-3.5 items-center justify-center", {icon} }
            span { "{label}" }
        }
    }
}

// ── fleeting capture ────────────────────────────────────────────────

/// A button that opens the global fleeting-capture modal. `compact`
/// renders icon-only (for the top bar); otherwise icon + label (for
/// the sidebar / bottom bar).
#[component]
pub fn FleetingButton(#[props(default = false)] compact: bool) -> Element {
    let mut open = use_fleeting_open();
    if compact {
        rsx! {
            button {
                class: "flex items-center gap-1.5 rounded-lg bg-primary px-2.5 py-1.5 text-sm font-medium text-primary-foreground transition-colors hover:bg-primary/80",
                title: "Capture a fleeting note",
                onclick: move |_| open.set(true),
                Feather { size: 15 }
                span { class: "hidden xl:inline", "Capture" }
            }
        }
    } else {
        rsx! {
            button {
                class: "flex w-full items-center gap-2 rounded-lg bg-primary/10 px-3 py-2 text-sm font-medium text-primary transition-colors hover:bg-primary/20",
                onclick: move |_| open.set(true),
                span { class: "flex h-4 w-4 items-center justify-center", Feather { size: 16 } }
                span { "Fleeting note" }
            }
        }
    }
}

/// The single global capture modal. Render once (in the app shell). A
/// textarea + Capture; Enter submits, Shift+Enter newlines, Esc / click
/// outside cancels. Captures into the active org's inbox.
#[component]
pub fn FleetingModal() -> Element {
    let mut open = use_fleeting_open();
    let selection = use_context::<Signal<OrgSelection>>();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();
    let target = use_memo(move || resolve_org(&selection.read(), &org_list.read()));
    let muts = crate::stores::use_inbox_mutations();

    let mut draft = use_signal(String::new);

    if !open() {
        return rsx! {};
    }

    // Optimistic: the capture lands in the shared inbox store
    // instantly (chip + /inbox update), persists in the background,
    // and rolls back + notifies on failure.
    let mut submit = move || {
        let text = draft.peek().trim().to_string();
        if text.is_empty() {
            open.set(false);
            return;
        }
        let Some((slug, _)) = target() else {
            return;
        };
        let id = uuid::Uuid::new_v4().to_string();
        let created = Utc::now().to_rfc3339();
        muts.capture(
            slug,
            inbox_proto::InboxItem::capture(id, text, "ui", created),
        );
        draft.set(String::new());
        open.set(false);
    };

    rsx! {
        div {
            class: "fixed inset-0 z-50 flex items-start justify-center bg-black/40 p-4 pt-[12vh]",
            onclick: move |_| open.set(false),
            div {
                class: "flex w-full max-w-lg flex-col gap-3 rounded-xl border border-border bg-card p-4 shadow-xl",
                onclick: move |e| e.stop_propagation(),
                div { class: "flex items-center gap-2 text-sm text-muted-foreground",
                    Feather { size: 15 }
                    span { "Fleeting note" }
                    span { class: "ml-auto text-xs", "↵ to capture · esc to close" }
                }
                textarea {
                    class: "min-h-[7rem] w-full resize-none rounded-lg border border-input bg-input/30 px-3 py-2 text-sm outline-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 placeholder:text-muted-foreground",
                    placeholder: "Get it out of your head…",
                    // `autofocus` only fires on initial page load, not when
                    // the modal is inserted dynamically — focus on mount.
                    onmounted: move |e: Event<MountedData>| {
                        spawn(async move {
                            let _ = e.data().set_focus(true).await;
                        });
                    },
                    value: "{draft}",
                    oninput: move |e| draft.set(e.value()),
                    onkeydown: move |e| {
                        if e.key() == Key::Enter && !e.modifiers().contains(Modifiers::SHIFT) {
                            e.prevent_default();
                            submit();
                        } else if e.key() == Key::Escape {
                            open.set(false);
                        }
                    },
                }
                div { class: "flex items-center justify-end gap-2",
                    Button {
                        variant: ButtonVariant::Ghost,
                        size: ButtonSize::Small,
                        on_click: move |_| open.set(false),
                        "Cancel"
                    }
                    Button {
                        variant: ButtonVariant::Primary,
                        size: ButtonSize::Small,
                        on_click: move |_| submit(),
                        "Capture"
                    }
                }
            }
        }
    }
}

/// Mobile floating action button for fleeting capture — a circular
/// button pinned bottom-right above the tab bar. Hidden on desktop
/// (the top bar + sidebar carry the capture button there).
#[component]
pub fn FleetingFab() -> Element {
    let mut open = use_fleeting_open();
    rsx! {
        button {
            class: "fixed bottom-24 right-4 z-30 flex h-12 w-12 items-center justify-center rounded-full bg-primary text-primary-foreground shadow-lg transition-transform active:scale-95 md:hidden",
            style: "margin-bottom: env(safe-area-inset-bottom, 0px);",
            title: "Capture a fleeting note",
            onclick: move |_| open.set(true),
            Feather { size: 20 }
        }
    }
}

// ── timer widget ────────────────────────────────────────────────────

/// Compact live timer: when a session is running, a pulsing dot +
/// elapsed clock + Stop; otherwise a small description input + Start.
/// Org-scoped, mirrors `/timer`.
#[component]
pub fn TimerWidget() -> Element {
    let selection = use_context::<Signal<OrgSelection>>();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();
    let target = use_memo(move || resolve_org(&selection.read(), &org_list.read()));

    // The running session is derived from the shared session store —
    // the same rows /timer renders, so the widget and the page can
    // never disagree.
    let session_rows = crate::stores::use_session_list();
    let muts = crate::stores::use_timer_mutations();
    let active: Option<crate::stores::OrgSession> = target().and_then(|(slug, org_id)| {
        let owner = owner_id(org_id);
        session_rows.value().and_then(|rows| {
            rows.iter()
                .map(|(_, r)| r)
                .find(|r| {
                    r.slug == slug && r.session.user_id == owner && r.session.end_time.is_none()
                })
                .cloned()
        })
    });

    // Live clock — re-render once a second so the running elapsed advances.
    let tick = use_signal(|| 0u64);
    use_second_tick(tick);
    let _ = tick();

    let mut draft = use_signal(String::new);

    // Optimistic: the running card appears/clears instantly and
    // reconciles against the server (rollback + tray on failure).
    let mut start = move || {
        let Some((slug, org_id)) = target() else {
            return;
        };
        let desc = draft.peek().trim().to_string();
        draft.set(String::new());
        muts.start(
            slug,
            timer_proto::StartTimerRequest {
                user_id: owner_id(org_id),
                org_id,
                project_id: None,
                project_path: String::new(),
                task_note_path: String::new(),
                description: desc,
            },
        );
    };

    let active_for_stop = active.clone();
    let stop = move || {
        let Some((slug, org_id)) = target() else {
            return;
        };
        let Some(open) = active_for_stop.as_ref() else {
            return;
        };
        muts.stop(slug, owner_id(org_id), open.session.id);
    };

    if target().is_none() {
        return rsx! {};
    }

    match active.as_ref().map(|r| &r.session) {
        Some(s) => {
            let elapsed = (Utc::now() - s.start_time).num_seconds();
            let title = if s.description.trim().is_empty() {
                "Tracking".to_string()
            } else {
                s.description.clone()
            };
            rsx! {
                div { class: "flex items-center gap-2 rounded-lg border border-emerald-500/40 bg-emerald-500/5 py-1 pl-2.5 pr-1.5",
                    span { class: "relative flex size-2",
                        span { class: "absolute inline-flex size-full animate-ping rounded-full bg-emerald-400/70" }
                        span { class: "relative inline-flex size-2 rounded-full bg-emerald-400" }
                    }
                    span { class: "max-w-[10rem] truncate text-xs text-muted-foreground", "{title}" }
                    span { class: "font-mono text-sm font-semibold tabular-nums", "{fmt_hms(elapsed)}" }
                    button {
                        class: "flex h-6 w-6 items-center justify-center rounded-md text-muted-foreground transition-colors hover:bg-destructive/10 hover:text-destructive",
                        title: "Stop timer",
                        onclick: move |_| stop(),
                        Square { size: 14 }
                    }
                }
            }
        }
        None => {
            rsx! {
                div { class: "flex items-center gap-1 rounded-lg border border-border bg-card/40 py-1 pl-2.5 pr-1",
                    input {
                        class: "w-32 bg-transparent text-xs outline-none placeholder:text-muted-foreground focus:w-44 xl:w-40 xl:focus:w-56",
                        placeholder: "Start a timer…",
                        value: "{draft}",
                        oninput: move |e| draft.set(e.value()),
                        onkeydown: move |e| {
                            if e.key() == Key::Enter {
                                start();
                            }
                        },
                    }
                    button {
                        class: "flex h-6 w-6 items-center justify-center rounded-md text-muted-foreground transition-colors hover:bg-emerald-500/10 hover:text-emerald-400",
                        title: "Start timer",
                        onclick: move |_| start(),
                        Play { size: 14 }
                    }
                }
            }
        }
    }
}

// ── shared helpers (also used by pages::timer) ─────────────────────

/// Resolve the selection to `(slug, org_id)`: the chosen org in `One`
/// mode, else the home org. `None` until the org list (with ids) loads.
pub(crate) fn resolve_org(sel: &OrgSelection, orgs: &[OrgMeta]) -> Option<(String, Uuid)> {
    let meta = match sel {
        OrgSelection::One(slug) => orgs.iter().find(|o| &o.slug == slug),
        OrgSelection::All => orgs.iter().find(|o| o.is_home).or_else(|| orgs.first()),
    }?;
    Some((meta.slug.clone(), meta.id?))
}

/// Stable per-org "local owner" user id — a single-user stand-in until
/// auth threads a real signed-in user id through. Deterministic so
/// start / stop / list all key on the same user.
pub(crate) fn owner_id(org_id: Uuid) -> Uuid {
    Uuid::new_v5(&org_id, b"task-local-owner")
}

/// `HH:MM:SS` from a (possibly negative, clamped) second count.
pub(crate) fn fmt_hms(secs: i64) -> String {
    let s = secs.max(0);
    format!("{:02}:{:02}:{:02}", s / 3600, (s % 3600) / 60, s % 60)
}

/// Re-render once a second so running clocks advance. Wasm sleeps via
/// `gloo-timers`; native parks (chrome is web-only today).
pub(crate) fn use_second_tick(mut tick: Signal<u64>) {
    use_future(move || async move {
        loop {
            sleep_one_second().await;
            tick += 1;
        }
    });
}

#[cfg(target_arch = "wasm32")]
async fn sleep_one_second() {
    gloo_timers::future::TimeoutFuture::new(1000).await;
}

#[cfg(not(target_arch = "wasm32"))]
async fn sleep_one_second() {
    futures_util::future::pending::<()>().await;
}
