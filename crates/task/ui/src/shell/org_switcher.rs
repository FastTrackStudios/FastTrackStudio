//! Organization switcher + per-org theme picker.
//!
//! The dropdown drives the **data** selection — `All` (every hosted
//! org, the default) or a single org — via the `Signal<OrgSelection>`
//! context; the hosted-org list comes from the `Signal<Vec<OrgMeta>>`
//! context (discovered at app start). The Palette button is the
//! per-org theme picker, unchanged.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Building2, Palette};
use fts_ui::prelude::*;
use fts_ui::primitives::{ContentAlign, ContentSide};

use crate::orgs::{OrgMeta, OrgSelection, home_slug};
use crate::theming::{OrgThemeOverrides, state_from_preset_name};

/// `rail`: icon-only skin for the icon rail's foot — a Building2
/// button for the org menu and the Palette button below it, stacked;
/// the selection label moves into the button tooltip.
#[component]
pub fn OrgSwitcher(
    #[props(default = false)] compact: bool,
    #[props(default = false)] rail: bool,
) -> Element {
    let mut org_overrides = use_context::<OrgThemeOverrides>();
    // Data selection + discovered org list.
    let mut selection = use_context::<Signal<OrgSelection>>();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();

    let mut open = use_signal(|| false);
    let mut theme_open = use_signal(|| false);

    // Theme edits apply to the active org (the selected one, or home
    // under "All"), keyed by slug in the overrides map.
    let active_slug = use_memo(move || match &*selection.read() {
        OrgSelection::One(slug) => slug.clone(),
        OrgSelection::All => home_slug(&org_list.read()),
    });

    let list = org_list();
    let current = selection();
    let trigger_label = current.label(&list);

    // ── theme picker state (unchanged) ──────────────────────────────
    let switcher_state = use_signal(|| {
        let name = org_overrides
            .map
            .read()
            .get(&active_slug())
            .cloned()
            .unwrap_or_default();
        let mode = *org_overrides.mode.read();
        state_from_preset_name(&name, mode)
    });

    use_effect(move || {
        let name = switcher_state.read().preset.clone();
        let slug = active_slug();
        // Guard only — peek, so this effect doesn't subscribe to the
        // map it writes (the `prev != name` check keeps it stable, but
        // a subscription would still re-run it on every other slug's
        // theme change).
        let prev = org_overrides.map.peek().get(&slug).cloned();
        if prev.as_deref() != Some(name.as_str()) {
            let mut m = org_overrides.map.write();
            m.insert(slug, name);
        }
    });

    let mut org_mode = org_overrides.mode;
    use_effect(move || {
        let mode = switcher_state.read().mode;
        if *org_mode.peek() != mode {
            org_mode.set(mode);
        }
    });

    rsx! {
        HStack {
            class: if rail {
                "flex-col items-center gap-0.5"
            } else if compact {
                "items-center gap-1"
            } else {
                "items-center gap-1 w-full"
            },
            Dropdown {
                open: open(),
                on_open_change: move |o| open.set(o),
                class: if compact || rail { "" } else { "w-full flex-1" },
                DropdownTrigger { class: if compact || rail { "" } else { "w-full" },
                    if rail {
                        button {
                            r#type: "button",
                            class: "flex h-8 w-8 items-center justify-center rounded-lg text-muted-foreground hover:bg-accent/50 hover:text-foreground",
                            title: "Organization — {trigger_label}",
                            Building2 { size: 15 }
                        }
                    } else {
                        button {
                            r#type: "button",
                            class: "flex items-center gap-2 rounded-full border border-border bg-card px-2.5 py-1.5 text-xs font-semibold text-foreground hover:bg-accent",
                            span { class: "max-w-[10rem] truncate", "{trigger_label}" }
                        }
                    }
                }
                DropdownContent {
                    side: if compact { "bottom" } else { "top" },
                    align: if rail { "start" } else { "end" },
                    width: "w-64",
                    DropdownLabel { "View organization" }
                    // All — the default aggregate view.
                    DropdownItem {
                        value: "__all".to_string(),
                        index: 0,
                        on_select: move |_| {
                            selection.set(OrgSelection::All);
                            open.set(false);
                        },
                        div { class: "flex w-full items-center justify-between gap-2",
                            span { "All organizations" }
                            if current == OrgSelection::All {
                                span { class: "text-xs text-primary", "●" }
                            }
                        }
                    }
                    for (idx, org) in list.iter().enumerate() {
                        {
                            let slug = org.slug.clone();
                            let selected = current == OrgSelection::One(slug.clone());
                            rsx! {
                                DropdownItem {
                                    key: "{org.slug}",
                                    value: org.slug.clone(),
                                    index: idx + 1,
                                    on_select: move |_| {
                                        selection.set(OrgSelection::One(slug.clone()));
                                        open.set(false);
                                    },
                                    div { class: "flex w-full items-center justify-between gap-2",
                                        span { class: "truncate", "{org.name}" }
                                        if selected {
                                            span { class: "text-xs text-primary", "●" }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            Popover {
                open: theme_open(),
                is_modal: false,
                on_open_change: move |o| theme_open.set(o),
                PopoverTrigger { class: "inline-flex",
                    button {
                        r#type: "button",
                        class: if rail {
                            "flex h-8 w-8 items-center justify-center rounded-lg text-muted-foreground hover:bg-accent/50 hover:text-foreground"
                        } else {
                            "inline-flex h-8 w-8 items-center justify-center rounded-md border border-border bg-card text-muted-foreground hover:bg-accent hover:text-accent-foreground"
                        },
                        title: "Theme",
                        onclick: move |_| {
                            let v = !*theme_open.read();
                            theme_open.set(v);
                        },
                        Palette { size: if rail { 15 } else { 14 } }
                    }
                }
                PopoverContent {
                    side: ContentSide::Top,
                    align: if rail { ContentAlign::Start } else { ContentAlign::End },
                    class: "w-[17rem] p-3 max-h-[70vh] overflow-y-auto",
                    div { class: "flex flex-col gap-2",
                        span { class: "text-xs font-semibold uppercase tracking-widest text-muted-foreground",
                            "Theme"
                        }
                        ThemeSwitcher { state: switcher_state }
                    }
                }
            }
        }
    }
}
