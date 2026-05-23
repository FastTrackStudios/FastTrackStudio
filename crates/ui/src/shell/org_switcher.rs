//! Organization switcher + per-org theme picker.
//!
//! Reads `Signal<Organization>` + [`OrgThemeOverrides`] from
//! context (provided by [`crate::app::App`]). Theme picker is a
//! `Popover` over the workspace's `ThemeSwitcher`.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::Palette;
use fts_ui::prelude::*;
use fts_ui::primitives::{ContentAlign, ContentSide};

use crate::data::{Organization, organizations};
use crate::theming::{OrgThemeOverrides, state_from_preset_name};

#[component]
pub fn OrgSwitcher(
    #[props(default = Vec::new())] orgs: Vec<Organization>,
    #[props(default = false)] compact: bool,
) -> Element {
    let mut active_org = use_context::<Signal<Organization>>();
    let mut org_overrides = use_context::<OrgThemeOverrides>();
    let mut open = use_signal(|| false);
    let mut theme_open = use_signal(|| false);
    let orgs = if orgs.is_empty() {
        organizations()
    } else {
        orgs
    };
    let active = active_org();

    let mut switcher_state = use_signal(|| {
        let name = org_overrides
            .map
            .read()
            .get(active.id)
            .cloned()
            .unwrap_or_else(|| active.theme_preset.to_string());
        let mode = *org_overrides.mode.read();
        state_from_preset_name(&name, mode)
    });

    use_effect(move || {
        let org = active_org.read().clone();
        let name = org_overrides
            .map
            .read()
            .get(org.id)
            .cloned()
            .unwrap_or_else(|| org.theme_preset.to_string());
        let prev_mode = switcher_state.peek().mode;
        if switcher_state.peek().preset == name {
            return;
        }
        switcher_state.set(state_from_preset_name(&name, prev_mode));
    });

    let active_id_for_effect: &'static str = active.id;
    use_effect(move || {
        let name = switcher_state.read().preset.clone();
        let prev = org_overrides.map.read().get(active_id_for_effect).cloned();
        if prev.as_deref() != Some(name.as_str()) {
            let mut m = org_overrides.map.write();
            m.insert(active_id_for_effect.to_string(), name);
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
        HStack { class: if compact { "items-center gap-1" } else { "items-center gap-1 w-full" },
            Dropdown {
                open: open(),
                on_open_change: move |o| open.set(o),
                class: if compact { "" } else { "w-full flex-1" },
                DropdownTrigger { class: if compact { "" } else { "w-full" },
                    button {
                        r#type: "button",
                        class: "flex items-center gap-2 rounded-full border border-border bg-card px-2.5 py-1.5 text-xs font-semibold text-foreground hover:bg-accent",
                        "{active.name}"
                    }
                }
                DropdownContent {
                    side: if compact { "bottom" } else { "top" },
                    align: "end",
                    width: "w-64",
                    DropdownLabel { "Switch organization" }
                    for (idx, org) in orgs.iter().enumerate() {
                        {
                            let org_for_select = org.clone();
                            rsx! {
                                DropdownItem {
                                    key: "{org.id}",
                                    value: org.id.to_string(),
                                    index: idx,
                                    on_select: move |_| {
                                        active_org.set(org_for_select.clone());
                                        open.set(false);
                                    },
                                    "{org.name}"
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
                        class: "inline-flex h-8 w-8 items-center justify-center rounded-md border border-border bg-card text-muted-foreground hover:bg-accent hover:text-accent-foreground",
                        title: "Organization theme",
                        onclick: move |_| {
                            let v = !*theme_open.read();
                            theme_open.set(v);
                        },
                        Palette { size: 14 }
                    }
                }
                PopoverContent {
                    side: ContentSide::Top,
                    align: ContentAlign::End,
                    class: "w-[17rem] p-3 max-h-[70vh] overflow-y-auto",
                    div { class: "flex flex-col gap-2",
                        span { class: "text-xs font-semibold uppercase tracking-widest text-muted-foreground",
                            "Theme · {active.name}"
                        }
                        ThemeSwitcher { state: switcher_state }
                    }
                }
            }
        }
    }
}
