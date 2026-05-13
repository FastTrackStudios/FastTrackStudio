//! `VaultPicker` — small `Dropdown` for switching the active vault.
//!
//! Pure dumb component. The "Open folder…" action is grayed out on
//! `wasm32` (no filesystem) and only fires on native.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{ChevronDown, FolderOpen, Library, Plus};
use fts_ui::prelude::*;
use knowledge_proto::Vault;
use uuid::Uuid;

#[component]
pub fn VaultPicker(
    vaults: Vec<Vault>,
    active: Option<Uuid>,
    on_pick: EventHandler<Uuid>,
    on_new_in_memory: EventHandler<()>,
    on_open_folder: EventHandler<()>,
) -> Element {
    let active_label = active
        .and_then(|id| vaults.iter().find(|v| v.id == id))
        .map(|v| v.name.clone())
        .unwrap_or_else(|| "No vault".to_string());

    #[cfg(target_arch = "wasm32")]
    let open_folder_disabled = true;
    #[cfg(not(target_arch = "wasm32"))]
    let open_folder_disabled = false;

    let mut index = 0usize;
    let new_inmem_index = vaults.len();
    let open_folder_index = vaults.len() + 1;

    rsx! {
        Dropdown {
            DropdownTrigger {
                button {
                    class: "inline-flex items-center gap-2 rounded-md border border-border bg-card px-2 py-1 text-sm hover:bg-accent",
                    Library { size: 14 }
                    span { class: "truncate max-w-[10rem]", "{active_label}" }
                    ChevronDown { size: 12 }
                }
            }
            DropdownContent {
                DropdownLabel { "Vaults" }
                for v in vaults.iter().cloned() {
                    {
                        let vid = v.id;
                        let name = v.name.clone();
                        let path = v.root_path.clone().unwrap_or_else(|| "in-memory".to_string());
                        let is_active = Some(vid) == active;
                        let idx = index;
                        index += 1;
                        rsx! {
                            DropdownItem {
                                key: "{vid}",
                                value: vid.to_string(),
                                index: idx,
                                on_select: move |_| on_pick.call(vid),
                                div { class: "flex w-full items-center justify-between gap-3",
                                    div { class: "flex flex-col min-w-0",
                                        span { class: "truncate text-sm", "{name}" }
                                        span { class: "truncate text-xs text-muted-foreground", "{path}" }
                                    }
                                    if is_active {
                                        span { class: "text-xs text-primary", "active" }
                                    }
                                }
                            }
                        }
                    }
                }
                DropdownSeparator {}
                DropdownItem {
                    value: "__new_in_memory__".to_string(),
                    index: new_inmem_index,
                    on_select: move |_| on_new_in_memory.call(()),
                    icon: rsx! { Plus { size: 14 } },
                    "New in-memory vault"
                }
                DropdownItem {
                    value: "__open_folder__".to_string(),
                    index: open_folder_index,
                    disabled: open_folder_disabled,
                    on_select: move |_| {
                        if !open_folder_disabled {
                            on_open_folder.call(());
                        }
                    },
                    icon: rsx! { FolderOpen { size: 14 } },
                    if open_folder_disabled {
                        span { class: "flex items-center gap-2",
                            "Open folder…"
                            span { class: "text-xs text-muted-foreground", "(native only)" }
                        }
                    } else {
                        "Open folder…"
                    }
                }
            }
        }
    }
}
