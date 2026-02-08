//! Tab bar — horizontal tab strip for multi-panel tiles with drag support.

use crate::hooks::use_dock_actions;
use crate::prelude::*;
use dock_proto::{NodeId, TabGroup};

/// Horizontal tab bar shown when a tile has multiple panels.
/// Tabs are draggable for rearrangement.
#[component]
pub fn DockTabBar(node_id: NodeId, tabs: TabGroup) -> Element {
    let actions = use_dock_actions();

    rsx! {
        div {
            class: "flex flex-row bg-muted border-b border-border text-xs overflow-x-auto",
            for (i, panel_id) in tabs.panels.iter().enumerate() {
                {
                    let is_active = i == tabs.active_index;
                    let panel = *panel_id;
                    let bg = if is_active {
                        "bg-accent text-foreground border-b-2 border-b-blue-500"
                    } else {
                        "bg-muted text-muted-foreground hover:text-foreground hover:bg-accent border-b-2 border-b-transparent"
                    };
                    rsx! {
                        div {
                            class: "flex items-center gap-1 px-3 py-1.5 cursor-pointer border-r border-border/50 {bg} transition-all duration-100 select-none",
                            draggable: "true",
                            ondragstart: {
                                let actions = actions.clone();
                                move |_: DragEvent| {
                                    actions.start_drag.call((panel, node_id));
                                }
                            },
                            ondragend: {
                                let actions = actions.clone();
                                move |_: DragEvent| {
                                    actions.cancel_drag.call(());
                                }
                            },
                            onclick: {
                                let actions = actions.clone();
                                move |_| {
                                    actions.switch_tab.call((node_id, i));
                                }
                            },
                            span { class: "truncate max-w-[120px]", "{panel.display_name()}" }
                            // Close tab button (only show if more than 1 tab)
                            if tabs.panels.len() > 1 {
                                button {
                                    class: "ml-1 text-muted-foreground hover:text-foreground rounded-sm hover:bg-accent px-0.5 transition-colors",
                                    onclick: {
                                        let actions = actions.clone();
                                        move |e: MouseEvent| {
                                            e.stop_propagation();
                                            actions.remove_tab.call((node_id, panel));
                                        }
                                    },
                                    "\u{2715}"
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
