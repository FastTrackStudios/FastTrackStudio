//! Module Editor View — placeholder showing predefined module templates.
//!
//! Displays the Drive Module and Amp/Cab Module slot layouts with their
//! predefined block positions. Full editing functionality is a future phase.

use crate::prelude::*;

/// Predefined module template for display.
struct ModuleTemplate {
    name: &'static str,
    color: &'static str,
    slots: &'static [ModuleSlot],
}

struct ModuleSlot {
    label: &'static str,
    block_type_label: &'static str,
    icon: &'static str,
}

const DRIVE_MODULE: ModuleTemplate = ModuleTemplate {
    name: "Drive Module",
    color: "text-orange-400",
    slots: &[
        ModuleSlot {
            label: "Boost",
            block_type_label: "Boost",
            icon: "B",
        },
        ModuleSlot {
            label: "Drive 1",
            block_type_label: "Drive",
            icon: "D",
        },
        ModuleSlot {
            label: "Drive 2",
            block_type_label: "Drive",
            icon: "D",
        },
        ModuleSlot {
            label: "Drive 3",
            block_type_label: "Drive",
            icon: "D",
        },
    ],
};

const AMP_CAB_MODULE: ModuleTemplate = ModuleTemplate {
    name: "Amp/Cab Module",
    color: "text-amber-400",
    slots: &[
        ModuleSlot {
            label: "Amp 1",
            block_type_label: "Amp",
            icon: "A",
        },
        ModuleSlot {
            label: "Amp 2",
            block_type_label: "Amp",
            icon: "A",
        },
        ModuleSlot {
            label: "Cab 1",
            block_type_label: "Cab",
            icon: "C",
        },
        ModuleSlot {
            label: "Cab 2",
            block_type_label: "Cab",
            icon: "C",
        },
        ModuleSlot {
            label: "Room Send",
            block_type_label: "Send",
            icon: "S",
        },
    ],
};

#[component]
pub fn ModuleEditorView() -> Element {
    let templates = [&DRIVE_MODULE, &AMP_CAB_MODULE];

    rsx! {
        div { class: "h-full w-full flex flex-col bg-card overflow-hidden",
            div { class: "flex-1 flex min-h-0 overflow-hidden",

                // ── Left: Module Type List ───────────────────────
                div { class: "w-56 flex-shrink-0 border-r border-border flex flex-col min-h-0 bg-zinc-950/30",
                    div { class: "px-3 py-2 border-b border-border flex-shrink-0",
                        h3 { class: "text-[10px] font-semibold text-muted-foreground uppercase tracking-wider",
                            "Module Templates"
                        }
                    }
                    div { class: "flex-1 overflow-y-auto px-1.5 py-1.5",
                        for template in templates.iter() {
                            div { class: "px-3 py-2.5 rounded-lg mb-1 border border-transparent hover:bg-accent/30 \
                                          cursor-pointer transition-all",
                                div { class: "flex items-center gap-2",
                                    span { class: "text-xs font-medium {template.color}",
                                        "{template.name}"
                                    }
                                    span { class: "text-[9px] text-muted-foreground bg-muted px-1.5 rounded-full",
                                        "{template.slots.len()} slots"
                                    }
                                }
                            }
                        }
                    }
                }

                // ── Center: Module Slot Layout ───────────────────
                div { class: "flex-1 flex flex-col min-h-0 min-w-0 overflow-y-auto p-4",
                    for template in templates.iter() {
                        div { class: "mb-6",
                            h3 { class: "text-sm font-semibold {template.color} mb-3",
                                "{template.name}"
                            }
                            div { class: "flex flex-wrap gap-3",
                                for slot in template.slots.iter() {
                                    // Block slot card
                                    div { class: "w-40 border border-border rounded-xl bg-card hover:border-primary/30 \
                                                  transition-all cursor-pointer group",
                                        // Header
                                        div { class: "px-3 py-2 border-b border-border/50",
                                            div { class: "flex items-center gap-2",
                                                div { class: "w-6 h-6 rounded flex items-center justify-center text-[10px] \
                                                              font-bold bg-muted text-muted-foreground",
                                                    "{slot.icon}"
                                                }
                                                div {
                                                    p { class: "text-xs font-medium text-foreground", "{slot.label}" }
                                                    p { class: "text-[9px] text-muted-foreground", "{slot.block_type_label}" }
                                                }
                                            }
                                        }
                                        // Preset slot (empty)
                                        div { class: "px-3 py-3 text-center",
                                            p { class: "text-[10px] text-muted-foreground/50 italic",
                                                "Empty"
                                            }
                                            p { class: "text-[9px] text-muted-foreground/30 mt-1",
                                                "Assign from Block Library"
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // ── Right: Block Library Browser ─────────────────
                div { class: "w-64 flex-shrink-0 border-l border-border flex flex-col min-h-0 bg-zinc-950/30",
                    div { class: "px-3 py-2 border-b border-border flex-shrink-0",
                        h3 { class: "text-[10px] font-semibold text-muted-foreground uppercase tracking-wider",
                            "Block Library"
                        }
                    }
                    div { class: "flex-1 flex items-center justify-center",
                        div { class: "text-center px-4",
                            p { class: "text-xs text-muted-foreground mb-1",
                                "Block presets you capture in the Block Editor will appear here"
                            }
                            p { class: "text-[10px] text-muted-foreground/50",
                                "Drag blocks into module slots to assign them"
                            }
                        }
                    }
                }
            }

            // ── Bottom status ────────────────────────────────────
            div { class: "px-3 py-1.5 border-t border-border flex items-center gap-2 flex-shrink-0 bg-zinc-900/40",
                span { class: "text-[10px] text-muted-foreground",
                    "Module Editor — assign block presets to module slots"
                }
            }
        }
    }
}
