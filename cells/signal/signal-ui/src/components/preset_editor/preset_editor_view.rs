//! Preset Editor View — placeholder showing module-to-preset composition.
//!
//! Displays a vertical list of module assignments that compose a full rig preset.
//! Full editing functionality is a future phase.

use crate::prelude::*;

/// Module assignment row in a preset.
struct PresetModuleRow {
    position: usize,
    module_type: &'static str,
    icon: &'static str,
    color: &'static str,
    description: &'static str,
}

const PRESET_ROWS: &[PresetModuleRow] = &[
    PresetModuleRow {
        position: 1,
        module_type: "Drive Module",
        icon: "D",
        color: "text-orange-400 bg-orange-400/10 border-orange-400/20",
        description: "Boost + drive pedals",
    },
    PresetModuleRow {
        position: 2,
        module_type: "Amp/Cab Module",
        icon: "A",
        color: "text-amber-400 bg-amber-400/10 border-amber-400/20",
        description: "Amplifier + cabinet + room",
    },
    PresetModuleRow {
        position: 3,
        module_type: "EQ Module",
        icon: "E",
        color: "text-blue-400 bg-blue-400/10 border-blue-400/20",
        description: "Post-amp tone shaping",
    },
    PresetModuleRow {
        position: 4,
        module_type: "Effects Module",
        icon: "F",
        color: "text-purple-400 bg-purple-400/10 border-purple-400/20",
        description: "Delay, reverb, modulation",
    },
];

#[component]
pub fn PresetEditorView() -> Element {
    rsx! {
        div { class: "h-full w-full flex flex-col bg-card overflow-hidden",
            div { class: "flex-1 flex min-h-0 overflow-hidden",

                // ── Left: Preset List ────────────────────────────
                div { class: "w-56 flex-shrink-0 border-r border-border flex flex-col min-h-0 bg-zinc-950/30",
                    div { class: "px-3 py-2 border-b border-border flex-shrink-0",
                        h3 { class: "text-[10px] font-semibold text-muted-foreground uppercase tracking-wider",
                            "Rig Presets"
                        }
                    }
                    div { class: "flex-1 flex items-center justify-center",
                        div { class: "text-center px-4",
                            p { class: "text-xs text-muted-foreground mb-1",
                                "No presets yet"
                            }
                            p { class: "text-[10px] text-muted-foreground/50",
                                "Create a preset by composing modules"
                            }
                        }
                    }
                }

                // ── Center: Module Chain ─────────────────────────
                div { class: "flex-1 flex flex-col min-h-0 min-w-0 overflow-y-auto",
                    // Header
                    div { class: "px-4 py-3 border-b border-border flex-shrink-0",
                        h3 { class: "text-sm font-semibold text-foreground mb-0.5",
                            "Signal Chain"
                        }
                        p { class: "text-[10px] text-muted-foreground",
                            "Modules are processed top to bottom"
                        }
                    }

                    // Module assignment rows
                    div { class: "flex-1 px-4 py-3",
                        for row in PRESET_ROWS.iter() {
                            // Connection line between modules
                            if row.position > 1 {
                                div { class: "flex justify-center py-1",
                                    div { class: "w-px h-4 bg-border" }
                                }
                            }

                            // Module card
                            div { class: "border border-border rounded-xl bg-card hover:border-primary/20 transition-all group",
                                div { class: "flex items-center gap-3 px-4 py-3",
                                    // Position number
                                    div { class: "w-5 text-[10px] font-mono text-muted-foreground/50 flex-shrink-0",
                                        "{row.position}"
                                    }

                                    // Module icon
                                    div { class: "w-8 h-8 rounded-lg border flex items-center justify-center text-xs font-bold flex-shrink-0 {row.color}",
                                        "{row.icon}"
                                    }

                                    // Module info
                                    div { class: "flex-1 min-w-0",
                                        p { class: "text-xs font-medium text-foreground",
                                            "{row.module_type}"
                                        }
                                        p { class: "text-[10px] text-muted-foreground",
                                            "{row.description}"
                                        }
                                    }

                                    // Assignment status
                                    div { class: "flex-shrink-0",
                                        span { class: "text-[10px] text-muted-foreground/40 italic",
                                            "Empty"
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // ── Right: Module Library ────────────────────────
                div { class: "w-64 flex-shrink-0 border-l border-border flex flex-col min-h-0 bg-zinc-950/30",
                    div { class: "px-3 py-2 border-b border-border flex-shrink-0",
                        h3 { class: "text-[10px] font-semibold text-muted-foreground uppercase tracking-wider",
                            "Module Library"
                        }
                    }
                    div { class: "flex-1 flex items-center justify-center",
                        div { class: "text-center px-4",
                            p { class: "text-xs text-muted-foreground mb-1",
                                "Module presets you create in the Module Editor will appear here"
                            }
                            p { class: "text-[10px] text-muted-foreground/50",
                                "Assign modules to chain positions above"
                            }
                        }
                    }
                }
            }

            // ── Bottom status ────────────────────────────────────
            div { class: "px-3 py-1.5 border-t border-border flex items-center gap-2 flex-shrink-0 bg-zinc-900/40",
                span { class: "text-[10px] text-muted-foreground",
                    "Preset Editor — compose modules into full rig configurations"
                }
            }
        }
    }
}
