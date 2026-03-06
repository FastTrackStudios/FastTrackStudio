mod browser_dialog;
mod editor_tab;
mod fx_capture;
pub(crate) mod macro_bar;
mod manage_tab;
mod midi_tab;
pub(crate) mod performance_tab;
pub(crate) mod preset_tab;
mod setlist_tab;

use dioxus::prelude::*;
use session_ui::PerformanceLayout;

pub(crate) use browser_dialog::SignalBrowserDialog;
pub(crate) use editor_tab::SignalEditorTab;
pub(crate) use fx_capture::SignalCaptureTab;
pub(crate) use manage_tab::SignalManageTab;
pub(crate) use performance_tab::SignalPerformanceTab;
pub(crate) use midi_tab::SignalMidiTab;
pub(crate) use preset_tab::SignalPresetTab;
pub(crate) use setlist_tab::SignalSetlistTab;

/// Top-level Signal modes — determines which sub-view is shown.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SignalMode {
    Preset,
    Profile,
    Song,
}

/// Global signal for the active Signal mode. Default: Profile.
pub(crate) static SIGNAL_MODE: GlobalSignal<SignalMode> = Signal::global(|| SignalMode::Profile);

/// Global signal for the selected setlist in Signal tabs.
/// Uses `"all"` for the virtual "All Songs" selection.
pub(crate) static SIGNAL_SELECTED_SETLIST_ID: GlobalSignal<String> =
    Signal::global(|| "all".to_string());

/// A preset item in the manage tab sidebar — either a Rig or Layer.
#[derive(Clone, PartialEq)]
pub(crate) struct ManagePresetItem {
    pub(crate) id: String,
    pub(crate) name: String,
    pub(crate) is_rig: bool,
    pub(crate) sub_items: Vec<(String, String)>, // (id, name) — scenes for rigs, variants for layers
}

/// A profile item in the manage tab sidebar — with expandable patches.
#[derive(Clone, PartialEq)]
pub(crate) struct ManageProfileItem {
    pub(crate) id: String,
    pub(crate) name: String,
    pub(crate) patches: Vec<(String, String)>, // (id, name)
}

/// Performance view — renders the PerformanceLayout.
#[component]
pub(crate) fn PerformanceWithChartToggle() -> Element {
    rsx! {
        div {
            class: "relative h-full w-full bg-zinc-950",
            PerformanceLayout {}
        }
    }
}

// ---------------------------------------------------------------------------
// Signal view — mode selector + sub-tabs + browser dialog
// ---------------------------------------------------------------------------

/// Sub-tabs for Profile/Song modes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SignalTab {
    Performance,
    Manage,
    Setlist,
    Editor,
    Capture,
    MidiSettings,
}

/// Signal top-level view with mode selector, sub-tabs, and Browser dialog.
#[component]
pub(crate) fn SignalView() -> Element {
    let mut active_tab = use_signal(|| SignalTab::Performance);
    let mut browser_open = use_signal(|| false);
    let mode = SIGNAL_MODE();

    rsx! {
        div { class: "h-full w-full flex flex-col bg-zinc-950 overflow-hidden",
            // Top row: mode selector (left) + browser button (right)
            div { class: "flex items-center justify-between px-3 py-1.5 border-b border-white/[0.06] bg-white/[0.02] flex-shrink-0",
                div { class: "flex items-center gap-2",
                    // Mode selector — primary tabs
                    div { class: "flex items-center gap-0.5 bg-white/[0.06] rounded-lg p-0.5",
                        for (m, label) in [
                            (SignalMode::Preset, "Preset"),
                            (SignalMode::Profile, "Profile"),
                            (SignalMode::Song, "Song"),
                        ] {
                            {
                                let is_active = mode == m;
                                rsx! {
                                    button {
                                        class: if is_active {
                                            "px-3 py-1 text-xs font-semibold text-white bg-white/[0.15] rounded-md transition-colors"
                                        } else {
                                            "px-3 py-1 text-xs font-medium text-zinc-400 hover:text-zinc-200 hover:bg-white/[0.06] rounded-md transition-colors"
                                        },
                                        onclick: move |_| *SIGNAL_MODE.write() = m,
                                        "{label}"
                                    }
                                }
                            }
                        }
                    }

                    // Sub-tab pills — only shown in Profile/Song modes
                    if mode != SignalMode::Preset {
                        div { class: "flex items-center gap-0.5 bg-white/[0.04] rounded-lg p-0.5 ml-1",
                            for (tab, label) in [
                                (SignalTab::Performance, "Performance"),
                                (SignalTab::Manage, "Manage"),
                                (SignalTab::Setlist, "Setlist"),
                                (SignalTab::Editor, "Editor"),
                                (SignalTab::Capture, "Capture"),
                                (SignalTab::MidiSettings, "MIDI"),
                            ] {
                                {
                                    let is_active = active_tab() == tab;
                                    rsx! {
                                        button {
                                            class: if is_active {
                                                "px-2.5 py-0.5 text-[11px] font-medium text-white bg-white/[0.10] rounded-md transition-colors"
                                            } else {
                                                "px-2.5 py-0.5 text-[11px] font-medium text-zinc-500 hover:text-zinc-300 hover:bg-white/[0.06] rounded-md transition-colors"
                                            },
                                            onclick: move |_| active_tab.set(tab),
                                            "{label}"
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // Right: browser button
                button {
                    class: "px-3 py-1 text-xs font-medium text-zinc-400 hover:text-zinc-200 hover:bg-white/[0.06] rounded-md transition-colors",
                    onclick: move |_| browser_open.set(true),
                    "Browser"
                }
            }

            // View content — mode-dependent
            div { class: "flex-1 min-h-0 overflow-hidden",
                match mode {
                    SignalMode::Preset => rsx! {
                        SignalPresetTab {}
                    },
                    SignalMode::Profile | SignalMode::Song => {
                        // Profile and Song modes share the existing sub-tab views
                        match active_tab() {
                            SignalTab::Performance => rsx! {
                                SignalPerformanceTab {}
                            },
                            SignalTab::Manage => rsx! {
                                SignalManageTab {}
                            },
                            SignalTab::Setlist => rsx! {
                                SignalSetlistTab {}
                            },
                            SignalTab::Editor => rsx! {
                                SignalEditorTab {}
                            },
                            SignalTab::Capture => rsx! {
                                SignalCaptureTab {}
                            },
                            SignalTab::MidiSettings => rsx! {
                                SignalMidiTab {}
                            },
                        }
                    }
                }
            }

            // Browser dialog (near-full-screen)
            if browser_open() {
                SignalBrowserDialog {
                    on_close: move |_| browser_open.set(false),
                }
            }
        }
    }
}
