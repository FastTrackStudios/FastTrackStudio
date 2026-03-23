mod browser_dialog;
mod editor_tab;
mod fx_capture;
mod live_fx_view;
pub(crate) mod macro_bar;
mod manage_tab;
mod midi_tab;
pub(crate) mod performance_tab;
pub(crate) mod preset_tab;
mod setlist_tab;

use dioxus::prelude::*;
use fts_ui::prelude::*;
use session_ui::PerformanceLayout;

pub(crate) use browser_dialog::SignalBrowserDialog;
pub(crate) use editor_tab::SignalEditorTab;
pub(crate) use fx_capture::SignalCaptureTab;
pub(crate) use manage_tab::SignalManageTab;
pub(crate) use midi_tab::SignalMidiTab;
pub(crate) use performance_tab::SignalPerformanceTab;
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
    LiveFx,
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
            div { class: "flex items-center justify-between px-3 py-1.5 flex-shrink-0 border-b border-border bg-card/50",
                div { class: "flex items-center gap-2",
                    // Mode selector — primary tabs
                    SegmentedControl {
                        value: format!("{:?}", mode).to_lowercase(),
                        on_change: move |v: String| {
                            let m = match v.as_str() {
                                "preset" => SignalMode::Preset,
                                "profile" => SignalMode::Profile,
                                "song" => SignalMode::Song,
                                _ => return,
                            };
                            *SIGNAL_MODE.write() = m;
                        },
                        options: vec![
                            (String::from("preset"), String::from("Preset")),
                            (String::from("profile"), String::from("Profile")),
                            (String::from("song"), String::from("Song")),
                        ],
                    }

                    // Sub-tab pills — only shown in Profile/Song modes
                    if mode != SignalMode::Preset {
                        SegmentedControl {
                            value: format!("{:?}", active_tab()).to_lowercase(),
                            on_change: move |v: String| {
                                let tab = match v.as_str() {
                                    "performance" => SignalTab::Performance,
                                    "manage" => SignalTab::Manage,
                                    "setlist" => SignalTab::Setlist,
                                    "editor" => SignalTab::Editor,
                                    "livefx" => SignalTab::LiveFx,
                                    "capture" => SignalTab::Capture,
                                    "midisettings" => SignalTab::MidiSettings,
                                    _ => return,
                                };
                                active_tab.set(tab);
                            },
                            options: vec![
                                (String::from("performance"), String::from("Performance")),
                                (String::from("manage"), String::from("Manage")),
                                (String::from("setlist"), String::from("Setlist")),
                                (String::from("editor"), String::from("Editor")),
                                (String::from("livefx"), String::from("Live FX")),
                                (String::from("capture"), String::from("Capture")),
                                (String::from("midisettings"), String::from("MIDI")),
                            ],
                            size: SegmentedControlSize::Small,
                        }
                    }
                }

                // Right: browser button
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: move |_| browser_open.set(true),
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
                            SignalTab::LiveFx => rsx! {
                                live_fx_view::LiveFxView {}
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
