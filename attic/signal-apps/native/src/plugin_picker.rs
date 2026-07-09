//! Plugin picker UI for the native harness.
//!
//! Renders the list of plugins discovered in standard install dirs
//! (`signal_plugin_host::scan_plugins`) plus a "Browse…" button for ad-hoc
//! paths. The selection callback receives the chosen `path` so the parent
//! can call `SamplerRig::load_preset_master_plugin` (or its mixer
//! variant) and refresh its layout.
//!
//! The scan runs once on first mount (and on explicit refresh) — on a
//! typical Linux box it walks `~/.clap`, `/usr/lib/clap`, `~/.vst3`,
//! `/usr/lib/vst3` with `max_depth = 2`. Fast enough to do on the UI
//! thread; if it ever isn't, move it behind a background task.
//!
//! "Browse…" reads the path from an inline text field — keeps the panel
//! self-contained (no GTK/qt file-dialog dep). Drop a path in, press
//! Enter / "Load".

use std::path::PathBuf;

use dioxus::prelude::*;
use signal_plugin_host::{PluginEntry, PluginFormat, scan_plugins};

/// The plugin picker overlay panel. When the user picks something, the
/// caller's `on_pick(path)` runs (and the parent typically closes the
/// picker by setting its `open` signal to false).
#[component]
pub fn PluginPicker(open: Signal<bool>, on_pick: EventHandler<PathBuf>) -> Element {
    let mut entries = use_signal(Vec::<PluginEntry>::new);
    let mut filter = use_signal(String::new);
    let mut browse_path = use_signal(String::new);

    // Lazy scan on first mount + explicit refresh button. CLAP only by
    // default; flip on the `vst3-host` feature in apps/native + add VST3
    // here to scan both.
    use_effect(move || {
        if entries.read().is_empty() {
            let scan = scan_plugins(&[PluginFormat::Clap, PluginFormat::Vst3], 2);
            entries.set(scan);
        }
    });

    if !open() {
        return rsx! { div {} };
    }

    let needle = filter().to_lowercase();
    let visible: Vec<PluginEntry> = entries()
        .iter()
        .filter(|e| {
            if needle.is_empty() {
                true
            } else {
                e.display_name.to_lowercase().contains(&needle)
                    || e.path.to_string_lossy().to_lowercase().contains(&needle)
            }
        })
        .cloned()
        .collect();
    let count = visible.len();
    let total = entries().len();

    rsx! {
        // Fullscreen-overlay backdrop. Clicking outside closes the picker.
        div {
            style: "position:fixed; inset:0; background:rgba(0,0,0,0.55); \
                    display:flex; align-items:center; justify-content:center; \
                    z-index:1000;",
            onclick: move |_| open.set(false),

            // Picker dialog. Stop click propagation so clicks inside don't close.
            div {
                style: "min-width:520px; max-width:720px; max-height:80vh; \
                        display:flex; flex-direction:column; \
                        background:#181a1f; border:1px solid #3a3a3c; \
                        border-radius:8px; overflow:hidden;",
                onclick: move |e| e.stop_propagation(),

                // Header
                div {
                    style: "display:flex; align-items:center; gap:10px; padding:10px 14px; \
                            background:#1c1c1e; border-bottom:1px solid #3a3a3c;",
                    strong { style: "color:#e8813a; font-size:13px;", "Load FX plugin" }
                    span { style: "color:#888; font-size:11px;", "{count} / {total} match" }
                    button {
                        style: "margin-left:auto; padding:2px 10px; font-size:11px; \
                                background:#2a2a2e; color:#ccc; border:1px solid #444; \
                                border-radius:4px; cursor:pointer;",
                        onclick: move |_| {
                            let scan = scan_plugins(&[PluginFormat::Clap, PluginFormat::Vst3], 2);
                            entries.set(scan);
                        },
                        "↻ Rescan"
                    }
                    button {
                        style: "padding:2px 10px; font-size:11px; \
                                background:#2a2a2e; color:#ccc; border:1px solid #444; \
                                border-radius:4px; cursor:pointer;",
                        onclick: move |_| open.set(false),
                        "Close"
                    }
                }

                // Filter
                div {
                    style: "padding:8px 14px; border-bottom:1px solid #2c2c2e;",
                    input {
                        style: "width:100%; padding:6px 8px; font-size:12px; \
                                background:#0e0e10; color:#e0e0e0; \
                                border:1px solid #3a3a3c; border-radius:4px; outline:none;",
                        placeholder: "Filter by name…",
                        value: "{filter}",
                        oninput: move |e| filter.set(e.value()),
                    }
                }

                // Plugin list (scrollable)
                div {
                    style: "flex:1; overflow:auto; padding:6px 0;",
                    if visible.is_empty() {
                        div {
                            style: "padding:24px; text-align:center; color:#777; font-size:12px;",
                            if entries().is_empty() {
                                "No plugins found in standard install directories. Use Browse… below to load by path."
                            } else {
                                "No matches."
                            }
                        }
                    } else {
                        for entry in visible.iter().cloned() {
                            {
                                let path = entry.path.clone();
                                let label = entry.display_name.clone();
                                let fmt = format!("{:?}", entry.format);
                                let path_str = entry.path.to_string_lossy().to_string();
                                rsx! {
                                    div {
                                        style: "display:flex; align-items:center; gap:10px; \
                                                padding:6px 14px; cursor:pointer; \
                                                border-bottom:1px solid #232327;",
                                        onclick: move |_| {
                                            on_pick.call(path.clone());
                                            open.set(false);
                                        },
                                        div {
                                            style: "flex:1; min-width:0;",
                                            div { style: "color:#e0e0e0; font-size:12px;", "{label}" }
                                            div { style: "color:#666; font-size:10px; \
                                                          white-space:nowrap; overflow:hidden; text-overflow:ellipsis;",
                                                "{path_str}"
                                            }
                                        }
                                        span {
                                            style: "color:#5a8cff; font-size:10px; font-weight:600; \
                                                    padding:2px 6px; background:#1d2a4a; border-radius:3px;",
                                            "{fmt}"
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // Browse… (ad-hoc path)
                div {
                    style: "display:flex; align-items:center; gap:8px; padding:10px 14px; \
                            background:#1c1c1e; border-top:1px solid #2c2c2e;",
                    span { style: "color:#888; font-size:11px; font-weight:600;", "Browse:" }
                    input {
                        style: "flex:1; padding:5px 8px; font-size:11px; \
                                background:#0e0e10; color:#ddd; border:1px solid #3a3a3c; \
                                border-radius:4px; outline:none;",
                        placeholder: "/path/to/Plugin.clap or .vst3",
                        value: "{browse_path}",
                        oninput: move |e| browse_path.set(e.value()),
                        onkeypress: move |e| {
                            if e.key() == Key::Enter {
                                let p = browse_path();
                                if !p.is_empty() {
                                    on_pick.call(PathBuf::from(p));
                                    open.set(false);
                                }
                            }
                        },
                    }
                    button {
                        style: "padding:4px 12px; font-size:11px; \
                                background:#e8813a; color:#111; border:none; \
                                border-radius:4px; cursor:pointer; font-weight:600;",
                        onclick: move |_| {
                            let p = browse_path();
                            if !p.is_empty() {
                                on_pick.call(PathBuf::from(p));
                                open.set(false);
                            }
                        },
                        "Load"
                    }
                }
            }
        }
    }
}
