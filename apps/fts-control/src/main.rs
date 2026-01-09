//! FTS Control - Native desktop app for visualizing and controlling FTS plugins.
//!
//! This app connects to the Plugin Hub via shared memory to display real-time
//! state of all FTS plugins running in the DAW.
//!
//! # Running
//!
//! ```bash
//! dx serve --platform native
//! # or
//! cargo run -p fts-control
//! ```

use dioxus::prelude::*;
use plugin_hub::{PluginInfo, PluginType};

// Shared UI components
use fts_plugins_ui::DARK_THEME;
use fts_plugins_ui::prelude::*;

// Plugin-specific components
use fts_gain_core::GainParamValues;
use fts_gain_core::ui::GainStrip;

fn main() {
    // Initialize logging
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive(tracing::Level::INFO.into()),
        )
        .init();

    tracing::info!("Starting FTS Control");

    // Launch native app
    dioxus::launch(App);
}

/// Main app component
#[component]
fn App() -> Element {
    let theme = DARK_THEME;

    // Simulated plugin list (will be replaced with hub connection)
    let plugins = use_signal(|| {
        vec![
            PluginInfo {
                id: 1,
                plugin_type: PluginType::Gain,
                name: "FTS Gain".to_string(),
                track_name: Some("Drums".to_string()),
                track_index: Some(0),
                fx_index: Some(0),
            },
            PluginInfo {
                id: 2,
                plugin_type: PluginType::Eq,
                name: "FTS EQ".to_string(),
                track_name: Some("Drums".to_string()),
                track_index: Some(0),
                fx_index: Some(1),
            },
            PluginInfo {
                id: 3,
                plugin_type: PluginType::Compressor,
                name: "FTS Comp".to_string(),
                track_name: Some("Bass".to_string()),
                track_index: Some(1),
                fx_index: Some(0),
            },
        ]
    });

    // Connection status
    let connected = use_signal(|| false);

    rsx! {
        div {
            style: "width: 100vw; height: 100vh; background: {theme.bg_primary}; color: {theme.text_primary}; font-family: system-ui, -apple-system, sans-serif; display: flex; flex-direction: column;",

            // Header
            Header { connected: *connected.read() }

            // Main content
            div {
                style: "flex: 1; display: flex; overflow: hidden;",

                // Sidebar - Track list
                Sidebar { plugins: plugins.read().clone() }

                // Main area - Channel strips
                MainContent { plugins: plugins.read().clone() }
            }

            // Footer
            Footer {}
        }
    }
}

/// Header component
#[component]
fn Header(connected: bool) -> Element {
    let theme = DARK_THEME;
    let status_color = if connected {
        theme.meter_green
    } else {
        theme.meter_red
    };
    let status_text = if connected {
        "Connected"
    } else {
        "Disconnected"
    };

    rsx! {
        header {
            style: "height: 48px; background: {theme.bg_secondary}; border-bottom: 1px solid {theme.border}; display: flex; align-items: center; justify-content: space-between; padding: 0 16px;",

            // Logo
            div {
                style: "display: flex; align-items: center; gap: 8px;",
                h1 {
                    style: "font-size: 18px; font-weight: 600; margin: 0;",
                    "FTS Control"
                }
            }

            // Connection status
            div {
                style: "display: flex; align-items: center; gap: 8px;",
                div {
                    style: "width: 8px; height: 8px; border-radius: 50%; background: {status_color};",
                }
                span {
                    style: "font-size: 12px; color: {theme.text_secondary};",
                    "{status_text}"
                }
            }
        }
    }
}

/// Sidebar with track list
#[component]
fn Sidebar(plugins: Vec<PluginInfo>) -> Element {
    let theme = DARK_THEME;

    // Group plugins by track
    let mut tracks: std::collections::HashMap<String, Vec<&PluginInfo>> =
        std::collections::HashMap::new();
    for plugin in &plugins {
        let track_name = plugin.track_name.clone().unwrap_or("Unknown".to_string());
        tracks.entry(track_name).or_default().push(plugin);
    }

    rsx! {
        aside {
            style: "width: 200px; background: {theme.bg_tertiary}; border-right: 1px solid {theme.border}; overflow-y: auto;",

            // Header
            div {
                style: "padding: 12px 16px; border-bottom: 1px solid {theme.border};",
                h2 {
                    style: "font-size: 12px; font-weight: 600; text-transform: uppercase; color: {theme.text_tertiary}; margin: 0;",
                    "Tracks"
                }
            }

            // Track list
            div {
                style: "padding: 8px;",
                for (track_name, track_plugins) in tracks {
                    div {
                        style: "margin-bottom: 8px;",

                        // Track name
                        div {
                            style: "padding: 8px 12px; background: {theme.bg_secondary}; border-radius: 6px; cursor: pointer;",
                            div {
                                style: "font-size: 13px; font-weight: 500;",
                                "{track_name}"
                            }
                            div {
                                style: "font-size: 11px; color: {theme.text_tertiary}; margin-top: 2px;",
                                "{track_plugins.len()} plugins"
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Main content area with channel strips
#[component]
fn MainContent(plugins: Vec<PluginInfo>) -> Element {
    let theme = DARK_THEME;
    let is_empty = plugins.is_empty();

    rsx! {
        main {
            style: "flex: 1; padding: 16px; overflow-x: auto; display: flex; gap: 16px;",

            for plugin in plugins {
                PluginStrip { plugin: plugin }
            }

            // Add placeholder if no plugins
            if is_empty {
                div {
                    style: "flex: 1; display: flex; align-items: center; justify-content: center; color: {theme.text_tertiary};",
                    div {
                        style: "text-align: center;",
                        div {
                            style: "font-size: 14px;",
                            "No plugins connected"
                        }
                        div {
                            style: "font-size: 12px; margin-top: 4px;",
                            "Load FTS plugins in your DAW to see them here"
                        }
                    }
                }
            }
        }
    }
}

/// Plugin strip that renders the appropriate component based on plugin type
#[component]
fn PluginStrip(plugin: PluginInfo) -> Element {
    // Simulated meter values (will come from hub)
    let input_db: f32 = -12.0;
    let output_db: f32 = -10.0;

    match plugin.plugin_type {
        PluginType::Gain => {
            // Use the shared GainStrip component
            let params = GainParamValues {
                gain_db: 0.0,
                phase_invert: false,
            };

            rsx! {
                GainStrip {
                    input_db: input_db,
                    output_db: output_db,
                    params: params,
                    name: plugin.name.clone(),
                    track_name: plugin.track_name.clone(),
                }
            }
        }
        _ => {
            // Generic strip for EQ, Compressor, etc. (until specific modules exist)
            rsx! {
                GenericPluginStrip { plugin: plugin }
            }
        }
    }
}

/// Generic channel strip for plugins that don't have specific UI components yet
#[component]
fn GenericPluginStrip(plugin: PluginInfo) -> Element {
    let theme = DARK_THEME;

    // Simulated meter values
    let peak_l_db: f32 = -18.0;
    let peak_r_db: f32 = -20.0;

    let plugin_icon = match plugin.plugin_type {
        PluginType::Gain => "G",
        PluginType::Eq => "EQ",
        PluginType::Compressor => "C",
        _ => "?",
    };

    let track_name = plugin.track_name.clone().unwrap_or_default();
    let plugin_name = plugin.name.clone();

    rsx! {
        div {
            style: "width: 120px; background: {theme.bg_secondary}; border-radius: 8px; padding: 12px; display: flex; flex-direction: column; gap: 12px;",

            // Plugin name
            div {
                style: "text-align: center;",
                div {
                    style: "font-size: 16px; font-weight: bold; color: {theme.accent_primary}; background: {theme.bg_tertiary}; width: 32px; height: 32px; border-radius: 4px; display: inline-flex; align-items: center; justify-content: center;",
                    "{plugin_icon}"
                }
                div {
                    style: "font-size: 12px; font-weight: 500; margin-top: 8px;",
                    "{plugin_name}"
                }
                div {
                    style: "font-size: 10px; color: {theme.text_tertiary}; margin-top: 2px;",
                    "{track_name}"
                }
            }

            // Meters - using shared StereoMeter component
            StereoMeter {
                level_l_db: peak_l_db,
                level_r_db: peak_r_db,
                height: 120,
                bar_width: 14,
                show_labels: true,
            }

            // Controls placeholder
            div {
                style: "display: flex; flex-direction: column; gap: 8px;",

                // Fader placeholder
                div {
                    style: "height: 4px; background: {theme.bg_tertiary}; border-radius: 2px;",
                    div {
                        style: "width: 70%; height: 100%; background: {theme.accent_primary}; border-radius: 2px;",
                    }
                }

                // Value display
                div {
                    style: "text-align: center; font-size: 11px; color: {theme.text_secondary}; font-family: monospace;",
                    "0.0 dB"
                }
            }
        }
    }
}

/// Footer component
#[component]
fn Footer() -> Element {
    let theme = DARK_THEME;

    rsx! {
        footer {
            style: "height: 32px; background: {theme.bg_tertiary}; border-top: 1px solid {theme.border}; display: flex; align-items: center; justify-content: center; padding: 0 16px;",
            span {
                style: "font-size: 11px; color: {theme.text_tertiary};",
                "FastTrackStudio | FTS Control v0.1.0"
            }
        }
    }
}
