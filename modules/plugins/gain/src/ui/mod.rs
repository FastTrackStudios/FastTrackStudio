//! UI components for the gain plugin.

use dioxus::prelude::*;
use fts_plugins_ui::MeterOrientation;
use fts_plugins_ui::prelude::*;

use crate::params::GainParamValues;

/// A complete gain channel strip component.
///
/// This can be used both in the plugin editor and in the control app.
#[component]
pub fn GainStrip(
    /// Input level in dB for meter display
    input_db: f32,
    /// Output level in dB for meter display
    output_db: f32,
    /// Current parameter values
    params: GainParamValues,
    /// Plugin name to display
    #[props(default = "FTS Gain".to_string())]
    name: String,
    /// Track name (optional)
    #[props(default = None)]
    track_name: Option<String>,
    /// Callback when gain changes
    #[props(default = None)]
    on_gain_change: Option<EventHandler<f32>>,
    /// Callback when phase invert changes
    #[props(default = None)]
    on_phase_change: Option<EventHandler<bool>>,
) -> Element {
    rsx! {
        div {
            style: "width: 120px; background: #1a1a1e; border-radius: 8px; padding: 12px; display: flex; flex-direction: column; gap: 12px;",

            // Header
            div {
                style: "text-align: center;",
                div {
                    style: "font-size: 12px; font-weight: 600; color: white;",
                    "{name}"
                }
                if let Some(track) = &track_name {
                    div {
                        style: "font-size: 10px; color: #666; margin-top: 2px;",
                        "{track}"
                    }
                }
            }

            // Meters
            div {
                style: "display: flex; gap: 8px; justify-content: center;",

                PeakMeter {
                    level_db: input_db,
                    label: Some("IN".to_string()),
                    size: 16,
                    length: 120,
                }

                PeakMeter {
                    level_db: output_db,
                    label: Some("OUT".to_string()),
                    size: 16,
                    length: 120,
                }
            }

            // Gain control
            div {
                style: "display: flex; flex-direction: column; gap: 8px;",

                Knob {
                    value: normalize_gain_db(params.gain_db),
                    label: Some("Gain".to_string()),
                    value_text: Some(format!("{:.1} dB", params.gain_db)),
                    size: 48,
                }

                // Phase invert
                Toggle {
                    value: params.phase_invert,
                    label: Some("Phase".to_string()),
                }
            }
        }
    }
}

/// Normalize gain dB to 0-1 range for knob display.
fn normalize_gain_db(db: f32) -> f32 {
    // Map -60..+24 to 0..1
    ((db + 60.0) / 84.0).clamp(0.0, 1.0)
}

/// A compact gain meter display (for control app sidebar, etc.)
#[component]
pub fn GainMeterCompact(
    /// Input level in dB
    input_db: f32,
    /// Output level in dB
    output_db: f32,
    /// Plugin name
    #[props(default = "Gain".to_string())]
    name: String,
) -> Element {
    rsx! {
        div {
            style: "display: flex; align-items: center; gap: 8px; padding: 4px 8px; background: #1a1a1e; border-radius: 4px;",

            span {
                style: "font-size: 11px; color: #888; min-width: 40px;",
                "{name}"
            }

            PeakMeter {
                level_db: input_db,
                size: 8,
                length: 60,
                orientation: MeterOrientation::Horizontal,
                show_value: false,
            }

            PeakMeter {
                level_db: output_db,
                size: 8,
                length: 60,
                orientation: MeterOrientation::Horizontal,
                show_value: false,
            }
        }
    }
}
