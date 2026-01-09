//! Meter components for displaying audio levels.

use crate::theme::DARK_THEME;
use dioxus::prelude::*;

/// Orientation for meters.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum MeterOrientation {
    #[default]
    Vertical,
    Horizontal,
}

/// A single peak meter bar.
#[component]
pub fn PeakMeter(
    /// Current level in dB (-60 to +6 typical)
    #[props(default = -60.0)]
    level_db: f32,
    /// Minimum dB value (bottom of meter)
    #[props(default = -60.0)]
    min_db: f32,
    /// Maximum dB value (top of meter)
    #[props(default = 6.0)]
    max_db: f32,
    /// Width in pixels (for vertical) or height (for horizontal)
    #[props(default = 16)]
    size: u32,
    /// Length in pixels (height for vertical, width for horizontal)
    #[props(default = 150)]
    length: u32,
    /// Meter orientation
    #[props(default = MeterOrientation::Vertical)]
    orientation: MeterOrientation,
    /// Optional label below/beside the meter
    #[props(default = None)]
    label: Option<String>,
    /// Show dB value
    #[props(default = true)]
    show_value: bool,
) -> Element {
    let range = max_db - min_db;
    let normalized = ((level_db - min_db) / range).clamp(0.0, 1.0);
    let pct = normalized * 100.0;

    let theme = DARK_THEME;

    match orientation {
        MeterOrientation::Vertical => {
            rsx! {
                div {
                    style: "display: flex; flex-direction: column; align-items: center; gap: 4px;",

                    // Meter bar
                    div {
                        style: "width: {size}px; height: {length}px; background: {theme.bg_tertiary}; border-radius: 4px; position: relative; overflow: hidden;",
                        div {
                            style: "position: absolute; bottom: 0; left: 0; right: 0; background: linear-gradient(to top, {theme.meter_green} 0%, {theme.meter_green} 60%, {theme.meter_yellow} 80%, {theme.meter_red} 100%); transition: height 50ms; height: {pct}%;",
                        }
                    }

                    // Label
                    if let Some(lbl) = label {
                        span {
                            style: "color: {theme.text_secondary}; font-size: 11px;",
                            "{lbl}"
                        }
                    }

                    // Value
                    if show_value {
                        span {
                            style: "color: {theme.text_secondary}; font-size: 10px; font-family: monospace;",
                            "{level_db:.1}"
                        }
                    }
                }
            }
        }
        MeterOrientation::Horizontal => {
            rsx! {
                div {
                    style: "display: flex; align-items: center; gap: 4px;",

                    // Label
                    if let Some(lbl) = label {
                        span {
                            style: "color: {theme.text_secondary}; font-size: 11px; min-width: 24px;",
                            "{lbl}"
                        }
                    }

                    // Meter bar
                    div {
                        style: "width: {length}px; height: {size}px; background: {theme.bg_tertiary}; border-radius: 4px; position: relative; overflow: hidden;",
                        div {
                            style: "position: absolute; left: 0; top: 0; bottom: 0; background: linear-gradient(to right, {theme.meter_green} 0%, {theme.meter_green} 60%, {theme.meter_yellow} 80%, {theme.meter_red} 100%); transition: width 50ms; width: {pct}%;",
                        }
                    }

                    // Value
                    if show_value {
                        span {
                            style: "color: {theme.text_secondary}; font-size: 10px; font-family: monospace; min-width: 40px;",
                            "{level_db:.1} dB"
                        }
                    }
                }
            }
        }
    }
}

/// A stereo meter pair (left and right channels).
#[component]
pub fn StereoMeter(
    /// Left channel level in dB
    #[props(default = -60.0)]
    level_l_db: f32,
    /// Right channel level in dB
    #[props(default = -60.0)]
    level_r_db: f32,
    /// Minimum dB value
    #[props(default = -60.0)]
    min_db: f32,
    /// Maximum dB value
    #[props(default = 6.0)]
    max_db: f32,
    /// Width of each meter bar
    #[props(default = 16)]
    bar_width: u32,
    /// Height of the meter
    #[props(default = 150)]
    height: u32,
    /// Gap between L and R bars
    #[props(default = 4)]
    gap: u32,
    /// Show L/R labels
    #[props(default = true)]
    show_labels: bool,
) -> Element {
    rsx! {
        div {
            style: "display: flex; gap: {gap}px; justify-content: center;",

            // Left meter
            PeakMeter {
                level_db: level_l_db,
                min_db: min_db,
                max_db: max_db,
                size: bar_width,
                length: height,
                label: if show_labels { Some("L".to_string()) } else { None },
                show_value: false,
            }

            // Right meter
            PeakMeter {
                level_db: level_r_db,
                min_db: min_db,
                max_db: max_db,
                size: bar_width,
                length: height,
                label: if show_labels { Some("R".to_string()) } else { None },
                show_value: false,
            }
        }
    }
}

/// Gain reduction meter (displays compression amount).
#[component]
pub fn GainReductionMeter(
    /// Gain reduction in dB (negative value, e.g., -6.0)
    #[props(default = 0.0)]
    gr_db: f32,
    /// Maximum GR to display (positive value for range, e.g., 20.0 for -20dB max)
    #[props(default = 20.0)]
    max_gr: f32,
    /// Width of the meter
    #[props(default = 16)]
    width: u32,
    /// Height of the meter
    #[props(default = 100)]
    height: u32,
    /// Show value
    #[props(default = true)]
    show_value: bool,
) -> Element {
    let theme = DARK_THEME;

    // GR is negative, so we take the absolute value
    let gr_abs = gr_db.abs();
    let pct = (gr_abs / max_gr).clamp(0.0, 1.0) * 100.0;

    rsx! {
        div {
            style: "display: flex; flex-direction: column; align-items: center; gap: 4px;",

            // Meter bar (fills from top down)
            div {
                style: "width: {width}px; height: {height}px; background: {theme.bg_tertiary}; border-radius: 4px; position: relative; overflow: hidden;",
                div {
                    style: "position: absolute; top: 0; left: 0; right: 0; background: {theme.meter_gr}; transition: height 50ms; height: {pct}%;",
                }
            }

            // Label
            span {
                style: "color: {theme.text_secondary}; font-size: 11px;",
                "GR"
            }

            // Value
            if show_value {
                span {
                    style: "color: {theme.text_secondary}; font-size: 10px; font-family: monospace;",
                    "{gr_db:.1}"
                }
            }
        }
    }
}
