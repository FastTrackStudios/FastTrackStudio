//! Transfer curve visualization — shows input vs output dB.
//!
//! Used by compressor, limiter, gate, and expander plugins to
//! display their gain transfer function.

use crate::theme::use_theme;
use nih_plug_dioxus::prelude::*;

/// Compute gain reduction for a given input level using soft-knee compression.
///
/// Returns output_db for a given input_db.
fn compress_transfer(input_db: f32, threshold_db: f32, ratio: f32, knee_db: f32) -> f32 {
    if ratio <= 1.0 {
        return input_db;
    }

    let slope = 1.0 - 1.0 / ratio;
    let half_knee = knee_db * 0.5;

    if knee_db > 0.001 && (input_db - threshold_db).abs() < half_knee {
        // Soft knee region: quadratic interpolation
        let x = input_db - threshold_db + half_knee;
        let gr = slope * x * x / (2.0 * knee_db);
        input_db - gr
    } else if input_db > threshold_db {
        // Above knee: standard compression
        let gr = slope * (input_db - threshold_db);
        input_db - gr
    } else {
        // Below threshold: no compression
        input_db
    }
}

/// Transfer curve visualization — shows input vs output dB.
///
/// Renders a grid, 1:1 reference line, threshold crosshair,
/// the transfer curve itself, and an optional "ball" indicator
/// showing the current input level on the curve.
#[component]
pub fn TransferCurve(
    /// Threshold in dBFS.
    threshold_db: f32,
    /// Compression ratio (e.g. 4.0 = 4:1).
    ratio: f32,
    /// Soft knee width in dB (0 = hard knee, 6 = gentle).
    #[props(default = 6.0)]
    knee_db: f32,
    /// Current input level (for the "ball" indicator), or None.
    #[props(default)]
    input_level_db: Option<f32>,
    /// Widget width in pixels.
    #[props(default = 200.0)]
    width: f32,
    /// Widget height in pixels.
    #[props(default = 200.0)]
    height: f32,
    /// dB range (e.g. 60.0 means -60 to 0).
    #[props(default = 60.0)]
    range_db: f32,
) -> Element {
    let t = use_theme();
    let t = *t.read();

    let min_db = -range_db;
    let num_points: usize = 80;

    let db_to_x = |db: f32| -> f32 { ((db - min_db) / range_db) * width };
    let db_to_y = |db: f32| -> f32 { height - ((db - min_db) / range_db) * height };

    let points: Vec<(f32, f32)> = (0..=num_points)
        .map(|i| {
            let input = min_db + (i as f32 / num_points as f32) * range_db;
            let output = compress_transfer(input, threshold_db, ratio, knee_db);
            (db_to_x(input), db_to_y(output))
        })
        .collect();

    let thresh_x = db_to_x(threshold_db);
    let thresh_y = db_to_y(threshold_db);

    rsx! {
        div {
            style: format!(
                "position:relative; width:{width}px; height:{height}px; \
                 background:{}; border-radius:6px; overflow:hidden; \
                 border:1px solid {};",
                t.surface, t.border
            ),

            // Grid lines (every 12 dB)
            for db_val in [-48, -36, -24, -12] {
                {
                    let x = db_to_x(db_val as f32);
                    let y = db_to_y(db_val as f32);
                    rsx! {
                        div {
                            style: format!(
                                "position:absolute; left:{x}px; top:0; width:1px; \
                                 height:100%; background:{};",
                                t.grid_line
                            ),
                        }
                        div {
                            style: format!(
                                "position:absolute; left:0; top:{y}px; width:100%; \
                                 height:1px; background:{};",
                                t.grid_line
                            ),
                        }
                    }
                }
            }

            // 1:1 reference line (dotted diagonal)
            for i in 0..40 {
                {
                    let frac = i as f32 / 40.0;
                    let db = min_db + frac * range_db;
                    let x = db_to_x(db);
                    let y = db_to_y(db);
                    rsx! {
                        div {
                            style: format!(
                                "position:absolute; left:{x}px; top:{y}px; \
                                 width:2px; height:2px; border-radius:1px; \
                                 background:{};",
                                t.reference_dot
                            ),
                        }
                    }
                }
            }

            // Transfer curve segments (line approximation)
            for i in 0..points.len().saturating_sub(1) {
                {
                    let (x1, y1) = points[i];
                    let (x2, y2) = points[i + 1];
                    let dx = x2 - x1;
                    let dy = y2 - y1;
                    let len = (dx * dx + dy * dy).sqrt().max(1.0);
                    let angle = dy.atan2(dx) * 180.0 / std::f32::consts::PI;
                    rsx! {
                        div {
                            style: format!(
                                "position:absolute; left:{x1}px; top:{y1}px; \
                                 width:{len}px; height:2px; \
                                 background:{}; \
                                 transform-origin:0 0; \
                                 transform:rotate({angle}deg);",
                                t.accent
                            ),
                        }
                    }
                }
            }

            // Threshold crosshair
            div {
                style: format!(
                    "position:absolute; left:{thresh_x}px; top:0; width:1px; \
                     height:100%; background:{};",
                    t.crosshair
                ),
            }
            div {
                style: format!(
                    "position:absolute; left:0; top:{thresh_y}px; width:100%; \
                     height:1px; background:{};",
                    t.crosshair
                ),
            }

            // Input level indicator (ball on curve)
            if let Some(level) = input_level_db {
                {
                    let out = compress_transfer(level, threshold_db, ratio, knee_db);
                    let bx = db_to_x(level) - 3.0;
                    let by = db_to_y(out) - 3.0;
                    rsx! {
                        div {
                            style: format!(
                                "position:absolute; left:{bx}px; top:{by}px; \
                                 width:6px; height:6px; border-radius:3px; \
                                 background:#fff;"
                            ),
                        }
                    }
                }
            }

            // Corner labels
            div {
                style: format!(
                    "position:absolute; left:4px; bottom:2px; font-size:9px; color:{};",
                    t.text_dim
                ),
                "{min_db:.0}"
            }
            div {
                style: format!(
                    "position:absolute; right:4px; bottom:2px; font-size:9px; color:{};",
                    t.text_dim
                ),
                "0 dB"
            }
        }
    }
}
