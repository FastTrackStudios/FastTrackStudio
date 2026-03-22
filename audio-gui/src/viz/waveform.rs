//! Waveform display — renders audio sample data as a centered waveform
//! or a scrolling peak level history.

use crate::theme::use_theme;
use nih_plug_dioxus::prelude::*;

/// Centered bipolar waveform display.
///
/// Each entry in `samples` renders as one column, centered around the
/// horizontal midline. Values range from -1.0 to 1.0.
#[component]
pub fn WaveformDisplay(
    /// Normalized sample peaks (-1.0 to 1.0). Each entry renders as one column.
    #[props(default)]
    samples: Vec<f64>,
    /// Width in pixels.
    #[props(default = 200)]
    width: u32,
    /// Height in pixels.
    #[props(default = 64)]
    height: u32,
    /// Bar color.
    #[props(default)]
    color: Option<String>,
) -> Element {
    let t = use_theme();
    let t = *t.read();
    let color = color.as_deref().unwrap_or(t.accent);

    let w = width;
    let h = height;
    let hf = h as f64;
    let mid = hf / 2.0;
    let count = samples.len().max(1);
    let bar_w = (w as f64 / count as f64).max(1.0);

    rsx! {
        div {
            style: format!(
                "position:relative; overflow:hidden; border-radius:4px; \
                 background:rgba(34,34,64,0.3); width:{w}px; height:{h}px;"
            ),

            // Center line
            div {
                style: format!(
                    "position:absolute; left:0; top:50%; width:100%; height:1px; \
                     background:rgba(136,136,136,0.2);"
                ),
            }

            // Waveform bars
            for (i, sample) in samples.iter().enumerate() {
                {
                    let amp = sample.abs().clamp(0.0, 1.0);
                    let bar_h = (amp * mid).max(1.0);
                    let bar_top = mid - bar_h;
                    let bar_full_h = bar_h * 2.0;
                    let left = i as f64 * bar_w;
                    rsx! {
                        div {
                            style: format!(
                                "position:absolute; left:{left:.1}px; top:{bar_top:.1}px; \
                                 width:{bar_w:.1}px; height:{bar_full_h:.1}px; \
                                 border-radius:1px; background:{color}; opacity:0.8;"
                            ),
                        }
                    }
                }
            }
        }
    }
}

/// Scrolling peak level waveform with optional gain reduction overlay.
///
/// Used by dynamics plugins (compressor, gate, rider) to show
/// amplitude history over time with GR envelope overlay.
#[component]
pub fn PeakWaveform(
    /// Peak levels (0.0–1.0, newest at end).
    levels: Vec<f32>,
    /// Gain reduction levels (0.0–1.0, same length as `levels`).
    #[props(default = Vec::new())]
    gr_levels: Vec<f32>,
    /// Width in pixels.
    #[props(default = 400.0)]
    width: f32,
    /// Height in pixels.
    #[props(default = 80.0)]
    height: f32,
) -> Element {
    let t = use_theme();
    let t = *t.read();

    let num_bars = levels.len().max(1);
    let bar_width = width / num_bars as f32;

    rsx! {
        div {
            style: format!(
                "position:relative; width:{width}px; height:{height}px; \
                 background:#0a0a14; border-radius:4px; overflow:hidden; \
                 border:1px solid {border}; display:flex; align-items:flex-end;",
                border = t.border,
            ),

            // Level bars
            for (i, &level) in levels.iter().enumerate() {
                {
                    let bar_h = (level.clamp(0.0, 1.0) * height).max(0.0);
                    let x = i as f32 * bar_width;
                    let accent_dim = t.accent_dim;
                    rsx! {
                        div {
                            style: format!(
                                "position:absolute; left:{x}px; bottom:0; \
                                 width:{bar_width}px; height:{bar_h}px; \
                                 background:{accent_dim};"
                            ),
                        }
                    }
                }
            }

            // GR overlay bars (from top)
            for (i, &gr) in gr_levels.iter().enumerate() {
                {
                    let gr_h = (gr.clamp(0.0, 1.0) * height).max(0.0);
                    let x = i as f32 * bar_width;
                    rsx! {
                        div {
                            style: format!(
                                "position:absolute; left:{x}px; top:0; \
                                 width:{bar_width}px; height:{gr_h}px; \
                                 background:rgba(248,113,113,0.3);"
                            ),
                        }
                    }
                }
            }
        }
    }
}
