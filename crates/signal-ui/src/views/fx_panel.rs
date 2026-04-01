//! FX panel — embedded EQ and Compressor GUI backed by live DSP.

use nih_plug_dioxus::prelude::*;

use audio_gui::meters::GrMeter;
use audio_gui::viz::{EqBand, EqBandShape, EqGraph};
use eq_dsp::filter_type::FilterType;
use signal_audio::ProcessingChain;

// ── Mapping helpers ────────────────────────────────────────────────────────────

fn eq_band_shape_to_filter_type(shape: EqBandShape) -> FilterType {
    match shape {
        EqBandShape::Bell => FilterType::Peak,
        EqBandShape::LowShelf => FilterType::LowShelf,
        EqBandShape::HighShelf => FilterType::HighShelf,
        EqBandShape::LowCut => FilterType::Highpass,
        EqBandShape::HighCut => FilterType::Lowpass,
        EqBandShape::Notch => FilterType::Notch,
        EqBandShape::BandPass => FilterType::Bandpass,
        EqBandShape::TiltShelf => FilterType::TiltShelf,
        EqBandShape::FlatTilt => FilterType::FlatTilt,
        EqBandShape::AllPass => FilterType::Allpass,
    }
}

fn filter_type_to_eq_band_shape(ft: FilterType) -> EqBandShape {
    match ft {
        FilterType::Peak => EqBandShape::Bell,
        FilterType::LowShelf => EqBandShape::LowShelf,
        FilterType::HighShelf => EqBandShape::HighShelf,
        FilterType::Highpass => EqBandShape::LowCut,
        FilterType::Lowpass => EqBandShape::HighCut,
        FilterType::Notch => EqBandShape::Notch,
        FilterType::Bandpass => EqBandShape::BandPass,
        FilterType::TiltShelf => EqBandShape::TiltShelf,
        FilterType::FlatTilt => EqBandShape::FlatTilt,
        FilterType::Allpass => EqBandShape::AllPass,
        // BandShelf has no direct EqBandShape equivalent — fall back to Bell
        FilterType::BandShelf => EqBandShape::Bell,
    }
}

// ── Top-level FX view ──────────────────────────────────────────────────────────

/// Top-level FX view — renders EQ and Compressor panels side-by-side.
///
/// Gets the [`ProcessingChain`] from Dioxus context (injected by the desktop
/// app's `App` component). Renders a placeholder if no chain is available.
#[component]
pub fn FxView() -> Element {
    let chain = use_context::<ProcessingChain>();

    rsx! {
        div {
            style: "display:flex; flex-direction:row; gap:16px; width:100%; height:100%; \
                    padding:16px; box-sizing:border-box; overflow:auto;",
            EqPanel { chain: chain.clone() }
            CompPanel { chain: chain.clone() }
        }
    }
}

// ── EQ panel ──────────────────────────────────────────────────────────────────

#[component]
fn EqPanel(chain: ProcessingChain) -> Element {
    // Initialise EqBand state from the chain's current eq bands.
    let mut bands: Signal<Vec<EqBand>> = use_signal(|| {
        if let Ok(eq) = chain.eq.lock() {
            let n = eq.num_bands();
            (0..n)
                .filter_map(|i| {
                    eq.band(i).map(|b| EqBand {
                        index: i,
                        used: true,
                        enabled: b.enabled,
                        frequency: b.freq_hz as f32,
                        gain: b.gain_db as f32,
                        q: b.q as f32,
                        shape: filter_type_to_eq_band_shape(b.filter_type),
                        ..Default::default()
                    })
                })
                .collect()
        } else {
            // Fallback default bands if lock fails
            vec![
                EqBand {
                    index: 0,
                    used: true,
                    enabled: true,
                    frequency: 80.0,
                    gain: 0.0,
                    q: 0.707,
                    shape: EqBandShape::LowCut,
                    ..Default::default()
                },
                EqBand {
                    index: 1,
                    used: true,
                    enabled: true,
                    frequency: 500.0,
                    gain: 0.0,
                    q: 1.0,
                    shape: EqBandShape::Bell,
                    ..Default::default()
                },
                EqBand {
                    index: 2,
                    used: true,
                    enabled: true,
                    frequency: 8000.0,
                    gain: 0.0,
                    q: 0.707,
                    shape: EqBandShape::HighShelf,
                    ..Default::default()
                },
            ]
        }
    });

    let chain_on_change = chain.clone();
    let mut on_band_change = move |(idx, band): (usize, EqBand)| {
        // Update UI state
        let mut bs = bands.write();
        if idx < bs.len() {
            bs[idx] = band.clone();
        }
        drop(bs);

        // Sync to DSP chain
        if let Ok(mut eq) = chain_on_change.eq.try_lock() {
            if let Some(b) = eq.band_mut(idx) {
                b.filter_type = eq_band_shape_to_filter_type(band.shape);
                b.freq_hz = band.frequency as f64;
                b.gain_db = band.gain as f64;
                b.q = band.q as f64;
                b.enabled = band.enabled;
            }
            eq.update_band(idx);
        }
    };

    let chain_on_add = chain.clone();
    let mut on_band_add = move |band: EqBand| {
        if let Ok(mut eq) = chain_on_add.eq.try_lock() {
            let idx = eq.add_band();
            if let Some(b) = eq.band_mut(idx) {
                b.filter_type = eq_band_shape_to_filter_type(band.shape);
                b.freq_hz = band.frequency as f64;
                b.gain_db = band.gain as f64;
                b.q = band.q as f64;
                b.enabled = band.enabled;
            }
            eq.update_band(idx);
        }
        let mut new_band = band.clone();
        new_band.index = bands.read().len();
        bands.write().push(new_band);
    };

    let chain_on_remove = chain.clone();
    let mut on_band_remove = move |idx: usize| {
        if let Ok(mut eq) = chain_on_remove.eq.try_lock() {
            eq.remove_band(idx);
        }
        let mut bs = bands.write();
        if idx < bs.len() {
            bs.remove(idx);
            // Re-index
            for (i, b) in bs.iter_mut().enumerate() {
                b.index = i;
            }
        }
    };

    rsx! {
        div {
            style: "flex:1; min-width:0; background:#1a1a1a; border-radius:8px; \
                    padding:16px; box-sizing:border-box; display:flex; flex-direction:column; gap:8px;",
            div {
                style: "font-size:12px; font-weight:600; color:#a1a1aa; text-transform:uppercase; \
                        letter-spacing:0.08em;",
                "EQ"
            }
            EqGraph {
                bands: bands,
                sample_rate: 48000.0,
                on_band_change: move |(idx, band)| on_band_change((idx, band)),
                on_band_add: move |band| on_band_add(band),
                on_band_remove: move |idx| on_band_remove(idx),
            }
        }
    }
}

// ── Compressor panel ───────────────────────────────────────────────────────────

#[component]
fn CompPanel(chain: ProcessingChain) -> Element {
    let mut threshold = use_signal(|| -18.0f64);
    let mut ratio = use_signal(|| 4.0f64);
    let mut attack = use_signal(|| 10.0f64);
    let mut release = use_signal(|| 100.0f64);
    let mut auto_makeup = use_signal(|| false);

    // Initialise from chain
    use_hook(|| {
        if let Ok(comp) = chain.comp.lock() {
            threshold.set(comp.comp.threshold_db);
            ratio.set(comp.comp.ratio);
            attack.set(comp.comp.attack_ms);
            release.set(comp.comp.release_ms);
            auto_makeup.set(comp.comp.auto_makeup);
        }
    });

    let gr = chain.gain_reduction_db();

    let chain_thresh = chain.clone();
    let chain_ratio = chain.clone();
    let chain_attack = chain.clone();
    let chain_release = chain.clone();
    let chain_makeup = chain.clone();

    rsx! {
        div {
            style: "width:260px; flex-shrink:0; background:#1a1a1a; border-radius:8px; \
                    padding:16px; box-sizing:border-box; display:flex; flex-direction:column; gap:12px;",

            // Title
            div {
                style: "font-size:12px; font-weight:600; color:#a1a1aa; text-transform:uppercase; \
                        letter-spacing:0.08em;",
                "Compressor"
            }

            // GR Meter + params row
            div {
                style: "display:flex; flex-direction:row; gap:12px; align-items:flex-start;",

                // GR meter
                GrMeter {
                    gain_reduction_db: gr,
                    height: 150.0,
                }

                // Parameter sliders
                div {
                    style: "flex:1; display:flex; flex-direction:column; gap:10px;",

                    // Threshold
                    ParamRow {
                        label: "Threshold",
                        value: *threshold.read(),
                        min: -60.0,
                        max: 0.0,
                        unit: " dB",
                        on_change: move |v: f64| {
                            threshold.set(v);
                            if let Ok(mut comp) = chain_thresh.comp.try_lock() {
                                comp.comp.threshold_db = v;
                            }
                        },
                    }

                    // Ratio
                    ParamRow {
                        label: "Ratio",
                        value: *ratio.read(),
                        min: 1.0,
                        max: 20.0,
                        unit: ":1",
                        on_change: move |v: f64| {
                            ratio.set(v);
                            if let Ok(mut comp) = chain_ratio.comp.try_lock() {
                                comp.comp.ratio = v;
                            }
                        },
                    }

                    // Attack
                    ParamRow {
                        label: "Attack",
                        value: *attack.read(),
                        min: 0.1,
                        max: 200.0,
                        unit: " ms",
                        on_change: move |v: f64| {
                            attack.set(v);
                            if let Ok(mut comp) = chain_attack.comp.try_lock() {
                                comp.comp.attack_ms = v;
                            }
                        },
                    }

                    // Release
                    ParamRow {
                        label: "Release",
                        value: *release.read(),
                        min: 5.0,
                        max: 2000.0,
                        unit: " ms",
                        on_change: move |v: f64| {
                            release.set(v);
                            if let Ok(mut comp) = chain_release.comp.try_lock() {
                                comp.comp.release_ms = v;
                            }
                        },
                    }
                }
            }

            // Auto Makeup toggle
            div {
                style: "display:flex; align-items:center; gap:8px;",
                button {
                    style: {
                        let active = *auto_makeup.read();
                        if active {
                            "padding:4px 10px; border-radius:4px; font-size:11px; font-weight:600; \
                             background:#3b82f6; color:#fff; border:none; cursor:pointer;"
                        } else {
                            "padding:4px 10px; border-radius:4px; font-size:11px; font-weight:600; \
                             background:#27272a; color:#a1a1aa; border:1px solid #3f3f46; cursor:pointer;"
                        }
                    },
                    onclick: move |_| {
                        let new_val = !*auto_makeup.read();
                        auto_makeup.set(new_val);
                        if let Ok(mut comp) = chain_makeup.comp.try_lock() {
                            comp.comp.auto_makeup = new_val;
                        }
                    },
                    "Auto Makeup"
                }
            }
        }
    }
}

// ── Helper: labeled parameter row with Slider ─────────────────────────────────

#[component]
fn ParamRow(
    label: &'static str,
    value: f64,
    min: f64,
    max: f64,
    unit: &'static str,
    on_change: EventHandler<f64>,
) -> Element {
    let display = format!("{:.1}{unit}", value);

    rsx! {
        div {
            style: "display:flex; flex-direction:column; gap:3px;",
            div {
                style: "display:flex; justify-content:space-between; align-items:baseline;",
                span {
                    style: "font-size:10px; color:#71717a; text-transform:uppercase; letter-spacing:0.06em;",
                    "{label}"
                }
                span {
                    style: "font-size:11px; color:#e4e4e7; font-variant-numeric:tabular-nums;",
                    "{display}"
                }
            }
            audio_gui::controls::Slider {
                value: value,
                min: min,
                max: max,
                on_change: Some(Callback::new(move |v: f64| on_change.call(v))),
            }
        }
    }
}
