//! FTS Meter editor — follows the exact VelloCanvas pattern from the compressor.
//!
//! Each tab is a separate Dioxus component containing a `VelloCanvas` (fill mode).
//! `VelloCanvas` handles its own `onmounted` + `get_client_rect()` + `set_rect()`
//! internally, which is exactly how `PeakWaveform` works in the compressor.
//!
//! The `SceneOverlayAdapter` wraps our `SceneOverlay` painters as `CanvasPainter`
//! (interior-mutable via `RefCell`) so they can be passed to `VelloCanvas`.

use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;
use std::sync::atomic::Ordering;

use audio_gui::controls::dropdown::Dropdown;
use audio_gui::controls::segment::SegmentButton;
use audio_gui::prelude::{use_init_theme, CanvasPainter, DragProvider, VelloCanvas};
use fts_plugin_core::prelude::*;
use nih_plug_dioxus::prelude::vello::kurbo::Affine;
use nih_plug_dioxus::prelude::vello::Scene;
use parking_lot::RwLock;

use meter_dsp::bit_depth::BitDepthState;
use meter_dsp::k_meter::KMeterState;
use meter_dsp::lufs::LufsState;
use meter_dsp::phase::PhaseState;
use meter_dsp::spectrum::SpectrumState;

use meter_ui::bit_meter_painter::{BitMeterConfig, BitMeterPainter};
use meter_ui::goniometer_painter::{GoniometerConfig, GoniometerPainter};
use meter_ui::k_meter_painter::{KMeterConfig, KMeterPainter};
use meter_ui::lufs_painter::{LufsConfig, LufsPainter};
use meter_ui::phase_painter::{PhaseConfig, PhasePainter};
use meter_ui::spectrograph_painter::{SpectrographConfig, SpectrographPainter};
use meter_ui::spectrum_painter::{SpectrumConfig, SpectrumPainter};

use crate::MeterUiState;

const TABS: &[&str] = &[
    "Spectrum",
    "Spectrograph",
    "LUFS",
    "K-Meter",
    "Goniometer",
    "Phase",
    "Bits",
];

// ── SceneOverlay → CanvasPainter adapter ─────────────────────────────────────

/// Wraps any `SceneOverlay` as a `CanvasPainter` for use with `VelloCanvas`.
///
/// Uses `RefCell` so the immutable `CanvasPainter::paint(&self)` can call the
/// mutable `SceneOverlay::paint(&mut self)` on the inner painter.
struct SceneOverlayAdapter<P: SceneOverlay + 'static>(Rc<RefCell<P>>);

impl<P: SceneOverlay + 'static> Clone for SceneOverlayAdapter<P> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<P: SceneOverlay + 'static> CanvasPainter for SceneOverlayAdapter<P> {
    fn paint(&self, scene: &mut Scene, transform: Affine, width: f64, height: f64) {
        self.0
            .borrow_mut()
            .paint(scene, transform, width as u32, height as u32, 1.0);
    }
}

/// Convenience: wrap a painter into the type-erased form `VelloCanvas` expects.
fn make_canvas<P: SceneOverlay + 'static>(painter: P) -> Rc<RefCell<dyn CanvasPainter>> {
    Rc::new(RefCell::new(SceneOverlayAdapter(Rc::new(RefCell::new(
        painter,
    )))))
}

// ── Frequency / dB helpers (module-level, usable in RSX) ─────────────────────

fn freq_to_pct(f: f64) -> f64 {
    (f.log10() - 20.0_f64.log10()) / (20_000.0_f64.log10() - 20.0_f64.log10()) * 100.0
}

fn db_to_pct_spectrum(db: f64) -> f64 {
    // min_db = -120, max_db = 0  →  0% = top (0 dBFS), 100% = bottom (-120 dBFS)
    (0.0 - db) / 120.0 * 100.0
}

// ── Per-tab view components ───────────────────────────────────────────────────
//
// Each is a thin wrapper: creates the painter once via `use_hook`, then hands it
// to `VelloCanvas { fill: true, background: true }`.  Mounting/unmounting is
// handled naturally when the parent switches tabs.
//
// Props structs use manual `PartialEq` via `Arc::ptr_eq` because the inner
// state types contain `RwLock` and cannot derive `PartialEq`.

#[derive(Clone, Props)]
struct SpectrumViewProps {
    state: Arc<SpectrumState>,
    r_state: Arc<SpectrumState>,
}
impl PartialEq for SpectrumViewProps {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.state, &other.state) && Arc::ptr_eq(&self.r_state, &other.r_state)
    }
}

const SLOPES: &[(f64, &str)] = &[
    (0.0, "0"),
    (1.5, "1.5"),
    (3.0, "3"),
    (4.5, "4.5"),
    (6.0, "6"),
    (9.0, "9"),
];

const SMOOTHINGS: &[(f64, &str)] = &[
    (0.0, "Off"),
    (0.17, "1/6"),
    (0.33, "1/3"),
    (0.5, "1/2"),
    (1.0, "1 oct"),
];

const FFT_SIZES: &[(usize, &str)] = &[
    (512, "512"),
    (1024, "1024"),
    (2048, "2048"),
    (4096, "4096"),
    (8192, "8192"),
];

#[allow(non_snake_case)]
fn SpectrumView(props: SpectrumViewProps) -> Element {
    // Config shared with the main painter
    let config = use_hook(|| Arc::new(RwLock::new(SpectrumConfig::default())));

    // Underlay config (right channel, warm color, no grid, no background)
    let ulay_config = use_hook(|| {
        Arc::new(RwLock::new(SpectrumConfig {
            color: nih_plug_dioxus::prelude::vello::peniko::Color::from_rgba8(255, 160, 80, 160),
            max_color: nih_plug_dioxus::prelude::vello::peniko::Color::from_rgba8(
                255, 160, 80, 80,
            ),
            show_max: false,
            show_grid: false,
            draw_background: false,
            smooth_octaves: 0.5,
            slope: 4.5,
            ..SpectrumConfig::default()
        }))
    });

    // Painters
    let main_painter =
        use_hook(|| make_canvas(SpectrumPainter::new(props.state.clone(), config.clone())));
    let ulay_painter =
        use_hook(|| make_canvas(SpectrumPainter::new(props.r_state.clone(), ulay_config.clone())));

    // UI state
    let mut show_underlay = use_signal(|| false);
    let mut slope_idx = use_signal(|| 3usize); // 4.5 dB/oct default
    let mut smooth_idx = use_signal(|| 3usize); // 1/2 oct default
    let mut fft_idx = use_signal(|| 2usize); // 2048 default
    let mut show_max = use_signal(|| true);

    // Cursor tracking (fraction 0..1 of spectrum area width)
    let mut cursor_frac: Signal<Option<f64>> = use_signal(|| None);

    // Apply slope/smooth/max changes to config
    {
        let slope = SLOPES[*slope_idx.read()].0;
        let smooth = SMOOTHINGS[*smooth_idx.read()].0;
        let show_mx = *show_max.read();
        let mut cfg = config.write();
        cfg.slope = slope;
        cfg.smooth_octaves = smooth;
        cfg.show_max = show_mx;
        // Sync underlay slope/smooth
        let mut ucfg = ulay_config.write();
        ucfg.slope = slope;
        ucfg.smooth_octaves = smooth;
    }

    // Apply FFT size change
    {
        let desired = FFT_SIZES[*fft_idx.read()].0;
        let current = props.state.fft_size.load(Ordering::Relaxed);
        if desired != current {
            props.state.desired_fft_size.store(desired, Ordering::Relaxed);
            props.r_state.desired_fft_size.store(desired, Ordering::Relaxed);
        }
    }

    // Read stats
    let rms = props.state.rms_db.load(Ordering::Relaxed);
    let peak = props.state.peak_db.load(Ordering::Relaxed);
    let clipped = props.state.clipped.load(Ordering::Relaxed);
    let fft_size_current = props.state.fft_size.load(Ordering::Relaxed);

    // Compute cursor frequency from fraction
    let cursor_freq_str: String = if let Some(frac) = *cursor_frac.read() {
        let min_freq = config.read().min_freq;
        let max_freq = config.read().max_freq;
        let freq = min_freq * (max_freq / min_freq).powf(frac);
        if freq >= 1000.0 {
            format!("{:.1} kHz", freq / 1000.0)
        } else {
            format!("{:.0} Hz", freq)
        }
    } else {
        String::new()
    };

    let cursor_pct = (*cursor_frac.read()).map(|f| f * 100.0).unwrap_or(-100.0);

    // Format stat strings
    let rms_str = if rms.is_finite() {
        format!("{:.1}", rms)
    } else {
        "-\u{221e}".to_string()
    };
    let peak_str = if peak.is_finite() {
        format!("{:.1}", peak)
    } else {
        "-\u{221e}".to_string()
    };
    let peak_color_style = if clipped { "color:#ff4444;" } else { "" };

    // Frequency label positions
    let freq_labels: &[(f64, &str)] = &[
        (20.0, "20"),
        (50.0, "50"),
        (100.0, "100"),
        (200.0, "200"),
        (500.0, "500"),
        (1000.0, "1k"),
        (2000.0, "2k"),
        (5000.0, "5k"),
        (10000.0, "10k"),
        (20000.0, "20k"),
    ];

    // dB label positions
    let db_labels: &[(f64, &str)] = &[
        (-20.0, "-20"),
        (-40.0, "-40"),
        (-60.0, "-60"),
        (-80.0, "-80"),
        (-100.0, "-100"),
    ];

    let slope_items: Vec<String> =
        SLOPES.iter().map(|(_, s)| format!("{} dB/oct", s)).collect();
    let smooth_items: Vec<String> = SMOOTHINGS.iter().map(|(_, s)| s.to_string()).collect();
    let fft_items: Vec<String> = FFT_SIZES.iter().map(|(_, s)| s.to_string()).collect();

    let show_underlay_val = *show_underlay.read();
    let show_max_val = *show_max.read();

    rsx! {
        div {
            style: "display:flex; flex-direction:column; width:100%; height:100%; background:transparent;",

            // ── Controls bar ─────────────────────────────────────────────────
            div {
                style: "display:flex; align-items:center; gap:12px; padding:3px 8px; \
                        flex-shrink:0; background:rgba(0,0,0,0.7); \
                        border-bottom:1px solid rgba(255,255,255,0.08); \
                        font-size:11px; user-select:none;",

                span { style: "color:#888;", "FFT" }
                Dropdown {
                    items: fft_items,
                    selected: *fft_idx.read(),
                    on_change: move |i| fft_idx.set(i),
                    width: "70px",
                }

                span { style: "color:#888;", "Slope" }
                Dropdown {
                    items: slope_items,
                    selected: *slope_idx.read(),
                    on_change: move |i| slope_idx.set(i),
                    width: "90px",
                }

                span { style: "color:#888;", "Smooth" }
                Dropdown {
                    items: smooth_items,
                    selected: *smooth_idx.read(),
                    on_change: move |i| smooth_idx.set(i),
                    width: "75px",
                }

                // Max toggle
                div {
                    style: format!(
                        "padding:2px 8px; border-radius:3px; cursor:pointer; \
                         font-size:11px; border:1px solid; \
                         background:{}; border-color:{}; color:{};",
                        if show_max_val { "rgba(100,200,255,0.15)" } else { "transparent" },
                        if show_max_val { "rgba(100,200,255,0.5)" } else { "rgba(255,255,255,0.2)" },
                        if show_max_val { "#64c8ff" } else { "#888" },
                    ),
                    onclick: move |_| show_max.set(!show_max_val),
                    "Max"
                }

                // Underlay (R channel) toggle
                div {
                    style: format!(
                        "padding:2px 8px; border-radius:3px; cursor:pointer; \
                         font-size:11px; border:1px solid; \
                         background:{}; border-color:{}; color:{};",
                        if show_underlay_val { "rgba(255,160,80,0.15)" } else { "transparent" },
                        if show_underlay_val { "rgba(255,160,80,0.5)" } else { "rgba(255,255,255,0.2)" },
                        if show_underlay_val { "#ffa050" } else { "#888" },
                    ),
                    onclick: move |_| show_underlay.set(!show_underlay_val),
                    "R"
                }

                // Current FFT size display
                span { style: "color:#555; margin-left:auto;", "{fft_size_current} pts" }
            }

            // ── Spectrum area ────────────────────────────────────────────────
            div {
                style: "position:relative; flex:1; min-height:0; background:transparent;",

                // Main spectrum canvas — in normal flow so get_client_rect works on first mount
                VelloCanvas { painter: main_painter.clone(), fill: true, background: true }

                // Underlay canvas (R channel) — absolute, paints behind main in Vello layer order
                if show_underlay_val {
                    div { style: "position:absolute; inset:0; background:transparent;",
                        VelloCanvas { painter: ulay_painter.clone(), fill: true, background: true }
                    }
                }

                // dB labels (absolute left overlay)
                div {
                    style: "position:absolute; left:0; top:0; bottom:20px; width:28px; \
                            pointer-events:none; background:transparent;",
                    for &(db, label) in db_labels {
                        span {
                            style: format!(
                                "position:absolute; right:2px; top:{pct:.1}%; \
                                 transform:translateY(-50%); \
                                 font-size:9px; color:rgba(180,180,180,0.6); \
                                 font-family:monospace; white-space:nowrap;",
                                pct = db_to_pct_spectrum(db),
                            ),
                            "{label}"
                        }
                    }
                }

                // Freq labels (absolute bottom overlay)
                div {
                    style: "position:absolute; bottom:0; left:28px; right:0; height:18px; \
                            pointer-events:none; background:transparent;",
                    for &(freq, label) in freq_labels {
                        span {
                            style: format!(
                                "position:absolute; left:{pct:.1}%; \
                                 transform:translateX(-50%); \
                                 font-size:9px; color:rgba(180,180,180,0.6); \
                                 font-family:monospace; white-space:nowrap; bottom:2px;",
                                pct = freq_to_pct(freq),
                            ),
                            "{label}"
                        }
                    }
                }

                // Cursor / interaction overlay
                div {
                    style: "position:absolute; inset:0; cursor:crosshair; background:transparent;",
                    onmousemove: move |e| {
                        let coords = e.data().element_coordinates();
                        let x = coords.x;
                        // Approximate: divide by 800 px as a reasonable default width.
                        // The cursor line position uses the same fraction so it stays correct
                        // relative to itself; only the frequency readout is approximate.
                        cursor_frac.set(Some((x.max(0.0) / 800.0).min(1.0)));
                    },
                    onmouseleave: move |_| {
                        cursor_frac.set(None);
                    },

                    // Cursor line
                    if cursor_frac.read().is_some() {
                        div {
                            style: format!(
                                "position:absolute; top:0; bottom:0; width:1px; \
                                 background:rgba(255,255,255,0.3); pointer-events:none; \
                                 left:{:.1}%;",
                                cursor_pct,
                            ),
                        }
                        // Frequency label near cursor
                        div {
                            style: format!(
                                "position:absolute; top:4px; \
                                 background:rgba(0,0,0,0.7); \
                                 padding:2px 4px; border-radius:2px; \
                                 font-size:10px; color:#fff; pointer-events:none; \
                                 left:{:.1}%; transform:translateX(-50%);",
                                cursor_pct,
                            ),
                            "{cursor_freq_str}"
                        }
                    }
                }
            }

            // ── Stats bar ────────────────────────────────────────────────────
            div {
                style: "display:flex; gap:20px; padding:3px 8px; flex-shrink:0; \
                        background:rgba(0,0,0,0.7); \
                        border-top:1px solid rgba(255,255,255,0.08); \
                        font-size:11px; font-family:monospace; align-items:center;",

                span { style: "color:#888;", "L" }
                span { style: "color:#aaa;", "RMS " }
                span { style: "color:#64c8ff;", "{rms_str} dBFS" }
                span { style: "color:#aaa;", "  Peak " }
                span { style: format!("color:#64c8ff; {}", peak_color_style), "{peak_str} dBFS" }
                if clipped {
                    span { style: "color:#ff4444; font-weight:bold;", "CLIP" }
                }

                // Reset stats button
                div {
                    style: "margin-left:auto; padding:1px 8px; \
                            border:1px solid rgba(255,255,255,0.2); \
                            border-radius:3px; cursor:pointer; color:#888; font-size:10px;",
                    onclick: {
                        let state = props.state.clone();
                        let r_state = props.r_state.clone();
                        move |_| {
                            state.peak_db.store(f32::NEG_INFINITY, Ordering::Relaxed);
                            state.clipped.store(false, Ordering::Relaxed);
                            r_state.peak_db.store(f32::NEG_INFINITY, Ordering::Relaxed);
                            r_state.clipped.store(false, Ordering::Relaxed);
                            {
                                let mut bins = state.max_bins_db.write();
                                for v in bins.iter_mut() {
                                    *v = f32::NEG_INFINITY;
                                }
                            }
                            {
                                let mut bins = r_state.max_bins_db.write();
                                for v in bins.iter_mut() {
                                    *v = f32::NEG_INFINITY;
                                }
                            }
                        }
                    },
                    "Reset"
                }
            }
        }
    }
}

#[derive(Clone, Props)]
struct SpectrographViewProps {
    state: Arc<SpectrumState>,
}
impl PartialEq for SpectrographViewProps {
    fn eq(&self, other: &Self) -> bool { Arc::ptr_eq(&self.state, &other.state) }
}

fn SpectrographView(props: SpectrographViewProps) -> Element {
    let painter = use_hook(|| {
        make_canvas(SpectrographPainter::new(props.state.clone(), SpectrographConfig::default()))
    });
    rsx! {
        VelloCanvas { painter: painter, fill: true, background: true }
    }
}

#[derive(Clone, Props)]
struct LufsViewProps {
    state: Arc<LufsState>,
}
impl PartialEq for LufsViewProps {
    fn eq(&self, other: &Self) -> bool { Arc::ptr_eq(&self.state, &other.state) }
}

fn LufsView(props: LufsViewProps) -> Element {
    let painter =
        use_hook(|| make_canvas(LufsPainter::new(props.state.clone(), LufsConfig::default())));
    rsx! {
        VelloCanvas { painter: painter, fill: true, background: true }
    }
}

#[derive(Clone, Props)]
struct KMeterViewProps {
    l_state: Arc<KMeterState>,
    r_state: Arc<KMeterState>,
}
impl PartialEq for KMeterViewProps {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.l_state, &other.l_state) && Arc::ptr_eq(&self.r_state, &other.r_state)
    }
}

fn KMeterView(props: KMeterViewProps) -> Element {
    let painter_l =
        use_hook(|| make_canvas(KMeterPainter::new(props.l_state.clone(), KMeterConfig::default())));
    let painter_r =
        use_hook(|| make_canvas(KMeterPainter::new(props.r_state.clone(), KMeterConfig::default())));
    rsx! {
        div { style: "display:flex; width:100%; height:100%;",
            div { style: "flex:1; height:100%;",
                VelloCanvas { painter: painter_l, fill: true, background: true }
            }
            div { style: "flex:1; height:100%;",
                VelloCanvas { painter: painter_r, fill: true, background: true }
            }
        }
    }
}

#[derive(Clone, Props)]
struct GoniometerViewProps {
    state: Arc<PhaseState>,
}
impl PartialEq for GoniometerViewProps {
    fn eq(&self, other: &Self) -> bool { Arc::ptr_eq(&self.state, &other.state) }
}

fn GoniometerView(props: GoniometerViewProps) -> Element {
    let painter = use_hook(|| {
        make_canvas(GoniometerPainter::new(props.state.clone(), GoniometerConfig::default()))
    });
    rsx! {
        VelloCanvas { painter: painter, fill: true, background: true }
    }
}

#[derive(Clone, Props)]
struct PhaseViewProps {
    state: Arc<PhaseState>,
}
impl PartialEq for PhaseViewProps {
    fn eq(&self, other: &Self) -> bool { Arc::ptr_eq(&self.state, &other.state) }
}

fn PhaseView(props: PhaseViewProps) -> Element {
    let painter =
        use_hook(|| make_canvas(PhasePainter::new(props.state.clone(), PhaseConfig::default())));
    rsx! {
        VelloCanvas { painter: painter, fill: true, background: true }
    }
}

#[derive(Clone, Props)]
struct BitsViewProps {
    state: Arc<BitDepthState>,
}
impl PartialEq for BitsViewProps {
    fn eq(&self, other: &Self) -> bool { Arc::ptr_eq(&self.state, &other.state) }
}

fn BitsView(props: BitsViewProps) -> Element {
    let painter =
        use_hook(|| make_canvas(BitMeterPainter::new(props.state.clone(), BitMeterConfig::default())));
    rsx! {
        VelloCanvas { painter: painter, fill: true, background: true }
    }
}

// ── Root component ────────────────────────────────────────────────────────────

/// Root editor component.
#[component]
pub fn App() -> Element {
    let t = use_init_theme();
    let t = *t.read();

    let shared = use_context::<SharedState>();
    let ui = shared.get::<MeterUiState>().expect("MeterUiState missing");
    let mut tab = use_signal(|| 0usize);

    let cur_tab = *tab.read();

    rsx! {
        document::Style { {format!(
            "*, *::before, *::after {{ box-sizing: border-box; margin: 0; padding: 0; }} \
             html, body {{ width: 100%; height: 100%; overflow: hidden; \
             background: transparent; color: {text}; \
             font-family: {font}; font-size: 13px; }}",
            text = t.text,
            font = t.font_family,
        )} }

        DragProvider {
            div {
                style: format!(
                    "width:100vw; height:100vh; display:flex; flex-direction:column; \
                     color:{text}; font-family:{font}; font-size:13px; \
                     user-select:none; position:relative;",
                    text = t.text,
                    font = t.font_family,
                ),

                // ── Tab bar ──────────────────────────────────────────────────
                div {
                    style: format!(
                        "display:flex; flex-wrap:wrap; align-items:center; gap:4px; \
                         padding:4px 8px; min-height:36px; flex-shrink:0; \
                         background:{bg}; border-bottom:1px solid {border};",
                        bg = t.card_bg,
                        border = t.border_subtle,
                    ),
                    for (i, label) in TABS.iter().enumerate() {
                        SegmentButton {
                            label: label,
                            selected: cur_tab == i,
                            on_click: move |_| { tab.set(i); },
                        }
                    }
                }

                // ── Active meter view ─────────────────────────────────────────
                // Only the selected tab's component is mounted. Each mounts its
                // own VelloCanvas which self-measures and registers its overlay.
                div { style: "flex:1; min-height:0; background:transparent;",
                    match cur_tab {
                        0 => rsx! {
                            SpectrumView {
                                state: ui.spectrum_state.clone(),
                                r_state: ui.spectrum_r_state.clone(),
                            }
                        },
                        1 => rsx! { SpectrographView { state: ui.spectrum_state.clone() } },
                        2 => rsx! { LufsView { state: ui.lufs_state.clone() } },
                        3 => rsx! {
                            KMeterView {
                                l_state: ui.k_meter_l_state.clone(),
                                r_state: ui.k_meter_r_state.clone(),
                            }
                        },
                        4 => rsx! { GoniometerView { state: ui.phase_state.clone() } },
                        5 => rsx! { PhaseView { state: ui.phase_state.clone() } },
                        _ => rsx! { BitsView { state: ui.bits_state.clone() } },
                    }
                }
            }
        }
    }
}
