//! Gate graph widget.
//!
//! A Dioxus component that renders a noise gate transfer curve with interactive controls,
//! inspired by FabFilter Pro-G. Features:
//! - Transfer curve visualization (input dB vs output dB)
//! - Large draggable threshold knob (top-left)
//! - Smaller ratio and range knobs (below threshold)
//! - Envelope controls panel (attack, release, hold, knee, lookahead)
//! - Real-time gain reduction meter
//! - Input/output level visualization
//! - Blue-themed color scheme

use dioxus::prelude::*;

use crate::theming::context::ThemeContext;
use crate::widgets::knob::{Knob, KnobVariant};
use crate::theming::ThemeProvider;

/// Gate operating mode.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum GateMode {
    /// Standard downward gate (attenuate below threshold).
    #[default]
    Gate,
    /// Expander mode (more gradual attenuation).
    Expander,
    /// Ducker mode (attenuate above threshold, for ducking/sidechain).
    Ducker,
}

impl GateMode {
    /// Get display label.
    pub fn label(&self) -> &'static str {
        match self {
            Self::Gate => "Gate",
            Self::Expander => "Expander",
            Self::Ducker => "Ducker",
        }
    }

    /// All available modes.
    pub fn all() -> &'static [GateMode] {
        &[Self::Gate, Self::Expander, Self::Ducker]
    }
}

/// Gate parameters for the graph.
#[derive(Debug, Clone, PartialEq)]
pub struct GateParams {
    /// Operating mode.
    pub mode: GateMode,
    /// Threshold in dB (-60 to 0).
    pub threshold: f32,
    /// Expansion ratio (1:1 to infinity:1, stored as ratio value).
    /// For gates, higher ratio = harder gate. 1:1 = no gating, inf:1 = hard gate.
    pub ratio: f32,
    /// Range/floor in dB (how much to attenuate when closed, e.g. -80dB).
    pub range: f32,
    /// Knee width in dB (0 = hard knee, up to ~24 dB soft knee).
    pub knee: f32,
    /// Attack time in milliseconds (how fast the gate opens).
    pub attack: f32,
    /// Hold time in milliseconds (how long to stay open after signal drops).
    pub hold: f32,
    /// Release time in milliseconds (how fast the gate closes).
    pub release: f32,
    /// Lookahead in milliseconds.
    pub lookahead: f32,
    /// Hysteresis in dB (difference between open and close thresholds).
    pub hysteresis: f32,
    /// Bypass state.
    pub bypass: bool,
}

impl Default for GateParams {
    fn default() -> Self {
        Self {
            mode: GateMode::Gate,
            threshold: -30.0,
            ratio: 10.0, // 10:1 expansion ratio
            range: -80.0,
            knee: 6.0,
            attack: 0.5, // Fast attack
            hold: 50.0,
            release: 100.0,
            lookahead: 0.0,
            hysteresis: 0.0,
            bypass: false,
        }
    }
}

/// Real-time metering data for the gate.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct GateMetering {
    /// Current input level in dB.
    pub input_level: f32,
    /// Current output level in dB.
    pub output_level: f32,
    /// Current gain reduction in dB (negative value).
    pub gain_reduction: f32,
    /// Peak input level (with hold).
    pub input_peak: f32,
    /// Peak output level (with hold).
    pub output_peak: f32,
    /// Peak gain reduction.
    pub gr_peak: f32,
    /// Gate state: 0.0 = closed, 1.0 = open (for animation).
    pub gate_state: f32,
    /// History of gain reduction values for waveform display.
    pub gr_history: Vec<f32>,
    /// History of input level values for waveform display.
    pub input_history: Vec<f32>,
}

/// Default number of history samples to display.
pub const DEFAULT_HISTORY_SIZE: usize = 128;

/// dB range options for the graph.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum GateDbRange {
    /// -48 to 0 dB.
    Range48,
    /// -60 to 0 dB.
    #[default]
    Range60,
    /// -80 to 0 dB.
    Range80,
    /// -96 to 0 dB.
    Range96,
}

impl GateDbRange {
    /// Get the minimum dB value.
    pub fn min_db(&self) -> f32 {
        match self {
            Self::Range48 => -48.0,
            Self::Range60 => -60.0,
            Self::Range80 => -80.0,
            Self::Range96 => -96.0,
        }
    }

    /// Get display label.
    pub fn label(&self) -> &'static str {
        match self {
            Self::Range48 => "48 dB",
            Self::Range60 => "60 dB",
            Self::Range80 => "80 dB",
            Self::Range96 => "96 dB",
        }
    }
}

/// Graph layout constants
struct GraphLayout {
    width: f64,
    height: f64,
    padding: f64,
    graph_size: f64,
    min_db: f64,
}

impl GraphLayout {
    fn new(min_db: f32) -> Self {
        let width = 300.0;
        let height = 300.0;
        let padding = 32.0;
        let graph_size = width - 2.0 * padding;
        Self {
            width,
            height,
            padding,
            graph_size,
            min_db: min_db as f64,
        }
    }

    fn db_to_x(&self, db: f64) -> f64 {
        self.padding + ((db - self.min_db) / -self.min_db) * self.graph_size
    }

    fn db_to_y(&self, db: f64) -> f64 {
        self.padding + (1.0 - (db - self.min_db) / -self.min_db) * self.graph_size
    }
}

/// Compute gate transfer function output for a given input.
fn compute_gate_transfer(
    input_db: f64,
    mode: GateMode,
    threshold: f64,
    ratio: f64,
    knee: f64,
    range: f64,
) -> f64 {
    let knee_w = knee / 2.0;
    let low_th = threshold - knee_w;
    let high_th = threshold + knee_w;

    match mode {
        GateMode::Gate | GateMode::Expander => {
            // Below threshold: attenuate (expand downward)
            if input_db >= high_th {
                // Above knee: unity gain
                input_db
            } else if input_db > low_th {
                // In knee region: smooth transition
                let knee_factor = (high_th - input_db) / knee;
                let knee_factor_sq = knee_factor * knee_factor;
                let gain_reduction = knee_factor_sq * (ratio - 1.0) * (input_db - threshold) / ratio;
                (input_db + gain_reduction).max(range)
            } else {
                // Below threshold: expansion
                let expanded = threshold + (input_db - threshold) * ratio;
                expanded.max(range)
            }
        }
        GateMode::Ducker => {
            // Ducker: attenuate ABOVE threshold (inverse gate for ducking)
            if input_db <= low_th {
                // Below threshold: unity gain
                input_db
            } else if input_db < high_th {
                // In knee region
                let knee_factor = (input_db - low_th) / knee;
                let knee_factor_sq = knee_factor * knee_factor;
                let gain_reduction = knee_factor_sq * (1.0 - 1.0 / ratio) * (threshold - input_db);
                (input_db + gain_reduction).max(range)
            } else {
                // Above threshold: compress/duck
                let ducked = threshold + (input_db - threshold) / ratio;
                ducked.max(range)
            }
        }
    }
}

/// Props for the GateGraph component.
#[derive(Props, Clone, PartialEq)]
pub struct GateGraphProps {
    /// Current gate parameters.
    pub params: GateParams,
    /// Real-time metering data.
    #[props(default)]
    pub metering: GateMetering,
    /// dB range for the graph.
    #[props(default)]
    pub db_range: GateDbRange,
    /// Whether to show the grid.
    #[props(default = true)]
    pub show_grid: bool,
    /// Whether to show gain reduction meter.
    #[props(default = true)]
    pub show_gr_meter: bool,
    /// Whether to show input/output level visualization on curve.
    #[props(default = true)]
    pub show_levels: bool,
    /// Whether to show the GR history trace (scrolling waveform).
    #[props(default = true)]
    pub show_gr_trace: bool,
    /// Whether to show the knob controls panel.
    #[props(default = true)]
    pub show_controls: bool,
    /// Whether interaction is enabled.
    #[props(default = true)]
    pub interactive: bool,
    /// Callback when threshold changes.
    #[props(default)]
    pub on_threshold_change: Option<EventHandler<f32>>,
    /// Callback when ratio changes.
    #[props(default)]
    pub on_ratio_change: Option<EventHandler<f32>>,
    /// Callback when range changes.
    #[props(default)]
    pub on_range_change: Option<EventHandler<f32>>,
    /// Callback when knee changes.
    #[props(default)]
    pub on_knee_change: Option<EventHandler<f32>>,
    /// Callback when attack changes.
    #[props(default)]
    pub on_attack_change: Option<EventHandler<f32>>,
    /// Callback when hold changes.
    #[props(default)]
    pub on_hold_change: Option<EventHandler<f32>>,
    /// Callback when release changes.
    #[props(default)]
    pub on_release_change: Option<EventHandler<f32>>,
    /// Callback when lookahead changes.
    #[props(default)]
    pub on_lookahead_change: Option<EventHandler<f32>>,
    /// Callback when any parameter changes.
    #[props(default)]
    pub on_params_change: Option<EventHandler<GateParams>>,
}

/// Gate graph component.
///
/// Renders a noise gate interface inspired by FabFilter Pro-G with:
/// - Transfer curve graph
/// - Large threshold knob with smaller ratio/range knobs
/// - Envelope controls (attack, hold, release, knee, lookahead)
/// - Blue color theme
#[component]
pub fn GateGraph(props: GateGraphProps) -> Element {
    let layout = GraphLayout::new(props.db_range.min_db());

    // Create signals for knob controls
    let mut threshold_sig = use_signal(|| props.params.threshold);
    let mut ratio_sig = use_signal(|| props.params.ratio);
    let mut range_sig = use_signal(|| props.params.range);
    let mut knee_sig = use_signal(|| props.params.knee);
    let mut attack_sig = use_signal(|| props.params.attack);
    let mut hold_sig = use_signal(|| props.params.hold);
    let mut release_sig = use_signal(|| props.params.release);
    let mut lookahead_sig = use_signal(|| props.params.lookahead);

    // Sync signals with props
    use_effect(move || {
        threshold_sig.set(props.params.threshold);
        ratio_sig.set(props.params.ratio);
        range_sig.set(props.params.range);
        knee_sig.set(props.params.knee);
        attack_sig.set(props.params.attack);
        hold_sig.set(props.params.hold);
        release_sig.set(props.params.release);
        lookahead_sig.set(props.params.lookahead);
    });

    // Pre-compute values
    let threshold = props.params.threshold as f64;
    let ratio = props.params.ratio as f64;
    let knee = props.params.knee as f64;
    let range = props.params.range as f64;
    let mode = props.params.mode;
    let min_db = layout.min_db;

    // Generate transfer curve path
    let curve_path = {
        let mut path = String::new();
        let num_points = 200;
        for i in 0..=num_points {
            let input_db = min_db + (i as f64 / num_points as f64) * -min_db;
            let output_db = compute_gate_transfer(input_db, mode, threshold, ratio, knee, range);
            let x = layout.db_to_x(input_db);
            let y = layout.db_to_y(output_db.clamp(min_db, 0.0));
            if i == 0 {
                path.push_str(&format!("M {x:.1} {y:.1}"));
            } else {
                path.push_str(&format!(" L {x:.1} {y:.1}"));
            }
        }
        path
    };

    // Unity gain reference line (diagonal)
    let unity_path = format!(
        "M {:.1} {:.1} L {:.1} {:.1}",
        layout.padding,
        layout.padding + layout.graph_size,
        layout.padding + layout.graph_size,
        layout.padding
    );

    // Threshold marker position
    let threshold_x = layout.db_to_x(threshold);
    let threshold_y = layout.db_to_y(threshold);

    // Range floor line
    let range_y = layout.db_to_y(range.max(min_db));

    // Current input/output positions for level visualization
    let input_level = props.metering.input_level.clamp(min_db as f32, 0.0) as f64;
    let input_x = layout.db_to_x(input_level);
    let input_y = layout.db_to_y(input_level);
    let output_db = compute_gate_transfer(input_level, mode, threshold, ratio, knee, range);
    let output_y = layout.db_to_y(output_db.clamp(min_db, 0.0));

    // GR meter dimensions
    let gr_meter_width = 10.0;
    let gr_meter_x = layout.width - layout.padding + 6.0;

    // Grid lines
    let grid_lines = if props.show_grid {
        let step = if min_db <= -80.0 { 12.0 } else { 6.0 };
        let mut lines = Vec::new();
        let mut db = 0.0;
        while db > min_db {
            db -= step;
            if db > min_db {
                let pos = layout.db_to_x(db);
                lines.push(format!(
                    "M {:.1} {:.1} L {:.1} {:.1}",
                    pos,
                    layout.padding,
                    pos,
                    layout.padding + layout.graph_size
                ));
                let ypos = layout.db_to_y(db);
                lines.push(format!(
                    "M {:.1} {:.1} L {:.1} {:.1}",
                    layout.padding,
                    ypos,
                    layout.padding + layout.graph_size,
                    ypos
                ));
            }
        }
        lines.join(" ")
    } else {
        String::new()
    };

    // dB scale markers
    let db_markers: Vec<f64> = {
        let step = if min_db <= -80.0 { 12.0 } else { 6.0 };
        let mut m = vec![0.0];
        let mut db = 0.0;
        while db > min_db {
            db -= step;
            if db >= min_db {
                m.push(db);
            }
        }
        m
    };

    let marker_positions: Vec<(f64, f64, i32)> = db_markers
        .iter()
        .map(|&db| (layout.db_to_x(db), layout.db_to_y(db), db as i32))
        .collect();

    // GR history trace
    let gr_trace_path = if props.show_gr_trace && !props.metering.gr_history.is_empty() {
        let history = &props.metering.gr_history;
        let num_samples = history.len();
        let samples_to_show = num_samples.min(DEFAULT_HISTORY_SIZE);
        let start_idx = num_samples.saturating_sub(samples_to_show);

        let mut path = String::new();
        let x_step = layout.graph_size / (samples_to_show.max(1) as f64 - 1.0).max(1.0);
        let right_x = layout.padding + layout.graph_size;
        let unity_y = layout.db_to_y(0.0);

        path.push_str(&format!("M {:.1} {:.1}", right_x, unity_y));

        for i in 0..samples_to_show {
            let x = right_x - (i as f64) * x_step;
            path.push_str(&format!(" L {:.1} {:.1}", x, unity_y));
        }

        for i in (0..samples_to_show).rev() {
            let sample_idx = start_idx + (samples_to_show - 1 - i);
            let gr = history.get(sample_idx).copied().unwrap_or(0.0);
            let output_db = gr.clamp(min_db as f32, 0.0) as f64;
            let x = right_x - (i as f64) * x_step;
            let y = layout.db_to_y(output_db);
            path.push_str(&format!(" L {:.1} {:.1}", x, y));
        }

        path.push_str(" Z");
        path
    } else {
        String::new()
    };

    // Gate state indicator (open/closed)
    let gate_state = props.metering.gate_state;
    let gate_indicator_color = if gate_state > 0.5 {
        "rgba(59, 130, 246, 0.8)" // Blue when open
    } else {
        "rgba(59, 130, 246, 0.2)" // Dim blue when closed
    };

    // GR meter fill height
    let gr_db = props.metering.gain_reduction.abs().min(-min_db as f32);
    let gr_height = (gr_db as f64 / -min_db) * layout.graph_size;

    // Blue theme colors
    let curve_color = "#3b82f6"; // Blue-500
    let unity_color = "rgba(255, 255, 255, 0.15)";
    let grid_color = "rgba(255, 255, 255, 0.06)";
    let threshold_color = "#60a5fa"; // Blue-400
    let range_color = "rgba(96, 165, 250, 0.3)";
    let gr_color = "#2563eb"; // Blue-600
    let level_color = "#93c5fd"; // Blue-300
    let text_color = "rgba(255, 255, 255, 0.5)";

    // Value formatters
    let format_db = |v: f32| format!("{v:.1} dB");
    let format_ratio = |v: f32| {
        if v >= 100.0 {
            "∞:1".to_string()
        } else {
            format!("{v:.1}:1")
        }
    };
    let format_ms = |v: f32| {
        if v >= 1000.0 {
            format!("{:.2}s", v / 1000.0)
        } else {
            format!("{v:.1}ms")
        }
    };

    rsx! {
        ThemeProvider { theme: ThemeContext::new(),
            div {
                class: "gate-graph flex gap-4",
                style: "background: linear-gradient(180deg, #0f172a 0%, #1e293b 100%); border-radius: 12px; padding: 16px;",

                // Left side: Knob controls
                if props.show_controls {
                    div {
                        class: "gate-controls flex flex-col gap-4",
                        style: "min-width: 120px;",

                        // Large threshold knob
                        div {
                            class: "threshold-section",
                            Knob {
                                value: threshold_sig,
                                min: -60.0,
                                max: 0.0,
                                size: 72,
                                label: Some("THRESHOLD".to_string()),
                                value_display: Some(format_db(threshold_sig())),
                                disabled: !props.interactive,
                                on_change: move |v: f32| {
                                    if let Some(cb) = &props.on_threshold_change {
                                        cb.call(v);
                                    }
                                },
                            }
                        }

                        // Smaller ratio and range knobs
                        div {
                            class: "flex gap-2 justify-center",

                            Knob {
                                value: ratio_sig,
                                min: 1.0,
                                max: 100.0,
                                size: 48,
                                label: Some("RATIO".to_string()),
                                value_display: Some(format_ratio(ratio_sig())),
                                disabled: !props.interactive,
                                on_change: move |v: f32| {
                                    if let Some(cb) = &props.on_ratio_change {
                                        cb.call(v);
                                    }
                                },
                            }

                            Knob {
                                value: range_sig,
                                min: -96.0,
                                max: 0.0,
                                size: 48,
                                label: Some("RANGE".to_string()),
                                value_display: Some(format_db(range_sig())),
                                disabled: !props.interactive,
                                on_change: move |v: f32| {
                                    if let Some(cb) = &props.on_range_change {
                                        cb.call(v);
                                    }
                                },
                            }
                        }
                    }
                }

                // Center: Transfer curve graph
                div {
                    class: "gate-graph-area flex-1",

                    svg {
                        width: "100%",
                        height: "100%",
                        view_box: "0 0 {layout.width} {layout.height}",
                        preserve_aspect_ratio: "xMidYMid meet",
                        style: "background: #0d1117; border-radius: 8px;",

                        // Background
                        rect {
                            x: "{layout.padding}",
                            y: "{layout.padding}",
                            width: "{layout.graph_size}",
                            height: "{layout.graph_size}",
                            fill: "#080c14",
                            rx: "4",
                        }

                        // Grid lines
                        if props.show_grid && !grid_lines.is_empty() {
                            path {
                                d: "{grid_lines}",
                                stroke: "{grid_color}",
                                stroke_width: "1",
                                fill: "none",
                            }
                        }

                        // Range floor region (shaded area below range)
                        rect {
                            x: "{layout.padding}",
                            y: "{range_y}",
                            width: "{layout.graph_size}",
                            height: "{layout.padding + layout.graph_size - range_y}",
                            fill: "{range_color}",
                        }

                        // GR history trace
                        if props.show_gr_trace && !gr_trace_path.is_empty() {
                            path {
                                d: "{gr_trace_path}",
                                fill: "rgba(37, 99, 235, 0.25)",
                                stroke: "none",
                            }
                        }

                        // Unity gain line (diagonal)
                        path {
                            d: "{unity_path}",
                            stroke: "{unity_color}",
                            stroke_width: "1",
                            stroke_dasharray: "4,4",
                            fill: "none",
                        }

                        // Transfer curve
                        path {
                            d: "{curve_path}",
                            stroke: "{curve_color}",
                            stroke_width: "2.5",
                            stroke_linecap: "round",
                            stroke_linejoin: "round",
                            fill: "none",
                        }

                        // Threshold vertical line
                        line {
                            x1: "{threshold_x}",
                            y1: "{layout.padding}",
                            x2: "{threshold_x}",
                            y2: "{layout.padding + layout.graph_size}",
                            stroke: "{threshold_color}",
                            stroke_width: "1",
                            stroke_dasharray: "4,2",
                        }

                        // Threshold point
                        circle {
                            cx: "{threshold_x}",
                            cy: "{threshold_y}",
                            r: "6",
                            fill: "{threshold_color}",
                            stroke: "#fff",
                            stroke_width: "2",
                        }

                        // Gate state indicator (glow around threshold point)
                        circle {
                            cx: "{threshold_x}",
                            cy: "{threshold_y}",
                            r: "12",
                            fill: "none",
                            stroke: "{gate_indicator_color}",
                            stroke_width: "2",
                        }

                        // Input level indicator
                        if props.show_levels && props.metering.input_level > min_db as f32 {
                            // Vertical line from input to curve
                            line {
                                x1: "{input_x}",
                                y1: "{input_y}",
                                x2: "{input_x}",
                                y2: "{output_y}",
                                stroke: "{level_color}",
                                stroke_width: "2",
                                opacity: "0.7",
                            }
                            // Input point
                            circle {
                                cx: "{input_x}",
                                cy: "{input_y}",
                                r: "3",
                                fill: "{level_color}",
                            }
                            // Output point on curve
                            circle {
                                cx: "{input_x}",
                                cy: "{output_y}",
                                r: "4",
                                fill: "{curve_color}",
                                stroke: "#fff",
                                stroke_width: "1.5",
                            }
                        }

                        // GR meter
                        if props.show_gr_meter {
                            // GR meter background
                            rect {
                                x: "{gr_meter_x}",
                                y: "{layout.padding}",
                                width: "{gr_meter_width}",
                                height: "{layout.graph_size}",
                                fill: "#080c14",
                                rx: "2",
                            }
                            // GR meter fill (from top, grows downward)
                            rect {
                                x: "{gr_meter_x}",
                                y: "{layout.padding}",
                                width: "{gr_meter_width}",
                                height: "{gr_height}",
                                fill: "{gr_color}",
                                rx: "2",
                            }
                        }

                        // dB scale markers (X and Y axis)
                        for (x_pos, y_pos, db_int) in marker_positions.iter().cloned() {
                            text {
                                x: "{x_pos}",
                                y: "{layout.padding + layout.graph_size + 12.0}",
                                text_anchor: "middle",
                                fill: "{text_color}",
                                font_size: "8",
                                font_family: "system-ui, -apple-system, sans-serif",
                                "{db_int}"
                            }
                            text {
                                x: "{layout.padding - 4.0}",
                                y: "{y_pos + 3.0}",
                                text_anchor: "end",
                                fill: "{text_color}",
                                font_size: "8",
                                font_family: "system-ui, -apple-system, sans-serif",
                                "{db_int}"
                            }
                        }

                        // Threshold label
                        text {
                            x: "{threshold_x}",
                            y: "{layout.padding - 6.0}",
                            text_anchor: "middle",
                            fill: "{threshold_color}",
                            font_size: "9",
                            font_family: "system-ui, -apple-system, sans-serif",
                            "{props.params.threshold:.1} dB"
                        }

                        // GR label
                        if props.show_gr_meter {
                            text {
                                x: "{gr_meter_x + gr_meter_width / 2.0}",
                                y: "{layout.padding - 6.0}",
                                text_anchor: "middle",
                                fill: "{gr_color}",
                                font_size: "8",
                                font_family: "system-ui, -apple-system, sans-serif",
                                "GR"
                            }
                        }
                    }
                }

                // Right side: Envelope controls
                if props.show_controls {
                    div {
                        class: "envelope-controls flex flex-col gap-3",
                        style: "min-width: 100px; padding-left: 8px; border-left: 1px solid rgba(255,255,255,0.1);",

                        // Attack
                        div {
                            class: "flex flex-col items-center",
                            Knob {
                                value: attack_sig,
                                min: 0.01,
                                max: 250.0,
                                size: 40,
                                label: Some("ATTACK".to_string()),
                                value_display: Some(format_ms(attack_sig())),
                                disabled: !props.interactive,
                                on_change: move |v: f32| {
                                    if let Some(cb) = &props.on_attack_change {
                                        cb.call(v);
                                    }
                                },
                            }
                        }

                        // Hold
                        div {
                            class: "flex flex-col items-center",
                            Knob {
                                value: hold_sig,
                                min: 0.0,
                                max: 500.0,
                                size: 40,
                                label: Some("HOLD".to_string()),
                                value_display: Some(format_ms(hold_sig())),
                                disabled: !props.interactive,
                                on_change: move |v: f32| {
                                    if let Some(cb) = &props.on_hold_change {
                                        cb.call(v);
                                    }
                                },
                            }
                        }

                        // Release
                        div {
                            class: "flex flex-col items-center",
                            Knob {
                                value: release_sig,
                                min: 1.0,
                                max: 2000.0,
                                size: 40,
                                label: Some("RELEASE".to_string()),
                                value_display: Some(format_ms(release_sig())),
                                disabled: !props.interactive,
                                on_change: move |v: f32| {
                                    if let Some(cb) = &props.on_release_change {
                                        cb.call(v);
                                    }
                                },
                            }
                        }

                        // Knee
                        div {
                            class: "flex flex-col items-center",
                            Knob {
                                value: knee_sig,
                                min: 0.0,
                                max: 30.0,
                                size: 40,
                                label: Some("KNEE".to_string()),
                                value_display: Some(format_db(knee_sig())),
                                disabled: !props.interactive,
                                on_change: move |v: f32| {
                                    if let Some(cb) = &props.on_knee_change {
                                        cb.call(v);
                                    }
                                },
                            }
                        }

                        // Lookahead
                        div {
                            class: "flex flex-col items-center",
                            Knob {
                                value: lookahead_sig,
                                min: 0.0,
                                max: 10.0,
                                size: 40,
                                label: Some("LOOK".to_string()),
                                value_display: Some(format_ms(lookahead_sig())),
                                disabled: !props.interactive,
                                on_change: move |v: f32| {
                                    if let Some(cb) = &props.on_lookahead_change {
                                        cb.call(v);
                                    }
                                },
                            }
                        }
                    }
                }
            }
        }
    }
}
