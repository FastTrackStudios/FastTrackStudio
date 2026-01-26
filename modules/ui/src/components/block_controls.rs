//! Block parameter control components.
//!
//! This module provides audio-style controls for various effect blocks:
//! - Amp: Gain, Bass, Mid, Treble
//! - Reverb: Mix, Decay, Size, Damping
//! - Delay: Mix, Time, Feedback, Modulation

use audio_controls::prelude::*;
use dioxus::prelude::*;
use uuid::Uuid;

/// Amp block control parameters.
#[derive(Debug, Clone, PartialEq)]
pub struct AmpParams {
    pub gain: f32,      // 0.0 - 1.0
    pub bass: f32,      // 0.0 - 1.0 (center at 0.5)
    pub mid: f32,       // 0.0 - 1.0 (center at 0.5)
    pub treble: f32,    // 0.0 - 1.0 (center at 0.5)
    pub presence: f32,  // 0.0 - 1.0 (center at 0.5)
    pub master: f32,    // 0.0 - 1.0
}

impl Default for AmpParams {
    fn default() -> Self {
        Self {
            gain: 0.5,
            bass: 0.5,
            mid: 0.5,
            treble: 0.5,
            presence: 0.5,
            master: 0.7,
        }
    }
}

/// Reverb block control parameters.
#[derive(Debug, Clone, PartialEq)]
pub struct ReverbParams {
    pub mix: f32,       // 0.0 - 1.0
    pub decay: f32,     // 0.0 - 1.0 (maps to 0.1s - 10s)
    pub size: f32,      // 0.0 - 1.0
    pub damping: f32,   // 0.0 - 1.0
    pub predelay: f32,  // 0.0 - 1.0 (maps to 0 - 100ms)
}

impl Default for ReverbParams {
    fn default() -> Self {
        Self {
            mix: 0.3,
            decay: 0.5,
            size: 0.6,
            damping: 0.4,
            predelay: 0.1,
        }
    }
}

/// Delay block control parameters.
#[derive(Debug, Clone, PartialEq)]
pub struct DelayParams {
    pub mix: f32,       // 0.0 - 1.0
    pub time: f32,      // 0.0 - 1.0 (maps to tempo-synced or ms)
    pub feedback: f32,  // 0.0 - 1.0
    pub modulation: f32, // 0.0 - 1.0
    pub high_cut: f32,  // 0.0 - 1.0
}

impl Default for DelayParams {
    fn default() -> Self {
        Self {
            mix: 0.25,
            time: 0.5,
            feedback: 0.3,
            modulation: 0.2,
            high_cut: 0.7,
        }
    }
}

/// Drive/Overdrive block control parameters.
#[derive(Debug, Clone, PartialEq)]
pub struct DriveParams {
    pub drive: f32,     // 0.0 - 1.0
    pub tone: f32,      // 0.0 - 1.0
    pub level: f32,     // 0.0 - 1.0
}

impl Default for DriveParams {
    fn default() -> Self {
        Self {
            drive: 0.5,
            tone: 0.5,
            level: 0.7,
        }
    }
}

/// Cabinet/IR block control parameters.
#[derive(Debug, Clone, PartialEq)]
pub struct CabParams {
    pub mix: f32,       // 0.0 - 1.0 (dry/wet)
    pub low_cut: f32,   // 0.0 - 1.0 (maps to 20Hz - 500Hz)
    pub high_cut: f32,  // 0.0 - 1.0 (maps to 2kHz - 20kHz)
    pub mic_distance: f32, // 0.0 - 1.0
}

impl Default for CabParams {
    fn default() -> Self {
        Self {
            mix: 1.0,
            low_cut: 0.2,
            high_cut: 0.8,
            mic_distance: 0.3,
        }
    }
}

/// Amp control panel with gain and EQ knobs.
#[component]
pub fn AmpControls(
    block_id: Uuid,
    params: Signal<AmpParams>,
    on_change: Callback<(Uuid, u32, f32)>,
    #[props(default = false)] disabled: bool,
    #[props(default = false)] compact: bool,
) -> Element {
    let knob_size = if compact { 48 } else { 56 };

    // Create individual signals for each parameter
    let mut gain = use_signal(|| params().gain);
    let mut bass = use_signal(|| params().bass);
    let mut mid = use_signal(|| params().mid);
    let mut treble = use_signal(|| params().treble);
    let mut presence = use_signal(|| params().presence);
    let mut master = use_signal(|| params().master);

    // Sync with external params
    use_effect(move || {
        let p = params();
        gain.set(p.gain);
        bass.set(p.bass);
        mid.set(p.mid);
        treble.set(p.treble);
        presence.set(p.presence);
        master.set(p.master);
    });

    // Format value as percentage
    let format_pct = |v: f32| format!("{:.0}%", v * 100.0);
    // Format EQ value as dB (assuming +/- 12dB range)
    let format_eq = |v: f32| {
        let db = (v - 0.5) * 24.0;
        if db >= 0.0 {
            format!("+{db:.1}")
        } else {
            format!("{db:.1}")
        }
    };

    rsx! {
        ThemeProvider { theme: ThemeContext::new(),
            div { class: "amp-controls",
                // Top row: Gain and Master
                div { class: "flex items-center justify-between gap-4 mb-4",
                    Knob {
                        value: gain,
                        size: knob_size,
                        label: Some("Gain".to_string()),
                        value_display: Some(format_pct(gain())),
                        disabled,
                        on_change: {
                            let on_change = on_change.clone();
                            move |v| on_change.call((block_id, 0, v))
                        },
                    }
                    Knob {
                        value: master,
                        size: knob_size,
                        label: Some("Master".to_string()),
                        value_display: Some(format_pct(master())),
                        disabled,
                        on_change: {
                            let on_change = on_change.clone();
                            move |v| on_change.call((block_id, 5, v))
                        },
                    }
                }
                // EQ row
                div { class: "flex items-center justify-between gap-2",
                    Knob {
                        value: bass,
                        size: knob_size - 8,
                        label: Some("Bass".to_string()),
                        value_display: Some(format_eq(bass())),
                        variant: KnobVariant::ArcBipolar,
                        disabled,
                        on_change: {
                            let on_change = on_change.clone();
                            move |v| on_change.call((block_id, 1, v))
                        },
                    }
                    Knob {
                        value: mid,
                        size: knob_size - 8,
                        label: Some("Mid".to_string()),
                        value_display: Some(format_eq(mid())),
                        variant: KnobVariant::ArcBipolar,
                        disabled,
                        on_change: {
                            let on_change = on_change.clone();
                            move |v| on_change.call((block_id, 2, v))
                        },
                    }
                    Knob {
                        value: treble,
                        size: knob_size - 8,
                        label: Some("Treble".to_string()),
                        value_display: Some(format_eq(treble())),
                        variant: KnobVariant::ArcBipolar,
                        disabled,
                        on_change: {
                            let on_change = on_change.clone();
                            move |v| on_change.call((block_id, 3, v))
                        },
                    }
                    Knob {
                        value: presence,
                        size: knob_size - 8,
                        label: Some("Pres".to_string()),
                        value_display: Some(format_eq(presence())),
                        variant: KnobVariant::ArcBipolar,
                        disabled,
                        on_change: {
                            let on_change = on_change.clone();
                            move |v| on_change.call((block_id, 4, v))
                        },
                    }
                }
            }
        }
    }
}

/// Reverb control panel with mix, decay, size, damping knobs.
#[component]
pub fn ReverbControls(
    block_id: Uuid,
    params: Signal<ReverbParams>,
    on_change: Callback<(Uuid, u32, f32)>,
    #[props(default = false)] disabled: bool,
    #[props(default = false)] compact: bool,
) -> Element {
    let knob_size = if compact { 44 } else { 52 };

    let mut mix = use_signal(|| params().mix);
    let mut decay = use_signal(|| params().decay);
    let mut size = use_signal(|| params().size);
    let mut damping = use_signal(|| params().damping);
    let mut predelay = use_signal(|| params().predelay);

    use_effect(move || {
        let p = params();
        mix.set(p.mix);
        decay.set(p.decay);
        size.set(p.size);
        damping.set(p.damping);
        predelay.set(p.predelay);
    });

    let format_pct = |v: f32| format!("{:.0}%", v * 100.0);
    let format_decay = |v: f32| {
        let seconds = 0.1 + v * 9.9; // 0.1s to 10s
        if seconds < 1.0 {
            format!("{:.0}ms", seconds * 1000.0)
        } else {
            format!("{seconds:.1}s")
        }
    };
    let format_predelay = |v: f32| format!("{:.0}ms", v * 100.0);

    rsx! {
        ThemeProvider { theme: ThemeContext::new(),
            div { class: "reverb-controls",
                div { class: "flex items-center justify-between gap-2",
                    Knob {
                        value: mix,
                        size: knob_size,
                        label: Some("Mix".to_string()),
                        value_display: Some(format_pct(mix())),
                        disabled,
                        on_change: {
                            let on_change = on_change.clone();
                            move |v| on_change.call((block_id, 0, v))
                        },
                    }
                    Knob {
                        value: decay,
                        size: knob_size,
                        label: Some("Decay".to_string()),
                        value_display: Some(format_decay(decay())),
                        disabled,
                        on_change: {
                            let on_change = on_change.clone();
                            move |v| on_change.call((block_id, 1, v))
                        },
                    }
                    Knob {
                        value: size,
                        size: knob_size,
                        label: Some("Size".to_string()),
                        value_display: Some(format_pct(size())),
                        disabled,
                        on_change: {
                            let on_change = on_change.clone();
                            move |v| on_change.call((block_id, 2, v))
                        },
                    }
                    Knob {
                        value: damping,
                        size: knob_size,
                        label: Some("Damp".to_string()),
                        value_display: Some(format_pct(damping())),
                        disabled,
                        on_change: {
                            let on_change = on_change.clone();
                            move |v| on_change.call((block_id, 3, v))
                        },
                    }
                    Knob {
                        value: predelay,
                        size: knob_size,
                        label: Some("Pre".to_string()),
                        value_display: Some(format_predelay(predelay())),
                        disabled,
                        on_change: {
                            let on_change = on_change.clone();
                            move |v| on_change.call((block_id, 4, v))
                        },
                    }
                }
            }
        }
    }
}

/// Drive/Overdrive control panel with drive, tone, level knobs.
#[component]
pub fn DriveControls(
    block_id: Uuid,
    params: Signal<DriveParams>,
    on_change: Callback<(Uuid, u32, f32)>,
    #[props(default = false)] disabled: bool,
    #[props(default = false)] compact: bool,
) -> Element {
    let knob_size = if compact { 48 } else { 56 };

    let mut drive = use_signal(|| params().drive);
    let mut tone = use_signal(|| params().tone);
    let mut level = use_signal(|| params().level);

    use_effect(move || {
        let p = params();
        drive.set(p.drive);
        tone.set(p.tone);
        level.set(p.level);
    });

    let format_pct = |v: f32| format!("{:.0}%", v * 100.0);

    rsx! {
        ThemeProvider { theme: ThemeContext::new(),
            div { class: "drive-controls",
                div { class: "flex items-center justify-center gap-6",
                    Knob {
                        value: drive,
                        size: knob_size,
                        label: Some("Drive".to_string()),
                        value_display: Some(format_pct(drive())),
                        disabled,
                        on_change: {
                            let on_change = on_change.clone();
                            move |v| on_change.call((block_id, 0, v))
                        },
                    }
                    Knob {
                        value: tone,
                        size: knob_size,
                        label: Some("Tone".to_string()),
                        value_display: Some(format_pct(tone())),
                        disabled,
                        on_change: {
                            let on_change = on_change.clone();
                            move |v| on_change.call((block_id, 1, v))
                        },
                    }
                    Knob {
                        value: level,
                        size: knob_size,
                        label: Some("Level".to_string()),
                        value_display: Some(format_pct(level())),
                        disabled,
                        on_change: {
                            let on_change = on_change.clone();
                            move |v| on_change.call((block_id, 2, v))
                        },
                    }
                }
            }
        }
    }
}

/// Cabinet/IR control panel with mix, low cut, high cut, mic distance.
#[component]
pub fn CabControls(
    block_id: Uuid,
    params: Signal<CabParams>,
    on_change: Callback<(Uuid, u32, f32)>,
    #[props(default = false)] disabled: bool,
    #[props(default = false)] compact: bool,
) -> Element {
    let knob_size = if compact { 44 } else { 52 };

    let mut mix = use_signal(|| params().mix);
    let mut low_cut = use_signal(|| params().low_cut);
    let mut high_cut = use_signal(|| params().high_cut);
    let mut mic_distance = use_signal(|| params().mic_distance);

    use_effect(move || {
        let p = params();
        mix.set(p.mix);
        low_cut.set(p.low_cut);
        high_cut.set(p.high_cut);
        mic_distance.set(p.mic_distance);
    });

    let format_pct = |v: f32| format!("{:.0}%", v * 100.0);
    let format_lowcut = |v: f32| {
        let freq = 20.0 + v * 480.0; // 20Hz to 500Hz
        format!("{freq:.0}Hz")
    };
    let format_highcut = |v: f32| {
        let freq = 2000.0 + v * 18000.0; // 2kHz to 20kHz
        format!("{:.1}k", freq / 1000.0)
    };

    rsx! {
        ThemeProvider { theme: ThemeContext::new(),
            div { class: "cab-controls",
                div { class: "flex items-center justify-between gap-2",
                    Knob {
                        value: mix,
                        size: knob_size,
                        label: Some("Mix".to_string()),
                        value_display: Some(format_pct(mix())),
                        disabled,
                        on_change: {
                            let on_change = on_change.clone();
                            move |v| on_change.call((block_id, 0, v))
                        },
                    }
                    Knob {
                        value: low_cut,
                        size: knob_size,
                        label: Some("LoCut".to_string()),
                        value_display: Some(format_lowcut(low_cut())),
                        disabled,
                        on_change: {
                            let on_change = on_change.clone();
                            move |v| on_change.call((block_id, 1, v))
                        },
                    }
                    Knob {
                        value: high_cut,
                        size: knob_size,
                        label: Some("HiCut".to_string()),
                        value_display: Some(format_highcut(high_cut())),
                        disabled,
                        on_change: {
                            let on_change = on_change.clone();
                            move |v| on_change.call((block_id, 2, v))
                        },
                    }
                    Knob {
                        value: mic_distance,
                        size: knob_size,
                        label: Some("Mic".to_string()),
                        value_display: Some(format_pct(mic_distance())),
                        disabled,
                        on_change: {
                            let on_change = on_change.clone();
                            move |v| on_change.call((block_id, 3, v))
                        },
                    }
                }
            }
        }
    }
}

/// Delay control panel with mix, time, feedback, modulation knobs.
#[component]
pub fn DelayControls(
    block_id: Uuid,
    params: Signal<DelayParams>,
    on_change: Callback<(Uuid, u32, f32)>,
    #[props(default = false)] disabled: bool,
    #[props(default = false)] compact: bool,
) -> Element {
    let knob_size = if compact { 44 } else { 52 };

    let mut mix = use_signal(|| params().mix);
    let mut time = use_signal(|| params().time);
    let mut feedback = use_signal(|| params().feedback);
    let mut modulation = use_signal(|| params().modulation);
    let mut high_cut = use_signal(|| params().high_cut);

    use_effect(move || {
        let p = params();
        mix.set(p.mix);
        time.set(p.time);
        feedback.set(p.feedback);
        modulation.set(p.modulation);
        high_cut.set(p.high_cut);
    });

    let format_pct = |v: f32| format!("{:.0}%", v * 100.0);
    let format_time = |v: f32| {
        // Map 0-1 to tempo divisions: 1/32, 1/16, 1/8, 1/4, 1/2, etc.
        let divisions = ["1/32", "1/16T", "1/16", "1/8T", "1/8", "1/4T", "1/4", "1/2", "1/1"];
        let idx = ((v * (divisions.len() - 1) as f32).round() as usize).min(divisions.len() - 1);
        divisions[idx].to_string()
    };
    let format_highcut = |v: f32| {
        let freq = 1000.0 + v * 19000.0; // 1kHz to 20kHz
        if freq >= 1000.0 {
            format!("{:.1}k", freq / 1000.0)
        } else {
            format!("{freq:.0}")
        }
    };

    rsx! {
        ThemeProvider { theme: ThemeContext::new(),
            div { class: "delay-controls",
                div { class: "flex items-center justify-between gap-2",
                    Knob {
                        value: mix,
                        size: knob_size,
                        label: Some("Mix".to_string()),
                        value_display: Some(format_pct(mix())),
                        disabled,
                        on_change: {
                            let on_change = on_change.clone();
                            move |v| on_change.call((block_id, 0, v))
                        },
                    }
                    Knob {
                        value: time,
                        size: knob_size,
                        label: Some("Time".to_string()),
                        value_display: Some(format_time(time())),
                        disabled,
                        on_change: {
                            let on_change = on_change.clone();
                            move |v| on_change.call((block_id, 1, v))
                        },
                    }
                    Knob {
                        value: feedback,
                        size: knob_size,
                        label: Some("Fdbk".to_string()),
                        value_display: Some(format_pct(feedback())),
                        disabled,
                        on_change: {
                            let on_change = on_change.clone();
                            move |v| on_change.call((block_id, 2, v))
                        },
                    }
                    Knob {
                        value: modulation,
                        size: knob_size,
                        label: Some("Mod".to_string()),
                        value_display: Some(format_pct(modulation())),
                        disabled,
                        on_change: {
                            let on_change = on_change.clone();
                            move |v| on_change.call((block_id, 3, v))
                        },
                    }
                    Knob {
                        value: high_cut,
                        size: knob_size,
                        label: Some("HiCut".to_string()),
                        value_display: Some(format_highcut(high_cut())),
                        disabled,
                        on_change: {
                            let on_change = on_change.clone();
                            move |v| on_change.call((block_id, 4, v))
                        },
                    }
                }
            }
        }
    }
}

/// Generic block with expandable parameter controls.
#[component]
pub fn ExpandableBlockControls(
    block_id: Uuid,
    block_name: String,
    block_type: BlockType,
    enabled: bool,
    on_toggle_bypass: Callback<Uuid>,
    on_param_change: Callback<(Uuid, u32, f32)>,
    #[props(default = true)] initially_expanded: bool,
) -> Element {
    let mut expanded = use_signal(|| initially_expanded);

    // Create default params based on block type
    let amp_params = use_signal(AmpParams::default);
    let drive_params = use_signal(DriveParams::default);
    let cab_params = use_signal(CabParams::default);
    let reverb_params = use_signal(ReverbParams::default);
    let delay_params = use_signal(DelayParams::default);

    let enabled_class = if enabled {
        "border-primary bg-card"
    } else {
        "border-border bg-muted/50 opacity-60"
    };

    rsx! {
        div { class: "border rounded-lg overflow-hidden transition-all {enabled_class}",
            // Header row with bypass toggle
            div {
                class: "flex items-center justify-between p-3 cursor-pointer hover:bg-muted/20",
                onclick: move |_| expanded.toggle(),
                div { class: "flex items-center gap-3",
                    // Bypass button
                    button {
                        class: "w-8 h-8 rounded-full flex items-center justify-center transition-colors",
                        class: if enabled { "bg-green-500 text-white" } else { "bg-muted text-muted-foreground" },
                        onclick: move |e| {
                            e.stop_propagation();
                            on_toggle_bypass.call(block_id);
                        },
                        if enabled { "ON" } else { "OFF" }
                    }
                    span { class: "font-medium", "{block_name}" }
                }
                // Expand indicator
                span { class: "text-muted-foreground text-sm",
                    if expanded() { "▼" } else { "▶" }
                }
            }

            // Expanded parameter controls
            if expanded() {
                div { class: "p-4 border-t border-border bg-card/50",
                    match block_type {
                        BlockType::Drive => rsx! {
                            DriveControls {
                                block_id,
                                params: drive_params,
                                on_change: on_param_change.clone(),
                                disabled: !enabled,
                                compact: true,
                            }
                        },
                        BlockType::Amp => rsx! {
                            AmpControls {
                                block_id,
                                params: amp_params,
                                on_change: on_param_change.clone(),
                                disabled: !enabled,
                                compact: true,
                            }
                        },
                        BlockType::Cab => rsx! {
                            CabControls {
                                block_id,
                                params: cab_params,
                                on_change: on_param_change.clone(),
                                disabled: !enabled,
                                compact: true,
                            }
                        },
                        BlockType::Delay => rsx! {
                            DelayControls {
                                block_id,
                                params: delay_params,
                                on_change: on_param_change.clone(),
                                disabled: !enabled,
                                compact: true,
                            }
                        },
                        BlockType::Reverb => rsx! {
                            ReverbControls {
                                block_id,
                                params: reverb_params,
                                on_change: on_param_change.clone(),
                                disabled: !enabled,
                                compact: true,
                            }
                        },
                    }
                }
            }
        }
    }
}

/// Block type for determining which controls to show.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlockType {
    Drive,
    Amp,
    Cab,
    Delay,
    Reverb,
}

impl BlockType {
    /// Infer block type from name.
    #[must_use]
    pub fn from_name(name: &str) -> Self {
        let lower = name.to_lowercase();
        if lower.contains("reverb") || lower.contains("verb") || lower.contains("room") || lower.contains("hall") {
            Self::Reverb
        } else if lower.contains("delay") || lower.contains("echo") {
            Self::Delay
        } else if lower.contains("cab") || lower.contains("ir") || lower.contains("speaker") {
            Self::Cab
        } else if lower.contains("drive") || lower.contains("overdrive") || lower.contains("distortion")
            || lower.contains("fuzz") || lower.contains("boost") || lower.contains("screamer") {
            Self::Drive
        } else {
            // Default to Amp for anything else
            Self::Amp
        }
    }
}
