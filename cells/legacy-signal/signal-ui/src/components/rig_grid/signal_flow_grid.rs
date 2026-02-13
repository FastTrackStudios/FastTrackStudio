//! Signal flow grid view — renders blocks on a CSS grid with routing.
//!
//! This component takes a `SignalFlowGrid` (from grid_model) and renders it as
//! a CSS Grid layout with:
//! - Blocks positioned via `grid-column` / `grid-row` spanning
//! - Color-coded blocks based on `BlockType`
//! - Specialized audio visualizations (EQ, Compressor, Gate) for larger blocks
//! - I/O jacks on left/right edges
//! - Routing connection lines between blocks

use crate::prelude::*;
use uuid::Uuid;

use super::block_colors::{block_type_bypassed_style, block_type_style};
use super::grid_model::{BlockWidget, GridBlock, GridJack, SignalFlowGrid, GRID_COLS, GRID_ROWS};

use audio_controls::widgets::{
    CompressorGraph, CompressorParams, EqBand, EqBandShape, EqGraph, GateGraph, GateParams, Knob,
};

/// Cell size in pixels for the grid.
const CELL_SIZE: u32 = 64;
/// Gap between cells in pixels.
const CELL_GAP: u32 = 4;

/// Props for the signal flow grid view.
#[derive(Props, Clone, PartialEq)]
pub struct SignalFlowGridViewProps {
    /// The grid data to render.
    pub grid: SignalFlowGrid,
    /// Callback when a block is clicked.
    #[props(default)]
    pub on_block_click: Option<Callback<Uuid>>,
    /// Callback when a block's bypass is toggled.
    #[props(default)]
    pub on_block_bypass: Option<Callback<Uuid>>,
}

/// Signal flow grid view — Quad Cortex / Helix style block grid.
#[component]
pub fn SignalFlowGridView(props: SignalFlowGridViewProps) -> Element {
    let grid = &props.grid;

    // Calculate total grid dimensions
    let total_width = GRID_COLS as u32 * (CELL_SIZE + CELL_GAP) + CELL_GAP;
    let total_height = GRID_ROWS as u32 * (CELL_SIZE + CELL_GAP) + CELL_GAP;

    // Build CSS grid template
    let col_template = format!("repeat({GRID_COLS}, {CELL_SIZE}px)");
    let row_template = format!("repeat({GRID_ROWS}, {CELL_SIZE}px)");

    rsx! {
        div { class: "relative w-full overflow-auto",
            // Grid container
            div {
                class: "relative mx-auto",
                style: "width: {total_width}px; min-height: {total_height}px;",

                // I/O jacks — left side (inputs)
                div {
                    class: "absolute left-0 top-0 bottom-0 w-8 flex flex-col justify-start gap-2 pt-2",
                    for jack in &grid.inputs {
                        JackLabel { jack: jack.clone() }
                    }
                }

                // I/O jacks — right side (outputs)
                div {
                    class: "absolute right-0 top-0 bottom-0 w-8 flex flex-col justify-start gap-2 pt-2",
                    for jack in &grid.outputs {
                        JackLabel { jack: jack.clone() }
                    }
                }

                // CSS Grid layout for blocks
                div {
                    class: "inline-grid",
                    style: "grid-template-columns: {col_template}; \
                            grid-template-rows: {row_template}; \
                            gap: {CELL_GAP}px; \
                            padding: {CELL_GAP}px;",

                    for block in &grid.blocks {
                        GridBlockCell {
                            block: block.clone(),
                            on_click: props.on_block_click.clone(),
                            on_bypass: props.on_block_bypass.clone(),
                        }
                    }
                }
            }
        }
    }
}

// ── Jack label ──────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct JackLabelProps {
    jack: GridJack,
}

#[component]
fn JackLabel(props: JackLabelProps) -> Element {
    let jack = &props.jack;
    let row_offset = jack.row as u32 * (CELL_SIZE + CELL_GAP) + CELL_GAP;
    let side_class = if jack.is_input { "left-0" } else { "right-0" };

    rsx! {
        div {
            class: "absolute {side_class} flex items-center",
            style: "top: {row_offset}px; height: {CELL_SIZE}px;",
            div {
                class: "px-1 py-0.5 text-[10px] font-mono text-zinc-400 \
                        bg-zinc-800/80 rounded whitespace-nowrap",
                "{jack.label}"
            }
        }
    }
}

// ── Grid block cell ─────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct GridBlockCellProps {
    block: GridBlock,
    #[props(default)]
    on_click: Option<Callback<Uuid>>,
    #[props(default)]
    on_bypass: Option<Callback<Uuid>>,
}

/// A single block rendered inside the CSS grid.
#[component]
fn GridBlockCell(props: GridBlockCellProps) -> Element {
    let block = &props.block;

    // CSS grid placement (1-indexed)
    let col_start = block.position.col + 1;
    let col_end = col_start + block.size.width;
    let row_start = block.position.row + 1;
    let row_end = row_start + block.size.height;

    let style_str = if block.bypassed {
        block_type_bypassed_style(block.block_type)
    } else {
        block_type_style(block.block_type)
    };

    let block_id = block.id;
    let on_click = props.on_click.clone();
    let on_bypass = props.on_bypass.clone();

    // Pixel dimensions for the block content area
    let px_w = block.size.width as u32 * CELL_SIZE
        + (block.size.width as u32).saturating_sub(1) * CELL_GAP;
    let px_h = block.size.height as u32 * CELL_SIZE
        + (block.size.height as u32).saturating_sub(1) * CELL_GAP;

    rsx! {
        div {
            class: "relative rounded-lg border-2 overflow-hidden cursor-pointer \
                    hover:brightness-110 transition-all duration-150 \
                    flex flex-col",
            style: "grid-column: {col_start} / {col_end}; \
                    grid-row: {row_start} / {row_end}; \
                    {style_str}",
            onclick: move |_| {
                if let Some(cb) = &on_click {
                    cb.call(block_id);
                }
            },

            // Block header (always visible)
            BlockHeader {
                name: block.name.clone(),
                short_label: block.short_label.clone(),
                bypassed: block.bypassed,
                block_id,
                on_bypass: on_bypass.clone(),
                compact: block.size.width == 1 && block.size.height == 1,
            }

            // Block content — widget or label
            div { class: "flex-1 min-h-0 overflow-hidden",
                BlockContent {
                    widget: block.widget,
                    width: px_w,
                    height: px_h.saturating_sub(24), // subtract header height
                    bypassed: block.bypassed,
                }
            }
        }
    }
}

// ── Block header ────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct BlockHeaderProps {
    name: String,
    short_label: String,
    bypassed: bool,
    block_id: Uuid,
    #[props(default)]
    on_bypass: Option<Callback<Uuid>>,
    #[props(default)]
    compact: bool,
}

#[component]
fn BlockHeader(props: BlockHeaderProps) -> Element {
    let block_id = props.block_id;
    let on_bypass = props.on_bypass.clone();

    if props.compact {
        // 1x1 block: just show the short label centered
        rsx! {
            div {
                class: "flex items-center justify-center h-full text-xs font-bold \
                        select-none",
                style: if props.bypassed { "opacity: 0.5;" } else { "" },
                "{props.short_label}"
            }
        }
    } else {
        // Multi-cell block: name + bypass dot
        rsx! {
            div {
                class: "flex items-center justify-between px-2 py-1 \
                        text-[11px] font-semibold select-none",
                span {
                    style: if props.bypassed { "opacity: 0.5;" } else { "" },
                    "{props.name}"
                }
                // Bypass indicator dot
                button {
                    class: "w-2.5 h-2.5 rounded-full border border-current/30 \
                            hover:scale-125 transition-transform",
                    style: if props.bypassed {
                        "background: transparent; opacity: 0.4;"
                    } else {
                        "background: currentColor;"
                    },
                    title: if props.bypassed { "Enable" } else { "Bypass" },
                    onclick: move |e| {
                        e.stop_propagation();
                        if let Some(cb) = &on_bypass {
                            cb.call(block_id);
                        }
                    },
                }
            }
        }
    }
}

// ── Block content (widget routing) ──────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub(crate) struct BlockContentProps {
    pub widget: BlockWidget,
    pub width: u32,
    pub height: u32,
    pub bypassed: bool,
}

/// Routes to the appropriate visualization widget based on the `BlockWidget` enum.
#[component]
pub(crate) fn BlockContent(props: BlockContentProps) -> Element {
    if props.bypassed {
        return rsx! {
            div { class: "flex items-center justify-center h-full text-xs opacity-30",
                "BYPASSED"
            }
        };
    }

    // Minimum size thresholds for rendering widgets
    let has_room = props.width >= 100 && props.height >= 60;

    match props.widget {
        BlockWidget::EqGraph if has_room => rsx! { EqGraphBlock {} },
        BlockWidget::CompressorGraph if has_room => rsx! { CompressorGraphBlock {} },
        BlockWidget::GateGraph if has_room => rsx! { GateGraphBlock {} },
        BlockWidget::AmpCab if has_room => rsx! { AmpCabBlock {} },
        BlockWidget::DelayGraph if has_room => rsx! { TimeEffectBlock { label: "DLY" } },
        BlockWidget::ReverbGraph if has_room => rsx! { TimeEffectBlock { label: "REV" } },
        BlockWidget::ModulationGraph if has_room => rsx! { ModulationBlock {} },
        BlockWidget::DriveGraph if has_room => rsx! { DriveBlock {} },
        BlockWidget::Tuner => rsx! { TunerBlock {} },
        BlockWidget::Looper => rsx! { LooperBlock {} },
        // Label or too-small widget: show nothing extra (header suffices)
        _ => rsx! {},
    }
}

// ── Specialized widget blocks ───────────────────────────────────────

/// EQ visualization using the audio-controls EqGraph.
#[component]
pub(crate) fn EqGraphBlock() -> Element {
    // Create sample EQ bands for display
    let bands = use_signal(|| {
        vec![
            EqBand {
                index: 0,
                used: true,
                enabled: true,
                frequency: 100.0,
                gain: 3.0,
                q: 0.7,
                shape: EqBandShape::LowShelf,
                ..Default::default()
            },
            EqBand {
                index: 1,
                used: true,
                enabled: true,
                frequency: 800.0,
                gain: -2.0,
                q: 1.4,
                shape: EqBandShape::Bell,
                ..Default::default()
            },
            EqBand {
                index: 2,
                used: true,
                enabled: true,
                frequency: 3500.0,
                gain: 4.0,
                q: 1.0,
                shape: EqBandShape::Bell,
                ..Default::default()
            },
            EqBand {
                index: 3,
                used: true,
                enabled: true,
                frequency: 10000.0,
                gain: -1.5,
                q: 0.7,
                shape: EqBandShape::HighShelf,
                ..Default::default()
            },
        ]
    });

    rsx! {
        div {
            class: "p-1 h-full",
            EqGraph {
                bands,
                show_freq_labels: false,
                show_db_labels: false,
            }
        }
    }
}

/// Compressor visualization using the audio-controls CompressorGraph.
#[component]
pub(crate) fn CompressorGraphBlock() -> Element {
    let params = CompressorParams::default();

    rsx! {
        div {
            class: "flex items-center justify-center h-full p-1",
            CompressorGraph {
                params,
                show_grid: false,
                show_gr_meter: false,
                show_levels: false,
            }
        }
    }
}

/// Gate visualization using the audio-controls GateGraph.
#[component]
pub(crate) fn GateGraphBlock() -> Element {
    let params = use_signal(GateParams::default);

    rsx! {
        div {
            class: "flex items-center justify-center h-full p-1",
            GateGraph {
                params,
                show_grid: false,
                show_gr_meter: false,
            }
        }
    }
}

/// Amp/Cab block — stylized amp icon with knobs.
#[component]
pub(crate) fn AmpCabBlock() -> Element {
    let mut gain = use_signal(|| 0.5f32);
    let mut tone = use_signal(|| 0.5f32);

    rsx! {
        div {
            class: "flex flex-col items-center justify-center h-full gap-1 p-1",
            // Two mini knobs
            div { class: "flex gap-2",
                Knob { value: gain, size: 32, label: Some("Gain".to_string()),
                    on_change: move |v| gain.set(v),
                }
                Knob { value: tone, size: 32, label: Some("Tone".to_string()),
                    on_change: move |v| tone.set(v),
                }
            }
        }
    }
}

/// Drive block — shows a drive curve hint and knob.
#[component]
pub(crate) fn DriveBlock() -> Element {
    let mut drive = use_signal(|| 0.4f32);

    rsx! {
        div {
            class: "flex flex-col items-center justify-center h-full gap-1 p-1",
            // Drive knob
            Knob { value: drive, size: 40, label: Some("Drive".to_string()),
                on_change: move |v| drive.set(v),
            }
        }
    }
}

/// Modulation block — shows rate visualization.
#[component]
pub(crate) fn ModulationBlock() -> Element {
    let mut rate = use_signal(|| 0.3f32);
    let mut depth = use_signal(|| 0.5f32);

    rsx! {
        div {
            class: "flex items-center justify-center h-full gap-2 p-1",
            Knob { value: rate, size: 28, label: Some("Rate".to_string()),
                on_change: move |v| rate.set(v),
            }
            Knob { value: depth, size: 28, label: Some("Depth".to_string()),
                on_change: move |v| depth.set(v),
            }
        }
    }
}

/// Time-based effect (Delay/Reverb) — label + time/mix knobs.
#[component]
pub(crate) fn TimeEffectBlock(label: &'static str) -> Element {
    let mut time = use_signal(|| 0.3f32);
    let mut mix = use_signal(|| 0.3f32);

    rsx! {
        div {
            class: "flex items-center justify-center h-full gap-2 p-1",
            Knob { value: time, size: 28, label: Some("Time".to_string()),
                on_change: move |v| time.set(v),
            }
            Knob { value: mix, size: 28, label: Some("Mix".to_string()),
                on_change: move |v| mix.set(v),
            }
        }
    }
}

/// Tuner block — simple display.
#[component]
pub(crate) fn TunerBlock() -> Element {
    rsx! {
        div { class: "flex items-center justify-center h-full text-[10px] font-mono",
            "A 440"
        }
    }
}

/// Looper block — simple controls.
#[component]
pub(crate) fn LooperBlock() -> Element {
    rsx! {
        div { class: "flex items-center justify-center h-full gap-1 text-[10px]",
            button { class: "px-1 py-0.5 rounded bg-black/20 hover:bg-black/30", "REC" }
            button { class: "px-1 py-0.5 rounded bg-black/20 hover:bg-black/30", "PLAY" }
        }
    }
}
