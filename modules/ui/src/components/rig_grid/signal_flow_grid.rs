//! Signal flow grid UI component.
//!
//! Renders a 16x8 grid with blocks that can span multiple cells,
//! containing actual widget visualizations (EQ, compressor, etc.).

use dioxus::prelude::*;
use uuid::Uuid;

use super::block_colors::block_type_color;
use super::grid_model::{BlockWidget, GridBlock, GridConnection, GridJack, GridPosition, SignalFlowGrid, GRID_COLS, GRID_ROWS};

// Import audio-controls widgets
use audio_controls::widgets::{EqBand, EqBandShape, EqGraph, CompressorGraph, CompressorParams, CompressorMode, DbRange};

/// Props for the signal flow grid view.
#[derive(Props, Clone, PartialEq)]
pub struct SignalFlowGridViewProps {
    /// The grid data to render.
    pub grid: SignalFlowGrid,
    /// Callback when a block is clicked.
    #[props(default)]
    pub on_block_click: Option<Callback<Uuid>>,
    /// Callback when a block bypass is toggled.
    #[props(default)]
    pub on_toggle_bypass: Option<Callback<Uuid>>,
    /// Callback when an empty cell is clicked.
    #[props(default)]
    pub on_cell_click: Option<Callback<GridPosition>>,
}

/// Signal flow grid component - renders the full-screen 16x8 grid.
#[component]
pub fn SignalFlowGridView(props: SignalFlowGridViewProps) -> Element {
    // Grid fills the container, cells are calculated dynamically
    rsx! {
        div {
            class: "relative w-full h-full bg-zinc-900 rounded-lg overflow-hidden",

            // CSS Grid container for the 16x8 layout
            div {
                class: "absolute inset-4 grid gap-1",
                style: "grid-template-columns: 40px repeat({GRID_COLS}, 1fr) 40px; \
                        grid-template-rows: repeat({GRID_ROWS}, 1fr);",

                // Input jacks column (column 0)
                for row in 0..GRID_ROWS {
                    {
                        let jack = props.grid.inputs.iter().find(|j| j.row == row);
                        rsx! {
                            div {
                                key: "input-{row}",
                                class: "flex items-center justify-end pr-2",
                                style: "grid-column: 1; grid-row: {row + 1};",
                                if let Some(j) = jack {
                                    JackIndicator { jack: j.clone() }
                                }
                            }
                        }
                    }
                }

                // Grid cells and blocks
                // First render empty cell backgrounds
                for row in 0..GRID_ROWS {
                    for col in 0..GRID_COLS {
                        {
                            let pos = GridPosition::new(row, col);
                            // Only show background if no block occupies this cell
                            let has_block = props.grid.has_block_at(pos);
                            // Check if this is the top-left of a block
                            let is_block_origin = props.grid.blocks.iter()
                                .any(|b| b.position == pos);

                            if !has_block {
                                rsx! {
                                    div {
                                        key: "cell-{row}-{col}",
                                        class: "flex items-center justify-center",
                                        style: "grid-column: {col + 2}; grid-row: {row + 1};",
                                        // Subtle dot for empty cells
                                        div { class: "w-1.5 h-1.5 rounded-full bg-zinc-700/40" }
                                    }
                                }
                            } else if !is_block_origin {
                                // Cell is part of a multi-cell block but not the origin
                                // Don't render anything here
                                rsx! {}
                            } else {
                                rsx! {}
                            }
                        }
                    }
                }

                // Render blocks (only at their origin position)
                for block in &props.grid.blocks {
                    BlockCell {
                        key: "{block.id}",
                        block: block.clone(),
                        on_click: props.on_block_click.clone(),
                        on_toggle_bypass: props.on_toggle_bypass.clone(),
                    }
                }

                // Output jacks column (last column)
                for row in 0..GRID_ROWS {
                    {
                        let jack = props.grid.outputs.iter().find(|j| j.row == row);
                        rsx! {
                            div {
                                key: "output-{row}",
                                class: "flex items-center justify-start pl-2",
                                style: "grid-column: {GRID_COLS + 2}; grid-row: {row + 1};",
                                if let Some(j) = jack {
                                    JackIndicator { jack: j.clone() }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Props for block cell.
#[derive(Props, Clone, PartialEq)]
struct BlockCellProps {
    block: GridBlock,
    on_click: Option<Callback<Uuid>>,
    on_toggle_bypass: Option<Callback<Uuid>>,
}

/// A block cell that can span multiple grid cells and contain a widget.
#[component]
fn BlockCell(props: BlockCellProps) -> Element {
    let block = &props.block;
    let color = block_type_color(block.block_type);

    // Grid position (CSS grid is 1-indexed, plus 1 for jack column)
    let col_start = block.position.col + 2;
    let col_end = col_start + block.size.width;
    let row_start = block.position.row + 1;
    let row_end = row_start + block.size.height;

    let block_id = block.id;
    let on_click = props.on_click.clone();
    let on_toggle_bypass = props.on_toggle_bypass.clone();

    // Style based on bypass state
    let (bg_opacity, border_opacity) = if block.bypassed {
        ("30", "40")
    } else {
        ("", "")
    };

    let bg_color = if block.bypassed {
        format!("{}30", color.bg)
    } else {
        color.bg.to_string()
    };

    let border_color = if block.bypassed {
        format!("{}60", color.border)
    } else {
        color.border.to_string()
    };

    rsx! {
        div {
            class: "relative rounded-lg border-2 overflow-hidden cursor-pointer \
                    transition-all hover:scale-[1.02] hover:shadow-lg active:scale-[0.98]",
            style: "grid-column: {col_start} / {col_end}; \
                    grid-row: {row_start} / {row_end}; \
                    background-color: {bg_color}; \
                    border-color: {border_color};",
            onclick: move |e| {
                e.stop_propagation();
                if let Some(cb) = on_click.clone() {
                    cb.call(block_id);
                }
            },
            oncontextmenu: move |e| {
                e.prevent_default();
                e.stop_propagation();
                if let Some(cb) = on_toggle_bypass.clone() {
                    cb.call(block_id);
                }
            },
            title: "{block.name}",

            // Block content based on widget type
            BlockContent {
                block: block.clone(),
            }

            // Bypass indicator
            if block.bypassed {
                div {
                    class: "absolute top-1 right-1 w-3 h-3 rounded-full bg-red-500 border border-zinc-900",
                }
            }

            // Label overlay for small blocks or when showing label
            if block.size.width == 1 && block.size.height == 1 {
                div {
                    class: "absolute inset-0 flex items-center justify-center",
                    span {
                        class: "text-xs font-bold",
                        style: "color: {color.fg};",
                        "{block.short_label}"
                    }
                }
            } else {
                // Name label at bottom for larger blocks
                div {
                    class: "absolute bottom-0 left-0 right-0 px-2 py-1 bg-black/50",
                    span {
                        class: "text-xs font-medium text-white truncate",
                        "{block.name}"
                    }
                }
            }
        }
    }
}

/// Props for block content.
#[derive(Props, Clone, PartialEq)]
struct BlockContentProps {
    block: GridBlock,
}

/// Renders the appropriate widget inside a block.
#[component]
fn BlockContent(props: BlockContentProps) -> Element {
    let block = &props.block;

    match block.widget {
        BlockWidget::Label => {
            // Simple label - handled by parent
            rsx! {}
        }
        BlockWidget::EqGraph => {
            // EQ graph with interactive bands
            let mut bands = use_signal(|| vec![
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
                    q: 1.5,
                    shape: EqBandShape::Bell,
                    ..Default::default()
                },
                EqBand {
                    index: 2,
                    used: true,
                    enabled: true,
                    frequency: 4000.0,
                    gain: 2.5,
                    q: 1.0,
                    shape: EqBandShape::HighShelf,
                    ..Default::default()
                },
            ]);

            rsx! {
                div {
                    class: "w-full h-full p-1",
                    onclick: |e| e.stop_propagation(), // Prevent block click when interacting with EQ
                    EqGraph {
                        bands: bands,
                        db_range: 12.0,
                        show_grid: true,
                        fill_curve: true,
                        disabled: false, // Enable interaction!
                        on_band_change: move |(idx, band): (usize, EqBand)| {
                            tracing::debug!("EQ band {} changed: freq={:.0} Hz, gain={:.1} dB", idx, band.frequency, band.gain);
                            bands.write()[idx] = band;
                        },
                        on_band_add: move |band: EqBand| {
                            tracing::debug!("EQ band added: freq={:.0} Hz, gain={:.1} dB", band.frequency, band.gain);
                            let mut all_bands = bands();
                            all_bands.push(band);
                            bands.set(all_bands);
                        },
                        on_band_remove: move |idx: usize| {
                            tracing::debug!("EQ band {} removed", idx);
                            let mut all_bands = bands();
                            all_bands.remove(idx);
                            bands.set(all_bands);
                        },
                    }
                }
            }
        }
        BlockWidget::CompressorGraph => {
            // Compressor graph with default parameters
            let params = CompressorParams {
                threshold: -20.0,
                ratio: 4.0,
                attack: 10.0,
                release: 100.0,
                knee: 6.0,
                makeup: 0.0,
                ..Default::default()
            };

            rsx! {
                div { class: "w-full h-full p-1",
                    CompressorGraph {
                        params: params,
                        db_range: DbRange::default(),
                        show_grid: false,
                    }
                }
            }
        }
        BlockWidget::GateGraph => {
            rsx! {
                div { class: "w-full h-full flex items-center justify-center",
                    // Simplified gate visualization
                    svg {
                        class: "w-full h-full",
                        view_box: "0 0 100 60",
                        preserve_aspect_ratio: "xMidYMid meet",
                        // Gate curve
                        path {
                            d: "M 10 50 L 30 50 L 40 10 L 90 10",
                            fill: "none",
                            stroke: "#3B82F6",
                            stroke_width: "2",
                        }
                    }
                }
            }
        }
        BlockWidget::DelayGraph => {
            rsx! {
                div { class: "w-full h-full flex items-center justify-center p-2",
                    // Delay taps visualization
                    svg {
                        class: "w-full h-full",
                        view_box: "0 0 100 40",
                        preserve_aspect_ratio: "xMidYMid meet",
                        // Original signal
                        rect { x: "5", y: "5", width: "8", height: "30", fill: "#06B6D4" }
                        // Delay taps (decreasing)
                        rect { x: "25", y: "10", width: "8", height: "25", fill: "#06B6D480" }
                        rect { x: "45", y: "15", width: "8", height: "20", fill: "#06B6D460" }
                        rect { x: "65", y: "20", width: "8", height: "15", fill: "#06B6D440" }
                        rect { x: "85", y: "25", width: "8", height: "10", fill: "#06B6D420" }
                    }
                }
            }
        }
        BlockWidget::ReverbGraph => {
            rsx! {
                div { class: "w-full h-full flex items-center justify-center p-2",
                    // Reverb decay visualization
                    svg {
                        class: "w-full h-full",
                        view_box: "0 0 100 40",
                        preserve_aspect_ratio: "xMidYMid meet",
                        // Decay curve
                        path {
                            d: "M 5 35 Q 20 5, 50 20 T 95 35",
                            fill: "none",
                            stroke: "#0EA5E9",
                            stroke_width: "2",
                        }
                        // Fill
                        path {
                            d: "M 5 35 Q 20 5, 50 20 T 95 35 L 95 40 L 5 40 Z",
                            fill: "#0EA5E920",
                        }
                    }
                }
            }
        }
        BlockWidget::AmpCab => {
            rsx! {
                div { class: "w-full h-full flex items-center justify-center",
                    // Amp/cab icon
                    svg {
                        class: "w-3/4 h-3/4",
                        view_box: "0 0 48 48",
                        // Cabinet outline
                        rect {
                            x: "4", y: "8", width: "40", height: "32",
                            rx: "4",
                            fill: "none",
                            stroke: "#EAB308",
                            stroke_width: "2",
                        }
                        // Speaker cone
                        circle {
                            cx: "24", cy: "24", r: "12",
                            fill: "none",
                            stroke: "#EAB30880",
                            stroke_width: "2",
                        }
                        circle {
                            cx: "24", cy: "24", r: "6",
                            fill: "#EAB30840",
                        }
                    }
                }
            }
        }
        BlockWidget::DriveGraph => {
            rsx! {
                div { class: "w-full h-full flex items-center justify-center p-2",
                    // Drive/saturation curve
                    svg {
                        class: "w-full h-full",
                        view_box: "0 0 100 60",
                        preserve_aspect_ratio: "xMidYMid meet",
                        // Soft clipping curve
                        path {
                            d: "M 10 55 Q 30 55, 50 30 T 90 5",
                            fill: "none",
                            stroke: "#F97316",
                            stroke_width: "2",
                        }
                        // Linear reference
                        line {
                            x1: "10", y1: "55", x2: "90", y2: "5",
                            stroke: "#F9731640",
                            stroke_width: "1",
                            stroke_dasharray: "4,4",
                        }
                    }
                }
            }
        }
        BlockWidget::ModulationGraph => {
            rsx! {
                div { class: "w-full h-full flex items-center justify-center p-1",
                    // LFO wave
                    svg {
                        class: "w-full h-full",
                        view_box: "0 0 80 30",
                        preserve_aspect_ratio: "xMidYMid meet",
                        // Sine wave
                        path {
                            d: "M 0 15 Q 10 0, 20 15 T 40 15 T 60 15 T 80 15",
                            fill: "none",
                            stroke: "#A855F7",
                            stroke_width: "2",
                        }
                    }
                }
            }
        }
        BlockWidget::Tuner => {
            rsx! {
                div { class: "w-full h-full flex flex-col items-center justify-center",
                    span { class: "text-lg font-bold text-green-400", "A" }
                    span { class: "text-xs text-zinc-400", "440 Hz" }
                }
            }
        }
        BlockWidget::Looper => {
            rsx! {
                div { class: "w-full h-full flex items-center justify-center gap-1",
                    // Play/Record buttons
                    div { class: "w-4 h-4 rounded-full bg-red-500" }
                    div { class: "w-4 h-4 bg-white" }
                }
            }
        }
    }
}

/// Props for jack indicator.
#[derive(Props, Clone, PartialEq)]
struct JackIndicatorProps {
    jack: GridJack,
}

/// Input/output jack indicator.
#[component]
fn JackIndicator(props: JackIndicatorProps) -> Element {
    let jack = &props.jack;

    rsx! {
        div { class: "flex items-center gap-1",
            if jack.is_input {
                span { class: "text-xs text-zinc-400", "{jack.label}" }
                div { class: "w-2.5 h-2.5 rounded-full bg-zinc-500 border border-zinc-400" }
            } else {
                div { class: "w-2.5 h-2.5 rounded-full bg-zinc-500 border border-zinc-400" }
                span { class: "text-xs text-zinc-400", "{jack.label}" }
            }
        }
    }
}
