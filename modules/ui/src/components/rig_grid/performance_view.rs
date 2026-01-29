//! Performance view - maximized layout for live use.
//!
//! Organizes modules into horizontal racks with optimal spacing,
//! minimizing chrome and maximizing widget visibility.

use dioxus::prelude::*;

use super::node_graph::{Module, NodeGraph};
use super::block_colors::{block_type_color, block_instance_color};
use audio_controls::widgets::{CompressorGraph, CompressorParams, DbRange, EqBand, EqBandShape, EqGraph};

/// Detail level for module display in performance view.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum DetailLevel {
    /// Single macro control
    Macro,
    /// Normal widget view
    #[default]
    Standard,
    /// Full detailed parameters
    Detailed,
}

/// Module with performance view settings.
#[derive(Debug, Clone, PartialEq)]
pub struct PerformanceModule {
    pub module: Module,
    pub detail_level: DetailLevel,
    pub preferred_aspect_ratio: f64, // width / height
}

impl PerformanceModule {
    pub fn new(module: Module, detail_level: DetailLevel) -> Self {
        // Calculate preferred aspect ratio based on content
        let aspect_ratio = Self::calculate_aspect_ratio(&module, detail_level);
        Self {
            module,
            detail_level,
            preferred_aspect_ratio: aspect_ratio,
        }
    }

    fn calculate_aspect_ratio(module: &Module, detail_level: DetailLevel) -> f64 {
        // Intelligent aspect ratio based on module content

        // Special case: single node modules inherit the node's optimal ratio
        if module.nodes.len() == 1 {
            let node = &module.nodes[0];
            return match node.widget {
                super::node_graph::NodeWidget::CompressorGraph => 1.0,  // Square for compressor
                super::node_graph::NodeWidget::EqGraph => 1.5,          // Wider for EQ
                super::node_graph::NodeWidget::Label => {
                    // For label widgets, check the module name for hints
                    if module.name.to_lowercase().contains("volume") {
                        0.5  // Tall/narrow like 500-series (iPhone aspect)
                    } else {
                        1.0
                    }
                }
                _ => 1.0,
            };
        }

        // Multi-node modules: calculate based on layout
        if module.name.to_lowercase().contains("amp") || module.name.to_lowercase().contains("cab") {
            // Amp/Cab: two amps vertically into two cabs, wider layout needed
            return 1.8;
        }

        if module.name.to_lowercase().contains("special") {
            // Special effects benefit from wider layout
            return 2.5;
        }

        if module.name.to_lowercase().contains("drive") {
            // Drive section with multiple drives benefits from wider layout
            return 2.5;
        }

        // Default: base on detail level
        match detail_level {
            DetailLevel::Macro => 1.0,      // Square for macro
            DetailLevel::Standard => 1.5,   // Wider for standard widgets
            DetailLevel::Detailed => 2.0,   // Widest for detailed view
        }
    }
}

/// A horizontal rack containing one or more modules.
#[derive(Debug, Clone)]
struct Rack {
    modules: Vec<PerformanceModule>,
    total_aspect_ratio: f64,
}

impl Rack {
    fn new() -> Self {
        Self {
            modules: Vec::new(),
            total_aspect_ratio: 0.0,
        }
    }

    fn add_module(&mut self, module: PerformanceModule) {
        self.total_aspect_ratio += module.preferred_aspect_ratio;
        self.modules.push(module);
    }

    fn can_fit(&self, viewport_aspect_ratio: f64, max_modules_per_rack: usize) -> bool {
        self.modules.len() < max_modules_per_rack && self.total_aspect_ratio < viewport_aspect_ratio * 1.2
    }
}

/// Layout algorithm for organizing modules into racks.
fn layout_racks(modules: Vec<PerformanceModule>, viewport_width: f64, viewport_height: f64) -> Vec<Rack> {
    let viewport_aspect = viewport_width / viewport_height;
    let mut racks = Vec::new();
    let mut current_rack = Rack::new();

    for module in modules {
        // Try to fit in current rack
        if current_rack.can_fit(viewport_aspect, 4) {
            current_rack.add_module(module);
        } else {
            // Start new rack
            if !current_rack.modules.is_empty() {
                racks.push(current_rack);
            }
            current_rack = Rack::new();
            current_rack.add_module(module);
        }
    }

    // Add final rack
    if !current_rack.modules.is_empty() {
        racks.push(current_rack);
    }

    racks
}

/// Layout algorithm for fit-all mode: ensures all modules visible without scrolling.
fn layout_racks_fit_all(modules: Vec<PerformanceModule>, viewport_width: f64, viewport_height: f64) -> Vec<Rack> {
    if modules.is_empty() {
        return Vec::new();
    }

    let viewport_aspect = viewport_width / viewport_height;
    let module_count = modules.len();

    // Calculate optimal number of racks based on module count and viewport
    // Try to balance vertical stacking with horizontal spreading
    let optimal_racks = ((module_count as f64).sqrt().ceil() as usize).max(1);
    let modules_per_rack = (module_count as f64 / optimal_racks as f64).ceil() as usize;

    let mut racks = Vec::new();
    let mut current_rack = Rack::new();

    for module in modules {
        if current_rack.modules.len() >= modules_per_rack {
            racks.push(current_rack);
            current_rack = Rack::new();
        }
        current_rack.add_module(module);
    }

    if !current_rack.modules.is_empty() {
        racks.push(current_rack);
    }

    racks
}

/// Props for performance view.
#[derive(Props, Clone, PartialEq)]
pub struct PerformanceViewProps {
    pub graph: Signal<NodeGraph>,
}

/// Performance view component - maximized layout for live performance.
#[component]
pub fn PerformanceView(props: PerformanceViewProps) -> Element {
    let graph = props.graph.read();

    // Fit-all mode: forces all modules visible on screen
    let mut fit_all = use_signal(|| false);

    // Convert modules to performance modules with detail levels
    // In real implementation, this would be stored state
    let perf_modules: Vec<PerformanceModule> = graph.modules.iter()
        .map(|m| PerformanceModule::new(m.clone(), DetailLevel::Standard))
        .collect();

    // Use reasonable viewport aspect ratio (16:9)
    // Since we're using flex layout, we don't need exact pixel dimensions
    let viewport_width = 1920.0;
    let viewport_height = 1080.0;

    // Layout modules into racks
    let racks = if fit_all() {
        // In fit-all mode, force all modules into racks that fit on screen
        layout_racks_fit_all(perf_modules, viewport_width, viewport_height)
    } else {
        layout_racks(perf_modules, viewport_width, viewport_height)
    };
    let rack_count = racks.len().max(1);

    rsx! {
        div {
            class: "w-full h-full bg-zinc-950 flex flex-col relative",
            style: "margin: 0; padding: 0;",

            // Fit All toggle button (top-left, below view mode toggle)
            div {
                class: "absolute top-16 right-3 z-10",
                button {
                    class: if fit_all() {
                        "px-3 py-1.5 rounded-lg bg-purple-600 text-white text-xs font-medium"
                    } else {
                        "px-3 py-1.5 rounded-lg bg-zinc-800 hover:bg-zinc-700 text-zinc-300 text-xs font-medium"
                    },
                    onclick: move |_| fit_all.toggle(),
                    "Fit All"
                }
            }

            // Racks container - no gaps
            div {
                class: if fit_all() {
                    "w-full h-full flex flex-col overflow-hidden"
                } else {
                    "w-full h-full flex flex-col overflow-y-auto"
                },
                style: "margin: 0; padding: 0; gap: 0;",

                // Render racks
                for (rack_idx, rack) in racks.iter().enumerate() {
                    {
                        // Each rack takes equal vertical space
                        let height_percent = 100.0 / rack_count as f64;
                        let min_height = if fit_all() { "120px" } else { "200px" };

                        rsx! {
                            div {
                                key: "{rack_idx}",
                                class: "flex w-full flex-shrink-0",
                                style: "height: {height_percent}%; min-height: {min_height}; margin: 0; padding: 0; gap: 0;",

                                // Render modules in rack
                                for (mod_idx, perf_module) in rack.modules.iter().enumerate() {
                                    {
                                        // Calculate width based on aspect ratio
                                        let width_ratio = perf_module.preferred_aspect_ratio / rack.total_aspect_ratio;
                                        let width_percent = width_ratio * 100.0;
                                        let min_width = if fit_all() { "80px" } else { "150px" };

                                        rsx! {
                                            div {
                                                key: "{mod_idx}",
                                                class: "flex flex-col flex-shrink-0 relative group",
                                                style: "width: {width_percent}%; min-width: {min_width}; margin: 0; padding: 0;",

                                                PerformanceModuleRenderer {
                                                    perf_module: perf_module.clone(),
                                                }

                                                // Resize handle (right edge) - visible on hover
                                                div {
                                                    class: "absolute right-0 top-0 bottom-0 w-1 cursor-col-resize \
                                                            hover:bg-blue-500 group-hover:bg-zinc-600 transition-colors",
                                                    title: "Drag to resize module width",
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Props for performance module renderer.
#[derive(Props, Clone, PartialEq)]
struct PerformanceModuleRendererProps {
    perf_module: PerformanceModule,
}

/// Renders a single module in performance view with minimal chrome.
#[component]
fn PerformanceModuleRenderer(props: PerformanceModuleRendererProps) -> Element {
    let module = &props.perf_module.module;
    let detail_level = props.perf_module.detail_level;
    let color = block_instance_color(module.block_type, &module.name);

    let bg_color = if module.bypassed {
        format!("{}20", color.bg)
    } else {
        format!("{}10", color.bg)
    };

    rsx! {
        div {
            class: "w-full h-full flex flex-col relative",
            style: "background-color: {bg_color}; margin: 0; padding: 0;",

            // Ultra-compact name indicator in top-left corner (overlaid)
            div {
                class: "absolute top-1 left-1 z-10 flex items-center gap-1 px-1.5 py-0.5 rounded text-[10px] font-semibold",
                style: "background-color: rgba(0, 0, 0, 0.75); border: 1px solid {color.border}; color: #e4e4e7;",

                span { "{module.name}" }

                // Bypass indicator
                if module.bypassed {
                    div {
                        class: "w-1.5 h-1.5 rounded-full bg-red-500"
                    }
                }
            }

            // Content area - full height, no padding, no margin
            div {
                class: "w-full h-full",
                style: "margin: 0; padding: 0;",

                PerformanceModuleContent {
                    module: module.clone(),
                    detail_level: detail_level,
                }
            }
        }
    }
}

/// Props for performance module content.
#[derive(Props, Clone, PartialEq)]
struct PerformanceModuleContentProps {
    module: Module,
    detail_level: DetailLevel,
}

/// Renders module content based on detail level.
#[component]
fn PerformanceModuleContent(props: PerformanceModuleContentProps) -> Element {
    let module = &props.module;

    // If single node, render that node's widget
    if module.nodes.len() == 1 {
        let node = &module.nodes[0];

        match node.widget {
            super::node_graph::NodeWidget::EqGraph => {
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
                        class: "w-full h-full relative",
                        style: "margin: 0; padding: 0; border: 1px solid rgba(255, 255, 255, 0.1);",

                        // Tiny block name indicator
                        div {
                            class: "absolute bottom-0.5 right-0.5 px-1 py-0.5 text-[8px] font-medium rounded z-10",
                            style: "background-color: rgba(0, 0, 0, 0.4); color: rgba(255, 255, 255, 0.6);",
                            "{node.short_label.as_deref().unwrap_or(&node.name)}"
                        }

                        EqGraph {
                            bands: bands,
                            db_range: 12.0,
                            show_grid: true,
                            fill_curve: true,
                            disabled: false,
                            on_band_change: move |(idx, band): (usize, EqBand)| {
                                bands.write()[idx] = band;
                            },
                            on_band_add: move |band: EqBand| {
                                let mut all_bands = bands();
                                all_bands.push(band);
                                bands.set(all_bands);
                            },
                            on_band_remove: move |idx: usize| {
                                let mut all_bands = bands();
                                all_bands.remove(idx);
                                bands.set(all_bands);
                            },
                        }
                    }
                }
            }
            super::node_graph::NodeWidget::CompressorGraph => {
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
                    div {
                        class: "w-full h-full relative",
                        style: "margin: 0; padding: 0; border: 1px solid rgba(255, 255, 255, 0.1);",

                        // Tiny block name indicator
                        div {
                            class: "absolute bottom-0.5 right-0.5 px-1 py-0.5 text-[8px] font-medium rounded z-10",
                            style: "background-color: rgba(0, 0, 0, 0.4); color: rgba(255, 255, 255, 0.6);",
                            "{node.short_label.as_deref().unwrap_or(&node.name)}"
                        }

                        CompressorGraph {
                            params: params,
                            db_range: DbRange::default(),
                            show_grid: true,
                        }
                    }
                }
            }
            _ => {
                rsx! {
                    div {
                        class: "w-full h-full relative",
                        style: "margin: 0; padding: 0; border: 1px solid rgba(255, 255, 255, 0.1);",

                        // Tiny block name indicator
                        div {
                            class: "absolute bottom-0.5 right-0.5 px-1 py-0.5 text-[8px] font-medium rounded z-10",
                            style: "background-color: rgba(0, 0, 0, 0.4); color: rgba(255, 255, 255, 0.6);",
                            "{node.short_label.as_deref().unwrap_or(&node.name)}"
                        }
                    }
                }
            }
        }
    } else {
        // Multiple nodes - create a gap-free CSS Grid layout with row spanning

        // Determine unique Y positions (rows) - only use positions where multiple nodes start
        // or where nodes start at regular intervals
        let mut all_y_positions: Vec<f64> = module.nodes.iter().map(|n| n.position.y).collect();
        all_y_positions.sort_by(|a, b| a.partial_cmp(b).unwrap());
        all_y_positions.dedup_by(|a, b| (*a - *b).abs() < 10.0);

        // Only keep Y positions that have multiple nodes starting there (are actual row starts)
        let y_positions: Vec<f64> = all_y_positions.iter().filter(|&&y| {
            let nodes_at_y = module.nodes.iter()
                .filter(|n| (n.position.y - y).abs() < 10.0)
                .count();
            nodes_at_y >= 2 // At least 2 nodes start at this Y position
        }).copied().collect();

        let mut x_positions: Vec<f64> = module.nodes.iter().map(|n| n.position.x).collect();
        x_positions.sort_by(|a, b| a.partial_cmp(b).unwrap());
        x_positions.dedup_by(|a, b| (*a - *b).abs() < 10.0);

        let row_count = y_positions.len().max(1);
        let col_count = x_positions.len().max(1);

        // Calculate which grid cells each node occupies
        let mut grid_items: Vec<_> = module.nodes.iter().map(|node| {
            let node_bottom = node.position.y + node.size.height;

            // Find starting row: which row does this node start in?
            let start_row = y_positions.iter()
                .position(|&y| node.position.y >= y - 10.0 && node.position.y <= y + 10.0)
                .unwrap_or_else(|| {
                    // Node doesn't align with a row start, find which row it's within
                    y_positions.iter()
                        .position(|&y| node.position.y >= y)
                        .unwrap_or(0)
                })
                + 1; // +1 for 1-indexed CSS Grid

            // Find ending row: which row does this node's bottom reach?
            let end_row = y_positions.iter()
                .enumerate()
                .rev()
                .find(|(_, y)| node_bottom >= *y + 10.0) // Node extends past this row's start
                .map(|(i, _)| i + 2) // +2: index + 1 for grid line + 1 for next line
                .unwrap_or(start_row + 1); // Default: spans only its starting row

            // Find starting column
            let start_col = x_positions.iter()
                .position(|&x| (node.position.x - x).abs() < 10.0)
                .map(|i| i + 1)
                .unwrap_or(1);

            (node, start_row, end_row, start_col)
        }).collect();

        rsx! {
            div {
                class: "w-full h-full",
                style: "display: grid; grid-template-rows: repeat({row_count}, 1fr); grid-template-columns: repeat({col_count}, 1fr); margin: 0; padding: 0; gap: 0;",

                // Render each node with proper grid placement
                for (node, start_row, end_row, start_col) in grid_items.iter() {
                    {
                        let color = block_instance_color(node.block_type, &node.name);
                        let grid_row = format!("{} / {}", start_row, end_row);
                        let grid_col = format!("{} / {}", start_col, start_col + 1);

                        rsx! {
                            div {
                                key: "{node.id}",
                                class: "overflow-hidden relative",
                                style: "grid-row: {grid_row}; grid-column: {grid_col}; background-color: {color.bg}; margin: 0; padding: 0; border: 1px solid rgba(255, 255, 255, 0.1);",

                                // Tiny block name indicator in bottom-right corner
                                div {
                                    class: "absolute bottom-0.5 right-0.5 px-1 py-0.5 text-[8px] font-medium rounded z-10",
                                    style: "background-color: rgba(0, 0, 0, 0.4); color: rgba(255, 255, 255, 0.6);",
                                    "{node.short_label.as_deref().unwrap_or(&node.name)}"
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
