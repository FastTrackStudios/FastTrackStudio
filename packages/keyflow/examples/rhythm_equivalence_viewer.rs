//! Rhythm Equivalence Viewer
//!
//! Visual demonstration that different rhythm notations produce equivalent charts.
//! Shows multiple notations side by side for comparison.
//!
//! # Usage
//!
//! ```bash
//! cargo run -p keyflow --example rhythm_equivalence_viewer --features engraver-example
//! ```
//!
//! # Controls
//!
//! - Left mouse drag: Pan
//! - Scroll: Zoom (centered on cursor)
//! - R key: Reset view
//! - Space/Right arrow: Next example
//! - Left arrow: Previous example
//! - Escape: Close window

use std::sync::Arc;

use kurbo::{Affine, Point, Rect};
use vello::Scene;
use vello::peniko::Color;
use winit::{
    application::ApplicationHandler,
    dpi::LogicalSize,
    event::{ElementState, MouseButton, MouseScrollDelta, WindowEvent},
    event_loop::{ActiveEventLoop, EventLoop},
    keyboard::{KeyCode, PhysicalKey},
    window::{Window, WindowId},
};

use keyflow::Chart;
use keyflow::engraver::fonts::SMuFLFont;
use keyflow::engraver::layout::chart::{ChartLayoutEngine, ChartLayoutResult, LayoutMode};
use keyflow::engraver::renderer::{SceneRenderBuilder, VelloRenderContext};
use keyflow::engraver::style::MStyle;

const WINDOW_WIDTH: u32 = 1400;
const WINDOW_HEIGHT: u32 = 900;

/// Screen DPI for rendering
const SCREEN_DPI: f64 = 96.0;
/// Points per inch (typographical standard)
const POINTS_PER_INCH: f64 = 72.0;
/// DPI scaling factor: converts points to screen pixels
const DPI_SCALE: f64 = SCREEN_DPI / POINTS_PER_INCH;

// Font paths relative to workspace root
const SMUFL_FONT_PATH: &str = "libs/reference/sheet-music/musescore/fonts/bravura/Bravura.otf";
const SMUFL_METADATA_PATH: &str =
    "libs/reference/sheet-music/musescore/fonts/bravura/bravura_metadata.json";
const TEXT_FONT_PATH: &str = "libs/reference/sheet-music/musescore/fonts/FreeSans.ttf";
const MUSEJAZZ_FONT_PATH: &str =
    "libs/reference/sheet-music/musescore/fonts/musejazz/MuseJazzText.otf";

// ============================================================================
// region: --- Example Definitions
// ============================================================================

/// A set of equivalent notations with a description
struct EquivalenceSet {
    /// Description of this equivalence set
    description: &'static str,
    /// Multiple ways to write the same thing
    variants: Vec<(&'static str, &'static str)>,
}

fn get_equivalence_sets() -> Vec<EquivalenceSet> {
    vec![
        EquivalenceSet {
            description: "Simple Progression - No Metadata (defaults to 4/4, C)",
            variants: vec![
                // No time signature or key needed - defaults to 4/4, C major
                (
                    "Scale degrees (default)",
                    r#"
VS
1 4 5 1
"#,
                ),
                (
                    "Scale degrees with _1 (whole note)",
                    r#"
VS
1_1 4_1 5_1 1_1
"#,
                ),
                (
                    "Scale degrees with //// (4 slashes)",
                    r#"
VS
1 //// | 4 //// | 5 //// | 1 ////
"#,
                ),
                (
                    "Explicit chords (C major)",
                    r#"
VS
C | F | G | C
"#,
                ),
            ],
        },
        EquivalenceSet {
            description: "Key of G (only need #G)",
            variants: vec![
                // Just #G to change key - time sig defaults to 4/4
                (
                    "With key signature only",
                    r#"
#G

VS
1 4 6 5
"#,
                ),
                (
                    "Explicit chords",
                    r#"
#G

VS
G | C | Em | D
"#,
                ),
            ],
        },
        EquivalenceSet {
            description: "Two Chords Per Measure (half notes)",
            variants: vec![
                (
                    "With // (half note)",
                    r#"
VS
C // Am // | F // G //
"#,
                ),
                (
                    "With _2 (half note)",
                    r#"
VS
C_2 Am_2 | F_2 G_2
"#,
                ),
                (
                    "Mixed notation",
                    r#"
VS
C_2 Am // | F // G_2
"#,
                ),
            ],
        },
        EquivalenceSet {
            description: "Quarter Notes (4 per measure)",
            variants: vec![
                (
                    "With / (quarter note)",
                    r#"
VS
C / Am / F / G /
"#,
                ),
                (
                    "With _4 (quarter note)",
                    r#"
VS
C_4 Am_4 F_4 G_4
"#,
                ),
            ],
        },
        EquivalenceSet {
            description: "Mixed Rhythm (half + two quarters)",
            variants: vec![
                (
                    "Slash notation",
                    r#"
VS
C // Am / G /
"#,
                ),
                (
                    "Explicit notation",
                    r#"
VS
C_2 Am_4 G_4
"#,
                ),
                (
                    "Scale degrees with slashes",
                    r#"
VS
1 // 2 / 5 /
"#,
                ),
            ],
        },
        EquivalenceSet {
            description: "Dotted Rhythms (defaults to 4/4, C)",
            variants: vec![
                (
                    "Dotted quarter + eighth",
                    r#"
VS
C /. D_8 Em //
"#,
                ),
                (
                    "Explicit dotted",
                    r#"
VS
C_4. D_8 Em_2
"#,
                ),
            ],
        },
        EquivalenceSet {
            description: "With Rests (defaults to 4/4, C)",
            variants: vec![
                (
                    "Rest after chord",
                    r#"
VS
C /// r4
"#,
                ),
                (
                    "Explicit rest",
                    r#"
VS
C_2. r4
"#,
                ),
            ],
        },
        EquivalenceSet {
            description: "6/8 Time (only time signature needed)",
            variants: vec![
                (
                    "Explicit 6/8 with chords",
                    r#"
6/8

VS
C // Am //
"#,
                ),
                (
                    "With key change to G",
                    r#"
6/8 #G

VS
G // Em //
"#,
                ),
            ],
        },
    ]
}

// endregion: --- Example Definitions

// ============================================================================
// region: --- Layout Helpers
// ============================================================================

/// Fonts loaded for rendering
struct SnippetFonts {
    smufl_font: &'static SMuFLFont<'static>,
    text_font_data: Arc<Vec<u8>>,
    musejazz_font_data: Arc<Vec<u8>>,
}

impl SnippetFonts {
    fn load() -> Self {
        let font_data: &'static [u8] = Box::leak(
            std::fs::read(SMUFL_FONT_PATH)
                .expect("Failed to load Bravura font")
                .into_boxed_slice(),
        );

        let metadata_file =
            std::fs::File::open(SMUFL_METADATA_PATH).expect("Failed to load Bravura metadata");

        let smufl_font: &'static SMuFLFont<'static> = Box::leak(Box::new(
            SMuFLFont::from_reader(font_data, metadata_file).expect("Failed to load SMuFL font"),
        ));

        let text_font_data =
            Arc::new(std::fs::read(TEXT_FONT_PATH).expect("Failed to load text font"));

        let musejazz_font_data =
            Arc::new(std::fs::read(MUSEJAZZ_FONT_PATH).expect("Failed to load MuseJazz font"));

        Self {
            smufl_font,
            text_font_data,
            musejazz_font_data,
        }
    }
}

/// A laid out variant for display
struct LayoutedVariant {
    label: &'static str,
    chart: Chart,
    layout: ChartLayoutResult,
}

/// A complete equivalence set ready for display
struct DisplaySet {
    description: &'static str,
    variants: Vec<LayoutedVariant>,
}

fn layout_all_sets(sets: &[EquivalenceSet], fonts: &SnippetFonts) -> Vec<DisplaySet> {
    let style: &'static MStyle = Box::leak(Box::new(MStyle::default()));
    let layout_engine = ChartLayoutEngine::new(
        style,
        fonts.text_font_data.clone(),
        fonts.musejazz_font_data.clone(),
    );

    // Use Snippet mode to auto-fit height to content
    let layout_mode = LayoutMode::snippet(400.0); // Narrow for side-by-side

    sets.iter()
        .map(|set| {
            let variants = set
                .variants
                .iter()
                .map(|(label, source)| {
                    let chart = Chart::parse(source).expect("Failed to parse chart");
                    let layout = layout_engine.layout_chart(&chart, &layout_mode);
                    LayoutedVariant {
                        label,
                        chart,
                        layout,
                    }
                })
                .collect();

            DisplaySet {
                description: set.description,
                variants,
            }
        })
        .collect()
}

// endregion: --- Layout Helpers

// ============================================================================
// region: --- Main Entry Point
// ============================================================================

fn main() {
    env_logger::init();

    println!("=== Rhythm Equivalence Viewer ===");
    println!();
    println!("Controls:");
    println!("  Left drag: Pan");
    println!("  Scroll: Zoom");
    println!("  Space/Right: Next example");
    println!("  Left: Previous example");
    println!("  R: Reset view");
    println!("  Escape: Close");
    println!();

    // Load fonts
    let fonts = SnippetFonts::load();

    // Layout all equivalence sets
    let sets = get_equivalence_sets();
    let display_sets = layout_all_sets(&sets, &fonts);

    // Print summary
    for (i, set) in display_sets.iter().enumerate() {
        println!("{}. {}", i + 1, set.description);
        for variant in &set.variants {
            let section = &variant.chart.sections[0];
            let measures = section.measures().len();
            let chords: usize = section.measures().iter().map(|m| m.chords.len()).sum();
            println!(
                "     - {}: {} measures, {} chords",
                variant.label, measures, chords
            );
        }
    }
    println!();

    // Show window
    let event_loop = EventLoop::new().expect("Failed to create event loop");
    let mut app = App {
        state: None,
        display_sets,
        fonts: Some(fonts),
        current_set: 0,
    };
    event_loop.run_app(&mut app).expect("Event loop failed");
}

// endregion: --- Main Entry Point

// ============================================================================
// region: --- Window Application
// ============================================================================

struct App {
    state: Option<AppState>,
    display_sets: Vec<DisplaySet>,
    fonts: Option<SnippetFonts>,
    current_set: usize,
}

struct AppState {
    render_ctx: VelloRenderContext,
    transform: Affine,
    mouse_down: bool,
    prior_position: Option<Point>,
    smufl_font: &'static SMuFLFont<'static>,
    text_font_data: Arc<Vec<u8>>,
    musejazz_font_data: Arc<Vec<u8>>,
}

impl ApplicationHandler for App {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        if self.state.is_some() {
            return;
        }

        let title = format!(
            "Rhythm Equivalence Viewer - {} [{}/{}]",
            self.display_sets[self.current_set].description,
            self.current_set + 1,
            self.display_sets.len()
        );

        let window_attrs = Window::default_attributes()
            .with_title(&title)
            .with_inner_size(LogicalSize::new(WINDOW_WIDTH, WINDOW_HEIGHT));

        let window = Arc::new(
            event_loop
                .create_window(window_attrs)
                .expect("Failed to create window"),
        );

        let render_ctx = VelloRenderContext::new(window);
        let fonts = self.fonts.take().expect("Fonts should be loaded");

        let transform = Affine::translate((30.0, 30.0)) * Affine::scale(DPI_SCALE);

        self.state = Some(AppState {
            render_ctx,
            transform,
            mouse_down: false,
            prior_position: None,
            smufl_font: fonts.smufl_font,
            text_font_data: fonts.text_font_data,
            musejazz_font_data: fonts.musejazz_font_data,
        });

        if let Some(state) = &self.state {
            state.render_ctx.request_redraw();
        }
    }

    fn window_event(&mut self, event_loop: &ActiveEventLoop, _id: WindowId, event: WindowEvent) {
        let Some(state) = &mut self.state else {
            return;
        };

        match event {
            WindowEvent::CloseRequested => {
                event_loop.exit();
            }

            WindowEvent::KeyboardInput { event, .. } => {
                if event.state == ElementState::Pressed {
                    if let PhysicalKey::Code(code) = event.physical_key {
                        match code {
                            KeyCode::Escape => event_loop.exit(),
                            KeyCode::KeyR => {
                                state.transform =
                                    Affine::translate((30.0, 30.0)) * Affine::scale(DPI_SCALE);
                                state.render_ctx.request_redraw();
                            }
                            KeyCode::Space | KeyCode::ArrowRight => {
                                self.current_set = (self.current_set + 1) % self.display_sets.len();
                                state.transform =
                                    Affine::translate((30.0, 30.0)) * Affine::scale(DPI_SCALE);
                                // Update window title
                                let title = format!(
                                    "Rhythm Equivalence Viewer - {} [{}/{}]",
                                    self.display_sets[self.current_set].description,
                                    self.current_set + 1,
                                    self.display_sets.len()
                                );
                                state.render_ctx.window.set_title(&title);
                                state.render_ctx.request_redraw();
                            }
                            KeyCode::ArrowLeft => {
                                if self.current_set == 0 {
                                    self.current_set = self.display_sets.len() - 1;
                                } else {
                                    self.current_set -= 1;
                                }
                                state.transform =
                                    Affine::translate((30.0, 30.0)) * Affine::scale(DPI_SCALE);
                                let title = format!(
                                    "Rhythm Equivalence Viewer - {} [{}/{}]",
                                    self.display_sets[self.current_set].description,
                                    self.current_set + 1,
                                    self.display_sets.len()
                                );
                                state.render_ctx.window.set_title(&title);
                                state.render_ctx.request_redraw();
                            }
                            _ => {}
                        }
                    }
                }
            }

            WindowEvent::MouseWheel { delta, .. } => {
                const BASE: f64 = 1.05;
                const PIXELS_PER_LINE: f64 = 20.0;

                if let Some(prior_position) = state.prior_position {
                    let exponent = match delta {
                        MouseScrollDelta::LineDelta(_, y) => y as f64,
                        MouseScrollDelta::PixelDelta(delta) => delta.y / PIXELS_PER_LINE,
                    };
                    state.transform = state
                        .transform
                        .then_scale_about(BASE.powf(exponent), prior_position);
                    state.render_ctx.request_redraw();
                }
            }

            WindowEvent::CursorMoved { position, .. } => {
                let pos = Point::new(position.x, position.y);
                if state.mouse_down {
                    if let Some(prior) = state.prior_position {
                        let delta = pos - prior;
                        state.transform = state.transform.then_translate(delta);
                        state.render_ctx.request_redraw();
                    }
                }
                state.prior_position = Some(pos);
            }

            WindowEvent::MouseInput {
                state: button_state,
                button,
                ..
            } => {
                if button == MouseButton::Left {
                    state.mouse_down = button_state == ElementState::Pressed;
                }
            }

            WindowEvent::Resized(new_size) => {
                state.render_ctx.resize(new_size.width, new_size.height);
            }

            WindowEvent::RedrawRequested => {
                let (width, height) = state.render_ctx.viewport_size();

                let mut vello_scene = Scene::new();

                // Dark background
                vello_scene.fill(
                    vello::peniko::Fill::NonZero,
                    Affine::IDENTITY,
                    Color::from_rgb8(48, 48, 48),
                    None,
                    &Rect::new(0.0, 0.0, width as f64, height as f64),
                );

                // Create renderer
                let mut renderer = SceneRenderBuilder::new()
                    .spatium(5.0)
                    .build()
                    .with_font(state.smufl_font)
                    .with_text_font_arc(state.text_font_data.clone())
                    .with_named_font_arc("MuseJazzText", state.musejazz_font_data.clone())
                    .with_named_font_arc("MuseJazz", state.musejazz_font_data.clone());

                // Render all variants in the current set side by side
                let current_display = &self.display_sets[self.current_set];
                let variant_count = current_display.variants.len();
                let column_width = 420.0; // Width per column in points

                for (i, variant) in current_display.variants.iter().enumerate() {
                    // Offset for this column
                    let x_offset = i as f64 * column_width;

                    // Clone and transform the scene for this variant
                    let mut variant_scene = variant.layout.scene.clone();

                    // Apply column offset, then view transform
                    variant_scene.transform = state.transform * Affine::translate((x_offset, 0.0));

                    renderer.render(&mut vello_scene, &variant_scene);
                }

                state.render_ctx.render(&vello_scene);
            }

            _ => {}
        }
    }
}

// endregion: --- Window Application
