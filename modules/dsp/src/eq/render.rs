//! EQ graph rendering using anyrender patterns.
//!
//! This module provides rendering primitives for parametric EQ visualization
//! using the same patterns as keyflow's engraver module (kurbo paths, Vello colors).
//!
//! The rendering is backend-agnostic and can be rendered to:
//! - WGPU (via tessellation)
//! - SVG (via path serialization)
//! - Canvas2D (web)

use kurbo::{BezPath, Point, Rect};

use super::filter::FrequencyResponse;
use super::params::{AnalyzerParams, BandParams, EqParams, MAX_BANDS};

/// Colors for EQ visualization.
///
/// Uses sRGB linear color space for consistency with Vello.
#[derive(Debug, Clone, Copy)]
pub struct EqColor {
    pub r: f32,
    pub g: f32,
    pub b: f32,
    pub a: f32,
}

impl EqColor {
    /// Create a new color.
    pub const fn new(r: f32, g: f32, b: f32, a: f32) -> Self {
        Self { r, g, b, a }
    }

    /// Create a color from RGB values (0-255).
    #[must_use]
    pub fn from_rgb8(r: u8, g: u8, b: u8) -> Self {
        Self {
            r: r as f32 / 255.0,
            g: g as f32 / 255.0,
            b: b as f32 / 255.0,
            a: 1.0,
        }
    }

    /// Create a color from RGBA values (0-255).
    #[must_use]
    pub fn from_rgba8(r: u8, g: u8, b: u8, a: u8) -> Self {
        Self {
            r: r as f32 / 255.0,
            g: g as f32 / 255.0,
            b: b as f32 / 255.0,
            a: a as f32 / 255.0,
        }
    }

    /// Create a color with modified alpha.
    #[must_use]
    pub const fn with_alpha(self, a: f32) -> Self {
        Self {
            r: self.r,
            g: self.g,
            b: self.b,
            a,
        }
    }

    /// Convert to CSS rgba() string.
    #[must_use]
    pub fn to_css(&self) -> String {
        let r = (self.r * 255.0).round() as u8;
        let g = (self.g * 255.0).round() as u8;
        let b = (self.b * 255.0).round() as u8;
        if (self.a - 1.0).abs() < f32::EPSILON {
            format!("#{r:02x}{g:02x}{b:02x}")
        } else {
            format!("rgba({r},{g},{b},{:.3})", self.a)
        }
    }

    /// Convert to [f32; 4] array for GPU.
    #[must_use]
    pub const fn to_array(&self) -> [f32; 4] {
        [self.r, self.g, self.b, self.a]
    }

    // Predefined colors for EQ visualization
    pub const BACKGROUND: Self = Self::new(0.067, 0.075, 0.086, 1.0); // #111316
    pub const GRID_MAJOR: Self = Self::new(0.3, 0.3, 0.35, 0.5);
    pub const GRID_MINOR: Self = Self::new(0.2, 0.2, 0.25, 0.3);
    pub const CURVE_FILL: Self = Self::new(0.2, 0.6, 0.8, 0.3);
    pub const CURVE_STROKE: Self = Self::new(0.4, 0.8, 1.0, 1.0);
    pub const SPECTRUM_PRE: Self = Self::new(0.5, 0.5, 0.5, 0.4);
    pub const SPECTRUM_POST: Self = Self::new(0.2, 0.8, 0.4, 0.5);
    pub const BAND_ACTIVE: Self = Self::new(1.0, 0.8, 0.2, 1.0);
    pub const BAND_INACTIVE: Self = Self::new(0.5, 0.5, 0.5, 0.5);
    pub const TEXT: Self = Self::new(0.7, 0.7, 0.7, 1.0);
}

/// Line cap style for strokes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum LineCap {
    #[default]
    Butt,
    Round,
    Square,
}

/// Line join style for strokes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum LineJoin {
    #[default]
    Miter,
    Round,
    Bevel,
}

/// A paint command for EQ rendering.
///
/// Similar to keyflow's PaintCommand but specialized for EQ visualization.
#[derive(Debug, Clone)]
pub enum EqPaintCommand {
    /// Fill a path with a solid color.
    Fill {
        path: BezPath,
        color: EqColor,
    },

    /// Stroke a path.
    Stroke {
        path: BezPath,
        color: EqColor,
        width: f64,
        line_cap: LineCap,
        line_join: LineJoin,
    },

    /// Draw a line.
    Line {
        start: Point,
        end: Point,
        color: EqColor,
        width: f64,
    },

    /// Draw a rectangle.
    Rect {
        rect: Rect,
        fill: Option<EqColor>,
        stroke: Option<EqColor>,
        stroke_width: f64,
    },

    /// Draw a circle (for band control points).
    Circle {
        center: Point,
        radius: f64,
        fill: Option<EqColor>,
        stroke: Option<EqColor>,
        stroke_width: f64,
    },

    /// Draw text.
    Text {
        text: String,
        position: Point,
        font_size: f64,
        color: EqColor,
        anchor: TextAnchor,
    },
}

/// Text anchor point.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum TextAnchor {
    #[default]
    Start,
    Middle,
    End,
}

impl EqPaintCommand {
    /// Create a simple line.
    #[must_use]
    pub fn line(start: Point, end: Point, color: EqColor, width: f64) -> Self {
        Self::Line {
            start,
            end,
            color,
            width,
        }
    }

    /// Create a filled circle.
    #[must_use]
    pub fn filled_circle(center: Point, radius: f64, color: EqColor) -> Self {
        Self::Circle {
            center,
            radius,
            fill: Some(color),
            stroke: None,
            stroke_width: 0.0,
        }
    }

    /// Create a stroked circle.
    #[must_use]
    pub fn stroked_circle(center: Point, radius: f64, color: EqColor, width: f64) -> Self {
        Self::Circle {
            center,
            radius,
            fill: None,
            stroke: Some(color),
            stroke_width: width,
        }
    }

    /// Create a filled and stroked circle.
    #[must_use]
    pub fn circle(
        center: Point,
        radius: f64,
        fill: EqColor,
        stroke: EqColor,
        stroke_width: f64,
    ) -> Self {
        Self::Circle {
            center,
            radius,
            fill: Some(fill),
            stroke: Some(stroke),
            stroke_width,
        }
    }

    /// Create a filled path.
    #[must_use]
    pub fn filled_path(path: BezPath, color: EqColor) -> Self {
        Self::Fill { path, color }
    }

    /// Create a stroked path.
    #[must_use]
    pub fn stroked_path(path: BezPath, color: EqColor, width: f64) -> Self {
        Self::Stroke {
            path,
            color,
            width,
            line_cap: LineCap::Round,
            line_join: LineJoin::Round,
        }
    }
}

/// Configuration for EQ graph rendering.
#[derive(Debug, Clone)]
pub struct EqGraphConfig {
    /// Total width of the graph in pixels.
    pub width: f64,
    /// Total height of the graph in pixels.
    pub height: f64,
    /// Padding around the graph.
    pub padding: f64,
    /// dB range for vertical axis (symmetric around 0).
    pub db_range: f64,
    /// Minimum frequency (Hz).
    pub min_freq: f64,
    /// Maximum frequency (Hz).
    pub max_freq: f64,
    /// Sample rate for filter calculations.
    pub sample_rate: f64,
    /// Number of points for curve rendering.
    pub curve_points: usize,
    /// Show grid lines.
    pub show_grid: bool,
    /// Show frequency labels.
    pub show_freq_labels: bool,
    /// Show dB labels.
    pub show_db_labels: bool,
    /// Show spectrum analyzer.
    pub show_spectrum: bool,
    /// Fill under the curve.
    pub fill_curve: bool,
    /// Band control point radius.
    pub control_radius: f64,
    /// Colors.
    pub colors: EqGraphColors,
}

/// Color configuration for EQ graph.
#[derive(Debug, Clone)]
pub struct EqGraphColors {
    pub background: EqColor,
    pub grid_major: EqColor,
    pub grid_minor: EqColor,
    pub curve_stroke: EqColor,
    pub curve_fill: EqColor,
    pub spectrum_pre: EqColor,
    pub spectrum_post: EqColor,
    pub band_active: EqColor,
    pub band_inactive: EqColor,
    pub text: EqColor,
}

impl Default for EqGraphColors {
    fn default() -> Self {
        Self {
            background: EqColor::BACKGROUND,
            grid_major: EqColor::GRID_MAJOR,
            grid_minor: EqColor::GRID_MINOR,
            curve_stroke: EqColor::CURVE_STROKE,
            curve_fill: EqColor::CURVE_FILL,
            spectrum_pre: EqColor::SPECTRUM_PRE,
            spectrum_post: EqColor::SPECTRUM_POST,
            band_active: EqColor::BAND_ACTIVE,
            band_inactive: EqColor::BAND_INACTIVE,
            text: EqColor::TEXT,
        }
    }
}

impl Default for EqGraphConfig {
    fn default() -> Self {
        Self {
            width: 800.0,
            height: 300.0,
            padding: 40.0,
            db_range: 24.0,
            min_freq: 20.0,
            max_freq: 20000.0,
            sample_rate: 48000.0,
            curve_points: 400,
            show_grid: true,
            show_freq_labels: true,
            show_db_labels: true,
            show_spectrum: true,
            fill_curve: true,
            control_radius: 8.0,
            colors: EqGraphColors::default(),
        }
    }
}

impl EqGraphConfig {
    /// Get the drawable area (excluding padding).
    #[must_use]
    pub fn graph_rect(&self) -> Rect {
        Rect::new(
            self.padding,
            self.padding,
            self.width - self.padding,
            self.height - self.padding,
        )
    }

    /// Convert frequency to X coordinate.
    #[must_use]
    pub fn freq_to_x(&self, freq: f64) -> f64 {
        let rect = self.graph_rect();
        let log_min = self.min_freq.log10();
        let log_max = self.max_freq.log10();
        let normalized = (freq.log10() - log_min) / (log_max - log_min);
        rect.x0 + normalized * rect.width()
    }

    /// Convert X coordinate to frequency.
    #[must_use]
    pub fn x_to_freq(&self, x: f64) -> f64 {
        let rect = self.graph_rect();
        let normalized = (x - rect.x0) / rect.width();
        let log_min = self.min_freq.log10();
        let log_max = self.max_freq.log10();
        10.0_f64.powf(log_min + normalized * (log_max - log_min))
    }

    /// Convert dB to Y coordinate.
    #[must_use]
    pub fn db_to_y(&self, db: f64) -> f64 {
        let rect = self.graph_rect();
        let normalized = 0.5 - db / (2.0 * self.db_range);
        rect.y0 + normalized * rect.height()
    }

    /// Convert Y coordinate to dB.
    #[must_use]
    pub fn y_to_db(&self, y: f64) -> f64 {
        let rect = self.graph_rect();
        let normalized = (y - rect.y0) / rect.height();
        (0.5 - normalized) * 2.0 * self.db_range
    }
}

/// Renders an EQ graph to a list of paint commands.
pub struct EqGraphRenderer {
    config: EqGraphConfig,
    freq_response: FrequencyResponse,
}

impl EqGraphRenderer {
    /// Create a new EQ graph renderer.
    #[must_use]
    pub fn new(config: EqGraphConfig) -> Self {
        let freq_response = FrequencyResponse::new(
            config.sample_rate,
            config.curve_points,
            config.min_freq,
            config.max_freq,
        );
        Self {
            config,
            freq_response,
        }
    }

    /// Create with default configuration.
    #[must_use]
    pub fn default_for_size(width: f64, height: f64) -> Self {
        let config = EqGraphConfig {
            width,
            height,
            ..Default::default()
        };
        Self::new(config)
    }

    /// Render the complete EQ graph.
    #[must_use]
    pub fn render(&self, params: &EqParams) -> Vec<EqPaintCommand> {
        let mut commands = Vec::with_capacity(256);

        // Background
        commands.push(EqPaintCommand::Rect {
            rect: Rect::new(0.0, 0.0, self.config.width, self.config.height),
            fill: Some(self.config.colors.background),
            stroke: None,
            stroke_width: 0.0,
        });

        // Grid
        if self.config.show_grid {
            self.render_grid(&mut commands);
        }

        // Labels
        if self.config.show_freq_labels {
            self.render_freq_labels(&mut commands);
        }
        if self.config.show_db_labels {
            self.render_db_labels(&mut commands);
        }

        // Combined EQ curve
        self.render_eq_curve(&mut commands, &params.bands);

        // Individual band curves (optional, for debugging)
        // self.render_band_curves(&mut commands, &params.bands);

        // Band control points
        self.render_band_controls(&mut commands, &params.bands);

        commands
    }

    /// Render the frequency/dB grid.
    fn render_grid(&self, commands: &mut Vec<EqPaintCommand>) {
        let rect = self.config.graph_rect();

        // Major frequency lines: 100, 1k, 10k Hz
        let major_freqs = [100.0, 1000.0, 10000.0];
        for freq in major_freqs {
            let x = self.config.freq_to_x(freq);
            commands.push(EqPaintCommand::line(
                Point::new(x, rect.y0),
                Point::new(x, rect.y1),
                self.config.colors.grid_major,
                1.0,
            ));
        }

        // Minor frequency lines: 20, 50, 200, 500, 2k, 5k, 20k Hz
        let minor_freqs = [20.0, 50.0, 200.0, 500.0, 2000.0, 5000.0, 20000.0];
        for freq in minor_freqs {
            let x = self.config.freq_to_x(freq);
            commands.push(EqPaintCommand::line(
                Point::new(x, rect.y0),
                Point::new(x, rect.y1),
                self.config.colors.grid_minor,
                0.5,
            ));
        }

        // Major dB lines: 0 dB (center)
        let y_zero = self.config.db_to_y(0.0);
        commands.push(EqPaintCommand::line(
            Point::new(rect.x0, y_zero),
            Point::new(rect.x1, y_zero),
            self.config.colors.grid_major,
            1.0,
        ));

        // Minor dB lines: every 6 dB
        let db_step = 6.0;
        let mut db = db_step;
        while db <= self.config.db_range {
            let y_pos = self.config.db_to_y(db);
            let y_neg = self.config.db_to_y(-db);

            commands.push(EqPaintCommand::line(
                Point::new(rect.x0, y_pos),
                Point::new(rect.x1, y_pos),
                self.config.colors.grid_minor,
                0.5,
            ));
            commands.push(EqPaintCommand::line(
                Point::new(rect.x0, y_neg),
                Point::new(rect.x1, y_neg),
                self.config.colors.grid_minor,
                0.5,
            ));

            db += db_step;
        }
    }

    /// Render frequency labels.
    fn render_freq_labels(&self, commands: &mut Vec<EqPaintCommand>) {
        let rect = self.config.graph_rect();
        let y = rect.y1 + 20.0;

        let labels = [
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

        for (freq, label) in labels {
            let x = self.config.freq_to_x(freq);
            commands.push(EqPaintCommand::Text {
                text: label.to_string(),
                position: Point::new(x, y),
                font_size: 10.0,
                color: self.config.colors.text,
                anchor: TextAnchor::Middle,
            });
        }
    }

    /// Render dB labels.
    fn render_db_labels(&self, commands: &mut Vec<EqPaintCommand>) {
        let rect = self.config.graph_rect();
        let x = rect.x0 - 10.0;

        let db_step = 6.0;
        let mut db = 0.0;

        // Center (0 dB)
        commands.push(EqPaintCommand::Text {
            text: "0".to_string(),
            position: Point::new(x, self.config.db_to_y(0.0)),
            font_size: 10.0,
            color: self.config.colors.text,
            anchor: TextAnchor::End,
        });

        db = db_step;
        while db <= self.config.db_range {
            // Positive
            commands.push(EqPaintCommand::Text {
                text: format!("+{}", db as i32),
                position: Point::new(x, self.config.db_to_y(db)),
                font_size: 10.0,
                color: self.config.colors.text,
                anchor: TextAnchor::End,
            });

            // Negative
            commands.push(EqPaintCommand::Text {
                text: format!("{}", -(db as i32)),
                position: Point::new(x, self.config.db_to_y(-db)),
                font_size: 10.0,
                color: self.config.colors.text,
                anchor: TextAnchor::End,
            });

            db += db_step;
        }
    }

    /// Render the combined EQ response curve.
    fn render_eq_curve(&self, commands: &mut Vec<EqPaintCommand>, bands: &[BandParams; MAX_BANDS]) {
        let rect = self.config.graph_rect();

        // Calculate combined response
        let response_db = self.freq_response.combined_response_db(bands);
        let frequencies = self.freq_response.frequencies();

        if frequencies.is_empty() {
            return;
        }

        // Build the curve path
        let mut curve_path = BezPath::new();
        let mut fill_path = BezPath::new();

        // Start at first point
        let first_x = self.config.freq_to_x(frequencies[0]);
        let first_y = self.config.db_to_y(response_db[0]);
        curve_path.move_to(Point::new(first_x, first_y));

        // For fill, start at bottom-left corner
        if self.config.fill_curve {
            fill_path.move_to(Point::new(first_x, rect.y1));
            fill_path.line_to(Point::new(first_x, first_y));
        }

        // Draw curve using line segments (could use cubic Bezier for smoother curves)
        for i in 1..frequencies.len() {
            let x = self.config.freq_to_x(frequencies[i]);
            let y = self.config.db_to_y(response_db[i]);
            curve_path.line_to(Point::new(x, y));

            if self.config.fill_curve {
                fill_path.line_to(Point::new(x, y));
            }
        }

        // Close fill path
        if self.config.fill_curve {
            let last_x = self.config.freq_to_x(*frequencies.last().unwrap());
            fill_path.line_to(Point::new(last_x, rect.y1));
            fill_path.close_path();

            commands.push(EqPaintCommand::filled_path(
                fill_path,
                self.config.colors.curve_fill,
            ));
        }

        // Stroke the curve
        commands.push(EqPaintCommand::stroked_path(
            curve_path,
            self.config.colors.curve_stroke,
            2.0,
        ));
    }

    /// Render band control points.
    fn render_band_controls(
        &self,
        commands: &mut Vec<EqPaintCommand>,
        bands: &[BandParams; MAX_BANDS],
    ) {
        for (i, band) in bands.iter().enumerate() {
            if !band.used {
                continue;
            }

            let x = self.config.freq_to_x(band.frequency as f64);
            let y = self.config.db_to_y(band.gain as f64);

            let color = if band.enabled {
                self.config.colors.band_active
            } else {
                self.config.colors.band_inactive
            };

            // Draw control point
            commands.push(EqPaintCommand::circle(
                Point::new(x, y),
                self.config.control_radius,
                color.with_alpha(0.3),
                color,
                2.0,
            ));

            // Draw band number
            commands.push(EqPaintCommand::Text {
                text: format!("{}", i + 1),
                position: Point::new(x, y + 4.0),
                font_size: 10.0,
                color: EqColor::new(0.0, 0.0, 0.0, 1.0),
                anchor: TextAnchor::Middle,
            });
        }
    }

    /// Render spectrum analyzer bars.
    pub fn render_spectrum(
        &self,
        commands: &mut Vec<EqPaintCommand>,
        magnitudes_db: &[f64],
        color: EqColor,
    ) {
        let rect = self.config.graph_rect();

        if magnitudes_db.is_empty() {
            return;
        }

        // Build spectrum path as filled area
        let mut path = BezPath::new();
        let num_points = magnitudes_db.len();

        // Map spectrum bins to frequencies (assuming linear spacing in the analyzer)
        let first_x = rect.x0;
        let first_db = magnitudes_db[0].clamp(-self.config.db_range, self.config.db_range);
        let first_y = self.config.db_to_y(first_db);

        path.move_to(Point::new(first_x, rect.y1));
        path.line_to(Point::new(first_x, first_y));

        for i in 1..num_points {
            let t = i as f64 / (num_points - 1) as f64;
            let x = rect.x0 + t * rect.width();
            let db = magnitudes_db[i].clamp(-self.config.db_range, self.config.db_range);
            let y = self.config.db_to_y(db);
            path.line_to(Point::new(x, y));
        }

        path.line_to(Point::new(rect.x1, rect.y1));
        path.close_path();

        commands.push(EqPaintCommand::filled_path(path, color));
    }

    /// Hit test: find which band control point is at the given position.
    #[must_use]
    pub fn hit_test(&self, x: f64, y: f64, bands: &[BandParams; MAX_BANDS]) -> Option<usize> {
        let hit_radius = self.config.control_radius + 4.0;
        let hit_radius_sq = hit_radius * hit_radius;

        for (i, band) in bands.iter().enumerate() {
            if !band.used {
                continue;
            }

            let band_x = self.config.freq_to_x(band.frequency as f64);
            let band_y = self.config.db_to_y(band.gain as f64);

            let dx = x - band_x;
            let dy = y - band_y;

            if dx * dx + dy * dy <= hit_radius_sq {
                return Some(i);
            }
        }

        None
    }

    /// Convert screen position to band parameters.
    #[must_use]
    pub fn screen_to_band_params(&self, x: f64, y: f64) -> (f32, f32) {
        let freq = self.config.x_to_freq(x).clamp(10.0, 30000.0) as f32;
        let gain = self.config.y_to_db(y).clamp(-30.0, 30.0) as f32;
        (freq, gain)
    }

    /// Get the config.
    #[must_use]
    pub const fn config(&self) -> &EqGraphConfig {
        &self.config
    }
}

/// Convert paint commands to SVG string.
#[must_use]
pub fn commands_to_svg(commands: &[EqPaintCommand], width: f64, height: f64) -> String {
    let mut svg = format!(
        r#"<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{height}" viewBox="0 0 {width} {height}">"#
    );

    for cmd in commands {
        match cmd {
            EqPaintCommand::Fill { path, color } => {
                let d = path_to_svg_d(path);
                svg.push_str(&format!(
                    r#"<path d="{d}" fill="{}" fill-rule="nonzero"/>"#,
                    color.to_css()
                ));
            }
            EqPaintCommand::Stroke {
                path,
                color,
                width,
                line_cap,
                line_join,
            } => {
                let d = path_to_svg_d(path);
                let cap = match line_cap {
                    LineCap::Butt => "butt",
                    LineCap::Round => "round",
                    LineCap::Square => "square",
                };
                let join = match line_join {
                    LineJoin::Miter => "miter",
                    LineJoin::Round => "round",
                    LineJoin::Bevel => "bevel",
                };
                svg.push_str(&format!(
                    r#"<path d="{d}" fill="none" stroke="{}" stroke-width="{width}" stroke-linecap="{cap}" stroke-linejoin="{join}"/>"#,
                    color.to_css()
                ));
            }
            EqPaintCommand::Line {
                start,
                end,
                color,
                width,
            } => {
                svg.push_str(&format!(
                    r#"<line x1="{:.2}" y1="{:.2}" x2="{:.2}" y2="{:.2}" stroke="{}" stroke-width="{width}"/>"#,
                    start.x, start.y, end.x, end.y, color.to_css()
                ));
            }
            EqPaintCommand::Rect {
                rect,
                fill,
                stroke,
                stroke_width,
            } => {
                let fill_str = fill.map_or("none".to_string(), |c| c.to_css());
                let stroke_str = stroke.map_or("none".to_string(), |c| c.to_css());
                svg.push_str(&format!(
                    r#"<rect x="{:.2}" y="{:.2}" width="{:.2}" height="{:.2}" fill="{fill_str}" stroke="{stroke_str}" stroke-width="{stroke_width}"/>"#,
                    rect.x0, rect.y0, rect.width(), rect.height()
                ));
            }
            EqPaintCommand::Circle {
                center,
                radius,
                fill,
                stroke,
                stroke_width,
            } => {
                let fill_str = fill.map_or("none".to_string(), |c| c.to_css());
                let stroke_str = stroke.map_or("none".to_string(), |c| c.to_css());
                svg.push_str(&format!(
                    r#"<circle cx="{:.2}" cy="{:.2}" r="{radius}" fill="{fill_str}" stroke="{stroke_str}" stroke-width="{stroke_width}"/>"#,
                    center.x, center.y
                ));
            }
            EqPaintCommand::Text {
                text,
                position,
                font_size,
                color,
                anchor,
            } => {
                let anchor_str = match anchor {
                    TextAnchor::Start => "start",
                    TextAnchor::Middle => "middle",
                    TextAnchor::End => "end",
                };
                svg.push_str(&format!(
                    r#"<text x="{:.2}" y="{:.2}" font-size="{font_size}" fill="{}" text-anchor="{anchor_str}" dominant-baseline="middle">{}</text>"#,
                    position.x, position.y, color.to_css(), text
                ));
            }
        }
    }

    svg.push_str("</svg>");
    svg
}

/// Convert a BezPath to SVG path data string.
fn path_to_svg_d(path: &BezPath) -> String {
    use kurbo::PathEl;

    let mut d = String::new();

    for el in path.elements() {
        match el {
            PathEl::MoveTo(pt) => {
                d.push_str(&format!("M{:.2} {:.2}", pt.x, pt.y));
            }
            PathEl::LineTo(pt) => {
                d.push_str(&format!("L{:.2} {:.2}", pt.x, pt.y));
            }
            PathEl::QuadTo(p1, p2) => {
                d.push_str(&format!(
                    "Q{:.2} {:.2} {:.2} {:.2}",
                    p1.x, p1.y, p2.x, p2.y
                ));
            }
            PathEl::CurveTo(p1, p2, p3) => {
                d.push_str(&format!(
                    "C{:.2} {:.2} {:.2} {:.2} {:.2} {:.2}",
                    p1.x, p1.y, p2.x, p2.y, p3.x, p3.y
                ));
            }
            PathEl::ClosePath => {
                d.push('Z');
            }
        }
    }

    d
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eq::params::BandShape;

    #[test]
    fn test_eq_color_to_css() {
        let opaque = EqColor::from_rgb8(255, 128, 0);
        assert_eq!(opaque.to_css(), "#ff8000");

        let transparent = EqColor::from_rgba8(255, 128, 0, 128);
        assert!(transparent.to_css().starts_with("rgba(255,128,0,"));
    }

    #[test]
    fn test_freq_to_x_inverse() {
        let config = EqGraphConfig::default();
        let freq = 1000.0;
        let x = config.freq_to_x(freq);
        let freq_back = config.x_to_freq(x);
        assert!((freq - freq_back).abs() < 1.0);
    }

    #[test]
    fn test_db_to_y_inverse() {
        let config = EqGraphConfig::default();
        let db = 6.0;
        let y = config.db_to_y(db);
        let db_back = config.y_to_db(y);
        assert!((db - db_back).abs() < 0.01);
    }

    #[test]
    fn test_render_empty_eq() {
        let renderer = EqGraphRenderer::default_for_size(800.0, 300.0);
        let params = EqParams::default();
        let commands = renderer.render(&params);

        // Should have at least background and grid
        assert!(!commands.is_empty());
    }

    #[test]
    fn test_render_with_bands() {
        let renderer = EqGraphRenderer::default_for_size(800.0, 300.0);
        let mut params = EqParams::default();

        // Enable a bell filter
        params.bands[0].used = true;
        params.bands[0].enabled = true;
        params.bands[0].frequency = 1000.0;
        params.bands[0].gain = 6.0;
        params.bands[0].q = 1.0;
        params.bands[0].shape = BandShape::Bell;

        let commands = renderer.render(&params);
        assert!(!commands.is_empty());

        // Should have a control point
        let has_circle = commands.iter().any(|c| matches!(c, EqPaintCommand::Circle { .. }));
        assert!(has_circle, "Should render band control point");
    }

    #[test]
    fn test_hit_test() {
        let renderer = EqGraphRenderer::default_for_size(800.0, 300.0);
        let config = renderer.config();

        let mut params = EqParams::default();
        params.bands[0].used = true;
        params.bands[0].frequency = 1000.0;
        params.bands[0].gain = 0.0;

        // Test hit on band control
        let band_x = config.freq_to_x(1000.0);
        let band_y = config.db_to_y(0.0);
        let hit = renderer.hit_test(band_x, band_y, &params.bands);
        assert_eq!(hit, Some(0));

        // Test miss
        let miss = renderer.hit_test(0.0, 0.0, &params.bands);
        assert_eq!(miss, None);
    }

    #[test]
    fn test_svg_export() {
        let renderer = EqGraphRenderer::default_for_size(400.0, 150.0);
        let params = EqParams::default();
        let commands = renderer.render(&params);
        let svg = commands_to_svg(&commands, 400.0, 150.0);

        assert!(svg.starts_with("<svg"));
        assert!(svg.ends_with("</svg>"));
        assert!(svg.contains("xmlns"));
    }
}
