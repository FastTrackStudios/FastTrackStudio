//! The arrange-view theme context: ruler, grid, lanes, items, cursors.
//!
//! Unlike the strip contexts (`tcp.*`/`mcp.*`), REAPER's arrange view and
//! time ruler carry **no WALTER layout** — they are themed entirely through
//! `.ReaperTheme` palette keys (`col_tl_*` ruler, `col_gridlines*` grid,
//! `col_tr1/2_*` row pairs, `col_mi_*` media items, cursor + marker/region
//! keys). [`ArrangeTheme`] is the typed mirror of that vocabulary, with FTS
//! dark defaults so a bare theme still renders.

use super::theme::Color;

/// Arrange + ruler colours — the `.ReaperTheme` arrange vocabulary.
/// Importers fill these from the palette (drawmode alphas pre-applied);
/// every field has a sensible FTS dark default.
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct ArrangeTheme {
    // ── backdrop ──
    /// `col_arrangebg` — the timeline backdrop behind the lanes.
    pub bg: Color,
    /// `col_tracklistbg` — the empty area below the last track.
    pub empty_bg: Color,
    /// `col_tr1_bg` / `col_tr2_bg` — alternating lane backgrounds
    /// (REAPER alternates per track index).
    pub row_bg: [Color; 2],
    /// `col_tr1_divline` / `col_tr2_divline` — the lane divider lines.
    pub row_divider: [Color; 2],

    // ── grid (drawmode alpha applied) ──
    /// `col_gridlines2` (+ `col_gridlines2dm`) — start-of-measure lines.
    pub grid_measure: Color,
    /// `col_gridlines3` (+ `col_gridlines3dm`) — start-of-beat lines.
    pub grid_beat: Color,
    /// `col_gridlines` (+ `col_gridlines1dm`) — in-between-beat lines.
    pub grid_sub: Color,

    // ── time ruler ──
    /// `col_tl_bg` — ruler background.
    pub ruler_bg: Color,
    /// `col_tl_fg` — ruler text + primary tick marks.
    pub ruler_fg: Color,
    /// `col_tl_fg2` — secondary tick marks / minor labels.
    pub ruler_fg2: Color,
    /// `col_tl_bgsel` — the time-selection band in the ruler.
    pub ruler_sel_bg: Color,
    /// `col_tl_bgsel2` — the ruler background inside loop points.
    pub ruler_loop_bg: Color,

    // ── cursors ──
    /// `col_cursor` — the edit cursor line.
    pub edit_cursor: Color,
    /// `playcursor_color` (+ `playcursor_drawmode`) — the play cursor line.
    pub play_cursor: Color,

    // ── media items ──
    /// `col_mi_bg` — item body fallback (uncolored items).
    pub item_bg: Color,
    /// `col_mi_label` — item label text.
    pub item_label: Color,
    /// `col_mi_label_sel` — selected-item label text.
    pub item_label_sel: Color,
    /// `col_peaksedge` — peak outline; doubles as the item border.
    pub item_edge: Color,
    /// `col_tr1_peaks` / `col_tr2_peaks` — waveform peak fills.
    pub peaks: [Color; 2],
    /// `col_tr1_itembgsel` / `col_tr2_itembgsel` — selected item bodies.
    pub item_bg_sel: [Color; 2],

    // ── markers / regions (ruler lanes) ──
    /// `marker` — marker flag fill.
    pub marker: Color,
    /// `marker_lane_bg` / `marker_lane_text`.
    pub marker_lane_bg: Color,
    pub marker_lane_text: Color,
    /// `region` — region band fill.
    pub region: Color,
    /// `region_lane_bg` / `region_lane_text`.
    pub region_lane_bg: Color,
    pub region_lane_text: Color,

    // ── selections ──
    /// `areasel_fill` (+ `areasel_drawmode`) — razor/area selection fill.
    pub sel_fill: Color,
    /// `marquee_fill` (+ `marquee_drawmode`) — marquee selection fill.
    pub marquee_fill: Color,
}

impl ArrangeTheme {
    /// FTS dark defaults (match `Theme::dark`'s token family).
    pub fn fts_default() -> Self {
        let c = |hex: u32| {
            Color::rgba(
                ((hex >> 16) & 0xff) as u8,
                ((hex >> 8) & 0xff) as u8,
                (hex & 0xff) as u8,
                255,
            )
        };
        Self {
            bg: c(0x0a0a0c),
            empty_bg: c(0x09090b),
            row_bg: [c(0x121215), c(0x101013)],
            row_divider: [c(0x1d1d21), c(0x1d1d21)],
            grid_measure: Color::rgba(255, 255, 255, 36),
            grid_beat: Color::rgba(255, 255, 255, 20),
            grid_sub: Color::rgba(255, 255, 255, 10),
            ruler_bg: c(0x18181b),
            ruler_fg: c(0xa1a1aa),
            ruler_fg2: c(0x71717a),
            ruler_sel_bg: Color::rgba(56, 189, 248, 64),
            ruler_loop_bg: c(0x27272a),
            edit_cursor: c(0x38bdf8),
            play_cursor: Color::rgba(255, 255, 255, 160),
            item_bg: c(0x3f3f46),
            item_label: c(0x0c0c0f),
            item_label_sel: c(0xffffff),
            item_edge: c(0x0a0a0c),
            peaks: [c(0x18181b), c(0x18181b)],
            item_bg_sel: [c(0x52525b), c(0x52525b)],
            marker: c(0xef4444),
            marker_lane_bg: c(0x18181b),
            marker_lane_text: c(0xa1a1aa),
            region: c(0x22c55e),
            region_lane_bg: c(0x18181b),
            region_lane_text: c(0xa1a1aa),
            sel_fill: Color::rgba(56, 189, 248, 40),
            marquee_fill: Color::rgba(56, 189, 248, 40),
        }
    }
}

impl Default for ArrangeTheme {
    fn default() -> Self {
        Self::fts_default()
    }
}
