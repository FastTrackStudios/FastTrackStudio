//! FastTrackStudio Which-Key Tree Definitions
//!
//! Declarative prefix trees for the FTS preset, defining multi-key chord
//! sequences for visibility management, FX routing, and automation control.
//!
//! These trees are registered when the FTS keybind preset is activated and
//! cleared when switching to another preset.

use crate::input::keybinds::which_key::WhichKeyEntry;

/// Returns all FTS which-key prefix trees.
///
/// Each tuple is `(prefix_key, label, entries)`.
pub fn fts_which_key_trees() -> Vec<(String, String, Vec<WhichKeyEntry>)> {
    vec![
        // ── v → Visibility Manager ──────────────────────────────────
        (
            "v".into(),
            "Visibility Manager".into(),
            vec![
                WhichKeyEntry::leaf("d", "Drums", "FTS_VISIBILITY_MANAGER_TOGGLE_DRUMS"),
                WhichKeyEntry::leaf(
                    "p",
                    "Percussion",
                    "FTS_VISIBILITY_MANAGER_TOGGLE_PERCUSSION",
                ),
                WhichKeyEntry::leaf("b", "Bass", "FTS_VISIBILITY_MANAGER_TOGGLE_BASS"),
                WhichKeyEntry::leaf("g", "Guitars", "FTS_VISIBILITY_MANAGER_TOGGLE_GUITARS"),
                WhichKeyEntry::leaf("k", "Keys", "FTS_VISIBILITY_MANAGER_TOGGLE_KEYS"),
                WhichKeyEntry::leaf("s", "Synths", "FTS_VISIBILITY_MANAGER_TOGGLE_SYNTHS"),
                WhichKeyEntry::leaf("h", "Horns", "FTS_VISIBILITY_MANAGER_TOGGLE_HORNS"),
                WhichKeyEntry::leaf("v", "Vocals", "FTS_VISIBILITY_MANAGER_TOGGLE_VOCALS"),
                WhichKeyEntry::leaf("c", "Choir", "FTS_VISIBILITY_MANAGER_TOGGLE_CHOIR"),
                WhichKeyEntry::leaf("o", "Orchestra", "FTS_VISIBILITY_MANAGER_TOGGLE_ORCHESTRA"),
                WhichKeyEntry::leaf("x", "SFX", "FTS_VISIBILITY_MANAGER_TOGGLE_SFX"),
                WhichKeyEntry::leaf("r", "Reference", "FTS_VISIBILITY_MANAGER_TOGGLE_REFERENCE"),
                WhichKeyEntry::leaf("0", "Show All", "FTS_VISIBILITY_MANAGER_SHOW_ALL"),
                WhichKeyEntry::leaf("9", "Hide All", "FTS_VISIBILITY_MANAGER_HIDE_ALL"),
            ],
        ),
        // ── f → Semantic FX Manager ─────────────────────────────────
        (
            "f".into(),
            "Semantic FX Manager".into(),
            vec![
                // ff = FX search
                WhichKeyEntry::leaf("f", "FX Search", "fx:search"),
                // fe → EQ / Rescue
                WhichKeyEntry::branch(
                    "e",
                    "EQ / Relational",
                    vec![
                        WhichKeyEntry::leaf("r", "Rescue EQ", "fx:rescue_eq"),
                        WhichKeyEntry::leaf("q", "Relational EQ", "fx:relational_eq"),
                    ],
                ),
                // ft → Treatment / Rescue
                WhichKeyEntry::branch(
                    "t",
                    "Treatment",
                    vec![
                        WhichKeyEntry::leaf("p", "De-Clip", "fx:declip"),
                        WhichKeyEntry::leaf("c", "De-Click", "fx:declick"),
                        WhichKeyEntry::leaf("t", "Tuning", "fx:tuning"),
                        WhichKeyEntry::leaf("a", "Time-Alignment", "fx:time_alignment"),
                        WhichKeyEntry::leaf("l", "Live Tuner", "fx:live_tuner"),
                    ],
                ),
                // fg → Gate
                WhichKeyEntry::branch(
                    "g",
                    "Gate",
                    vec![WhichKeyEntry::leaf("g", "Gate", "fx:gate")],
                ),
                // fc → Compression / Tone-Shaping
                WhichKeyEntry::branch(
                    "c",
                    "Compression",
                    vec![
                        WhichKeyEntry::leaf("c", "Control Compressor", "fx:control_compressor"),
                        WhichKeyEntry::leaf("t", "Tonal Compressor", "fx:tonal_compressor"),
                    ],
                ),
                // fs → Saturation & Separation
                WhichKeyEntry::branch(
                    "s",
                    "Saturation / Separation",
                    vec![
                        WhichKeyEntry::leaf("s", "Saturation", "fx:saturation"),
                        WhichKeyEntry::leaf("c", "Clipper", "fx:clipper"),
                    ],
                ),
                // fp → Parallel Processing
                WhichKeyEntry::branch(
                    "p",
                    "Parallel Processing",
                    vec![
                        WhichKeyEntry::leaf("f", "Parallel Filters", "fx:parallel_filters"),
                        WhichKeyEntry::leaf("c", "Parallel Compressor", "fx:parallel_compressor"),
                        WhichKeyEntry::leaf("p", "Panning Plugin", "fx:panning"),
                    ],
                ),
                // fd → De-essing / Polish
                WhichKeyEntry::branch(
                    "d",
                    "De-essing / Polish",
                    vec![
                        WhichKeyEntry::leaf("s", "De-Esser", "fx:deesser"),
                        WhichKeyEntry::leaf("r", "Resonance Suppressor", "fx:resonance_suppressor"),
                    ],
                ),
                // fr = Reverb (direct leaf under f)
                WhichKeyEntry::leaf("r", "Reverb", "fx:reverb"),
            ],
        ),
        // ── a → Automation ──────────────────────────────────────────
        (
            "a".into(),
            "Automation".into(),
            vec![
                WhichKeyEntry::leaf("c", "Trim/Read", "40878"),
                WhichKeyEntry::leaf("r", "Read", "40879"),
                WhichKeyEntry::leaf("g", "No Override (per-track)", "40876"),
                WhichKeyEntry::leaf("t", "Touch", "40880"),
                WhichKeyEntry::leaf("l", "Latch", "40881"),
                WhichKeyEntry::leaf("p", "Latch Preview", "42022"),
                WhichKeyEntry::leaf("w", "Write", "40882"),
                WhichKeyEntry::leaf("b", "Toggle Bypass All", "40908"),
                WhichKeyEntry::leaf("a", "Show Active Envelopes", "40888"),
                WhichKeyEntry::leaf("s", "Show Active (All Tracks)", "40926"),
                WhichKeyEntry::leaf("h", "Hide All Envelopes", "41150"),
            ],
        ),
        // ── z → Zoom ──────────────────────────────────────────────────
        (
            "z".into(),
            "Zoom".into(),
            vec![
                WhichKeyEntry::leaf("z", "Zoom Project", "40295"),
                WhichKeyEntry::leaf("p", "Zoom Project", "40295"),
                WhichKeyEntry::leaf("s", "Zoom Selection", "40031"),
                WhichKeyEntry::leaf("t", "Zoom Tracks", "40111"),
                WhichKeyEntry::leaf("r", "Zoom Reset", "40112"),
            ],
        ),
        // ── o → Organize ────────────────────────────────────────────
        (
            "o".into(),
            "Organize".into(),
            vec![WhichKeyEntry::branch(
                "s",
                "Sort",
                vec![
                    WhichKeyEntry::leaf("a", "Sort All", "FTS_DYNAMIC_TEMPLATE_SORT_ALL"),
                    WhichKeyEntry::leaf("s", "Sort Selected", "FTS_DYNAMIC_TEMPLATE_SORT_SELECTED"),
                ],
            )],
        ),
    ]
}
