//! Layout integration tests.
//!
//! These tests verify the layout engine produces correct results,
//! following patterns from MuseScore's layout test suite.

use engraver::layout::context::{LayoutConfiguration, LayoutContext};
use engraver::layout::orchestrator::{layout_score, LayoutEngine, LayoutEngineConfig};
use engraver::layout::shape::Shape;
use engraver::layout::skyline::{Skyline, SkylineElement, SkylineLine};
use engraver::layout::autoplace::{Autoplace, AutoplaceConfig, AutoplaceState};
use engraver::layout::tlayout::note::{layout_note, NoteDuration, NoteParams, Accidental};
use engraver::layout::tlayout::rest::{layout_rest, RestDuration, RestParams};
use engraver::layout::tlayout::chord::{layout_chord, ChordParams, ChordNote, StemDirection};
use engraver::layout::tlayout::clef::{layout_clef, ClefParams, ClefType};
use engraver::layout::tlayout::timesig::{layout_timesig, TimeSigParams, TimeSigType};
use engraver::layout::tlayout::keysig::{layout_keysig, KeySigParams, KeySigType};
use engraver::model::{
    Duration, DurationKind, Measure, MusicElement, Note, Octave, Part, PartId, Pitch, PitchClass,
    Rest, Score, Voice,
};
use engraver::scene::traverse::NodeIterator;
use engraver::style::MStyle;
use kurbo::Rect;

// ============================================================================
// Test Helpers
// ============================================================================

/// Create a default layout context for testing.
fn test_context() -> LayoutContext<'static> {
    let style = Box::leak(Box::new(MStyle::default()));
    LayoutContext::minimal(style)
}

/// Create a simple score with one measure containing quarter notes.
fn create_simple_score() -> Score {
    let mut score = Score::default();

    let mut part = Part::new(PartId(0), "Test Part");
    let mut measure = Measure::new(1);

    // Add a voice with notes
    let mut voice = Voice::default();

    // C4 quarter note
    let note1 = Note::new(Pitch::new(PitchClass::C, Octave::new(4)), Duration::QUARTER);
    voice.elements.push(MusicElement::Note(note1));

    // D4 quarter note
    let note2 = Note::new(Pitch::new(PitchClass::D, Octave::new(4)), Duration::QUARTER);
    voice.elements.push(MusicElement::Note(note2));

    // E4 quarter note
    let note3 = Note::new(Pitch::new(PitchClass::E, Octave::new(4)), Duration::QUARTER);
    voice.elements.push(MusicElement::Note(note3));

    // F4 quarter note
    let note4 = Note::new(Pitch::new(PitchClass::F, Octave::new(4)), Duration::QUARTER);
    voice.elements.push(MusicElement::Note(note4));

    measure.voices.push(voice);
    part.add_measure(measure);
    score.parts.push(part);

    score
}

/// Create a score with rests.
fn create_rest_score() -> Score {
    let mut score = Score::default();

    let mut part = Part::new(PartId(0), "Test Part");
    let mut measure = Measure::new(1);

    let mut voice = Voice::default();

    // Quarter rest
    voice.elements.push(MusicElement::Rest(Rest::new(Duration::QUARTER)));

    // Half rest
    voice.elements.push(MusicElement::Rest(Rest::new(Duration::HALF)));

    // Quarter rest
    voice.elements.push(MusicElement::Rest(Rest::new(Duration::QUARTER)));

    measure.voices.push(voice);
    part.add_measure(measure);
    score.parts.push(part);

    score
}

/// Create a score with multiple measures.
fn create_multi_measure_score(measure_count: usize) -> Score {
    let mut score = Score::default();

    let mut part = Part::new(PartId(0), "Test Part");

    for i in 0..measure_count {
        let mut measure = Measure::new(i as u32 + 1);
        let mut voice = Voice::default();

        // Add four quarter notes per measure
        let pitch_classes = [PitchClass::C, PitchClass::D, PitchClass::E, PitchClass::F];
        for j in 0..4 {
            let pitch = Pitch::new(pitch_classes[j], Octave::new(4));
            let note = Note::new(pitch, Duration::QUARTER);
            voice.elements.push(MusicElement::Note(note));
        }

        measure.voices.push(voice);
        part.add_measure(measure);
    }

    score.parts.push(part);
    score
}

// ============================================================================
// Layout Elements Tests (from MuseScore's layoutelements_tests.cpp)
// ============================================================================

/// Test that all elements have valid bounding boxes after layout.
/// Based on MuseScore's tstLayoutAll test.
#[test]
fn test_layout_elements_have_valid_bounds() {
    let ctx = test_context();

    // Test note layout
    let note_params = NoteParams {
        id: 1,
        duration: NoteDuration::Quarter,
        line: 0,
        ..Default::default()
    };
    let (layout, _node) = layout_note(&note_params, &ctx);
    assert!(!layout.bbox.is_zero_area(), "Note should have valid bounding box");
    assert!(layout.bbox.width() > 0.0, "Note bbox width should be positive");
    assert!(layout.bbox.height() > 0.0, "Note bbox height should be positive");

    // Test rest layout
    let rest_params = RestParams {
        id: 2,
        duration: RestDuration::Quarter,
        dots: 0,
        line: 0,
    };
    let (layout, _node) = layout_rest(&rest_params, &ctx);
    assert!(!layout.bbox.is_zero_area(), "Rest should have valid bounding box");

    // Test clef layout
    let clef_params = ClefParams {
        id: 3,
        clef_type: ClefType::Treble,
        is_change: false,
        ..Default::default()
    };
    let (layout, _node) = layout_clef(&clef_params, &ctx);
    assert!(!layout.bbox.is_zero_area(), "Clef should have valid bounding box");

    // Test time signature layout
    let ts_params = TimeSigParams {
        id: 4,
        sig_type: TimeSigType::Numeric { numerator: 4, denominator: 4 },
        large: false,
    };
    let (layout, _node) = layout_timesig(&ts_params, &ctx);
    assert!(!layout.bbox.is_zero_area(), "Time signature should have valid bounding box");
}

/// Test that all note durations produce valid layouts.
#[test]
fn test_all_note_durations_have_valid_layout() {
    let ctx = test_context();

    let durations = [
        NoteDuration::Whole,
        NoteDuration::Half,
        NoteDuration::Quarter,
        NoteDuration::Eighth,
        NoteDuration::Sixteenth,
        NoteDuration::ThirtySecond,
        NoteDuration::SixtyFourth,
    ];

    for duration in durations {
        let params = NoteParams {
            id: 1,
            duration,
            line: 0,
            ..Default::default()
        };
        let (layout, node) = layout_note(&params, &ctx);

        assert!(
            !layout.bbox.is_zero_area(),
            "Note with duration {:?} should have valid bounding box",
            duration
        );
        assert!(
            !node.commands.is_empty(),
            "Note with duration {:?} should have paint commands",
            duration
        );
    }
}

/// Test that all rest durations produce valid layouts.
#[test]
fn test_all_rest_durations_have_valid_layout() {
    let ctx = test_context();

    let durations = [
        RestDuration::Whole,
        RestDuration::Half,
        RestDuration::Quarter,
        RestDuration::Eighth,
        RestDuration::Sixteenth,
        RestDuration::ThirtySecond,
        RestDuration::SixtyFourth,
    ];

    for duration in durations {
        let params = RestParams {
            id: 1,
            duration,
            dots: 0,
            line: 0,
        };
        let (layout, node) = layout_rest(&params, &ctx);

        assert!(
            !layout.bbox.is_zero_area(),
            "Rest with duration {:?} should have valid bounding box",
            duration
        );
        assert!(
            !node.commands.is_empty(),
            "Rest with duration {:?} should have paint commands",
            duration
        );
    }
}

/// Test that all clef types produce valid layouts.
#[test]
fn test_all_clef_types_have_valid_layout() {
    let ctx = test_context();

    let clef_types = [
        ClefType::Treble,
        ClefType::Bass,
        ClefType::Alto,
        ClefType::Tenor,
    ];

    for clef_type in clef_types {
        let params = ClefParams {
            id: 1,
            clef_type,
            is_change: false,
            ..Default::default()
        };
        let (layout, node) = layout_clef(&params, &ctx);

        assert!(
            !layout.bbox.is_zero_area(),
            "Clef type {:?} should have valid bounding box",
            clef_type
        );
        assert!(
            !node.commands.is_empty(),
            "Clef type {:?} should have paint commands",
            clef_type
        );
    }
}

/// Test that key signatures with different accidental counts produce valid layouts.
#[test]
fn test_key_signatures_have_valid_layout() {
    let ctx = test_context();

    // Test various key signatures from 7 flats to 7 sharps
    for fifths in -7i8..=7 {
        let params = KeySigParams {
            id: 1,
            key: KeySigType::Standard(fifths),
            ..Default::default()
        };
        let (layout, _node) = layout_keysig(&params, &ctx);

        // Key signatures can have zero area if they have no accidentals (C major)
        if fifths != 0 {
            assert!(
                !layout.bbox.is_zero_area(),
                "Key signature with {} fifths should have valid bounding box",
                fifths
            );
        }
    }
}

// ============================================================================
// Note Position Tests (from MuseScore's note_tests.cpp patterns)
// ============================================================================

/// Test that notes on different staff lines have different Y positions.
#[test]
fn test_note_staff_positions() {
    let ctx = test_context();
    let spatium = ctx.spatium();

    // Note on line 0 (middle line)
    let params_line_0 = NoteParams {
        id: 1,
        line: 0,
        ..Default::default()
    };
    let (layout_0, _) = layout_note(&params_line_0, &ctx);

    // Note on line 2 (above middle)
    let params_line_2 = NoteParams {
        id: 2,
        line: 2,
        ..Default::default()
    };
    let (layout_2, _) = layout_note(&params_line_2, &ctx);

    // Note on line -2 (below middle)
    let params_line_neg2 = NoteParams {
        id: 3,
        line: -2,
        ..Default::default()
    };
    let (layout_neg2, _) = layout_note(&params_line_neg2, &ctx);

    // Notes on higher lines should have lower Y values (Y-down coordinate system)
    // Each line difference is half a spatium
    let y_diff_expected = spatium; // 2 lines * 0.5 spatium per line

    let y_0 = layout_0.position.y;
    let y_2 = layout_2.position.y;
    let y_neg2 = layout_neg2.position.y;

    // Line 2 is above line 0, so y_2 should be more negative
    assert!(
        (y_2 - y_0).abs() > 0.01,
        "Notes on different lines should have different Y positions"
    );
}

/// Test that notes with accidentals are wider.
#[test]
fn test_note_with_accidental_is_wider() {
    let ctx = test_context();

    let params_no_acc = NoteParams {
        id: 1,
        line: 0,
        accidental: Accidental::None,
        ..Default::default()
    };
    let (layout_no_acc, _) = layout_note(&params_no_acc, &ctx);

    let params_with_acc = NoteParams {
        id: 2,
        line: 0,
        accidental: Accidental::Sharp,
        ..Default::default()
    };
    let (layout_with_acc, _) = layout_note(&params_with_acc, &ctx);

    assert!(
        layout_with_acc.bbox.width() > layout_no_acc.bbox.width(),
        "Note with accidental should be wider than note without"
    );
}

/// Test that dotted notes are wider than undotted notes.
#[test]
fn test_dotted_note_is_wider() {
    let ctx = test_context();

    let params_no_dot = NoteParams {
        id: 1,
        line: 0,
        dots: 0,
        ..Default::default()
    };
    let (layout_no_dot, _) = layout_note(&params_no_dot, &ctx);

    let params_dotted = NoteParams {
        id: 2,
        line: 0,
        dots: 1,
        ..Default::default()
    };
    let (layout_dotted, _) = layout_note(&params_dotted, &ctx);

    assert!(
        layout_dotted.bbox.width() > layout_no_dot.bbox.width(),
        "Dotted note should be wider than undotted note"
    );
}

// ============================================================================
// Chord Layout Tests
// ============================================================================

/// Test that chords with multiple notes have correct layout.
#[test]
fn test_chord_layout() {
    let ctx = test_context();

    // Single note chord
    let single_params = ChordParams {
        id: 1,
        duration: NoteDuration::Quarter,
        notes: vec![ChordNote {
            line: 0,
            accidental: Accidental::None,
            tie: false,
        }],
        ..Default::default()
    };
    let (single_layout, _) = layout_chord(&single_params, &ctx);

    // Three note chord (triad)
    let triad_params = ChordParams {
        id: 2,
        duration: NoteDuration::Quarter,
        notes: vec![
            ChordNote { line: 0, accidental: Accidental::None, tie: false },
            ChordNote { line: 2, accidental: Accidental::None, tie: false },
            ChordNote { line: 4, accidental: Accidental::None, tie: false },
        ],
        ..Default::default()
    };
    let (triad_layout, _) = layout_chord(&triad_params, &ctx);

    // Chord with multiple notes should be taller (span more staff lines)
    assert!(
        triad_layout.bbox.height() > single_layout.bbox.height(),
        "Chord with multiple notes should be taller"
    );
}

/// Test that chord stem direction affects layout.
#[test]
fn test_chord_stem_direction() {
    let ctx = test_context();

    let notes = vec![ChordNote {
        line: 0,
        accidental: Accidental::None,
        tie: false,
    }];

    let params_up = ChordParams {
        id: 1,
        duration: NoteDuration::Quarter,
        notes: notes.clone(),
        stem_direction: StemDirection::Up,
        ..Default::default()
    };
    let (layout_up, _) = layout_chord(&params_up, &ctx);

    let params_down = ChordParams {
        id: 2,
        duration: NoteDuration::Quarter,
        notes,
        stem_direction: StemDirection::Down,
        ..Default::default()
    };
    let (layout_down, _) = layout_chord(&params_down, &ctx);

    // Both should have valid bounding boxes
    assert!(!layout_up.bbox.is_zero_area());
    assert!(!layout_down.bbox.is_zero_area());
}

// ============================================================================
// Skyline/Autoplace Tests
// ============================================================================

/// Test that skyline correctly tracks element positions.
#[test]
fn test_skyline_basic() {
    let mut skyline = SkylineLine::north();

    // Add a rectangle
    skyline.add(Rect::new(0.0, -20.0, 100.0, 0.0));

    assert_eq!(skyline.len(), 1);
    assert!(!skyline.is_empty());

    // Check that max_at returns correct value
    let max = skyline.max_at(50.0);
    assert_eq!(max, Some(-20.0));
}

/// Test skyline with multiple elements.
#[test]
fn test_skyline_multiple_elements() {
    let mut skyline = SkylineLine::north();

    // Add two rectangles at different positions
    skyline.add(Rect::new(0.0, -20.0, 50.0, 0.0));
    skyline.add(Rect::new(50.0, -30.0, 100.0, -10.0));

    assert_eq!(skyline.len(), 2);

    // At x=25, only first rect covers it
    assert_eq!(skyline.max_at(25.0), Some(-20.0));

    // At x=75, only second rect covers it
    assert_eq!(skyline.max_at(75.0), Some(-30.0));
}

/// Test autoplace collision avoidance.
#[test]
fn test_autoplace_no_collision() {
    let autoplace = Autoplace::new();
    let shape = Shape::from_rect(Rect::new(0.0, -10.0, 50.0, 0.0));
    let skyline = SkylineLine::north();

    let result = autoplace.autoplace_element(&shape, &skyline, true, 10.0);

    // With empty skyline, no movement needed
    assert!(!result.moved);
    assert_eq!(result.offset_y, 0.0);
}

/// Test autoplace with collision.
#[test]
fn test_autoplace_with_collision() {
    let autoplace = Autoplace::new();
    let shape = Shape::from_rect(Rect::new(10.0, -15.0, 40.0, -5.0));

    let mut skyline = SkylineLine::north();
    skyline.add(Rect::new(0.0, -20.0, 50.0, -10.0));

    let result = autoplace.autoplace_element(&shape, &skyline, true, 10.0);

    // Shape should move up to avoid skyline
    assert!(result.moved);
    assert!(result.offset_y < 0.0, "Element should move up (negative Y)");
}

// ============================================================================
// Full Score Layout Tests
// ============================================================================

/// Test that layout_score produces a valid scene graph.
#[test]
fn test_layout_score_produces_scene() {
    let score = create_simple_score();
    let style = MStyle::default();

    let engine = LayoutEngine::new(&style);
    let result = engine.layout_score(&score);

    // Should have at least one system
    assert!(result.system_count > 0, "Should have at least one system");

    // Scene should have children (systems)
    assert!(
        !result.scene.children.is_empty(),
        "Scene should have children"
    );

    // Total dimensions should be positive
    assert!(result.total_width > 0.0, "Total width should be positive");
    assert!(result.total_height > 0.0, "Total height should be positive");
}

/// Test that score with multiple measures produces multiple measure nodes.
#[test]
fn test_multi_measure_layout() {
    let score = create_multi_measure_score(4);
    let style = MStyle::default();

    let engine = LayoutEngine::new(&style);
    let result = engine.layout_score(&score);

    assert_eq!(result.measure_count, 4, "Should have 4 measures");
}

/// Test that rest-only measure layouts correctly.
#[test]
fn test_rest_only_measure_layout() {
    let score = create_rest_score();
    let style = MStyle::default();

    let engine = LayoutEngine::new(&style);
    let result = engine.layout_score(&score);

    assert!(result.system_count > 0);
}

// ============================================================================
// Scene Graph Tests
// ============================================================================

/// Test that scene nodes have semantic IDs.
#[test]
fn test_scene_nodes_have_semantic_ids() {
    let ctx = test_context();

    let params = NoteParams {
        id: 42,
        line: 0,
        ..Default::default()
    };
    let (_, node) = layout_note(&params, &ctx);

    assert!(node.id.is_some(), "Note node should have semantic ID");
    let id = node.id.as_ref().unwrap();
    assert_eq!(id.id, 42, "Semantic ID should match");
}

/// Test that scene can be traversed.
#[test]
fn test_scene_traversal() {
    let score = create_simple_score();
    let style = MStyle::default();

    let engine = LayoutEngine::new(&style);
    let result = engine.layout_score(&score);

    // Count total nodes in scene
    let mut node_count = 0;
    for _node in NodeIterator::new(&result.scene) {
        node_count += 1;
    }

    assert!(node_count > 1, "Scene should have multiple nodes");
}

// ============================================================================
// Edge Cases
// ============================================================================

/// Test layout with empty score.
#[test]
fn test_empty_score_layout() {
    let score = Score::default();
    let style = MStyle::default();

    let engine = LayoutEngine::new(&style);
    let result = engine.layout_score(&score);

    // Should handle empty score gracefully
    assert_eq!(result.measure_count, 0);
}

/// Test note on extreme ledger lines.
#[test]
fn test_ledger_line_notes() {
    let ctx = test_context();

    // High ledger line note
    let params_high = NoteParams {
        id: 1,
        line: 10, // Well above staff
        ledger_lines: true,
        ..Default::default()
    };
    let (layout_high, node_high) = layout_note(&params_high, &ctx);

    // Low ledger line note
    let params_low = NoteParams {
        id: 2,
        line: -10, // Well below staff
        ledger_lines: true,
        ..Default::default()
    };
    let (layout_low, node_low) = layout_note(&params_low, &ctx);

    // Both should produce valid layout
    assert!(!layout_high.bbox.is_zero_area());
    assert!(!layout_low.bbox.is_zero_area());

    // Should have ledger line paint commands
    assert!(node_high.commands.len() > 1, "High note should have ledger lines");
    assert!(node_low.commands.len() > 1, "Low note should have ledger lines");
}

// ============================================================================
// Lyrics Tests
// ============================================================================

use engraver::layout::tlayout::lyrics::{
    layout_lyrics, layout_lyrics_dash, layout_melisma, LyricsParams, LyricsPlacement, SyllabicType,
};

/// Test basic lyrics layout.
#[test]
fn test_lyrics_basic_layout() {
    let ctx = test_context();
    let params = LyricsParams {
        id: 1,
        text: "love".to_string(),
        syllabic: SyllabicType::Single,
        verse: 0,
        placement: LyricsPlacement::Below,
        note_x: 100.0,
        note_width: 10.0,
        ..Default::default()
    };

    let (layout, node) = layout_lyrics(&params, &ctx);

    assert!(!layout.bbox.is_zero_area(), "Lyrics should have valid bounding box");
    assert!(node.id.is_some(), "Lyrics node should have semantic ID");
    assert!(!node.commands.is_empty(), "Lyrics should have paint commands");
}

/// Test multi-syllable lyrics with dashes.
#[test]
fn test_lyrics_multi_syllable() {
    let ctx = test_context();

    // First syllable (begin)
    let params1 = LyricsParams {
        id: 1,
        text: "beau".to_string(),
        syllabic: SyllabicType::Begin,
        verse: 0,
        note_x: 100.0,
        note_width: 10.0,
        ..Default::default()
    };
    let (layout1, _) = layout_lyrics(&params1, &ctx);

    // Second syllable (middle)
    let params2 = LyricsParams {
        id: 2,
        text: "ti".to_string(),
        syllabic: SyllabicType::Middle,
        verse: 0,
        note_x: 150.0,
        note_width: 10.0,
        ..Default::default()
    };
    let (layout2, _) = layout_lyrics(&params2, &ctx);

    // Third syllable (end)
    let params3 = LyricsParams {
        id: 3,
        text: "ful".to_string(),
        syllabic: SyllabicType::End,
        verse: 0,
        note_x: 200.0,
        note_width: 10.0,
        ..Default::default()
    };
    let (layout3, _) = layout_lyrics(&params3, &ctx);

    // All should have valid bboxes and be positioned correctly
    assert!(!layout1.bbox.is_zero_area());
    assert!(!layout2.bbox.is_zero_area());
    assert!(!layout3.bbox.is_zero_area());

    // X positions should increase
    assert!(layout2.position.x > layout1.position.x);
    assert!(layout3.position.x > layout2.position.x);
}

/// Test lyrics verse stacking.
#[test]
fn test_lyrics_verse_stacking() {
    let ctx = test_context();

    // Verse 0
    let params_v0 = LyricsParams {
        id: 1,
        text: "First".to_string(),
        verse: 0,
        note_x: 100.0,
        note_width: 10.0,
        ..Default::default()
    };
    let (layout_v0, _) = layout_lyrics(&params_v0, &ctx);

    // Verse 1
    let params_v1 = LyricsParams {
        id: 2,
        text: "Second".to_string(),
        verse: 1,
        note_x: 100.0,
        note_width: 10.0,
        ..Default::default()
    };
    let (layout_v1, _) = layout_lyrics(&params_v1, &ctx);

    // Verse 2
    let params_v2 = LyricsParams {
        id: 3,
        text: "Third".to_string(),
        verse: 2,
        note_x: 100.0,
        note_width: 10.0,
        ..Default::default()
    };
    let (layout_v2, _) = layout_lyrics(&params_v2, &ctx);

    // Verses below should stack downward (increasing Y)
    assert!(layout_v1.position.y > layout_v0.position.y, "Verse 1 below verse 0");
    assert!(layout_v2.position.y > layout_v1.position.y, "Verse 2 below verse 1");
}

/// Test lyrics placement above staff.
#[test]
fn test_lyrics_placement_above() {
    let ctx = test_context();

    let params_below = LyricsParams {
        id: 1,
        text: "below".to_string(),
        placement: LyricsPlacement::Below,
        note_x: 100.0,
        note_width: 10.0,
        ..Default::default()
    };
    let (layout_below, _) = layout_lyrics(&params_below, &ctx);

    let params_above = LyricsParams {
        id: 2,
        text: "above".to_string(),
        placement: LyricsPlacement::Above,
        note_x: 100.0,
        note_width: 10.0,
        ..Default::default()
    };
    let (layout_above, _) = layout_lyrics(&params_above, &ctx);

    // Above should have smaller Y than below
    assert!(layout_above.position.y < layout_below.position.y);
}

/// Test lyrics dash layout.
#[test]
fn test_lyrics_dash() {
    let ctx = test_context();
    let (layout, node) = layout_lyrics_dash(100.0, 150.0, 20.0, &ctx);

    assert!(!layout.bbox.is_zero_area());
    assert!(!node.commands.is_empty(), "Dash should have paint commands");
}

/// Test melisma line layout.
#[test]
fn test_lyrics_melisma() {
    let ctx = test_context();
    let (layout, node) = layout_melisma(100.0, 200.0, 20.0, &ctx);

    assert!(!layout.bbox.is_zero_area());
    assert!(!node.commands.is_empty(), "Melisma should have paint commands");
}

// ============================================================================
// Dynamics Tests
// ============================================================================

use engraver::layout::tlayout::dynamics::{
    layout_dynamic, DynamicType, DynamicsAlign, DynamicsParams, DynamicsPlacement,
};

/// Test basic dynamic layout.
#[test]
fn test_dynamics_basic_layout() {
    let ctx = test_context();
    let params = DynamicsParams {
        id: 1,
        dynamic_type: DynamicType::Mf,
        x: 100.0,
        note_width: 10.0,
        ..Default::default()
    };

    let (layout, node) = layout_dynamic(&params, &ctx);

    assert!(!layout.bbox.is_zero_area(), "Dynamic should have valid bounding box");
    assert!(node.id.is_some(), "Dynamic node should have semantic ID");
    assert!(!node.commands.is_empty(), "Dynamic should have paint commands");
}

/// Test all dynamic types have valid layout.
#[test]
fn test_all_dynamics_have_valid_layout() {
    let ctx = test_context();

    let types = [
        DynamicType::Ppp,
        DynamicType::Pp,
        DynamicType::P,
        DynamicType::Mp,
        DynamicType::Mf,
        DynamicType::F,
        DynamicType::Ff,
        DynamicType::Fff,
        DynamicType::Sfz,
        DynamicType::Sfp,
        DynamicType::Fz,
        DynamicType::Fp,
    ];

    for dynamic_type in types {
        let params = DynamicsParams {
            id: 1,
            dynamic_type,
            x: 100.0,
            note_width: 10.0,
            ..Default::default()
        };

        let (layout, node) = layout_dynamic(&params, &ctx);
        assert!(
            !layout.bbox.is_zero_area(),
            "{:?} should have valid bounding box",
            dynamic_type
        );
        assert!(node.id.is_some(), "{:?} should have semantic ID", dynamic_type);
    }
}

/// Test dynamic placement above and below staff.
#[test]
fn test_dynamics_placement() {
    let ctx = test_context();

    let params_below = DynamicsParams {
        id: 1,
        dynamic_type: DynamicType::F,
        placement: DynamicsPlacement::Below,
        x: 100.0,
        ..Default::default()
    };
    let (layout_below, _) = layout_dynamic(&params_below, &ctx);

    let params_above = DynamicsParams {
        id: 2,
        dynamic_type: DynamicType::F,
        placement: DynamicsPlacement::Above,
        x: 100.0,
        ..Default::default()
    };
    let (layout_above, _) = layout_dynamic(&params_above, &ctx);

    // Above should have smaller Y than below
    assert!(layout_above.position.y < layout_below.position.y);
}

/// Test dynamic alignment options.
#[test]
fn test_dynamics_alignment() {
    let ctx = test_context();
    let note_x = 100.0;
    let note_width = 20.0;

    let params_left = DynamicsParams {
        id: 1,
        dynamic_type: DynamicType::F,
        align: DynamicsAlign::Left,
        x: note_x,
        note_width,
        ..Default::default()
    };
    let (layout_left, _) = layout_dynamic(&params_left, &ctx);

    let params_center = DynamicsParams {
        id: 2,
        dynamic_type: DynamicType::F,
        align: DynamicsAlign::Center,
        x: note_x,
        note_width,
        ..Default::default()
    };
    let (layout_center, _) = layout_dynamic(&params_center, &ctx);

    // Left-aligned should start at note_x
    assert!((layout_left.position.x - note_x).abs() < 0.1, "Left align should be at note_x");

    // Both should have valid layouts
    assert!(!layout_left.bbox.is_zero_area());
    assert!(!layout_center.bbox.is_zero_area());
}

/// Test custom text dynamic.
#[test]
fn test_dynamics_custom_text() {
    let ctx = test_context();
    let params = DynamicsParams {
        id: 1,
        dynamic_type: DynamicType::Other,
        custom_text: Some("molto cresc.".to_string()),
        x: 100.0,
        ..Default::default()
    };

    let (layout, node) = layout_dynamic(&params, &ctx);

    assert!(!layout.bbox.is_zero_area());
    assert!(node.id.is_some());
}
