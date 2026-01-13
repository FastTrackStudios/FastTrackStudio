# Chart Rendering Architecture

## Overview

This document describes the architecture for rendering lead sheet charts in FastTrackStudio, focusing on the rhythm slash and chord symbol positioning system. The implementation follows MuseScore's layout algorithms for professional music notation.

## Current State

### Components

```
┌─────────────────────────────────────────────────────────────────────┐
│                           Data Flow                                  │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│  ┌──────────────┐    ┌──────────────┐    ┌────────────────────┐    │
│  │   Keyflow    │───▶│    Chart     │───▶│   Chart Renderer   │    │
│  │   Parser     │    │   (Model)    │    │   (fts-native)     │    │
│  └──────────────┘    └──────────────┘    └────────────────────┘    │
│                             │                      │                │
│                             ▼                      ▼                │
│                    ┌──────────────┐       ┌──────────────┐         │
│                    │  ChordInstance│       │  WGPU/Vello  │         │
│                    │  RhythmSlash │       │  Rendering   │         │
│                    │  Measure     │       └──────────────┘         │
│                    └──────────────┘                                 │
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘
```

### Key Files

| File | Purpose |
|------|---------|
| `packages/keyflow/src/chart/types.rs` | Core data types: `ChordInstance`, `RhythmSlash`, `Measure` |
| `packages/keyflow/src/chart/parser.rs` | Chart text parsing and rhythm slash generation |
| `apps/fts-native/src/chart_renderer.rs` | WGPU-based chart rendering with MuseScore positioning |
| `packages/engraver/src/style/mod.rs` | MStyle system for spacing values (Sid enum) |

### Rhythm Slash System

Rhythm slashes are stemless noteheads that indicate beats in lead sheet notation. They appear on the staff below chord symbols.

#### Data Model (Keyflow)

```rust
/// Represents a rhythm slash (stemless notehead indicating a beat)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RhythmSlash {
    /// Beat number within the measure (0-indexed)
    pub beat: u8,
    /// Position within the song (for rendering)
    pub position: AbsolutePosition,
}
```

#### Generation

Slashes are generated in `Measure::generate_rhythm_slashes()` during chart post-processing:

```rust
pub fn generate_rhythm_slashes(&mut self, measure_number: i32, section_index: usize) {
    let beats = self.beats_per_measure();
    let mut suppressed_beats: Vec<bool> = vec![false; beats as usize];

    // Only suppress slashes for explicit rests or explicit slash notation
    for chord in &self.chords {
        let chord_beat = chord.position.beats() as usize;
        match &chord.rhythm {
            ChordRhythm::Rest { .. } => { /* suppress */ }
            ChordRhythm::Slashes(count) => { /* suppress covered beats */ }
            _ => {} // Chord symbols don't suppress slashes
        }
    }

    // Generate slashes for all non-suppressed beats
    for beat in 0..beats {
        if !suppressed_beats[beat as usize] {
            self.rhythm_slashes.push(RhythmSlash::new(beat, position));
        }
    }
}
```

**Key Design Decision**: Chord symbols do NOT suppress slashes. Only explicit rests and slash notation (e.g., `G////`) suppress slashes. This follows standard lead sheet convention where slashes appear on every beat regardless of chord changes.

### Horizontal Positioning System

#### MuseScore-Compatible Spacing

The positioning system uses MuseScore's spacing algorithm with these key values from `MStyle`:

| Property | Default | Description |
|----------|---------|-------------|
| `BarNoteDistance` | 1.5 sp | Distance from barline to first beat |
| `NoteBarDistance` | 1.0 sp | Distance from last beat to barline |
| `Spatium` | 1.75mm | Base unit for all spacing |

#### Beat Position Calculation

Both chord symbols and rhythm slashes use identical positioning formulas:

```rust
// Calculate usable width within measure
let usable_width = measure_width - bar_note_distance - note_bar_distance;

// Calculate beat position
let beat_fraction = beat / beats_per_measure;
let beat_x = measure_x + bar_note_distance + (beat_fraction * usable_width);

// Apply alignment offset (CENTER alignment)
let notehead_width = staff_space * 1.18;  // Standard notehead width
let offset = notehead_width * 0.5;
let final_x = beat_x - offset;
```

#### Coordinate Flow

```
For a 4/4 measure with measure_width = 100px:

bar_note_distance = 10px
note_bar_distance = 10px
usable_width = 100 - 10 - 10 = 80px

Beat 0: beat_x = 10 + (0/4 × 80) = 10px
Beat 1: beat_x = 10 + (1/4 × 80) = 30px
Beat 2: beat_x = 10 + (2/4 × 80) = 50px
Beat 3: beat_x = 10 + (3/4 × 80) = 70px

After offset (−5.9px for centering):
Beat 0: final_x ≈ 4.1px
Beat 1: final_x ≈ 24.1px
Beat 2: final_x ≈ 44.1px
Beat 3: final_x ≈ 64.1px
```

### SMuFL Glyph System

Rhythm slashes use SMuFL (Standard Music Font Layout) glyphs from the Leland font:

| Glyph | SMuFL Name | Width (sp) | Usage |
|-------|------------|------------|-------|
| Primary | `NoteheadSlashHorizontalEnds` | ~2.0 | Quarter note slashes |
| Fallback | `RepeatBarSlash` | 1.924 | If primary unavailable |

**Glyph Anchor Points**: SMuFL glyphs have their origin at the **left edge**. The glyph extends to the right from the x position.

From `leland_metadata.json`:
```json
"repeatBarSlash": {
    "bBoxNE": [1.924, 1.0],
    "bBoxSW": [0.0, -1.0]
}
```

### Rendering Pipeline

```
┌─────────────────────────────────────────────────────────────────────┐
│                      Rendering Pipeline                              │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│  ┌────────────────┐                                                 │
│  │  Chord Symbols │──▶ glyphon TextArea ──▶ Text Renderer           │
│  │  (text)        │                                                 │
│  └────────────────┘                                                 │
│                                                                      │
│  ┌────────────────┐                                                 │
│  │ Rhythm Slashes │──▶ tessellate_glyph_to_ndc ──▶ Vertex Pipeline  │
│  │  (glyphs)      │                                                 │
│  └────────────────┘                                                 │
│                                                                      │
│  Both use world-space coordinates that are transformed by camera    │
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘
```

## Architecture Decisions

### 1. Slash Generation in Keyflow (Not Renderer)

**Decision**: Generate rhythm slashes in the Keyflow parser during post-processing, not in the renderer.

**Rationale**:
- Keeps rendering logic simple (just render what's in the model)
- Allows slash data to be serialized/exported
- Enables future features like slash customization per beat
- Follows separation of concerns (data vs presentation)

### 2. MuseScore-Compatible Positioning

**Decision**: Use MuseScore's exact spacing values and algorithms.

**Rationale**:
- Professional, industry-standard appearance
- Well-documented reference implementation
- Consistent with other music notation software
- Easy to compare output for debugging

### 3. MStyle System for Configuration

**Decision**: Implement a MuseScore-compatible style system (`MStyle` with `Sid` enum).

**Rationale**:
- Centralizes all spacing/style values
- Allows easy customization
- Matches MuseScore's architecture for reference
- Supports future style presets (Jazz, Classical, etc.)

## Known Issues

### Potential Alignment Issues

If slashes and chords appear misaligned:

1. **Coordinate System Differences**: Text (glyphon) and glyphs (tessellation) go through different rendering paths
2. **Camera Transform**: Text positions are transformed during TextArea creation; glyph positions are baked into NDC
3. **Glyph Anchors**: Different glyphs may have different anchor points

### Debugging Steps

1. Add visual markers at beat positions to verify calculations
2. Log exact x coordinates for both chords and slashes
3. Check camera/zoom state affects both equally
4. Verify MStyle values are consistent between calls

## Future Plans

### Phase 1: Vertical Positioning Refinement
- [ ] Implement proper staff line positioning for slashes (currently hardcoded to center line)
- [ ] Add support for different slash glyph types based on note duration
- [ ] Implement stem rendering for half/whole note slashes

### Phase 2: Style System Enhancement
- [ ] Complete MStyle implementation with all ~80 properties
- [ ] Add style presets (Jazz Lead Sheet, Classical, Nashville Number)
- [ ] Implement style import/export

### Phase 3: Advanced Rhythm Notation
- [ ] Tied notes across barlines
- [ ] Syncopation notation
- [ ] Rhythm kicks and accents
- [ ] Multi-voice rhythm patterns

### Phase 4: Interactive Editing
- [ ] Click-to-select slashes
- [ ] Drag to adjust timing
- [ ] Right-click context menu for slash properties
- [ ] Keyboard shortcuts for rhythm entry

## Reference Implementation

The MuseScore reference code is available at:
- `libs/reference/sheet-music/musescore/src/engraving/rendering/score/horizontalspacing.cpp`
- `libs/reference/sheet-music/musescore/src/engraving/rendering/score/harmonylayout.cpp`

Key functions to study:
- `getFirstSegmentXPos()` - First beat positioning
- `chordRestSegmentNaturalWidth()` - Beat width calculation
- `HarmonyLayout::calculateBoundingRect()` - Chord symbol alignment

## Related Documents

- [MuseScore-Style Layout Plan](/.claude/plans/imperative-crafting-bird.md)
- [Engraver Style System](../packages/engraver/src/style/)
- [Keyflow Chart Types](../packages/keyflow/src/chart/types.rs)
