# Rhythm Trait Design

## Overview

The `Rhythm` trait provides a common interface for any musical element that has rhythmic information (duration and position in time). This allows us to write generic code that works with any rhythmic element, whether it's a chord, a rest, a melodic note, or any other musical event.

## Core Concepts

### MusicalDuration

Represents a duration in musical time:

```rust
pub struct MusicalDuration {
    pub measures: usize,
    pub beats: usize,
    pub subdivision: usize,
}
```

### MusicalPosition

Represents an absolute position in the entire song:

```rust
pub struct MusicalPosition {
    pub total_duration: MusicalDuration, // Absolute position from the start
    pub section_index: usize,            // Which section this position is in
}
```

## The Rhythm Trait

```rust
pub trait Rhythm {
    /// Get the duration of this element
    fn duration(&self) -> &MusicalDuration;

    /// Get the position of this element in the song
    fn position(&self) -> &MusicalPosition;

    /// Set the position of this element
    fn set_position(&mut self, position: MusicalPosition);

    /// Calculate the end position based on duration and current position
    fn end_position(&self) -> MusicalPosition {
        let mut end_pos = self.position().clone();
        end_pos.total_duration = end_pos.total_duration.clone() + self.duration().clone();
        end_pos
    }

    /// Check if this element spans multiple measures
    fn spans_multiple_measures(&self) -> bool {
        self.duration().measures > 0 ||
        (self.duration().measures == 0 && self.duration().beats >= 4)
    }
}
```

## The RhythmParser Trait

For parsing rhythmic elements from strings with optional time signature context:

```rust
pub trait RhythmParser: Sized {
    /// Parse a rhythmic element from a string
    ///
    /// The `time_signature` parameter is optional and may be needed for
    /// context-dependent parsing (e.g., slash notation that depends on meter)
    fn parse_with_rhythm(input: &str, time_signature: Option<(u8, u8)>) -> Result<Self, String>;
}
```

## Implementations

### ChordInstance

`ChordInstance` now implements the `Rhythm` trait:

```rust
pub struct ChordInstance {
    pub root: RootNote,
    pub full_symbol: String,
    pub parsed: ChordData,
    pub rhythm: ChordRhythm,
    pub timing_modifier: Option<TimingModifier>,
    pub original_token: String,
    pub duration: MusicalDuration,
    pub position: MusicalPosition, // NEW FIELD
}

impl Rhythm for ChordInstance {
    fn duration(&self) -> &MusicalDuration { &self.duration }
    fn position(&self) -> &MusicalPosition { &self.position }
    fn set_position(&mut self, position: MusicalPosition) { self.position = position; }
}
```

## Benefits

1. **Unified Interface**: All rhythmic elements share the same interface for accessing duration and position
2. **Generic Algorithms**: Write functions that work with any `impl Rhythm` without caring about the specific type
3. **Extensibility**: Easy to add rhythm support to new musical elements
4. **Type Safety**: The trait ensures all rhythmic elements have the required fields and methods

## Future Implementations

The `Rhythm` trait can be implemented for:

- `MelodicNote` - individual notes in a melody
- `Rest` - musical rests
- `Space` - silent space in the chart
- `Section` - entire sections with their total duration
- `Measure` - individual measures

## Example Usage

```rust
fn calculate_total_duration<R: Rhythm>(elements: &[R]) -> MusicalDuration {
    elements.iter()
        .map(|e| e.duration().clone())
        .fold(MusicalDuration::zero(), |acc, dur| acc + dur)
}

fn find_elements_at_position<R: Rhythm>(
    elements: &[R],
    target: &MusicalPosition
) -> Vec<&R> {
    elements.iter()
        .filter(|e| e.position() == target)
        .collect()
}
```

## Integration with Mini-Parsers

The `RhythmParser` trait can be implemented by mini-parsers to create rhythmic elements:

```rust
impl RhythmParser for ChordInstance {
    fn parse_with_rhythm(input: &str, time_signature: Option<(u8, u8)>) -> Result<Self, String> {
        // Use ChordMiniParser to parse the chord
        // Use rhythm notation to calculate duration
        // Position is set later by the chart parser
        todo!()
    }
}
```

## Related Types

### MusicalToken with Accidental Support

The new `MusicalToken` enum includes accidental support:

```rust
pub enum MusicalToken {
    NoteName(String),
    ScaleName(String),
    ScaleDegree(u8),
    Extension(String),
    Accidental(Modifier), // NEW: Sharp, Flat, DoubleSharp, DoubleFlat
}
```

Helper methods for accidentals:

- `parse_accidental(c: char)` - Parse single character accidentals
- `parse_accidental_str(s: &str)` - Parse string accidentals (including double sharps/flats)
- `is_accidental(c: char)` - Check if a character is an accidental

This allows mini-parsers to work with accidentals in a type-safe way.
