# Keyflow Parsing Status

## Overview

The `keyflow` package provides a complete chart parsing system that converts text syntax into a structured `Chart` data structure.

## Main Data Structure

**Everything is represented in a single `Chart` struct** (`packages/keyflow/src/chart/chart.rs`):

```rust
pub struct Chart {
    pub metadata: SongMetadata,              // Title, artist
    pub sections: Vec<ChartSection>,         // All sections (Verse, Chorus, etc.)
    pub current_key: Option<Key>,             // Current key during parsing
    pub initial_key: Option<Key>,            // Starting key
    pub ending_key: Option<Key>,             // Ending key
    pub key_changes: Vec<KeyChange>,         // All key changes with positions
    pub tempo: Option<Tempo>,                // BPM
    pub time_signature: Option<TimeSignature>, // Current time signature
    pub initial_time_signature: Option<TimeSignature>,
    pub time_signature_changes: Vec<TimeSignatureChange>,
    pub settings: ChartSettings,             // Chart settings
    // Internal:
    pub(crate) chord_memory: ChordMemory,
    pub(crate) templates: TemplateManager,
}
```

## Current Capabilities

### ✅ Syntax → Struct (Parsing)
- **Entry Point**: `Chart::parse(input: &str) -> Result<Chart, String>`
- **Macro**: `chart! { "..." }` for inline chart definition
- **Status**: ✅ **FULLY IMPLEMENTED**

### ❌ Struct → Syntax (Serialization)
- **Status**: ❌ **NOT IMPLEMENTED**
- **What's Missing**: No function to convert `Chart` back to text syntax
- **Current Display**: Only has `Display` trait for human-readable output (not round-trip)

## Implemented Features

### Chord Parsing
- ✅ All chord qualities (major, minor, diminished, augmented, etc.)
- ✅ Extensions (7th, 9th, 11th, 13th)
- ✅ Alterations (b9, #11, etc.)
- ✅ Additions (add2, add4, add9, etc.)
- ✅ Omissions (omit3, omit5, etc.)
- ✅ Slash chords (C/E, 1/3)
- ✅ Multiple notation systems:
  - Note names (C, D, E, etc.)
  - Scale degrees (1, 2, 3, etc.)
  - Roman numerals (I, ii, iii, etc.)

### Rhythm & Duration
- ✅ Lily-style syntax (`_4`, `_8`, `_2`, `_1`)
- ✅ Slash notation (`/`, `//`, `///`)
- ✅ Dotted rhythms (`_4.`, `/.`)
- ✅ Rests (`r4`, `r8`)
- ✅ Spaces (`s4`, `s8`)
- ✅ Push/pull notation (`'C`, `C'`)

### Chart Structure
- ✅ Metadata (title, artist, tempo, time signature, key)
- ✅ Sections (Intro, Verse, Chorus, Bridge, Outro, Instrumental, Pre/Post)
- ✅ **Custom sections** (`[SectionName]` syntax) ✅ **IMPLEMENTED**
- ✅ Section numbering
- ✅ Subsection prefix (`^Band-In`)
- ✅ Measure separators (`|`) ✅ **IMPLEMENTED**
- ✅ Auto-duration splitting between separators ✅ **IMPLEMENTED**

### Repeats
- ✅ Fixed repeats (`x4`, `x8`)
- ✅ Smart repeats (`x^`) - auto-calculates based on section length ✅ **IMPLEMENTED**

### Commands
- ✅ Fermata (`/fermata`)
- ✅ Accent (`->` or `/accent`)

### Advanced Features
- ✅ Chord memory (remembers chord qualities across sections)
- ✅ Templates (recall previously defined sections)
- ✅ Text cues (`@keys "synth here"`)
- ✅ Comments (`; comment text`)
- ✅ Settings (`/SMART_REPEATS=true`)

## Missing/Incomplete Features

### 1. Smart Repeats Algorithm (4-bar grouping)
**Status**: ⚠️ **PARTIAL**
- Setting is parsed (`/SMART_REPEATS=true`)
- Algorithm not implemented (should group phrases into 4-bar units)

### 2. Struct → Syntax Serialization
**Status**: ❌ **NOT IMPLEMENTED**
- No `to_syntax()` or `serialize()` method
- `Display` trait exists but outputs formatted display, not round-trip syntax
- Would need to implement:
  - `Chart::to_syntax() -> String`
  - Convert all struct fields back to text format
  - Preserve original formatting where possible

### 3. Additional Commands
**Status**: ⚠️ **PARTIAL**
- Only `/fermata` and accent implemented
- Could add: staccato, legato, dynamics, etc.

## Round-Trip Capability

### Current State
- ✅ **Syntax → Struct**: Fully working via `Chart::parse()`
- ❌ **Struct → Syntax**: Not implemented

### What Would Be Needed for Round-Trip

1. **Serialization Function**:
   ```rust
   impl Chart {
       pub fn to_syntax(&self) -> String {
           // Convert Chart back to text syntax
       }
   }
   ```

2. **Components to Serialize**:
   - Metadata (title, artist, tempo, time signature, key)
   - Sections with their measures
   - Chords with their durations and positions
   - Key changes (`#G` in sections)
   - Time signature changes
   - Commands (`/fermata`, `->`)
   - Text cues (`@keys "text"`)
   - Comments (if preserved)
   - Settings (`/SMART_REPEATS=true`)

3. **Challenges**:
   - Original formatting is lost (spacing, line breaks)
   - Some information might be normalized (chord qualities, etc.)
   - Push/pull positions need to be converted back to apostrophe notation
   - Measure separators need to be reinserted at correct positions

## Summary

| Feature | Status | Notes |
|---------|--------|-------|
| **Syntax → Struct** | ✅ Complete | `Chart::parse()` fully functional |
| **Struct → Syntax** | ❌ Missing | No serialization function |
| **Single Struct** | ✅ Yes | Everything in `Chart` struct |
| **Custom Sections** | ✅ Implemented | `[SectionName]` syntax works |
| **Measure Separators** | ✅ Implemented | `\|` syntax works |
| **Smart Repeats (`x^`)** | ✅ Implemented | Auto-calculates repeat count |
| **Smart Repeats Algorithm** | ⚠️ Partial | Setting parsed, algorithm missing |
| **Push/Pull Notation** | ✅ Implemented | `'C` (push), `C'` (pull) |
| **Auto-Duration** | ✅ Implemented | Auto-splits between `\|` separators |

## Recommendations

1. **Add Serialization**: Implement `Chart::to_syntax()` for round-trip capability
2. **Complete Smart Repeats**: Implement 4-bar grouping algorithm
3. **Preserve Formatting**: Consider storing original formatting hints for better round-trip

