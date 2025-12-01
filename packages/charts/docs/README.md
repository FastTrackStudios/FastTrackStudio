# FTS Chart Parser Documentation

A powerful chart parsing system for music notation that intelligently handles chords, sections, rhythms, melodies, and key signatures.

## Architecture

The parser is built with a modular architecture:

### Core Parser (`parser.rs`)

The main orchestrator that brings together all parsing components and manages:

- Song metadata (title, artist, tempo, time signature, key)
- Section structure and auto-numbering
- Chord memory (global and section-specific)
- Section templates for chord progression reuse
- Content delegation to specialized parsers

### Specialized Parsers

- **`chord.rs`** - Semantic chord parsing (note names, scale degrees, roman numerals)
- **`chord_rhythm.rs`** - Chord + rhythm notation (slashes, push/pull, durations)
- **`melody.rs`** - Melody sequences (Lilypond and scale degree notation)
- **`text_cues.rs`** - Performance instructions (@all, @keys, etc.)
- **`sections.rs`** - Section definitions and auto-numbering logic
- **`metadata.rs`** - Song information parsing

### Atomic Parsers (`blocks/`)

- **`tempo.rs`** - BPM parsing
- **`time_signature.rs`** - Time signature parsing
- **`key.rs`** - Key signature and scale structures
- **`note.rs`** - Individual note parsing (pitch + accidentals)
- **`rhythm.rs`** - Rhythm duration parsing

## Quick Start Example

```md
Reckless Love - Cory Asbury
68bpm 6/8 #E

Intro 4
Emaj7 C#m7 B A

Verse 8
E C#m B A

Chorus 4
E B C#m A

Bridge 8
E E E E

Outro 4
```

## Documentation Sections

### Getting Started

- **[Song Metadata](./metadata.md)** - Title, artist, tempo, time signature, and key information

### Chart Structure

- **[Song Sections](./sections.md)** - Organizing your chart with auto-numbered sections and templates
- **[Chord Input](./chord-input.md)** - How to enter chords using note names, scale degrees, or roman numerals
- **[Key Signatures](./key-signatures.md)** - Setting and changing keys throughout your chart

### Rhythm and Timing

- **[Chord Rhythms](./chord-rhythm.md)** - Specifying rhythms and durations for chords
- **[Rhythm Notation](./rhythm.md)** - Advanced rhythm patterns, variables, and inline rhythms

### Melodies and Expression

- **[Melody Notation](./melody.md)** - Adding melodies using Lilypond syntax, inline or in parallel
- **[Articulations](./articulations.md)** - Accents, staccato, tenuto, and other articulation marks

### Additional Elements

- **[Text Cues](./text-cues.md)** - Adding performance instructions and callouts for the band

## Core Features

### 1. Smart Chord Memory

Only define a chord once. After entering `Gm7b5#11`, just type `g` to recall it.

```md
Intro
Gmaj7 Cadd9 Em7 D13sus

Verse
g c e d // Expands to: Gmaj7 Cadd9 Em7 D13sus
```

### 2. Section-Specific Memory

Each section type remembers its own chord definitions:

```md
Chorus
c g e d // Chorus remembers these chords

Verse
c g7 // Verse overrides G to G7 (only affects Verse sections)

Chorus
c g e d // Still uses original Gmaj7
```

### 3. One-Time Overrides

Use `!` to override chord memory temporarily without affecting future recalls:

```md
Verse
e d e !g7 // Use G7 just this once
e d e g // Returns to Gmaj7 (if that was defined earlier)
```

### 4. Automatic Section Numbering

- Sections only get numbers if they appear more than once
- Consecutive sections get split letters (1a, 1b)

```md
Intro // No number (only one Intro)
Verse // Gets "Verse 1" (multiple verses exist)
Verse // Gets "Verse 2"
Bridge // No number (only one Bridge)
```

### 5. Dynamic Section Lengths

- With chords: Length determined by chord count (each chord = 1 bar)
- Without chords: Use explicit length (e.g., `Bridge 8`)

```md
Verse
g c d em // 4 bars (4 chords)

Bridge 8 // 8 bars (no chords specified)
```

### 6. Song Metadata

Parse song information at the beginning:

```md
Song Title - Artist Name
120bpm 4/4 #C

// Extracts:
// - Title, Artist, Tempo, Time Signature, Key
```

### 7. Mid-Song Key Changes

Change keys anywhere, even mid-phrase:

```md
G C D Em
#D // Key change
D A Bm G

// Or mid-phrase:
G C #D D A // Changes to D major before the D chord
```

## Section Types

- `in` / `intro` - Intro
- `vs` / `verse` - Verse
- `ch` / `chorus` - Chorus
- `br` / `bridge` - Bridge
- `inst` / `instrumental` - Instrumental
- `out` / `outro` - Outro
- `pre` - Pre-Chorus (auto-attaches to following section)
- `post` - Post-Chorus (auto-attaches to preceding section)

### Special Sections

**Pre/Post Sections** automatically inherit the name of their attached section:

```md
pre
g c

ch
c g e d // Pre becomes "Pre-Chorus"

post
e e e e // Post becomes "Post-Chorus"
```

### 8. Text Cues

Add performance instructions for the band or specific instruments:

```md
Verse
@all "Quiet, intimate feel"
@keys "arps here"
@drums "Light shuffle"
G C Em D
```

## Advanced Features (Coming Soon)

- **Repeat notation**: `x2`, `x3` to repeat progressions
- **Roman numerals**: Full support for `I`, `ii`, `iii`, etc.
- **Melodic notation**: Advanced syntax for melodies
- **URL param driven**: Complete control via URL parameters

## Design Goals

1. **Mode-free input** - No need to switch between different input modes
2. **Intelligent parsing** - Automatically distinguish between notes, chords, and commands
3. **Minimal typing** - Define once, reuse everywhere
4. **Professional output** - Clean, readable charts
5. **Flexible rhythms** - Multiple ways to specify timing
6. **Context-aware** - Smart defaults based on key and section type
