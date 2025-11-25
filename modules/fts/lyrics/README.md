# Lyrics Sync Engine

A lyrics synchronization engine for FastTrackStudio that parses lyrics text, matches sections with setlist songs, and generates multiple output formats.

## Features

- **Text Parsing**: Parse lyrics from a simple text format with section markers and parenthetical text
- **Section Matching**: Automatically match lyric sections with song sections from the setlist
- **Multiple Output Formats**:
  - Formatted lyric sheets (for printing/display)
  - Slides (line-by-line for ProPresenter-style display)
  - Syllables (karaoke-style syllable splits)
  - MIDI events (notes for each syllable)

## Usage

### Basic Parsing

```rust
use lyrics::{parse_lyrics, Lyrics};

let text = r#"[Intro]
(Woo)
(Are you ready?)
Well!
Well!

[Verse 1]
Drowning, fishing, dropping, screaming under the lights
I'm feeling everything crashing, burning, I lost track of time
Into you

[Chorus]
I'm breathing, I'm breathing, I think I'm reading you well
I'm breathing, I'm breathing, I think I'm reading you well"#;

let lyrics = parse_lyrics(text, "My Song".to_string())?;
```

### Section Matching with Setlist

```rust
use lyrics::{parse_lyrics, sync::SectionMatcher};
use setlist::core::Song;

let mut lyrics = parse_lyrics(text, "My Song".to_string())?;
let song: Song = /* ... get song from setlist ... */;

// Match sections
let matches = SectionMatcher::match_sections(&mut lyrics, &song);

// Apply timing information from song sections
SectionMatcher::apply_timing(&mut lyrics, &song);
```

### Generating Output Formats

```rust
use lyrics::{parse_lyrics, output::*};

let lyrics = parse_lyrics(text, "My Song".to_string())?;

// Generate formatted lyric sheet
let sheet = LyricSheet::generate(&lyrics);
println!("{}", sheet);

// Generate slides (for ProPresenter)
let slides = Slides::generate(&lyrics);
for slide in slides {
    println!("Slide: {}", slide.text);
}

// Generate syllables (for karaoke)
let syllable_lines = Syllables::generate(&lyrics);
for line in syllable_lines {
    println!("Syllables: {:?}", line.syllables);
}

// Generate MIDI events
let midi_events = MidiGenerator::generate(&lyrics, 60, 100);
for event in midi_events {
    println!("MIDI: note={}, time={}", event.note, event.start_time);
}
```

## Text Format

The parser supports a simple text format:

- **Section markers**: `[Section Name]` (e.g., `[Intro]`, `[Verse 1]`, `[Chorus]`)
- **Parenthetical text**: `(Woo)`, `(Are you ready?)` - shown in parentheses
- **Regular lyrics**: Normal text lines

Example:

```text
[Intro]
(Woo)
(Are you ready?)
Well!
Well!

[Verse 1]
Drowning, fishing, dropping, screaming under the lights
I'm feeling everything crashing, burning, I lost track of time
Into you

[Chorus]
I'm breathing, I'm breathing, I think I'm reading you well
I'm breathing, I'm breathing, I think I'm reading you well
```

## Section Matching

The sync engine can automatically match lyric sections with song sections from the setlist:

1. **Exact name match**: `[Intro]` matches `"Intro"` section
2. **Type and number match**: `[Verse 1]` matches `Verse` section with number `1`
3. **Fuzzy matching**: Partial matches based on section type

The matcher returns confidence scores (0.0 to 1.0) for each match, allowing you to review and adjust matches as needed.

## Integration with Setlist

The lyrics module integrates with the `setlist` module to:

- Match lyric sections with song sections by name, type, and number
- Apply timing information from song sections to lyric lines
- Support automatic section detection and matching

This allows lyrics to be synchronized with the song structure defined in the setlist, making it easy to display lyrics at the right time during performance.

