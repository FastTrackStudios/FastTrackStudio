# Analysis of lrxed Lyrics Implementation

## Overview

The `lrxed` library uses the **LRC (LyRiCs)** file format, which is a standard format for synchronized lyrics. This is different from our section-based approach but has some excellent features we should consider adopting.

## Key Features from lrxed

### 1. LRC Format Support
- **Format**: `[MM:SS.CC] Line text`
  - Example: `[00:12.34] Drowning, fishing, dropping, screaming under the lights`
- **Metadata tags**: `[ti:Title]`, `[ar:Artist]`, `[al:Album]`, etc.
- **Unsynced lines**: Lines without timestamps are unsynced

### 2. Timestamp Structure
```rust
pub struct Timestamp {
    time: Duration,
    text: String,  // Formatted as "MM:SS.CC"
}
```
- Uses `std::time::Duration` for precise timing
- Format: `MM:SS.CC` (minutes:seconds.centiseconds)
- Parses from string format

### 3. TimeIndex for Efficient Lookup
```rust
pub struct TimeIndex {
    entries: Vec<TimeIndexEntry>,
}

pub struct TimeIndexEntry {
    time: Duration,
    line_num: Option<u16>,
}
```
- Binary search for finding which line to display at a given time
- Supports both random access and sequential access with hints
- Very efficient for real-time playback

### 4. Sync Percentage
- Tracks how many lines have timestamps
- Calculated as: `(synced_lines / total_lines) * 100`
- Useful for showing progress during synchronization

### 5. Simple Structure
- Just a list of `LyricLine` with optional timestamps
- No sections - flat structure
- Each line can have a timestamp

## Comparison with Our Implementation

| Feature | lrxed | Our Implementation |
|---------|-------|-------------------|
| **Format** | LRC format (`[MM:SS.CC]`) | Section-based (`[Intro]`, `[Verse 1]`) |
| **Structure** | Flat list of lines | Hierarchical (sections → lines) |
| **Timestamps** | Per-line timestamps | Per-line start/end times (f64 seconds) |
| **Time Lookup** | TimeIndex with binary search | Linear search through sections |
| **Metadata** | LRC tags (`[ti:Title]`) | HashMap metadata |
| **Integration** | Standalone | Integrated with setlist sections |
| **Output Formats** | LRC export | Multiple (sheet, slides, syllables, MIDI) |

## Recommendations

### 1. Add LRC Format Support
We should add support for:
- **Import**: Parse LRC files into our section-based structure
- **Export**: Export our lyrics to LRC format
- This gives us compatibility with existing LRC files and tools

### 2. Add TimeIndex
Implement a `TimeIndex` similar to lrxed for efficient time-based lookups:
- Useful for real-time playback
- Binary search for finding current line
- Can work with our section-based structure

### 3. Improve Timestamp Format
Consider using a `Timestamp` type similar to lrxed:
- More precise (centiseconds)
- Standard format (`MM:SS.CC`)
- Easy to parse/write

### 4. Add Sync Percentage
Track sync status:
- Useful for UI feedback
- Shows progress during manual sync
- Can be calculated from lines with timestamps

### 5. Keep Our Advantages
Our section-based approach has benefits:
- **Better organization**: Sections match setlist structure
- **Multiple outputs**: Slides, syllables, MIDI generation
- **Setlist integration**: Automatic section matching
- **Parenthetical text**: Better handling of stage directions

## Proposed Enhancements

### Phase 1: LRC Import/Export
```rust
// Add to parser.rs
pub fn parse_lrc(text: &str, song_name: String) -> Result<Lyrics, ParseError>
pub fn export_lrc(lyrics: &Lyrics) -> String
```

### Phase 2: TimeIndex
```rust
// Add to core.rs or new time_index.rs
pub struct TimeIndex {
    entries: Vec<TimeIndexEntry>,
}

impl TimeIndex {
    pub fn from_lyrics(lyrics: &Lyrics) -> Self;
    pub fn find_line_at_time(&self, time: f64) -> Option<usize>;
}
```

### Phase 3: Timestamp Type
```rust
// Add to core.rs
pub struct Timestamp {
    time: Duration,
    text: String,  // "MM:SS.CC"
}

impl Timestamp {
    pub fn from_seconds(seconds: f64) -> Self;
    pub fn to_seconds(&self) -> f64;
}
```

### Phase 4: Sync Percentage
```rust
// Add to Lyrics
impl Lyrics {
    pub fn sync_percentage(&self) -> u8;
    fn calc_sync_percentage(&self) -> u8;
}
```

## Conclusion

lrxed provides excellent reference implementation for:
- LRC format parsing/writing
- Efficient time-based lookups
- Timestamp handling

We should adopt these features while keeping our section-based structure and setlist integration, which provide unique value for FastTrackStudio's use case.

