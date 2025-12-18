# Fold Pattern Usage Guide

## Quick Start

The Fold pattern is now implemented and ready to use! It provides an immutable, context-aware way to transform templates.

## Basic Usage

```rust
use fts::smart_template::template::fold::*;
use fts::smart_template::template::traits::Template;

// Create a template
let template = /* ... */;

// Apply a folder transformation
let result = template.fold(AutoIncrementGroupFolder::new());
```

## Available Folders

### 1. AutoIncrementGroupFolder

Automatically puts incremented tracks (e.g., "Snare 2") into separate groups if the base track exists.

```rust
// If "Snare" exists and we see "Snare 2", put it in a separate group
let result = template.fold(AutoIncrementGroupFolder::new());
```

**Use case**: When parsing tracks, if "Snare" exists and you encounter "Snare 2", automatically make it a separate group instead of a child.

### 2. PlaylistMatchingFolder

Handles playlist matching intelligently by marking tracks for playlist usage.

```rust
let result = template.fold(PlaylistMatchingFolder::new());
```

**Use case**: When you have tracks like "Snare .1" and "Snare .2", mark them for playlist (fixed item lanes) usage.

### 3. ModeFilterFolder

Filters tracks by group mode (Full, Recording, Midi, Minimal).

```rust
let result = template.fold(ModeFilterFolder::new(GroupMode::Recording));
```

**Use case**: Show only tracks that belong to a specific mode.

### 4. TrackRenamerFolder

Renames tracks based on a mapping.

```rust
let mut renamer = TrackRenamerFolder::new();
renamer.add_rename("Old Name", "New Name");
let result = template.fold(renamer);
```

### 5. TrackPropertyModifier

Modifies track properties (volume, mute, etc.).

```rust
let result = template.fold(
    TrackPropertyModifier::new()
        .with_volume_multiplier(0.8)
        .with_mute_all(false)
);
```

## Composing Transformations

You can chain multiple folders together:

```rust
let result = template
    .fold(AutoIncrementGroupFolder::new())
    .fold(PlaylistMatchingFolder::new())
    .fold(ModeFilterFolder::new(GroupMode::Recording));
```

## Context-Aware Transformations

The Fold pattern provides `FoldContext` which gives you read-only access to the original template. This allows you to make decisions based on what exists:

```rust
// Check if a track exists
if context.has_track("Snare") {
    // Do something
}

// Get tracks with the same base name
let snare_variants = context.get_tracks_by_base_name("Snare");
// Returns: ["Snare", "Snare 2", "Snare .1", etc.]

// Count tracks by base name
let count = context.count_by_base_name("Snare");
```

## Creating Custom Folders

You can create your own folders by implementing the `TemplateFolder` trait:

```rust
struct MyCustomFolder {
    // Your state here
}

impl TemplateFolder for MyCustomFolder {
    fn transform_track(&mut self, mut track: Track, context: &FoldContext) -> Track {
        // Your transformation logic
        // You can read from context to make decisions
        if context.has_track("Some Track") {
            track.set_track_type("CUSTOM");
        }
        track
    }
    
    // Optionally override fold_track if you need to filter tracks
    fn fold_track(&mut self, track: Track, context: &FoldContext) -> Option<Track> {
        // Return None to filter out the track
        // Return Some(track) to include it
        Some(self.transform_track(track, context))
    }
}
```

## Real-World Examples

### Example 1: Auto-increment during parsing

```rust
// During track parsing, if you encounter "Snare 2" and "Snare" already exists,
// automatically put "Snare 2" in a separate group
let template = parse_tracks_to_template(tracks);
let template = template.fold(AutoIncrementGroupFolder::new());
```

### Example 2: Playlist matching

```rust
// When matching existing tracks, handle playlist variations
let template = template.fold(PlaylistMatchingFolder::new());

// Then when matching, check for playlist metadata
for track in &template.tracks {
    if track.get_metadata("fts.use_playlist") == Some(&"true".to_string()) {
        // Use playlists (fixed item lanes) for this track
    }
}
```

### Example 3: Context-aware grouping

```rust
struct SmartGroupingFolder {
    // Your logic here
}

impl TemplateFolder for SmartGroupingFolder {
    fn transform_track(&mut self, mut track: Track, context: &FoldContext) -> Track {
        let base_name = FoldContext::normalize_base_name(&track.name);
        
        // If multiple variants exist, create separate groups
        if context.count_by_base_name(&base_name) > 1 {
            track.set_parent_name(""); // Make it a root
            track.set_track_type("BUS");
        }
        
        track
    }
}
```

## Benefits

✅ **Immutable**: Creates new templates, never mutates originals  
✅ **Stateless**: Each transformation is independent  
✅ **Context-aware**: Can read from original template to make decisions  
✅ **Composable**: Chain multiple transformations easily  
✅ **Testable**: Each folder can be tested independently  
✅ **Functional**: Matches Rust's preference for immutability  

## Integration Points

The Fold pattern can be integrated into:

1. **Track parsing**: Apply folders after parsing tracks into templates
2. **Template matching**: Use folders to prepare templates for matching
3. **Template generation**: Apply folders during template generation
4. **Template filtering**: Use `ModeFilterFolder` instead of `filter_by_mode`

## Next Steps

1. Use `AutoIncrementGroupFolder` when parsing tracks with numbers (e.g., "Snare 2")
2. Use `PlaylistMatchingFolder` when handling playlist variations
3. Create custom folders for your specific transformation needs
4. Replace existing `filter_by_mode` calls with `ModeFilterFolder`
