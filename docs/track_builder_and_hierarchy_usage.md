# Track Builder and Hierarchy Usage Guide

## Overview

We now have easy-to-use functions and macros for building track structures with hierarchy using the Track builder pattern and fold patterns.

## Building Simple Tracks

### Using the `tracks!` macro

**Simple form** (name and group):
```rust
let expected = tracks!(
    ("Kick In", "kick"),
    ("Kick Out", "kick"),
    ("Snare Top", "snare"),
);
```

**With parent relationships**:
```rust
let expected = tracks!(
    ("Snare", "snare", None),  // Root track
    ("Snare Top", "snare", Some("Snare")),  // Child of Snare
    ("Snare Bottom", "snare", Some("Snare")),  // Child of Snare
);
```

**With track type**:
```rust
let expected = tracks!(
    ("Snare", "snare", None, Some("BUS")),  // Root with type
    ("Snare Top", "snare", Some("Snare"), None),  // Child without type
);
```

## Building Hierarchical Structures

### Using `build_tracks_with_hierarchy`

For more complex hierarchies, use the `build_tracks_with_hierarchy` function:

```rust
use fts::smart_template::template::parse_helpers::build_tracks_with_hierarchy;

let tracks = build_tracks_with_hierarchy(vec![
    // Root track: (name, group, track_type, parent, children)
    ("Snare", "snare", Some("BUS"), None, vec![
        // Children: (name, group, track_type, parent_override)
        ("Snare Top", "snare", None, None),
        ("Snare Bottom", "snare", None, None),
    ]),
    // Another root
    ("Kick", "kick", Some("BUS"), None, vec![
        ("Kick In", "kick", None, None),
        ("Kick Out", "kick", None, None),
    ]),
]);
```

This creates:
- "Snare" track with group "snare" and type "BUS" (root)
- "Snare Top" track with group "snare" and parent "Snare"
- "Snare Bottom" track with group "snare" and parent "Snare"
- "Kick" track with group "kick" and type "BUS" (root)
- "Kick In" track with group "kick" and parent "Kick"
- "Kick Out" track with group "kick" and parent "Kick"

## Parsing Track Names into Templates

### Using `parse_fts_structure`

Parse track names and automatically apply fold transformations:

```rust
use fts::smart_template::template::parse_helpers::parse_fts_structure;

let track_names = &[
    "Kick In",
    "Kick Out",
    "Snare Top",
    "Snare Bottom",
    "Snare 2",  // Will be auto-incremented into separate group
];

let template = parse_fts_structure(track_names);
```

This function:
1. Parses track names using all available parsers
2. Creates a Template with parsed tracks
3. Applies default fold transformations:
   - Auto-increment grouping (if "Snare" exists, "Snare 2" becomes separate group)
   - Playlist matching (handles "Snare .1", "Snare .2" variations)

### Custom Fold Transformations

```rust
use fts::smart_template::template::parse_helpers::{parse_fts_structure_with_folders, DefaultFolders};

let folders = DefaultFolders::new()
    .with_auto_increment(true)
    .with_playlist_matching(false);

let template = parse_fts_structure_with_folders(track_names, folders);
```

## Example: Rewriting Tests

### Before (old way):
```rust
let expected_tracks: Vec<Track> = vec![
    create_track_with_group("Kick In", "kick"),
    create_track_with_group("Kick Out", "kick"),
    create_track_with_group("Snare Top", "snare"),
];
```

### After (new way):
```rust
let expected_tracks = tracks!(
    ("Kick In", "kick"),
    ("Kick Out", "kick"),
    ("Snare Top", "snare"),
);
```

### With Hierarchy:
```rust
let expected_tracks = tracks!(
    ("Snare", "snare", None, Some("BUS")),
    ("Snare Top", "snare", Some("Snare"), None),
    ("Snare Bottom", "snare", Some("Snare"), None),
);
```

### Complex Hierarchy:
```rust
let expected_tracks = build_tracks_with_hierarchy(vec![
    ("Snare", "snare", Some("BUS"), None, vec![
        ("Snare (SUM)", "snare", Some("SUM"), None),
    ]),
]);

// Then add children of SUM
let mut sum_children = tracks!(
    ("Snare Top", "snare", Some("Snare (SUM)"), None),
    ("Snare Bottom", "snare", Some("Snare (SUM)"), None),
);
expected_tracks.extend(sum_children);
```

## Benefits

✅ **Uses Track builder pattern** - Consistent with the rest of the codebase  
✅ **Easy hierarchy definition** - Simple syntax for parent-child relationships  
✅ **Type-safe** - Compile-time checking of track structures  
✅ **Composable** - Can combine `tracks!` macro with `build_tracks_with_hierarchy`  
✅ **Works with fold patterns** - `parse_fts_structure` automatically applies transformations  

## Integration with Fold Patterns

The `parse_fts_structure` function uses fold patterns internally, so you get:
- **Auto-increment grouping**: If "Snare" exists and you parse "Snare 2", it automatically becomes a separate group
- **Playlist matching**: Tracks like "Snare .1" and "Snare .2" are properly handled
- **Context-aware**: Decisions are made based on what tracks already exist

This makes it perfect for parsing real track names and automatically organizing them into the correct structure!
