# Naming Convention Domain Module

## Status

✅ **Basic Structure**: Set up with `TrackName` and `ComponentType`  
✅ **Group Support**: Recursively nestable `FullGroup` structure  
✅ **Compiles**: Both naming-convention and visibility crates compile successfully

## Current Structure

### TrackName

The core struct representing a parsed track name with all components:

```rust
pub struct TrackName {
    pub rec_tag: Option<String>,           // "PASS-01", "TAKE-02"
    pub group_prefix: Option<String>,       // "D", "GTR", "Bass"
    pub sub_type: Option<Vec<String>>,      // ["Kick"], ["Electric"]
    pub performer: Option<String>,          // "Cody", "Joshua"
    pub arrangement: Option<String>,        // "Rhythm", "Solo"
    pub section: Option<String>,            // "Intro", "Verse", "Chorus"
    pub layers: Option<String>,             // "DBL", "OCT", "L", "R"
    pub multi_mic: Option<Vec<String>>,     // ["Top", "Bottom"]
    pub increment: Option<String>,          // "1", "2"
    pub playlist: Option<String>,           // ".1", ".2", ".A"
    pub track_type: Option<String>,         // "BUS", "SUM", "DI"
    pub unparsed_words: Option<Vec<String>>, // Words that didn't match
    pub file_extension: Option<String>,     // ".wav", ".aiff"
}
```

### ComponentType

Enum representing the different components:

```rust
pub enum ComponentType {
    RecTag,
    GroupPrefix,
    SubType,
    Performer,
    Arrangement,
    Section,
    Layers,
    MultiMic,
    Increment,
    Playlist,
    TrackType,
    Unknown,
}
```

### Group Structure

Uses recursively nestable `FullGroup` structure - groups can contain child groups to any depth.

## Next Steps

1. ✅ **TrackName struct** - DONE
2. ⏳ **ComponentType enum** - DONE
3. ⏳ **Basic Group structure** - DONE (basic version)
4. ⏳ **NamingConventionSource trait** - TODO (for parsing)
5. ⏳ **Formatter** - TODO (TrackName → String)
6. ⏳ **Simplified nested groups** - TODO (replace sub_types with children)

## Integration

- **Visibility module**: ✅ Uses `FullGroup` from naming-convention
- **Parser**: Will be in separate crate (fts-naming-parser equivalent)
- **Formatter**: Will convert `TrackName` back to strings

## Example Usage

```rust
use naming_convention::TrackName;

let mut track_name = TrackName::new();
track_name.group_prefix = Some("D".to_string());
track_name.sub_type = Some(vec!["Kick".to_string()]);
track_name.section = Some("Verse".to_string());
track_name.performer = Some("Cody".to_string());

// Check components
assert!(track_name.has_component(ComponentType::GroupPrefix));
assert_eq!(track_name.component_count(), 4);
assert!(track_name.is_valid());
```

