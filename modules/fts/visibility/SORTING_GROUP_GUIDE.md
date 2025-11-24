# SortingGroup System - Complete Guide

## Overview

The `SortingGroup` system provides a **simple, nestable group structure** for organizing tracks in the visibility management domain. It's designed to be:
- **Simple**: Just groups with children - no supergroups, no sub-types
- **Flexible**: Nest to any depth
- **Type-safe**: Use Rust types or concrete instances
- **Auto-convenient**: Auto-generates IDs, inherits prefixes

## Architecture

### Two-Layer Design

1. **`SortingGroup` Trait**: Defines groups as Rust types (zero-sized)
   - Implemented by types like `Drums`, `Orchestra`, `Bass`
   - Provides static configuration via `config()`
   - Can be converted to concrete `SortingGroupData`

2. **`SortingGroupData` Struct**: Concrete runtime data
   - Contains all the actual group information
   - Has `children: Vec<SortingGroupData>` for nesting
   - Can be created directly or from a `SortingGroup` type

### Core Structure

```rust
pub struct SortingGroupData {
    // Identity
    pub id: String,                    // Unique ID (e.g., "DRUMS")
    pub name: String,                  // Display name (e.g., "Drums")
    pub prefix: String,                // Naming prefix (e.g., "D")
    
    // Naming convention integration
    pub naming_group: Option<String>,  // Reference to naming convention group
    
    // Track scope (which tracks belong to this group)
    pub scope: TrackScope,
    
    // State
    pub is_global: bool,               // Affects all tracks?
    pub active: bool,                  // Currently visible?
    pub view_mode: Option<ViewMode>,   // Override global view mode
    
    // Visual
    pub color: Option<u32>,            // ARGB color
    pub icon: Option<String>,
    
    // Snapshots
    pub selected_tcp_snapshot: Option<String>,
    pub selected_mcp_snapshot: Option<String>,
    
    // NESTED CHILDREN - This is the key!
    pub children: Vec<SortingGroupData>,
}
```

## Key Features

### 1. Simple Nesting

Just `children: Vec<SortingGroupData>` - that's it! No special types, no supergroups.

```rust
let mut drums = SortingGroupData::new("DRUMS", "Drums");
drums.add_child_by_name("Kick");   // Creates "DRUMS_KICK"
drums.add_child_by_name("Snare");  // Creates "DRUMS_SNARE"
```

### 2. Auto-Generated IDs

When you add a child, the ID is automatically generated from parent + child name:

```rust
drums.add_child_by_name("Kick");
// Child ID becomes: "DRUMS_KICK"

drums.add_child_by_name("Hi-Hat");
// Child ID becomes: "DRUMS_HI-HAT" (spaces become underscores)
```

### 3. Prefix Inheritance

Children automatically inherit the parent's prefix:

```rust
let mut drums = SortingGroupData::new("DRUMS", "Drums");
drums.prefix = "D".to_string();

let kick = drums.add_child_by_name("Kick");
// kick.prefix is now "D" (inherited from parent)
```

### 4. Type-Based Groups

Define groups as Rust types for clean, reusable definitions:

```rust
pub struct Drums;
impl SortingGroup for Drums {
    fn config() -> GroupConfig {
        GroupConfig {
            id: "DRUMS",
            name: "Drums",
            prefix: "D",
            children: vec!["Kick", "Snare", "Hi-Hat", "Toms", "Cymbals"],
        }
    }
}
```

Then use them directly:

```rust
let mut root = SortingGroupData::new("ROOT", "All Groups");
root.add_child(Drums);      // Just pass the type!
root.add_child(Orchestra);  // Nested children included automatically
```

### 5. Flexible Child Addition

You can add children in multiple ways:

```rust
// Method 1: By name (simplest)
drums.add_child_by_name("Kick");

// Method 2: From a type
drums.add_child(Drums);  // If Drums had a child type

// Method 3: From concrete instance
let mut kick = SortingGroupData::new("KICK", "Kick");
drums.add_child(kick);

// Method 4: Builder pattern (returns mutable ref)
drums.add_child_by_name("Hi-Hat")
    .prefix = "HH".to_string();  // Customize after creation
```

## Usage Patterns

### Pattern 1: Declarative Type-Based Groups

**Best for**: Reusable, predefined groups

```rust
// Define once
pub struct Orchestra;
impl SortingGroup for Orchestra {
    fn config() -> GroupConfig {
        GroupConfig {
            id: "ORCHESTRA",
            name: "Orchestra",
            prefix: "Orch",
            children: vec!["Winds", "Brass", "Strings", "Orch Percussion", "Harp"],
        }
    }
}

// Use anywhere
let orchestra = Orchestra::build();
// Automatically includes all children: Winds, Brass, Strings, etc.
```

### Pattern 2: Dynamic Construction

**Best for**: Runtime-created groups, user-defined groups

```rust
let mut drums = SortingGroupData::new("DRUMS", "Drums");
drums.prefix = "D".to_string();

// Add children dynamically
drums.add_child_by_name("Kick");
drums.add_child_by_name("Snare");

// Can customize children
let hihat = drums.add_child_by_name("Hi-Hat");
hihat.prefix = "HH".to_string();
hihat.color = Some(0xFF0000FF);  // Red color
```

### Pattern 3: Mixed Approach

**Best for**: Combining predefined and dynamic groups

```rust
let mut root = SortingGroupData::new("ROOT", "All Groups");

// Add predefined groups
root.add_child(Drums);      // Includes all Drums children
root.add_child(Orchestra);  // Includes all Orchestra children

// Add custom dynamic group
let mut custom = root.add_child_by_name("Custom Group");
custom.prefix = "CUSTOM".to_string();
custom.add_child_by_name("Sub Group 1");
custom.add_child_by_name("Sub Group 2");
```

## Track Scope

Groups define which tracks they control via `TrackScope`:

```rust
pub struct TrackScope {
    pub parent_track: Option<TrackIdentifier>,      // Main parent track
    pub additional_tracks: Vec<TrackIdentifier>,     // Extra tracks
    pub include_children: bool,                      // Include child tracks?
    pub recursive_children: bool,                    // Recursively include?
}
```

**Example**:
```rust
let mut drums = SortingGroupData::new("DRUMS", "Drums");

// Set parent track (includes all children by default)
drums.set_parent_track(TrackIdentifier::guid("drum-bus-guid"));

// Add additional tracks
drums.add_track(TrackIdentifier::name("Drum Room Mic"));

// Scope now includes:
// - The drum bus track
// - All children of the drum bus (recursively)
// - The room mic track
```

## Recursive Operations

### Finding Children

```rust
// Find direct child
if let Some(kick) = drums.find_child("DRUMS_KICK") {
    // Found!
}

// Find nested child (searches recursively)
if let Some(nested) = drums.find_child("DRUMS_KICK_SUB") {
    // Found even if deeply nested!
}

// Mutable version
if let Some(kick) = drums.find_child_mut("DRUMS_KICK") {
    kick.active = true;
}
```

### Getting All Descendants

```rust
let all_descendants = drums.all_descendants();
// Returns: [Kick, Snare, Hi-Hat, Toms, Cymbals, ...]
// Includes all nested children at any depth
```

### Checking Hierarchy

```rust
if drums.has_children() {
    // Has at least one child
}

for child in &drums.children {
    // Iterate direct children
    if child.has_children() {
        // Child also has children (nested)
    }
}
```

## Integration with Naming Convention

Groups can reference naming convention groups:

```rust
use fts_domain::naming_convention::FullGroup;

// Create from naming convention group
let naming_group: FullGroup = /* ... */;
let visibility_group = SortingGroupData::from_naming_group(&naming_group);

// The visibility group now has:
// - Same name and prefix as naming group
// - Reference stored in naming_group field
// - Can be used to match tracks by naming convention
```

## Real-World Examples

### Example 1: Drums Group

```rust
pub struct Drums;
impl SortingGroup for Drums {
    fn config() -> GroupConfig {
        GroupConfig {
            id: "DRUMS",
            name: "Drums",
            prefix: "D",
            children: vec!["Kick", "Snare", "Hi-Hat", "Toms", "Cymbals", "Overheads", "Room"],
        }
    }
}

// When built:
let drums = Drums::build();
// Creates:
// - DRUMS (parent)
//   - DRUMS_KICK
//   - DRUMS_SNARE
//   - DRUMS_HI-HAT
//   - DRUMS_TOMS
//   - DRUMS_CYMBALS
//   - DRUMS_OVERHEADS
//   - DRUMS_ROOM
```

### Example 2: Orchestra with Nested Groups

```rust
pub struct Orchestra;
impl SortingGroup for Orchestra {
    fn config() -> GroupConfig {
        GroupConfig {
            id: "ORCHESTRA",
            name: "Orchestra",
            prefix: "Orch",
            children: vec!["Winds", "Brass", "Strings", "Orch Percussion", "Harp"],
        }
    }
}

// Could be extended with nested children:
let mut orchestra = Orchestra::build();
let mut winds = orchestra.find_child_mut("ORCHESTRA_WINDS").unwrap();
winds.add_child_by_name("Flute");
winds.add_child_by_name("Clarinet");
winds.add_child_by_name("Oboe");

// Result:
// - ORCHESTRA
//   - ORCHESTRA_WINDS
//     - ORCHESTRA_WINDS_FLUTE
//     - ORCHESTRA_WINDS_CLARINET
//     - ORCHESTRA_WINDS_OBOE
//   - ORCHESTRA_BRASS
//   - ORCHESTRA_STRINGS
//   ...
```

### Example 3: Dynamic User Groups

```rust
// User creates a custom group
let mut user_group = SortingGroupData::with_name("My Custom Group");
user_group.prefix = "CUSTOM".to_string();
user_group.color = Some(0x00FF00FF);  // Green

// Add tracks
user_group.set_parent_track(TrackIdentifier::guid("some-track-guid"));

// Add sub-groups
user_group.add_child_by_name("Sub Group 1");
user_group.add_child_by_name("Sub Group 2");

// Customize sub-group
let sub1 = user_group.find_child_mut("MY_CUSTOM_GROUP_SUB_GROUP_1").unwrap();
sub1.active = true;
sub1.view_mode = Some(ViewMode::Exclusive);
```

## Comparison with Naming Parser Structure

### Current Naming Parser (Complex)
```
SuperGroup
  └── Group
      └── SubType
          └── SubType (nested)
```

### SortingGroup (Simple)
```
Group
  └── Group (children)
      └── Group (children)
```

**Key Differences**:
- ✅ No SuperGroups - just nest groups
- ✅ No SubTypes - just groups with different names
- ✅ One concept instead of three
- ✅ Simpler mental model
- ✅ Easier to maintain

## Benefits

1. **Simplicity**: One concept (Group) instead of multiple (SuperGroup, Group, SubType)
2. **Flexibility**: Nest to any depth without special handling
3. **Type Safety**: Use Rust types for compile-time safety
4. **Convenience**: Auto-generated IDs, inherited prefixes
5. **Consistency**: Same pattern throughout visibility domain
6. **Maintainability**: Less code, fewer edge cases

## API Summary

### Creating Groups

```rust
// From scratch
SortingGroupData::new("ID", "Name")
SortingGroupData::with_name("Name")  // Auto-generates ID

// From naming convention
SortingGroupData::from_naming_group(&naming_group)

// From type
Drums::build()
Orchestra::build()
```

### Adding Children

```rust
group.add_child_by_name("Child Name")           // Simplest
group.add_child(SomeGroupType)                  // From type
group.add_child(concrete_group_data)            // From instance
```

### Finding Children

```rust
group.find_child("ID")                          // Immutable, recursive
group.find_child_mut("ID")                      // Mutable, recursive
group.has_children()                            // Check if has children
group.all_descendants()                         // Get all descendants
```

### Track Scope

```rust
group.set_parent_track(track_id)
group.add_track(track_id)
```

## Type Alias

For convenience, there's a type alias:

```rust
pub type Group = SortingGroupData;
```

So you can use either:
- `SortingGroupData` (explicit)
- `Group` (shorter, but conflicts with naming convention `Group` trait)

## See Also

- `main/domain/src/visibility/group.rs` - Core implementation
- `main/domain/src/visibility/group_config.rs` - Configuration struct
- `main/domain/src/visibility/default_groups/` - Example group definitions
- `main/domain/src/visibility/README.md` - General visibility domain docs

