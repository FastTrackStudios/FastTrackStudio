# Nested Groups Architecture

## Overview

Groups in the naming convention now support **recursive nesting** instead of the old super-group/sub-type hierarchy. This allows for flexible hierarchies of any depth.

## Structure

### Structure
```
FullGroup
  └── FullGroup (child)
      └── FullGroup (grandchild)
          └── FullGroup (great-grandchild)
              ... (unlimited depth)
```

## Example Hierarchy

```rust
// Band Section
let mut band_section = FullGroup::new("Band Section", "BAND");

// Drums (child of Band Section)
let mut drums = FullGroup::new("Drums", "D");

// Kick, Snare, Hi-Hat (children of Drums)
let mut kick = FullGroup::new("Kick", "K");
kick.patterns.push("kick".to_string());
kick.patterns.push("kickdrum".to_string());

drums.add_child(kick);
drums.add_child(FullGroup::new("Snare", "S"));
drums.add_child(FullGroup::new("Hi-Hat", "HH"));

band_section.add_child(drums);
band_section.add_child(FullGroup::new("Guitars", "GTR"));
```

This creates:
```
Band Section (BAND)
├── Drums (D)
│   ├── Kick (K)
│   ├── Snare (S)
│   └── Hi-Hat (HH)
└── Guitars (GTR)
```

## API

### Creating Groups

```rust
let mut group = FullGroup::new("Drums", "D");
```

### Adding Children

```rust
let mut drums = FullGroup::new("Drums", "D");
drums.add_child(FullGroup::new("Kick", "K"));
drums.add_child(FullGroup::new("Snare", "S"));
```

### Finding Children

```rust
// By name
if let Some(kick) = drums.find_child("Kick") {
    // Use kick group
}

// By prefix
if let Some(kick) = drums.find_child_by_prefix("K") {
    // Use kick group
}
```

### Traversing Hierarchy

```rust
// Get all descendants (children, grandchildren, etc.)
let all_descendants = group.all_descendants();

// Get depth of hierarchy
let depth = group.depth(); // 0 = no children, 1 = one level, etc.

// Count all descendants
let count = group.descendant_count();
```

### Helper Methods

```rust
// Check if group has children
if group.has_children() {
    // Process children
}

// Get patterns with child override
let arrangement_patterns = group.get_arrangement_patterns(child_group);
let section_patterns = group.get_section_patterns(child_group);
let multi_mic_patterns = group.get_multi_mic_patterns(child_group);
```

## Benefits

1. **Unified Structure**: Everything is a group - no distinction between groups and sub-types
2. **Unlimited Depth**: Nest as deeply as needed
3. **Simpler API**: One type (`FullGroup`) for all groups
4. **Consistent**: Matches the visibility module's `SortingGroupData` pattern
5. **Flexible**: Easy to reorganize hierarchies

## Tests

All tests pass (20 tests):
- Basic nesting
- Finding children
- Traversing descendants
- Depth calculation
- Descendant counting
- Pattern inheritance from parent to child

