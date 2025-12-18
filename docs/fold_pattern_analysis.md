# Fold Pattern Analysis

## What is the Fold Pattern?

The Fold pattern is used to transform recursive data structures (like trees or ASTs) by:
- **Traversing** each node in the structure
- **Transforming** each node (optionally)
- **Building** a new structure from the transformed nodes
- **Preserving** the hierarchical relationships

### Key Characteristics:
1. **Recursive transformation**: Processes nodes recursively, handling children
2. **Immutable**: Creates new structures rather than mutating existing ones
3. **Composable**: Different folders can be combined or chained
4. **Type-safe**: The type system ensures correct transformations

### In Rust:
The Fold pattern is implemented using:
- A `Folder` trait that defines transformation methods for each node type
- Default implementations that recursively fold children
- Concrete implementations that override specific transformation logic

## Current Usage in This Codebase

### ✅ Already Using Similar Patterns

1. **Template Display** (`modules/fts/src/smart_template/template/display.rs`)
   - Recursively traverses track hierarchy
   - Transforms tracks into display format
   - However, it's **mutable** (writes to formatter) rather than building a new structure

2. **Template Filtering** (`modules/fts/src/smart_template/template/traits.rs`)
   - `filter_by_mode()` creates a new template from an existing one
   - Transforms the track collection
   - However, it's **imperative** (loops and sets) rather than using a fold pattern

## Opportunities for Improvement

### 🔴 High Priority: Template Transformation Fold

**Location**: `modules/fts/src/smart_template/template/`

**Problem**: 
Currently, template transformations (like `filter_by_mode`) use imperative loops and manual tree traversal. A Fold pattern would:
- Make transformations more composable
- Separate traversal logic from transformation logic
- Make it easier to add new transformations
- Make the code more functional and easier to reason about

**Fold Pattern Solution**:
```rust
// In modules/fts/src/smart_template/template/fold.rs

use crate::smart_template::template::traits::Template;
use crate::smart_template::template::track_helpers::TrackExt;
use daw::tracks::Track;

/// Trait for folding (transforming) templates
pub trait TemplateFolder {
    /// Fold a track node
    /// 
    /// Default implementation recursively folds children.
    /// Override this to transform individual tracks.
    fn fold_track(&mut self, track: Track) -> Track {
        let mut folded = self.transform_track(track.clone());
        
        // Recursively fold children (tracks that have this track as parent)
        // Note: In a real implementation, we'd need access to the full template
        // to find children. This is a simplified example.
        
        folded
    }
    
    /// Transform a single track (without children)
    /// 
    /// Override this to change track properties.
    fn transform_track(&mut self, track: Track) -> Track {
        track // Default: no transformation
    }
    
    /// Fold an entire template
    fn fold_template(&mut self, template: Template) -> Template {
        // Build a map of parent -> children for efficient lookup
        let children_map = self.build_children_map(&template);
        
        // Fold root tracks (tracks without parents)
        let root_tracks: Vec<Track> = template.tracks
            .iter()
            .filter(|t| t.parent_name().is_none())
            .map(|t| self.fold_track_recursive(t, &template, &children_map))
            .collect();
        
        Template {
            name: self.transform_template_name(&template.name),
            tracks: root_tracks,
        }
    }
    
    /// Transform template name
    fn transform_template_name(&mut self, name: &str) -> String {
        name.to_string() // Default: no change
    }
    
    /// Recursively fold a track and its children
    fn fold_track_recursive(
        &mut self,
        track: &Track,
        template: &Template,
        children_map: &std::collections::HashMap<String, Vec<&Track>>,
    ) -> Track {
        // Transform this track
        let mut folded_track = self.transform_track(track.clone());
        
        // Find and fold children
        if let Some(children) = children_map.get(&track.name) {
            // Note: In a real implementation, we'd need to handle
            // the hierarchical structure properly. This is simplified.
        }
        
        folded_track
    }
    
    /// Build a map of parent name -> children tracks
    fn build_children_map(&self, template: &Template) -> std::collections::HashMap<String, Vec<&Track>> {
        let mut map = std::collections::HashMap::new();
        for track in &template.tracks {
            if let Some(parent) = track.parent_name() {
                map.entry(parent.to_string())
                    .or_insert_with(Vec::new)
                    .push(track);
            }
        }
        map
    }
}

// Example: Mode filter folder
pub struct ModeFilterFolder {
    mode: crate::smart_template::shared::GroupMode,
    include_parents: bool,
}

impl ModeFilterFolder {
    pub fn new(mode: crate::smart_template::shared::GroupMode) -> Self {
        Self {
            mode,
            include_parents: true,
        }
    }
}

impl TemplateFolder for ModeFilterFolder {
    fn transform_track(&mut self, track: Track) -> Track {
        use crate::smart_template::template::track_helpers::TrackExt;
        
        let modes = track.get_modes();
        let should_include = modes.is_empty() || modes.contains(&self.mode);
        
        if should_include {
            track
        } else {
            // Return a marker that this track should be filtered out
            // In a real implementation, we'd use Option<Track> or a different approach
            track
        }
    }
    
    fn fold_template(&mut self, template: Template) -> Template {
        use crate::smart_template::template::track_helpers::TrackExt;
        
        // First pass: collect tracks that match the mode
        let mut included = std::collections::HashSet::new();
        for track in &template.tracks {
            let modes = track.get_modes();
            if modes.is_empty() || modes.contains(&self.mode) {
                included.insert(track.name.clone());
            }
        }
        
        // Second pass: include parents if their children are included
        if self.include_parents {
            let mut changed = true;
            while changed {
                changed = false;
                for track in &template.tracks {
                    if included.contains(&track.name) {
                        continue;
                    }
                    
                    // Check if any child is included
                    let has_included_child = template.tracks.iter().any(|child| {
                        child.parent_name().map(|p| p == &track.name).unwrap_or(false)
                            && included.contains(&child.name)
                    });
                    
                    if has_included_child {
                        included.insert(track.name.clone());
                        changed = true;
                    }
                }
            }
        }
        
        // Build filtered template
        let filtered_tracks: Vec<Track> = template.tracks
            .iter()
            .filter(|t| included.contains(&t.name))
            .cloned()
            .collect();
        
        Template {
            name: format!("{} ({})", template.name, self.mode.as_str()),
            tracks: filtered_tracks,
        }
    }
}

// Example: Track renamer folder
pub struct TrackRenamerFolder {
    rename_map: std::collections::HashMap<String, String>,
}

impl TrackRenamerFolder {
    pub fn new() -> Self {
        Self {
            rename_map: std::collections::HashMap::new(),
        }
    }
    
    pub fn add_rename(&mut self, from: impl Into<String>, to: impl Into<String>) {
        self.rename_map.insert(from.into(), to.into());
    }
}

impl TemplateFolder for TrackRenamerFolder {
    fn transform_track(&mut self, mut track: Track) -> Track {
        if let Some(new_name) = self.rename_map.get(&track.name) {
            track.name = new_name.clone();
        }
        track
    }
}

// Example: Track property modifier
pub struct TrackPropertyModifier {
    volume_multiplier: Option<f64>,
    mute_all: bool,
}

impl TrackPropertyModifier {
    pub fn new() -> Self {
        Self {
            volume_multiplier: None,
            mute_all: false,
        }
    }
    
    pub fn with_volume_multiplier(mut self, multiplier: f64) -> Self {
        self.volume_multiplier = Some(multiplier);
        self
    }
    
    pub fn with_mute_all(mut self, mute: bool) -> Self {
        self.mute_all = mute;
        self
    }
}

impl TemplateFolder for TrackPropertyModifier {
    fn transform_track(&mut self, mut track: Track) -> Track {
        if let Some(multiplier) = self.volume_multiplier {
            track.volume *= multiplier;
        }
        
        if self.mute_all {
            track.muted = true;
        }
        
        track
    }
}

// Usage:
let template = /* ... */;

// Filter by mode
let filtered = ModeFilterFolder::new(GroupMode::Recording)
    .fold_template(template.clone());

// Rename tracks
let mut renamer = TrackRenamerFolder::new();
renamer.add_rename("Kick", "Bass Drum");
let renamed = renamer.fold_template(template.clone());

// Modify properties
let modified = TrackPropertyModifier::new()
    .with_volume_multiplier(0.8)
    .with_mute_all(false)
    .fold_template(template.clone());

// Compose transformations
let result = ModeFilterFolder::new(GroupMode::Recording)
    .fold_template(
        TrackPropertyModifier::new()
            .with_volume_multiplier(0.9)
            .fold_template(template)
    );
```

**Benefits**:
- **Composable**: Transformations can be chained
- **Testable**: Each folder can be tested independently
- **Extensible**: Easy to add new transformations
- **Functional**: Immutable transformations are easier to reason about
- **Reusable**: Folders can be reused across different templates

### 🟡 Medium Priority: Track Hierarchy Fold

**Location**: `modules/fts/src/smart_template/template/`

**Problem**: 
The current `filter_by_mode` and similar methods manually traverse the tree structure. A proper fold would handle the hierarchy more elegantly.

**Enhanced Fold Pattern**:
```rust
/// Enhanced folder that properly handles hierarchical structures
pub trait HierarchicalFolder {
    /// Fold a track and all its descendants
    fn fold_track_tree(
        &mut self,
        track: &Track,
        get_children: impl Fn(&Track) -> Vec<&Track>,
    ) -> Track {
        // Transform this track
        let mut folded = self.transform_track(track.clone());
        
        // Get and fold children
        let children = get_children(track);
        let folded_children: Vec<Track> = children
            .iter()
            .map(|child| self.fold_track_tree(child, &get_children))
            .collect();
        
        // Update parent references in children
        for mut child in folded_children {
            child.set_parent_name(&folded.name);
            // Add child to template (in real implementation)
        }
        
        folded
    }
    
    fn transform_track(&mut self, track: Track) -> Track;
}
```

### 🟢 Low Priority: Display Fold

**Location**: `modules/fts/src/smart_template/template/display.rs`

**Current Implementation**: 
Uses mutable `fmt::Formatter` and imperative recursion.

**Fold Pattern Alternative**:
```rust
/// Folder that builds a display string
pub struct DisplayFolder {
    show_modes: bool,
    output: String,
}

impl DisplayFolder {
    pub fn new(show_modes: bool) -> Self {
        Self {
            show_modes,
            output: String::new(),
        }
    }
    
    pub fn into_string(self) -> String {
        self.output
    }
}

impl TemplateFolder for DisplayFolder {
    fn fold_template(&mut self, template: Template) -> Template {
        self.output.push_str(&format!("Template: {}\n\n", template.name));
        
        // Fold tracks (which will build the display)
        let _ = super::TemplateFolder::fold_template(self, template);
        
        // Return empty template since we're just building display
        Template {
            name: String::new(),
            tracks: Vec::new(),
        }
    }
    
    fn transform_track(&mut self, track: Track) -> Track {
        // Build display string for this track
        // (simplified - real implementation would handle hierarchy)
        self.output.push_str(&format!("  {}\n", track.name));
        track
    }
}
```

## Recommendations

### Priority 1: Implement Template Fold Trait
Create a `TemplateFolder` trait that allows transforming templates in a composable way. This would replace or complement the current `filter_by_mode` approach.

### Priority 2: Refactor filter_by_mode to Use Fold
Once the fold pattern is implemented, refactor `filter_by_mode` to use it. This demonstrates the pattern and provides a migration path.

### Priority 3: Add Common Folders
Implement common folders:
- `ModeFilterFolder` - Filter by group mode
- `TrackRenamerFolder` - Rename tracks
- `TrackPropertyModifier` - Modify track properties
- `TemplateValidator` - Validate template structure

## When NOT to Use Fold Pattern

Don't use the Fold pattern when:
- The transformation is trivial (a simple map is sufficient)
- You need to mutate the original structure in place
- Performance is critical and the overhead of building new structures is too high
- The data structure is not recursive or tree-like

## Comparison with Current Approach

### Current Approach (Imperative)
```rust
pub fn filter_by_mode(&self, mode: GroupMode) -> Template {
    let mut included = HashSet::new();
    // ... loops and manual logic ...
    Template { name: ..., tracks: ... }
}
```

### Fold Pattern Approach (Functional)
```rust
let filtered = ModeFilterFolder::new(mode).fold_template(template);
```

**Advantages of Fold**:
- More composable
- Easier to test
- Separates traversal from transformation
- Can chain multiple transformations
- More functional/immutable style

**Disadvantages of Fold**:
- More complex for simple cases
- Requires understanding the pattern
- May have slight performance overhead

## Summary

The Fold pattern would be highly beneficial for your `Template` structure because:

1. **Templates are hierarchical** - Tracks have parent-child relationships
2. **Multiple transformations needed** - Filtering, renaming, property modification
3. **Composability desired** - You might want to chain transformations
4. **Current code is imperative** - Fold would make it more functional

The main opportunity is in `modules/fts/src/smart_template/template/` where templates are transformed. Implementing a `TemplateFolder` trait would make transformations more composable and easier to extend.
