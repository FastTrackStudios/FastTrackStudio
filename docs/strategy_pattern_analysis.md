# Strategy Pattern Analysis

## What is the Strategy Pattern?

The Strategy pattern is a behavioral design pattern that enables you to define a family of algorithms, encapsulate each one, and make them interchangeable. It lets the algorithm vary independently from clients that use it.

### Key Benefits:
1. **Separation of Concerns**: The algorithm's implementation is separated from the code that uses it
2. **Dependency Inversion**: High-level modules don't depend on low-level modules; both depend on abstractions
3. **Open/Closed Principle**: Open for extension (new strategies), closed for modification
4. **Easier Testing**: Each strategy can be tested independently
5. **Runtime Flexibility**: Strategies can be swapped at runtime

### In Rust:
Rust implements the Strategy pattern primarily through:
- **Traits**: Define the strategy interface
- **Structs**: Implement different strategies
- **Generic Functions**: Accept any strategy that implements the trait

## Current Usage in This Codebase

Your codebase **already uses** the Strategy pattern in several places:

### ✅ Already Using Strategy Pattern

1. **ConfigLoader Trait** (`modules/fts/src/smart_template/shared/config_loader.rs`)
   - `JsonConfigLoader` implements `ConfigLoader`
   - Could easily add `YamlConfigLoader`, `TomlConfigLoader`, etc.
   - ✅ **Well-designed example of Strategy pattern**

2. **TemplateGenerator Trait** (`modules/fts/src/smart_template/template/traits.rs`)
   - Multiple implementations: `KickTemplateGenerator`, `SnareTemplateGenerator`, `TomTemplateGenerator`, etc.
   - Each generates templates differently
   - ✅ **Good use of Strategy pattern**

3. **Matcher Trait** (`modules/fts/src/smart_template/template/traits.rs`)
   - Different matchers: `KickMatcher`, `SnareMatcher`, `DrumKitMatcher`, etc.
   - Each has different matching logic
   - ✅ **Good use of Strategy pattern**

## Opportunities for Improvement

### 🔴 High Priority: Template Display Modes

**Location**: `modules/fts/src/smart_template/template/display.rs`

**Current Implementation** (lines 138-144):
```rust
pub fn display_with_mode(&self, mode: TemplateDisplayMode) -> String {
    match mode {
        TemplateDisplayMode::Full => format!("{}", self),
        TemplateDisplayMode::WithModes => self.display_with_modes(),
        TemplateDisplayMode::Minimal => self.display_minimal(),
    }
}
```

**Problem**: 
- Uses a `match` statement to select display strategy
- Adding new display modes requires modifying this match statement
- Display logic is mixed with selection logic

**Strategy Pattern Solution**:
```rust
// Define the strategy trait
pub trait DisplayStrategy {
    fn format(&self, template: &Template) -> String;
}

// Implement strategies
pub struct FullDisplay;
impl DisplayStrategy for FullDisplay {
    fn format(&self, template: &Template) -> String {
        format!("{}", template)
    }
}

pub struct WithModesDisplay;
impl DisplayStrategy for WithModesDisplay {
    fn format(&self, template: &Template) -> String {
        template.display_with_modes()
    }
}

pub struct MinimalDisplay;
impl DisplayStrategy for MinimalDisplay {
    fn format(&self, template: &Template) -> String {
        template.display_minimal()
    }
}

// Use the strategy
impl Template {
    pub fn display_with_strategy<S: DisplayStrategy>(&self, strategy: S) -> String {
        strategy.format(self)
    }
    
    // Convenience method that maps enum to strategy
    pub fn display_with_mode(&self, mode: TemplateDisplayMode) -> String {
        match mode {
            TemplateDisplayMode::Full => self.display_with_strategy(FullDisplay),
            TemplateDisplayMode::WithModes => self.display_with_strategy(WithModesDisplay),
            TemplateDisplayMode::Minimal => self.display_with_strategy(MinimalDisplay),
        }
    }
}
```

**Benefits**:
- Easy to add new display strategies (e.g., `JsonDisplay`, `MarkdownDisplay`)
- Each strategy is independently testable
- Can compose strategies (e.g., `WithModesDisplay` + color formatting)
- Strategies can be passed around and stored

### 🔴 High Priority: Inheritance Mode Strategy

**Location**: `modules/fts/src/smart_template/shared/implementations/json_config_loader.rs`

**Current Implementation** (lines 67-84):
```rust
fn load_with_inheritance(
    &self,
    defaults: &str,
    overrides: &str,
    mode: InheritanceMode,
) -> Result<Self::Config, Self::Error> {
    match mode {
        InheritanceMode::DefaultOnly => {
            self.load(defaults)
        }
        InheritanceMode::OverrideOnly => {
            self.load(overrides)
        }
        InheritanceMode::DefaultPlusOverride => {
            let mut default_config = self.load(defaults)?;
            let override_config = self.load(overrides)?;
            default_config.merge(&override_config);
            Ok(default_config)
        }
    }
}
```

**Problem**:
- Merge logic is embedded in the match statement
- Different merge strategies (deep merge, shallow merge, priority-based) require modifying this code
- Hard to test merge logic independently

**Strategy Pattern Solution**:
```rust
pub trait MergeStrategy {
    fn merge(&self, defaults: &TemplateConfig, overrides: &TemplateConfig) -> TemplateConfig;
}

pub struct DefaultOnlyMerge;
impl MergeStrategy for DefaultOnlyMerge {
    fn merge(&self, defaults: &TemplateConfig, _overrides: &TemplateConfig) -> TemplateConfig {
        defaults.clone()
    }
}

pub struct OverrideOnlyMerge;
impl MergeStrategy for OverrideOnlyMerge {
    fn merge(&self, _defaults: &TemplateConfig, overrides: &TemplateConfig) -> TemplateConfig {
        overrides.clone()
    }
}

pub struct DefaultPlusOverrideMerge;
impl MergeStrategy for DefaultPlusOverrideMerge {
    fn merge(&self, defaults: &TemplateConfig, overrides: &TemplateConfig) -> TemplateConfig {
        let mut result = defaults.clone();
        result.merge(overrides);
        result
    }
}

// In JsonConfigLoader
impl ConfigLoader for JsonConfigLoader {
    fn load_with_inheritance(
        &self,
        defaults: &str,
        overrides: &str,
        mode: InheritanceMode,
    ) -> Result<Self::Config, Self::Error> {
        let default_config = self.load(defaults)?;
        let override_config = self.load(overrides)?;
        
        let strategy: Box<dyn MergeStrategy> = match mode {
            InheritanceMode::DefaultOnly => Box::new(DefaultOnlyMerge),
            InheritanceMode::OverrideOnly => Box::new(OverrideOnlyMerge),
            InheritanceMode::DefaultPlusOverride => Box::new(DefaultPlusOverrideMerge),
        };
        
        Ok(strategy.merge(&default_config, &override_config))
    }
}
```

**Benefits**:
- Easy to add new merge strategies (e.g., `DeepMerge`, `PriorityBasedMerge`)
- Merge logic is testable independently
- Can compose merge strategies
- Could even allow custom merge strategies to be passed in

### 🟡 Medium Priority: Track Matching Strategies

**Location**: `modules/fts/src/smart_template/template/track_matcher.rs`

**Current State**: Already uses some strategy-like patterns, but could be enhanced.

**Opportunity**: The `match_track` method has a fixed order of matching strategies:
1. Exact name match
2. Normalized match (without playlist)
3. Template-based matching

**Potential Enhancement**: Make the matching order configurable via a strategy:
```rust
pub trait MatchingStrategy {
    fn match_track<T: TrackNameLike>(
        &self,
        parsed_name: &T,
        matcher: &TrackMatcher,
        template: Option<&Template>,
    ) -> Option<TrackMatch>;
}

pub struct ExactFirstStrategy;
impl MatchingStrategy for ExactFirstStrategy {
    // Current implementation
}

pub struct FuzzyFirstStrategy;
impl MatchingStrategy for FuzzyFirstStrategy {
    // Try fuzzy matching first, then exact
}
```

### 🟡 Medium Priority: Group Mode Filtering

**Location**: `modules/fts/src/smart_template/template/traits.rs`

**Current State**: The `filter_by_mode` method uses a match-like pattern internally.

**Opportunity**: Could extract the filtering logic into strategies:
```rust
pub trait FilterStrategy {
    fn should_include(&self, track: &Track, mode: GroupMode) -> bool;
}

pub struct StandardFilter;
impl FilterStrategy for StandardFilter {
    fn should_include(&self, track: &Track, mode: GroupMode) -> bool {
        let modes = track.get_modes();
        modes.is_empty() || modes.contains(&mode)
    }
}

pub struct StrictFilter;
impl FilterStrategy for StrictFilter {
    fn should_include(&self, track: &Track, mode: GroupMode) -> bool {
        let modes = track.get_modes();
        !modes.is_empty() && modes.contains(&mode)
    }
}
```

## Recommendations

### Priority 1: Refactor Template Display Modes
This is the clearest candidate. The display logic is already separated into methods, making it easy to extract into strategies.

### Priority 2: Refactor Inheritance Mode
The merge logic is more complex and would benefit from being separated into strategies, especially if you plan to add more merge modes.

### Priority 3: Consider Strategy Pattern for Future Features
When adding new features that have multiple ways of doing the same thing, consider using the Strategy pattern from the start rather than match statements.

## When NOT to Use Strategy Pattern

The Strategy pattern adds complexity. Don't use it when:
- You only have 1-2 implementations that are unlikely to change
- The selection logic is trivial (a simple if/else)
- Performance is critical and the trait indirection would hurt
- The implementations are tightly coupled to the context

## Summary

Your codebase already demonstrates good use of the Strategy pattern with traits like `ConfigLoader`, `TemplateGenerator`, and `Matcher`. The main opportunities are:

1. **Template Display Modes** - Extract display formatting into strategies
2. **Inheritance Mode Merging** - Extract merge logic into strategies
3. **Future Features** - Consider Strategy pattern when adding new multi-implementation features

The pattern would make your code more extensible, testable, and maintainable, especially as you add more display formats or merge strategies.
