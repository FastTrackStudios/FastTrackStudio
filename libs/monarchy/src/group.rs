use crate::{IntoField, Metadata};
use serde::{Deserialize, Serialize};

/// Helper trait to convert single items or collections into a Vec
/// 
/// This allows builder methods to accept both single items and collections:
/// - Single item: `"pattern"` or `field` or `group`
/// - Collection: `vec!["pattern1", "pattern2"]` or `[field1, field2]` or `[group1, group2]`
pub trait IntoVec<T> {
    fn into_vec(self) -> Vec<T>;
}

// Implementation for single item
impl<T> IntoVec<T> for T {
    fn into_vec(self) -> Vec<T> {
        vec![self]
    }
}

// Implementation for Vec<T>
impl<T> IntoVec<T> for Vec<T> {
    fn into_vec(self) -> Vec<T> {
        self
    }
}

// Implementation for arrays [T; N]
impl<T, const N: usize> IntoVec<T> for [T; N] {
    fn into_vec(self) -> Vec<T> {
        self.into_iter().collect()
    }
}

// Implementation for slices &[T]
impl<T: Clone> IntoVec<T> for &[T] {
    fn into_vec(self) -> Vec<T> {
        self.to_vec()
    }
}

// Special implementation for patterns: convert &str and collections of &str to Vec<String>
impl IntoVec<String> for &str {
    fn into_vec(self) -> Vec<String> {
        vec![self.to_string()]
    }
}

impl IntoVec<String> for Vec<&str> {
    fn into_vec(self) -> Vec<String> {
        self.into_iter().map(|s| s.to_string()).collect()
    }
}

impl<const N: usize> IntoVec<String> for [&str; N] {
    fn into_vec(self) -> Vec<String> {
        self.into_iter().map(|s| s.to_string()).collect()
    }
}

impl IntoVec<String> for &[&str] {
    fn into_vec(self) -> Vec<String> {
        self.iter().map(|s| (*s).to_string()).collect()
    }
}

/// Defines a group pattern for organizing items
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Group<M: Metadata> {
    /// Name of this group
    pub name: String,

    /// Optional prefix to apply to this group and its children
    pub prefix: Option<String>,

    /// Whether this group should inherit prefixes from parent groups
    pub inherit_prefix: bool,

    /// Specific prefixes to block from being inherited (e.g., ["D"] to avoid "D Drum Kit")
    pub blocked_prefixes: Vec<String>,

    /// Patterns to match (any of these will match)
    pub patterns: Vec<String>,

    /// Patterns to exclude (none of these should match)
    pub negative_patterns: Vec<String>,

    /// Which metadata fields this group cares about
    pub metadata_fields: Vec<M::Field>,

    /// Nested groups (like folders)
    pub groups: Vec<Group<M>>,

    /// Priority for matching (higher = checked first)
    pub priority: i32,

    /// Optional variant field - items with different values for this field
    /// will be kept as separate variants within the same group
    pub tagged_collection: Option<M::Field>,

    /// If true, this group's children are promoted to the parent level in the output
    /// The group still exists for matching/organization but doesn't create nesting
    pub transparent: bool,

    /// If true, this group is used only for metadata extraction and does not create
    /// a structure node in the hierarchy. It will always match (or match with very
    /// low priority) to extract metadata fields like Section, MultiMic, etc.
    pub metadata_only: bool,
}

impl<M: Metadata> Group<M> {
    /// Start building a new group - this is the only way to create a Group
    pub fn builder(name: impl Into<String>) -> GroupBuilder<M> {
        GroupBuilder::new(name)
    }

    /// Check if a string matches this group's patterns
    pub fn matches(&self, text: &str) -> bool {
        let text_lower = text.to_lowercase();

        // Check negative patterns first
        for pattern in &self.negative_patterns {
            let pattern_lower = pattern.to_lowercase();
            if text_lower.contains(&pattern_lower) {
                return false;
            }
        }

        // Check positive patterns
        if self.patterns.is_empty() {
            // If no patterns specified, match by default
            return true;
        }

        for pattern in &self.patterns {
            let pattern_lower = pattern.to_lowercase();
            if text_lower.contains(&pattern_lower) {
                return true;
            }
        }

        false
    }
}

impl<M: Metadata> GroupBuilder<M> {
    /// Create a new GroupBuilder with the given name
    pub fn new(name: impl Into<String>) -> Self {
        GroupBuilder {
            group: Group {
                name: name.into(),
                prefix: None,
                inherit_prefix: true,
                blocked_prefixes: Vec::new(),
                patterns: Vec::new(),
                negative_patterns: Vec::new(),
                metadata_fields: Vec::new(),
                groups: Vec::new(),
                priority: 0,
                tagged_collection: None,
                transparent: false,
                metadata_only: false,
            },
        }
    }
}

/// Builder for creating Groups with a fluent API
pub struct GroupBuilder<M: Metadata> {
    /// The group being built (public for extension traits)
    pub group: Group<M>,
}

impl<M: Metadata> GroupBuilder<M> {
    /// Add patterns to match (accepts single pattern or collection)
    pub fn patterns<P>(mut self, patterns: P) -> Self
    where
        P: IntoVec<String>,
    {
        self.group.patterns.extend(patterns.into_vec());
        self
    }

    /// Add exclusion patterns (accepts single pattern or collection)
    pub fn exclude<P>(mut self, patterns: P) -> Self
    where
        P: IntoVec<String>,
    {
        self.group.negative_patterns.extend(patterns.into_vec());
        self
    }

    /// Add metadata fields this group cares about (accepts single field or collection)
    pub fn field<F>(mut self, fields: F) -> Self
    where
        F: IntoVec<M::Field>,
    {
        self.group.metadata_fields.extend(fields.into_vec());
        self
    }

    /// Add configuration for a specific metadata field
    /// 
    /// This is a generic method that works with any metadata field.
    /// The derive macro generates convenience methods (like `.multi_mic()`) that call this.
    /// 
    /// # Example
    /// 
    /// ```ignore
    /// Group::builder("Kick")
    ///     .metadata_field(ItemMetadataField::MultiMic, multi_mic_group)
    ///     .build()
    /// ```
    pub fn metadata_field<F>(mut self, field: M::Field, field_group: F) -> Self
    where
        F: IntoField<M>,
    {
        let field_group = field_group.into_field();
        self.group.metadata_fields.push(field);
        // Store the field_group itself as a nested group so we can find it later
        // This allows the parser to extract values from the field's patterns
        self.group.groups.push(field_group);
        self
    }

    /// Set a prefix for this group
    pub fn prefix(mut self, prefix: impl Into<String>) -> Self {
        self.group.prefix = Some(prefix.into());
        self
    }

    /// Disable prefix inheritance for this group
    pub fn no_inherit_prefix(mut self) -> Self {
        self.group.inherit_prefix = false;
        self
    }

    /// Block prefixes from being inherited (accepts single prefix or collection)
    pub fn block_prefix<P>(mut self, prefixes: P) -> Self
    where
        P: IntoVec<String>,
    {
        self.group.blocked_prefixes.extend(prefixes.into_vec());
        self
    }

    /// Add nested groups (accepts single group or collection)
    /// 
    /// Accepts anything that can be converted to Group<M>:
    /// - Single group: `Group<M>` or anything implementing `Into<Group<M>>`
    /// - Collections: `Vec<G>` or `[G; N]` where `G: Into<Group<M>>`
    pub fn group<G, I>(mut self, groups: G) -> Self
    where
        G: IntoVec<I>,
        I: Into<Group<M>>,
    {
        self.group.groups.extend(groups.into_vec().into_iter().map(Into::into));
        self
    }

    /// Set the priority for this group
    pub fn priority(mut self, priority: i32) -> Self {
        self.group.priority = priority;
        self
    }

    /// Set the variant field for this group
    /// Items with different values for this field will be kept as separate variants
    pub fn variant_field(mut self, field: M::Field) -> Self {
        self.group.tagged_collection = Some(field);
        self
    }

    /// Automatically use variant fields from metadata if they're marked with #[monarchy(variant)]
    pub fn use_auto_variants(mut self) -> Self {
        let variant_fields = M::variant_fields();
        if !variant_fields.is_empty() {
            // Use the first variant field if multiple are defined
            self.group.tagged_collection = Some(variant_fields[0].clone());
        }
        self
    }

    /// Make this group transparent - its children will be promoted to parent level
    /// Use this when you want logical grouping without creating nested output structures
    pub fn transparent(mut self) -> Self {
        self.group.transparent = true;
        self
    }

    /// Mark this group as metadata-only - it will extract metadata but not create
    /// a structure node in the hierarchy. Use this for global metadata field patterns
    /// like Section, MultiMic, etc. that should apply across all groups.
    pub fn metadata_only(mut self) -> Self {
        self.group.metadata_only = true;
        // Metadata-only groups should have very low priority so they don't interfere
        // with regular group matching, but still get checked for metadata extraction
        if self.group.priority == 0 {
            self.group.priority = -1000;
        }
        self
    }

    /// Build the final Group
    pub fn build(self) -> Group<M> {
        self.group
    }
}

// No longer need GroupProvider trait - use Into<Group<M>> instead
