use crate::Metadata;
use serde::{Deserialize, Serialize};

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
    pub variant_field: Option<M::Field>,
}

impl<M: Metadata> Group<M> {
    /// Start building a new group - this is the only way to create a Group
    pub fn builder(name: impl Into<String>) -> GroupBuilder<M> {
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
                variant_field: None,
            },
        }
    }

    /// Check if a string matches this group's patterns
    pub fn matches(&self, text: &str) -> bool {
        let text_lower = text.to_lowercase();

        // Check negative patterns first
        for pattern in &self.negative_patterns {
            if text_lower.contains(&pattern.to_lowercase()) {
                return false;
            }
        }

        // Check positive patterns
        if self.patterns.is_empty() {
            // If no patterns specified, match by default
            return true;
        }

        for pattern in &self.patterns {
            if text_lower.contains(&pattern.to_lowercase()) {
                return true;
            }
        }

        false
    }
}

/// Builder for creating Groups with a fluent API
pub struct GroupBuilder<M: Metadata> {
    group: Group<M>,
}

impl<M: Metadata> GroupBuilder<M> {
    /// Add a pattern to match
    pub fn pattern(mut self, pattern: impl Into<String>) -> Self {
        self.group.patterns.push(pattern.into());
        self
    }

    /// Add multiple patterns to match
    pub fn patterns(mut self, patterns: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.group
            .patterns
            .extend(patterns.into_iter().map(|p| p.into()));
        self
    }

    /// Add a negative pattern (exclusion)
    pub fn exclude(mut self, pattern: impl Into<String>) -> Self {
        self.group.negative_patterns.push(pattern.into());
        self
    }

    /// Add multiple negative patterns
    pub fn excludes(mut self, patterns: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.group
            .negative_patterns
            .extend(patterns.into_iter().map(|p| p.into()));
        self
    }

    /// Add a metadata field this group cares about
    pub fn field(mut self, field: M::Field) -> Self {
        self.group.metadata_fields.push(field);
        self
    }

    /// Add multiple metadata fields
    pub fn fields(mut self, fields: impl IntoIterator<Item = M::Field>) -> Self {
        self.group.metadata_fields.extend(fields);
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

    /// Block specific prefixes from being inherited
    pub fn block_prefix(mut self, prefix: impl Into<String>) -> Self {
        self.group.blocked_prefixes.push(prefix.into());
        self
    }

    /// Block multiple prefixes from being inherited
    pub fn block_prefixes(mut self, prefixes: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.group
            .blocked_prefixes
            .extend(prefixes.into_iter().map(|p| p.into()));
        self
    }

    /// Add a nested group
    pub fn group<G>(mut self, group: G) -> Self
    where
        G: Into<Group<M>>,
    {
        self.group.groups.push(group.into());
        self
    }

    /// Add multiple nested groups
    pub fn groups<G>(mut self, groups: impl IntoIterator<Item = G>) -> Self
    where
        G: Into<Group<M>>,
    {
        self.group.groups.extend(groups.into_iter().map(Into::into));
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
        self.group.variant_field = Some(field);
        self
    }

    /// Automatically use variant fields from metadata if they're marked with #[monarchy(variant)]
    pub fn use_auto_variants(mut self) -> Self {
        let variant_fields = M::variant_fields();
        if !variant_fields.is_empty() {
            // Use the first variant field if multiple are defined
            self.group.variant_field = Some(variant_fields[0].clone());
        }
        self
    }

    /// Build the final Group
    pub fn build(self) -> Group<M> {
        self.group
    }
}

// No longer need GroupProvider trait - use Into<Group<M>> instead
