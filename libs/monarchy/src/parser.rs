use crate::{Config, Item, Metadata, MonarchyError, Result};

/// Parser that converts input strings into Items with metadata
pub struct Parser<M: Metadata> {
    config: Config<M>,
}

impl<M: Metadata> Parser<M> {
    /// Create a new parser with the given configuration
    pub fn new(config: Config<M>) -> Self {
        Self { config }
    }

    /// Parse an input string into an Item with metadata
    pub fn parse(&self, input: String) -> Result<Item<M>> {
        let mut metadata = M::default();
        let mut matched_group = None;

        // Sort groups by priority (metadata_only groups have low priority)
        let mut sorted_groups = self.config.groups.clone();
        sorted_groups.sort_by(|a, b| b.priority.cmp(&a.priority));

        // First, extract metadata from metadata_only groups (they always match or have low priority)
        for group in &sorted_groups {
            if group.metadata_only && group.matches(&input) {
                // Extract metadata from metadata-only groups
                for field in &group.metadata_fields {
                    if let Some(value) = self.extract_field_value(&input, field, group) {
                        metadata.set(field.clone(), value);
                    }
                }
            }
        }

        // Find the first matching non-metadata-only group
        for group in &sorted_groups {
            if !group.metadata_only && group.matches(&input) {
                matched_group = Some(group.name.clone());

                // Extract metadata based on the group's fields
                for field in &group.metadata_fields {
                    if let Some(value) = self.extract_field_value(&input, field, group) {
                        metadata.set(field.clone(), value);
                    }
                }

                break;
            }
        }

        // Handle unmatched items based on fallback strategy
        if matched_group.is_none() {
            if let crate::config::FallbackStrategy::Reject = self.config.fallback_strategy {
                return Err(MonarchyError::NoMatch(input));
            }
        }

        Ok(Item {
            id: generate_id(),
            original: input,
            metadata,
            matched_group,
        })
    }

    /// Extract a value for a specific field from the input
    fn extract_field_value(
        &self,
        _input: &str,
        _field: &M::Field,
        _group: &crate::Group<M>,
    ) -> Option<M::Value> {
        // This is a simplified extraction - in practice, you'd have more sophisticated logic
        // based on the field type and parser rules

        // For now, we'll just check if any of the group's patterns appear in the input
        // Real implementation would parse based on field type
        None
    }
}

/// Generate a unique ID for an item
fn generate_id() -> String {
    // Simple counter-based ID for now
    use std::sync::atomic::{AtomicUsize, Ordering};
    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    format!("item_{}", COUNTER.fetch_add(1, Ordering::SeqCst))
}
