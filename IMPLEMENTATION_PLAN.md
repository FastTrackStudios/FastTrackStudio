# Monarchy Parser Implementation Plan

## Overview
Implement parsing logic in `libs/monarchy/src/parser.rs` to extract metadata from input strings based on group configurations. The parser should:
1. Extract metadata fields from nested groups (e.g., `multi_mic` from "Kick In")
2. Set the `group` field in metadata when a group matches
3. Set `original_name` in metadata
4. Handle different value types (String vs Vec<String>)

## Phase 1: Basic Metadata Extraction

### Step 1.1: Set `group` and `original_name` in metadata
**Location:** `libs/monarchy/src/parser.rs` - `parse()` method

**Changes:**
- After setting `matched_group`, also set it in metadata using the `group` field
- Set `original_name` field in metadata to the input string
- Need to find the Field enum variant for "group" and "original_name"

**Implementation:**
```rust
// After line 38 where matched_group is set:
if let Some(ref group_name) = matched_group {
    // Find the "group" field enum variant
    // Set it in metadata: metadata.set(ItemMetadataField::Group, ItemMetadataValue::Group(group_name.clone()))
}

// Set original_name
// metadata.set(ItemMetadataField::OriginalName, ItemMetadataValue::OriginalName(input.clone()))
```

**Challenge:** Need to dynamically find Field enum variants. Options:
- Use `M::fields()` to iterate and find by name
- Or add a helper method to Metadata trait to get field by name
- Or use a match on all possible fields (not scalable)

**Recommendation:** Add helper method to Metadata trait or use string-based field lookup.

### Step 1.2: Implement basic `extract_field_value` for direct patterns
**Location:** `libs/monarchy/src/parser.rs` - `extract_field_value()` method

**Current state:** Returns `None`

**Initial implementation:**
- Check if any of the group's patterns match the input
- For String fields: return the matched pattern (or first match)
- For Vec<String> fields: collect all matching patterns

**Example for "Kick In":**
- Group "Kick" matches
- Field `multi_mic` is in `group.metadata_fields`
- Need to check nested groups in `group.groups` for the `multi_mic` group
- The `multi_mic` group has patterns `["In", "Out", "Top", "Bottom"]`
- "In" matches, so return `ItemMetadataValue::MultiMic(vec!["In".to_string()])`

## Phase 2: Nested Group Extraction

### Step 2.1: Find nested groups for a field
**Location:** `libs/monarchy/src/parser.rs` - `extract_field_value()` method

**Problem:** When extracting `multi_mic` field, we need to find the nested group that was configured via `.multi_mic(multi_mic_group)`.

**Current structure:**
- `group.metadata_fields` contains `[ItemMetadataField::MultiMic]`
- `group.groups` contains the nested `multi_mic` Group
- But we need to know which nested group corresponds to which field

**Solution options:**
1. **Store field-to-group mapping in Group struct** (requires Group changes)
2. **Search nested groups by matching field name** (hacky, but works)
3. **Store field groups separately** (requires Group changes)

**Recommendation:** For now, search `group.groups` for a group that matches the field name pattern. The nested group name is "MultiMic" and the field is `MultiMic`, so we can match by name.

**Implementation:**
```rust
fn find_nested_group_for_field(
    &self,
    field: &M::Field,
    parent_group: &Group<M>
) -> Option<&Group<M>> {
    // Convert field enum to string representation
    let field_name = format!("{:?}", field);
    
    // Search nested groups for one that matches the field name
    for nested_group in &parent_group.groups {
        if nested_group.name == field_name || 
           nested_group.name.to_lowercase() == field_name.to_lowercase() {
            return Some(nested_group);
        }
    }
    None
}
```

### Step 2.2: Extract values from nested group patterns
**Location:** `libs/monarchy/src/parser.rs` - `extract_field_value()` method

**Implementation:**
1. Find nested group for the field (Step 2.1)
2. Check if nested group's patterns match the input
3. Extract matching patterns
4. Convert to appropriate Value type based on field type

**For "Kick In" with `multi_mic` field:**
- Find nested group named "MultiMic"
- Check patterns `["In", "Out", "Top", "Bottom"]` against input "Kick In"
- "In" matches
- Return `ItemMetadataValue::MultiMic(vec!["In".to_string()])`

**For String fields:**
- Return first matching pattern as String
- Return `ItemMetadataValue::Section("Intro".to_string())` for example

**For Vec<String> fields:**
- Collect all matching patterns
- Return `ItemMetadataValue::MultiMic(vec!["In".to_string()])`

## Phase 3: Value Type Handling

### Step 3.1: Determine field type from Field enum
**Location:** `libs/monarchy/src/parser.rs` - `extract_field_value()` method

**Problem:** Need to know if a field is `Option<String>` or `Option<Vec<String>>` to create the correct Value enum variant.

**Solution:** Use pattern matching on the Field enum and create corresponding Value enum variants. The derive macro generates matching variants, so we can match on Field and construct Value.

**Implementation approach:**
- Match on `field` to determine which Value variant to create
- For each field, check the nested group patterns
- Create appropriate Value based on field type

**Challenge:** This requires knowing all possible fields at compile time, which we do via the Field enum.

**Alternative:** Use a helper trait or method that maps Field -> expected Value type. But this might be complex.

**Simpler approach:** For now, hardcode the logic for known fields, or use a match statement that covers all Field variants.

## Phase 4: Testing and Refinement

### Step 4.1: Test with kick tests
- Run `cargo test --package dynamic-template kick::tests`
- Verify "Kick In" parses to `group: "Kick"`, `multi_mic: ["In"]`, `original_name: "Kick In"`
- Verify "Kick Out" parses to `group: "Kick"`, `multi_mic: ["Out"]`, `original_name: "Kick Out"`

### Step 4.2: Handle edge cases
- Multiple matches (e.g., "Kick In Out" - should extract both?)
- No matches (return None)
- Case sensitivity (already handled by `matches()` method)
- Special characters in patterns

### Step 4.3: Refactor and optimize
- Extract helper methods
- Improve error handling
- Add documentation

## Implementation Order

1. **Set `group` and `original_name`** (Step 1.1) - Simplest, gets basic fields working
2. **Basic pattern matching** (Step 1.2) - Extract direct patterns from group
3. **Nested group finding** (Step 2.1) - Find which nested group corresponds to a field
4. **Nested group extraction** (Step 2.2) - Extract values from nested group patterns
5. **Value type handling** (Step 3.1) - Properly construct Value enum variants
6. **Testing** (Phase 4) - Verify everything works

## Key Challenges

1. **Field enum to string mapping**: Need to convert `ItemMetadataField::MultiMic` to "MultiMic" or "multi_mic" to find nested group
2. **Value enum construction**: Need to create correct Value variant based on Field type
3. **Nested group lookup**: Need to find which nested group corresponds to which field
4. **Type safety**: Working with generic M::Field and M::Value while needing to know specific types

## Notes

- The `metadata_field()` method in GroupBuilder stores nested groups in `group.groups`, but doesn't maintain a direct field->group mapping
- We'll need to search `group.groups` to find the right nested group
- The nested group name should match the field name (e.g., "MultiMic" group for `MultiMic` field)
- Consider adding a helper method to Group to find nested group by field name

