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
- Use `M::fields()` to iterate and find the correct Field enum variants

**Implementation:**
```rust
// After line 38 where matched_group is set:
if let Some(ref group_name) = matched_group {
    // Iterate over all fields to find "group" field
    for field in M::fields() {
        let field_str = format!("{:?}", field);
        if field_str == "Group" || field_str.to_lowercase() == "group" {
            // Create Value variant - need to match on field to create correct Value
            // This will require matching on the Field enum
            metadata.set(field, create_value_for_field(field, group_name.clone()));
            break;
        }
    }
}

// Set original_name similarly
for field in M::fields() {
    let field_str = format!("{:?}", field);
    if field_str == "OriginalName" || field_str.to_lowercase() == "original_name" {
        metadata.set(field, create_value_for_field(field, input.clone()));
        break;
    }
}
```

**Note:** We'll need a helper function `create_value_for_field` that matches on Field enum to create the correct Value variant.

### Step 1.2: Implement `extract_field_value` using field iteration
**Location:** `libs/monarchy/src/parser.rs` - `extract_field_value()` method

**Current state:** Returns `None`

**Approach:** Use `M::fields()` to iterate over all possible fields and match based on field configurations.

**Key insight:** We have the full `config` available in the parser, and the `group` parameter contains:
- `group.metadata_fields` - list of fields this group cares about
- `group.groups` - nested groups that correspond to those fields

**Implementation strategy:**
1. Check if the field is in `group.metadata_fields` (if not, return None)
2. Find the nested group in `group.groups` that corresponds to this field
3. Extract matching patterns from that nested group
4. Convert to appropriate Value type

**Example for "Kick In" with `multi_mic` field:**
- Group "Kick" matches, `group.metadata_fields` contains `[ItemMetadataField::MultiMic]`
- Find nested group in `group.groups` named "MultiMic" (or matching field name)
- The nested group has patterns `["In", "Out", "Top", "Bottom"]`
- Check which patterns match "Kick In" -> "In" matches
- Return `ItemMetadataValue::MultiMic(vec!["In".to_string()])`

## Phase 2: Nested Group Extraction

### Step 2.1: Find nested groups for a field using field iteration
**Location:** `libs/monarchy/src/parser.rs` - `extract_field_value()` method

**Approach:** Use `M::fields()` to iterate and match field configurations.

**Current structure:**
- `group.metadata_fields` contains `[ItemMetadataField::MultiMic]` - fields this group cares about
- `group.groups` contains nested groups - one for each field in `metadata_fields`
- The nested group name should match the field name (e.g., "MultiMic" group for `MultiMic` field)

**Implementation:**
```rust
fn extract_field_value(
    &self,
    input: &str,
    field: &M::Field,
    group: &Group<M>,
) -> Option<M::Value> {
    // First check if this field is in the group's metadata_fields
    if !group.metadata_fields.contains(field) {
        return None;
    }
    
    // Convert field enum to string to find matching nested group
    let field_name = format!("{:?}", field);
    
    // Find nested group that matches this field
    // The nested group name should match the field name
    let nested_group = group.groups.iter()
        .find(|g| {
            g.name == field_name || 
            g.name.to_lowercase() == field_name.to_lowercase() ||
            g.name.to_lowercase().replace("_", "") == field_name.to_lowercase().replace("_", "")
        })?;
    
    // Extract matching patterns from nested group
    let matches = self.extract_patterns_from_group(input, nested_group);
    
    if matches.is_empty() {
        return None;
    }
    
    // Convert matches to Value based on field type
    self.create_value_for_field(field, matches)
}
```

**Note:** We need helper methods:
- `extract_patterns_from_group()` - finds which patterns match
- `create_value_for_field()` - converts matches to correct Value variant

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

### Step 3.1: Create Value from Field using pattern matching
**Location:** `libs/monarchy/src/parser.rs` - `create_value_for_field()` helper method

**Problem:** Need to create the correct Value enum variant based on Field enum and extracted matches.

**Solution:** Use exhaustive pattern matching on the Field enum. Since we're in the monarchy crate and working with generic `M::Field` and `M::Value`, we can't directly match. However, we can:

1. **Use the Metadata trait's get/set methods** - These already handle the conversion
2. **Match on field string representation** - Convert Field to string, match, create Value
3. **Use a helper that works with the actual type** - For ItemMetadata specifically

**Better approach:** Since we're generic over `M: Metadata`, we need a way to construct Values. The Metadata trait doesn't provide a way to construct Values from strings.

**Recommended solution:** 
- For now, we'll need to work with the actual ItemMetadata type in dynamic-template
- Or add a helper method to Metadata trait: `fn create_value(field: &Self::Field, value: String) -> Option<Self::Value>`
- Or use a match on Field enum variants (requires knowing all variants at compile time)

**Practical implementation:**
Since we're parsing ItemMetadata specifically, we can use a match statement that covers all ItemMetadataField variants. For a truly generic solution, we'd need to extend the Metadata trait.

**For Vec<String> fields (multi_mic, effect, unparsed_words):**
- Collect all matching patterns into Vec<String>
- Create Value variant with that Vec

**For String fields (everything else):**
- Take first matching pattern (or all if multiple)
- Create Value variant with that String

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

1. **Set `group` and `original_name`** (Step 1.1) - Use `M::fields()` to find and set these fields
2. **Implement `extract_field_value` with field iteration** (Step 1.2) - Use `M::fields()` and `group.metadata_fields` to find relevant fields
3. **Find nested groups for fields** (Step 2.1) - Match nested group names to field names
4. **Extract patterns from nested groups** (Step 2.2) - Check which patterns match the input
5. **Create Value variants** (Step 3.1) - Convert extracted patterns to correct Value enum variants
6. **Testing** (Phase 4) - Verify everything works

## Key Implementation Details

### Using M::fields() for Field Discovery
- Iterate over `M::fields()` to find fields by name
- Match field string representation (`format!("{:?}", field)`) to field names
- Use this for both `group`/`original_name` and custom fields

### Using group.metadata_fields
- Only process fields that are in `group.metadata_fields`
- This tells us which fields this group cares about
- For each field, find its corresponding nested group in `group.groups`

### Nested Group Matching
- Nested group name should match field name (e.g., "MultiMic" for `MultiMic` field)
- Handle case variations and underscore differences
- The nested group contains the patterns to match against

### Pattern Extraction
- For each pattern in nested group, check if it matches the input
- Use case-insensitive matching (already handled by `Group::matches()`)
- Collect all matches for Vec<String> fields, first match for String fields

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

