# Parsing Flow: Track Name → ItemProperties

This document describes the complete flow for parsing a track name like `"D Kick In .02"` into `ItemProperties` using the fold pattern with full config (all groups).

## High-Level Flow

```
Input: "D Kick In .02"
  ↓
Phase 1: Group Matching (Group Folder)
  ↓
Phase 2: Component Parsing (Component Folder)
  ↓
Output: ItemProperties
```

## Detailed Flow

### Entry Point

```rust
parse_fts_item_properties("D Kick In .02", None)
```

### Phase 1: Group Matching (Fold over Groups)

**Goal**: Find the best matching group from all available groups.

**Input**: 
- Track name: `"D Kick In .02"`
- All groups: `[Drums, Kick, Snare, Tom, Cymbals, Rooms, Bass, Guitar Electric, ...]`

**Process** (Fold Pattern):

1. **Initialize accumulator**: `best_match = None`, `best_score = 0`

2. **Fold over each group** (in priority order):
   ```
   For each group in groups:
     - Check if name matches group patterns
     - Check negative patterns (reject if match)
     - Calculate match score (priority + pattern matches)
     - If score > best_score: update best_match
   ```

3. **Example iteration for "D Kick In .02"**:
   ```
   Group: Drums
     - Pattern "D" matches ✓
     - No negative patterns match ✓
     - Score: 5 (priority)
     - best_match = Drums, best_score = 5
   
   Group: Kick (child of Drums)
     - Pattern "Kick" matches ✓
     - Negative patterns: "Keys", "Guitar" - no match ✓
     - Score: 5 + 1 = 6
     - best_match = Kick, best_score = 6
   
   Group: Snare
     - Pattern "Snare" doesn't match ✗
     - Skip
   
   ... (continue for all groups)
   ```

4. **Result**: `GroupConfig` for "Kick" group

**Output**: 
- Matched `GroupConfig`: Kick group config
- Extracted: `group_prefix = "D"`, `sub_type = ["Kick"]`
- Marked words: `{"d", "kick"}`

### Phase 2: Component Parsing (Fold over Components in Order)

**Goal**: Parse remaining components in `ComponentOrder`.

**Input**:
- Track name: `"D Kick In .02"`
- Matched group config: Kick `GroupConfig`
- Already matched words: `{"d", "kick"}`
- Remaining words: `["In", ".02"]`

**Process** (Fold Pattern):

1. **Initialize accumulator**: `ItemProperties` with:
   ```rust
   ItemProperties {
     original_name: Some("D Kick In .02"),
     group_prefix: Some("D"),
     sub_type: Some(vec!["Kick"]),
     ..Default::default()
   }
   ```

2. **Create ParseContext**:
   ```rust
   ParseContext {
     original_name: "D Kick In .02",
     name_lower: "d kick in .02",
     matched_words: {"d", "kick"},
     group_config: Some(KickGroupConfig),
   }
   ```

3. **Fold over components in ComponentOrder**:
   ```
   ComponentOrder::default() = [
     RecTag,
     GroupPrefix,      // Already set in Phase 1
     SubType,          // Already set in Phase 1
     Performer,
     Arrangement,
     Section,
     Layers,
     MultiMic,
     Effect,
     TrackType,
     Increment,
     Channel,
     Playlist,
   ]
   ```

4. **Iteration through components**:

   **RecTag**:
   - Check patterns: `["REC", "RECORDING", ...]`
   - No match in remaining words
   - Result: `NotFound`
   - Props unchanged

   **GroupPrefix**:
   - Already set in Phase 1: `"D"`
   - Skip (placeholder in order)

   **SubType**:
   - Already set in Phase 1: `["Kick"]`
   - Skip (placeholder in order)

   **Performer**:
   - Check patterns: `["John", "Jane", ...]`
   - No match
   - Result: `NotFound`
   - Props unchanged

   **Arrangement**:
   - Check patterns: `["Thump", "Click", ...]`
   - No match
   - Result: `NotFound`
   - Props unchanged

   **Section**:
   - Check patterns: `["Verse", "Chorus", ...]`
   - No match
   - Result: `NotFound`
   - Props unchanged

   **Layers**:
   - Check patterns: `["Layer1", "Layer2", ...]`
   - No match
   - Result: `NotFound`
   - Props unchanged

   **MultiMic** (Important!):
   - Check group-specific patterns from Kick config:
     - `["In", "Out", "Trig", "Sub", "Ambient"]`
   - Match found: `"In"` ✓
   - Mark `"in"` as matched
   - Update context: `matched_words = {"d", "kick", "in"}`
   - Result: `Found("In")`
   - Props: `multi_mic = Some(vec!["In"])`

   **Effect**:
   - Check patterns: `["Verb", "Delay", ...]`
   - No match
   - Result: `NotFound`
   - Props unchanged

   **TrackType**:
   - Check for parentheses: `"(BUS)"`, `"(AUX)"`
   - No match
   - Result: `NotFound`
   - Props unchanged

   **Increment**:
   - Check patterns: `["1", "2", "3", ...]`
   - No match
   - Result: `NotFound`
   - Props unchanged

   **Channel**:
   - Check patterns: `["L", "R", "M", "S"]`
   - No match
   - Result: `NotFound`
   - Props unchanged

   **Playlist** (Important!):
   - Check patterns: `[".1", ".2", ".A", ".B", ...]`
   - Match found: `".02"` ✓
   - Mark `".02"` as matched
   - Update context: `matched_words = {"d", "kick", "in", ".02"}`
   - Result: `Found(".02")`
   - Props: `playlist = Some(".02")`

5. **Collect unparsed words**:
   - All words matched: `{"d", "kick", "in", ".02"}`
   - Unparsed: `[]`
   - Props: `unparsed_words = None`

### Final Output

```rust
ItemProperties {
    original_name: Some("D Kick In .02"),
    rec_tag: None,
    group_prefix: Some("D"),
    sub_type: Some(vec!["Kick"]),
    performer: None,
    arrangement: None,
    section: None,
    layers: None,
    multi_mic: Some(vec!["In"]),
    effect: None,
    increment: None,
    channel: None,
    playlist: Some(".02"),
    track_type: None,
    unparsed_words: None,
    file_extension: None,
}
```

## Key Benefits of Fold Pattern

1. **Two-Phase Separation**:
   - Phase 1: Group matching (which group?)
   - Phase 2: Component parsing (what components?)

2. **Order Independence**:
   - Components parsed in `ComponentOrder` order
   - Can change order without changing component parsers

3. **Group-Specific Patterns**:
   - Each component parser checks group config for custom patterns
   - Example: MultiMic uses Kick-specific patterns `["In", "Out", ...]`

4. **Stateless Transformation**:
   - Each phase transforms input → output
   - No mutation of original data
   - Easy to test and reason about

5. **Composability**:
   - Can swap `GroupFolder` implementation
   - Can swap `ComponentOrder`
   - Can add/remove component parsers

## Example: Complex Name

**Input**: `"REC D Kick In Verb .02"`

**Phase 1**: Matches Kick group (same as above)

**Phase 2**:
- RecTag: `Found("REC")` → `rec_tag = Some("REC")`
- MultiMic: `Found("In")` → `multi_mic = Some(vec!["In"])`
- Effect: `Found("Verb")` → `effect = Some(vec!["Verb"])`
- Playlist: `Found(".02")` → `playlist = Some(".02")`

**Result**: All components extracted correctly!

## Logging Points

For debugging, you'd want to log:

1. **Phase 1 Logs**:
   ```
   [GROUP_MATCH] Checking group: Drums
   [GROUP_MATCH] Pattern "D" matched, score: 5
   [GROUP_MATCH] Checking group: Kick
   [GROUP_MATCH] Pattern "Kick" matched, score: 6
   [GROUP_MATCH] Best match: Kick (score: 6)
   ```

2. **Phase 2 Logs**:
   ```
   [COMPONENT] Parsing RecTag: NotFound
   [COMPONENT] Parsing MultiMic: Found("In")
   [COMPONENT] Parsing Effect: NotFound
   [COMPONENT] Parsing Playlist: Found(".02")
   [COMPONENT] Unparsed words: []
   ```
