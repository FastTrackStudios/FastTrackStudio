# Default Groups Configuration

## Overview

The `default-groups` feature provides pre-configured audio groups based on the fts-naming-parser configuration. These groups use the new nested structure (no sub-types or super groups).

## Kick Group

The Kick group is the first default group implemented, following the structure from fts-naming-parser.

### Structure

```
Kick (K)
├── In (IN)
├── Out (OUT)
├── Trigger (TRIG)
└── Sub (SUB)
```

### Main Kick Group

- **Name**: "Kick"
- **Prefix**: "K"
- **Patterns**: `["kick", "kik", "bd", "bassdrum"]`
- **Negative Patterns**: `["keys", "guitar", "gtr", "k", "bass"]`
- **Parent Track**: "D KICK (Sum)"

**Pattern Arrays**:
- **Arrangement**: Thump, Click, Sub, Beater, Shell, Fundamental, Harmonic, Trig
- **Layers**: Click, Thump, Attack, Body
- **Multi-Mic**: (empty - handled by nested children)

### Nested Children

#### In Group
- **Name**: "In"
- **Prefix**: "IN"
- **Patterns**: `["in"]`
- **Arrangement**: Inside, Internal, Beater, Attack

#### Out Group
- **Name**: "Out"
- **Prefix**: "OUT"
- **Patterns**: `["out"]`
- **Arrangement**: Outside, External, Shell, Body

#### Trigger Group
- **Name**: "Trigger"
- **Prefix**: "TRIG"
- **Patterns**: `["trig", "trigger"]`
- **Arrangement**: Trigger, Sample, Replacement

#### Sub Group
- **Name**: "Sub"
- **Prefix**: "SUB"
- **Patterns**: `["sub"]`
- **Parent Track**: "D KICK (BUS)"
- **Arrangement**: Deep, Low, Fundamental, Thump

## Usage

### Enable the Feature

Add to your `Cargo.toml`:

```toml
[dependencies]
naming-convention = { path = "../naming-convention", features = ["default-groups"] }
```

### Create Drums Group

```rust
use naming_convention::default_groups::create_drums_group;

let drums = create_drums_group();

// Access children
if let Some(kick) = drums.find_child("Kick") {
    println!("Found kick group: {}", kick.name);
    
    // Access nested children
    if let Some(sub) = kick.find_child("Sub") {
        println!("Found kick sub: {}", sub.name);
    }
}
```

### Create All Default Groups

```rust
use naming_convention::default_groups::create_default_groups;

let groups = create_default_groups();
// Currently returns: [Drums]
// TODO: Will include Bass, Guitars, Keys, etc.
```

## Test Results

✅ **23 tests passing** (including 3 new default-groups tests):
- `test_create_drums_group` - Verifies drums group structure
- `test_drums_group_structure` - Verifies all child groups exist
- `test_drums_group_patterns` - Verifies pattern arrays are populated

## Next Steps

1. ✅ **Drums** - COMPLETE
2. ⏳ **Bass** - TODO
3. ⏳ **Guitars** - TODO
4. ⏳ **Keys** - TODO
5. ⏳ **Synths** - TODO
6. ⏳ **Vocals** - TODO
7. ⏳ **Orchestra** - TODO

## Notes

- All groups use the new nested `FullGroup` structure (no sub-types)
- Patterns match the fts-naming-parser config.json exactly
- Group types are set where appropriate (e.g., Tom uses `Increment`)
- Parent tracks are preserved from the original config

