# Supported Audio Groups

## Current Status

The **naming-convention** crate provides the **infrastructure** for defining groups, but does **not** include pre-defined groups. Groups are meant to be defined by parsers or configuration files.

However, the structure supports **any audio group hierarchy** you want to create.

## What the Structure Supports

### Group Capabilities

1. **Recursive Nesting** - Groups can contain child groups to any depth
2. **Pattern Matching** - Each group can have:
   - `patterns` - Strings/regexes that match track names
   - `negative_patterns` - Patterns to exclude
   - `priority` - For conflict resolution
3. **Component Patterns** - Groups can define patterns for:
   - Arrangements (Rhythm, Solo, Lead, etc.)
   - Sections (Verse, Chorus, Bridge, etc.)
   - Layers (DBL, OCT, L, R, etc.)
   - Multi-mic positions (Top, Bottom, In, Out, etc.)
   - Track types (BUS, SUM, DI, NOFX, etc.)
   - Rec tags (PASS-01, TAKE-02, etc.)
   - Performers (Cody, Joshua, etc.)
   - Playlists (.1, .2, .A, etc.)
4. **Group Types**:
   - `Static` - Standard group
   - `Increment` - Supports numbered instances (Tom 1, Tom 2)
   - `DynamicHierarchy` - Creates nested track structures dynamically

## Example Group Hierarchies

Based on the visibility module and common audio production needs, here are examples of what you **could** create:

### Band/Studio Groups

```
Band Section
├── Drums (D)
│   ├── Kick (K)
│   ├── Snare (S)
│   ├── Hi-Hat (HH)
│   ├── Toms (T)
│   ├── Cymbals (C)
│   ├── Overheads (OH)
│   └── Room (R)
├── Bass (B)
├── Guitars (GTR)
│   ├── Electric
│   ├── Acoustic
│   └── Bass
├── Keys (K)
│   ├── Piano
│   ├── Organ
│   └── Synth
├── Synths (SY)
├── Vocals (V)
│   ├── Lead
│   ├── Background (BGVS)
│   └── Choir
└── Orchestra
    ├── Strings
    ├── Winds
    ├── Brass
    └── Percussion
```

### More Detailed Examples

#### Drums Hierarchy
```
Drums (D)
├── Kick (K)
│   ├── Kick In
│   ├── Kick Out
│   └── Kick Sub
├── Snare (S)
│   ├── Snare Top
│   ├── Snare Bottom
│   └── Snare Room
├── Hi-Hat (HH)
├── Toms (T)
│   ├── Tom 1
│   ├── Tom 2
│   ├── Tom 3
│   └── Floor Tom
├── Cymbals (C)
│   ├── Crash
│   ├── Ride
│   └── Splash
├── Overheads (OH)
│   ├── OH L
│   └── OH R
└── Room (R)
```

#### Guitars Hierarchy
```
Guitars (GTR)
├── Electric
│   ├── Rhythm
│   ├── Lead
│   └── Crunch
├── Acoustic
│   ├── Steel String
│   └── Nylon String
└── Bass
    ├── Electric Bass
    └── Upright Bass
```

#### Orchestra Hierarchy
```
Orchestra
├── Strings
│   ├── Violins
│   ├── Violas
│   ├── Cellos
│   └── Basses
├── Winds
│   ├── Flutes
│   ├── Oboes
│   ├── Clarinets
│   └── Bassoons
├── Brass
│   ├── Trumpets
│   ├── Trombones
│   ├── French Horns
│   └── Tubas
└── Percussion
    ├── Timpani
    ├── Mallet Percussion
    └── Auxiliary
```

## Creating Groups

### Basic Example

```rust
use naming_convention::FullGroup;

// Create a drums group
let mut drums = FullGroup::new("Drums", "D");
drums.patterns.push("drum".to_string());
drums.patterns.push("drums".to_string());

// Add kick as a child
let mut kick = FullGroup::new("Kick", "K");
kick.patterns.push("kick".to_string());
kick.patterns.push("kickdrum".to_string());
kick.patterns.push("bd".to_string()); // bass drum

drums.add_child(kick);

// Add snare
let mut snare = FullGroup::new("Snare", "S");
snare.patterns.push("snare".to_string());
snare.patterns.push("sd".to_string());

drums.add_child(snare);
```

### With Component Patterns

```rust
let mut guitars = FullGroup::new("Guitars", "GTR");
guitars.patterns.push("guitar".to_string());
guitars.patterns.push("gtr".to_string());

// Define arrangement patterns
guitars.arrangement_patterns.push("Rhythm".to_string());
guitars.arrangement_patterns.push("Lead".to_string());
guitars.arrangement_patterns.push("Solo".to_string());
guitars.arrangement_patterns.push("Crunch".to_string());

// Define section patterns
guitars.section_patterns.push("Verse".to_string());
guitars.section_patterns.push("Chorus".to_string());
guitars.section_patterns.push("Bridge".to_string());

// Add electric as child
let mut electric = FullGroup::new("Electric", "ELEC");
electric.patterns.push("electric".to_string());
electric.patterns.push("elec".to_string());
guitars.add_child(electric);
```

## What's Missing

Currently, the naming-convention crate does **not** include:
- Pre-defined default groups (like Drums, Guitars, etc.)
- A parser implementation (that would load groups from config)
- Group configuration loading (from JSON/YAML)

These would typically be provided by:
1. A parser crate (like `fts-naming-parser`) that loads groups from config
2. A configuration file (like `config.json`) that defines all groups
3. Or programmatically defined groups in your application

## Next Steps

To actually use groups, you would:
1. Create a parser that implements `NamingConventionSource`
2. Load group definitions from a config file or define them programmatically
3. Use the groups to parse track names and match them to groups

The structure is ready - you just need to populate it with your group definitions!

