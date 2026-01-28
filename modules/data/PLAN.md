# Default Guitar Rig Config — Implementation Plan

## Summary

Add a default FTS-Guitar configuration as a `pub mod defaults` in the `data` crate, containing a `pub fn guitar_rig()` that returns a fully constructed rig with all module presets, top-level presets, profiles, songs, and a setlist. Also update `ModuleType` and `BlockType` to match the actual signal chain naming.

---

## Step 1: Rename and extend `ModuleType` enum

**File:** `src/module.rs`

Current → New mapping:
- `Transient = 30` → **rename to `Motion = 30`** (display: "Motion")
- `PostFx = 21` → **rename to `Time = 21`** (display: "Time")
- `Cabinet = 18` → **remove** (Cab is now part of the Amp module)
- **Add** `Master = 22` (display: "Master")

The Amp module now covers Amp + Cab + Room Send as blocks within the module preset, not as separate module types.

Update `display_name()` and `Display` impl. Update the exhaustive match in `typesafety_demo.rs`.

## Step 2: Extend `BlockType` enum

**File:** `src/block.rs`

**Add variants:**
- `Pitch` — for pitch/octave effects
- `Tremolo` — for tremolo/vibrato/rotary
- `Limiter` — for master limiter
- `Send` — for room send blocks
- `Special` — for envelope filter, wah, doubler, etc.
- `Freeze` — for freeze effects

This gives each module's blocks a more precise functional category.

## Step 3: Add `block_overrides` field to `Scene` and `SceneTemplate`

**Files:** `src/performance.rs`, `src/profile.rs`

Both `Scene` and `SceneTemplate` currently only have `module_overrides: Vec<ModuleOverride>`. Per the user's requirement, scenes can hold **both** module overrides AND block-level overrides for fine-grained per-scene tweaks on top of the loaded preset.

Add to both structs:
```rust
pub block_overrides: Vec<BlockOverride>,
```

With helper methods `add_block_override()` and `get_block_override()`.

This enables the "save only what's different" pattern — a scene loads its base preset + module snapshot, then applies sparse block overrides on top.

## Step 4: Create `src/defaults/mod.rs` and `src/defaults/guitar.rs`

**New files:** `src/defaults/mod.rs`, `src/defaults/guitar.rs`

Register in `src/lib.rs`:
```rust
pub mod defaults;
```

### `defaults/mod.rs`
```rust
pub mod guitar;
pub use guitar::*;
```

### `defaults/guitar.rs` — Return type

```rust
pub struct GuitarRigDefaults {
    pub rig: Rig,
    pub presets: Vec<Preset>,
    pub profiles: Vec<Profile>,
    pub songs: Vec<PerformanceSong>,
    pub setlist: PerformanceSetlist,
}

pub fn guitar_rig() -> GuitarRigDefaults { ... }
```

### Signal Chain Order (as module assignments in each preset)

| Order | Type | What |
|-------|------|------|
| 0 | Module | Source — "Guitar Input" |
| 1 | GlobalBlock | EQ Block |
| 2 | Module | Dynamics — "Compressor" |
| 3 | Module | Special — "Envelope Filter" / "Wah Pedal" / etc. |
| 4 | Module | Drive — "Blues Stack" / "Protein + JHS Kilt" / etc. |
| 5 | GlobalBlock | Volume Pedal |
| 6 | Module | PreFx — "Gravity Tank" |
| 7 | Module | Amp — "Dream and Ruby" / etc. (contains Amp + Cab + Room Send blocks) |
| 8 | GlobalBlock | Post EQ |
| 9 | Module | Modulation — "Chorus" / "Flanger" / "Phaser" |
| 10 | Module | Time — "Delay" / "Reverb" / "Freeze" |
| 11 | Module | Motion — "Tremolo 8th" / etc. |
| 12 | Module | Master — "EQ + Multiband Comp + Output Volume" |

### Module Presets to build (26 total)

**Source (1):**
- "Guitar Input" — blocks: [Input Gate, Input Volume]. Snapshots: Subtle Gate, No Gate, Heavy Gate

**Dynamics (1):**
- "Compressor" — blocks: [Compressor]

**Special (4):**
- "Envelope Filter" — blocks: [Envelope Filter]
- "Wah Pedal" — blocks: [Wah Pedal]
- "Pitch Octave FX" — blocks: [Pitch Octave]
- "Doubler" — blocks: [Doubler]

**Drive (2):**
- "Blues Stack" — blocks: [Boost, Drive 1, Drive 2, Drive 3]. Snapshots: Halfman, Halfman + Teal, Halfman + BluesBreaker
- "Protein + JHS Kilt" — blocks: [Boost, Drive 1, Drive 2, Drive 3]. Snapshots: Blue Light, Green Light, Blue Heavy, Green Heavy, Blue + Green

**PreFx (1):**
- "Gravity Tank" — blocks: [Delay, Reverb]. Snapshots: Harmonic Tremolo Light, Harmonic Tremolo Strong, Spring Reverb Light, Boing

**Amp (4):** (each contains Amp + Cab + Room Send blocks)
- "Dream and Ruby" — snapshots: Clean, Breakup, Drive
- "Deluxe and AC30" — snapshots: Clean, Breakup, Drive
- "Dumble and Two-Rock" — snapshots: Ultra-Clean, Breakup, Can't Find the Light, Roomy
- "Marshall Stack" — snapshots: Clean, Drive

**Modulation (3):**
- "Chorus", "Flanger", "Phaser" — each one block, no snapshots

**Time (3):**
- "Delay", "Reverb", "Freeze" — each one block, no snapshots

**Motion (5):**
- "Tremolo 8th", "Tremolo 16th", "Vibrato", "Rotary", "Too Much to Drink" — each one block, no snapshots

**Master (1):**
- "Master" — blocks: [EQ, Multiband Compressor, Output Volume]

### Top-Level Presets (10)

Each preset assigns module presets at specific orders + snapshots:

1. **"AC30 Ambient Clean"** — Generic/Clean — Amp: Dream and Ruby/Clean, Time: Reverb, Mod: Chorus
2. **"Tremolo Swells"** — Generic/Clean — Amp: Dream and Ruby/Clean, Motion: Tremolo 8th
3. **"80's Drive"** — Genre/Blues/Drive — Drive: Blues Stack/Halfman + Teal, Amp: Deluxe and AC30/Breakup
4. **"Stank"** — Genre/Rock/Drive — Drive: Protein + JHS Kilt/Blue + Green, Amp: Marshall Stack/Drive
5. **"Edge of Breakup"** — Genre/Blues/Crunch — Amp: Dumble and Two-Rock/Breakup
6. **"Sunday Morning"** — Generic/Clean — Amp: Dream and Ruby/Clean, Mod: Chorus, Time: Delay
7. **"Funk Machine"** — Genre/Funk/Clean — Special: Envelope Filter, Amp: Deluxe and AC30/Clean, Dynamics: Compressor
8. **"Ambient Swell"** — Generic/Ambient — Amp: Dream and Ruby/Clean, Time: Freeze, Motion: Vibrato
9. **"Country Twang"** — Genre/Country/Clean — Amp: Deluxe and AC30/Clean, Special: Doubler, Motion: Tremolo 8th
10. **"Heavy Riff"** — Genre/Rock/Drive — Drive: Protein + JHS Kilt/Green Heavy, Amp: Marshall Stack/Drive, Time: Delay

### Profiles (3)

**Worship** (8 scene templates):
- Clean → AC30 Ambient Clean, PreFx override: Gravity Tank / Spring Reverb Light
- Crunch → Edge of Breakup
- Drive → 80's Drive
- Lead → 80's Drive (snapshot variant possible)
- Ambient → Ambient Swell
- Tremolo → Tremolo Swells
- Delay → Sunday Morning
- Solo → Stank

**Blues** (8 scene templates):
- Clean → AC30 Ambient Clean
- Crunch → Edge of Breakup
- Drive → 80's Drive
- Lead → 80's Drive
- Funk → Funk Machine
- Q-Tron → Funk Machine (Special override: Envelope Filter)
- Roomy → Edge of Breakup (Amp snapshot override: Roomy)
- Solo → Stank

**Rock** (8 scene templates):
- Clean → AC30 Ambient Clean
- Crunch → Edge of Breakup
- Drive → Stank
- Lead → Heavy Riff
- Ambient → Ambient Swell
- Phaser → AC30 Ambient Clean (Mod override: Phaser)
- DLY Lead → Heavy Riff (Time override: Delay)
- Solo → Stank

### Songs (3)

**"Cryin' - Mateus Asato"** (4 scenes):
- Ambient → AC30 Ambient Clean
- Rhythm → Edge of Breakup
- Lead → 80's Drive
- Solo → Stank
- Song-level override: Time → Delay

**"Thriller - Dirty Loops"** (5 scenes):
- Crunch → Edge of Breakup
- Rock Lead → Stank
- Bridge → 80's Drive (scene-level Mod override: Phaser)
- Solo → Stank
- Ambient → AC30 Ambient Clean

**"Girl Goodbye - Toto"** (4 scenes):
- Drive → 80's Drive
- Verse → Edge of Breakup
- Chorus → Stank
- Solo → Heavy Riff

### Setlist

All 3 songs in order.

### Global Blocks on Rig (3)

1. EQ Block (order 1) — GlobalBlock
2. Volume Pedal (order 5) — GlobalBlock
3. Post EQ (order 8) — GlobalBlock

---

## Step 5: Update `guitar_rig.rs` integration test

Replace the existing test with one that calls `data::defaults::guitar_rig()` and validates the returned structure. This replaces the hand-built fixture with the real default config.

## Step 6: Update `typesafety_demo.rs`

Update the exhaustive `ModuleType` match to reflect the renamed/added variants.

## Step 7: Build, test, clippy — zero errors

---

## Files touched

| File | Change |
|------|--------|
| `src/module.rs` | Rename `Transient`→`Motion`, `PostFx`→`Time`, remove `Cabinet`, add `Master` |
| `src/block.rs` | Add `Pitch`, `Tremolo`, `Limiter`, `Send`, `Special`, `Freeze` variants |
| `src/performance.rs` | Add `block_overrides` to `Scene` |
| `src/profile.rs` | Add `block_overrides` to `SceneTemplate` |
| `src/lib.rs` | Add `pub mod defaults;` |
| `src/defaults/mod.rs` | **New** — module declaration |
| `src/defaults/guitar.rs` | **New** — full default config builder |
| `tests/guitar_rig.rs` | Rewrite to use `defaults::guitar_rig()` |
| `tests/typesafety_demo.rs` | Update exhaustive match |

All existing unit tests in affected files will be updated for the renames.
