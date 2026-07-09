# PRD: Signal Snapshots & Parameter Modulation System

A comprehensive snapshot and modulation system for the signal-ui rig control, inspired by REAPER's Snapshooter (tilr) for parameter morphing and Track Snapshot (Daniel Lumertz) for complete state replacement.

## Background

Guitar rigs need to transition between different configurations during performances:
- **Scene changes**: Switch from clean verse to heavy chorus
- **Smooth morphs**: Gradually blend between two amp settings
- **Song presets**: Load completely different effect chains per song

### Reference Implementations

**Snapshooter (tilr)** - Parameter-level morphing:
- Captures FX parameters, volume, pan, mute, sends
- Diffs current state vs snapshot (only changed params)
- Morphs between two snapshots A/B with slider (0-100%)
- Tweening with easing functions (linear, ease in/out)
- Writes transitions as automation points to timeline

**Track Snapshot (Daniel Lumertz)** - Chunk-level replacement:
- Captures complete track state (entire FX chain)
- Selective loading (choose what to restore)
- Version mode (auto-save before switching)
- Handles missing/deleted tracks gracefully

### Our Approach

Implement BOTH strategies:
1. **Parameter Morphing**: When source/target modules match, interpolate parameters
2. **State Replacement**: When modules differ, do atomic replacement with crossfade

## Quality Gates

These commands must pass for every user story:
- `cargo check -p signal-ui -p signal-proto -p signal-control` - Type checking
- `cargo clippy -p signal-ui -- -D warnings` - Linting
- `cargo test -p signal-proto` - Unit tests for data structures
- `cargo build -p fts-control-desktop` - Desktop app builds

## User Stories

---

### US-011: Define Parameter and Snapshot data structures

**Description:** As a developer, I need foundational data structures for parameters and snapshots that support both internal nodes and VST plugins.

**Acceptance Criteria:**
- [ ] Create `Parameter` struct in `signal-proto/src/parameter.rs`:
  ```rust
  struct Parameter {
      id: Uuid,
      name: String,
      value: f64,              // Normalized 0.0-1.0
      default_value: f64,
      min: f64,
      max: f64,
      unit: ParameterUnit,     // dB, Hz, %, ms, etc.
      param_type: ParameterType,
      display_value: String,   // Formatted for UI
  }
  ```
- [ ] Define `ParameterType` enum: `Continuous`, `Stepped(Vec<String>)`, `Toggle`, `Choice(Vec<String>)`
- [ ] Define `ParameterUnit` enum: `Decibels`, `Hertz`, `Percent`, `Milliseconds`, `Seconds`, `None`, `Custom(String)`
- [ ] Create `ParameterSnapshot` struct capturing parameter ID + value pairs
- [ ] Create `ModuleSnapshot` struct with module ID, block type, parameters, bypass state
- [ ] Create `RigSnapshot` struct containing Vec<ModuleSnapshot> + metadata (name, timestamp, tags)
- [ ] Implement `Serialize`/`Deserialize` for all types
- [ ] Add snapshot storage path configuration

---

### US-012: Implement parameter capture for internal nodes

**Description:** As a user, I want to capture the current state of all node parameters so I can save and recall my rig settings.

**Acceptance Criteria:**
- [ ] Add `parameters: Vec<Parameter>` field to `Node` struct in `node_graph.rs`
- [ ] Create `use_parameter_capture()` hook that reads all parameters from current graph
- [ ] Implement `capture_node_parameters(node_id: Uuid) -> Vec<(Uuid, f64)>` function
- [ ] Implement `capture_module_snapshot(module_id: Uuid) -> ModuleSnapshot` function
- [ ] Implement `capture_rig_snapshot() -> RigSnapshot` capturing entire rig state
- [ ] Add "Save Snapshot" button to rig toolbar
- [ ] Create snapshot naming dialog (default: timestamp, allow custom name)
- [ ] Store captured snapshots in `RIG_SNAPSHOTS: GlobalSignal<Vec<RigSnapshot>>`

---

### US-013: Implement snapshot recall (instant apply)

**Description:** As a user, I want to instantly recall a saved snapshot to restore my rig to a previous state.

**Acceptance Criteria:**
- [ ] Create `apply_snapshot(snapshot: &RigSnapshot)` function
- [ ] Implement parameter application: iterate snapshot, set each parameter value
- [ ] Handle missing nodes gracefully (skip with warning, don't crash)
- [ ] Handle extra nodes (nodes in rig but not in snapshot - leave unchanged)
- [ ] Add snapshot browser panel in right sidebar
- [ ] Double-click snapshot to apply instantly
- [ ] Show visual feedback during apply (brief highlight on affected nodes)
- [ ] Update `RIG_LAST_APPLIED_SNAPSHOT` signal for UI indication

---

### US-014: Implement snapshot diffing

**Description:** As a developer, I need to compute the difference between current state and a snapshot to enable efficient morphing and selective updates.

**Acceptance Criteria:**
- [ ] Create `SnapshotDiff` struct:
  ```rust
  struct SnapshotDiff {
      changed_params: Vec<ParameterChange>,
      added_modules: Vec<Uuid>,
      removed_modules: Vec<Uuid>,
      structural_changes: bool,  // true if modules differ
  }
  struct ParameterChange {
      node_id: Uuid,
      param_id: Uuid,
      from_value: f64,
      to_value: f64,
  }
  ```
- [ ] Implement `diff_snapshots(current: &RigSnapshot, target: &RigSnapshot) -> SnapshotDiff`
- [ ] Detect structural changes (different modules = can't morph, must replace)
- [ ] Detect parameter changes (same modules = can morph)
- [ ] Add diff preview UI showing what will change
- [ ] Color-code diff: green=changed, red=removed, blue=added

---

### US-015: Implement parameter morphing between snapshots

**Description:** As a user, I want to smoothly morph between two snapshots using a slider, blending my clean and dirty amp settings in real-time.

**Acceptance Criteria:**
- [ ] Create `SnapshotMorpher` struct managing A/B snapshots
- [ ] Add `morph_position: f64` field (0.0 = A, 1.0 = B)
- [ ] Implement linear interpolation: `lerp(a, b, t) = a + (b - a) * t`
- [ ] Create `MorphSlider` component (horizontal slider A---[thumb]---B)
- [ ] Add A/B snapshot assignment buttons (click to set current as A or B)
- [ ] Show A/B snapshot names above slider
- [ ] Update parameters in real-time as slider moves
- [ ] Handle non-morphable parameters (stepped/choice): snap at 0.5
- [ ] Only morph when `structural_changes == false`
- [ ] Show warning when structural differences prevent morphing

---

### US-016: Implement snapshot crossfade for structural changes

**Description:** As a user, when I need to switch between different module configurations, I want an audio crossfade to prevent clicks and pops.

**Acceptance Criteria:**
- [ ] Create `CrossfadeTransition` struct with duration and curve settings
- [ ] Implement crossfade logic: fade out old, swap modules, fade in new
- [ ] Add crossfade duration setting (default: 50ms, range: 0-500ms)
- [ ] Add crossfade curve options: linear, equal power, S-curve
- [ ] Show crossfade progress indicator during transition
- [ ] For modules that exist in both: morph parameters during crossfade
- [ ] For new modules: fade in from silence
- [ ] For removed modules: fade out to silence
- [ ] Queue parameter changes to apply at crossfade midpoint

---

### US-017: Implement tweening/easing for animated transitions

**Description:** As a user, I want animated transitions between snapshots with different easing curves for musical timing.

**Acceptance Criteria:**
- [ ] Create `TweenEngine` struct managing animated transitions
- [ ] Implement easing functions:
  - `linear(t)`: t
  - `ease_in(t)`: t²
  - `ease_out(t)`: 1 - (1-t)²
  - `ease_in_out(t)`: smooth S-curve
- [ ] Add tween duration options: 1/4 bar, 1/2 bar, 1 bar, 2 bar, 4 bar, custom ms
- [ ] Sync tween to transport tempo when available
- [ ] Create "Tween to Snapshot" action (vs instant "Apply Snapshot")
- [ ] Show tween progress bar during animation
- [ ] Allow canceling mid-tween (snaps to current interpolated position)
- [ ] Add tween settings to snapshot browser context menu

---

### US-018: Create snapshot slots UI (Snapshooter-style)

**Description:** As a user, I want quick-access snapshot slots for fast recall during performance.

**Acceptance Criteria:**
- [ ] Create `SnapshotSlots` component with 8 slots (expandable to pages of 12)
- [ ] Each slot shows: save button, name, apply button, write button
- [ ] Empty slots show "Slot N" placeholder
- [ ] Save button captures current state to slot (green when filled)
- [ ] Apply button recalls snapshot (shows checkmark on last applied)
- [ ] Add slot name editing (click name to rename)
- [ ] Add page navigation (< Page 01 >) for 10+ pages
- [ ] Add keyboard shortcuts: 1-8 to apply slots 1-8
- [ ] Add Shift+1-8 to save to slots 1-8
- [ ] Store slots in project/rig file for persistence

---

### US-019: Implement VST parameter bridge

**Description:** As a developer, I need to bridge the snapshot system to real VST plugin parameters via the rig control service.

**Acceptance Criteria:**
- [ ] Define `VstParameterBridge` trait in `signal-control`:
  ```rust
  trait VstParameterBridge {
      fn get_param_count(&self, fx_id: Uuid) -> usize;
      fn get_param(&self, fx_id: Uuid, param_idx: usize) -> f64;
      fn set_param(&self, fx_id: Uuid, param_idx: usize, value: f64);
      fn get_param_name(&self, fx_id: Uuid, param_idx: usize) -> String;
  }
  ```
- [ ] Implement mock `MockVstBridge` for testing
- [ ] Map node parameters to VST parameter indices
- [ ] Add parameter sync: internal → VST and VST → internal
- [ ] Handle parameter automation from DAW
- [ ] Cache parameter names/ranges on first access
- [ ] Add VST parameter discovery (enumerate all params on module load)

---

### US-020: Implement module state snapshots (chunk-style)

**Description:** As a user, when my module configurations differ completely, I want to save and restore entire module states like Track Snapshot does.

**Acceptance Criteria:**
- [ ] Create `ModuleStateChunk` struct capturing complete module state:
  ```rust
  struct ModuleStateChunk {
      module_id: Uuid,
      block_type: BlockType,
      nodes: Vec<NodeStateChunk>,
      internal_wires: Vec<Wire>,
      parameters: Vec<(Uuid, f64)>,
      vst_state: Option<Vec<u8>>,  // Binary VST state
  }
  ```
- [ ] Implement `capture_module_chunk(module_id) -> ModuleStateChunk`
- [ ] Implement `apply_module_chunk(chunk: ModuleStateChunk)`
- [ ] Add "Save Module Preset" to module context menu
- [ ] Store module presets in `.signal/module-chunks/` directory
- [ ] Allow selective loading: parameters only, nodes only, full state
- [ ] Preserve module position/connections when loading preset

---

### US-021: Implement modulation sources

**Description:** As a user, I want to modulate parameters from various sources (LFO, envelope, expression pedal) beyond static snapshots.

**Acceptance Criteria:**
- [ ] Define `ModulationSource` enum:
  - `Lfo { rate: f64, shape: LfoShape, phase: f64 }`
  - `Envelope { attack: f64, decay: f64, sustain: f64, release: f64 }`
  - `ExpressionPedal { cc: u8, channel: u8 }`
  - `MidiCC { cc: u8, channel: u8 }`
  - `SnapshotMorph { a: Uuid, b: Uuid }`  // Morph controlled by CC
- [ ] Create `ModulationRoute` struct: source → parameter → amount
- [ ] Implement `ModulationEngine` that updates parameters each frame
- [ ] Add modulation routing UI (drag source to parameter)
- [ ] Show modulation amount as arc on parameter knobs
- [ ] Add modulation depth control per route
- [ ] Store modulation routes in rig/preset

---

### US-022: Implement expression pedal/MIDI CC control

**Description:** As a user, I want to control the morph slider with my expression pedal for hands-free morphing during performance.

**Acceptance Criteria:**
- [ ] Add MIDI input handling to signal-ui
- [ ] Create `MidiLearn` component for assigning CCs
- [ ] Implement CC → morph position mapping
- [ ] Add CC curve options: linear, log, exp
- [ ] Add CC range limiting (min/max values)
- [ ] Show CC activity indicator (LED that blinks on input)
- [ ] Save CC assignments in rig/preset
- [ ] Support multiple CCs for different parameters
- [ ] Add "MIDI Learn" button: click, move pedal, auto-assign

---

### US-023: Implement snapshot automation/timeline

**Description:** As a user, I want to write snapshot transitions to the timeline for automated changes during playback.

**Acceptance Criteria:**
- [ ] Create `SnapshotAutomation` struct with time-stamped events
- [ ] Define automation event types:
  - `ApplySnapshot { time: f64, snapshot_id: Uuid }`
  - `StartMorph { time: f64, from: Uuid, to: Uuid, duration: f64 }`
  - `SetMorphPosition { time: f64, position: f64 }`
- [ ] Create automation lane UI in transport section
- [ ] Add "Write to Timeline" button (writes at time selection)
- [ ] Implement playback: trigger events at their timestamps
- [ ] Show automation events as markers on timeline
- [ ] Allow dragging events to adjust timing
- [ ] Support "Punch" mode: record morph slider movement

---

### US-024: Implement A/B comparison mode

**Description:** As a user, I want to quickly compare my current settings against a reference snapshot to hear the difference.

**Acceptance Criteria:**
- [ ] Create `CompareMode` component with A/B toggle
- [ ] Add "Set Reference" button to capture current as reference
- [ ] Add "A (Reference)" and "B (Current)" buttons
- [ ] Keyboard shortcut: C to toggle between A/B
- [ ] Visual indicator showing which is active (A or B highlighted)
- [ ] Flash transition when switching (brief crossfade)
- [ ] Auto-capture current as B when entering compare mode
- [ ] "Accept" button: keep B, clear reference
- [ ] "Revert" button: restore A, discard B changes

