[PRD]
# PRD: REAPER FX Bridge & Real Parameter Snapshots

## Overview

Bridge the signal rig control system to real REAPER FX chains, enabling:
- **Real parameter enumeration**: Read actual VST/CLAP/JS plugin parameter names and values from REAPER
- **State chunk capture**: Save/restore full plugin binary state (base64 blobs) for complete scene switching (Track Snapshot style)
- **Parameter snapshots**: Snapshooter-style diff-based capture/recall of individual parameter values through the DAW bridge

Currently the signal system uses `MockRigControlService` with hardcoded fake parameters. This PRD implements the real bridge so the UI shows actual plugin parameters from REAPER tracks.

## Goals

- Implement `ReaperFx` service in `daw-reaper` to bridge `FxService` trait to reaper-rs API calls
- Add state chunk operations to `daw-proto` FxService for full plugin state save/restore
- Wire the `daw-control` FxHandle/FxParamHandle through to real REAPER FX chains
- Map REAPER container hierarchy (INPUT, DRIVE, AMP, etc.) to signal `BlockType` variants
- Enable real parameter values to flow into the signal-ui node graph and snapshot system
- Support both parameter-level snapshots (Snapshooter-style) and full state chunk snapshots (Track Snapshot-style)

## Quality Gates

These commands must pass for every user story:
- `cargo check -p daw-reaper -p daw-proto -p daw-control` - Type checking for DAW crates
- `cargo check -p signal -p signal-proto -p signal-control -p signal-ui` - Type checking for signal crates
- `cargo build -p fts-control-desktop` - Desktop app builds

## User Stories

---

### US-001: Implement ReaperFx service for FX queries

**Description:** As a developer, I need the `ReaperFx` struct implementing `FxService` trait in `daw-reaper` so that FX chain queries work against real REAPER tracks.

**Acceptance Criteria:**
- [ ] Create `daw-reaper/src/fx.rs` with `ReaperFx` struct following the `ReaperTransport` pattern
- [ ] Implement `get_fx_list()` — enumerate all FX on a track using `TrackFX_GetCount` + `TrackFX_GetFXName` + `TrackFX_GetFXGUID`
- [ ] Implement `get_fx()` — get single FX info by GUID/index/name
- [ ] Implement `fx_count()` — return FX count for a chain context
- [ ] Implement `set_fx_enabled()` / `set_fx_offline()` — toggle bypass/offline state
- [ ] Use `main_thread_future()` for queries and `do_later_in_main_thread_asap()` for commands
- [ ] Register `ReaperFx` in the daw-reaper service setup (lib.rs exports)
- [ ] Handle container FX (nested FX chains) — detect and recurse into `CONTAINER` type FX

---

### US-002: Implement ReaperFx parameter read/write

**Description:** As a developer, I need to read and write individual FX parameters from real REAPER plugins so the UI can show and control actual knob values.

**Acceptance Criteria:**
- [ ] Implement `get_parameters()` — enumerate all params for an FX using `TrackFX_GetNumParams` + `TrackFX_GetParamName` + `TrackFX_GetParam`
- [ ] Implement `get_parameter()` — get single param by index (normalized 0.0-1.0)
- [ ] Implement `set_parameter()` — set param value by index
- [ ] Implement `get_parameter_by_name()` / `set_parameter_by_name()` — name-based access
- [ ] Return `FxParameter` structs with: index, name, normalized value, formatted display string (via `TrackFX_GetFormattedParamValue`)
- [ ] Handle parameter value clamping (0.0-1.0 range)
- [ ] Cache parameter names on first enumeration (names don't change per plugin instance)

---

### US-003: Add state chunk operations to FxService proto

**Description:** As a developer, I need RPC methods for saving/restoring full plugin binary state so we can do complete scene switches.

**Acceptance Criteria:**
- [ ] Add `get_fx_state_chunk()` method to `FxService` trait in `daw-proto/src/fx/service.rs` — returns `Vec<u8>` (raw binary state)
- [ ] Add `set_fx_state_chunk()` method — accepts `Vec<u8>` to restore plugin state
- [ ] Add `get_fx_state_chunk_encoded()` — returns base64-encoded string (for serialization)
- [ ] Add `set_fx_state_chunk_encoded()` — accepts base64 string
- [ ] Add corresponding request/response types
- [ ] Add `get_track_fx_chain_chunk()` — capture entire FX chain state (all FX on a track)
- [ ] Add `set_track_fx_chain_chunk()` — restore entire FX chain

---

### US-004: Implement state chunk operations in ReaperFx

**Description:** As a developer, I need the REAPER-side implementation of state chunk save/restore using reaper-rs chunk APIs.

**Acceptance Criteria:**
- [ ] Implement `get_fx_state_chunk()` using reaper-rs `Fx::vst_chunk()` (returns decoded bytes)
- [ ] Implement `set_fx_state_chunk()` using reaper-rs `Fx::set_vst_chunk()` 
- [ ] Implement `get_fx_state_chunk_encoded()` using `Fx::vst_chunk_encoded()` (base64 string)
- [ ] Implement `set_fx_state_chunk_encoded()` using `Fx::set_vst_chunk_encoded()`
- [ ] Implement `get_track_fx_chain_chunk()` by iterating all FX and collecting chunks
- [ ] Implement `set_track_fx_chain_chunk()` by restoring chunks in order
- [ ] Handle CLAP vs VST state format differences (both use `GetNamedConfigParm` under the hood)
- [ ] Test with real plugins: VST3 (NeuralAmpModeler), CLAP (FabFilter Pro-G), JS (ReaEQ)

---

### US-005: Add FxHandle to daw-control client layer

**Description:** As a developer, I need the `FxChain`/`FxHandle`/`FxParamHandle` client-side API to work with the new state chunk methods.

**Acceptance Criteria:**
- [ ] Add `state_chunk()` method to `FxHandle` — returns `Vec<u8>`
- [ ] Add `set_state_chunk()` method to `FxHandle` — restores binary state
- [ ] Add `state_chunk_encoded()` method — returns base64 string
- [ ] Add `chain_chunk()` method to `FxChain` — capture entire chain state
- [ ] Add `set_chain_chunk()` method to `FxChain` — restore entire chain
- [ ] Verify existing `parameters()`, `param()`, `set()` methods work through to ReaperFx

---

### US-006: Map REAPER container names to signal BlockType

**Description:** As a developer, I need a mapping between REAPER FX container names and signal-proto BlockType so the rig UI knows which module each container represents.

**Acceptance Criteria:**
- [ ] Create `ContainerMapping` in `signal-proto` or `signal-control` that maps REAPER names to `BlockType`:
  - "INPUT" -> `BlockType::Input`
  - "DRIVE" -> `BlockType::Drive`  
  - "PRE-FX" -> `BlockType::PreFx`
  - "AMP" -> `BlockType::Amp`
  - "MODULATION" -> `BlockType::Modulation`
  - "TIME" -> `BlockType::Time`
  - "MOTION" -> `BlockType::Motion`
  - "Container" (unnamed) -> `BlockType::Custom(name)`
- [ ] Handle case-insensitive matching
- [ ] Support user-defined container names that don't match known types
- [ ] Create `discover_rig_layout()` function that reads a REAPER track's container hierarchy and returns a `Vec<(BlockType, FxGuid)>` mapping

---

### US-007: Implement RealRigControlService

**Description:** As a developer, I need a real implementation of the rig control service that reads from REAPER instead of mock data, so the signal-ui shows actual plugin state.

**Acceptance Criteria:**
- [ ] Create `RealRigControlService` in `signal/src/` (or a new `signal-reaper` bridge crate)
- [ ] Constructor takes a `Daw` reference and a target track GUID
- [ ] Implement `get_engine_state()` — read all containers + their FX + parameters from the target track
- [ ] Implement `get_slot_state()` — read a specific container's FX parameters
- [ ] Build `PresetInfo` from the actual FX chain state (name from track name, scenes from saved snapshots)
- [ ] Map container FX hierarchy to module slots using `ContainerMapping` from US-006
- [ ] Wire into the signal-ui by providing `RealRigControlService` instead of `MockRigControlService` when a DAW connection is active

---

### US-008: Implement parameter snapshot capture via DAW bridge

**Description:** As a user, I want to capture a snapshot of all real plugin parameters (Snapshooter-style) so I can recall specific knob positions later.

**Acceptance Criteria:**
- [ ] Create `DawParameterSnapshot` struct containing: track GUID, Vec of (fx_guid, param_index, param_value) tuples
- [ ] Implement `capture_parameter_snapshot()` — reads all FX parameters from target track via `FxChain::all()` + `FxHandle::parameters()`
- [ ] Implement `apply_parameter_snapshot()` — writes parameter values back via `FxParamHandle::set()`
- [ ] Implement `diff_parameter_snapshots()` — compute only changed parameters (Snapshooter's diff approach)
- [ ] Only apply changed parameters (skip identical values for efficiency)
- [ ] Store snapshots using existing `RigSnapshot` / `ParameterSnapshot` proto types
- [ ] Wire capture/apply into the existing `use_parameter_capture()` hook so the UI "Save Snapshot" button captures real params

---

### US-009: Implement state chunk snapshot capture via DAW bridge

**Description:** As a user, I want to save and restore complete plugin states (including internal presets, IR files, etc.) for full scene switching.

**Acceptance Criteria:**
- [ ] Create `DawStateChunkSnapshot` struct containing: track GUID, Vec of (fx_guid, base64_state_chunk) pairs
- [ ] Implement `capture_state_chunks()` — reads state chunks for all FX on target track
- [ ] Implement `apply_state_chunks()` — restores all FX state chunks
- [ ] Handle FX ordering — restore chunks in the correct FX chain order
- [ ] Handle missing FX gracefully (FX removed since snapshot was taken)
- [ ] Store state chunk snapshots alongside parameter snapshots (both are part of a scene)
- [ ] Create `SceneSnapshot` type that combines both: parameter values (for morphing) + state chunks (for full restore)

---

### US-010: Wire real parameters into signal-ui node graph

**Description:** As a user, I want the rig grid UI to show actual plugin parameter names and values from my REAPER session.

**Acceptance Criteria:**
- [ ] When `RealRigControlService` is active, populate `NodeParameter` entries with real param names/values from FxHandle
- [ ] Update node graph parameters when DAW parameter values change (polling or event-driven)
- [ ] Show formatted parameter display strings (e.g., "-12.5 dB" not "0.35")
- [ ] Handle parameter count differences between plugins (some have 3 params, others have 100+)
- [ ] Group parameters by module (container) in the node property panel
- [ ] Allow editing parameters from the UI — changes flow back to REAPER via `FxParamHandle::set()`

## Functional Requirements

- FR-1: All FX service operations must dispatch to REAPER main thread via `TaskSupport` (REAPER APIs are not thread-safe)
- FR-2: State chunks must round-trip losslessly — capture then immediately restore must produce identical plugin state
- FR-3: Parameter snapshots must use normalized 0.0-1.0 values for cross-plugin compatibility
- FR-4: Container mapping must be configurable per-track (not all tracks use the same module layout)
- FR-5: The system must work with VST2, VST3, CLAP, and JS plugin formats
- FR-6: State chunk capture must handle plugins that report empty/null chunks gracefully
- FR-7: Parameter snapshot application must skip unchanged values (Snapshooter diff approach)

## Non-Goals

- MIDI CC / expression pedal control (separate PRD — US-021/022 in existing PRD)
- Automation timeline writing (separate PRD — US-023 in existing PRD)
- Snapshot morphing slider UI (already implemented in signal-ui, just needs real data)
- Multi-track snapshot capture (v1 targets single rig track)
- FX chain reordering from signal-ui (read-only structure, edit in REAPER)

## Technical Considerations

- **Threading**: All REAPER API calls must go through `TaskSupport::main_thread_future()` or `do_later_in_main_thread_asap()`. The main thread dispatch pattern is established in `daw-reaper/src/transport.rs`.
- **State chunk sizes**: VST state chunks can be several KB (simple plugins) to several MB (samplers). Base64 encoding adds ~33% overhead. Consider compression for storage.
- **Plugin identification**: Use FX GUID (stable across sessions) not FX index (changes when FX reordered). The REAPER RPP format uses `FXID {GUID}` for this.
- **Container FX**: REAPER's container system nests FX chains. The `TrackFX_GetNamedConfigParm("container_count")` API reveals container depth. Need recursive enumeration.
- **Existing reaper-rs APIs**: `Fx::vst_chunk()`, `Fx::set_vst_chunk()`, `Fx::vst_chunk_encoded()` in `reference/reaper-rs/main/high/src/fx.rs` handle the base64 encode/decode.
- **daw-proto already defines**: `FxService` trait, `Fx` struct, `FxParameter` struct, `FxTarget`/`FxRef`/`FxChainContext` types — no new proto types needed for basic operations.

## Success Metrics

- Real plugin parameter names appear in the signal-ui node property panel
- Saving and restoring a state chunk snapshot produces identical plugin behavior
- Parameter snapshot diff correctly identifies only changed values
- Round-trip latency for parameter read/write < 50ms
- All 5 plugin formats (VST2, VST3, CLAP, AU, JS) work for parameter enumeration

## Open Questions

- Should we support capturing snapshots across multiple tracks (e.g., bass + guitar rigs)?
- Should state chunk snapshots be stored in the project file (REAPER extended state) or externally?
- How do we handle container FX that contain other containers (nested depth > 1)?
- Should parameter polling be timer-based (e.g., 10Hz) or event-driven via REAPER change notifications?
[/PRD]
