# PRD: Snapshot & Scene Management with Live Morphing

## Overview

Implement a full snapshot and scene management system for the rig view that enables:
1. **Parameter snapshots** (Snapshooter-style) — capture FX parameter values and morph between them with easing
2. **Preset recall** (Track Snapshot-style) — save and recall full FX chain state (state chunks) as named presets
3. **SQLite persistence** — store snapshots and presets locally via SeaORM so they survive across sessions

This builds on existing infrastructure: `daw_bridge.rs` (capture/apply/diff functions), `signal-storage` (SeaORM entities + SQLite), and `snapshot_slots.rs` (UI slots).

## Goals

- Save parameter snapshots from a live DAW FX chain and recall them instantly
- Morph between two snapshots (A/B) with a slider, supporting easing curves
- Save full preset state (FX chain state chunks) and recall them to restore complete plugin state
- Persist all snapshots and presets to a local SQLite database
- Integrate the snapshot/preset system into the existing rig view UI

## Quality Gates

These commands must pass for every user story:
- `cargo check -p signal-storage` (if storage modified)
- `cargo check -p signal-control` (if control modified)
- `cargo check -p signal-ui` (if UI modified)
- `cargo check -p fts-control-desktop` (always — full build validation)

## User Stories

### US-001: Add SeaORM snapshot and preset persistence operations
**Description:** As a developer, I need CRUD operations in signal-storage for saving/loading parameter snapshots, state chunk snapshots, and presets to SQLite so the data survives across sessions.

**Acceptance Criteria:**
- [ ] Add `save_parameter_snapshot(track_guid, name, data) -> Uuid` function
- [ ] Add `load_parameter_snapshot(id) -> Option<DawParameterSnapshot>` function
- [ ] Add `list_parameter_snapshots(track_guid) -> Vec<SnapshotSummary>` function (id, name, created_at)
- [ ] Add `delete_parameter_snapshot(id)` function
- [ ] Add `save_state_chunk_snapshot(track_guid, name, data) -> Uuid` function
- [ ] Add `load_state_chunk_snapshot(id) -> Option<DawStateChunkSnapshot>` function
- [ ] Add `list_state_chunk_snapshots(track_guid) -> Vec<SnapshotSummary>` function
- [ ] Add `delete_state_chunk_snapshot(id)` function
- [ ] Uses existing signal-storage SeaORM entities and SQLite backend
- [ ] Snapshot data serialized as JSON in the `data` column

### US-002: Implement morph engine with easing curves
**Description:** As a developer, I need a morph engine that can interpolate between two parameter snapshots with configurable easing, producing intermediate parameter values that can be applied to the DAW.

**Acceptance Criteria:**
- [ ] Define `MorphEngine` struct holding snapshot A and snapshot B
- [ ] `MorphEngine::set_a(snapshot)` and `set_b(snapshot)` to load endpoints
- [ ] `MorphEngine::morph(t: f64) -> Vec<DawParamChange>` — interpolate at position t (0.0 = A, 1.0 = B)
- [ ] Support easing curves: Linear, EaseIn (quadratic), EaseOut (quadratic), EaseInOut (quadratic)
- [ ] `EasingCurve` enum with `apply(t: f64) -> f64` method
- [ ] Only produces changes for parameters that differ between A and B (diff-based)
- [ ] Unit tests for each easing curve and the morph output

### US-003: Wire snapshot capture and recall to DAW FX chain
**Description:** As a developer, I need functions that capture snapshots from the live DAW FX chain, save them to SQLite, and recall them by applying parameters or state chunks back.

**Acceptance Criteria:**
- [ ] `capture_and_save_snapshot(chain, track_guid, name) -> Uuid` — captures parameter snapshot from chain, saves to DB
- [ ] `recall_snapshot(chain, id)` — loads snapshot from DB, applies to chain (diff-based, only changed params)
- [ ] `capture_and_save_preset(chain, track_guid, name) -> Uuid` — captures state chunk snapshot, saves to DB
- [ ] `recall_preset(chain, id)` — loads state chunk from DB, applies to chain
- [ ] Uses existing `daw_bridge.rs` capture/apply functions
- [ ] Uses US-001 persistence functions for save/load
- [ ] Error handling with eyre::Result

### US-004: Add snapshot slot UI with save/recall/morph controls
**Description:** As a user, I want to save snapshots to numbered slots, recall them with one click, and morph between any two snapshots using a slider in the rig view.

**Acceptance Criteria:**
- [ ] Snapshot slots panel shows 8 slots per page (use existing `SnapshotSlots` component pattern)
- [ ] Click empty slot to save current FX state as parameter snapshot
- [ ] Click filled slot to recall that snapshot (apply to DAW)
- [ ] Right-click slot for context menu: Rename, Delete, Overwrite
- [ ] A/B morph section: two slot selectors + morph slider (0–100%)
- [ ] Moving morph slider applies interpolated parameters in real-time
- [ ] Easing curve selector dropdown (Linear, Ease In, Ease Out, Ease In/Out)
- [ ] Slots persist across sessions (loaded from SQLite on startup)
- [ ] Visual feedback: active slot highlighted, morph position indicator

### US-005: Add preset management panel with state chunk save/recall
**Description:** As a user, I want to save named presets that capture the complete FX chain state (not just parameters) and recall them to restore full plugin state including internal settings.

**Acceptance Criteria:**
- [ ] Preset list panel shows saved presets for the current track
- [ ] "Save Preset" button captures full state chunks + parameters
- [ ] Click preset to recall it (applies state chunks, then parameter overlay)
- [ ] Rename and delete presets via context menu or buttons
- [ ] Presets persist across sessions (loaded from SQLite on startup)
- [ ] Show preset metadata: name, date created, FX count
- [ ] Confirmation dialog before overwriting existing preset

## Functional Requirements

- FR-1: Parameter snapshots store normalized FX parameter values (0.0–1.0) per FX GUID + param index
- FR-2: State chunk snapshots store the full binary plugin state as base64-encoded strings per FX GUID
- FR-3: Morph interpolation uses `value = easing(t) * (b - a) + a` formula for each parameter
- FR-4: Snapshot recall only writes parameters that differ from current state (diff-based application)
- FR-5: All persistence uses the existing signal-storage SQLite backend via SeaORM
- FR-6: FX are identified by GUID (stable across reordering), not by index

## Non-Goals

- Automation envelope writing (Snapshooter's "Write" feature) — future phase
- Time-based tweening (auto-morph over N bars) — future phase
- Send/volume/pan/mute snapshots (mixer state) — future phase
- Cloud sync of snapshots/presets
- Multi-track snapshot groups (Track Snapshot's track group feature)

## Technical Considerations

- Existing `daw_bridge.rs` has capture/apply/diff functions — build on these, don't duplicate
- Existing `signal-storage` has SeaORM entities for `snapshot` table — extend or reuse
- `SnapshotSlots` component in signal-ui already has slot grid UI — extend for morph controls
- `use_parameter_capture` hook exists — may need modification for new flow
- Morph engine should be sync (no DAW calls) — it just computes intermediate values. DAW application is separate.

## Success Metrics

- Can save and recall parameter snapshots with < 50ms latency
- Morph slider updates parameters in real-time (< 16ms per frame)
- Presets survive app restart (SQLite persistence verified)
- All quality gate commands pass
