# PRD: Signal Variation Switching — End-to-End Integration

## Problem Statement

The signal system has all core components built (domain model, storage, appliers, controller, UI)
but they are not wired together end-to-end. SignalController currently lives in fts-control-desktop
instead of the REAPER extension, and the "Switch to Variation N" actions (declared in signal-proto)
have no handler implementations. A user cannot yet switch between guitar rig presets/scenes from
REAPER or fts-control.

## Goal

A user can trigger "FTS / Signal / Switch to Variation 1–16" from a REAPER action, MIDI CC,
keyboard shortcut, or fts-control UI, and the active rig/profile/song switches to the Nth
variation — gaplessly, with <5ms latency for preloaded scenes.

## Architecture

```
REAPER Extension (source of truth)
┌──────────────────────────────────────┐
│ SignalController                     │
│   ├─ ReaperPatchApplier              │
│   ├─ RigSceneManager                 │
│   ├─ SQLite DB (signal.db)           │
│   └─ Active context state            │
│                                      │
│ Exposed via roam RPC:                │
│   - Signal service traits            │
│   - Variation switch endpoint        │
│   - Action handlers (1–24)           │
└──────────────────────────────────────┘
          ▲ Unix socket (roam)
          │
fts-control-desktop (thin client)
┌──────────────────────────────────────┐
│ signal-ui views                      │
│ RPC calls to extension               │
│ No local SignalController            │
└──────────────────────────────────────┘
```

## Key Design Decisions

1. **Handlers live in the signal crate** — not reaper-extension — because roam abstracts the
   execution context. Both fts-control and the extension can invoke the same handler code.

2. **Context-free actions** — "Switch to Variation N" operates on whatever collection is active
   (profile patches, rig scenes, song sections). An `ActiveContext` enum tracks the current mode.

3. **Actions use the DAW abstraction** — handlers call through `daw-control` / `daw-proto`,
   so the same code works whether invoked from the extension or a remote client.

## User Stories

### US-001: Bootstrap SignalController in REAPER Extension
**Priority: P0**

Move SignalController creation from fts-control-desktop into the REAPER extension's
`register_daw_dispatcher()`. Create the SQLite DB connection, wire `ReaperPatchApplier`
and `RigSceneManager`, and store the controller in a global accessible to action handlers.

**Acceptance Criteria:**
- SignalController is created in the extension on startup
- Appliers are attached (ReaperPatchApplier + RigSceneManager)
- Controller is accessible from action handler context
- fts-control-desktop no longer creates its own SignalController

### US-002: Expose Signal Services via roam RPC
**Priority: P0**

Add signal service dispatchers to the extension's `RoutedHandler` so fts-control-desktop
(and future clients) can call signal operations over the Unix socket.

**Acceptance Criteria:**
- Signal service traits (Profile, Rig, Engine, Block, etc.) are callable via RPC
- fts-control-desktop uses RPC client instead of local SignalController
- signal-ui views work unchanged (they already use context-based access)

### US-003: Implement ActiveContext State Machine
**Priority: P0**

Create an `ActiveContext` enum in the signal crate that tracks what the user is currently
working with (a Profile, a Rig, or a Song). "Switch to Variation N" resolves against this
context to determine the concrete entity to activate.

**Acceptance Criteria:**
- `ActiveContext` enum with variants: `Profile { id, active_patch_index }`,
  `Rig { id, active_scene_index }`, `Song { id, active_section_index }`
- Thread-safe global or controller-owned state
- Set by UI navigation (opening a profile sets context to Profile mode)
- Queryable by action handlers

### US-004: Implement Variation Switch Handlers in Signal Crate
**Priority: P0**

Write the handler function `switch_to_variation(n: usize)` in the signal crate that:
1. Reads ActiveContext to determine current mode
2. Resolves variation N to a concrete ID (patch_id, scene_id, or section_id)
3. Calls the appropriate activate method (profiles().activate, rig scene switch, etc.)

**Acceptance Criteria:**
- `switch_to_variation(n)` works for all three modes (Profile, Rig, Song)
- Out-of-bounds N is a no-op (no crash, maybe a log warning)
- Handler is DAW-agnostic (uses controller API, not direct REAPER calls)
- Handlers for navigation actions (Next/Previous Song, Next/Previous Section) also implemented

### US-005: Register REAPER Actions for Variations 1–16
**Priority: P1**

Wire the signal action declarations (SWITCH_TO_VARIATION_1 through _16, plus navigation)
into the REAPER extension's action registry so they appear in REAPER's action list under
"FTS / Signal / Variations".

**Acceptance Criteria:**
- Actions 1–16 appear in REAPER's action list
- Each action calls `switch_to_variation(n)` via the signal crate handler
- Actions are assignable to keyboard shortcuts and MIDI in REAPER
- Navigation actions (Next/Previous Song/Section) also registered

### US-006: Auto-Preload Rig Scenes on Context Switch
**Priority: P1**

When ActiveContext switches to a Rig, automatically call `RigSceneManager.set_target()`
to preload all scenes to REAPER tracks (muted). This ensures <5ms switching latency.

**Acceptance Criteria:**
- Switching to a rig context triggers scene preloading
- All scenes are instantiated as muted REAPER tracks
- Variation switch uses mute/unmute (not FX chain rebuild)
- Preloading is async and doesn't block the UI

### US-007: Update fts-control-desktop to Use RPC
**Priority: P1**

Refactor fts-control-desktop to connect to the extension's SignalController via RPC
instead of running one in-process. Remove local DB connection, applier wiring, and
in-process controller creation.

**Acceptance Criteria:**
- Desktop app connects to signal services via Unix socket
- No local SignalController or SQLite DB
- daw_registry.rs no longer creates ReaperPatchApplier
- All signal-ui views work via RPC with no user-visible difference

### US-008: End-to-End Integration Test
**Priority: P2**

Create an integration test that:
1. Boots SignalController with mock DAW bridge
2. Sets ActiveContext to a Profile with 4 patches
3. Fires switch_to_variation(1) through switch_to_variation(4)
4. Verifies the correct patch was activated each time
5. Switches context to Rig, fires variations, verifies scene switches

**Acceptance Criteria:**
- Test covers Profile, Rig, and Song context modes
- Test uses MockDawBridge (no REAPER needed)
- Test verifies the correct activate calls were made
- Test is in signal crate's test suite
