# PRD: FX Integration — Container-Aware FX Tree Model

## Overview

REAPER's FX chain is a recursive tree structure where **containers** group child FX with serial or parallel routing, and containers can nest arbitrarily deep. Our current `Fx` struct and `FxService` trait treat the FX chain as a flat list — no awareness of containers, nesting, routing modes, or container names.

This epic delivers a **container-aware FX tree model** in `daw-proto`, full read/write support in `daw-reaper`, exposure through `daw-control`, and consumption in a new `daw-ui` crate. It also migrates all DAW-related UI components (TCP, ArrangementView, MixerPanel, FxBrowser) out of `signal-ui` into `daw-ui`.

### Problem
- `get_fx_list()` returns a flat `Vec<Fx>` — containers appear as opaque FX entries with no children
- No way to read container names, nesting structure, or parallel/serial routing
- No way to create containers, move FX into/out of containers, or toggle routing modes
- Snapshot/recall algorithms in signal need full FX tree fidelity for preset management
- DAW UI components are incorrectly housed in signal-ui (which should only handle rig management)

### Reference Implementation
ParanormalFX (Lua/ReaScript) demonstrates the full REAPER FX container model:
- Stride-based addressing: `0x2000000 + container_id + (stride * child_index)`
- `GetNamedConfigParm` for `container_count`, `parallel`, `fx_type`, `container_nch`
- Recursive tree traversal with depth tracking
- Full CRUD: create containers, move FX in/out, toggle parallel, reorder

## Goals
- Replace flat FX list model with recursive `FxNode` tree in `daw-proto`
- Implement full container traversal in `daw-reaper` using REAPER's stride-based addressing
- Support read + write for all container operations (create, move, delete, toggle parallel, bypass)
- Expose container channel counts (`container_nch`, `container_nch_in`, `container_nch_out`)
- Create `daw-ui` crate and migrate all DAW UI components out of `signal-ui`
- Abstract REAPER's raw index encoding behind a stable `FxNodeId` system in `daw-proto`

## Quality Gates

These commands must pass for every user story:
- `cargo check -p daw-proto`
- `cargo check -p daw-reaper`
- `cargo check -p daw-control`
- `cargo check -p daw-ui`
- `cargo check -p reaper-extension`
- `cargo check -p fts-control-desktop`

Where applicable:
- `cargo test -p daw-proto` (for tree model unit tests)

## User Stories

### US-001: Create daw-ui crate and migrate DAW components from signal-ui
**Description:** As a developer, I want all DAW-related UI components in a dedicated `daw-ui` crate so that `signal-ui` only contains rig management components.

**Acceptance Criteria:**
- [ ] Create `cells/daw/daw-ui/` crate with same pattern as `signal-ui` (components/, lib.rs, Cargo.toml)
- [ ] Move `TrackControlPanel` from `signal-ui/src/components/track_control_panel.rs` to `daw-ui/src/components/`
- [ ] Move `ArrangementView` from `signal-ui/src/components/arrangement_view.rs` to `daw-ui/src/components/`
- [ ] Move `MixerPanel` from `signal-ui/src/components/mixer.rs` to `daw-ui/src/components/`
- [ ] Move FX Browser component from `signal-ui` to `daw-ui/src/components/`
- [ ] Remove moved modules from `signal-ui/src/components/mod.rs` and `signal-ui/src/lib.rs`
- [ ] Update `daw-ui/src/lib.rs` to re-export all moved components
- [ ] Update `apps/fts-control/desktop/Cargo.toml` to depend on `daw-ui` instead of `signal-ui` for these components
- [ ] Update `apps/fts-control/desktop/src/main.rs` imports to use `daw_ui::` for moved components
- [ ] `signal-ui` has zero DAW-specific components remaining (only rig management: GuitarRigGrid, RigLayout, etc.)

### US-002: Define FxNode tree model in daw-proto
**Description:** As a developer, I want a recursive `FxNode` type in `daw-proto` that represents the full FX chain hierarchy so that containers, nesting, and routing modes are first-class concepts.

**Acceptance Criteria:**
- [ ] Add `FxNodeId` type — stable identifier for any node in the FX tree (not raw REAPER indices)
- [ ] Add `FxRoutingMode` enum: `Serial`, `Parallel`
- [ ] Add `FxNodeKind` enum: `Plugin(Fx)`, `Container { name, children, routing, channel_config }`
- [ ] Add `FxNode` struct with: `id: FxNodeId`, `kind: FxNodeKind`, `enabled: bool`, `parent_id: Option<FxNodeId>`
- [ ] Add `FxTree` struct: root-level ordered list of `FxNode` entries representing the full chain
- [ ] Add `FxContainerChannelConfig` struct with `nch`, `nch_in`, `nch_out` fields
- [ ] `FxTree` provides methods: `iter()`, `find_node(id)`, `find_by_guid(guid)`, `depth_of(id)`
- [ ] All new types derive `Clone, Debug, Facet`
- [ ] Unit tests: build a sample tree with nested containers, verify traversal and lookup methods
- [ ] Existing `Fx` struct unchanged — it represents a single plugin's info, now embedded in `FxNodeKind::Plugin`

### US-003: Extend FxService trait with tree-aware methods
**Description:** As a developer, I want the `FxService` trait to support container-aware operations so that callers can read the full FX tree and manipulate containers.

**Acceptance Criteria:**
- [ ] Add `get_fx_tree()` method returning `FxTree` (full recursive hierarchy for a chain)
- [ ] Add `create_container()` method — creates an empty named container at a position in the chain
- [ ] Add `move_to_container()` method — moves an FX node into a container at a specified child position
- [ ] Add `move_from_container()` method — moves an FX node out of its container to a parent-level position
- [ ] Add `set_routing_mode()` method — sets serial/parallel mode for an FX within a container
- [ ] Add `get_container_channel_config()` and `set_container_channel_config()` methods
- [ ] Add `enclose_in_container()` method — wraps one or more existing FX nodes in a new container
- [ ] Add `explode_container()` method — moves all children out and removes the container
- [ ] Add `rename_container()` method — sets the display name of a container
- [ ] Existing methods (`get_fx_list`, `add_fx`, `move_fx`, `remove_fx`, etc.) continue to work unchanged for flat-chain usage
- [ ] All new methods use `FxNodeId` (not raw indices) for addressing nodes within the tree

### US-004: Implement FxTree building in daw-reaper
**Description:** As a developer, I want `ReaperFx::get_fx_tree()` to recursively traverse REAPER's container structure and return a complete `FxTree` so that the UI can display the full hierarchy.

**Acceptance Criteria:**
- [ ] Implement `get_fx_tree()` in `ReaperFx` using REAPER's stride-based container addressing
- [ ] Use `GetNamedConfigParm(track, fx_id, "fx_type")` to detect containers vs plugins
- [ ] Use `GetNamedConfigParm(track, fx_id, "container_count")` to get child count
- [ ] Use `GetNamedConfigParm(track, fx_id, "parallel")` to read routing mode per child
- [ ] Use `GetNamedConfigParm(track, fx_id, "container_nch")` / `"container_nch_in"` / `"container_nch_out"` for channel config
- [ ] Correctly compute stride (`DIFF`) at each nesting level: `DIFF = (parent_count + 1) * prev_DIFF`
- [ ] Map REAPER's raw `0x2000000 + offset` indices to stable `FxNodeId` values
- [ ] Handle arbitrarily deep nesting (recursive traversal)
- [ ] Container names read via `GetFXName` on the container's FX slot
- [ ] Each `FxNode` includes `enabled` state from `GetEnabled` on the container/FX slot
- [ ] Returns both normal FX chain and input FX chain trees when requested

### US-005: Implement container mutation operations in daw-reaper
**Description:** As a developer, I want full CRUD operations on FX containers in `daw-reaper` so that the app can create, modify, and destroy container structures.

**Acceptance Criteria:**
- [ ] `create_container()` — uses `TrackFX_AddByName(track, "Container", position)` then sets name via `SetNamedConfigParm`
- [ ] `move_to_container()` — uses `TrackFX_CopyToTrack` with computed destination slot (0x2000000 + container offset)
- [ ] `move_from_container()` — copies FX to root/parent level then removes from original position
- [ ] `set_routing_mode()` — uses `SetNamedConfigParm(track, fx_id, "parallel", mode)` where `"0"` = serial, `"1"` = parallel
- [ ] `enclose_in_container()` — creates container at target position, moves specified FX into it
- [ ] `explode_container()` — moves all children to parent level, then deletes the container
- [ ] `rename_container()` — uses `SetNamedConfigParm(track, fx_id, "renamed_name", name)`
- [ ] All mutations correctly resolve `FxNodeId` to REAPER's raw stride-based indices
- [ ] Operations maintain FX chain integrity (no orphaned FX, correct index recalculation after mutations)
- [ ] `set_container_channel_config()` — uses `SetNamedConfigParm` for `container_nch`, `container_nch_in`, `container_nch_out`

### US-006: FxNodeId resolution layer in daw-reaper
**Description:** As a developer, I want a resolution layer that maps stable `FxNodeId` values to REAPER's raw stride-based FX indices so that callers never deal with raw encoding.

**Acceptance Criteria:**
- [ ] `FxNodeId` encodes enough information to resolve to a REAPER FX slot (e.g., container path + child index)
- [ ] Resolution function: `resolve_fx_node_id(project, track, node_id) -> Option<i32>` returns raw REAPER index
- [ ] Reverse mapping: `fx_node_id_from_raw(project, track, raw_index) -> FxNodeId`
- [ ] Resolution handles depth changes after mutations (re-resolve from tree state)
- [ ] GUID-based fallback: if stride-based resolution fails, scan by GUID
- [ ] All existing FxService methods that accept `FxRef` continue to work (backwards compatible)
- [ ] New container methods use `FxNodeId` exclusively

### US-007: Wire FxTree through daw-control facade
**Description:** As a developer, I want all new FxService container methods exposed through `daw-control`'s ergonomic API so that UI code can access the FX tree without dealing with RPC details.

**Acceptance Criteria:**
- [ ] `Daw` (or the appropriate facade in daw-control) exposes `fx_tree()` method returning `FxTree`
- [ ] All new container mutation methods accessible through the facade
- [ ] Methods follow same async pattern as existing daw-control methods
- [ ] Re-export new daw-proto types (`FxNode`, `FxTree`, `FxNodeId`, `FxRoutingMode`, etc.) from daw-control

### US-008: Update FxEvent for container changes
**Description:** As a developer, I want `FxEvent` to include container-specific events so that reactive UI can respond to container structure changes.

**Acceptance Criteria:**
- [ ] Add `ContainerCreated` variant with container `FxNodeId` and name
- [ ] Add `ContainerRemoved` variant
- [ ] Add `RoutingModeChanged` variant with node id and new mode
- [ ] Add `MovedToContainer` variant with node id, source, destination container
- [ ] Add `ContainerRenamed` variant
- [ ] Add `TreeStructureChanged` variant (catch-all for complex mutations that affect tree shape)
- [ ] Existing FxEvent variants unchanged for backwards compatibility
- [ ] `poll_and_broadcast_fx()` in daw-reaper detects container structure changes and emits appropriate events

### US-009: FX Chain Tree component in daw-ui
**Description:** As a user, I want an FX chain tree view in the UI that shows containers, nesting, and routing modes so that I can visualize and interact with my FX signal flow.

**Acceptance Criteria:**
- [ ] New `FxChainTree` component in `daw-ui/src/components/`
- [ ] Displays FX as a hierarchical tree with collapsible containers
- [ ] Container nodes show: name, routing mode icon (serial/parallel), enabled state, child count
- [ ] Plugin nodes show: name, plugin type, enabled state, preset name
- [ ] Indentation reflects nesting depth
- [ ] Containers are collapsible/expandable
- [ ] Poll-wait pattern for DAW connection (same as TCP/Mixer)
- [ ] Routing mode displayed as visual indicator (e.g., "S" for serial, "P" for parallel)
- [ ] Bypass/enable toggle per node (container or individual FX)
- [ ] Selected node highlighting

### US-010: FX Chain mutation UI in daw-ui
**Description:** As a user, I want to create, rename, and restructure FX containers from the UI so that I can organize my FX chains without switching to REAPER.

**Acceptance Criteria:**
- [ ] Right-click context menu on tree nodes with: Bypass, Rename (containers), Delete, Move Up/Down
- [ ] "Create Container" action — prompts for name, creates at selected position
- [ ] "Enclose in Container" action — wraps selected FX in a new container
- [ ] "Explode Container" action — moves children out and removes container
- [ ] Toggle routing mode (serial/parallel) via context menu or inline button on container children
- [ ] Drag-and-drop FX between containers (stretch goal — can be deferred)
- [ ] All mutations call through daw-control facade to daw-reaper
- [ ] UI refreshes tree after mutations

## Functional Requirements

- FR-1: `FxNode` must support arbitrary nesting depth (containers within containers within containers)
- FR-2: `FxNodeId` must be stable across tree queries within a session (same FX = same ID as long as it hasn't been removed)
- FR-3: `FxNodeId` must NOT expose REAPER's raw `0x2000000` stride-based encoding to daw-proto consumers
- FR-4: `get_fx_tree()` must run on the main thread via `TaskSupport::main_thread_future()` (same pattern as all ReaperFx methods)
- FR-5: Container mutations must use `do_later_in_main_thread_asap()` (same pattern as existing mutations)
- FR-6: The existing `get_fx_list()` method must continue to return a flat list for backwards compatibility
- FR-7: `FxTree` must be serializable via Facet for RPC transport
- FR-8: All DAW UI components must live in `daw-ui`, not `signal-ui`
- FR-9: `signal-ui` must contain only rig management components (GuitarRigGrid, RigLayout, etc.)
- FR-10: Channel config read support (`container_nch`, `container_nch_in`, `container_nch_out`) must be present from the start

## Non-Goals (Out of Scope)
- Full pin mapping read/write (per-FX `SetPinMappings`) — future enhancement
- Visual routing diagram (ParanormalFX-style canvas with signal flow lines) — future enhancement
- Take FX container support (only track FX chains for now)
- FX preset browser/search within containers
- Undo/redo stack for container operations (rely on REAPER's native undo)
- Drag-and-drop reordering in UI (can be added later, context menu sufficient for now)

## Technical Considerations

### REAPER Container Addressing
REAPER uses a stride-based flat array with encoded offsets:
- Root FX: index `0` to `count-1`
- Container children: `0x2000000 + container_id + (stride * child_index)`
- Stride at depth 0: `parent_fx_count + 1`
- Stride at depth N: `(parent_count + 1) * previous_stride`
- Key config params: `container_count`, `parallel`, `fx_type`, `container_nch*`

### reaper-rs Limitations
- `TrackFxLocation` only has `NormalFxChain(u32)` / `InputFxChain(u32)` — no container variant
- Must use raw `i32` indices passed through `TrackFxLocation::NormalFxChain()` for container-addressed FX
- `GetNamedConfigParm` / `SetNamedConfigParm` already wrapped in reaper-rs medium layer
- Container operations use standard `TrackFX_AddByName("Container")`, `CopyToTrack`, `Delete`

### Crate Dependencies
```
daw-proto (types: FxNode, FxTree, FxNodeId, FxRoutingMode)
  -> daw-reaper (implementation: tree traversal, stride math, mutations)
       -> daw-control (facade: ergonomic async API)
            -> daw-ui (components: FxChainTree, FxChainMutationUI)
                 -> fts-control-desktop (main app, panel rendering)
```

### Migration Path (US-001)
- `signal-ui` currently contains: TrackControlPanel, ArrangementView, MixerPanel, FxBrowser
- All move to `daw-ui` — signal-ui keeps only rig components
- Desktop main.rs imports change from `signal_ui::` to `daw_ui::` for moved components

## Success Metrics
- `get_fx_tree()` returns correct hierarchy for tracks with nested containers (verified against REAPER state chunk)
- All container CRUD operations execute without corrupting the FX chain
- FxChainTree component renders multi-level nesting with correct indentation
- Existing flat `get_fx_list()` continues to work unchanged
- `signal-ui` has zero DAW imports remaining after migration

## Open Questions
- Should `FxNodeId` be GUID-based (stable across sessions) or session-scoped? GUIDs are stable but require GUID lookup on every resolve. Session-scoped IDs are faster but invalid after REAPER restart.
- Should `get_fx_tree()` be a separate method or should `get_fx_list()` be enhanced to optionally return tree structure?
- How should the FX event broadcaster detect container structure changes? Poll tree hash comparison vs. tracking individual mutations?
