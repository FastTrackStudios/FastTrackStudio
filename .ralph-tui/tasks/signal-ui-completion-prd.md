# PRD: Signal UI Completion

Complete the signal-ui node graph system for guitar rig control, transforming it from a working interactive canvas with mock data into a fully-featured rig editor with persistence, parameter editing, and real service integration.

## Background

The signal-ui crate provides a Dioxus-based node graph interface for controlling guitar rigs (VST effects chains). The current implementation includes:

**Working:**
- Interactive canvas with pan/zoom/drag
- Module and node rendering with wire connections
- View modes (Flow, FlowCompact, Preset, Profile, Song)
- Global signal state management (17 signals)
- Mock service integration with sample guitar rig
- Keyboard shortcuts and context menus

**Incomplete:**
- Graph state resets on reload (no persistence)
- Parameter editing not implemented (display-only)
- Mock backend only (no real service)
- No undo/redo history
- Signal flow grid view disconnected

## Quality Gates

These commands must pass for every user story:
- `cargo check -p signal-ui` - Type checking
- `cargo clippy -p signal-ui -- -D warnings` - Linting
- `cargo test -p signal-ui` - Unit tests (if applicable)

For UI stories, also include:
- Build the desktop app with `cargo build -p fts-control-desktop`

## User Stories

### US-001: Add graph state persistence to local storage

**Description:** As a user, I want my node graph layout to persist across app reloads so I don't lose my rig configuration.

**Acceptance Criteria:**
- [ ] Create `GraphPersistence` trait in `node_graph.rs` with `save()` and `load()` methods
- [ ] Implement JSON serialization for `NodeGraph` using serde
- [ ] Add `#[derive(Serialize, Deserialize)]` to NodeGraph, GraphModule, Node, Wire, and related types
- [ ] Create `use_graph_persistence()` hook that saves on graph changes
- [ ] Load persisted graph on app startup instead of `sample_guitar_rig()`
- [ ] Fall back to `sample_guitar_rig()` if no persisted graph exists
- [ ] Store graph in `.signal/graph.json` or browser localStorage

### US-002: Add parameter editor modal for nodes

**Description:** As a user, I want to click a node and edit its parameters so I can configure my effects.

**Acceptance Criteria:**
- [ ] Create `ParameterEditor` component in `components/rig_grid/parameter_editor.rs`
- [ ] Add `parameters: Vec<Parameter>` field to `Node` struct
- [ ] Create `Parameter` struct with `id`, `name`, `value`, `min`, `max`, `unit`, `param_type`
- [ ] Implement `ParameterType` enum: `Continuous`, `Stepped`, `Toggle`, `Choice(Vec<String>)`
- [ ] Render appropriate control for each parameter type (knob, slider, toggle, dropdown)
- [ ] Use audio-controls widgets (Knob, HSlider) for continuous parameters
- [ ] Double-click node to open parameter editor modal
- [ ] Wire parameter changes to `set_parameter` action from `use_rig_actions()`
- [ ] Close modal with Escape key or clicking outside

### US-003: Implement undo/redo system for graph operations

**Description:** As a user, I want to undo and redo my changes so I can experiment without fear of losing work.

**Acceptance Criteria:**
- [ ] Create `GraphHistory` struct in `node_graph.rs` with undo/redo stacks
- [ ] Define `GraphOperation` enum: `MoveModule`, `MoveNode`, `AddWire`, `RemoveWire`, `AddModule`, `RemoveModule`, `AddNode`, `RemoveNode`, `BypassToggle`, `ParameterChange`
- [ ] Implement `push_operation()`, `undo()`, `redo()` methods
- [ ] Add `GRAPH_HISTORY` global signal in `signals.rs`
- [ ] Create `use_graph_history()` hook exposing undo/redo callbacks
- [ ] Wire Cmd+Z / Ctrl+Z to undo, Cmd+Shift+Z / Ctrl+Y to redo
- [ ] Show undo/redo buttons in top toolbar with disabled state when stack is empty
- [ ] Limit history to 50 operations to prevent memory bloat

### US-004: Add node property panel in sidebar

**Description:** As a user, I want to see and edit the selected node's properties in a sidebar panel so I can make quick adjustments without opening a modal.

**Acceptance Criteria:**
- [ ] Create `NodePropertyPanel` component in `components/rig_grid/node_property_panel.rs`
- [ ] Show panel in right sidebar when a node or module is selected
- [ ] Display: name (editable), block type, bypass status, position, size
- [ ] Show parameter list with inline editing (mini knobs/sliders)
- [ ] Add "Open Full Editor" button that opens parameter editor modal
- [ ] Show input/output port list with connection status
- [ ] Update panel reactively when selection changes
- [ ] Hide panel when nothing is selected (show placeholder text)

### US-005: Implement module preset save/load

**Description:** As a user, I want to save and load module configurations as presets so I can reuse my favorite settings.

**Acceptance Criteria:**
- [ ] Create `ModulePresetManager` in `components/rig_grid/module_preset_manager.rs`
- [ ] Add "Save as Preset" option to module context menu
- [ ] Create preset save dialog with name input and optional tags
- [ ] Store module presets in `.signal/module-presets/` directory
- [ ] Add "Load Preset" option to module context menu
- [ ] Show preset browser when loading (list with search/filter)
- [ ] Apply preset by replacing module's nodes and internal wires
- [ ] Preserve module position and external connections when loading preset

### US-006: Connect real RigControlService when available

**Description:** As a developer, I need the signal-ui to detect and use the real rig control backend when available, falling back to mock data otherwise.

**Acceptance Criteria:**
- [ ] Add `RigServiceMode` enum in `context/rig.rs`: `Mock`, `Real(RigControlService)`
- [ ] Implement service discovery/detection logic
- [ ] Update `RigServiceProvider` to accept mode configuration
- [ ] Add connection status indicator in top bar (green=connected, yellow=mock, red=disconnected)
- [ ] Show "Mock Mode" badge when using mock service
- [ ] Auto-reconnect on service availability change
- [ ] Log service mode transitions for debugging

### US-007: Add live audio visualization to node widgets

**Description:** As a user, I want to see real-time audio analysis in my EQ and compressor nodes so I can see how my effects are processing the signal.

**Acceptance Criteria:**
- [ ] Create `AudioVisualizationService` trait for providing audio data
- [ ] Add `audio_data: Option<AudioData>` prop to node widget components
- [ ] Update EqGraph widget to display frequency spectrum overlay
- [ ] Update CompressorGraph to show gain reduction meter
- [ ] Update GateGraph to show gate open/close state
- [ ] Add VU meters to module I/O ports when audio is flowing
- [ ] Use 60fps animation frame for smooth updates
- [ ] Fall back to static display when no audio data available

### US-008: Resurrect signal flow grid view

**Description:** As a user, I want an alternative grid-based view (like Quad Cortex) for simpler rig visualization without the full node graph complexity.

**Acceptance Criteria:**
- [ ] Review existing `signal_flow_grid.rs` implementation
- [ ] Add `Grid` variant to `ModuleViewMode` enum in `view_mode.rs`
- [ ] Wire grid view into `GuitarRigGrid` component
- [ ] Add view mode toggle button (Flow | Grid) in top bar
- [ ] Render 14x6 grid with blocks in their assigned positions
- [ ] Show I/O jacks on grid edges
- [ ] Support block bypass toggle via click
- [ ] Sync grid state with node graph state (same underlying model)

### US-009: Add module group collapse/expand

**Description:** As a user, I want to collapse module groups to reduce visual complexity when I'm focused on a specific part of my rig.

**Acceptance Criteria:**
- [ ] Add `collapsed: bool` field to `GraphModule` struct
- [ ] Add collapse/expand toggle button to module title bar
- [ ] When collapsed: hide internal nodes/wires, show compact summary
- [ ] Show badge with node count when collapsed (e.g., "5 blocks")
- [ ] Animate collapse/expand transition (height interpolation)
- [ ] Preserve collapse state in persistence
- [ ] Add "Collapse All" / "Expand All" buttons in top toolbar

### US-010: Implement wire color coding by signal type

**Description:** As a user, I want wires colored by signal type so I can quickly understand my routing at a glance.

**Acceptance Criteria:**
- [ ] Add `signal_type: SignalType` field to `NodePort` struct
- [ ] Define `SignalType` enum: `Audio`, `Midi`, `Control`, `Sidechain`, `Send`
- [ ] Assign default signal types to common block types
- [ ] Update wire rendering to use signal type colors:
  - Audio: white/gray
  - MIDI: blue
  - Control/CV: orange
  - Sidechain: purple
  - Send/Return: green
- [ ] Add legend in corner showing wire color meanings
- [ ] Allow user to override wire color via context menu

