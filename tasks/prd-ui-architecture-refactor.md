# PRD: UI Architecture Refactor — Dioxus Best Practices & Maintainability

## Overview

The signal-ui frontend crate has accumulated significant technical debt across its 70 files and ~25,500 lines of code. The core issues are: (1) overuse of GlobalSignal as a state management crutch (46 globals instead of props/context), (2) monolithic component files (5 files over 1,000 lines), (3) duplicated editor patterns (~5,000 lines across 5 nearly-identical editor views), and (4) a centralized PanelId enum that requires touching 7 match arms for every new dock panel.

These issues cause "things work then stop working" symptoms — stale closures from 82 `spawn(async)` calls mutating globals, hidden component dependencies, and race conditions between concurrent async tasks.

## Goals

- Reduce GlobalSignal count from 46 to ~15-20 (eliminate component-scoped globals)
- Extract shared patterns (EntityEditor, FuzzySearch) to reduce duplication by ~5,000 lines
- Split monolithic files to enforce a soft 300-line / hard 500-line component limit
- Decentralize dock panel registration so each domain crate owns its panels
- Extract `ChartView` (390 lines) and `ChartPreviewPanel` (524 lines) from main.rs to library crates
- Establish clear conventions for props vs globals vs context

## Quality Gates

These commands must pass for every user story:
- `cargo check -p signal-ui` - Signal UI compiles
- `cargo check -p dock-proto` - Dock proto compiles
- `cargo check -p fts-control-desktop` - Desktop app compiles
- No new warnings introduced (pre-existing warnings are acceptable)

## User Stories

### US-001: Extract component-local GlobalSignals to local state

**Description:** As a developer, I want component-scoped state to use `use_signal` instead of `GlobalSignal` so that state ownership is clear and components can be tested in isolation.

**Acceptance Criteria:**
- [ ] `daw_snapshot_panel.rs` — Convert its 13 file-scoped GlobalSignals (PANEL_STATUS, SNAPSHOT_CACHE, TRACKED_TRACK_GUID, etc.) to local `use_signal` within the component
- [ ] `block_editor/library.rs` — Convert its 9 file-scoped GlobalSignals (BLOCK_LIBRARY, SELECTED_BLOCK_TYPE, etc.) to local `use_signal`
- [ ] `song_editor_view.rs` — Convert its 9 file-scoped GlobalSignals (SONG_LIST, SELECTED_SONG_ID, etc.) to local `use_signal`
- [ ] All converted signals are passed as props or read from context where cross-component sharing is needed
- [ ] No change in visible behavior — all UI features continue to work identically
- [ ] Net reduction of at least 18 GlobalSignals

### US-002: Create shared EntityEditor layout component

**Description:** As a developer, I want a reusable three-panel editor component so that the 5 editor views (Block, Module, Preset, Song, Profile) share layout logic instead of duplicating it.

**Acceptance Criteria:**
- [ ] Create `components/shared/entity_editor.rs` with a generic `EntityEditor` component that accepts left (browser), center (editor), and right (detail) panes as child elements
- [ ] EntityEditor handles the common layout: flex row, resizable panel widths, selection state pattern
- [ ] Refactor `block_editor_view.rs` to use `EntityEditor` as a proof-of-concept (should reduce from ~1,449 lines to ~400 lines)
- [ ] Refactor `preset_editor_view.rs` to use `EntityEditor` (reduce from ~1,111 to ~300 lines)
- [ ] Existing editor functionality (CRUD, selection, search) is preserved

### US-003: Create shared FuzzySearch hook

**Description:** As a developer, I want a single `use_fuzzy_search` hook so that the 3 separate fuzzy search implementations (preset browser, profile browser, module browser) are consolidated.

**Acceptance Criteria:**
- [ ] Create `hooks/use_fuzzy_search.rs` with a generic `use_fuzzy_search<T>` hook using nucleo
- [ ] Hook accepts: items list, key extractor function, returns filtered+scored results
- [ ] Refactor `left_sidebar.rs` preset search to use the new hook
- [ ] Refactor `profile_sidebar.rs` profile search to use the new hook
- [ ] Refactor `module_browser_modal.rs` module search to use the new hook
- [ ] Search behavior is identical to current implementation

### US-004: Split node_graph.rs into focused modules

**Description:** As a developer, I want the node_graph data model (1,745 lines) to be split into focused files so that it's easier to understand and maintain.

**Acceptance Criteria:**
- [ ] Extract `node_graph/models.rs` — Node, Wire, GraphModule, NodeParameter, ParameterType structs
- [ ] Extract `node_graph/snapshot.rs` — RigSnapshot, snapshot capture/restore logic
- [ ] Extract `node_graph/builder.rs` — create_module_for_block_type, sample_guitar_rig, find_open_position
- [ ] Keep `node_graph/mod.rs` as the public API re-exporting all types
- [ ] All existing imports of `node_graph::*` types continue to work without changes
- [ ] No file exceeds 500 lines

### US-005: Split node_graph_view.rs into focused components

**Description:** As a developer, I want the node graph view (1,397 lines) decomposed into smaller components so that each concern (drag, wire rendering, context menu) can be maintained independently.

**Acceptance Criteria:**
- [ ] Extract `node_graph/canvas.rs` — The main SVG/canvas container and coordinate system
- [ ] Extract `node_graph/wire_renderer.rs` — SVG wire/connection rendering (~200 lines)
- [ ] Extract `node_graph/context_menu.rs` — Right-click context menu logic (~150 lines)
- [ ] Extract `node_graph/drag_handler.rs` — Drag-and-drop state machine (~200 lines)
- [ ] The parent `NodeGraphView` composes these sub-components
- [ ] No file exceeds 500 lines
- [ ] All drag, wire, and context menu interactions work identically

### US-006: Refactor remaining editor views to use EntityEditor

**Description:** As a developer, I want the remaining 3 editor views (Module, Song, Profile) to use the shared `EntityEditor` component from US-002.

**Acceptance Criteria:**
- [ ] Refactor `module_editor_view.rs` to use `EntityEditor` (reduce from ~1,367 to ~400 lines)
- [ ] Refactor `song_editor_view.rs` to use `EntityEditor` (reduce from ~779 to ~300 lines)
- [ ] Refactor `profile_editor_view.rs` to use `EntityEditor` (reduce from ~545 to ~200 lines)
- [ ] All editor-specific CRUD operations, search, and selection continue to work
- [ ] Total line reduction across all 5 editors is at least 3,000 lines

### US-007: Decentralize dock panel registration

**Description:** As a developer, I want each domain crate to register its own dock panels so that adding a new panel doesn't require editing the central PanelId enum.

**Acceptance Criteria:**
- [ ] Create a `PanelRendererRegistry` in dock-proto that maps string IDs to render functions
- [ ] Add a `register_panels()` function to signal-ui that registers all rig/signal panels with the renderer registry
- [ ] Add a `register_panels()` function to session-ui for session panels
- [ ] Add a `register_panels()` function to daw-ui for DAW panels
- [ ] The PanelRenderer match in main.rs is replaced by a registry lookup
- [ ] Keep the PanelId enum for built-in panels but make it non-exhaustive
- [ ] Adding a new panel requires changes only in the domain crate (no edits to panel.rs or main.rs)

### US-008: Extract ChartView and ChartPreviewPanel from main.rs

**Description:** As a developer, I want chart rendering components extracted from main.rs to library crates so that main.rs is focused on app initialization and routing.

**Acceptance Criteria:**
- [ ] Move `ChartView` (~390 lines) to `keyflow_ui::components::chart_editor_panel`
- [ ] Move `ChartPreviewPanel` (~524 lines) to `session_ui::components::chart_preview_panel` or `keyflow_ui`
- [ ] Move performance tracking structs (FpsTracker, PerfStaticSceneKey, etc.) to a rendering utilities module
- [ ] main.rs is reduced by at least 900 lines
- [ ] Chart rendering continues to work identically (WGPU surface, auto-follow, cursor)

### US-009: Establish Dioxus component conventions and document patterns

**Description:** As a developer, I want a clear set of conventions for when to use props vs GlobalSignal vs context so that new components follow consistent patterns.

**Acceptance Criteria:**
- [ ] Create `cells/signal/signal-ui/ARCHITECTURE.md` documenting:
  - When to use props (data flows parent→child, component is reusable)
  - When to use GlobalSignal (true app-wide singleton state like RIG_SERVICE, RIG_NODE_GRAPH)
  - When to use context (shared state within a subtree, e.g., dock workspace)
  - Component size guidelines (soft 300 / hard 500 lines)
  - Async pattern: use_effect + use_resource instead of bare spawn()
- [ ] List the ~15-20 GlobalSignals that are legitimately global (RIG_SERVICE, RIG_NODE_GRAPH, etc.)
- [ ] Mark all others as candidates for localization in future work

### US-010: Implement context providers for shared rig state

**Description:** As a developer, I want context providers for shared rig state so that components declare their dependencies explicitly instead of reading globals silently.

**Acceptance Criteria:**
- [ ] Create `RigStateProvider` context that bundles the core rig state (profile, preset, setlist, modules)
- [ ] Create `DawBindingProvider` context that bundles DAW binding state (fx_chain, fx_binding, binding_status)
- [ ] Update at least 5 components to use `use_context::<RigState>()` instead of directly reading GlobalSignals
- [ ] The provider is mounted once in the app root (or dock panel wrapper)
- [ ] Components that use context have explicit, traceable dependencies

## Functional Requirements

- FR-1: The EntityEditor component must support flexible left/center/right pane sizing with CSS flex
- FR-2: The FuzzySearch hook must maintain feature parity with existing nucleo-based search (scoring, ranking, case-insensitive matching)
- FR-3: Panel registration must be backward-compatible — existing PanelId enum values must continue to work for serialized dock layouts
- FR-4: All async operations in refactored components must use `use_effect` or `use_resource` with proper cleanup, not bare `spawn()`
- FR-5: Extracted chart components must maintain WGPU surface lifecycle (creation, resize, render loop)

## Non-Goals

- Complete elimination of all GlobalSignals (some are legitimately global)
- Rewriting the dock system from scratch (it's well-architected, just needs decentralization)
- UI visual redesign — this is a structural/architectural refactor only
- Removing the classic tab system (it's ~50 lines and provides a useful fallback)
- Migrating away from Dioxus framework

## Technical Considerations

- **Dioxus version**: Current codebase uses Dioxus 0.6 with `GlobalSignal`, `use_signal`, `use_effect`, `use_resource`, `spawn`
- **WGPU integration**: ChartView and ChartPreviewPanel interact with the WGPU surface via JS eval and window hooks — extraction must preserve this interface
- **Feature flags**: `web` (Wry WebView) vs `native` (Blitz/Vello) — prelude.rs handles this, refactored components must respect it
- **Crate dependency order**: signal-proto → signal → signal-control → signal-ui. EntityEditor goes in signal-ui. FuzzySearch hook goes in signal-ui.
- **The PanelRegistry in dock-proto already exists** (541 lines) — US-007 promotes it from metadata store to primary source of truth

## Success Metrics

- GlobalSignal count reduced from 46 to ≤20
- No file in signal-ui exceeds 500 lines (currently 10 files exceed this)
- main.rs reduced from 2,376 lines to ≤1,400 lines
- Total duplicated code across editor views reduced by ≥3,000 lines
- Adding a new dock panel requires changes in only 1 crate (currently requires 3)

## Open Questions

- Should the EntityEditor support drag-to-resize between panes, or is CSS flex with fixed ratios sufficient for now?
- Should we introduce a trait-based approach for editor entities (EntityBrowser<T>, EntityDetail<T>) or keep it simpler with element slots?
- For context providers, should RigState be a single monolith context or split into smaller focused contexts (PresetContext, SetlistContext, etc.)?
