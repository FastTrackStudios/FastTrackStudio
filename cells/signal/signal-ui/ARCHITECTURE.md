# signal-ui Architecture & Conventions

Guidelines for Dioxus component development in signal-ui.

## State Management Decision Tree

### 1. Props (data flows parent → child)

Use props when:
- Data is passed from a parent component to a child
- The component is reusable across different contexts
- The data has a clear owner that controls its lifetime

```rust
#[component]
fn ModuleCard(name: String, block_type: BlockType, on_click: EventHandler<Uuid>) -> Element {
    // Pure presentational — receives everything from parent
}
```

### 2. `use_signal` (component-local state)

Use `use_signal` when:
- State is private to a single component
- State resets when the component unmounts (e.g., tab switches)
- State is UI-only (search queries, modal open/closed, form inputs)

```rust
fn PresetEditor() -> Element {
    let mut search_query = use_signal(String::new);
    let mut selected_id = use_signal(|| None::<Uuid>);
    // Dies when user switches tabs — correct behavior
}
```

**Rule**: If a `static` GlobalSignal is only read/written in one file, convert it to `use_signal`.

### 3. GlobalSignal (true app-wide singletons)

Use GlobalSignal **only** when:
- The state is a service singleton (`RIG_SERVICE`)
- Multiple dock panels need the same live data (`RIG_NODE_GRAPH`, `RIG_MODULES`)
- The state must survive component unmount/remount (`DOCK_WORKSPACE`)
- Cross-crate access is required (`SETLIST_STRUCTURE`, `ACTIVE_INDICES`)

**Never** use GlobalSignal for:
- Editor-local selection, search text, form state
- Modal open/closed flags
- Temporary UI state

### 4. Context Providers (shared state within a subtree)

Use context when:
- A subtree of components shares state that doesn't belong globally
- You want to avoid prop drilling through 3+ levels
- The state is scoped to a feature area (e.g., "rig editor" context)

```rust
// Parent provides
use_context_provider(|| Signal::new(EditorState::default()));

// Any descendant consumes
let state = use_context::<Signal<EditorState>>();
```

**Candidates for future context migration** (from GlobalSignal):
- `RIG_GRID_SELECTED_SLOT` → RigGridEditor context
- `RIG_SELECTED_ENTITY` → NodeGraph context
- `DOCK_RESIZING`, `DOCK_DRAG_STATE` → DockRoot context

---

## Legitimate GlobalSignals

### signal-ui/src/signals.rs (~27 signals)

| Signal | Type | Justification |
|--------|------|---------------|
| `RIG_SERVICE` | `Option<SignalControl>` | Service singleton — any dock panel dispatches actions |
| `RIG_EDITOR_TAB` | `RigEditorTab` | Cross-crate: desktop reads for tab routing |
| `RIG_PROFILE` | `Option<ProfileInfo>` | Shared across preset/song/profile editors |
| `RIG_AVAILABLE_PROFILES` | `Vec<ProfileInfo>` | Shared across profile browser + song editor |
| `RIG_INFO` | `Option<RigInfo>` | Global rig metadata |
| `RIG_CURRENT_PRESET` | `Option<PresetInfo>` | Live rig state — displayed in multiple panels |
| `RIG_AVAILABLE_PRESETS` | `Vec<PresetInfo>` | Shared across preset browser + left sidebar |
| `RIG_PRELOADED_PRESETS` | `Vec<PresetInfo>` | Preload cache for instant switching |
| `RIG_CURRENT_SETLIST` | `Option<SetlistInfo>` | Live navigation state |
| `RIG_AVAILABLE_SETLISTS` | `Vec<SetlistInfo>` | Shared across song editor + setlist views |
| `RIG_SETLIST_SONGS` | `Vec<SongInfo>` | Song list for current setlist |
| `RIG_CURRENT_SONG` | `Option<SongInfo>` | Live navigation state |
| `RIG_SONG_INDEX` | `usize` | Playback position |
| `RIG_CURRENT_SCENE` | `Option<ProfileSceneInfo>` | Live scene state |
| `RIG_SCENE_INDEX` | `usize` | Playback position |
| `RIG_MODULES` | `Vec<Module>` | Shared across node graph + module editor |
| `RIG_FX_CHAIN` | `Option<FxChain>` | Live DAW FX state |
| `RIG_NODE_FX_BINDINGS` | `HashMap<Uuid, NodeFxBinding>` | FX-to-node binding state |
| `RIG_FX_BINDING` | `Option<FxRigBinding>` | FX binding config |
| `RIG_FX_BINDING_STATUS` | `String` | Binding status text |
| `RIG_CONNECTED` | `bool` | Global connection flag |
| `RIG_LOADING` | `bool` | Global loading flag |
| `RIG_NODE_GRAPH` | `NodeGraph` | Flow view graph — shared across dock panels |
| `RIG_SNAPSHOTS` | `Vec<RigSnapshot>` | Snapshot list — shared across panels |
| `RIG_CURRENT_PRESET_SNAPSHOT_ID` | `Option<Uuid>` | Active snapshot tracking |
| `RIG_LAST_APPLIED_SNAPSHOT` | `Option<Uuid>` | Morph target tracking |

### Localization candidates (move to context in US-010)

| Signal | Current Scope | Target |
|--------|--------------|--------|
| `RIG_GRID_SELECTED_SLOT` | 2 dock panels | RigGridEditor context |
| `RIG_SELECTED_ENTITY` | NodeGraph + PropertyPanel | NodeGraph context |

---

## Component Guidelines

### Size limits

- **Soft limit**: 300 lines per component file
- **Hard limit**: 500 lines — split into sub-components or extract helpers
- Current largest: `module_editor_view.rs` (~1,200 lines) — being addressed

### File structure

```
component_name/
├── mod.rs              # pub use re-exports
├── component_view.rs   # Main component (the #[component] fn)
├── helpers.rs          # Non-UI logic (parsing, formatting, data transforms)
└── sub_component.rs    # Extracted child components
```

### Shared components

Reusable layout components live in `components/shared/`:
- `EntityEditor` — 3-panel editor shell (left/center/right + accent strip + status bar)

Reusable hooks live in `hooks/`:
- `use_fuzzy_search` — Generic nucleo-based fuzzy search with scoring
- `use_rig_subscription` — ROAM subscription lifecycle
- `use_rig_actions` — Rig action dispatching

### Async patterns

**Prefer `use_effect` + signals** over bare `spawn()`:

```rust
// Good: Reactive — re-runs when dependencies change
use_effect(move || {
    let ctl = ctl.clone();
    spawn(async move {
        if let Ok(list) = ctl.list_presets().await {
            preset_list.set(list);
        }
    });
});

// Avoid: Imperative — runs once, doesn't react to changes
spawn(async move {
    let list = ctl.list_presets().await.unwrap();
    preset_list.set(list);
});
```

**For one-shot loads**, `use_resource` is acceptable:
```rust
let data = use_resource(move || async move {
    ctl.fetch_data().await.ok()
});
```

### Event handler pattern

Clone signal values before entering closures to avoid holding read guards:

```rust
fn MyComponent() -> Element {
    let items = use_signal(Vec::<Item>::new);

    // Clone data for template rendering (read guard dropped immediately)
    let items_list = items.cloned();

    rsx! {
        for item in items_list.iter() {
            button {
                onclick: move |_| {
                    // Use the signal directly in handlers (captures by move)
                    let mut current = items.cloned();
                    current.push(new_item);
                    items.set(current);
                },
            }
        }
    }
}
```

### EntityEditor usage

All entity editors (block, preset, module, song, profile) share the `EntityEditor` shell:

```rust
use crate::components::shared::EntityEditor;

EntityEditor {
    accent_gradient: Some("from-purple-500 via-orange-400 to-emerald-500".to_string()),
    left_width: "w-52".to_string(),
    right_width: "w-56".to_string(),
    left: rsx! { /* browser panel content */ },
    center: rsx! { /* main editor content */ },
    right: Some(rsx! { /* picker/guide panel */ }),
    status: rsx! {
        div { class: "w-1.5 h-1.5 rounded-full bg-purple-400/60" }
        span { class: "text-[10px] text-zinc-500 font-mono truncate flex-1", "{status}" }
    },
}
```
