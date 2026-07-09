# PRD: FTS-Control Web — Keyflow Chart Rendering Parity

## Overview

Bring full Keyflow chart rendering to the fts-control WASM web app so that any device on the local network can display the same live, interactive chord charts as the desktop app. The desktop currently renders charts via Vello/WGPU behind a transparent WebView overlay. The web app receives chart data (`SongChartHydrated` events via `SONG_CHARTS` signal) but never renders it. Since Vello and WGPU both support WASM (WebGPU/WebGL2), the same rendering pipeline can run in the browser with a platform-appropriate surface.

## Goals

- Render Keyflow charts in the WASM web app using the same Vello/WGPU pipeline as the desktop
- Support both `ChartView` (full-screen) and `ChartPreviewPanel` (compact preview) panels
- Live playback cursor tracking with auto-scroll and section highlighting
- Zero divergence in chart appearance between desktop and web
- Charts update in real-time as `SongChartHydrated` events arrive over WebSocket

## Quality Gates

These commands must pass for every user story:
- `cargo check -p fts-control-web` — native target check
- `cargo check -p fts-control-web --target wasm32-unknown-unknown` — WASM target check
- `cargo check -p keyflow-ui` — keyflow-ui still compiles with default features

## User Stories

### US-001: Create WASM-compatible ChartGraphics surface
**Description:** As a developer, I need a WASM variant of `ChartGraphics` that creates a Vello/WGPU renderer targeting an `HtmlCanvasElement` instead of a `tao::Window`, so the same chart rendering pipeline works in the browser.

**Acceptance Criteria:**
- [ ] New constructor `ChartGraphics::new_web(canvas: HtmlCanvasElement, width: u32, height: u32)` in `keyflow-ui/src/chart_graphics.rs` (or a sibling module) behind `#[cfg(target_arch = "wasm32")]`
- [ ] Creates a `wgpu::Surface` from the canvas element using `wgpu::Instance::create_surface_from_canvas()`
- [ ] Initializes `VelloWindowRenderer` (or equivalent anyrender abstraction) on the web surface
- [ ] `render_scene()` and `resize()` methods work identically to the desktop variant
- [ ] Desktop `ChartGraphics::new(window, w, h)` remains unchanged behind `#[cfg(not(target_arch = "wasm32"))]`
- [ ] `cargo check -p keyflow-ui --target wasm32-unknown-unknown` passes (with appropriate feature flags)

### US-002: Add keyflow-ui WASM feature flags and web app dependencies
**Description:** As a developer, I need `keyflow-ui` to expose a feature set that compiles for WASM (without desktop-only deps like `tao`), and the web app needs these dependencies added.

**Acceptance Criteria:**
- [ ] New `wasm-panels` feature in `keyflow-ui/Cargo.toml` that includes chart rendering deps (`anyrender_vello`, `vello`, `session-ui`, `daw-proto`) but excludes desktop-only deps (`dioxus::desktop`, `dock-dioxus`)
- [ ] `fts-control-web/Cargo.toml` depends on `keyflow-ui` with `wasm-panels` feature
- [ ] `fts-control-web/Cargo.toml` adds `vello`, `wgpu`, `anyrender`, `anyrender_vello` workspace dependencies as needed
- [ ] `web-sys` features in web app include `HtmlCanvasElement` and any required WebGPU/WebGL features
- [ ] Existing `keyflow-ui` `default = ["web"]` and `desktop-panels` features remain unchanged

### US-003: Integrate WGPU canvas element into web app Dioxus component tree
**Description:** As a developer, I need a Dioxus web component that creates and manages an `HtmlCanvasElement` for WGPU rendering, initializes `ChartGraphics`, and provides it via context to child components.

**Acceptance Criteria:**
- [ ] New `ChartCanvas` component (or similar) in the web app that renders a `<canvas>` element
- [ ] On mount, obtains the `HtmlCanvasElement` from the DOM and calls `ChartGraphics::new_web()`
- [ ] `ChartGraphics` stored in a signal or context provider accessible to chart panel components
- [ ] Canvas resizes correctly when the browser window resizes (calls `chart_graphics.resize()`)
- [ ] Canvas element has appropriate CSS styling (fills its container, correct z-index)
- [ ] Component cleans up WGPU resources on unmount

### US-004: Wire chart data flow from SONG_CHARTS signal to Vello renderer
**Description:** As a user viewing the web app, I want to see the current song's chord chart rendered in the browser, updating live as chart data arrives over WebSocket.

**Acceptance Criteria:**
- [ ] When `SONG_CHARTS` signal updates with a `SongChartHydration` for the active song, the chart renderer re-engraves and renders
- [ ] `SESSION_CHART_SOURCE` (from `keyflow-ui/src/signals.rs`) is synced when the active song changes, matching desktop behavior in `refresh_session_chart_source()`
- [ ] Chart engraver produces a Vello `Scene` that is passed to `ChartGraphics::render_scene()`
- [ ] Switching songs triggers a full chart re-render with the new song's data
- [ ] If no chart data exists for the current song, canvas shows empty/placeholder state

### US-005: Playback cursor tracking with auto-scroll and section highlighting
**Description:** As a performer viewing the web app on a tablet, I want the chart to show a live playback cursor that tracks the current position, auto-scrolls to keep the cursor visible, and highlights the active section.

**Acceptance Criteria:**
- [ ] Playback cursor renders at the correct position based on `SONG_TRANSPORT` state (measure/beat)
- [ ] Cursor updates at the same rate as transport events (~60Hz)
- [ ] Auto-scroll keeps the cursor within the visible viewport (same logic as desktop)
- [ ] Active section is visually highlighted (same styling as desktop)
- [ ] Audio latency compensation from `connection.rs` is applied to cursor position (already implemented in the web app's transport sync)
- [ ] Cursor disappears or resets when playback stops

### US-006: Adapt ChartView and ChartPreviewPanel for web
**Description:** As a user, I want both the full-screen chart editor view and the compact chart preview panel available in the web app, matching desktop parity.

**Acceptance Criteria:**
- [ ] `ChartView` component renders in the web app (full-screen chart display)
- [ ] `ChartPreviewPanel` component renders in the web app (compact inline preview)
- [ ] Components compile for `wasm32-unknown-unknown` target without `dock-dioxus` dependency (use web-appropriate layout instead)
- [ ] Tab or toggle to switch between Performance view and Chart view in the web app's tab bar
- [ ] Chart panels read from the same `SONG_CHARTS`, `CHART_AREA_BOUNDS`, and `SESSION_CHART_SOURCE` signals as desktop
- [ ] Visual output matches desktop rendering (same fonts, colors, spacing, notation)

## Functional Requirements

- FR-1: The web app must render Keyflow charts using the identical Vello scene graph as the desktop app — no separate HTML/CSS renderer
- FR-2: Chart rendering must use WebGPU when available, falling back to WebGL2 (wgpu handles this automatically)
- FR-3: The `chart_renderer.rs` module in `keyflow-ui` must be shared between desktop and web with zero code duplication
- FR-4: Canvas resolution must match device pixel ratio (`window.devicePixelRatio`) for crisp rendering on retina displays
- FR-5: Chart data arrives via the existing WebSocket gateway — no new endpoints or protocols required
- FR-6: The web app must handle the case where `ChartGraphics` initialization fails (e.g., no WebGPU/WebGL2 support) gracefully with a user-visible message

## Non-Goals

- Chart editing (text input, chord modification) in the web app — read-only display for now
- Offline chart rendering (charts require live connection to desktop gateway)
- Custom web-specific chart themes or styling — strict visual parity with desktop
- Touch gestures for chart navigation (pinch-zoom, swipe) — future enhancement
- Server-side chart rendering or image streaming

## Technical Considerations

- `keyflow-ui` lives in the `keyflow` repo at `/Users/codywright/Documents/Development/FastTrackStudio/keyflow/crates/keyflow-ui/`, not in the main FastTrackStudio workspace. Workspace dependency references need to account for this.
- `anyrender` is from `https://github.com/DioxusLabs/anyrender.git` (git dep). Verify its `VelloWindowRenderer` supports WASM, or determine if a `VelloCanvasRenderer` abstraction is needed.
- The desktop uses a transparent WebView overlay architecture (WGPU renders behind, Dioxus HTML floats on top). The web app doesn't need this trick — the canvas can be a regular DOM element within the Dioxus component tree.
- `wgpu` WASM support requires either WebGPU API (Chrome 113+, Firefox Nightly) or WebGL2 fallback. The `wgpu` crate handles this via feature flags (`webgpu`, `webgl`).
- Font loading for chart notation may differ on web (no system font access) — may need to bundle fonts as WASM assets.

## Success Metrics

- Keyflow chart renders identically in Chrome/Safari/Firefox on the local network as it does on the desktop app
- Playback cursor tracks within 1 frame of the desktop cursor position
- Chart re-renders within 100ms of receiving `SongChartHydrated` event
- No visual glitches or layout shifts during song transitions

## Open Questions

- Does `anyrender_vello`'s `VelloWindowRenderer` already support WASM canvas surfaces, or do we need a new renderer type?
- Are chart notation fonts bundled with `keyflow-ui` or loaded from the system? If system fonts, how do we handle web?
- Should the web app's chart view replace the Performance tab, or be a new tab alongside it?
