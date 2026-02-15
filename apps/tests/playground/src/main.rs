//! Playground for signal2 collection/variant UI.
//!
//! Tabbed gallery showcasing every smart view in `signal2_ui::views`.

#[cfg(feature = "desktop")]
use anyrender::PaintScene;
#[cfg(feature = "desktop")]
use anyrender::WindowRenderer;
#[cfg(feature = "desktop")]
use anyrender_vello::{VelloRendererOptions, VelloWindowRenderer};
#[cfg(feature = "desktop")]
use dioxus::desktop::tao::window::Window;
#[cfg(feature = "desktop")]
use dioxus::desktop::{tao::window::WindowBuilder, Config};
use dioxus::prelude::dioxus_elements::geometry::WheelDelta;
use dioxus::prelude::*;
use frame_import::import_figma_bytes;
use frame_proto::{FrameDocument, NodeId};
use frame_ui::{build_paint_primitives, paint_primitives_into_scene_with, TextFontRef};
#[cfg(feature = "desktop")]
use kurbo::{Affine, Rect, Stroke};
#[cfg(feature = "desktop")]
use peniko::{Color, Fill};
use signal2::{bootstrap_in_memory_controller_async, BlockType, SignalController};
use signal2_ui::views::{
    BlockEditor, CollectionBrowser, MetadataDisplay, ModuleView, ModuleViewMode, RigSceneGrid,
    SignalSlider,
};
#[cfg(feature = "desktop")]
use std::sync::{Arc, Mutex, OnceLock};
#[cfg(feature = "desktop")]
use std::{
    net::TcpStream,
    process::{Child, Command, Stdio},
    time::Duration,
};

const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");
const MAIN_CSS: Asset = asset!("/assets/main.css");
const FRAME_BRIDGE_JSON_PATH: &str = "/tmp/fts-figma-export.json";
#[cfg(feature = "desktop")]
const FRAME_BRIDGE_ADDR: &str = "127.0.0.1:43123";
#[cfg(feature = "desktop")]
const FRAME_BRIDGE_WORKDIR: &str =
    "/Users/codywright/Documents/Development/Rust/roam-test/packages/fts-figma-import";

// region: --- Tab definitions

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Tab {
    Slider,
    BlockEditor,
    ModuleView,
    CollectionBrowser,
    SceneGrid,
    Metadata,
    Frame,
}

impl Tab {
    const ALL: &[Tab] = &[
        Tab::Slider,
        Tab::BlockEditor,
        Tab::ModuleView,
        Tab::CollectionBrowser,
        Tab::SceneGrid,
        Tab::Metadata,
        Tab::Frame,
    ];

    fn label(self) -> &'static str {
        match self {
            Self::Slider => "Slider",
            Self::BlockEditor => "Block Editor",
            Self::ModuleView => "Module View",
            Self::CollectionBrowser => "Browser",
            Self::SceneGrid => "Scene Grid",
            Self::Metadata => "Metadata",
            Self::Frame => "Frame",
        }
    }
}

// endregion: --- Tab definitions

fn main() {
    #[cfg(feature = "desktop")]
    {
        let config = Config::new()
            .with_window(
                WindowBuilder::new()
                    .with_title("Playground")
                    .with_transparent(true),
            )
            .with_background_color((0, 0, 0, 0))
            .with_on_window(|window, dom| {
                let size = window.inner_size();
                let graphics = Arc::new(Mutex::new(FrameGraphics::new(
                    window,
                    size.width,
                    size.height,
                )));
                dom.provide_root_context(graphics);
            })
            .with_as_child_window();

        dioxus::LaunchBuilder::desktop()
            .with_cfg(config)
            .launch(App);
    }

    #[cfg(not(feature = "desktop"))]
    dioxus::launch(App);
}

#[cfg(feature = "desktop")]
struct FrameGraphics {
    renderer: VelloWindowRenderer,
    width: u32,
    height: u32,
}

#[cfg(feature = "desktop")]
impl FrameGraphics {
    fn new(window: Arc<Window>, width: u32, height: u32) -> Self {
        let mut renderer = VelloWindowRenderer::with_options(VelloRendererOptions {
            base_color: Color::TRANSPARENT,
            ..Default::default()
        });
        renderer.resume(window, width, height);
        Self {
            renderer,
            width,
            height,
        }
    }

    fn render<F>(&mut self, draw_fn: F)
    where
        F: FnOnce(&mut <VelloWindowRenderer as WindowRenderer>::ScenePainter<'_>),
    {
        self.renderer.render(draw_fn);
    }

    fn resize(&mut self, width: u32, height: u32) {
        self.width = width;
        self.height = height;
        self.renderer.set_size(width, height);
    }

    fn size(&self) -> (u32, u32) {
        (self.width, self.height)
    }
}

#[cfg(feature = "desktop")]
impl Drop for FrameGraphics {
    fn drop(&mut self) {
        self.renderer.suspend();
    }
}

#[cfg(feature = "desktop")]
fn ensure_frame_bridge_running() -> Result<(), String> {
    if TcpStream::connect_timeout(
        &FRAME_BRIDGE_ADDR
            .parse()
            .map_err(|e| format!("invalid bridge addr {FRAME_BRIDGE_ADDR}: {e}"))?,
        Duration::from_millis(150),
    )
    .is_ok()
    {
        return Ok(());
    }

    static BRIDGE_CHILD: OnceLock<Mutex<Option<Child>>> = OnceLock::new();
    let child_slot = BRIDGE_CHILD.get_or_init(|| Mutex::new(None));
    let mut guard = child_slot
        .lock()
        .map_err(|_| "failed to acquire bridge process lock".to_string())?;

    if guard.is_some() {
        return Ok(());
    }

    let child = Command::new("pnpm")
        .arg("bridge")
        .current_dir(FRAME_BRIDGE_WORKDIR)
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .map_err(|e| format!("failed to start bridge in {FRAME_BRIDGE_WORKDIR}: {e}"))?;

    *guard = Some(child);
    Ok(())
}

#[component]
fn App() -> Element {
    let mut controller = use_signal(|| None::<SignalController>);
    let mut active_tab = use_signal(|| Tab::Frame);
    let needs_transparency = active_tab() == Tab::Frame;

    use_effect(move || {
        spawn(async move {
            let built = bootstrap_in_memory_controller_async()
                .await
                .expect("failed to initialize in-memory signal storage");
            controller.set(Some(built));
        });
    });

    #[cfg(feature = "desktop")]
    use_effect(move || {
        if active_tab() == Tab::Frame {
            document::eval(r#"document.documentElement.classList.add('transparent-mode');"#);
        } else {
            document::eval(r#"document.documentElement.classList.remove('transparent-mode');"#);
        }
    });

    rsx! {
        document::Link { rel: "stylesheet", href: TAILWIND_CSS }
        document::Link { rel: "stylesheet", href: MAIN_CSS }

        div {
            class: if needs_transparency {
                "h-screen w-screen flex flex-col text-zinc-100"
            } else {
                "h-screen w-screen flex flex-col bg-zinc-950 text-zinc-100"
            },
            style: if needs_transparency {
                "background: transparent !important; background-color: transparent !important;"
            } else {
                ""
            },
            // Tab bar
            nav {
                class: if needs_transparency {
                    "flex items-center gap-1 px-4 py-2 border-b border-zinc-800/80 bg-zinc-900/50 flex-shrink-0"
                } else {
                    "flex items-center gap-1 px-4 py-2 border-b border-zinc-800 bg-zinc-900/80 flex-shrink-0"
                },
                span { class: "text-xs font-bold text-zinc-500 uppercase tracking-wider mr-3", "signal2" }
                for tab in Tab::ALL.iter() {
                    {
                        let t = *tab;
                        let is_active = active_tab() == t;
                        rsx! {
                            button {
                                key: "{t:?}",
                                class: if is_active {
                                    "px-3 py-1.5 text-sm font-medium rounded bg-zinc-700 text-zinc-100"
                                } else {
                                    "px-3 py-1.5 text-sm font-medium rounded text-zinc-400 hover:text-zinc-200 hover:bg-zinc-800"
                                },
                                onclick: move |_| active_tab.set(t),
                                "{t.label()}"
                            }
                        }
                    }
                }
            }

            // Content area
            div {
                class: if needs_transparency {
                    "flex-1 min-h-0 overflow-hidden"
                } else {
                    "flex-1 min-h-0 overflow-auto"
                },
                style: if needs_transparency {
                    "background: transparent !important; background-color: transparent !important;"
                } else {
                    ""
                },
                if let Some(ctrl) = controller() {
                    {render_tab(active_tab(), ctrl)}
                } else {
                    div { class: "flex items-center justify-center h-full",
                        p { class: "text-sm text-zinc-600", "Bootstrapping in-memory signal storage..." }
                    }
                }
            }
        }
    }
}

fn render_tab(tab: Tab, controller: SignalController) -> Element {
    match tab {
        Tab::Slider => rsx! {
            div { class: "max-w-2xl mx-auto p-6",
                SignalSlider { controller }
            }
        },
        Tab::BlockEditor => rsx! {
            div { class: "max-w-xl mx-auto p-6 space-y-6",
                h2 { class: "text-lg font-semibold text-zinc-300 mb-4", "Block Editors" }
                BlockEditor { controller: controller.clone(), block_type: BlockType::Amp }
                BlockEditor { controller: controller.clone(), block_type: BlockType::Drive }
                BlockEditor { controller: controller.clone(), block_type: BlockType::Cabinet }
                BlockEditor { controller, block_type: BlockType::Reverb }
            }
        },
        Tab::ModuleView => rsx! {
            ModuleViewTab { controller }
        },
        Tab::CollectionBrowser => rsx! {
            CollectionBrowserTab { controller }
        },
        Tab::SceneGrid => rsx! {
            SceneGridTab { controller }
        },
        Tab::Metadata => rsx! {
            div { class: "max-w-lg mx-auto p-6 space-y-6",
                h2 { class: "text-lg font-semibold text-zinc-300 mb-4", "MetadataDisplay Demo" }

                div { class: "rounded-lg border border-zinc-700 p-4 space-y-3",
                    h3 { class: "text-sm font-medium text-zinc-400", "With all fields" }
                    MetadataDisplay {
                        tags: vec![
                            "tone:clean".to_string(),
                            "guitar".to_string(),
                            "worship".to_string(),
                            "ambient".to_string(),
                        ],
                        description: Some("A warm, clean tone with shimmer reverb. Great for ambient pads and worship settings.".to_string()),
                        notes: Some("Try boosting the mids slightly for a fuller sound in the mix.".to_string()),
                    }
                }

                div { class: "rounded-lg border border-zinc-700 p-4 space-y-3",
                    h3 { class: "text-sm font-medium text-zinc-400", "Tags only" }
                    MetadataDisplay {
                        tags: vec!["high-gain".to_string(), "metal".to_string(), "lead".to_string()],
                    }
                }

                div { class: "rounded-lg border border-zinc-700 p-4 space-y-3",
                    h3 { class: "text-sm font-medium text-zinc-400", "Empty metadata" }
                    MetadataDisplay {}
                }
            }
        },
        Tab::Frame => rsx! {
            FramePreviewTab {}
        },
    }
}

// region: --- Module View Tab

/// Module view tab — loads a module collection and displays it.
#[component]
fn ModuleViewTab(controller: SignalController) -> Element {
    let mut module_data = use_signal(|| None::<signal2::Module>);
    let mut view_mode = use_signal(|| ModuleViewMode::Compact);

    {
        let controller = controller.clone();
        use_effect(move || {
            let controller = controller.clone();
            spawn(async move {
                let collections = controller.list_module_collections().await;
                if let Some(first) = collections.first() {
                    let snapshot = controller
                        .load_module_collection_default(first.id().to_string())
                        .await;
                    if let Some(snap) = snapshot {
                        module_data.set(Some(snap.module().clone()));
                    }
                }
            });
        });
    }

    rsx! {
        div { class: "max-w-xl mx-auto p-6 space-y-4",
            div { class: "flex items-center justify-between",
                h2 { class: "text-lg font-semibold text-zinc-300", "Module View" }
                div { class: "flex gap-2",
                    button {
                        class: if view_mode() == ModuleViewMode::Compact {
                            "px-3 py-1 text-xs rounded bg-zinc-700 text-zinc-100"
                        } else {
                            "px-3 py-1 text-xs rounded text-zinc-400 hover:bg-zinc-800"
                        },
                        onclick: move |_| view_mode.set(ModuleViewMode::Compact),
                        "Compact"
                    }
                    button {
                        class: if view_mode() == ModuleViewMode::Detail {
                            "px-3 py-1 text-xs rounded bg-zinc-700 text-zinc-100"
                        } else {
                            "px-3 py-1 text-xs rounded text-zinc-400 hover:bg-zinc-800"
                        },
                        onclick: move |_| view_mode.set(ModuleViewMode::Detail),
                        "Detail"
                    }
                }
            }

            if let Some(module) = module_data() {
                ModuleView {
                    module,
                    view_mode: view_mode(),
                    on_toggle_bypass: move |_block_id: String| {},
                    on_param_change: move |_change: signal2_ui::views::ParamChange| {},
                }
            } else {
                p { class: "text-sm text-zinc-500", "Loading module data..." }
            }
        }
    }
}

// endregion: --- Module View Tab

// region: --- Collection Browser Tab

/// Collection browser tab — self-contained multi-column cascading browser.
#[component]
fn CollectionBrowserTab(controller: SignalController) -> Element {
    rsx! {
        div { class: "h-full",
            CollectionBrowser { controller }
        }
    }
}

// endregion: --- Collection Browser Tab

// region: --- Scene Grid Tab

/// Scene grid tab — loads the first rig and displays its scenes.
#[component]
fn SceneGridTab(controller: SignalController) -> Element {
    let mut rig_id = use_signal(|| None::<String>);
    let mut active_scene = use_signal(|| None::<String>);

    {
        let controller = controller.clone();
        use_effect(move || {
            let controller = controller.clone();
            spawn(async move {
                let rigs = controller.list_rig_collections().await;
                if let Some(first) = rigs.first() {
                    rig_id.set(Some(first.id.to_string()));
                }
            });
        });
    }

    rsx! {
        div { class: "max-w-xl mx-auto p-6 space-y-4",
            h2 { class: "text-lg font-semibold text-zinc-300", "Rig Scene Grid" }

            if let Some(id) = rig_id() {
                div { class: "h-64 rounded-lg overflow-hidden border border-zinc-700",
                    RigSceneGrid {
                        controller,
                        rig_id: id,
                        active_scene_id: active_scene(),
                        on_scene_select: move |scene_id: String| {
                            active_scene.set(Some(scene_id));
                        },
                    }
                }
            } else {
                p { class: "text-sm text-zinc-500", "Loading rigs..." }
            }

            if let Some(scene) = active_scene() {
                p { class: "text-xs text-zinc-400", "Active scene: {scene}" }
            }
        }
    }
}

// endregion: --- Scene Grid Tab

#[component]
fn FramePreviewTab() -> Element {
    let mut selected_frame = use_signal(|| 0usize);
    let mut selected_layer = use_signal(|| None::<NodeId>);
    let mut hovered_layer = use_signal(|| None::<NodeId>);
    let mut node_overrides = use_signal(std::collections::HashMap::<NodeId, NodeLayoutOverride>::new);
    let mut edit_drag = use_signal(|| None::<EditDragState>);
    let mut cached_primitives = use_signal(Vec::<frame_ui::PaintPrimitive>::new);
    let mut cached_content_bounds = use_signal(PrimitiveBounds::default);
    let mut cached_node_bounds =
        use_signal(std::collections::HashMap::<NodeId, PrimitiveBounds>::new);
    let mut cached_render_root = use_signal(|| None::<NodeId>);
    let mut view_zoom = use_signal(|| 1.0f64);
    let mut pan_x = use_signal(|| 0.0f64);
    let mut pan_y = use_signal(|| 0.0f64);
    let mut is_panning = use_signal(|| false);
    let mut pan_start_mouse = use_signal(|| (0.0f64, 0.0f64));
    let mut pan_start_offset = use_signal(|| (0.0f64, 0.0f64));
    let mut frame_doc = use_signal(|| None::<FrameDocument>);
    let mut frame_error = use_signal(|| {
        Some(format!(
            "Waiting for bridge export at {}",
            FRAME_BRIDGE_JSON_PATH
        ))
    });
    let mut frame_status =
        use_signal(|| format!("Source: {FRAME_BRIDGE_JSON_PATH} (auto reload every 350ms)"));
    #[cfg(feature = "desktop")]
    let graphics = consume_context::<Arc<Mutex<FrameGraphics>>>();
    #[cfg(feature = "desktop")]
    let mut preview_bounds = use_signal(|| FramePreviewBounds::default());

    #[cfg(feature = "desktop")]
    {
        use_effect(move || {
            if let Err(err) = ensure_frame_bridge_running() {
                frame_status.set(format!("Bridge autostart failed: {err}"));
            } else {
                frame_status.set(format!(
                    "Source: {FRAME_BRIDGE_JSON_PATH} (bridge auto-start enabled, reload every 350ms)"
                ));
            }
        });

        use_future(move || async move {
            let mut last_signature: Option<(u64, u128)> = None;
            let mut last_error: Option<String> = None;
            loop {
                match tokio::fs::metadata(FRAME_BRIDGE_JSON_PATH).await {
                    Ok(meta) => {
                        let modified = meta
                            .modified()
                            .ok()
                            .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
                            .map(|d| d.as_millis())
                            .unwrap_or_default();
                        let signature = (meta.len(), modified);
                        if last_signature != Some(signature) {
                            last_signature = Some(signature);
                            match tokio::fs::read(FRAME_BRIDGE_JSON_PATH).await {
                                Ok(bytes) => match import_figma_bytes(&bytes) {
                                    Ok(doc) => {
                                        frame_doc.set(Some(doc));
                                        frame_error.set(None);
                                        last_error = None;
                                        frame_status.set(format!(
                                            "Source: {FRAME_BRIDGE_JSON_PATH} ({} bytes, updated {})",
                                            signature.0, modified
                                        ));
                                    }
                                    Err(err) => {
                                        let msg = format!(
                                            "Import failed for {}: {}",
                                            FRAME_BRIDGE_JSON_PATH, err
                                        );
                                        if last_error.as_deref() != Some(msg.as_str()) {
                                            frame_error.set(Some(msg.clone()));
                                            last_error = Some(msg);
                                        }
                                    }
                                },
                                Err(err) => {
                                    let msg = format!(
                                        "Failed reading {}: {}",
                                        FRAME_BRIDGE_JSON_PATH, err
                                    );
                                    if last_error.as_deref() != Some(msg.as_str()) {
                                        frame_error.set(Some(msg.clone()));
                                        last_error = Some(msg);
                                    }
                                }
                            }
                        }
                    }
                    Err(err) => {
                        let msg = format!(
                            "Bridge file not found at {}: {}",
                            FRAME_BRIDGE_JSON_PATH, err
                        );
                        if last_error.as_deref() != Some(msg.as_str()) {
                            frame_error.set(Some(msg.clone()));
                            last_error = Some(msg);
                        }
                    }
                }
                tokio::time::sleep(tokio::time::Duration::from_millis(350)).await;
            }
        });

        use_effect(move || {
            dioxus::desktop::window().window.request_redraw();
        });

        use_future(move || async move {
            loop {
                tokio::time::sleep(tokio::time::Duration::from_millis(200)).await;
                let result = document::eval(
                    r#"
                        const el = document.getElementById('frame-preview-surface');
                        if (!el) return "null";
                        const rect = el.getBoundingClientRect();
                        const dpr = window.devicePixelRatio || 1;
                        return JSON.stringify({
                            x: rect.x * dpr,
                            y: rect.y * dpr,
                            width: rect.width * dpr,
                            height: rect.height * dpr,
                            dpr
                        });
                    "#,
                );
                let Ok(value) = result.await else { continue };
                let json_str = value
                    .as_str()
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| value.to_string());
                if json_str == "null" || json_str == "\"null\"" {
                    continue;
                }
                let Ok(parsed) = serde_json::from_str::<serde_json::Value>(&json_str) else {
                    continue;
                };
                let next = FramePreviewBounds {
                    x: parsed["x"].as_f64().unwrap_or(0.0),
                    y: parsed["y"].as_f64().unwrap_or(0.0),
                    width: parsed["width"].as_f64().unwrap_or(0.0),
                    height: parsed["height"].as_f64().unwrap_or(0.0),
                    dpr: parsed["dpr"].as_f64().unwrap_or(1.0),
                };
                if next.width > 0.0 && next.height > 0.0 {
                    preview_bounds.set(next);
                }
            }
        });
    }

    #[cfg(feature = "desktop")]
    {
        use_effect(move || {
            let Some(doc) = frame_doc() else {
                cached_primitives.set(Vec::new());
                cached_content_bounds.set(PrimitiveBounds::default());
                cached_node_bounds.set(std::collections::HashMap::new());
                return;
            };
            let options = collect_frame_options(&doc);
            let selected_idx = selected_frame().min(options.len().saturating_sub(1));
            let root = options
                .get(selected_idx)
                .map(|o| o.id)
                .unwrap_or_else(|| preview_scene_root(&doc));
            let primitives = build_editor_primitives(&doc, root, &node_overrides());
            let content = primitive_bounds(&primitives);
            let node_bounds = node_bounds_map_from_primitives(&primitives);
            cached_primitives.set(primitives);
            cached_content_bounds.set(content);
            cached_node_bounds.set(node_bounds);
            cached_render_root.set(Some(root));
        });

        let graphics_loop = graphics.clone();
        use_future(move || {
            let graphics_loop = graphics_loop.clone();
            async move {
            loop {
                tokio::time::sleep(tokio::time::Duration::from_millis(8)).await;

                let bounds = preview_bounds();
                if !bounds.is_valid() {
                    continue;
                }

                let primitives = cached_primitives();
                if primitives.is_empty() {
                    continue;
                }

                let zoom = view_zoom();
                let current_pan_x = pan_x();
                let current_pan_y = pan_y();
                let content = cached_content_bounds();
                let node_bounds = cached_node_bounds();
                let active_node = selected_layer().or(cached_render_root());
                let hovered_node = hovered_layer();
                let selected_overlay_css = active_node
                    .and_then(|id| node_bounds.get(&id).copied())
                    .map(|world| {
                        selection_overlay_rect(
                            world,
                            content,
                            bounds,
                            zoom,
                            current_pan_x,
                            current_pan_y,
                        )
                    });
                let hovered_overlay_css = hovered_node
                    .and_then(|id| node_bounds.get(&id).copied())
                    .filter(|_| hovered_node != active_node)
                    .map(|world| {
                        selection_overlay_rect(
                            world,
                            content,
                            bounds,
                            zoom,
                            current_pan_x,
                            current_pan_y,
                        )
                    });
                let pad = 16.0 * bounds.dpr.max(1.0);
                let transform = Affine::translate((
                    bounds.x + pad + current_pan_x,
                    bounds.y + pad + current_pan_y,
                )) * Affine::scale(zoom)
                    * Affine::translate((-content.min_x, -content.min_y));
                let text_font = try_load_system_text_font().map(|bytes| TextFontRef {
                    bytes: bytes.as_slice(),
                    index: 0,
                });

                if let Ok(mut gfx) = graphics_loop.lock() {
                    let win_size = dioxus::desktop::window().window.inner_size();
                    let (sw, sh) = gfx.size();
                    if sw != win_size.width || sh != win_size.height {
                        gfx.resize(win_size.width, win_size.height);
                    }

                    gfx.render(|scene| {
                        let panel = Rect::new(
                            bounds.x,
                            bounds.y,
                            bounds.x + bounds.width,
                            bounds.y + bounds.height,
                        );
                        scene.fill(
                            Fill::NonZero,
                            Affine::IDENTITY,
                            Color::from_rgb8(5, 6, 12),
                            None,
                            &panel,
                        );
                        if let Some(font) = text_font {
                            paint_primitives_into_scene_with(
                                scene,
                                &primitives,
                                transform,
                                Some(font),
                            );
                        } else {
                            paint_primitives_into_scene_with(scene, &primitives, transform, None);
                        }

                        if let Some(hover) = hovered_overlay_css {
                            let rect = Rect::new(
                                bounds.x + hover.x * bounds.dpr,
                                bounds.y + hover.y * bounds.dpr,
                                bounds.x + (hover.x + hover.width) * bounds.dpr,
                                bounds.y + (hover.y + hover.height) * bounds.dpr,
                            );
                            scene.stroke(
                                &Stroke::new(1.0 * bounds.dpr.max(1.0)),
                                Affine::IDENTITY,
                                Color::from_rgba8(220, 220, 225, 170),
                                None,
                                &rect,
                            );
                        }

                        if let Some(sel) = selected_overlay_css {
                            let rect = Rect::new(
                                bounds.x + sel.x * bounds.dpr,
                                bounds.y + sel.y * bounds.dpr,
                                bounds.x + (sel.x + sel.width) * bounds.dpr,
                                bounds.y + (sel.y + sel.height) * bounds.dpr,
                            );
                            scene.stroke(
                                &Stroke::new(1.5 * bounds.dpr.max(1.0)),
                                Affine::IDENTITY,
                                Color::from_rgba8(59, 130, 246, 230),
                                None,
                                &rect,
                            );
                        }
                    });
                }
                dioxus::desktop::window().window.request_redraw();
            }
        }});
    }

    let frame_content = if let Some(doc) = frame_doc() {
        let options = collect_frame_options(&doc);
        let selected_idx = selected_frame().min(options.len().saturating_sub(1));
        let render_root = options
            .get(selected_idx)
            .map(|o| o.id)
            .unwrap_or_else(|| preview_scene_root(&doc));
        let layer_rows = collect_layer_rows(&doc);
        let edited_primitives = cached_primitives();
        let content_bounds = cached_content_bounds();
        let active_node = selected_layer().unwrap_or(render_root);
        let selected_world_bounds = cached_node_bounds().get(&active_node).copied();
        let hovered_layer_id = hovered_layer();
        let overlay_rect = selected_world_bounds.map(|world| {
            selection_overlay_rect(
                world,
                content_bounds,
                preview_bounds(),
                view_zoom(),
                pan_x(),
                pan_y(),
            )
        });
        let active_projection = doc.project_node(active_node);
        let selected_layer_id = selected_layer();
        let options_for_click = options.clone();
        let options_for_dblclick = options.clone();
        let click_primitives = edited_primitives.clone();
        let dblclick_primitives = edited_primitives.clone();
        let hover_primitives = edited_primitives.clone();
        let doc_for_hover = doc.clone();
        let doc_for_click = doc.clone();
        let doc_for_dblclick = doc.clone();
        rsx! {
            div { class: "flex gap-3 h-full min-h-0 overflow-hidden",
                div { class: "w-[280px] shrink-0 rounded border border-zinc-800 bg-zinc-900/70 flex flex-col min-h-0",
                    div { class: "px-3 py-2 border-b border-zinc-800 flex items-center justify-between",
                        h3 { class: "text-sm font-semibold text-zinc-200", "Layers" }
                        span { class: "text-[10px] uppercase tracking-wide text-zinc-500", "{layer_rows.len()} nodes" }
                    }
                    div { class: "overflow-auto p-2 space-y-0.5",
                        for row in layer_rows {
                            {
                                let node_id = row.id;
                                let frame_idx = frame_option_index_by_id(&options, node_id);
                                let is_selected = selected_layer_id
                                    .map(|id| id == node_id)
                                    .unwrap_or(node_id == render_root);
                                let is_hovered = hovered_layer_id.map(|id| id == node_id).unwrap_or(false);
                                rsx! {
                                    button {
                                        key: "{node_id}",
                                        class: if is_selected {
                                            "w-full text-left rounded px-2 py-1 text-xs bg-blue-500/20 text-blue-200 border border-blue-500/40"
                                        } else if is_hovered {
                                            "w-full text-left rounded px-2 py-1 text-xs text-zinc-100 bg-zinc-700/50 border border-zinc-500/40"
                                        } else {
                                            "w-full text-left rounded px-2 py-1 text-xs text-zinc-300 hover:bg-zinc-800/70 border border-transparent"
                                        },
                                        style: "padding-left: {8 + row.depth * 16}px;",
                                        onclick: move |_| {
                                            selected_layer.set(Some(node_id));
                                            if let Some(idx) = frame_idx {
                                                selected_frame.set(idx);
                                            }
                                        },
                                        span { class: "inline-block w-7 text-zinc-500 font-mono", "{row.icon}" }
                                        span { class: "text-zinc-100", "{row.name}" }
                                        span { class: "text-zinc-500 ml-1", "{row.figma_type}" }
                                    }
                                }
                            }
                        }
                    }
                }

                div { class: "flex-1 min-w-0 min-h-0 flex flex-col",
                    if !options.is_empty() {
                        div { class: "mb-3 flex items-center gap-2 flex-wrap shrink-0",
                            label { class: "text-xs text-zinc-400", "Frame:" }
                            select {
                                class: "px-2 py-1 text-xs rounded bg-zinc-800 text-zinc-200 border border-zinc-700",
                                value: "{selected_idx}",
                                onchange: move |evt: Event<FormData>| {
                                    if let Ok(next) = evt.value().parse::<usize>() {
                                        selected_frame.set(next);
                                        if let Some(option) = options.get(next) {
                                            selected_layer.set(Some(option.id));
                                        }
                                    }
                                },
                                for (idx, option) in options.iter().enumerate() {
                                    option {
                                        value: "{idx}",
                                        selected: idx == selected_idx,
                                        "{option.label}"
                                    }
                                }
                            }
                            button {
                                class: "px-2 py-1 text-xs rounded bg-zinc-800 text-zinc-200 border border-zinc-700",
                                onclick: move |_| {
                                    let bounds = preview_bounds();
                                    if bounds.is_valid() {
                                        let pad = 16.0 * bounds.dpr.max(1.0);
                                        let content_w = (content_bounds.max_x - content_bounds.min_x).max(1.0);
                                        let content_h = (content_bounds.max_y - content_bounds.min_y).max(1.0);
                                        let fit_x = ((bounds.width - pad * 2.0) / content_w).max(0.05);
                                        let fit_y = ((bounds.height - pad * 2.0) / content_h).max(0.05);
                                        view_zoom.set(fit_x.min(fit_y).clamp(0.05, 8.0));
                                        pan_x.set(0.0);
                                        pan_y.set(0.0);
                                    }
                                },
                                "Fit"
                            }
                            button {
                                class: "px-2 py-1 text-xs rounded bg-zinc-800 text-zinc-200 border border-zinc-700",
                                onclick: move |_| {
                                    view_zoom.set(1.0);
                                    pan_x.set(0.0);
                                    pan_y.set(0.0);
                                },
                                "Reset"
                            }
                            span {
                                class: "text-[11px] text-zinc-500",
                                "zoom "
                                {format!("{:.2}x", view_zoom())}
                            }
                        }
                    }
                    div {
                        id: "frame-preview-surface",
                        class: "w-full flex-1 min-h-0 rounded border border-zinc-800 relative overflow-hidden",
                        style: "background: transparent;",
                        onmousedown: move |evt| {
                            evt.prevent_default();
                            is_panning.set(true);
                            pan_start_mouse.set((evt.client_coordinates().x, evt.client_coordinates().y));
                            pan_start_offset.set((pan_x(), pan_y()));
                        },
                        onmousemove: move |evt| {
                            if let Some(drag) = edit_drag() {
                                let mx = evt.client_coordinates().x;
                                let my = evt.client_coordinates().y;
                                let dx = (mx - drag.start_mouse.0) * preview_bounds().dpr;
                                let dy = (my - drag.start_mouse.1) * preview_bounds().dpr;
                                let mut next = drag.start_rect;
                                match drag.mode {
                                    EditDragMode::Move => {
                                        next.x = drag.start_rect.x + dx / view_zoom().max(0.0001);
                                        next.y = drag.start_rect.y + dy / view_zoom().max(0.0001);
                                    }
                                    EditDragMode::Resize(handle) => {
                                        let zx = view_zoom().max(0.0001);
                                        apply_resize_handle(&mut next, handle, dx / zx, dy / zx);
                                    }
                                }
                                node_overrides.with_mut(|map| {
                                    map.insert(drag.node_id, NodeLayoutOverride::from_rect(next));
                                });
                                return;
                            }
                            if is_panning() {
                                let (sx, sy) = pan_start_mouse();
                                let (spx, spy) = pan_start_offset();
                                let dx = evt.client_coordinates().x - sx;
                                let dy = evt.client_coordinates().y - sy;
                                pan_x.set(spx + dx * preview_bounds().dpr);
                                pan_y.set(spy + dy * preview_bounds().dpr);
                            } else {
                                let world = client_to_world(
                                    evt.client_coordinates().x,
                                    evt.client_coordinates().y,
                                    preview_bounds(),
                                    content_bounds,
                                    view_zoom(),
                                    pan_x(),
                                    pan_y(),
                                );
                                let hit_stack =
                                    hit_test_node_stack(&doc_for_hover, &hover_primitives, world.0, world.1);
                                hovered_layer.set(hit_stack.first().copied());
                            }
                        },
                        onmouseup: move |_| {
                            is_panning.set(false);
                            edit_drag.set(None);
                        },
                        onmouseleave: move |_| {
                            is_panning.set(false);
                            edit_drag.set(None);
                            hovered_layer.set(None);
                        },
                        onwheel: move |evt| {
                            evt.prevent_default();
                            let delta = evt.delta();
                            let (raw_dx, raw_dy) = match delta {
                                WheelDelta::Pixels(p) => (p.x, p.y),
                                WheelDelta::Lines(l) => (l.x * 16.0, l.y * 16.0),
                                WheelDelta::Pages(p) => (p.x * 160.0, p.y * 160.0),
                            };
                            let modifiers = evt.modifiers();
                            let is_ctrl = modifiers.contains(keyboard_types::Modifiers::CONTROL)
                                || modifiers.contains(keyboard_types::Modifiers::META);
                            if is_ctrl {
                                let old_zoom = view_zoom();
                                let factor = if raw_dy < 0.0 { 1.08 } else { 1.0 / 1.08 };
                                view_zoom.set((old_zoom * factor).clamp(0.05, 16.0));
                            } else {
                                pan_x.set(pan_x() - raw_dx * preview_bounds().dpr);
                                pan_y.set(pan_y() - raw_dy * preview_bounds().dpr);
                            }
                        },
                        onclick: move |evt| {
                            let world = client_to_world(
                                evt.client_coordinates().x,
                                evt.client_coordinates().y,
                                preview_bounds(),
                                content_bounds,
                                view_zoom(),
                                pan_x(),
                                pan_y(),
                            );
                            let hit_stack =
                                hit_test_node_stack(&doc_for_click, &click_primitives, world.0, world.1);
                            if let Some(hit) = hit_stack.first().copied() {
                                selected_layer.set(Some(hit));
                                if let Some(idx) = frame_option_index_by_id(&options_for_click, hit) {
                                    selected_frame.set(idx);
                                }
                            }
                        },
                        ondoubleclick: move |evt| {
                            let world = client_to_world(
                                evt.client_coordinates().x,
                                evt.client_coordinates().y,
                                preview_bounds(),
                                content_bounds,
                                view_zoom(),
                                pan_x(),
                                pan_y(),
                            );
                            let hit_stack =
                                hit_test_node_stack(&doc_for_dblclick, &dblclick_primitives, world.0, world.1);
                            if hit_stack.is_empty() {
                                return;
                            }
                            let next = if let Some(sel) = selected_layer() {
                                if let Some(idx) = hit_stack.iter().position(|id| *id == sel) {
                                    hit_stack.get(idx + 1).copied().unwrap_or(sel)
                                } else {
                                    hit_stack[0]
                                }
                            } else {
                                hit_stack[0]
                            };
                            selected_layer.set(Some(next));
                            if let Some(idx) = frame_option_index_by_id(&options_for_dblclick, next) {
                                selected_frame.set(idx);
                            }
                        },

                        if let Some(overlay) = overlay_rect {
                            div {
                                class: "absolute pointer-events-auto",
                                style: "left:{overlay.x}px; top:{overlay.y}px; width:{overlay.width}px; height:{overlay.height}px;",
                                onmousedown: move |evt| {
                                    evt.stop_propagation();
                                    evt.prevent_default();
                                    if let Some(world) = selected_world_bounds {
                                        let drag = EditDragState {
                                            node_id: active_node,
                                            start_mouse: (evt.client_coordinates().x, evt.client_coordinates().y),
                                            start_rect: world.into(),
                                            mode: EditDragMode::Move,
                                        };
                                        edit_drag.set(Some(drag));
                                    }
                                },
                            }
                            for handle in ResizeHandle::ALL {
                                {
                                    let hs = overlay_handle_style(overlay, handle);
                                    rsx! {
                                        div {
                                            key: "{handle:?}",
                                            class: "absolute bg-blue-400 border border-blue-200 rounded-[2px] pointer-events-auto",
                                            style: "{hs}",
                                            onmousedown: move |evt| {
                                                evt.stop_propagation();
                                                evt.prevent_default();
                                                if let Some(world) = selected_world_bounds {
                                                    let drag = EditDragState {
                                                        node_id: active_node,
                                                        start_mouse: (evt.client_coordinates().x, evt.client_coordinates().y),
                                                        start_rect: world.into(),
                                                        mode: EditDragMode::Resize(handle),
                                                    };
                                                    edit_drag.set(Some(drag));
                                                }
                                            },
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                div { class: "w-[280px] shrink-0 rounded border border-zinc-800 bg-zinc-900/70 flex flex-col min-h-0",
                    div { class: "px-3 py-2 border-b border-zinc-800",
                        h3 { class: "text-sm font-semibold text-zinc-200", "Inspector" }
                    }
                    div { class: "p-3 overflow-auto text-xs text-zinc-300 space-y-3",
                        if let Some(node) = doc.get_node(active_node) {
                            div { class: "space-y-1",
                                p { class: "text-zinc-100 font-medium", "{node.name}" }
                                p { class: "text-zinc-500", "{node.figma_type}" }
                                p { class: "text-zinc-500 font-mono break-all", "{node.figma_id}" }
                            }
                        }
                        if let Some(proj) = active_projection {
                            div { class: "grid grid-cols-2 gap-2",
                                NumericInspectorField {
                                    label: "X",
                                    value: selected_world_bounds.map(|b| b.min_x).unwrap_or(0.0),
                                    on_change: move |next: f64| {
                                        if let Some(base) = selected_world_bounds {
                                            node_overrides.with_mut(|map| {
                                                let mut r: EditableRect = base.into();
                                                r.x = next;
                                                map.insert(active_node, NodeLayoutOverride::from_rect(r));
                                            });
                                        }
                                    }
                                }
                                NumericInspectorField {
                                    label: "Y",
                                    value: selected_world_bounds.map(|b| b.min_y).unwrap_or(0.0),
                                    on_change: move |next: f64| {
                                        if let Some(base) = selected_world_bounds {
                                            node_overrides.with_mut(|map| {
                                                let mut r: EditableRect = base.into();
                                                r.y = next;
                                                map.insert(active_node, NodeLayoutOverride::from_rect(r));
                                            });
                                        }
                                    }
                                }
                                NumericInspectorField {
                                    label: "W",
                                    value: selected_world_bounds.map(|b| b.max_x - b.min_x).unwrap_or(0.0),
                                    on_change: move |next: f64| {
                                        if let Some(base) = selected_world_bounds {
                                            node_overrides.with_mut(|map| {
                                                let mut r: EditableRect = base.into();
                                                r.width = next.max(1.0);
                                                map.insert(active_node, NodeLayoutOverride::from_rect(r));
                                            });
                                        }
                                    }
                                }
                                NumericInspectorField {
                                    label: "H",
                                    value: selected_world_bounds.map(|b| b.max_y - b.min_y).unwrap_or(0.0),
                                    on_change: move |next: f64| {
                                        if let Some(base) = selected_world_bounds {
                                            node_overrides.with_mut(|map| {
                                                let mut r: EditableRect = base.into();
                                                r.height = next.max(1.0);
                                                map.insert(active_node, NodeLayoutOverride::from_rect(r));
                                            });
                                        }
                                    }
                                }
                            }
                            p { class: "text-zinc-500", "Class: " {format!("{:?}", proj.class)} }
                        } else {
                            p { class: "text-zinc-500", "Select a node to inspect." }
                        }
                    }
                }
            }
        }
    } else if let Some(err) = frame_error() {
        rsx! {
            div { class: "rounded border border-red-800 bg-red-950/50 text-red-200 p-3 text-sm",
                "Import failed: {err}"
            }
        }
    } else {
        rsx! {
            div { class: "rounded border border-zinc-800 bg-zinc-950/50 text-zinc-300 p-3 text-sm",
                "Waiting for bridge payload at {FRAME_BRIDGE_JSON_PATH}..."
            }
        }
    };

    rsx! {
        div { class: "h-full p-4 overflow-hidden flex flex-col min-h-0",
            h2 { class: "text-lg font-semibold text-zinc-300 mb-3 shrink-0", "Frame .fig Preview" }
            p { class: "text-xs text-zinc-500 mb-2 shrink-0", "{frame_status()}" }
            div { class: "flex-1 min-h-0",
                {frame_content}
            }
        }
    }
}

#[cfg(feature = "desktop")]
#[derive(Debug, Clone, Copy, Default)]
struct FramePreviewBounds {
    x: f64,
    y: f64,
    width: f64,
    height: f64,
    dpr: f64,
}

#[cfg(feature = "desktop")]
impl FramePreviewBounds {
    fn is_valid(&self) -> bool {
        self.width > 0.0 && self.height > 0.0
    }
}

#[cfg(feature = "desktop")]
#[derive(Debug, Clone, Copy, Default)]
struct PrimitiveBounds {
    min_x: f64,
    min_y: f64,
    max_x: f64,
    max_y: f64,
}

impl PrimitiveBounds {
    fn contains(&self, x: f64, y: f64) -> bool {
        x >= self.min_x && x <= self.max_x && y >= self.min_y && y <= self.max_y
    }
}

#[cfg(feature = "desktop")]
fn primitive_bounds(primitives: &[frame_ui::PaintPrimitive]) -> PrimitiveBounds {
    let mut out = PrimitiveBounds {
        min_x: f64::INFINITY,
        min_y: f64::INFINITY,
        max_x: f64::NEG_INFINITY,
        max_y: f64::NEG_INFINITY,
    };

    for p in primitives {
        match p {
            frame_ui::PaintPrimitive::Rect {
                x,
                y,
                width,
                height,
                ..
            }
            | frame_ui::PaintPrimitive::Path {
                x,
                y,
                width,
                height,
                ..
            } => {
                out.min_x = out.min_x.min(*x);
                out.min_y = out.min_y.min(*y);
                out.max_x = out.max_x.max(*x + *width);
                out.max_y = out.max_y.max(*y + *height);
            }
            frame_ui::PaintPrimitive::Text {
                x,
                y,
                text,
                font_size,
                ..
            } => {
                let width = (*font_size * 0.55 * text.chars().count() as f64).max(*font_size);
                out.min_x = out.min_x.min(*x);
                out.min_y = out.min_y.min(*y);
                out.max_x = out.max_x.max(*x + width);
                out.max_y = out.max_y.max(*y + *font_size * 1.2);
            }
        }
    }

    if !out.min_x.is_finite()
        || !out.min_y.is_finite()
        || !out.max_x.is_finite()
        || !out.max_y.is_finite()
    {
        return PrimitiveBounds {
            min_x: 0.0,
            min_y: 0.0,
            max_x: 1024.0,
            max_y: 768.0,
        };
    }

    out
}

fn primitive_world_bounds(primitive: &frame_ui::PaintPrimitive) -> PrimitiveBounds {
    match primitive {
        frame_ui::PaintPrimitive::Rect {
            x,
            y,
            width,
            height,
            ..
        }
        | frame_ui::PaintPrimitive::Path {
            x,
            y,
            width,
            height,
            ..
        } => PrimitiveBounds {
            min_x: *x,
            min_y: *y,
            max_x: *x + *width,
            max_y: *y + *height,
        },
        frame_ui::PaintPrimitive::Text {
            x,
            y,
            text,
            font_size,
            ..
        } => {
            let width = (*font_size * 0.55 * text.chars().count() as f64).max(*font_size);
            PrimitiveBounds {
                min_x: *x,
                min_y: *y,
                max_x: *x + width,
                max_y: *y + *font_size * 1.2,
            }
        }
    }
}

fn client_to_world(
    client_x_css: f64,
    client_y_css: f64,
    preview: FramePreviewBounds,
    content: PrimitiveBounds,
    zoom: f64,
    pan_x: f64,
    pan_y: f64,
) -> (f64, f64) {
    let dpr = preview.dpr.max(1.0);
    let pad = 16.0 * dpr;
    let local_x = client_x_css * dpr - preview.x;
    let local_y = client_y_css * dpr - preview.y;
    let world_x = content.min_x + (local_x - pad - pan_x) / zoom.max(0.0001);
    let world_y = content.min_y + (local_y - pad - pan_y) / zoom.max(0.0001);
    (world_x, world_y)
}

fn hit_test_node_stack(
    doc: &FrameDocument,
    primitives: &[frame_ui::PaintPrimitive],
    world_x: f64,
    world_y: f64,
) -> Vec<NodeId> {
    let mut seen = std::collections::HashSet::<NodeId>::new();
    let mut candidates: Vec<(NodeId, usize, u8)> = Vec::new();
    for (z_index, primitive) in primitives.iter().rev().enumerate() {
        let Some(hit_kind) = primitive_hit_kind(primitive, world_x, world_y) else {
            continue;
        };
        let node_id = primitive_node_id(primitive);
        let Some(node) = doc.get_node(node_id) else {
            continue;
        };
        // Pointer selection ignores hidden/locked nodes like Figma.
        if !node.visible || node.locked {
            continue;
        }
        if seen.insert(node_id) {
            let priority = node_hit_priority(node.figma_type.as_str(), hit_kind);
            candidates.push((node_id, z_index, priority));
        }
    }
    candidates.sort_by(|a, b| b.2.cmp(&a.2).then_with(|| a.1.cmp(&b.1)));
    candidates.into_iter().map(|c| c.0).collect()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum HitKind {
    Shape,
    TextGlyph,
}

fn primitive_hit_kind(
    primitive: &frame_ui::PaintPrimitive,
    world_x: f64,
    world_y: f64,
) -> Option<HitKind> {
    match primitive {
        frame_ui::PaintPrimitive::Text {
            x,
            y,
            text,
            font_size,
            ..
        } => {
            if text_glyph_hit(*x, *y, text, *font_size, world_x, world_y) {
                Some(HitKind::TextGlyph)
            } else {
                None
            }
        }
        _ => {
            let bounds = primitive_world_bounds(primitive);
            if bounds.contains(world_x, world_y) {
                Some(HitKind::Shape)
            } else {
                None
            }
        }
    }
}

fn text_glyph_hit(
    x: f64,
    y: f64,
    text: &str,
    font_size: f64,
    world_x: f64,
    world_y: f64,
) -> bool {
    let line_h = font_size * 1.2;
    let glyph_w = (font_size * 0.55).max(1.0);
    let glyph_h = (font_size * 0.9).max(1.0);
    let baseline_top = y + font_size * 0.1;

    for (idx, ch) in text.chars().enumerate() {
        if ch.is_whitespace() {
            continue;
        }
        let gx = x + glyph_w * idx as f64;
        let gy = baseline_top;
        let in_x = world_x >= gx && world_x <= gx + glyph_w;
        let in_y = world_y >= gy && world_y <= gy + glyph_h.min(line_h);
        if in_x && in_y {
            return true;
        }
    }
    false
}

fn node_hit_priority(figma_type: &str, hit_kind: HitKind) -> u8 {
    match (figma_type, hit_kind) {
        ("INSTANCE", _) => 120,
        ("COMPONENT", _) | ("COMPONENT_SET", _) => 110,
        ("TEXT", HitKind::TextGlyph) => 100,
        ("FRAME", _) | ("GROUP", _) | ("SECTION", _) => 80,
        _ => 60,
    }
}

#[derive(Debug, Clone, Copy, Default)]
struct OverlayRectCss {
    x: f64,
    y: f64,
    width: f64,
    height: f64,
}

#[derive(Debug, Clone, Copy, Default)]
struct EditableRect {
    x: f64,
    y: f64,
    width: f64,
    height: f64,
}

#[derive(Debug, Clone, Copy, Default)]
struct NodeLayoutOverride {
    x: f64,
    y: f64,
    width: f64,
    height: f64,
}

impl NodeLayoutOverride {
    fn from_rect(r: EditableRect) -> Self {
        Self {
            x: r.x,
            y: r.y,
            width: r.width.max(1.0),
            height: r.height.max(1.0),
        }
    }
}

impl From<PrimitiveBounds> for EditableRect {
    fn from(value: PrimitiveBounds) -> Self {
        Self {
            x: value.min_x,
            y: value.min_y,
            width: (value.max_x - value.min_x).max(1.0),
            height: (value.max_y - value.min_y).max(1.0),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ResizeHandle {
    Nw,
    Ne,
    Sw,
    Se,
}

impl ResizeHandle {
    const ALL: [Self; 4] = [Self::Nw, Self::Ne, Self::Sw, Self::Se];
}

#[derive(Debug, Clone, Copy)]
enum EditDragMode {
    Move,
    Resize(ResizeHandle),
}

#[derive(Debug, Clone, Copy)]
struct EditDragState {
    node_id: NodeId,
    start_mouse: (f64, f64),
    start_rect: EditableRect,
    mode: EditDragMode,
}

fn build_editor_primitives(
    doc: &FrameDocument,
    root: NodeId,
    overrides: &std::collections::HashMap<NodeId, NodeLayoutOverride>,
) -> Vec<frame_ui::PaintPrimitive> {
    let mut primitives = build_paint_primitives(doc, root);
    if overrides.is_empty() {
        return primitives;
    }

    let base_bounds = node_bounds_map_from_primitives(&primitives);
    for (node_id, ov) in overrides {
        let Some(base) = base_bounds.get(node_id).copied() else {
            continue;
        };
        let base_w = (base.max_x - base.min_x).max(1.0);
        let base_h = (base.max_y - base.min_y).max(1.0);
        let sx = (ov.width / base_w).max(0.01);
        let sy = (ov.height / base_h).max(0.01);
        let subtree = doc.walk_subtree(*node_id);

        for primitive in primitives.iter_mut() {
            let pid = primitive_node_id(primitive);
            if !subtree.iter().any(|id| *id == pid) {
                continue;
            }
            transform_primitive(primitive, base.min_x, base.min_y, ov.x, ov.y, sx, sy);
        }
    }

    primitives
}

fn primitive_node_id(primitive: &frame_ui::PaintPrimitive) -> NodeId {
    match primitive {
        frame_ui::PaintPrimitive::Rect { node_id, .. }
        | frame_ui::PaintPrimitive::Path { node_id, .. }
        | frame_ui::PaintPrimitive::Text { node_id, .. } => *node_id,
    }
}

fn transform_primitive(
    primitive: &mut frame_ui::PaintPrimitive,
    from_x: f64,
    from_y: f64,
    to_x: f64,
    to_y: f64,
    sx: f64,
    sy: f64,
) {
    match primitive {
        frame_ui::PaintPrimitive::Rect {
            x,
            y,
            width,
            height,
            ..
        }
        | frame_ui::PaintPrimitive::Path {
            x,
            y,
            width,
            height,
            ..
        } => {
            *x = to_x + (*x - from_x) * sx;
            *y = to_y + (*y - from_y) * sy;
            *width = (*width * sx).max(1.0);
            *height = (*height * sy).max(1.0);
        }
        frame_ui::PaintPrimitive::Text {
            x, y, font_size, ..
        } => {
            *x = to_x + (*x - from_x) * sx;
            *y = to_y + (*y - from_y) * sy;
            *font_size = (*font_size * ((sx + sy) * 0.5)).max(1.0);
        }
    }
}

fn node_bounds_map_from_primitives(
    primitives: &[frame_ui::PaintPrimitive],
) -> std::collections::HashMap<NodeId, PrimitiveBounds> {
    let mut map: std::collections::HashMap<NodeId, PrimitiveBounds> = std::collections::HashMap::new();
    for primitive in primitives {
        let node_id = primitive_node_id(primitive);
        let entry = map.entry(node_id).or_insert(PrimitiveBounds {
            min_x: f64::INFINITY,
            min_y: f64::INFINITY,
            max_x: f64::NEG_INFINITY,
            max_y: f64::NEG_INFINITY,
        });

        match primitive {
            frame_ui::PaintPrimitive::Rect {
                x,
                y,
                width,
                height,
                ..
            }
            | frame_ui::PaintPrimitive::Path {
                x,
                y,
                width,
                height,
                ..
            } => {
                entry.min_x = entry.min_x.min(*x);
                entry.min_y = entry.min_y.min(*y);
                entry.max_x = entry.max_x.max(*x + *width);
                entry.max_y = entry.max_y.max(*y + *height);
            }
            frame_ui::PaintPrimitive::Text {
                x,
                y,
                text,
                font_size,
                ..
            } => {
                let width = (*font_size * 0.55 * text.chars().count() as f64).max(*font_size);
                entry.min_x = entry.min_x.min(*x);
                entry.min_y = entry.min_y.min(*y);
                entry.max_x = entry.max_x.max(*x + width);
                entry.max_y = entry.max_y.max(*y + *font_size * 1.2);
            }
        }
    }
    map.retain(|_, b| {
        b.min_x.is_finite() && b.min_y.is_finite() && b.max_x.is_finite() && b.max_y.is_finite()
    });
    map
}

fn selection_overlay_rect(
    world: PrimitiveBounds,
    content: PrimitiveBounds,
    preview: FramePreviewBounds,
    zoom: f64,
    pan_x: f64,
    pan_y: f64,
) -> OverlayRectCss {
    let dpr = preview.dpr.max(1.0);
    let pad = 16.0 * dpr;
    let left_dp = pad + pan_x + (world.min_x - content.min_x) * zoom;
    let top_dp = pad + pan_y + (world.min_y - content.min_y) * zoom;
    let width_dp = ((world.max_x - world.min_x) * zoom).max(1.0);
    let height_dp = ((world.max_y - world.min_y) * zoom).max(1.0);

    OverlayRectCss {
        x: left_dp / dpr,
        y: top_dp / dpr,
        width: width_dp / dpr,
        height: height_dp / dpr,
    }
}

fn apply_resize_handle(target: &mut EditableRect, handle: ResizeHandle, dx: f64, dy: f64) {
    match handle {
        ResizeHandle::Nw => {
            target.x += dx;
            target.y += dy;
            target.width -= dx;
            target.height -= dy;
        }
        ResizeHandle::Ne => {
            target.y += dy;
            target.width += dx;
            target.height -= dy;
        }
        ResizeHandle::Sw => {
            target.x += dx;
            target.width -= dx;
            target.height += dy;
        }
        ResizeHandle::Se => {
            target.width += dx;
            target.height += dy;
        }
    }
    if target.width < 1.0 {
        target.width = 1.0;
    }
    if target.height < 1.0 {
        target.height = 1.0;
    }
}

fn overlay_handle_style(rect: OverlayRectCss, handle: ResizeHandle) -> String {
    let size = 8.0;
    let half = size * 0.5;
    let (x, y, cursor) = match handle {
        ResizeHandle::Nw => (rect.x - half, rect.y - half, "nwse-resize"),
        ResizeHandle::Ne => (rect.x + rect.width - half, rect.y - half, "nesw-resize"),
        ResizeHandle::Sw => (rect.x - half, rect.y + rect.height - half, "nesw-resize"),
        ResizeHandle::Se => (
            rect.x + rect.width - half,
            rect.y + rect.height - half,
            "nwse-resize",
        ),
    };
    format!("left:{x}px; top:{y}px; width:{size}px; height:{size}px; cursor:{cursor};")
}

#[component]
fn NumericInspectorField(
    label: String,
    value: f64,
    on_change: EventHandler<f64>,
) -> Element {
    rsx! {
        label { class: "flex flex-col gap-1",
            span { class: "text-[10px] uppercase tracking-wide text-zinc-500", "{label}" }
            input {
                r#type: "number",
                step: "1",
                class: "w-full rounded bg-zinc-800 border border-zinc-700 px-2 py-1 text-zinc-200",
                value: format!("{:.0}", value),
                onchange: move |evt| {
                    if let Ok(next) = evt.value().parse::<f64>() {
                        on_change.call(next);
                    }
                }
            }
        }
    }
}

#[cfg(feature = "desktop")]
fn try_load_system_text_font() -> Option<&'static Vec<u8>> {
    static FONT: OnceLock<Option<Vec<u8>>> = OnceLock::new();
    FONT.get_or_init(|| {
        let candidates = [
            "/System/Library/Fonts/SFNS.ttf",
            "/System/Library/Fonts/Supplemental/Arial.ttf",
            "/Library/Fonts/Arial.ttf",
        ];
        for path in candidates {
            if let Ok(bytes) = std::fs::read(path) {
                return Some(bytes);
            }
        }
        None
    })
    .as_ref()
}

fn preview_scene_root(doc: &FrameDocument) -> NodeId {
    let Some(page_id) = doc.pages.first().copied() else {
        return doc.root;
    };
    let Some(page) = doc.get_node(page_id) else {
        return page_id;
    };
    let Some(root_id) = page.children.first().copied() else {
        return page_id;
    };
    let Some(root) = doc.get_node(root_id) else {
        return root_id;
    };

    // Common .fig bridge shape: a white wrapper frame with exactly one child
    // that is the actual UI scene we want to preview.
    if root.children.len() == 1 {
        return root.children[0];
    }

    root_id
}

#[derive(Debug, Clone)]
struct FrameOption {
    id: NodeId,
    label: String,
    wrapper_like: bool,
}

#[derive(Debug, Clone)]
struct LayerRow {
    id: NodeId,
    depth: usize,
    name: String,
    figma_type: String,
    icon: &'static str,
}

fn collect_layer_rows(doc: &FrameDocument) -> Vec<LayerRow> {
    let mut out = Vec::new();
    append_layer_rows(doc, doc.root, 0, &mut out);
    out
}

fn append_layer_rows(doc: &FrameDocument, node_id: NodeId, depth: usize, out: &mut Vec<LayerRow>) {
    let Some(node) = doc.get_node(node_id) else {
        return;
    };
    out.push(LayerRow {
        id: node_id,
        depth,
        name: node.name.clone(),
        figma_type: node.figma_type.clone(),
        icon: layer_icon(&node.figma_type),
    });
    for child in &node.children {
        append_layer_rows(doc, *child, depth + 1, out);
    }
}

fn layer_icon(figma_type: &str) -> &'static str {
    match figma_type {
        "DOCUMENT" | "CANVAS" => "##",
        "FRAME" | "GROUP" | "SECTION" => "[]",
        "TEXT" => "T",
        "INSTANCE" | "COMPONENT" | "COMPONENT_SET" => "<>",
        "VECTOR" | "BOOLEAN_OPERATION" | "STAR" | "POLYGON" => "v",
        "ELLIPSE" => "()",
        "LINE" => "--",
        "RECTANGLE" => "[]",
        _ => "..",
    }
}

fn frame_option_index_by_id(options: &[FrameOption], node_id: NodeId) -> Option<usize> {
    options.iter().position(|option| option.id == node_id)
}

fn collect_frame_options(doc: &FrameDocument) -> Vec<FrameOption> {
    let mut out = Vec::new();
    for node_id in doc.walk_subtree(doc.root) {
        let Some(node) = doc.get_node(node_id) else {
            continue;
        };
        if node.figma_type != "FRAME" {
            continue;
        }
        let wrapper_like = node.children.len() == 1;
        let figma_id = if node.figma_id.is_empty() {
            "synthetic".to_string()
        } else {
            node.figma_id.clone()
        };
        out.push(FrameOption {
            id: node_id,
            label: format!("{} ({figma_id})", node.name),
            wrapper_like,
        });
    }

    out.sort_by(|a, b| {
        a.wrapper_like
            .cmp(&b.wrapper_like)
            .then_with(|| a.label.cmp(&b.label))
    });
    out
}
