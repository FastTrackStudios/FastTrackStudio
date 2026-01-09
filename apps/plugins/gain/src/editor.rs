//! Dioxus UI for FTS Gain plugin.
//!
//! Uses PluginShell for consistent window chrome and size controls.

use fts_gain_core::GainMeterState;
use fts_plugin_core::prelude::*;
use nih_plug_dioxus::SharedState;
use std::sync::Arc;

use crate::GainParams;

/// Plugin version for display in the shell.
const PLUGIN_VERSION: &str = env!("CARGO_PKG_VERSION");

/// Create the Dioxus editor for the gain plugin.
pub fn create(params: Arc<GainParams>, meters: GainMeterState) -> Option<Box<dyn Editor>> {
    let ui_state = Arc::new(EditorState { params, meters });
    create_dioxus_editor_with_state(ui_state.params.editor_state.clone(), ui_state, App)
}

/// Shared state for the editor.
struct EditorState {
    params: Arc<GainParams>,
    meters: GainMeterState,
}

/// Main app component - wrapped in PluginShell.
#[component]
fn App() -> Element {
    rsx! {
        PluginShell {
            plugin_name: "FTS Gain".to_string(),
            version: Some(PLUGIN_VERSION.to_string()),
            GainContent {}
        }
    }
}

// Extra CSS to test if inline style elements work
// Test 1: Simple CSS (no @layer) - WORKS based on purple test
const TEST_CSS_SIMPLE: &str = r#"
.test-purple { background-color: #a855f7; }
.test-rounded { border-radius: 12px; }
.test-padding { padding: 16px; }
"#;

// Test 2: CSS with @layer (like Tailwind v4 output)
const TEST_CSS_WITH_LAYER: &str = r#"
@layer test {
    .layer-orange { background-color: #f97316; }
    .layer-rounded { border-radius: 12px; }
    .layer-padding { padding: 16px; }
}
"#;

// Test 3: CSS with @layer declaration first (Tailwind pattern)
const TEST_CSS_LAYER_ORDER: &str = r#"
@layer base, components, utilities;
@layer utilities {
    .ordered-pink { background-color: #ec4899; }
    .ordered-rounded { border-radius: 12px; }
}
"#;

// Test 4: CSS using var() - to test if CSS custom properties work
const TEST_CSS_VAR: &str = r#"
:root {
    --test-cyan: #06b6d4;
    --test-radius: 12px;
}
.var-cyan { background-color: var(--test-cyan); }
.var-rounded { border-radius: var(--test-radius); }
"#;

// Test 5: CSS using oklch() - to test if oklch color function works
const TEST_CSS_OKLCH: &str = r#"
.oklch-lime { background-color: oklch(79.5% 0.184 86.047); }
"#;

// Test 6: CSS using calc() - to test if calc works with var()
const TEST_CSS_CALC: &str = r#"
:root {
    --test-spacing: 0.25rem;
}
.calc-padding { padding: calc(var(--test-spacing) * 12); }
.calc-padding-rem { padding: calc(0.25rem * 12); }
.calc-padding-simple { padding: calc(4px * 4); }
.direct-padding { padding: 12px; }
.direct-padding-rem { padding: 0.75rem; }
/* Test using Tailwind's --spacing variable (defined in @layer theme) */
.calc-tailwind-spacing { padding: calc(var(--spacing) * 12); }
"#;

/// The actual gain plugin UI content (inside the shell).
///
/// This is a CSS test component to verify @layer support in Blitz.
/// Left side uses raw inline CSS, right side uses Tailwind classes.
#[component]
fn GainContent() -> Element {
    // Get the SharedState wrapper and downcast to our EditorState
    let shared = use_context::<SharedState>();
    let state = shared
        .get::<EditorState>()
        .expect("EditorState not in context");

    // Create signals for reactive UI updates
    let mut input_db = use_signal(|| -60.0f32);
    let mut output_db = use_signal(|| -60.0f32);
    let mut gain_db = use_signal(|| 0.0f32);
    let mut phase_invert = use_signal(|| false);

    // Demo state
    let mut click_count = use_signal(|| 0i32);

    // Poll for updates at ~30fps
    let state_for_poll = state.clone();
    use_future(move || {
        let state = state_for_poll.clone();
        async move {
            loop {
                // Read current values from atomics
                let (in_db, out_db) = state.meters.to_db();
                let params = state.params.to_values();

                // Update signals
                input_db.set(in_db);
                output_db.set(out_db);
                gain_db.set(params.gain_db);
                phase_invert.set(params.phase_invert);

                // Sleep for ~33ms (30fps)
                futures_timer::Delay::new(std::time::Duration::from_millis(33)).await;
            }
        }
    });

    // Read values
    let in_db = *input_db.read();
    let out_db = *output_db.read();
    let gain = *gain_db.read();
    let clicks = *click_count.read();

    // Normalize for display (0-100%)
    let input_pct = ((in_db + 60.0) / 66.0 * 100.0).clamp(0.0, 100.0);
    let output_pct = ((out_db + 60.0) / 66.0 * 100.0).clamp(0.0, 100.0);

    rsx! {
        // Inject test CSS in different formats
        style { {TEST_CSS_SIMPLE} }
        style { {TEST_CSS_WITH_LAYER} }
        style { {TEST_CSS_LAYER_ORDER} }
        style { {TEST_CSS_VAR} }
        style { {TEST_CSS_OKLCH} }
        style { {TEST_CSS_CALC} }

        // Main container - using inline CSS to ensure it works
        div {
            style: "width: 100%; height: 100%; display: flex; gap: 16px; padding: 16px; background-color: #18181b;",

            // ============================================
            // LEFT SIDE: Testing CSS features
            // ============================================
            div {
                style: "flex: 1; display: flex; flex-direction: column; gap: 6px;",

                // Header
                div {
                    style: "background-color: #3b82f6; color: white; padding: 6px; border-radius: 8px; font-weight: bold; text-align: center; font-size: 12px;",
                    "CSS Feature Tests"
                }

                // Test 1: Simple CSS (no @layer) - PURPLE
                div {
                    class: "test-purple test-rounded test-padding",
                    style: "color: white; font-weight: bold; font-size: 11px;",
                    "1. Simple CSS (purple)"
                }

                // Test 2: CSS with @layer block - ORANGE
                div {
                    class: "layer-orange layer-rounded layer-padding",
                    style: "color: white; font-weight: bold; font-size: 11px;",
                    "2. @layer block (orange)"
                }

                // Test 3: CSS with @layer order declaration - PINK
                div {
                    class: "ordered-pink ordered-rounded",
                    style: "color: white; font-weight: bold; font-size: 11px; padding: 16px;",
                    "3. @layer order (pink)"
                }

                // Test 4: CSS with var() - CYAN
                div {
                    class: "var-cyan var-rounded",
                    style: "color: white; font-weight: bold; font-size: 11px; padding: 16px;",
                    "4. var() custom props (cyan)"
                }

                // Test 5: CSS with oklch() - YELLOW/LIME
                div {
                    class: "oklch-lime",
                    style: "color: black; font-weight: bold; font-size: 11px; padding: 16px; border-radius: 12px;",
                    "5. oklch() color"
                }

                // Test 6a: calc() with var() - should have padding
                div {
                    class: "calc-padding",
                    style: "background-color: #14b8a6; color: white; font-weight: bold; font-size: 11px; border-radius: 12px;",
                    "6a. calc(var()*12) ✓"
                }

                // Test 6b: calc() with rem (no var) - should have padding
                div {
                    class: "calc-padding-rem",
                    style: "background-color: #06b6d4; color: white; font-weight: bold; font-size: 11px; border-radius: 12px;",
                    "6b. calc(0.25rem*12)"
                }

                // Test 6c: calc() with px - should have padding
                div {
                    class: "calc-padding-simple",
                    style: "background-color: #8b5cf6; color: white; font-weight: bold; font-size: 11px; border-radius: 12px;",
                    "6c. calc(4px*4)"
                }

                // Test 6d: direct padding px - should have padding
                div {
                    class: "direct-padding",
                    style: "background-color: #f43f5e; color: white; font-weight: bold; font-size: 11px; border-radius: 12px;",
                    "6d. padding: 12px"
                }

                // Test 6e: direct padding rem - should have padding
                div {
                    class: "direct-padding-rem",
                    style: "background-color: #ec4899; color: white; font-weight: bold; font-size: 11px; border-radius: 12px;",
                    "6e. padding: 0.75rem"
                }

                // Test 6f: calc with Tailwind's --spacing (from @layer theme)
                div {
                    class: "calc-tailwind-spacing",
                    style: "background-color: #0ea5e9; color: white; font-weight: bold; font-size: 11px; border-radius: 12px;",
                    "6f. calc(var(--spacing)*12)"
                }
            }

            // ============================================
            // RIGHT SIDE: Tailwind CSS (testing @layer)
            // ============================================
            div {
                class: "flex-1 flex flex-col gap-3",

                // Header - Tailwind
                div {
                    class: "bg-purple-500 text-white p-3 rounded-lg font-bold text-center",
                    "TAILWIND CSS (testing @layer)"
                }

                // Red box - Tailwind
                div {
                    class: "bg-red-500 text-white p-4 rounded-lg",
                    "Red box - Tailwind bg-red-500"
                }

                // Green box - Tailwind
                div {
                    class: "bg-green-500 text-white p-4 rounded-lg",
                    "Green box - Tailwind bg-green-500"
                }

                // Blue box with border - Tailwind
                div {
                    class: "bg-blue-500 text-white p-4 rounded-lg border-4 border-solid border-white",
                    "Blue box - Tailwind with border"
                }

                // Meter display - Tailwind
                div {
                    class: "bg-zinc-800 p-4 rounded-lg",

                    div {
                        class: "text-zinc-500 text-xs mb-2",
                        "GAIN VALUE (Tailwind)"
                    }

                    div {
                        class: "text-white text-3xl font-bold",
                        "{gain:+.1} dB"
                    }
                }

                // Button test - Tailwind
                div {
                    class: "bg-zinc-800 p-3 rounded-lg",

                    SimpleButton {
                        label: "Click me (Tailwind)",
                        variant: "primary",
                        on_click: move |_| {
                            let c = *click_count.read();
                            click_count.set(c + 1);
                        },
                    }
                }

                // Meters - Tailwind
                div {
                    class: "flex gap-3 flex-1",

                    MeterGroup {
                        label: "IN",
                        level_pct: input_pct,
                        level_db: in_db,
                    }

                    MeterGroup {
                        label: "OUT",
                        level_pct: output_pct,
                        level_db: out_db,
                    }
                }
            }
        }
    }
}

// ============================================================================
// Simple UI Components
// ============================================================================

/// Meter group with label
#[component]
fn MeterGroup(label: &'static str, level_pct: f32, level_db: f32) -> Element {
    rsx! {
        div {
            class: "flex flex-col items-center flex-1",

            div { class: "text-xs text-zinc-500 font-medium mb-1", "{label}" }

            div {
                class: "flex gap-1 flex-1 w-full",
                MeterBar { level_pct: level_pct }
                MeterBar { level_pct: level_pct }
            }

            div {
                class: "text-xs text-zinc-500 font-mono mt-1",
                "{level_db:.0}"
            }
        }
    }
}

/// Single meter bar
#[component]
fn MeterBar(level_pct: f32) -> Element {
    let green = level_pct.min(70.0);
    let yellow = (level_pct - 70.0).max(0.0).min(20.0);
    let red = (level_pct - 90.0).max(0.0);

    rsx! {
        div {
            class: "flex-1 bg-zinc-700 rounded-sm overflow-hidden flex flex-col-reverse min-h-32",

            // Green segment
            div {
                class: "w-full bg-green-500",
                style: format!("height: {}%;", green),
            }

            // Yellow segment
            if yellow > 0.0 {
                div {
                    class: "w-full bg-yellow-500",
                    style: format!("height: {}%;", yellow),
                }
            }

            // Red segment
            if red > 0.0 {
                div {
                    class: "w-full bg-red-500",
                    style: format!("height: {}%;", red),
                }
            }
        }
    }
}

/// Simple button
#[component]
fn SimpleButton(
    label: &'static str,
    #[props(default = "primary")] variant: &'static str,
    on_click: EventHandler<MouseEvent>,
) -> Element {
    let class = match variant {
        "primary" => {
            "w-full px-3 py-1.5 bg-blue-600 hover:bg-blue-700 text-white text-sm font-medium rounded cursor-pointer"
        }
        "secondary" => {
            "w-full px-3 py-1.5 bg-zinc-700 hover:bg-zinc-600 text-white text-sm font-medium rounded cursor-pointer"
        }
        "danger" => {
            "w-full px-3 py-1.5 bg-red-600 hover:bg-red-700 text-white text-sm font-medium rounded cursor-pointer"
        }
        _ => "w-full px-3 py-1.5 bg-zinc-700 text-white text-sm font-medium rounded cursor-pointer",
    };

    rsx! {
        button {
            class: "{class}",
            onclick: move |e| on_click.call(e),
            "{label}"
        }
    }
}
