//! Extension System Demo - WASM Local Services
//!
//! Demonstrates the extension architecture running locally in the browser.
//! Services run in-memory without any backend server.

use dioxus::prelude::*;
use daw_proto::*;
use daw_standalone::{DawStandaloneTransport, LocalDawClient};
use std::sync::Arc;

#[cfg(target_arch = "wasm32")]
use gloo_timers::future::TimeoutFuture;

/// State for the extension demo
#[derive(Clone)]
struct ExtensionDemoState {
    client: LocalDawClient,
    playback_state: Signal<Option<PlaybackState>>,
}

/// Extension demo component showing DAW transport controls
#[component]
pub fn ExtensionDemo() -> Element {
    // Create service instances (runs LOCALLY in browser)
    let mut state = use_signal(|| {
        let transport = Arc::new(DawStandaloneTransport::new());
        let client = LocalDawClient::new(transport);
        ExtensionDemoState {
            client,
            playback_state: Signal::new(None),
        }
    });

    // Update playback state periodically
    use_effect(move || {
        let client = state.read().client.clone();
        let mut state_clone = state.clone();
        spawn(async move {
            loop {
                // Poll playback state every 100ms
                #[cfg(target_arch = "wasm32")]
                TimeoutFuture::new(100).await;
                #[cfg(not(target_arch = "wasm32"))]
                tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

                // Get state from transport service
                let ps = client.get_state().await;
                state_clone.write().playback_state.set(Some(ps));
            }
        });
    });

    // Clone transport for each button handler
    let playback_state = state.read().playback_state.read().clone();

    rsx! {
        div { class: "max-w-4xl mx-auto p-8 space-y-6",
            // Header
            div { class: "space-y-2",
                h1 { class: "text-3xl font-bold",
                    "Extension System Demo"
                }
                p { class: "text-muted-foreground",
                    "DAW services running locally in your browser (no backend required)"
                }
            }

            // Architecture explanation
            div { class: "bg-muted p-6 rounded-lg space-y-3",
                h2 { class: "text-xl font-semibold mb-3",
                    "Architecture"
                }
                div { class: "space-y-2 text-sm",
                    div { class: "flex items-start gap-2",
                        span { class: "font-mono text-primary", "WASM:" }
                        span { "Services run directly in browser (DawStandaloneTransport)" }
                    }
                    div { class: "flex items-start gap-2",
                        span { class: "font-mono text-primary", "Native:" }
                        span { "Same service traits via SHM (DawStandaloneTransport or DawReaperTransport)" }
                    }
                    div { class: "flex items-start gap-2",
                        span { class: "font-mono text-primary", "Benefits:" }
                        span { "Write once, run everywhere - REAPER, desktop, browser" }
                    }
                }
            }

            // Transport controls
            div { class: "bg-card border rounded-lg p-6 space-y-4",
                h2 { class: "text-xl font-semibold",
                    "Transport Service (Standalone DAW)"
                }

                // Playback state display
                if let Some(state) = playback_state {
                    div { class: "bg-muted p-4 rounded space-y-2 font-mono text-sm",
                        div { class: "flex justify-between",
                            span { "Playing:" }
                            span { class: if state.is_playing { "text-green-500" } else { "text-muted-foreground" },
                                "{state.is_playing}"
                            }
                        }
                        div { class: "flex justify-between",
                            span { "Recording:" }
                            span { class: if state.is_recording { "text-red-500" } else { "text-muted-foreground" },
                                "{state.is_recording}"
                            }
                        }
                        div { class: "flex justify-between",
                            span { "Position:" }
                            span { "{state.position_seconds:.2}s" }
                        }
                        div { class: "flex justify-between",
                            span { "Tempo:" }
                            span { "{state.tempo_bpm:.1} BPM" }
                        }
                    }
                }

                // Control buttons
                div { class: "flex gap-3",
                    button {
                        class: "px-4 py-2 bg-green-600 hover:bg-green-700 text-white rounded-md transition-colors",
                        onclick: move |_| {
                            let client = state.read().client.clone();
                            spawn(async move {
                                let _ = client.play().await;
                            });
                        },
                        "▶ Play"
                    }
                    button {
                        class: "px-4 py-2 bg-blue-600 hover:bg-blue-700 text-white rounded-md transition-colors",
                        onclick: move |_| {
                            let client = state.read().client.clone();
                            spawn(async move {
                                let _ = client.stop().await;
                            });
                        },
                        "■ Stop"
                    }
                    button {
                        class: "px-4 py-2 bg-red-600 hover:bg-red-700 text-white rounded-md transition-colors",
                        onclick: move |_| {
                            let client = state.read().client.clone();
                            spawn(async move {
                                let _ = client.record().await;
                            });
                        },
                        "● Record"
                    }
                }

                // Position controls
                div { class: "space-y-2",
                    label { class: "text-sm font-medium",
                        "Seek Position"
                    }
                    div { class: "flex gap-3",
                        button {
                            class: "px-3 py-2 bg-secondary hover:bg-secondary/80 rounded-md transition-colors",
                            onclick: move |_| {
                                let client = state.read().client.clone();
                                spawn(async move {
                                    let _ = client.set_position(0.0).await;
                                });
                            },
                            "⏮ Start"
                        }
                        button {
                            class: "px-3 py-2 bg-secondary hover:bg-secondary/80 rounded-md transition-colors",
                            onclick: move |_| {
                                let client = state.read().client.clone();
                                spawn(async move {
                                    let current = client.get_state().await.position_seconds;
                                    let _ = client.set_position(current - 5.0).await;
                                });
                            },
                            "⏪ -5s"
                        }
                        button {
                            class: "px-3 py-2 bg-secondary hover:bg-secondary/80 rounded-md transition-colors",
                            onclick: move |_| {
                                let client = state.read().client.clone();
                                spawn(async move {
                                    let current = client.get_state().await.position_seconds;
                                    let _ = client.set_position(current + 5.0).await;
                                });
                            },
                            "⏩ +5s"
                        }
                    }
                }
            }

            // Code example
            div { class: "bg-muted p-6 rounded-lg",
                h2 { class: "text-xl font-semibold mb-3",
                    "Usage Example"
                }
                pre { class: "text-xs overflow-x-auto bg-background p-4 rounded",
                    code {
r#"// Same service trait works everywhere!

// In WASM (this page):
let transport = Arc::new(DawStandaloneTransport::new());
let client = LocalDawClient::new(transport);
client.play().await;

// In REAPER:
let client = TransportServiceClient::new(shm_handle);
client.play().await;

// In desktop app:
let client = TransportServiceClient::new(shm_handle);
client.play().await;"#
                    }
                }
            }
        }
    }
}
