//! Reasoning / extended-thinking panel.
//!
//! Auto-opens while streaming, auto-collapses on stream end — but
//! only on the streaming→idle transition. If the user manually
//! toggles after that, their choice sticks.

use dioxus::prelude::*;
use fts_ui::prelude::*;

#[derive(Props, Clone, PartialEq)]
pub struct ReasoningPanelProps {
    pub text: String,
    pub streaming: bool,
    #[props(default)]
    pub duration_ms: Option<u32>,
}

fn format_duration(ms: u32) -> String {
    if ms < 1_000 {
        format!("{ms}ms")
    } else if ms < 60_000 {
        let s = (ms as f64) / 1_000.0;
        format!("{s:.1}s")
    } else {
        let mins = ms / 60_000;
        let secs = (ms % 60_000) / 1_000;
        format!("{mins}m {secs}s")
    }
}

#[component]
pub fn ReasoningPanel(props: ReasoningPanelProps) -> Element {
    let mut open = use_signal(|| props.streaming);
    let mut last_streaming = use_signal(|| props.streaming);

    // Auto-toggle on streaming transitions, not on each render.
    let current_streaming = props.streaming;
    use_effect(move || {
        let prev = *last_streaming.read();
        if prev != current_streaming {
            open.set(current_streaming);
            last_streaming.set(current_streaming);
        }
    });

    let label = if props.streaming {
        "Thinking".to_string()
    } else {
        match props.duration_ms {
            Some(ms) => format!("Thought · {}", format_duration(ms)),
            None => "Thought".to_string(),
        }
    };

    let body = props.text.clone();
    let open_state = *open.read();

    rsx! {
        div { class: "my-2 border-l-2 border-amber-500/40 pl-3",
            Collapsible {
                open: Some(open_state),
                on_open_change: move |o: bool| open.set(o),
                CollapsibleTrigger {
                    class: "inline-flex items-center gap-2 text-amber-300/80 text-sm hover:text-amber-200",
                    if props.streaming {
                        Spinner { size: SpinnerSize::Small }
                    }
                    "{label}"
                }
                CollapsibleContent { class: "mt-2",
                    div { class: "italic text-muted-foreground text-sm whitespace-pre-wrap",
                        "{body}"
                    }
                }
            }
        }
    }
}
