//! `InvoiceStatusPipeline` — visual 5-segment lifecycle pipeline.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{ChevronRight, CircleCheck};

#[derive(Props, Clone, PartialEq)]
pub struct InvoiceStatusPipelineProps {
    pub status: String,
    #[props(default)]
    pub overdue: bool,
}

#[component]
pub fn InvoiceStatusPipeline(props: InvoiceStatusPipelineProps) -> Element {
    // 5-segment pipeline: Draft → Sent → Viewed → Partial → Paid.
    // Unknown / cancelled renders all neutral.
    let steps = ["draft", "sent", "viewed", "partial", "paid"];
    let labels = ["Draft", "Sent", "Viewed", "Partial", "Paid"];

    let active_idx = steps.iter().position(|s| *s == props.status.as_str());

    rsx! {
        div { class: "flex items-center gap-1 text-[11px]",
            for (i, label) in labels.iter().enumerate() {
                {
                    let (bg, fg, show_check) = match active_idx {
                        Some(active) if i < active => ("bg-muted", "text-muted-foreground", true),
                        Some(active) if i == active => {
                            if props.overdue && (label == &"Sent" || label == &"Viewed") {
                                ("bg-destructive", "text-destructive-foreground", false)
                            } else {
                                ("bg-primary", "text-primary-foreground", false)
                            }
                        }
                        _ => ("bg-muted/40", "text-muted-foreground", false),
                    };
                    let pill_class = format!(
                        "inline-flex items-center gap-1 rounded-full px-2 py-0.5 {} {}",
                        bg, fg
                    );
                    rsx! {
                        span { key: "{i}", class: "{pill_class}",
                            if show_check {
                                CircleCheck { size: 10 }
                            }
                            "{label}"
                        }
                        if i + 1 < labels.len() {
                            ChevronRight { size: 10 }
                        }
                    }
                }
            }
        }
    }
}
