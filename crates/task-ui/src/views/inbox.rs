use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Mail, MessagesSquare, PencilLine, Zap};
use fts_ui::prelude::*;

use crate::data::{InboxItem, InboxSource, inbox_items, project_name};

#[component]
pub fn Inbox() -> Element {
    let items = inbox_items();
    let unread = items.len();

    rsx! {
        div { class: "mx-auto flex max-w-6xl flex-col gap-8 p-6 lg:p-10",
            section { class: "flex flex-col gap-4 lg:flex-row lg:items-end lg:justify-between",
                div { class: "space-y-2",
                    Text { variant: TextVariant::Small, class: "uppercase tracking-[0.3em] text-primary font-semibold", "Inbox" }
                    Heading { level: HeadingLevel::H1, "Triage" }
                    Text { variant: TextVariant::Muted, class: "max-w-2xl",
                        "Captured items waiting on a decision. Convert into a task, file under a project, defer, or drop."
                    }
                }
                Card { class: "min-w-32",
                    CardContent { class: "p-4 pt-4 text-center",
                        div { class: "text-2xl font-extrabold text-foreground", "{unread}" }
                        Text { variant: TextVariant::Small, class: "uppercase tracking-[0.2em] text-muted-foreground font-semibold", "to triage" }
                    }
                }
            }

            if items.is_empty() {
                EmptyState {
                    message: "Inbox zero. Nothing waiting on a decision right now.",
                    icon: rsx! { Mail { size: 32 } },
                }
            } else {
                section { class: "flex flex-col gap-3",
                    for item in items {
                        InboxRow { key: "{item.id}", item }
                    }
                }
            }
        }
    }
}

fn source_icon(source: InboxSource) -> Element {
    match source {
        InboxSource::Email => rsx! { Mail { size: 18 } },
        InboxSource::Webhook => rsx! { Zap { size: 18 } },
        InboxSource::Talk => rsx! { MessagesSquare { size: 18 } },
        InboxSource::Manual => rsx! { PencilLine { size: 18 } },
    }
}

#[component]
fn InboxRow(item: InboxItem) -> Element {
    let hint_name = item.project_hint.and_then(project_name);

    rsx! {
        Card {
            CardHeader { class: "flex flex-col gap-3 sm:flex-row sm:items-start sm:justify-between",
                HStack { gap: "3", class: "items-start",
                    div { class: "flex h-9 w-9 shrink-0 items-center justify-center rounded-xl border border-border bg-card text-primary",
                        {source_icon(item.source)}
                    }
                    div { class: "flex flex-col gap-1",
                        CardTitle { "{item.title}" }
                        Text { variant: TextVariant::Small, class: "uppercase tracking-[0.2em] text-muted-foreground",
                            "{item.source.label()} · {item.from} · {item.received}"
                        }
                    }
                }
                if let Some(name) = hint_name {
                    Badge { variant: BadgeVariant::Secondary, "→ {name}" }
                }
            }
            CardContent {
                Text { "{item.summary}" }
                div { class: "mt-4 flex flex-wrap gap-2",
                    Button { variant: ButtonVariant::Outline, size: ButtonSize::Small, "Convert to task" }
                    if hint_name.is_some() {
                        Button { variant: ButtonVariant::Outline, size: ButtonSize::Small, "File to project" }
                    } else {
                        Button { variant: ButtonVariant::Outline, size: ButtonSize::Small, "Choose project" }
                    }
                    Button { variant: ButtonVariant::Ghost, size: ButtonSize::Small, "Defer" }
                    Button { variant: ButtonVariant::Destructive, size: ButtonSize::Small, "Drop" }
                }
            }
        }
    }
}
