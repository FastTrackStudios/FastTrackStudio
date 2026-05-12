use dioxus::prelude::*;

use crate::data::{inbox_items, project_name, InboxItem};

#[component]
pub fn Inbox() -> Element {
    let items = inbox_items();
    let unread = items.len();

    rsx! {
        div { class: "mx-auto flex max-w-6xl flex-col gap-8 p-6 lg:p-10",
            section { class: "flex flex-col gap-4 lg:flex-row lg:items-end lg:justify-between",
                div { class: "space-y-2",
                    p { class: "text-sm font-semibold uppercase tracking-[0.3em] text-cyan-300", "Inbox" }
                    h1 { class: "text-4xl font-black tracking-tight text-white", "Triage" }
                    p { class: "max-w-2xl text-slate-400", "Captured items waiting on a decision. Convert into a task, file under a project, defer, or drop." }
                }
                div { class: "rounded-2xl border border-slate-800 bg-slate-950/70 px-5 py-3 text-center",
                    div { class: "text-2xl font-black text-white", "{unread}" }
                    div { class: "text-xs font-semibold uppercase tracking-[0.2em] text-slate-500", "to triage" }
                }
            }

            if items.is_empty() {
                section { class: "rounded-2xl border border-slate-800 bg-slate-900/70 p-10 text-center",
                    p { class: "text-2xl font-bold text-emerald-300", "Inbox zero." }
                    p { class: "mt-2 text-slate-400", "Nothing waiting on a decision right now." }
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

#[component]
fn InboxRow(item: InboxItem) -> Element {
    let hint_name = item.project_hint.and_then(project_name);

    rsx! {
        article { class: "rounded-2xl border border-slate-800 bg-slate-900/70 p-5 shadow-md shadow-black/20 hover:border-slate-700",
            header { class: "flex flex-col gap-3 sm:flex-row sm:items-start sm:justify-between",
                div { class: "flex items-start gap-3",
                    span {
                        class: "flex h-9 w-9 shrink-0 items-center justify-center rounded-xl border border-slate-700 bg-slate-950 text-base text-cyan-300",
                        "{item.source.icon()}"
                    }
                    div { class: "space-y-1",
                        h2 { class: "text-base font-semibold text-white", "{item.title}" }
                        p { class: "text-xs uppercase tracking-[0.2em] text-slate-500",
                            "{item.source.label()} · {item.from} · {item.received}"
                        }
                    }
                }
                if let Some(name) = hint_name {
                    span { class: "shrink-0 rounded-full border border-cyan-400/30 bg-cyan-400/10 px-3 py-1 text-xs font-semibold text-cyan-200",
                        "→ {name}"
                    }
                }
            }
            p { class: "mt-4 text-sm text-slate-300", "{item.summary}" }
            footer { class: "mt-4 flex flex-wrap gap-2",
                ActionButton { label: "Convert to task" }
                if hint_name.is_some() {
                    ActionButton { label: "File to project" }
                } else {
                    ActionButton { label: "Choose project" }
                }
                ActionButton { label: "Defer" }
                ActionButton { label: "Drop", danger: true }
            }
        }
    }
}

#[component]
fn ActionButton(label: &'static str, #[props(default = false)] danger: bool) -> Element {
    let class = if danger {
        "rounded-xl border border-rose-400/30 bg-rose-400/10 px-3 py-1.5 text-xs font-semibold text-rose-200 hover:bg-rose-400/20"
    } else {
        "rounded-xl border border-slate-700 bg-slate-950/60 px-3 py-1.5 text-xs font-semibold text-slate-200 hover:bg-slate-800"
    };

    rsx! {
        // Actions are wireframes for now — wire to backend in a follow-up.
        button { class, onclick: move |_| {}, "{label}" }
    }
}
