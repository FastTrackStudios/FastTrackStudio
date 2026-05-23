//! Quick-add input bar. Today: plain title. Future: parse
//! natural language (`"Buy milk tomorrow #errands @shopping"`)
//! the same way `task::capture::capture` does on the desktop
//! side. Until that's reachable from wasm, the only thing the
//! input does is set the title.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::Plus;

use crate::TaskInfo;

#[derive(Props, Clone, PartialEq)]
pub struct QuickAddProps {
    pub on_create: EventHandler<TaskInfo>,
}

#[component]
pub fn QuickAdd(props: QuickAddProps) -> Element {
    let mut value = use_signal(String::new);

    let mut submit = move || {
        let title = value.read().trim().to_string();
        if title.is_empty() {
            return;
        }
        props.on_create.call(TaskInfo::new(title));
        value.set(String::new());
    };

    rsx! {
        form {
            class: "flex items-center gap-2 rounded-lg border border-border bg-card px-3 py-2 shadow-sm focus-within:ring-2 focus-within:ring-primary/50",
            onsubmit: move |e: Event<FormData>| {
                e.prevent_default();
                submit();
            },
            div { class: "flex h-6 w-6 items-center justify-center rounded-md bg-primary/15 text-primary",
                Plus { size: 14 }
            }
            input {
                r#type: "text",
                value: "{value}",
                placeholder: "Add a task — try \"Buy milk tomorrow #errands\"",
                class: "flex-1 bg-transparent text-sm text-foreground placeholder:text-muted-foreground outline-none",
                oninput: move |e| value.set(e.value()),
            }
            kbd { class: "rounded border border-border bg-muted/50 px-1.5 py-0.5 text-[10px] text-muted-foreground",
                "⏎"
            }
        }
    }
}
