//! `CheckoutDialog` — modal for assigning an item to a person.

use chrono::{DateTime, Utc};
use dioxus::prelude::*;
use fts_ui::prelude::*;
use person_proto::Person;
use uuid::Uuid;

/// Payload emitted when the user confirms a checkout.
#[derive(Clone, Debug, PartialEq)]
pub struct CheckoutCmd {
    pub item_id: Uuid,
    pub person_id: Uuid,
    pub due: Option<DateTime<Utc>>,
    pub note: Option<String>,
}

#[component]
pub fn CheckoutDialog(
    open: bool,
    item_id: Uuid,
    item_name: String,
    people: Vec<Person>,
    on_close: EventHandler<()>,
    on_confirm: EventHandler<CheckoutCmd>,
) -> Element {
    let mut selected = use_signal::<Option<Uuid>>(|| None);
    let note = use_signal(String::new);

    rsx! {
        Dialog {
            open: Some(open),
            on_close: move |_| on_close.call(()),
            DialogHeader {
                DialogTitle { "Check out " "{item_name}" }
                DialogDescription {
                    "Assign this item to a person. Status flips to ‘checked-out’ and a CheckoutEvent is created."
                }
            }
            div { class: "flex flex-col gap-3 p-4",
                Label { "Assign to" }
                Popover {
                    PopoverTrigger {
                        Button {
                            variant: ButtonVariant::Outline,
                            size: ButtonSize::Medium,
                            {
                                match *selected.read() {
                                    Some(id) => people.iter().find(|p| p.id == id).map(|p| p.name.clone()).unwrap_or_else(|| "Pick a person".into()),
                                    None => "Pick a person".to_string(),
                                }
                            }
                        }
                    }
                    PopoverContent { class: "p-1 min-w-60 max-h-72 overflow-auto",
                        for p in people.iter().cloned() {
                            button {
                                key: "{p.id}",
                                class: "w-full rounded px-2 py-1 text-left text-sm hover:bg-accent",
                                onclick: move |_| selected.set(Some(p.id)),
                                "{p.name}"
                            }
                        }
                    }
                }

                Label { "Note (optional)" }
                Textarea {
                    placeholder: "Where it's going, who'll bring it back, etc.",
                    value: note,
                }
            }
            DialogFooter {
                Button {
                    variant: ButtonVariant::Ghost,
                    on_click: move |_| on_close.call(()),
                    "Cancel"
                }
                Button {
                    on_click: move |_| {
                        if let Some(pid) = *selected.read() {
                            let n = note.read().clone();
                            let payload = CheckoutCmd {
                                item_id,
                                person_id: pid,
                                due: None,
                                note: if n.trim().is_empty() { None } else { Some(n) },
                            };
                            on_confirm.call(payload);
                        }
                    },
                    "Check out"
                }
            }
        }
    }
}
