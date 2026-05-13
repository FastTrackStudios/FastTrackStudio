//! `ClientPicker` — combobox over clients with a "+ New client" action,
//! plus `ClientCreateDialog` for the create-client flow.

use dioxus::prelude::*;
use fts_ui::components::ComboboxItemData;
use fts_ui::lucide_dioxus::UserPlus;
use fts_ui::prelude::*;
use invoice_proto::{Client, ClientCreate};
use uuid::Uuid;

use super::common::parse_money_input;

#[derive(Props, Clone, PartialEq)]
pub struct ClientPickerProps {
    pub clients: Vec<Client>,
    #[props(default)]
    pub value: Option<Uuid>,
    pub on_change: EventHandler<Option<Uuid>>,
    pub on_create_request: EventHandler<()>,
}

#[component]
pub fn ClientPicker(props: ClientPickerProps) -> Element {
    let clients = props.clients.clone();
    let on_change = props.on_change;
    let on_create = props.on_create_request;

    let combo_value = use_signal(|| props.value.map(|id| id.to_string()).unwrap_or_default());

    let mut last_value = use_signal(|| combo_value.read().clone());
    use_effect(move || {
        let cur = combo_value.read().clone();
        if cur != *last_value.read() {
            last_value.set(cur.clone());
            if cur.is_empty() {
                on_change.call(None);
            } else if let Ok(uid) = Uuid::parse_str(&cur) {
                on_change.call(Some(uid));
            }
        }
    });

    let items: Vec<ComboboxItemData> = clients
        .iter()
        .map(|c| ComboboxItemData::new(c.id.to_string(), c.name.clone()))
        .collect();

    rsx! {
        div { class: "flex items-center gap-2",
            div { class: "flex-1 min-w-0",
                Combobox {
                    value: combo_value,
                    ComboboxTrigger { placeholder: "Select client".to_string() }
                    ComboboxContent {
                        items,
                        empty: rsx! {
                            div { class: "py-4 text-center text-xs text-muted-foreground",
                                "No clients yet."
                            }
                        }
                    }
                }
            }
            Button {
                variant: ButtonVariant::Outline,
                size: ButtonSize::Small,
                on_click: move |_| on_create.call(()),
                UserPlus { size: 12 }
                " New"
            }
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct ClientCreateDialogProps {
    pub open: bool,
    pub on_submit: EventHandler<ClientCreate>,
    pub on_close: EventHandler<()>,
}

#[component]
pub fn ClientCreateDialog(props: ClientCreateDialogProps) -> Element {
    if !props.open {
        return rsx! {};
    }

    let name = use_signal(String::new);
    let email = use_signal(String::new);
    let currency = use_signal(|| "USD".to_string());
    let rate_str = use_signal(String::new);
    let notes = use_signal(String::new);

    let on_submit = props.on_submit;
    let on_close = props.on_close;

    let currency_items: Vec<ComboboxItemData> = ["USD", "EUR", "GBP", "CAD", "AUD", "JPY"]
        .iter()
        .map(|c| ComboboxItemData::new(c.to_string(), c.to_string()))
        .collect();

    let do_submit = use_callback(move |_: ()| {
        let n = name.read().clone();
        if n.trim().is_empty() {
            return;
        }
        let e = email.read().clone();
        let cur = currency.read().clone();
        let r = rate_str.read().clone();
        let notes_s = notes.read().clone();
        let payload = ClientCreate {
            name: n.trim().to_string(),
            email: if e.trim().is_empty() {
                None
            } else {
                Some(e.trim().to_string())
            },
            phone: None,
            billing_address_line1: None,
            billing_address_line2: None,
            billing_city: None,
            billing_region: None,
            billing_postal_code: None,
            billing_country: None,
            currency: if cur.trim().is_empty() {
                "USD".into()
            } else {
                cur
            },
            default_rate_cents: parse_money_input(&r),
            notes: if notes_s.trim().is_empty() {
                None
            } else {
                Some(notes_s)
            },
            tags: Vec::new(),
        };
        on_submit.call(payload);
        on_close.call(());
    });

    rsx! {
        Dialog {
            open: true,
            is_modal: true,
            on_open_change: move |open: bool| {
                if !open {
                    on_close.call(());
                }
            },
            DialogHeader {
                DialogTitle { "New client" }
                DialogDescription { "Add a billable customer to invoice." }
            }
            div { class: "flex flex-col gap-3",
                Label { "Name" }
                Input { value: name, placeholder: "Acme Studios" }

                Label { "Email" }
                Input { value: email, placeholder: "billing@acme.test" }

                div { class: "grid grid-cols-2 gap-3",
                    div { class: "flex flex-col gap-1",
                        Label { "Currency" }
                        Combobox {
                            value: currency,
                            ComboboxTrigger { placeholder: "USD".to_string() }
                            ComboboxContent { items: currency_items }
                        }
                    }
                    div { class: "flex flex-col gap-1",
                        Label { "Default rate" }
                        Input { value: rate_str, placeholder: "150.00" }
                    }
                }

                Label { "Notes" }
                Textarea { value: notes, placeholder: "Anything to remember about this client?".to_string() }
            }
            DialogFooter {
                Button {
                    variant: ButtonVariant::Ghost,
                    on_click: move |_| on_close.call(()),
                    "Cancel"
                }
                Button {
                    variant: ButtonVariant::Primary,
                    on_click: move |_| do_submit.call(()),
                    "Create client"
                }
            }
        }
    }
}
