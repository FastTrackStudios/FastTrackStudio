//! `ModelPicker` — searchable combobox over `ModelInfo`.
//!
//! Models are sorted by provider so the visual grouping comes for free
//! (the combobox's data-driven API doesn't expose group headers, so we
//! lean on order instead).

use agent_proto::ModelInfo;
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::Sparkles;
use fts_ui::prelude::*;

#[derive(Props, Clone, PartialEq)]
pub struct ModelPickerProps {
    pub value: String,
    pub models: Vec<ModelInfo>,
    pub on_change: EventHandler<String>,
}

#[component]
pub fn ModelPicker(props: ModelPickerProps) -> Element {
    let mut value = use_signal(|| props.value.clone());
    let prop_value = props.value.clone();
    let mut last_external = use_signal(|| prop_value.clone());

    // Keep our internal signal in sync if the parent changes the value.
    let prop_value_for_effect = prop_value.clone();
    use_effect(move || {
        let prev = last_external.read().clone();
        if prev != prop_value_for_effect {
            value.set(prop_value_for_effect.clone());
            last_external.set(prop_value_for_effect.clone());
        }
    });

    let on_change_handler = props.on_change;
    let combo_on_change = use_callback(move |new_value: String| {
        on_change_handler.call(new_value);
    });

    let mut models = props.models.clone();
    models.sort_by(|a, b| {
        a.provider
            .cmp(&b.provider)
            .then_with(|| a.display.cmp(&b.display))
    });

    // ComboboxTrigger renders the raw value (model id) by default. We
    // expose the display name through the `placeholder` slot — it
    // renders only when no value is selected, so we also display a
    // small visual indicator on the right when the current model has
    // reasoning capability via a sibling chip.
    let current = models.iter().find(|m| m.id == *value.read()).cloned();
    let trigger_label = current
        .as_ref()
        .map(|m| m.display.clone())
        .unwrap_or_else(|| "Select model".into());
    let trigger_reasoning = current.as_ref().is_some_and(|m| m.reasoning);

    rsx! {
        div { class: "w-[220px] flex items-center gap-1",
            Combobox {
                value,
                on_change: combo_on_change,
                ComboboxTrigger { placeholder: trigger_label.clone() }
                ComboboxContent { search_placeholder: "Search models…",
                    for model in models.into_iter() {
                        ComboboxItem {
                            key: "{model.id}",
                            value: model.id.clone(),
                            keywords: vec![
                                model.display.clone(),
                                model.provider.clone(),
                            ],
                            div { class: "flex items-center gap-2 w-full",
                                span { class: "flex-1 truncate", "{model.display}" }
                                if model.reasoning {
                                    Badge { variant: BadgeVariant::Secondary, "Reasoning" }
                                }
                                span { class: "text-xs text-muted-foreground",
                                    "{model.context_tokens / 1000}k"
                                }
                            }
                        }
                    }
                }
            }
            if trigger_reasoning {
                Sparkles { size: 12 }
            }
        }
    }
}
