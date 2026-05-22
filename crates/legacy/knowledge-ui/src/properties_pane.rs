//! Phase 6.5b — Properties pane.
//!
//! Renders the `PropertySchemaRegistry`-declared properties of a
//! page as an editable list of typed rows. Layout mirrors
//! Obsidian's Properties pane (`~/Development/research/obsidian-api/
//! obsidian.d.ts` — `MetadataCache.frontmatter`): one row per
//! property, key on the left, type-specific editor on the right.
//!
//! Scope for Phase 6.5b:
//!
//! - Text, Multitext, Number, Checkbox, Date, Datetime — native
//!   editors.
//! - Tags / Aliases / LinkList — chip list with an add-input.
//! - EnumWithMetadata — dropdown showing each option's label (color
//!   styling lands when fts-ui exposes a Select that takes color
//!   chips; for now we render `<select>`).
//! - Link — single-line input. Autocomplete against the basename
//!   index follows in 6.5c.
//! - LexoRank — hidden (internal). The kanban writes it on drop.
//! - Struct / StructList / Json — render as read-only YAML
//!   preview. Editable structured editor follows in 6.6.
//! - Computed — read-only display, marked.
//!
//! The pane is a dumb component: it takes the current frontmatter
//! JSON + the schema + an `on_change(key, new_value)` callback.
//! Caller (`KnowledgeLive`) writes the change into the local doc.

use dioxus::prelude::*;
use knowledge_proto::property_schema::{EnumOption, KindSchema, PropertyDef, PropertyType};
use serde_json::Value as JsonValue;

/// `PropertiesPane` — top-of-page typed editors for a page's
/// frontmatter.
#[component]
pub fn PropertiesPane(
    schema: KindSchema,
    frontmatter_json: String,
    on_change: Callback<(String, JsonValue)>,
) -> Element {
    let parsed: serde_json::Map<String, JsonValue> =
        serde_json::from_str::<JsonValue>(&frontmatter_json)
            .ok()
            .and_then(|v| v.as_object().cloned())
            .unwrap_or_default();
    let schema_kind = schema.kind.clone();
    // Group: required first, then optional. Skip internal-only
    // types (LexoRank). Sort within each group preserving the
    // schema's declared order so authors control display order.
    let mut required: Vec<PropertyDef> = Vec::new();
    let mut optional: Vec<PropertyDef> = Vec::new();
    for prop in &schema.properties {
        if matches!(prop.ty, PropertyType::LexoRank) {
            continue;
        }
        if prop.required {
            required.push(prop.clone());
        } else {
            optional.push(prop.clone());
        }
    }
    rsx! {
        section {
            "data-testid": "properties-pane",
            "data-kind": schema_kind,
            class: "flex flex-col gap-1",
            for prop in required.iter() {
                PropertyRow {
                    key: "{prop.key}",
                    prop: prop.clone(),
                    value: parsed.get(&prop.key).cloned().unwrap_or(JsonValue::Null),
                    on_change,
                }
            }
            if !required.is_empty() && !optional.is_empty() {
                div { class: "h-px bg-border/40 my-1" }
            }
            for prop in optional.iter() {
                PropertyRow {
                    key: "{prop.key}",
                    prop: prop.clone(),
                    value: parsed.get(&prop.key).cloned().unwrap_or(JsonValue::Null),
                    on_change,
                }
            }
        }
    }
}

#[component]
fn PropertyRow(
    prop: PropertyDef,
    value: JsonValue,
    on_change: Callback<(String, JsonValue)>,
) -> Element {
    // LexoRank rows are internal — skip rendering.
    if matches!(prop.ty, PropertyType::LexoRank) {
        return rsx! {};
    }
    let label = prop
        .display_name
        .clone()
        .unwrap_or_else(|| prop.key.clone());
    let row_testid = format!("prop-row-{}", prop.key);
    let required_marker = if prop.required { Some("●") } else { None };
    rsx! {
        div {
            "data-testid": row_testid,
            "data-prop-key": "{prop.key}",
            class: "grid grid-cols-[7rem_1fr] gap-2 items-start text-xs py-0.5",
            div { class: "flex items-center gap-1 text-muted-foreground pt-1.5",
                if let Some(m) = required_marker {
                    span { class: "text-amber-500/80 text-[8px]", title: "Required", "{m}" }
                }
                span { class: "truncate", title: prop.key.clone(), "{label}" }
            }
            div { class: "min-w-0",
                PropertyEditor {
                    prop: prop.clone(),
                    value,
                    on_change,
                }
            }
        }
    }
}

#[component]
fn PropertyEditor(
    prop: PropertyDef,
    value: JsonValue,
    on_change: Callback<(String, JsonValue)>,
) -> Element {
    let key = prop.key.clone();
    let editor_testid = format!("prop-editor-{}", prop.key);
    match prop.ty.clone() {
        PropertyType::Text | PropertyType::Link => {
            let initial = value.as_str().unwrap_or("").to_string();
            let key_for_input = key.clone();
            rsx! {
                input {
                    "data-testid": editor_testid,
                    r#type: "text",
                    class: "w-full rounded border border-border bg-background px-2 py-1",
                    value: "{initial}",
                    oninput: move |e| {
                        on_change.call((key_for_input.clone(), JsonValue::String(e.value())))
                    },
                }
            }
        }
        PropertyType::Multitext => {
            let initial = value.as_str().unwrap_or("").to_string();
            let key_for_input = key.clone();
            rsx! {
                textarea {
                    "data-testid": editor_testid,
                    class: "w-full rounded border border-border bg-background px-2 py-1",
                    rows: 3,
                    value: "{initial}",
                    oninput: move |e| {
                        on_change.call((key_for_input.clone(), JsonValue::String(e.value())))
                    },
                }
            }
        }
        PropertyType::Number => {
            let initial = value.as_f64().map(|f| f.to_string()).unwrap_or_default();
            let key_for_input = key.clone();
            rsx! {
                input {
                    "data-testid": editor_testid,
                    r#type: "number",
                    class: "w-full rounded border border-border bg-background px-2 py-1",
                    value: "{initial}",
                    oninput: move |e| {
                        let v = e.value();
                        let json = if v.is_empty() {
                            JsonValue::Null
                        } else {
                            match v.parse::<f64>() {
                                Ok(f) => serde_json::Number::from_f64(f)
                                    .map(JsonValue::Number)
                                    .unwrap_or(JsonValue::String(v)),
                                Err(_) => JsonValue::String(v),
                            }
                        };
                        on_change.call((key_for_input.clone(), json))
                    },
                }
            }
        }
        PropertyType::Checkbox => {
            let checked = value.as_bool().unwrap_or(false);
            let key_for_input = key.clone();
            rsx! {
                input {
                    "data-testid": editor_testid,
                    r#type: "checkbox",
                    checked,
                    onchange: move |_| {
                        on_change.call((key_for_input.clone(), JsonValue::Bool(!checked)))
                    },
                }
            }
        }
        PropertyType::Date | PropertyType::Datetime => {
            let initial = value.as_str().unwrap_or("").to_string();
            let key_for_input = key.clone();
            let input_type = if matches!(prop.ty, PropertyType::Datetime) {
                "datetime-local"
            } else {
                "date"
            };
            rsx! {
                input {
                    "data-testid": editor_testid,
                    r#type: input_type,
                    class: "rounded border border-border bg-background px-2 py-1",
                    value: "{initial}",
                    oninput: move |e| {
                        on_change.call((key_for_input.clone(), JsonValue::String(e.value())))
                    },
                }
            }
        }
        PropertyType::Tags | PropertyType::Aliases | PropertyType::LinkList => {
            let items: Vec<String> = match &value {
                JsonValue::Array(arr) => arr
                    .iter()
                    .filter_map(|v| v.as_str().map(|s| s.to_string()))
                    .collect(),
                JsonValue::String(s) => vec![s.clone()],
                _ => Vec::new(),
            };
            rsx! {
                ChipList {
                    field_key: key.clone(),
                    items,
                    on_change,
                    testid: editor_testid,
                }
            }
        }
        PropertyType::EnumWithMetadata { options } => {
            let current = value.as_str().unwrap_or("").to_string();
            let key_for_input = key.clone();
            rsx! {
                select {
                    "data-testid": editor_testid,
                    class: "rounded border border-border bg-background px-2 py-1",
                    onchange: move |e| {
                        on_change.call((key_for_input.clone(), JsonValue::String(e.value())))
                    },
                    for opt in options.iter() {
                        EnumOptionEl {
                            key: "{opt.value}",
                            opt: opt.clone(),
                            selected: opt.value == current,
                        }
                    }
                }
            }
        }
        PropertyType::Computed { formula } => rsx! {
            div { "data-testid": editor_testid,
                class: "text-muted-foreground italic",
                "computed: {formula}"
            }
        },
        PropertyType::Struct { .. } | PropertyType::StructList { .. } | PropertyType::Json => {
            // Read-only YAML-ish preview. Editor lands in 6.6.
            let preview = serde_json::to_string_pretty(&value).unwrap_or_default();
            rsx! {
                pre {
                    "data-testid": editor_testid,
                    class: "rounded border border-border bg-background px-2 py-1 text-xs overflow-x-auto",
                    "{preview}"
                }
            }
        }
        PropertyType::LexoRank => rsx! {},
    }
}

#[component]
fn EnumOptionEl(opt: EnumOption, selected: bool) -> Element {
    rsx! {
        option {
            value: "{opt.value}",
            selected,
            "{opt.label}"
        }
    }
}

#[component]
fn ChipList(
    field_key: String,
    items: Vec<String>,
    on_change: Callback<(String, JsonValue)>,
    testid: String,
) -> Element {
    let mut draft = use_signal(String::new);
    let add = {
        let items_for_add = items.clone();
        let key_for_add = field_key.clone();
        move |_: ()| {
            let v = draft.read().trim().to_string();
            if v.is_empty() {
                return;
            }
            let mut next = items_for_add.clone();
            if !next.iter().any(|x| x == &v) {
                next.push(v);
            }
            let json = JsonValue::Array(next.into_iter().map(JsonValue::String).collect());
            on_change.call((key_for_add.clone(), json));
            draft.set(String::new());
        }
    };
    let mut add_enter = add.clone();
    let mut add_blur = add;
    rsx! {
        div {
            "data-testid": testid,
            class: "flex flex-row flex-wrap gap-1 items-center",
            for (i, item) in items.iter().enumerate() {
                Chip {
                    key: "{i}",
                    label: item.clone(),
                    on_remove: {
                        let field_key = field_key.clone();
                        let items = items.clone();
                        let on_change = on_change;
                        let value = item.clone();
                        EventHandler::new(move |_| {
                            let next: Vec<JsonValue> = items
                                .iter()
                                .filter(|s| *s != &value)
                                .map(|s| JsonValue::String(s.clone()))
                                .collect();
                            on_change.call((field_key.clone(), JsonValue::Array(next)));
                        })
                    },
                }
            }
            input {
                r#type: "text",
                class: "rounded border border-border bg-background px-2 py-1 text-xs flex-1 min-w-[6rem]",
                value: "{draft}",
                placeholder: "+ add",
                oninput: move |e| draft.set(e.value()),
                onkeydown: move |e| {
                    if e.key() == Key::Enter { add_enter(()); }
                },
                onblur: move |_| add_blur(()),
            }
        }
    }
}

#[component]
fn Chip(label: String, on_remove: EventHandler<()>) -> Element {
    rsx! {
        span {
            class: "inline-flex items-center gap-1 rounded-full bg-muted px-2 py-0.5 text-xs",
            "{label}"
            button {
                class: "text-muted-foreground hover:text-foreground",
                onclick: move |_| on_remove.call(()),
                "×"
            }
        }
    }
}
