//! `PropertiesPanel` — generic key/value editor for frontmatter (or
//! block-level `properties_json`).
//!
//! Dumb component: caller owns the (ordered) list of entries and gets a
//! mutated copy back through `on_change` every time the user commits an
//! edit. Order is preserved via the caller's `Vec` — we never re-sort
//! the entries.
//!
//! Inference rules (bool / number / string / date / list / object) live
//! in [`infer_kind`]. Special-cased keys (`aliases`, `tags`, …) get
//! domain UI on top of the inferred default.
//!
//! FUTURE: full nested-object editor (today the panel falls back to a
//! JSON textarea for objects). v1.5b will add a recursive form.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Plus, Trash2};
use fts_ui::prelude::*;
use serde_json::Value;

#[component]
pub fn PropertiesPanel(
    entries: Vec<(String, Value)>,
    on_change: EventHandler<Vec<(String, Value)>>,
    on_add: EventHandler<()>,
    editable: bool,
    /// Tag autocomplete pool for the special `tags` / `cssclasses` keys.
    #[props(default)]
    known_tags: Vec<String>,
) -> Element {
    let entries_state = use_signal(|| entries.clone());

    // If the caller's entries change, sync our local cache.
    use_effect(use_reactive!(|entries| {
        entries_state.clone().set(entries.clone());
    }));

    rsx! {
        div { class: "rounded-md border border-border bg-card",
            div { class: "grid grid-cols-[12rem_1fr_2rem] divide-y divide-border",
                for (idx, (key, value)) in entries_state.read().iter().cloned().enumerate() {
                    PropertyRow {
                        key: "{idx}-{key}",
                        idx,
                        prop_key: key,
                        value,
                        editable,
                        known_tags: known_tags.clone(),
                        entries: entries_state,
                        on_change,
                    }
                }
            }
            if editable {
                div { class: "border-t border-border p-2",
                    Button {
                        size: ButtonSize::Small,
                        variant: ButtonVariant::Ghost,
                        on_click: move |_| on_add.call(()),
                        Plus { size: 14 }
                        "Add property"
                    }
                }
            }
        }
    }
}

#[component]
fn PropertyRow(
    idx: usize,
    prop_key: String,
    value: Value,
    editable: bool,
    known_tags: Vec<String>,
    entries: Signal<Vec<(String, Value)>>,
    on_change: EventHandler<Vec<(String, Value)>>,
) -> Element {
    let key_for_lookup = prop_key.clone();
    let kind = infer_kind(&value, &key_for_lookup);

    let mut entries_sig = entries;
    let key_for_close = prop_key.clone();
    let emit = move |new_val: Value| {
        let mut cur = entries_sig.read().clone();
        if let Some(slot) = cur.get_mut(idx) {
            slot.1 = new_val;
        }
        entries_sig.set(cur.clone());
        on_change.call(cur);
    };

    let mut entries_sig2 = entries;
    let on_delete = move |_| {
        let mut cur = entries_sig2.read().clone();
        if idx < cur.len() {
            cur.remove(idx);
        }
        entries_sig2.set(cur.clone());
        on_change.call(cur);
    };

    rsx! {
        div { class: "contents",
            div { class: "flex items-center px-2 py-1.5 text-xs font-mono text-muted-foreground border-r border-border bg-muted/30 truncate",
                "{key_for_close}"
            }
            div { class: "flex items-center px-2 py-1",
                {render_value_editor(kind, &value, &known_tags, editable, emit)}
            }
            div { class: "flex items-center justify-center",
                if editable {
                    button {
                        class: "p-1 text-muted-foreground hover:text-destructive",
                        title: "Delete",
                        onclick: on_delete,
                        Trash2 { size: 12 }
                    }
                }
            }
        }
    }
}

// ── Type inference + editors ───────────────────────────────────────────

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ValueKind {
    Null,
    Bool,
    Number,
    /// YYYY-MM-DD shaped string.
    Date,
    String,
    /// `Vec<Value>` where every element is a string.
    StringList,
    /// `Vec<Value>` mixed or non-string.
    List,
    Object,
    /// `aliases: list[str]` — chip editor.
    Aliases,
    /// `tags: str | list[str]` — tag chip editor with autocomplete.
    Tags,
    /// `cssclasses: str | list[str]` — chip editor.
    CssClasses,
    /// `publish: bool` — switch with tooltip.
    Publish,
}

fn infer_kind(value: &Value, key: &str) -> ValueKind {
    match key {
        "aliases" => return ValueKind::Aliases,
        "tags" => return ValueKind::Tags,
        "cssclasses" => return ValueKind::CssClasses,
        "publish" => return ValueKind::Publish,
        _ => {}
    }
    match value {
        Value::Null => ValueKind::Null,
        Value::Bool(_) => ValueKind::Bool,
        Value::Number(_) => ValueKind::Number,
        Value::String(s) => {
            if looks_like_date(s) {
                ValueKind::Date
            } else {
                ValueKind::String
            }
        }
        Value::Array(arr) => {
            if arr.iter().all(|v| v.is_string()) {
                ValueKind::StringList
            } else {
                ValueKind::List
            }
        }
        Value::Object(_) => ValueKind::Object,
    }
}

fn looks_like_date(s: &str) -> bool {
    s.len() == 10
        && s.as_bytes().get(4) == Some(&b'-')
        && s.as_bytes().get(7) == Some(&b'-')
        && s.chars().all(|c| c.is_ascii_digit() || c == '-')
}

fn render_value_editor<F>(
    kind: ValueKind,
    value: &Value,
    known_tags: &[String],
    editable: bool,
    emit: F,
) -> Element
where
    F: FnMut(Value) + 'static + Clone,
{
    match kind {
        ValueKind::Publish | ValueKind::Bool => render_bool(value, editable, emit),
        ValueKind::Number => render_number(value, editable, emit),
        ValueKind::Date => render_date(value, editable, emit),
        ValueKind::String => render_string(value, editable, emit),
        ValueKind::Aliases | ValueKind::CssClasses => render_chip_list(value, editable, emit),
        ValueKind::Tags => render_tag_chips(value, known_tags, editable, emit),
        ValueKind::StringList => render_chip_list(value, editable, emit),
        ValueKind::List | ValueKind::Object => render_json_textarea(value, editable, emit),
        ValueKind::Null => rsx! { span { class: "text-xs text-muted-foreground italic", "empty" } },
    }
}

fn render_bool<F: FnMut(Value) + 'static + Clone>(
    value: &Value,
    editable: bool,
    mut emit: F,
) -> Element {
    let on = value.as_bool().unwrap_or(false);
    let checked_sig = use_signal(|| on);
    let _ = editable;
    rsx! {
        Switch {
            checked: checked_sig,
            on_change: move |b| emit(Value::Bool(b)),
        }
    }
}

fn render_number<F: FnMut(Value) + 'static + Clone>(
    value: &Value,
    editable: bool,
    mut emit: F,
) -> Element {
    let s = value
        .as_f64()
        .map(|n| n.to_string())
        .unwrap_or_else(|| value.to_string());
    rsx! {
        input {
            r#type: "number",
            class: "w-full bg-transparent text-sm outline-none",
            value: "{s}",
            disabled: !editable,
            oninput: move |ev| {
                if let Ok(n) = ev.value().parse::<f64>() {
                    if let Some(num) = serde_json::Number::from_f64(n) {
                        emit(Value::Number(num));
                    }
                }
            },
        }
    }
}

fn render_date<F: FnMut(Value) + 'static + Clone>(
    value: &Value,
    editable: bool,
    mut emit: F,
) -> Element {
    let s = value.as_str().unwrap_or("").to_string();
    rsx! {
        input {
            r#type: "date",
            class: "w-full bg-transparent text-sm outline-none",
            value: "{s}",
            disabled: !editable,
            oninput: move |ev| emit(Value::String(ev.value())),
        }
    }
}

fn render_string<F: FnMut(Value) + 'static + Clone>(
    value: &Value,
    editable: bool,
    mut emit: F,
) -> Element {
    let s = value.as_str().unwrap_or("").to_string();
    rsx! {
        input {
            r#type: "text",
            class: "w-full bg-transparent text-sm outline-none",
            value: "{s}",
            disabled: !editable,
            oninput: move |ev| emit(Value::String(ev.value())),
        }
    }
}

fn render_chip_list<F: FnMut(Value) + 'static + Clone>(
    value: &Value,
    editable: bool,
    mut emit: F,
) -> Element {
    let items = value_as_string_vec(value);
    let mut chips = use_signal(|| items.clone());
    let mut draft = use_signal(String::new);

    rsx! {
        div { class: "flex flex-wrap items-center gap-1 w-full",
            for (i, chip) in chips.read().iter().cloned().enumerate() {
                Badge {
                    variant: BadgeVariant::Secondary,
                    class: "text-xs gap-1".to_string(),
                    span { "{chip}" }
                    if editable {
                        button {
                            class: "text-muted-foreground hover:text-destructive",
                            onclick: {
                                let mut emit2 = emit.clone();
                                move |_| {
                                    let mut cur = chips.read().clone();
                                    cur.remove(i);
                                    chips.set(cur.clone());
                                    emit2(strings_to_value(&cur));
                                }
                            },
                            "×"
                        }
                    }
                }
            }
            if editable {
                input {
                    r#type: "text",
                    class: "flex-1 min-w-[6rem] bg-transparent text-sm outline-none",
                    placeholder: "+ add",
                    value: "{draft}",
                    oninput: move |ev| draft.set(ev.value()),
                    onkeydown: move |ev| {
                        if ev.key() == Key::Enter {
                            let v = draft.read().trim().to_string();
                            if !v.is_empty() {
                                let mut cur = chips.read().clone();
                                cur.push(v);
                                chips.set(cur.clone());
                                draft.set(String::new());
                                emit(strings_to_value(&cur));
                            }
                        }
                    },
                }
            }
        }
    }
}

fn render_tag_chips<F: FnMut(Value) + 'static + Clone>(
    value: &Value,
    known_tags: &[String],
    editable: bool,
    emit: F,
) -> Element {
    // Same renderer as chips for now; surfacing autocomplete needs a
    // popover wired to `draft` — postponed to v1.5b.
    let _ = known_tags;
    render_chip_list(value, editable, emit)
}

fn render_json_textarea<F: FnMut(Value) + 'static + Clone>(
    value: &Value,
    editable: bool,
    mut emit: F,
) -> Element {
    let s = serde_json::to_string_pretty(value).unwrap_or_else(|_| "{}".to_string());
    rsx! {
        textarea {
            class: "w-full bg-transparent font-mono text-xs outline-none resize-y min-h-[3rem]",
            disabled: !editable,
            placeholder: "JSON",
            value: "{s}",
            oninput: move |ev| {
                if let Ok(v) = serde_json::from_str::<Value>(&ev.value()) {
                    emit(v);
                }
            },
        }
    }
}

fn value_as_string_vec(value: &Value) -> Vec<String> {
    match value {
        Value::String(s) => vec![s.clone()],
        Value::Array(arr) => arr
            .iter()
            .filter_map(|v| v.as_str().map(str::to_string))
            .collect(),
        _ => Vec::new(),
    }
}

fn strings_to_value(items: &[String]) -> Value {
    Value::Array(items.iter().map(|s| Value::String(s.clone())).collect())
}
