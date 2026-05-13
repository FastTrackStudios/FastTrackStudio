//! Bases table view — renders a `BaseResultSet` as a clickable grid.
//!
//! We deliberately avoid `fts-ui::DataTable` here: its API requires
//! heavyweight model wrappers we don't have for arbitrary JSON cell
//! values. A plain `Item`-per-row layout with a sticky header is
//! enough for v1; we can graduate to DataTable when sort/column-vis
//! interactions become more demanding.

use crate::bases::evaluator::{BaseResultSet, ColumnSpec, run_base};
use dioxus::prelude::*;
use fts_ui::prelude::*;
use knowledge_proto::Base;
use knowledge_proto::Page;
use knowledge_proto::bases::ParsedBase;
use serde_json::Value;
use uuid::Uuid;

#[component]
pub fn BaseTableView(
    base: Base,
    parsed: ParsedBase,
    pages: Vec<Page>,
    view_index: usize,
    on_open_page: EventHandler<Uuid>,
) -> Element {
    let sort_override = use_signal(|| None::<(String, bool)>); // (column key, asc)
    let rs = use_memo(use_reactive!(|(base, parsed, pages, view_index)| {
        run_base(&base, &parsed, &pages, view_index)
    }));
    let rs = rs.read().clone();

    // Apply optional sort override.
    let rs = match &*sort_override.read() {
        Some((key, asc)) => apply_sort(rs, key, *asc),
        None => rs,
    };

    if rs.columns.is_empty() {
        return rsx! {
            EmptyState {
                message: "This view has no columns configured.".to_string(),
            }
        };
    }

    let cols = rs.columns.clone();
    let rows = rs.rows.clone();
    let count = rows.len();

    rsx! {
        div { class: "flex flex-col gap-2",
            div { class: "text-xs text-muted-foreground px-1", "{count} rows" }
            div { class: "rounded-md border border-border overflow-hidden",
                // Header.
                div { class: "sticky top-0 z-10 grid bg-muted/50 border-b border-border text-xs font-medium",
                    style: format!("grid-template-columns: repeat({}, minmax(120px, 1fr));", cols.len()),
                    {cols.iter().map(|col| {
                        let key = col.key.clone();
                        let display = col.display.clone();
                        let mut sort_override = sort_override;
                        rsx! {
                            button {
                                key: "{key}",
                                class: "text-left px-3 py-2 hover:bg-muted/80 cursor-pointer",
                                onclick: move |_| {
                                    let mut s = sort_override.write();
                                    let next = match s.as_ref() {
                                        Some((k, asc)) if k == &key => Some((key.clone(), !asc)),
                                        _ => Some((key.clone(), true)),
                                    };
                                    *s = next;
                                },
                                "{display}"
                            }
                        }
                    })}
                }
                // Rows.
                div { class: "divide-y divide-border bg-card",
                    {rows.iter().map(|r| {
                        let page_id = r.page_id;
                        let cells = r.cells.clone();
                        let cols2 = cols.clone();
                        rsx! {
                            div {
                                key: "{page_id}",
                                class: "grid items-center hover:bg-muted/40 cursor-pointer text-sm",
                                style: format!("grid-template-columns: repeat({}, minmax(120px, 1fr));", cols2.len()),
                                onclick: move |_| on_open_page.call(page_id),
                                {cells.iter().enumerate().map(|(i, v)| {
                                    let key = cols2.get(i).map(|c| c.key.clone()).unwrap_or_default();
                                    rsx! {
                                        div { key: "{key}", class: "px-3 py-2 truncate",
                                            CellView { value: v.clone() }
                                        }
                                    }
                                })}
                            }
                        }
                    })}
                }
            }
        }
    }
}

#[component]
fn CellView(value: Value) -> Element {
    match value {
        Value::Null => rsx! { span { class: "text-muted-foreground", "—" } },
        Value::Bool(true) => rsx! {
            fts_ui::lucide_dioxus::CircleCheck { class: "h-4 w-4 text-primary" }
        },
        Value::Bool(false) => rsx! { span { class: "text-muted-foreground", "—" } },
        Value::Number(n) => rsx! {
            span { class: "tabular-nums text-right block", "{n}" }
        },
        Value::String(s) => render_string_cell(&s),
        Value::Array(arr) => rsx! {
            div { class: "flex flex-wrap gap-1",
                {arr.iter().map(|v| {
                    let label = match v {
                        Value::String(s) => s.clone(),
                        other => other.to_string(),
                    };
                    rsx! {
                        Badge {
                            variant: BadgeVariant::Secondary,
                            class: "text-[10px]".to_string(),
                            "{label}"
                        }
                    }
                })}
            }
        },
        Value::Object(_) => rsx! { span { class: "text-muted-foreground italic", "{{...}}" } },
    }
}

fn render_string_cell(s: &str) -> Element {
    if is_iso_date(s) {
        let formatted = format_iso_date(s);
        rsx! { span { "{formatted}" } }
    } else {
        let s = s.to_string();
        rsx! { span { "{s}" } }
    }
}

fn is_iso_date(s: &str) -> bool {
    let bytes = s.as_bytes();
    bytes.len() >= 10
        && bytes[0..4].iter().all(u8::is_ascii_digit)
        && bytes[4] == b'-'
        && bytes[5..7].iter().all(u8::is_ascii_digit)
        && bytes[7] == b'-'
        && bytes[8..10].iter().all(u8::is_ascii_digit)
}

fn format_iso_date(s: &str) -> String {
    // Accept YYYY-MM-DD prefix. If full RFC3339, parse and format
    // pretty; otherwise just slice the prefix.
    let date_part = &s[..10];
    if let Ok(d) = chrono::NaiveDate::parse_from_str(date_part, "%Y-%m-%d") {
        d.format("%b %e, %Y").to_string().trim().to_string()
    } else {
        date_part.to_string()
    }
}

fn apply_sort(rs: BaseResultSet, key: &str, asc: bool) -> BaseResultSet {
    let idx = match rs.columns.iter().position(|c| c.key == key) {
        Some(i) => i,
        None => return rs,
    };
    let BaseResultSet { columns, mut rows } = rs;
    rows.sort_by(|a, b| {
        let av = a.cells.get(idx).cloned().unwrap_or(Value::Null);
        let bv = b.cells.get(idx).cloned().unwrap_or(Value::Null);
        let ord = compare_json(&av, &bv);
        if asc { ord } else { ord.reverse() }
    });
    BaseResultSet { columns, rows }
}

fn compare_json(a: &Value, b: &Value) -> std::cmp::Ordering {
    use std::cmp::Ordering;
    match (a, b) {
        (Value::Number(x), Value::Number(y)) => x
            .as_f64()
            .unwrap_or(0.0)
            .partial_cmp(&y.as_f64().unwrap_or(0.0))
            .unwrap_or(Ordering::Equal),
        (Value::String(x), Value::String(y)) => x.cmp(y),
        (Value::Bool(x), Value::Bool(y)) => x.cmp(y),
        (Value::Null, Value::Null) => Ordering::Equal,
        (Value::Null, _) => Ordering::Less,
        (_, Value::Null) => Ordering::Greater,
        _ => format!("{a:?}").cmp(&format!("{b:?}")),
    }
}

// Silence unused warning for ColumnSpec while keeping it re-exported.
#[allow(dead_code)]
fn _column_spec_marker(_c: &ColumnSpec) {}
