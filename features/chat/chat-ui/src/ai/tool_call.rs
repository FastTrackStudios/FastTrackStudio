//! `ToolCallCard` — visual for one assistant tool invocation +
//! optional result. Parsing helper (`parse_tool_calls`) is best-effort
//! over the opaque `Message.tool_calls_json` blob.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::Wrench;
use fts_ui::prelude::*;

#[derive(Clone, Debug, PartialEq)]
pub enum ToolCallStatus {
    Pending,
    Running,
    Done,
    Error(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ToolCallView {
    pub id: String,
    pub name: String,
    pub arguments_json: String,
    pub result_json: Option<String>,
    pub status: ToolCallStatus,
}

/// Best-effort decoder over the opaque `Message.tool_calls_json`
/// payload. Expected shape:
/// `[{"id":"...","name":"...","arguments":"...","result":"...","status":"done|running|pending|error","error":"..."}]`.
///
/// Returns an empty Vec on parse failure or `None` — never errors. The
/// UI's job is to render what exists, not validate the wire format.
pub fn parse_tool_calls(tool_calls_json: Option<&str>) -> Vec<ToolCallView> {
    let Some(raw) = tool_calls_json else {
        return Vec::new();
    };
    let raw = raw.trim();
    if raw.is_empty() {
        return Vec::new();
    }
    let Ok(value) = serde_json::from_str::<serde_json::Value>(raw) else {
        return Vec::new();
    };
    let Some(arr) = value.as_array() else {
        return Vec::new();
    };
    arr.iter()
        .filter_map(|item| {
            let obj = item.as_object()?;
            let id = obj
                .get("id")
                .and_then(|v| v.as_str())
                .unwrap_or("")
                .to_string();
            let name = obj
                .get("name")
                .and_then(|v| v.as_str())
                .unwrap_or("(tool)")
                .to_string();
            let arguments_json = stringify_field(obj.get("arguments"));
            let result_field = obj.get("result");
            let result_json = match result_field {
                None | Some(serde_json::Value::Null) => None,
                Some(other) => Some(stringify_value(other)),
            };
            let status_str = obj.get("status").and_then(|v| v.as_str()).unwrap_or("done");
            let status = match status_str {
                "pending" => ToolCallStatus::Pending,
                "running" => ToolCallStatus::Running,
                "error" | "failed" => {
                    let msg = obj
                        .get("error")
                        .and_then(|v| v.as_str())
                        .unwrap_or("error")
                        .to_string();
                    ToolCallStatus::Error(msg)
                }
                _ => ToolCallStatus::Done,
            };
            Some(ToolCallView {
                id,
                name,
                arguments_json,
                result_json,
                status,
            })
        })
        .collect()
}

fn stringify_field(v: Option<&serde_json::Value>) -> String {
    match v {
        None | Some(serde_json::Value::Null) => String::new(),
        Some(other) => stringify_value(other),
    }
}

fn stringify_value(v: &serde_json::Value) -> String {
    match v {
        serde_json::Value::String(s) => s.clone(),
        other => serde_json::to_string_pretty(other).unwrap_or_else(|_| other.to_string()),
    }
}

const TRUNCATE_THRESHOLD: usize = 8 * 1024;

#[component]
pub fn ToolCallCard(call: ToolCallView) -> Element {
    let (dot_color, status_label) = status_meta(&call.status);
    let result_default_open = matches!(call.status, ToolCallStatus::Error(_));
    let result_present = call.result_json.is_some();

    let result_full = call.result_json.clone().unwrap_or_default();
    let mut show_full = use_signal(|| false);
    let truncated_pre = result_full.len() > TRUNCATE_THRESHOLD;
    let displayed_result = if truncated_pre && !*show_full.read() {
        format!("{}…", &result_full[..TRUNCATE_THRESHOLD])
    } else {
        result_full.clone()
    };

    rsx! {
        Card { class: "my-2 border-border/60",
            CardHeader { class: "flex flex-row items-center gap-2 py-2",
                StatusDot { color: dot_color, size: StatusDotSize::Medium }
                Badge { variant: BadgeVariant::Secondary,
                    span { class: "inline-flex items-center gap-1",
                        Wrench { size: 12 }
                        "{call.name}"
                    }
                }
                Spacer {}
                Text { variant: TextVariant::Muted, class: "text-xs", "{status_label}" }
            }
            CardContent { class: "flex flex-col gap-2 pt-0 pb-3",
                Collapsible {
                    CollapsibleTrigger {
                        class: "text-xs text-muted-foreground hover:text-foreground".to_string(),
                        "Arguments"
                    }
                    CollapsibleContent { class: "mt-1".to_string(),
                        pre { class: "rounded-md bg-muted p-2 text-xs font-mono whitespace-pre-wrap break-all",
                            "{call.arguments_json}"
                        }
                    }
                }
                if result_present {
                    Collapsible { default_open: result_default_open,
                        CollapsibleTrigger {
                            class: "text-xs text-muted-foreground hover:text-foreground".to_string(),
                            "Result"
                        }
                        CollapsibleContent { class: "mt-1 flex flex-col gap-1".to_string(),
                            pre { class: "rounded-md bg-muted p-2 text-xs font-mono whitespace-pre-wrap break-all",
                                "{displayed_result}"
                            }
                            if truncated_pre {
                                Button {
                                    variant: ButtonVariant::Ghost,
                                    size: ButtonSize::Small,
                                    on_click: move |_| {
                                        let cur = *show_full.read();
                                        show_full.set(!cur);
                                    },
                                    if *show_full.read() { "Collapse" } else { "(truncated — show full)" }
                                }
                            }
                        }
                    }
                }
                if let ToolCallStatus::Error(msg) = &call.status {
                    Text { variant: TextVariant::Muted, class: "text-xs text-rose-400", "{msg}" }
                }
            }
        }
    }
}

fn status_meta(status: &ToolCallStatus) -> (StatusDotColor, &'static str) {
    match status {
        ToolCallStatus::Pending => (StatusDotColor::Neutral, "pending"),
        ToolCallStatus::Running => (StatusDotColor::Warning, "running"),
        ToolCallStatus::Done => (StatusDotColor::Success, "done"),
        ToolCallStatus::Error(_) => (StatusDotColor::Danger, "error"),
    }
}
