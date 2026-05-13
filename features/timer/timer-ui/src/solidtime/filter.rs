//! `TimerFilterBar` + `apply_filter` — toolbar above the entry table.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Search, X};
use fts_ui::prelude::*;
use invoice_proto::Client;
use project_proto::Project;
use timer_proto::TimeEntry;
use uuid::Uuid;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum TimerBillableFilter {
    #[default]
    All,
    Billable,
    NonBillable,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct TimerFilterState {
    pub project_ids: Vec<Uuid>,
    pub client_ids: Vec<Uuid>,
    pub billable: TimerBillableFilter,
    pub tags: Vec<String>,
    pub search: String,
}

impl TimerFilterState {
    pub fn is_empty(&self) -> bool {
        self.project_ids.is_empty()
            && self.client_ids.is_empty()
            && self.billable == TimerBillableFilter::All
            && self.tags.is_empty()
            && self.search.trim().is_empty()
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct TimerFilterBarProps {
    pub state: TimerFilterState,
    pub projects: Vec<Project>,
    pub clients: Vec<Client>,
    pub all_tags: Vec<String>,
    pub on_change: EventHandler<TimerFilterState>,
}

#[component]
pub fn TimerFilterBar(props: TimerFilterBarProps) -> Element {
    let state = props.state.clone();

    let billable_value = match state.billable {
        TimerBillableFilter::All => "all",
        TimerBillableFilter::Billable => "billable",
        TimerBillableFilter::NonBillable => "non-billable",
    }
    .to_string();

    let search_v = state.clone();
    let on_change_search = props.on_change;

    let proj_v = state.clone();
    let on_change_proj = props.on_change;

    let client_v = state.clone();
    let on_change_client = props.on_change;

    let tag_v = state.clone();
    let on_change_tag = props.on_change;

    let clear_v = state.clone();
    let on_change_clear = props.on_change;

    let billable_v = state.clone();
    let on_change_billable = props.on_change;

    rsx! {
        div { class: "flex flex-wrap items-center gap-2",

            // Search
            div { class: "flex items-center gap-2 rounded-md border bg-background px-2 py-1 min-w-60",
                Search { size: 14 }
                input {
                    class: "flex-1 bg-transparent text-sm outline-none placeholder:text-muted-foreground",
                    placeholder: "Search descriptions, tags…",
                    value: "{state.search}",
                    oninput: move |e| {
                        let mut v = search_v.clone();
                        v.search = e.value();
                        on_change_search.call(v);
                    },
                }
            }

            // Project multi-select
            ProjectMultiSelect {
                value: state.project_ids.clone(),
                projects: props.projects.clone(),
                on_change: move |ids: Vec<Uuid>| {
                    let mut v = proj_v.clone();
                    v.project_ids = ids;
                    on_change_proj.call(v);
                },
            }

            // Client multi-select
            ClientMultiSelect {
                value: state.client_ids.clone(),
                clients: props.clients.clone(),
                on_change: move |ids: Vec<Uuid>| {
                    let mut v = client_v.clone();
                    v.client_ids = ids;
                    on_change_client.call(v);
                },
            }

            // Billable segmented
            SegmentedControl {
                value: billable_value,
                on_change: move |v: String| {
                    let mut s = billable_v.clone();
                    s.billable = match v.as_str() {
                        "billable" => TimerBillableFilter::Billable,
                        "non-billable" => TimerBillableFilter::NonBillable,
                        _ => TimerBillableFilter::All,
                    };
                    on_change_billable.call(s);
                },
                options: vec![
                    ("all".to_string(), "All".to_string()),
                    ("billable".to_string(), "Billable".to_string()),
                    ("non-billable".to_string(), "Internal".to_string()),
                ],
                size: SegmentedControlSize::Small,
            }

            // Tag multi-select
            TagMultiSelect {
                value: state.tags.clone(),
                tags: props.all_tags.clone(),
                on_change: move |tags: Vec<String>| {
                    let mut v = tag_v.clone();
                    v.tags = tags;
                    on_change_tag.call(v);
                },
            }

            // Clear
            if !state.is_empty() {
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: move |_| {
                        let _ = &clear_v;
                        on_change_clear.call(TimerFilterState::default());
                    },
                    X { size: 12 }
                    " Clear"
                }
            }
        }
    }
}

#[component]
fn ProjectMultiSelect(
    value: Vec<Uuid>,
    projects: Vec<Project>,
    on_change: EventHandler<Vec<Uuid>>,
) -> Element {
    let count = value.len();
    let label = if count == 0 {
        "All projects".to_string()
    } else if count == 1 {
        projects
            .iter()
            .find(|p| p.id == value[0])
            .map(|p| p.name.clone())
            .unwrap_or_else(|| "1 project".to_string())
    } else {
        format!("{count} projects")
    };
    rsx! {
        Popover {
            PopoverTrigger {
                Button {
                    variant: ButtonVariant::Outline,
                    size: ButtonSize::Small,
                    "{label}"
                }
            }
            PopoverContent { class: "p-1 min-w-56 max-h-80 overflow-auto",
                button {
                    class: "w-full rounded px-2 py-1 text-left text-sm hover:bg-accent",
                    onclick: move |_| on_change.call(Vec::new()),
                    "All projects"
                }
                for p in projects.iter() {
                    {
                        let pid = p.id;
                        let on = value.contains(&pid);
                        let value_for_click = value.clone();
                        let pname = p.name.clone();
                        let color = p.color.clone().unwrap_or_else(|| "#888".into());
                        rsx! {
                            button {
                                key: "{pid}",
                                class: if on { "w-full rounded px-2 py-1 text-left text-sm bg-accent" } else { "w-full rounded px-2 py-1 text-left text-sm hover:bg-accent" },
                                onclick: move |_| {
                                    let mut next = value_for_click.clone();
                                    if let Some(pos) = next.iter().position(|x| *x == pid) {
                                        next.remove(pos);
                                    } else {
                                        next.push(pid);
                                    }
                                    on_change.call(next);
                                },
                                span { class: "inline-flex items-center gap-2",
                                    span {
                                        class: "inline-block size-2.5 rounded-full",
                                        style: format!("background: {}", color),
                                    }
                                    "{pname}"
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn ClientMultiSelect(
    value: Vec<Uuid>,
    clients: Vec<Client>,
    on_change: EventHandler<Vec<Uuid>>,
) -> Element {
    let count = value.len();
    let label = if count == 0 {
        "All clients".to_string()
    } else if count == 1 {
        clients
            .iter()
            .find(|c| c.id == value[0])
            .map(|c| c.name.clone())
            .unwrap_or_else(|| "1 client".to_string())
    } else {
        format!("{count} clients")
    };
    rsx! {
        Popover {
            PopoverTrigger {
                Button {
                    variant: ButtonVariant::Outline,
                    size: ButtonSize::Small,
                    "{label}"
                }
            }
            PopoverContent { class: "p-1 min-w-56 max-h-80 overflow-auto",
                button {
                    class: "w-full rounded px-2 py-1 text-left text-sm hover:bg-accent",
                    onclick: move |_| on_change.call(Vec::new()),
                    "All clients"
                }
                for c in clients.iter() {
                    {
                        let cid = c.id;
                        let on = value.contains(&cid);
                        let value_for_click = value.clone();
                        let cname = c.name.clone();
                        rsx! {
                            button {
                                key: "{cid}",
                                class: if on { "w-full rounded px-2 py-1 text-left text-sm bg-accent" } else { "w-full rounded px-2 py-1 text-left text-sm hover:bg-accent" },
                                onclick: move |_| {
                                    let mut next = value_for_click.clone();
                                    if let Some(pos) = next.iter().position(|x| *x == cid) {
                                        next.remove(pos);
                                    } else {
                                        next.push(cid);
                                    }
                                    on_change.call(next);
                                },
                                "{cname}"
                            }
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn TagMultiSelect(
    value: Vec<String>,
    tags: Vec<String>,
    on_change: EventHandler<Vec<String>>,
) -> Element {
    let count = value.len();
    let label = if count == 0 {
        "Tags".to_string()
    } else if count == 1 {
        value[0].clone()
    } else {
        format!("{count} tags")
    };
    rsx! {
        Popover {
            PopoverTrigger {
                Button {
                    variant: ButtonVariant::Outline,
                    size: ButtonSize::Small,
                    "{label}"
                }
            }
            PopoverContent { class: "p-1 min-w-44 max-h-80 overflow-auto",
                button {
                    class: "w-full rounded px-2 py-1 text-left text-sm hover:bg-accent",
                    onclick: move |_| on_change.call(Vec::new()),
                    "Clear tags"
                }
                for t in tags.iter() {
                    {
                        let tag = t.clone();
                        let on = value.contains(&tag);
                        let value_for_click = value.clone();
                        let tag_display = tag.clone();
                        rsx! {
                            button {
                                key: "{tag}",
                                class: if on { "w-full rounded px-2 py-1 text-left text-sm bg-accent" } else { "w-full rounded px-2 py-1 text-left text-sm hover:bg-accent" },
                                onclick: move |_| {
                                    let mut next = value_for_click.clone();
                                    if let Some(pos) = next.iter().position(|x| *x == tag) {
                                        next.remove(pos);
                                    } else {
                                        next.push(tag.clone());
                                    }
                                    on_change.call(next);
                                },
                                "{tag_display}"
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Pure filter — keeps the route from reimplementing this.
pub fn apply_filter(entries: &[TimeEntry], state: &TimerFilterState) -> Vec<TimeEntry> {
    let needle = state.search.trim().to_lowercase();
    entries
        .iter()
        .filter(|e| {
            if !state.project_ids.is_empty() {
                let Some(pid) = e.project_id else {
                    return false;
                };
                if !state.project_ids.contains(&pid) {
                    return false;
                }
            }
            if !state.client_ids.is_empty() {
                let Some(cid) = e.client_id else {
                    return false;
                };
                if !state.client_ids.contains(&cid) {
                    return false;
                }
            }
            match state.billable {
                TimerBillableFilter::All => {}
                TimerBillableFilter::Billable => {
                    if !e.billable {
                        return false;
                    }
                }
                TimerBillableFilter::NonBillable => {
                    if e.billable {
                        return false;
                    }
                }
            }
            if !state.tags.is_empty() && !state.tags.iter().any(|t| e.tags.contains(t)) {
                return false;
            }
            if !needle.is_empty() {
                let in_desc = e
                    .description
                    .as_deref()
                    .map(|s| s.to_lowercase().contains(&needle))
                    .unwrap_or(false);
                let in_tags = e.tags.iter().any(|t| t.to_lowercase().contains(&needle));
                if !in_desc && !in_tags {
                    return false;
                }
            }
            true
        })
        .cloned()
        .collect()
}
