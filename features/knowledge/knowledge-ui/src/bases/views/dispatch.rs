//! `BaseView` — parses the .base YAML on the fly and dispatches to
//! the right view kind component. v1 ships table view fully; the
//! other four render an EmptyState placeholder.

use crate::bases::views::table::BaseTableView;
use dioxus::prelude::*;
use fts_ui::prelude::*;
use knowledge_proto::Base;
use knowledge_proto::Page;
use knowledge_proto::bases::{ViewKind, parse};
use uuid::Uuid;

#[component]
pub fn BaseView(
    base: Base,
    pages: Vec<Page>,
    view_index: usize,
    on_open_page: EventHandler<Uuid>,
) -> Element {
    let yaml = base.definition_yaml.clone();
    let parsed = parse(&yaml);

    match parsed {
        Err(e) => {
            let msg = e.to_string();
            rsx! {
                Alert { variant: AlertVariant::Destructive,
                    AlertTitle { "Couldn't parse this Base" }
                    AlertDescription {
                        div { class: "flex flex-col gap-2",
                            div { "{msg}" }
                            pre { class: "text-[10px] whitespace-pre-wrap bg-muted/40 rounded p-2 max-h-48 overflow-auto",
                                "{yaml}"
                            }
                        }
                    }
                }
            }
        }
        Ok(parsed) => {
            let view = parsed.views.get(view_index);
            let kind = view.map(|v| v.kind);
            match kind {
                Some(ViewKind::Table) => rsx! {
                    BaseTableView {
                        base,
                        parsed,
                        pages,
                        view_index,
                        on_open_page,
                    }
                },
                Some(other) => {
                    let label = match other {
                        ViewKind::Board => "Board",
                        ViewKind::Gallery => "Gallery",
                        ViewKind::Calendar => "Calendar",
                        ViewKind::List => "List",
                        ViewKind::Table => "Table",
                    };
                    rsx! {
                        EmptyState {
                            message: format!("{label} view coming soon"),
                        }
                    }
                }
                None => rsx! {
                    EmptyState {
                        message: "This Base has no views.".to_string(),
                    }
                },
            }
        }
    }
}
