//! CalendarEvent feature route. Holds a local `CrdtDoc` + `CalendarEventRepoLoro`,
//! syncs over WebSocket, drives `calendar-ui` dumb components.

use std::rc::Rc;

use architect::Page;
use calendar_crdt::{CalendarEventRepoLoro, CrdtDoc};
use calendar_proto::{CalendarEvent, CalendarEventCreate, CalendarEventRepo, CalendarEventUpdate};
use calendar_ui::{CalendarEventDashboard, InteractiveCalendar};
use dioxus::prelude::*;
use fts_ui::prelude::*;
use futures_channel::mpsc;
use futures_util::StreamExt;
use uuid::Uuid;
use wasm_bindgen_futures::spawn_local;

use crate::sync;

#[component]
pub fn CalendarEventView() -> Element {
    let repo: Rc<CalendarEventRepoLoro> = use_hook(|| {
        let doc = CrdtDoc::ephemeral();
        Rc::new(CalendarEventRepoLoro::new(&doc))
    });
    let doc: Rc<CrdtDoc> = use_hook(|| Rc::new(CrdtDoc::from_loro(repo.doc().clone())));

    let mut items = use_signal::<Vec<CalendarEvent>>(Vec::new);
    let mut status_msg = use_signal(|| "starting…".to_string());

    let refresh_tx: mpsc::UnboundedSender<()> = use_hook(|| {
        let (tx, mut rx) = mpsc::unbounded::<()>();
        let repo_for_loop = repo.clone();
        spawn_local(async move {
            while rx.next().await.is_some() {
                if let Ok(list) = repo_for_loop
                    .list(
                        Page {
                            index: 0,
                            size: 200,
                        },
                        None,
                        None,
                    )
                    .await
                {
                    items.set(list.items);
                }
            }
        });
        tx
    });

    let _session: Rc<Option<sync::SyncSession>> = use_hook({
        let doc = doc.clone();
        let tx_for_sync = refresh_tx.clone();
        move || {
            let _ = tx_for_sync.unbounded_send(());
            let ws_url = sync::sync_url(&format!("/sync/{}", sync::WORKSPACE_DOC_ID));
            let tx = tx_for_sync.clone();
            match sync::connect(&ws_url, &doc, move || {
                let _ = tx.unbounded_send(());
            }) {
                Ok(s) => {
                    status_msg.set(format!("connected to {ws_url}"));
                    Rc::new(Some(s))
                }
                Err(e) => {
                    status_msg.set(format!("ws connect failed: {e:?}"));
                    Rc::new(None)
                }
            }
        }
    });

    let on_submit = {
        let repo = repo.clone();
        let tx = refresh_tx.clone();
        move |payload: CalendarEventCreate| {
            let repo = repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let _ = repo.create(payload).await;
                let _ = tx.unbounded_send(());
            });
        }
    };

    let on_delete = {
        let repo = repo.clone();
        let tx = refresh_tx.clone();
        move |id: Uuid| {
            let repo = repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let _ = repo.delete(id).await;
                let _ = tx.unbounded_send(());
            });
        }
    };

    let on_update = {
        let repo = repo.clone();
        let tx = refresh_tx.clone();
        move |(id, update): (Uuid, CalendarEventUpdate)| {
            let repo = repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let _ = repo.update(id, update).await;
                let _ = tx.unbounded_send(());
            });
        }
    };

    let dashboard_create = on_submit.clone();
    let dashboard_delete = on_delete.clone();

    rsx! {
        div { class: "mx-auto flex max-w-6xl flex-col gap-4 p-6 lg:p-10",
            Tabs {
                default_value: "calendar".to_string(),
                TabList {
                    TabTrigger { value: "calendar".to_string(), index: 0usize, "Calendar" }
                    TabTrigger { value: "dashboard".to_string(), index: 1usize, "Dashboard" }
                }
                TabContent { value: "calendar".to_string(), index: 0usize,
                    InteractiveCalendar {
                        events: items(),
                        initial_view: None,
                        initial_date: None,
                        editable: true,
                        on_create: on_submit,
                        on_update,
                        on_delete,
                    }
                }
                TabContent { value: "dashboard".to_string(), index: 1usize,
                    CalendarEventDashboard {
                        items: items(),
                        status: status_msg(),
                        on_create: dashboard_create,
                        on_delete: dashboard_delete,
                    }
                }
            }
        }
    }
}
