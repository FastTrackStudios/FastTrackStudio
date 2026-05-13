//! Revenue feature route. Holds a local `CrdtDoc` + `RevenueRepoLoro`,
//! syncs over WebSocket, drives `finance-ui` dumb components.

use std::rc::Rc;

use architect::Page;
use finance_crdt::{CrdtDoc, RevenueRepoLoro};
use finance_proto::{Revenue, RevenueCreate, RevenueRepo};
use finance_ui::{RevenueCreateForm, RevenueList};
use dioxus::prelude::*;
use futures_channel::mpsc;
use futures_util::StreamExt;
use uuid::Uuid;
use wasm_bindgen_futures::spawn_local;

use crate::sync;

#[component]
pub fn RevenueView() -> Element {
    let repo: Rc<RevenueRepoLoro> = use_hook(|| {
        let doc = CrdtDoc::ephemeral();
        Rc::new(RevenueRepoLoro::new(&doc))
    });
    let doc: Rc<CrdtDoc> =
        use_hook(|| Rc::new(CrdtDoc::from_loro(repo.doc().clone())));

    let mut items = use_signal::<Vec<Revenue>>(Vec::new);
    let mut status_msg = use_signal(|| "starting…".to_string());

    let refresh_tx: mpsc::UnboundedSender<()> = use_hook(|| {
        let (tx, mut rx) = mpsc::unbounded::<()>();
        let repo_for_loop = repo.clone();
        spawn_local(async move {
            while rx.next().await.is_some() {
                if let Ok(list) = repo_for_loop
                    .list(Page { index: 0, size: 200 }, None, None)
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
        let _ = tx_for_sync.unbounded_send(());
        move || {
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
        move |payload: RevenueCreate| {
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

    rsx! {
        div { class: "mx-auto flex max-w-3xl flex-col gap-4 p-6 lg:p-10",
            h1 { class: "text-3xl font-bold", "Revenue (multiplayer)" }
            p { class: "text-xs text-slate-500", "{status_msg}" }

            RevenueCreateForm { on_submit }
            RevenueList { items: items(), on_delete }
        }
    }
}
