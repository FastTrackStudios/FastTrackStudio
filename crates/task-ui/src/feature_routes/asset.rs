//! Asset feature route. Holds a local `CrdtDoc` + `AssetRepoLoro`,
//! syncs over WebSocket, drives `asset-ui` dumb components.

use std::rc::Rc;

use architect::Page;
use asset_crdt::{AssetRepoLoro, CrdtDoc};
use asset_proto::{Asset, AssetCreate, AssetRepo};
use asset_ui::{AssetCreateForm, AssetList};
use dioxus::prelude::*;
use futures_channel::mpsc;
use futures_util::StreamExt;
use uuid::Uuid;
use wasm_bindgen_futures::spawn_local;

use crate::sync;

#[component]
pub fn AssetView() -> Element {
    crate::ui_log!("AssetView: component entry");
    let repo: Rc<AssetRepoLoro> = use_hook(|| {
        crate::ui_log!("AssetView: constructing CrdtDoc + AssetRepoLoro");
        let doc = CrdtDoc::ephemeral();
        Rc::new(AssetRepoLoro::new(&doc))
    });
    crate::ui_log!("AssetView: repo hook done");
    let doc: Rc<CrdtDoc> =
        use_hook(|| Rc::new(CrdtDoc::from_loro(repo.doc().clone())));
    crate::ui_log!("AssetView: doc hook done");

    let mut items = use_signal::<Vec<Asset>>(Vec::new);
    let mut status_msg = use_signal(|| "starting…".to_string());

    let refresh_tx: mpsc::UnboundedSender<()> = use_hook(|| {
        crate::ui_log!("AssetView: setting up refresh channel + drain task");
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
    crate::ui_log!("AssetView: refresh_tx hook done");

    let _session: Rc<Option<sync::SyncSession>> = use_hook({
        let doc = doc.clone();
        let tx_for_sync = refresh_tx.clone();
        move || {
            // Initial refresh — runs once on mount because we're now
            // inside the use_hook closure. Doing this outside the
            // closure made it fire every render, infinite-looping
            // through the drain task -> items.set -> re-render.
            let _ = tx_for_sync.unbounded_send(());
            crate::ui_log!("AssetView: connecting sync session");
            let ws_url = sync::sync_url(&format!("/sync/{}", sync::WORKSPACE_DOC_ID));
            let tx = tx_for_sync.clone();
            match sync::connect(&ws_url, &doc, move || {
                let _ = tx.unbounded_send(());
            }) {
                Ok(s) => {
                    crate::ui_log!("AssetView: sync::connect returned Ok");
                    status_msg.set(format!("connected to {ws_url}"));
                    Rc::new(Some(s))
                }
                Err(e) => {
                    crate::ui_log!("AssetView: sync::connect returned Err: {e:?}");
                    status_msg.set(format!("ws connect failed: {e:?}"));
                    Rc::new(None)
                }
            }
        }
    });
    crate::ui_log!("AssetView: session hook done, about to render");

    let on_submit = {
        let repo = repo.clone();
        let tx = refresh_tx.clone();
        move |payload: AssetCreate| {
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
            h1 { class: "text-3xl font-bold", "Assets (multiplayer)" }
            p { class: "text-xs text-slate-500", "{status_msg}" }

            AssetCreateForm { on_submit }
            AssetList { items: items(), on_delete }
        }
    }
}
