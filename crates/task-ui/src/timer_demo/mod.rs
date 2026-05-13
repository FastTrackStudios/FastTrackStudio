//! `/timer-demo` route — a minimal Loro multiplayer demo.
//!
//! Both browser tabs hit the same hardcoded `DOC_ID`. The server
//! relays Loro update bytes between them. Each tab owns a local
//! `CrdtDoc` + `TimeEntryRepoLoro` — the Repo trait is the same
//! shape as the native tests use, just over a local LoroDoc instead
//! of a server-side SeaORM connection.

use crate::sync;

use std::cell::RefCell;
use std::rc::Rc;

use chrono::Utc;
use crdt::CrdtDoc;
use dioxus::prelude::*;
use futures_channel::mpsc;
use futures_util::StreamExt;
use timer_crdt::TimeEntryRepoLoro;
use timer_proto::{TimeEntry, TimeEntryCreate, TimeEntryRepo, TimeEntryUpdate};
use uuid::{uuid, Uuid};
use wasm_bindgen_futures::spawn_local;

/// Hardcoded "workspace" UUID so every browser pointed at the demo
/// joins the same sync room. Change this and you get a fresh empty
/// doc; same value → same shared state across tabs / browsers.
const DEMO_DOC_ID: Uuid = uuid!("00000000-0000-0000-0000-000000000001");

#[component]
pub fn TimerDemo() -> Element {
    // We need stable handles across re-renders. `use_hook` runs once
    // per component mount and gives us the same `Rc`s on every
    // render — Dioxus signals trigger re-renders without rebuilding.
    let repo: Rc<TimeEntryRepoLoro> = use_hook(|| {
        let doc = CrdtDoc::ephemeral();
        Rc::new(TimeEntryRepoLoro::new(&doc))
    });

    // Cache the doc handle the sync layer borrows.
    let doc: Rc<CrdtDoc> = use_hook(|| {
        // CrdtDoc::ephemeral() is cheap, but we want the SAME doc as
        // the repo. Pull it from the repo's underlying handle.
        Rc::new(CrdtDoc::from_loro(repo_doc_clone(&repo)))
    });

    // Signals
    let mut entries = use_signal::<Vec<TimeEntry>>(Vec::new);
    let mut description_input = use_signal(String::new);
    let mut status_msg = use_signal(|| "starting…".to_string());

    // Re-render bridge. Loro's `subscribe_root` requires a Send+Sync
    // closure (its dyn-Fn bound), and Dioxus `Signal` isn't Send+Sync
    // — so we hand the sync layer an `UnboundedSender<()>` (which IS
    // Send+Sync) and run a coroutine that drains it and refreshes the
    // `entries` signal off the wasm main thread. Same trick the
    // browser's event loop uses internally.
    let refresh_tx: mpsc::UnboundedSender<()> = use_hook(|| {
        let (tx, rx) = mpsc::unbounded::<()>();
        // Stash the receiver so a sibling `use_hook` can pick it up.
        // `Rc<RefCell>` is fine — we're single-threaded on wasm and
        // the receiver moves into a spawned task exactly once.
        let cell: Rc<RefCell<Option<mpsc::UnboundedReceiver<()>>>> =
            Rc::new(RefCell::new(Some(rx)));
        // Store the cell as a side channel via use_context or a
        // local hook. Simpler: stash it on the Rc itself by tying
        // its lifetime to the sender below — we use a second hook
        // that takes from the cell.
        let cell_clone = cell.clone();
        // Spawn the refresh loop once, then forget the cell.
        let repo_clone = repo.clone();
        spawn_local(async move {
            let mut rx = cell_clone
                .borrow_mut()
                .take()
                .expect("refresh receiver taken twice");
            while rx.next().await.is_some() {
                if let Ok(list) = repo_clone
                    .list(
                        architect::Page { index: 0, size: 200 },
                        Some(architect::Sort {
                            field: "start_time".into(),
                            order: architect::SortOrder::Desc,
                        }),
                        None,
                    )
                    .await
                {
                    entries.set(list.items);
                }
            }
        });
        tx
    });

    // Connect to the sync server once. Wrapped in `Rc` because
    // `SyncSession` isn't `Clone` (Loro subscriptions aren't), but
    // `use_hook` requires its return type to be `Clone`.
    let _session: Rc<Option<sync::SyncSession>> = use_hook({
        let doc = doc.clone();
        let tx_for_sync = refresh_tx.clone();
        // Trigger an initial refresh as soon as the component mounts
        // — covers the case where the snapshot from the server lands
        // before our subscription fires.
        let _ = tx_for_sync.unbounded_send(());
        move || {
            let ws_url = sync::sync_url(&format!("/sync/{DEMO_DOC_ID}"));
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

    // Find the currently-running entry for this peer's user (if any).
    // For the demo we don't track per-user — first running entry wins.
    let running_id: Option<Uuid> = entries
        .read()
        .iter()
        .find(|e| e.end_time.is_none())
        .map(|e| e.id);

    rsx! {
        div { class: "mx-auto flex max-w-3xl flex-col gap-4 p-6 lg:p-10",
            h1 { class: "text-3xl font-bold", "Timer demo (multiplayer)" }
            p { class: "text-sm text-slate-400",
                "Open this page in two browsers — every change syncs in real time. "
                "Doc ID: "
                code { class: "text-slate-300", "{DEMO_DOC_ID}" }
            }
            p { class: "text-xs text-slate-500", "{status_msg}" }

            // Composer
            div { class: "flex gap-2",
                input {
                    class: "flex-1 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                    placeholder: "What are you working on?",
                    value: "{description_input}",
                    oninput: move |evt| description_input.set(evt.value()),
                }
                if let Some(rid) = running_id {
                    button {
                        class: "rounded-md bg-rose-500 px-4 py-2 text-sm font-semibold text-white hover:bg-rose-400",
                        onclick: {
                            let repo = repo.clone();
                            let tx = refresh_tx.clone();
                            move |_| {
                                let repo = repo.clone();
                                let tx = tx.clone();
                                spawn_local(async move {
                                    let _ = repo
                                        .update(
                                            rid,
                                            TimeEntryUpdate {
                                                end_time: Some(Some(Utc::now())),
                                                ..Default::default()
                                            },
                                        )
                                        .await;
                                    let _ = tx.unbounded_send(());
                                });
                            }
                        },
                        "Stop timer"
                    }
                } else {
                    button {
                        class: "rounded-md bg-cyan-500 px-4 py-2 text-sm font-semibold text-slate-950 hover:bg-cyan-400",
                        onclick: {
                            let repo = repo.clone();
                            let tx = refresh_tx.clone();
                            move |_| {
                                let repo = repo.clone();
                                let tx = tx.clone();
                                let desc = description_input.read().clone();
                                description_input.set(String::new());
                                spawn_local(async move {
                                    let now = Utc::now();
                                    let _ = repo
                                        .create(TimeEntryCreate {
                                            task_id: None,
                                            user: peer_label(),
                                            start_time: now,
                                            end_time: None,
                                            description: if desc.is_empty() { None } else { Some(desc) },
                                            billable: false,
                                            billable_rate_cents: None,
                                            tags: Vec::new(),
                                            invoiced_at: None,
                                        })
                                        .await;
                                    let _ = tx.unbounded_send(());
                                });
                            }
                        },
                        "Start timer"
                    }
                }
            }

            // Entries — inlined (no sub-component) because the
            // `Rc<TimeEntryRepoLoro>` we hand to delete handlers isn't
            // a valid Dioxus Props type (no PartialEq via foreign
            // trait). Cheap enough to render in-place for a demo.
            div { class: "flex flex-col gap-2",
                if entries.read().is_empty() {
                    p { class: "text-slate-500", "No entries yet. Start a timer to see this sync between browsers." }
                }
                for entry in entries.read().iter().cloned() {
                    {
                        let elapsed = format_duration(&entry);
                        let running = entry.end_time.is_none();
                        let id = entry.id;
                        let desc = entry.description.clone().unwrap_or_else(|| "(no description)".into());
                        let user = entry.user.clone().unwrap_or_else(|| "unknown".into());
                        let repo = repo.clone();
                        let tx = refresh_tx.clone();
                        rsx! {
                            div {
                                key: "{id}",
                                class: "flex items-center justify-between rounded-md border border-slate-800 bg-slate-900 px-4 py-3",
                                div { class: "flex flex-col",
                                    span { class: "text-sm font-medium text-slate-100", "{desc}" }
                                    span { class: "text-xs text-slate-500",
                                        "{user} · {elapsed}"
                                        if running { " · running" }
                                    }
                                }
                                button {
                                    class: "text-xs text-slate-500 hover:text-rose-400",
                                    onclick: move |_| {
                                        let repo = repo.clone();
                                        let tx = tx.clone();
                                        spawn_local(async move {
                                            let _ = repo.delete(id).await;
                                            let _ = tx.unbounded_send(());
                                        });
                                    },
                                    "Delete"
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

// ── Helpers ───────────────────────────────────────────────────────────

/// Borrow the LoroDoc out of a fresh repo so the SyncSession and Repo
/// share the same doc. `LoroDoc` is `Arc`-shared internally, so
/// cloning it is cheap and the two handles see each other's writes.
fn repo_doc_clone(repo: &TimeEntryRepoLoro) -> loro::LoroDoc {
    // `LoroDoc::Clone` returns a cheap clone of the Arc-shared
    // internal state.
    repo.doc().clone()
}

/// Cheap "who's this browser" tag. Reads navigator.userAgent and
/// trims to something short. The demo treats this as a fake user id.
fn peer_label() -> Option<String> {
    let win = web_sys::window()?;
    let nav = win.navigator();
    let ua = nav.user_agent().ok()?;
    // Just take the first browser-ish token.
    let short = ua
        .split_whitespace()
        .next()
        .unwrap_or("anon")
        .to_string();
    Some(short)
}

fn format_duration(entry: &TimeEntry) -> String {
    let end = entry.end_time.unwrap_or_else(Utc::now);
    let d = end - entry.start_time;
    let total_secs = d.num_seconds().max(0);
    let mins = total_secs / 60;
    let secs = total_secs % 60;
    if mins == 0 {
        format!("{secs}s")
    } else {
        format!("{mins}m {secs}s")
    }
}
