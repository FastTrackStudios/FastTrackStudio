//! Stage 4a — the in-**browser** session engine.
//!
//! The wasm sibling of `apps/fasttrackstudio/src/session_engine.rs`: it
//! builds a headless `daw-standalone` setlist player entirely inside the
//! browser tab — no server, no REAPER, no cpal — so the setlist becomes
//! DATA the page can eventually PLAY. This is the payoff of the architect
//! framework work in this branch: `architect::LocalServer` +
//! `Services::into_router()` now compile for wasm, so the `!Send`
//! single-threaded `Standalone` backend can host its own RPC in-process
//! over architect's in-memory `Link`.
//!
//! Construction mirrors the native engine's `bootstrap()`, trimmed to the
//! browser reality:
//!
//! 1. `Standalone::new()` + one `seed_project` per demo song +
//!    `stamp_song_with_default_tempo_native` — a playable demo setlist as
//!    ONE standalone project per song (each 0-based, its own tempo). No
//!    media stems, no FX factory (those are the native audio path).
//! 2. `build_in_process_daw` serves Standalone's bundle over architect's
//!    in-memory link and wires the global `daw::` facade to it
//!    (`daw::init_from_parts` — the 1-arg wasm form, no block_on runtime).
//! 3. `SetlistServiceImpl::with_daw` + an `architect::LocalServer` hosting
//!    the setlist RPC (+ its `#[subscribe]` stream sibling) behind a
//!    `LayerRouter`; `build_from_open_projects()` builds the setlist and
//!    `start_stream_pumps()` starts the per-hub pumps.
//!
//! Everything runs under `wasm_bindgen_futures::spawn_local` (the browser
//! has no tokio runtime and no `block_on`). The built engine is parked in
//! a wasm `thread_local` singleton — it and every client it holds are
//! `!Send`, which is exactly why it lives thread-local rather than in a
//! `static`.
//!
//! STAGE 4b (dormant): the UI wiring — `session_ui::Session::init` and the
//! [`SessionEventBridge`] that folds the service's streams into session-ui's
//! global signals — is present but OFF, so this stage does not disturb the
//! existing browser `type: song` data path (see `pages/song_session.rs`
//! and `pages/setlist_session.rs`). Flip [`STAGE_4B`] to wire it up.

#![cfg(target_arch = "wasm32")]

use std::cell::RefCell;

use daw::service::{ExtState as _, ProjectContext, ProjectInfo};
use daw_standalone::bootstrap::{InProcessDaw, build_in_process_daw};
use daw_standalone::sync::Standalone;
use session::services::setlist_service::{
    SetlistServiceStreamClient, setlist_service_stream_service_descriptor,
    stream_serve as setlist_service_stream_serve,
};
use session::setlist_service::demo::{
    demo_chart_for, demo_songs_base, stamp_song_with_default_tempo_native,
};
use session::{
    SetlistServiceClient, SetlistServiceImpl, serve_setlist_service,
    setlist_service_service_descriptor,
};

/// STAGE 4b master switch. While `false`, the engine builds and parks but
/// no UI is wired to it — the live `type: song` browser path is untouched.
/// Flip to `true` to install the session-ui `Session` client + mount
/// [`SessionEventBridge`].
pub const STAGE_4B: bool = false;

/// GUID for the demo song at setlist index `i` (one project per song).
fn demo_song_guid(i: usize) -> String {
    format!("demo-song-{i:02}")
}

/// The parked in-browser engine. All fields are `!Send` (single-threaded
/// wasm vox: `Rc<RefCell<..>>` sinks), so this lives in a `thread_local`.
pub struct SessionEngine {
    /// Shared service handle — a Stage-4b UI bridge attaches to its
    /// `#[subscribe]` hubs (in-process, no wire) through the stream client.
    pub setlist: SetlistServiceImpl<Standalone>,
    /// RPC client over the in-process `LocalServer` — the same
    /// `SetlistServiceClient` a remote gets over a WebSocket, here over
    /// architect's in-memory link.
    pub client: SetlistServiceClient,
    /// Stream client for the `#[subscribe]` events + active_indices streams.
    pub stream_client: SetlistServiceStreamClient,
    /// The standalone backend itself (kept for future direct native-trait
    /// access from the browser engine).
    #[allow(dead_code)]
    pub standalone: Standalone,
    /// Keeps the daw-facade in-memory link's acceptor alive.
    _daw: InProcessDaw,
    /// Keeps the setlist RPC `LocalServer`'s acceptor + lanes alive.
    _scope: std::sync::Arc<architect::Scope>,
}

thread_local! {
    /// The one browser engine, once [`bootstrap`] has succeeded. `RefCell`
    /// (not `OnceCell`) so accessors can hand out clones of its `Clone`
    /// clients without borrowing across an `.await`.
    static ENGINE: RefCell<Option<SessionEngine>> = const { RefCell::new(None) };
}

/// Whether the engine has finished building and is parked.
pub fn is_running() -> bool {
    ENGINE.with(|e| e.borrow().is_some())
}

/// A clone of the setlist RPC client, once the engine is up. `Clone` is
/// cheap (an `Arc`'d `Caller`); returning a clone avoids holding the
/// thread-local `RefCell` borrow across `.await` points in callers.
pub fn client() -> Option<SetlistServiceClient> {
    ENGINE.with(|e| e.borrow().as_ref().map(|s| s.client.clone()))
}

/// A clone of the `#[subscribe]` stream client, once the engine is up.
pub fn stream_client() -> Option<SetlistServiceStreamClient> {
    ENGINE.with(|e| e.borrow().as_ref().map(|s| s.stream_client.clone()))
}

/// Kick off the engine build in the background (once). Safe to call at app
/// boot: it no-ops if already running/building and never blocks the caller
/// — the whole build runs under `spawn_local`.
pub fn bootstrap() {
    if is_running() {
        return;
    }
    wasm_bindgen_futures::spawn_local(async move {
        match build().await {
            Ok(engine) => {
                // STAGE 4b: install the session-ui RPC client singleton so
                // transport panels / the bridge drive THIS engine. Kept off
                // so the live `type: song` path (which never reads
                // `session_ui::Session`) is undisturbed.
                if STAGE_4B {
                    if let Err(e) = session_ui::Session::init(engine.client.clone()) {
                        tracing::warn!("session engine: Session::init failed: {e:?}");
                    }
                }
                ENGINE.with(|c| *c.borrow_mut() = Some(engine));
                tracing::info!("session engine: ready (parked in-browser setlist player)");
            }
            Err(e) => tracing::warn!("session engine: bootstrap failed: {e:?}"),
        }
    });
}

/// The async build — mirrors the native `bootstrap()` minus audio/guide.
async fn build() -> eyre::Result<SessionEngine> {
    // 1. Standalone backend seeded with a playable demo setlist — ONE
    //    project per song (each 0-based, its own default tempo/time-sig).
    //    Names are zero-padded so `Projects::list()` (name-sorted, followed
    //    by `build_from_open_projects`) keeps authored order.
    tracing::info!("session engine: seeding demo setlist …");
    let standalone = Standalone::new();
    let songs = demo_songs_base();
    let mut song_guids: Vec<String> = Vec::with_capacity(songs.len());
    for (i, song) in songs.iter().enumerate() {
        let guid = demo_song_guid(i);
        standalone.seed_project(ProjectInfo {
            guid: guid.clone(),
            name: format!("{i:02} {}", song.name),
            path: String::new(),
        });
        stamp_song_with_default_tempo_native(
            &standalone,
            ProjectContext::Project(guid.clone()),
            song,
        )
        .map_err(|e| eyre::eyre!("stamp song {} ({}): {e:?}", i, song.name))?;
        // Attach the bundled keyflow chart (ext-state `FTS/chart_text`) so
        // setlist hydration can serve it — the chart pane renders this text.
        if let Some(chart) = demo_chart_for(song.name) {
            standalone
                .set_project(
                    ProjectContext::Project(guid.clone()),
                    session::setlist_service::CHART_EXT_STATE_SECTION,
                    session::setlist_service::CHART_EXT_STATE_KEY,
                    chart,
                )
                .map_err(|e| eyre::eyre!("stamp chart for {}: {e:?}", song.name))?;
        }
        song_guids.push(guid);
    }
    let first_guid = song_guids
        .first()
        .cloned()
        .ok_or_else(|| eyre::eyre!("demo setlist produced no songs"))?;
    standalone.set_current_project(&first_guid);
    tracing::info!(
        "session engine: stamped {} per-song projects ('{}' …)",
        song_guids.len(),
        first_guid,
    );

    // 2. In-process daw facade over architect's in-memory link. The setlist
    //    build/hydration path resolves the daw through `daw::get()`, so
    //    install the global facade (1-arg wasm form — no block_on runtime).
    tracing::info!("session engine: building in-process daw facade …");
    let bundle = build_in_process_daw(standalone.clone()).await?;
    daw::init_from_parts(bundle.daw.clone());

    // 3. The setlist service over the standalone backend, hosted in-process
    //    behind a LocalServer (RPC + its `#[subscribe]` stream sibling).
    let setlist = SetlistServiceImpl::with_daw(standalone.clone());
    let router = daw::LayerRouter::new()
        .with(
            setlist_service_service_descriptor(),
            serve_setlist_service(setlist.clone()),
        )
        // The `#[subscribe]` stream sibling (events + active_indices),
        // served from the impl's PubSub hubs. Without it the stream client's
        // subscribe calls return `UnknownMethod`.
        .with(
            setlist_service_stream_service_descriptor(),
            setlist_service_stream_serve(setlist.clone()),
        );
    let scope = architect::Scope::new();
    let server = architect::LocalServer::serve(router, std::sync::Arc::clone(&scope));
    tracing::info!("session engine: LocalServer up; establishing clients …");
    let caller = server
        .caller()
        .await
        .map_err(|e| eyre::eyre!("local setlist caller: {e:?}"))?;
    let client = SetlistServiceClient::new(caller);
    let stream_client = server
        .establish::<SetlistServiceStreamClient>()
        .await
        .map_err(|e| eyre::eyre!("local setlist stream client: {e:?}"))?;

    // Initial build from the seeded projects. Later (UI-driven) builds
    // republish through the hub.
    client
        .build_from_open_projects()
        .await
        .map_err(|e| eyre::eyre!("build_from_open_projects: {e:?}"))?;
    tracing::info!("session engine: setlist built from standalone projects");

    // Start the `#[subscribe]` stream pumps AFTER the build so the events
    // pump snapshots the populated setlist (same ordering constraint as the
    // native engine).
    setlist.start_stream_pumps();

    Ok(SessionEngine {
        setlist,
        client,
        stream_client,
        standalone,
        _daw: bundle,
        _scope: scope,
    })
}

// ── STAGE 4b (dormant) ──────────────────────────────────────────────────

use dioxus::prelude::*;

/// STAGE 4b (dormant): bridges the setlist service's `#[subscribe]`
/// streams into session-ui's global signals — the in-process flavor of the
/// web remote's subscription. Ported from
/// `apps/fasttrackstudio/src/session_view.rs` with the engine accessor
/// swapped for [`client`]/[`stream_client`] and all `guide::*` calls
/// dropped.
///
/// Mounted **nowhere** while [`STAGE_4B`] is `false`; kept compiling so 4b
/// is a one-line flip. It must NOT be mounted alongside the live
/// `type: song` path (which owns session-ui's globals today) until 4b.
#[component]
pub fn SessionEventBridge() -> Element {
    // ── Events stream: setlist structure + per-song transport ──────────
    use_future(move || async move {
        let Some(stream_client) = stream_client() else {
            tracing::warn!("session engine not running; setlist events unavailable");
            return;
        };
        let Some(client) = client() else { return };

        // Consume the `events` `#[subscribe]` stream through the stream
        // client so the vox lane pumps it (a raw in-process hub attach is
        // never drained — the lane is what moves data).
        let (tx, mut rx) = vox::channel::<session::SetlistEvent>();
        spawn(async move {
            if let Err(e) = stream_client.events(tx).await {
                tracing::warn!("events subscription ended: {e:?}");
            }
        });

        // Deterministic initial snapshot (no reliance on the stream's first
        // republish).
        match client.setlist().await {
            Ok(setlist) => session_ui::apply_setlist_event(
                &session::SetlistEvent::SetlistChanged(setlist),
            ),
            Err(e) => tracing::warn!("initial setlist snapshot failed: {e:?}"),
        }

        while let Ok(Some(ev)) = rx.recv().await {
            session_ui::apply_setlist_event(ev.get());
        }
        tracing::warn!("setlist event stream ended");
    });

    // ── Active-indices stream: the cursor (which song/section is current) ─
    use_future(move || async move {
        let Some(stream_client) = stream_client() else { return };
        let Some(client) = client() else { return };

        let (tx, mut rx) = vox::channel::<session_proto::ActiveIndices>();
        spawn(async move {
            if let Err(e) = stream_client.active_indices(tx).await {
                tracing::warn!("active_indices subscription ended: {e:?}");
            }
        });

        // Open on song 0 / section 0 (fired concurrently so `rx` is already
        // polling when the cursor publish arrives).
        spawn(async move {
            match client.seek_to_section(0, 0).await {
                Ok(_) => tracing::info!("opened setlist on song 0 / section 0"),
                Err(e) => tracing::warn!("initial seek to song 0 failed: {e:?}"),
            }
        });

        while let Ok(Some(ai)) = rx.recv().await {
            session_ui::apply_active_indices(ai.get());
        }
        tracing::warn!("active-indices stream ended");
    });

    rsx! {}
}
