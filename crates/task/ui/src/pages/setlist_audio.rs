//! Stage 4b-2 — **audio** for the engine-fed browser setlist.
//!
//! `daw-standalone`'s [`WebRenderer`] runs **inside an AudioWorklet** — the
//! browser twin of the native cpal callback. The audio thread owns the render
//! graph and is called every 128-frame quantum; the main thread only:
//!
//!  1. compiles the dedicated worklet wasm bundle once
//!     (`assets/worklet/daw_standalone_bg.wasm`, built by
//!     `just task-worklet-wasm` — ~650 KB, release-opt, NOT the app bundle),
//!  2. seeds the song's tracks + fetches/decodes stem PCM
//!     (`decodeAudioData` only exists on the main thread) and streams it over
//!     the node's `MessagePort`, and
//!  3. relays transport commands (play / pause / seek).
//!
//! The worklet posts a ~21 ms state tick (position / play-state / per-track
//! peaks) that this side mirrors for the UI (VU meters, drift-kill). There is
//! NO main-thread rendering and NO jitter buffer — realtime at worklet
//! quantum size, the same path an in-browser signal guitar rig needs.
//!
//! One [`SetlistAudio`] per ACTIVE song (rebuilt on song switch — one
//! project's graph renders at a time), all on ONE app-wide shared
//! `AudioContext` (Chrome hard-caps live contexts per tab; churning them
//! mutes everything permanently).

// Browser-only. The module still has to compile on native (pages/mod.rs
// declares it unconditionally), so gate the whole implementation on wasm32.
#[cfg(target_arch = "wasm32")]
mod imp {
    use std::cell::{Cell, RefCell};
    use std::rc::Rc;

    use wasm_bindgen::prelude::*;
    use wasm_bindgen::JsCast;
    use wasm_bindgen_futures::JsFuture;
    use web_sys::{
        AudioBuffer, AudioContext, AudioWorkletNode, AudioWorkletNodeOptions, MessageEvent,
        MessagePort, Response,
    };

    use crate::pages::song_session::imp::Manifest;

    /// Stable take-guid for stem `i` (each song gets a fresh worklet renderer,
    /// so an index-based key is unique + stable — the async decode attaches
    /// PCM by the same key the seed used).
    fn stem_take_guid(i: usize) -> String {
        format!("setlist-audio-take-{i:02}")
    }

    // The worklet bundle, embedded at COMPILE time (rebuilt by
    // `just task-worklet-wasm`; cargo tracks the files). Embedding — instead
    // of fetching `/assets/worklet/...` at runtime — sidesteps two dev-server
    // traps that both abort `addModule`: dx serves static assets with no
    // `Content-Type` (module loads hard-require a JS MIME), and the app's
    // service worker (scoped to `/assets/`) answers asset fetches with the
    // HTML shell. A Blob URL carries its own MIME and needs no server.
    const WORKLET_GLUE: &str =
        include_str!("../../../../../apps/task/web/assets/worklet/daw_standalone.js");
    const WORKLET_PROC: &str =
        include_str!("../../../../../apps/task/web/assets/worklet/processor.js");
    const WORKLET_WASM: &[u8] =
        include_bytes!("../../../../../apps/task/web/assets/worklet/daw_standalone_bg.wasm");

    thread_local! {
        /// ONE `AudioContext` for the whole app, created lazily and NEVER
        /// closed (see module docs).
        static SHARED_CTX: RefCell<Option<AudioContext>> = const { RefCell::new(None) };
        /// Whether `processor.js` has been registered on the shared context.
        static WORKLET_REGISTERED: Cell<bool> = const { Cell::new(false) };
        /// Whether the shared context's output sink has been cycled (see
        /// [`cycle_sink`]) — once per context.
        static SINK_CYCLED: Cell<bool> = const { Cell::new(false) };
    }

    /// Force the context to open a FRESH physical output stream by cycling
    /// `setSinkId` → `{type:"none"}` → `""` (default). Chrome opens a
    /// context's device stream ONCE and never retries it — if that open
    /// happened while the OS audio path was briefly broken (this rig's
    /// pipewire-pulse restarts every few minutes, stranding the browser's
    /// audio service), the context is stuck on a silent FAKE stream forever:
    /// graph runs, meters move, zero sound, no PipeWire node. New stream
    /// opens recover (that's why other tabs play) — so force one. Same-ID
    /// `setSinkId` calls are spec'd no-ops, hence the none→default cycle.
    /// Done via `Reflect` (`setSinkId` is Chrome 110+; web-sys coverage
    /// varies).
    async fn cycle_sink(ctx: &AudioContext) {
        let f = match js_sys::Reflect::get(ctx.as_ref(), &JsValue::from_str("setSinkId")) {
            Ok(f) if f.is_function() => js_sys::Function::from(f),
            _ => return, // pre-110 browser: nothing to do
        };
        let none = js_sys::Object::new();
        let _ = js_sys::Reflect::set(
            &none,
            &JsValue::from_str("type"),
            &JsValue::from_str("none"),
        );
        for arg in [JsValue::from(none), JsValue::from_str("").into()] {
            match f.call1(ctx.as_ref(), &arg) {
                Ok(p) => {
                    if let Ok(p) = p.dyn_into::<js_sys::Promise>() {
                        let _ = JsFuture::from(p).await;
                    }
                }
                Err(e) => {
                    tracing::warn!("setlist audio: setSinkId failed: {e:?}");
                    return;
                }
            }
        }
        tracing::info!("setlist audio: output sink cycled (fresh device stream)");
    }

    /// The app-wide shared [`AudioContext`], created on first use.
    fn shared_ctx() -> Result<AudioContext, String> {
        SHARED_CTX.with(|c| {
            let mut slot = c.borrow_mut();
            if let Some(ctx) = slot.as_ref() {
                return Ok(ctx.clone());
            }
            // `latencyHint: "playback"`: Chrome's default WebAudio stream is
            // the low-latency "interactive" category, and when the device
            // open for it fails (this rig's PipeWire runs a large fixed
            // quantum for the Dante/console chain), Chrome silently hands the
            // context a FAKE output stream — the graph runs, meters move,
            // zero sound, no PipeWire node. The "playback" category opens the
            // same higher-latency stream media elements use (which audibly
            // works here). Latency is fine for a setlist player; the future
            // low-latency rig path needs the rig's quantum unlocked instead.
            let opts = web_sys::AudioContextOptions::new();
            opts.set_latency_hint(&JsValue::from_str("playback"));
            let ctx = AudioContext::new_with_context_options(&opts)
                .map_err(|e| format!("AudioContext: {e:?}"))?;
            *slot = Some(ctx.clone());
            Ok(ctx)
        })
    }

    /// Register the worklet module on the context (once) and compile the
    /// worklet wasm bundle (once).
    ///
    /// The module is assembled as a self-contained **Blob** module: dx's dev
    /// server serves static assets with NO `Content-Type`, and module loading
    /// hard-fails without a JS MIME type ("Unable to load a worklet's
    /// module") — a Blob URL carries its own type, on any server. We fetch
    /// `processor.js` + the wasm-bindgen glue as TEXT, strip the module
    /// syntax (the glue's `export`s; processor's `import` line — polyfills
    /// stay ABOVE the glue so its guarded `TextDecoder` consts see them), and
    /// register the concatenation.
    async fn ensure_worklet_assets(ctx: &AudioContext) -> Result<(), String> {
        if !WORKLET_REGISTERED.with(Cell::get) {
            let proc = WORKLET_PROC;

            // Glue: `export class X {` → `class X {`; drop the final
            // `export { initSync, __wbg_init as default };` line.
            let glue: String = WORKLET_GLUE
                .lines()
                .filter(|l| !l.trim_start().starts_with("export {"))
                .map(|l| l.replacen("export class ", "class ", 1))
                .map(|l| l.replacen("export function ", "function ", 1))
                .map(|l| l.replacen("export const ", "const ", 1))
                .collect::<Vec<_>>()
                .join("\n");
            // Processor: split at its `import` line — polyfills above, the
            // processor class below; glue goes in between.
            let (pre, post) = match proc.lines().position(|l| l.trim_start().starts_with("import "))
            {
                Some(idx) => {
                    let lines: Vec<&str> = proc.lines().collect();
                    (lines[..idx].join("\n"), lines[idx + 1..].join("\n"))
                }
                None => (String::new(), proc.to_string()),
            };
            let src = format!("{pre}\n{glue}\n{post}");

            let parts = js_sys::Array::of1(&JsValue::from_str(&src));
            let opts = web_sys::BlobPropertyBag::new();
            opts.set_type("application/javascript");
            let blob = web_sys::Blob::new_with_str_sequence_and_options(&parts, &opts)
                .map_err(|e| format!("worklet blob: {e:?}"))?;
            let url = web_sys::Url::create_object_url_with_blob(&blob)
                .map_err(|e| format!("worklet blob url: {e:?}"))?;
            let promise = ctx
                .audio_worklet()
                .map_err(|e| format!("audio_worklet(): {e:?}"))?
                .add_module(&url)
                .map_err(|e| format!("add_module: {e:?}"))?;
            JsFuture::from(promise)
                .await
                .map_err(|e| format!("add_module await: {e:?}"))?;
            let _ = web_sys::Url::revoke_object_url(&url);
            WORKLET_REGISTERED.with(|r| r.set(true));
        }

        Ok(())
    }

    /// Build a `{kind: ...}` message object.
    fn msg(kind: &str) -> js_sys::Object {
        let o = js_sys::Object::new();
        let _ = js_sys::Reflect::set(&o, &JsValue::from_str("kind"), &JsValue::from_str(kind));
        o
    }
    fn set(o: &js_sys::Object, k: &str, v: &JsValue) {
        let _ = js_sys::Reflect::set(o, &JsValue::from_str(k), v);
    }

    /// The live worklet node + the tick-handler closure (kept alive together).
    struct Pump {
        node: AudioWorkletNode,
        port: MessagePort,
        /// The `<audio>` element playing the rendered MediaStream (the
        /// media-path output sink — see the connect site).
        sink: web_sys::HtmlAudioElement,
        _onmsg: Closure<dyn FnMut(MessageEvent)>,
    }

    /// One active song's audio. Dropping it stops the worklet transport and
    /// disconnects the node (the shared context stays alive for the next song).
    pub(crate) struct SetlistAudio {
        ctx: AudioContext,
        pump: Rc<RefCell<Option<Pump>>>,
        /// Transport intent while the worklet is still attaching (the async
        /// module setup takes ~a frame; a Play click can beat it).
        want_play: Rc<Cell<bool>>,
        pending_seek: Rc<Cell<Option<f64>>>,
        // ── mirrors of the worklet's ~21 ms state tick ──
        pos: Rc<Cell<f64>>,
        playing: Rc<Cell<bool>>,
        peaks: Rc<RefCell<Vec<f32>>>,
    }

    impl SetlistAudio {
        /// Wire one song's audio: (async) register + compile the worklet
        /// assets, create the node, seed a track per stem, and stream each
        /// stem's decoded PCM in as it lands.
        pub(crate) fn build(slug: &str, manifest: &Manifest) -> Result<SetlistAudio, String> {
            let ctx = shared_ctx()?;
            let audio = SetlistAudio {
                ctx: ctx.clone(),
                pump: Rc::new(RefCell::new(None)),
                want_play: Rc::new(Cell::new(false)),
                pending_seek: Rc::new(Cell::new(None)),
                pos: Rc::new(Cell::new(0.0)),
                playing: Rc::new(Cell::new(false)),
                peaks: Rc::new(RefCell::new(Vec::new())),
            };

            let stems: Vec<(String, String, String)> = manifest
                .stems
                .iter()
                .enumerate()
                .map(|(i, s)| {
                    (
                        s.name.clone(),
                        stem_take_guid(i),
                        format!("/media/songs/{slug}/{}", s.file),
                    )
                })
                .collect();

            {
                let ctx = ctx.clone();
                let pump = audio.pump.clone();
                let want_play = audio.want_play.clone();
                let pending_seek = audio.pending_seek.clone();
                let pos = audio.pos.clone();
                let playing = audio.playing.clone();
                let peaks = audio.peaks.clone();
                wasm_bindgen_futures::spawn_local(async move {
                    if let Err(e) = attach(
                        &ctx,
                        stems,
                        &pump,
                        &want_play,
                        &pending_seek,
                        pos,
                        playing,
                        peaks,
                    )
                    .await
                    {
                        tracing::warn!("setlist audio: worklet attach failed: {e}");
                    }
                });
            }

            tracing::info!("setlist audio: built (worklet-hosted renderer, v4b2c)");
            Ok(audio)
        }

        /// Start playback. Resumes the context (the caller's Play click is the
        /// required user gesture) and rolls the worklet transport.
        pub(crate) fn play(&self) {
            if let Ok(promise) = self.ctx.resume() {
                let ctx = self.ctx.clone();
                wasm_bindgen_futures::spawn_local(async move {
                    if let Err(e) = JsFuture::from(promise).await {
                        tracing::warn!("setlist audio: ctx.resume() failed: {e:?}");
                    }
                    // Once per context (on the Play gesture, after resume):
                    // force a fresh physical output stream in case the
                    // original open landed on a fake sink (see cycle_sink).
                    if !SINK_CYCLED.with(Cell::get) {
                        SINK_CYCLED.with(|c| c.set(true));
                        cycle_sink(&ctx).await;
                    }
                });
            }
            self.want_play.set(true);
            if let Some(p) = self.pump.borrow().as_ref() {
                let _ = p.port.post_message(&msg("play"));
                // The Play click is the autoplay gesture for the media-path
                // sink element too.
                let _ = p.sink.play();
            }
        }

        /// Pause playback (context stays alive so Play resumes instantly).
        pub(crate) fn pause(&self) {
            self.want_play.set(false);
            if let Some(p) = self.pump.borrow().as_ref() {
                let _ = p.port.post_message(&msg("pause"));
            }
        }

        /// Seek the worklet transport to `seconds` (playing or paused).
        pub(crate) fn seek(&self, seconds: f64) {
            if let Some(p) = self.pump.borrow().as_ref() {
                let m = msg("seek");
                set(&m, "seconds", &JsValue::from_f64(seconds));
                let _ = p.port.post_message(&m);
            } else {
                self.pending_seek.set(Some(seconds));
            }
        }

        /// The render transport's position (audio truth, mirrored from the
        /// worklet tick) — for the drift-kill refinement.
        #[allow(dead_code)]
        pub(crate) fn position(&self) -> f64 {
            self.pos.get()
        }

        /// Whether the render transport is rolling (mirrored).
        pub(crate) fn is_playing(&self) -> bool {
            self.playing.get()
        }

        /// Per-stem peak levels (track order == stem order, mirrored from the
        /// worklet tick) — feeds the mixer VU meters.
        pub(crate) fn peaks(&self) -> Vec<f32> {
            self.peaks.borrow().clone()
        }
    }

    impl Drop for SetlistAudio {
        fn drop(&mut self) {
            // Stop the worklet transport and unhook the node. The shared
            // AudioContext is NOT closed — it's app-wide.
            if let Some(p) = self.pump.borrow_mut().take() {
                let _ = p.port.post_message(&msg("stop"));
                p.port.set_onmessage(None);
                p.node.disconnect();
                let _ = p.sink.pause();
                p.sink.set_src_object(None);
            }
        }
    }

    /// Register assets, create the worklet node, init the in-worklet renderer,
    /// seed the stems, flush any queued transport intent, and start the
    /// decodes. Port messages are ordered, so `init` → `add_stem`* →
    /// `attach`*/transport all land against a live renderer.
    #[allow(clippy::too_many_arguments)]
    async fn attach(
        ctx: &AudioContext,
        stems: Vec<(String, String, String)>,
        pump: &Rc<RefCell<Option<Pump>>>,
        want_play: &Rc<Cell<bool>>,
        pending_seek: &Rc<Cell<Option<f64>>>,
        pos: Rc<Cell<f64>>,
        playing: Rc<Cell<bool>>,
        peaks: Rc<RefCell<Vec<f32>>>,
    ) -> Result<(), String> {
        ensure_worklet_assets(ctx).await?;

        let opts = AudioWorkletNodeOptions::new();
        opts.set_number_of_inputs(0);
        opts.set_output_channel_count(&js_sys::Array::of1(&JsValue::from(2u32)));
        let node = AudioWorkletNode::new_with_options(ctx, "fts-daw-processor", &opts)
            .map_err(|e| format!("AudioWorkletNode: {e:?}"))?;
        // A processor that throws during construction or process() surfaces
        // ONLY through this event — without it the node just goes silent.
        let onprocerr = Closure::wrap(Box::new(move |e: web_sys::Event| {
            tracing::warn!("setlist audio: worklet PROCESSOR ERROR: {:?}", e.type_());
        }) as Box<dyn FnMut(web_sys::Event)>);
        node.set_onprocessorerror(Some(onprocerr.as_ref().unchecked_ref()));
        onprocerr.forget(); // node-lifetime handler, tiny leak per song build
        let port = node.port().map_err(|e| format!("worklet port: {e:?}"))?;

        // Tick handler: mirror the worklet's position / play-state / peaks.
        let onmsg = Closure::wrap(Box::new(move |e: MessageEvent| {
            let data = e.data();
            let kind = js_sys::Reflect::get(&data, &JsValue::from_str("kind"))
                .ok()
                .and_then(|k| k.as_string())
                .unwrap_or_default();
            match kind.as_str() {
                "tick" => {
                    if let Ok(p) = js_sys::Reflect::get(&data, &JsValue::from_str("pos")) {
                        if let Some(p) = p.as_f64() {
                            pos.set(p);
                        }
                    }
                    if let Ok(pl) = js_sys::Reflect::get(&data, &JsValue::from_str("playing")) {
                        if let Some(pl) = pl.as_bool() {
                            playing.set(pl);
                        }
                    }
                    if let Ok(pk) = js_sys::Reflect::get(&data, &JsValue::from_str("peaks")) {
                        if let Ok(arr) = pk.dyn_into::<js_sys::Float32Array>() {
                            *peaks.borrow_mut() = arr.to_vec();
                        }
                    }
                }
                "hello" => tracing::info!("setlist audio: worklet processor constructed"),
                "ready" => tracing::info!("setlist audio: worklet renderer ready"),
                "error" => {
                    let m = js_sys::Reflect::get(&data, &JsValue::from_str("message"))
                        .ok()
                        .and_then(|v| v.as_string())
                        .unwrap_or_default();
                    tracing::warn!("setlist audio: worklet error: {m}");
                }
                _ => {}
            }
        }) as Box<dyn FnMut(MessageEvent)>);
        port.set_onmessage(Some(onmsg.as_ref().unchecked_ref()));

        // init (the renderer constructs at the worklet's global sampleRate).
        // Raw wasm BYTES, transferred — a pre-compiled WebAssembly.Module
        // fails to deserialize into an AudioWorkletGlobalScope (the message
        // dies as a silent `messageerror`); `initSync` compiles the bytes on
        // the audio thread, where synchronous compilation is allowed.
        let bytes = js_sys::Uint8Array::from(WORKLET_WASM);
        let init = msg("init");
        set(&init, "wasmBytes", &bytes.buffer());
        let transfer = js_sys::Array::of1(&bytes.buffer().into());
        port.post_message_with_transferable(&init, &transfer)
            .map_err(|e| format!("post init: {e:?}"))?;

        // Seed one track + take per stem.
        for (name, guid, path) in &stems {
            let m = msg("add_stem");
            set(&m, "name", &JsValue::from_str(name));
            set(&m, "guid", &JsValue::from_str(guid));
            set(&m, "path", &JsValue::from_str(path));
            let _ = port.post_message(&m);
        }

        // Output via the MEDIA path, not `ctx.destination()`: on this rig
        // (pipewire-pulse cycling + Brave), Chrome's WebAudio destination
        // stream opens land on a silent FAKE device stream (graph runs,
        // meters move, zero sound, no PipeWire node — and even a successful
        // `setSinkId` cycle hands back another dead one), while media-element
        // playback provably works (YouTube). So render into a
        // MediaStreamAudioDestinationNode and play that stream through an
        // `<audio>` element — the same output pipeline media uses.
        let msd = ctx
            .create_media_stream_destination()
            .map_err(|e| format!("create_media_stream_destination: {e:?}"))?;
        node.connect_with_audio_node(&msd)
            .map_err(|e| format!("connect worklet node: {e:?}"))?;
        let sink = web_sys::HtmlAudioElement::new()
            .map_err(|e| format!("HtmlAudioElement: {e:?}"))?;
        sink.set_src_object(Some(&msd.stream()));
        sink.set_autoplay(true);
        let _ = sink.play();

        // Flush transport intent queued while attaching.
        if let Some(s) = pending_seek.take() {
            let m = msg("seek");
            set(&m, "seconds", &JsValue::from_f64(s));
            let _ = port.post_message(&m);
        }
        if want_play.get() {
            let _ = port.post_message(&msg("play"));
        }

        *pump.borrow_mut() = Some(Pump {
            node,
            port: port.clone(),
            sink,
            _onmsg: onmsg,
        });

        // Decode every stem, THROTTLED (a handful of range requests at a time
        // — firing ~23 at once 503s the media server), streaming each result
        // into the worklet as it lands.
        const DECODE_CONCURRENCY: usize = 5;
        let jobs = Rc::new(stems);
        let next = Rc::new(Cell::new(0usize));
        for _ in 0..DECODE_CONCURRENCY.min(jobs.len().max(1)) {
            let jobs = jobs.clone();
            let next = next.clone();
            let ctx = ctx.clone();
            let port = port.clone();
            wasm_bindgen_futures::spawn_local(async move {
                loop {
                    let i = next.get();
                    if i >= jobs.len() {
                        break;
                    }
                    next.set(i + 1);
                    let (_, guid, url) = &jobs[i];
                    if let Err(e) = decode_and_send(&ctx, &port, guid, url).await {
                        tracing::warn!("setlist audio: stem `{url}` decode failed: {e}");
                    }
                }
            });
        }

        tracing::info!("setlist audio: worklet pump attached ({} stems)", jobs.len());
        Ok(())
    }

    /// Fetch `url`, decode it through the context (`decodeAudioData` resamples
    /// to the context's rate == the worklet renderer's rate), interleave the
    /// channels, and post the PCM to the worklet (buffer transferred).
    async fn decode_and_send(
        ctx: &AudioContext,
        port: &MessagePort,
        take_guid: &str,
        url: &str,
    ) -> Result<(), String> {
        let array_buffer = fetch_array_buffer(url).await?;
        let promise = ctx
            .decode_audio_data(&array_buffer)
            .map_err(|e| format!("decode_audio_data: {e:?}"))?;
        let decoded = JsFuture::from(promise)
            .await
            .map_err(|e| format!("decode await: {e:?}"))?;
        let buffer: AudioBuffer = decoded
            .dyn_into()
            .map_err(|_| "decodeAudioData did not return an AudioBuffer".to_string())?;

        let channels = buffer.number_of_channels();
        let frames = buffer.length() as usize;
        let ch = channels.max(1) as usize;
        let mut pcm = vec![0.0_f32; frames * ch];
        for c in 0..channels {
            let data = buffer
                .get_channel_data(c)
                .map_err(|e| format!("get_channel_data({c}): {e:?}"))?;
            for (i, &s) in data.iter().enumerate() {
                pcm[i * ch + c as usize] = s;
            }
        }

        let jarr = js_sys::Float32Array::from(&pcm[..]);
        let m = msg("attach");
        set(&m, "guid", &JsValue::from_str(take_guid));
        set(&m, "pcm", &jarr);
        set(&m, "channels", &JsValue::from(channels));
        set(&m, "sampleRate", &JsValue::from(ctx.sample_rate() as u32));
        let transfer = js_sys::Array::of1(&jarr.buffer().into());
        port.post_message_with_transferable(&m, &transfer)
            .map_err(|e| format!("post attach: {e:?}"))?;
        Ok(())
    }

    /// Same-origin `fetch` → `arrayBuffer()`.
    async fn fetch_array_buffer(url: &str) -> Result<js_sys::ArrayBuffer, String> {
        let win = web_sys::window().ok_or_else(|| "no window".to_string())?;
        let resp_val = JsFuture::from(win.fetch_with_str(url))
            .await
            .map_err(|e| format!("fetch {url}: {e:?}"))?;
        let resp: Response = resp_val
            .dyn_into()
            .map_err(|_| "fetch did not return a Response".to_string())?;
        if !resp.ok() {
            return Err(format!("{url}: HTTP {}", resp.status()));
        }
        let promise = resp
            .array_buffer()
            .map_err(|e| format!("{url}: array_buffer: {e:?}"))?;
        let val = JsFuture::from(promise)
            .await
            .map_err(|e| format!("{url}: array_buffer await: {e:?}"))?;
        val.dyn_into::<js_sys::ArrayBuffer>()
            .map_err(|_| format!("{url}: response was not an ArrayBuffer"))
    }
}

#[cfg(target_arch = "wasm32")]
pub(crate) use imp::SetlistAudio;
