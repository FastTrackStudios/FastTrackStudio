# Handoff — state & outstanding work (2026-07-09)

Repo: codeberg `FastTrackStudios/FastTrackStudio`, local
`/run/media/Development/FastTrackStudio`. ONE root workspace
(~195 members): `apps/ crates/ features/ libs/ docs/`. Everything
builds from root; `nix develop` is the environment; `just` has the
tailwind/site/docs recipes. Read CLAUDE.md (rules) and LAYOUT.md
(placement map) before changing structure.

## Live state on this machine

- The **live guitar rig** is a systemd user service (`signal-engine`,
  release build at ~/.local/lib/fts, deployed by `just rig-install`;
  logs: `journalctl --user -u signal-engine -f`). NOTE: after the
  single-binary consolidation (item 14, done in-tree) the unit keeps its
  `signal-engine` name but the NEXT `just rig-install` deploys ONE
  binary — `~/.local/lib/fts/fasttrackstudio` run as `fasttrackstudio
  --engine`, web remote embedded (`--features embed-web`). The CURRENTLY
  deployed install still runs the old `signal-engine` binary +
  `signal-web` bundle until that deploy happens (both left in place by
  rig-install; clean by hand after confirming). Hardened + drilled
  2026-07-10: kill -9 → serving in 1.45s with last patch/song/tempo
  restored; pipewire daemon death → detected → self-restart, back in
  ~15s; USB device re-link watchdog; plugin panics bypass the block;
  MIDI hot-plug; poison-tolerant locks throughout. It serves ws + the web remote at http://<host>:4040/ and its
  iroh id (`~/.config/signal/iroh-endpoint-id`). Rig config: `~/.config/signal/rig` is a SYMLINK into the
  repo (`features/rigs/guitar/default-config/`, set up by `just
  rig-link`; old dir kept as rig.bak-*) — live edits (editor or the
  rig's auto-save, which stores NAM paths rig-dir-relative) are
  working-tree diffs to commit. Reload with the ↻ header button.
- **Voyager (mac)** still has the OLD multi-repo clones + bootstrap
  script (~/voyager-bootstrap.sh) — must be re-pointed: fresh clone of
  the monorepo, `nix develop`, `just rig-install` (one binary:
  `fasttrackstudio --engine`, web bundle embedded). Its rig config/NAM paths were already migrated
  to /Users/codywright. The flake is darwin-ready (apple-sdk_15 branch)
  and the engine's PipeWire backend is now Linux-gated (plain cpal →
  CoreAudio elsewhere) — darwin build is UNTESTED, first `cargo build`
  on voyager will tell.

## Outstanding work (rough priority)

1. **Auto-updater**: `apps/fasttrackstudio/src/updates.rs` is a
   skeleton (Updater trait + CodebergUpdater stub + Settings UI stub).
   `libs/installer-core` already has retry/progress/download/extract
   primitives — build the real thing on those. Then rebuild
   `apps/installer` as a thin fasttrackstudio-downloader (README in
   place).
2. **Deploys not run**: apps/docs-site (fly app `fts-docs`,
   docs.fasttrackstudio.app; 188 pages, 0 broken links — build green,
   never deployed) + keyflow.fasttrackstudio.app → /keyflow/ redirect;
   apps/site (fts-site) likewise built, not deployed.
3. **Dedup queue** (CLAUDE.md): keyflow-daw-analysis → daw-proto types
   only; fold vendored `features/audio/audio-controls` into
   features/daw-ui (or delete once signal-ui migrates off it);
   consolidate duplicate wav/resampler helpers. (keyflow-midi is
   EXEMPT — user decision, it stays.)
4. **fx plugin shells** (eq-plugin, reverb-plugin, …, meter-plugin):
   non-members / removed; they referenced pre-move paths and
   meter-plugin has audiocore-gui API drift. Revive from git history
   when plugins matter again.
5. **TTS activation**: session-guide's Chatterbox works but needs
   `--features tts`, `FTS_TTS=1`, and a voice wav at
   `~/.config/fts/tts-voice.wav`; cues cache to `~/.config/fts/tts-cache/`.
   Models download from HF on first synthesis. Untested end-to-end
   with real models.
6. **Known unverified corners**: fts-story visual snapshots not
   re-rendered after the blitz alpha.6 bump (compiles; parity
   unchecked); keyflow-sync `onnx`/`whisper` features vs the ort
   =2.0.0-rc.10 pin; `nix develop` wasm CC resource-dir (dx web builds
   worked, but see apps/site notes if `stdarg.h not found` appears).
7. **Architect wishlist** (framework now in-tree at libs/architect):
   parameterized `#[subscribe]` (daw streams currently filter
   client-side by project); the `task` project should repoint its
   architect dep to this repo's git URL.
8. **Legacy folder**: /run/media/Development/FastTrackStudio-legacy
   holds old checkouts + unrelated side projects (better-auth, sea-orm
   forks, etc.). Audit before deleting; nothing in-tree depends on it.
9. **fasttrackstudio app polish**: Home landing page (Session / Signal /
   Charts cards) → Signal is a rig picker (Guitar live, rest of the
   swarm listed as coming) → GuitarRigRemote (works); Session tab plays
   the demo setlist with the guide clicking. Next: real setlists from
   ~/.config/signal styx into the session engine, guide TTS cue bank
   wiring in-app, engine log surfacing in the Engines panel.
10. **One UI, every surface**: apps/fasttrackstudio is dual-platform —
   the desktop binary (dioxus desktop) and `dx build --platform web
   --no-default-features --features signal` are the SAME app; no
   desktop-only pages. Prefs go through src/prefs.rs (~/.config/fts
   files natively, localStorage `fts.*` on web); engines supervisor +
   updater are native-only; web derives the engine ws URL from the
   page origin (served by the engine = connected to it). Session on
   web needs the session engine served over the network (it's
   in-process native-only today) — that's the next architectural step.
   Wasm builds of iroh-consuming crates need getrandom `wasm_js` (see
   apps/fasttrackstudio Cargo.toml wasm table) and ring needs unwrapped
   clang env vars (see the flake notes / agent report in git history).
11. **iroh p2p transport** (`architect::iroh_link`, feature `iroh`):
   vox Link over iroh 1.0 QUIC bi-streams (u32-BE framing, ALPN
   `architect/vox/1`). The signal engine serves it beside ws by default —
   device key `~/.config/signal/iroh.key`, id logged + written to
   `~/.config/signal/iroh-endpoint-id`. The app dials it via
   `SIGNAL_ENGINE_IROH_ID` or the connect-screen form (saved to
   `~/.config/fts/signal-engine-iroh-id`; app device key
   `~/.config/fts/iroh.key`). iroh is wired in the BROWSER too:
   the fts web build dials by rig key (device key in
   localStorage `fts.iroh-key`, relay-only in the sandbox). The engine
   serves the web remote itself (embedded bundle / SIGNAL_WEB_DIST /
   legacy dirs — see item 14).
   Loopback vox handshake tested (`cargo test -p architect --features
   iroh`); real cross-network dial via n0 discovery/relays UNTESTED —
   browser iroh dial also untested end-to-end. Next: engine id
   QR/pairing UX, per-device key roster, public fasttrackstudio.app
   deploy of the fts web bundle.

12. **Default rig config lives in-repo**:
   `features/rigs/guitar/default-config/` — snapshot of the live worship
   rig (6 styx files + 11 NAM captures, 3.2 MB), embedded into
   signal-guitar via include_str!/include_bytes!. First run seeds
   `<rig_dir>` (files + models/) and NAM paths in the seed are
   rig-dir-relative (`models/<file>.nam`, resolved at library load;
   absolute paths pass through). Verified: fresh SIGNAL_RIG_DIR →
   "profile loaded (13 patches)". NOTE: the NAM captures are
   third-party (ToneHunt-style downloads) — check redistribution
   licensing before making the repo public. To refresh the defaults,
   re-copy ~/.config/signal/rig and re-run the path rewrite.
13. **Rig-in-the-browser (WebAudio) roadmap**: the DSP layer is now
   FULLY wasm-clean — neural-amp-modeler has a pure-Rust inference
   engine (src/pure/, parity-tested vs the C++ core on all 11 shipped
   models, LSTM bit-exact; NamModel::from_bytes for browsers; native
   FFI path unchanged), and signalsmith-stretch is REMOVED entirely
   (Shimmer never used it; PitchChain's Signalsmith algorithm is gone,
   Rubberband-off falls back to WSOLA; pitch-dsp is pure Rust and
   chain compiles on wasm).
   eq/comp/reverb/delay/audiocore-dsp/daw-audio-graph all check on
   wasm32. Remaining for the PoC (mic → NAM → out): a GuitarWorklet
   wasm-bindgen adapter (copy daw-standalone's
   src/audio_engine/web.rs + examples/web_worklet/processor.js
   pattern), getUserMedia glue, then the big one — split a wasm-safe
   control core out of GuitarRigBackend (pipewire/midir/fs shell vs
   plain state; proto traits are the seam) so the browser serves
   Rig/AudioSettings/RigStream locally. Perf notes: heaviest rig
   WaveNet ~3.9x realtime native scalar; try +simd128 on wasm, and a
   set_slimmable_size knob on PureNamModel is a small follow-up.

14. **Single-binary consolidation — DONE (2026-07-10, not yet deployed)**:
   apps/signal-engine folded into apps/fasttrackstudio
   (`src/engine_main.rs`; `fasttrackstudio --engine` = the headless
   engine, plain-arg match, no GUI/session bootstrap in that mode);
   apps/signal-web DELETED (the fts web build supersedes it — its
   tailwind input.css/fts-theme.css moved to apps/fasttrackstudio/,
   sheet now assets/tailwind-signal.css, distinct from the session
   sheet assets/tailwind.css). Web bundle EMBEDDED via include_dir
   behind feature `embed-web` (staging: `just web-stage` → dx web build
   → apps/fasttrackstudio/web-dist/, gitignored). Engine web-asset
   precedence: SIGNAL_WEB_DIST → embedded → <exe_dir>/signal-web
   (legacy) → target/dx/fasttrackstudio → headless-only. SIGNAL_WEB_DIST
   still honored as an override. engine-launcher's SIGNAL_ENGINE now
   resolves the `fasttrackstudio` binary with `--engine` (self-spawn via
   current_exe when the app supervises its own engine; systemd unit name
   stays `signal-engine`). Justfile: `signal-web-sync` removed,
   `web-stage` added, `rig-install` deploys the one binary, `tailwind`
   builds the signal sheet in apps/fasttrackstudio. Run `just
   rig-install` to move the live service onto the new artifact. Process
   split stays a runtime property. (dx note: wasm-opt SIGABRTs in this
   shell — dx continues with unoptimized wasm; bundle works.)

## Gotchas that will bite again

- Dead-repo `[patch]` tables only resolve with a cached git db /
  committed lockfile (cargo fetches patched sources on fresh resolve).
  Keep `Cargo.lock` committed. Remaining such table: keyflow.git
  (Editor.git references it).
- `pkill -f <pattern>` kills your own shell if the pattern appears in
  the command line (exit 144). Use narrowed patterns — and note the
  engine process is now `fasttrackstudio --engine` (the old deploy was
  `signal-engine$`).
- Tailwind: run `just tailwind` BEFORE `dx build` whenever UI classes
  change.
- One blitz rev tree-wide (`links = servo_style_crate` collisions);
  currently 727dab01.
