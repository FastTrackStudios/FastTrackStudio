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

15. **v0.0.1-alpha PUBLISHED** (codeberg release, prerelease): tarball +
   fts-installer + SHA256SUMS. fts-installer resolves latest incl.
   prereleases, verifies sums, installs/updates/uninstalls
   (`just release-package` builds dist/; binaries patchelf'd + stripped
   for stock distros — need webkit2gtk4.1/gtk3/alsa/pipewire/openssl3).
   NOTE: org asset quota is 1.5GiB (Snowflake) — cleaned 141 stale Task
   container versions to make room; a truncated upload serves a
   truncated asset silently, so verify sizes after upload.
16. **fts-extensions vendoring — DONE + integration tests GREEN**:
   lives at apps/extensions/reaper-fts-extensions (cdylib
   `reaper_fts_extensions.so`, `just ext-install` symlinks it into
   ~/.fts-dev/UserPlugins). `just reaper-integration-test` boots a
   headless REAPER (Dummy audio) with the extension loaded and runs
   apps/extensions/reaper-fts-extensions/tests/extension_loads.rs —
   13/13 green (~11s): action-surface inventory, action-list presence,
   keyflow marker/region insertion incl. lane/color/retro-rename
   assertions. The extension IS the daw socket host (host-hooks default
   feature) — never install daw-bridge next to it, the two fight over
   fts-daw-<pid>.sock.
17. **REAPER test harness (daw::test, feature test-harness)** — the
   two Linux killers are fixed: (a) REAPER's ALSA-via-PipeWire path
   can block the MAIN THREAD forever in snd_pcm_prepare, starving
   extension timers so every main-thread RPC hangs (ConnectionClosed);
   patch_ini now forces `linux_audio_mode=2` (Dummy) for test rigs,
   override with FTS_LINUX_AUDIO_MODE. (b) several
   ActionRegistration methods were stubs since the architect::rpc
   port — execute_named_action / execute_command / execute_action /
   list_actions / set_toggle_state / unregister are now real.
18. **daw REAPER suite — `just daw-reaper-test` GREEN (95 tests)**:
   daw-reaper-xtask (features/reaper/daw-reaper/xtask) builds an
   isolated rig at target/fts-reaper-test (only daw-bridge in
   UserPlugins), boots headless REAPER, runs
   features/reaper/daw-reaper/tests/reaper_*.rs — action registry
   (30), transport, tempo map, items, routing, ext-state, automation,
   dawfile, dock host, screenset, window geometry, multi-daw. The
   ActionRegistration port was completed for this (list_actions /
   execute_action / set_toggle_state / unregister / toggle-flip on
   trigger — all were stubs). KNOWN-BROKEN skips live in the xtask
   with reasons; the big one is the **stale-ReaProject crash class**:
   any suite that creates tracks/items/takes in an isolated tab and
   closes it (reaper_project_isolation, reaper_takes,
   fx_operations_…) panics daw-bridge off-main with reaper-rs
   "ReaProject doesn't exist anymore" and kills the daw runtime.
   Fix belongs in daw-reaper's pointer validation / whatever
   background path holds the closed tab (suspect: audio-sync hook /
   broadcast pollers). Smaller skips: vox response-schema bug on
   restore_layout; tempo add_point count drift on REAPER 7.75;
   toolbar button registration in the isolated rig; main-window
   nudge headless; 60s timer soak.
19. **CI (Forgejo Actions on codeberg)**: Actions ENABLED on the repo
   (was off). `.forgejo/workflows/ci.yml` runs on push/PR using
   codeberg's shared runners — but those cap at 4cpu/8GB/10min
   (codeberg-tiny/small/medium), which cannot cold-build this
   workspace, so the shared stage is only `cargo metadata --locked`
   (manifest + lockfile gate; the tree is NOT rustfmt-clean, so no fmt
   gate). `.forgejo/workflows/ci-heavy.yml` runs on push/PR/dispatch on
   the SELF-HOSTED runner: "THEBATTLESHIP", registered ORG-WIDE to
   FastTrackStudios (covers Task too — do not touch its registration),
   NixOS service `gitea-runner-codeberg-org` on this machine, defined
   in ~/.flake/hosts/THEBATTLESHIP/THEBATTLESHIP.nix. Labels: `nix-host`
   (HOST mode, user gitea-runner, nix on PATH — what ci-heavy targets),
   `docker` / `ubuntu-latest` (podman, /nix/store ro). Pipeline (all
   steps under `nix develop -c`): local-first checkout (clones from
   /run/media/Development/FastTrackStudio when it has the commit —
   Task's images.yml pattern), workspace check (--exclude vox-discover),
   test suites (signal-guitar, daw-standalone --features audio,
   neural-amp-modeler incl. NAM parity, architect --features iroh,
   pitch-dsp), wasm check of fasttrackstudio, then a REAPER job
   (daw-reaper-xtask 95 tests + fts-extensions-xtask 13 tests) against
   the pinned REAPER 7.75 store path (gcroot'd; nixpkgs 7.67 fallback),
   isolated rigs only — never ~/.config/signal, never port 4040.
   github.com egress is UNRELIABLE from this network (connects time
   out; codeberg/crates.io are fine), so the CI cargo-home git db is
   SEEDED from ~cody/.cargo/git/db (3GB rsync, chown gitea-runner) —
   with the committed lockfile cargo then never fetches github. If a
   new github git dep lands in Cargo.lock, re-seed the same way
   (`rsync -a ~cody/.cargo/git/db/ .../fts-ci/cargo-home/git/db/`).
   Persistent caches (gitea-runner-owned, NEVER the dev tree's target/):
   /var/lib/gitea-runner/codeberg-org/fts-ci/{target,cargo-home,reaper,
   reaper-rig,devshell}. daw::test::runner now honors CARGO_TARGET_DIR
   when locating built .so artifacts (was hardcoded <workspace>/target).
   REAPER's license forbids redistribution, so the REAPER stage can
   only ever run self-hosted.
   **FULLY GREEN as of 2eb89862** (run 10: build-and-test 5m, reaper
   job 8m30s — daw suite 3m05s, extension suite 5m23s). Three
   environment classes were fixed to get there, all of the
   works-on-the-dev-box-only kind: (a) fresh CARGO_HOME + dead github
   egress → seeded git db (above); (b) flake's
   AR_wasm32_unknown_unknown pointed at bintools-WRAPPER/bin/llvm-ar,
   which doesn't exist (wrapper exposes unprefixed `ar` only; a warm
   dev target/ never re-ran ring's build script so it never bit) →
   bintools-unwrapped, plus ci-heavy gcroots the dev shell
   (`nix develop --profile $FTS_CI/devshell`) because the wrapper path
   had ALSO been GC'd; (c) cross-user /tmp collisions — the harness's
   /tmp/fts-daw-reaper.log and /tmp/reaper-tests/ created by dev runs
   are unwritable for gitea-runner → REAPER log lives in the rig and
   per-test logs in log_dir() (FTS_TEST_LOG_DIR →
   <FTS_REAPER_RESOURCES>/test-logs → legacy /tmp path).
   Reading step logs on codeberg (REST API has none): POST
   /<owner>/<repo>/actions/runs/<run_number>/jobs/<idx>/attempt/1
   with {"logCursors":[{"step":N,"cursor":null,"expanded":true}]}
   (token auth works; empty logCursors returns the step list).

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
