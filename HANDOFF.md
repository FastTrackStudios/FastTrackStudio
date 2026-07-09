# Handoff — state & outstanding work (2026-07-09)

Repo: codeberg `FastTrackStudios/FastTrackStudio`, local
`/run/media/Development/FastTrackStudio`. ONE root workspace
(~195 members): `apps/ crates/ features/ libs/ docs/`. Everything
builds from root; `nix develop` is the environment; `just` has the
tailwind/site/docs recipes. Read CLAUDE.md (rules) and LAYOUT.md
(placement map) before changing structure.

## Live state on this machine

- The **live guitar rig** still runs a PRE-restructure binary out of
  the stray gitignored `signal/target/` dir (kept alive on purpose).
  Next restart: `cargo build -p signal-engine && ./target/debug/signal-engine`
  (or `fts signal engine`), then delete the leftover `signal/` dir.
  Rig config: `~/.config/signal/rig/*.styx` (profile, songs, setlists,
  keymap, midi map — all text, reload with the ↻ header button).
- Web remote serves from `target/dx/signal-web/...` via
  `python3 -m http.server 8093` (see root Justfile).
- **Voyager (mac)** still has the OLD multi-repo clones + bootstrap
  script (~/voyager-bootstrap.sh) — must be re-pointed: fresh clone of
  the monorepo, `cargo build --release -p signal-engine`, re-sync the
  web bundle. Its rig config/NAM paths were already migrated to
  /Users/codywright.

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
9. **fasttrackstudio app polish**: Rig tab embeds GuitarRigRemote (works);
   Session tab plays the demo setlist with the guide clicking. Next:
   real setlists from ~/.config/signal styx into the session engine,
   guide TTS cue bank wiring in-app, engine log surfacing in the
   Engines panel.

## Gotchas that will bite again

- Dead-repo `[patch]` tables only resolve with a cached git db /
  committed lockfile (cargo fetches patched sources on fresh resolve).
  Keep `Cargo.lock` committed. Remaining such table: keyflow.git
  (Editor.git references it).
- `pkill -f <pattern>` kills your own shell if the pattern appears in
  the command line (exit 144). Use narrowed patterns (`signal-engine$`).
- Tailwind: run `just tailwind` BEFORE `dx build` whenever UI classes
  change.
- One blitz rev tree-wide (`links = servo_style_crate` collisions);
  currently 727dab01.
