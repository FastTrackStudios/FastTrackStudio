# FastTrackStudio Docs — docs.fasttrackstudio.app

The single unified docs site for the whole FTS stack. One dodeca site,
one section per domain:

```
content/
  _index.md    landing page — index of the domains
  keyflow/     chart notation language (moved wholesale from crates/keyflow/docs)
  signal/      signal-chain / rigs / sampler / plugins
  session/     transport + session coordination
  daw/         REAPER integration + DAW file formats
  architect/   the entity/RPC framework
  guide/       cross-domain guides (facet, styx, tracey, RPC, facade pattern)
```

## Toolchain

- **dodeca (`ddc`)** — bearcove's static site generator; config in
  `.config/dodeca.styx`, Tera templates in `templates/`, assets in
  `static/`. Install via bearcove's install script (prebuilt binaries).
- **`kf docs`** (keyflow-cli, in this workspace) — pre-render stage that
  turns every ```` ```kf ```` fenced block in `content/` into inline SVG
  under `.rendered/` (a generated mirror tree) and writes the shared
  engraving-font stylesheet to `static/kf-fonts.css`. dodeca builds the
  rendered tree, never the raw authored content.

## Commands

```bash
./build.sh        # kf docs + ddc build → output/
./serve.sh        # live-reload dev server on :8080
./deploy.sh       # build + fly deploy (app: fts-docs)
```

Or from the repo root: `just docs-build` / `just docs-serve`.

## Deployment

Fly.io app `fts-docs`, served by static-web-server from the `output/`
directory (see `Dockerfile` + `fly.toml`). Domain:
**docs.fasttrackstudio.app**.

One-time setup:

```bash
fly apps create fts-docs
fly certs add docs.fasttrackstudio.app -a fts-docs
# point Cloudflare DNS at fts-docs.fly.dev (see fly certs show)
```

### keyflow.fasttrackstudio.app redirect

The old per-project docsite at **keyflow.fasttrackstudio.app** (fly app
`keyflow-docs`) is superseded by this site's `/keyflow/` section. Keep a
redirect from `keyflow.fasttrackstudio.app/*` →
`docs.fasttrackstudio.app/keyflow/*` (Cloudflare redirect rule, or a
tiny redirect config on the old fly app) before decommissioning
`keyflow-docs`.
