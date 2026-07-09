# Keyflow docs → apps/docs-site

The keyflow docsite content, templates, and deploy infra moved to the
unified FTS docs site: **`apps/docs-site/`** (served at
docs.fasttrackstudio.app, keyflow section at `/keyflow/`).

- Authored pages: `apps/docs-site/content/keyflow/`
- Build/serve/deploy: `apps/docs-site/{build,serve,deploy}.sh` or
  `just docs-build` / `just docs-serve` from the repo root
- The old keyflow.fasttrackstudio.app site (fly app `keyflow-docs`)
  should redirect to docs.fasttrackstudio.app/keyflow/ — see
  `apps/docs-site/README.md`

What stays here is crate-internal documentation (not site content):

- `ARCHITECTURE.md` — internal architecture of the keyflow crates
- `IMPROVEMENTS.md` — engraving-quality punch list
- `MELODY_PIPELINE.md` — melody pipeline implementation notes
