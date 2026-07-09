# attic/keyflow-apps — parked crates from the dissolved keyflow/ workspace

Parked here during the wave-3 restructure (LAYOUT.md park/delete policy).
These directories are OUTSIDE every Cargo workspace — they do not build.
Revive one by moving it into apps/ (or examples/) and adding it to the root
`[workspace] members`, converting its old relative `path = "…"` deps to
`x.workspace = true`.

| dir | was | why parked |
|---|---|---|
| `web-editor` | keyflow/examples/web-editor | wasm-only standalone chart editor demo built on the Editor.git widget stack; reviving it also needs keyflow's old `[patch.crates-io] mermaid-rs-renderer` (→ Editor.git) and the `getrandom_backend="wasm_js"` rustflag from keyflow's .cargo/config.toml |
| `keyflow-xtask` | keyflow/xtask | keyflow workspace task runner (`cargo xtask`); depended on the fts-repo git dep and the dissolved workspace's alias |

The old workspace's crates moved to `crates/keyflow/*`,
`features/engraver/{engraver,proto}` and `apps/keyflow-cli`; its docs,
skills, and example chart data moved to `crates/keyflow/{docs,skills,examples}/`.
