# attic/dynamic-template-xtask — parked from the subtree-imported dynamic-template

Parked here during the wave-3 restructure (LAYOUT.md park/delete policy).
This directory is OUTSIDE every Cargo workspace — it does not build.

| dir | was | why parked |
|---|---|---|
| `xtask` | dynamic-template/xtask | workspace task runner (`cargo xtask`); depended on `fts-devtools` from the dead daw.git remote and the fts-repo git dep |

The dynamic-template crates live at `features/dynamic-template/`
(dynamic-template, dynamic-template-proto, dynamic-template-extension,
music-catalog, color-palette, tests).
