# FastTrackStudio REAPER theme

The FastTrackStudio REAPER theme, kept **unpacked** in-tree so it is
editable like source instead of like a binary blob.

```
features/reaper/fts-theme/
  FastTrackStudio.ReaperTheme     colors + fonts (INI; ~420 keys)
  FastTrackStudio/                images + WALTER layout
    rtconfig.txt                  THE layout file (~3.7k lines of WALTER)
    *.png                         ~1000 100%-scale images
    150/  200/                    HiDPI variants (same filenames)
    alt/ strip/ separator/        layout-specific image sets
    blue/ green/ …/ purple/       accent-color variants
    transport_classic/            alternate transport button set
    chordbox.ini  gridbox.ini     REAPER 7 chord/grid box styling
```

## Provenance and licensing

This is a fork of the **Reapertips Theme v1.93 (Dark)** by Alejandro
Hernandez and FeedTheCat (Ilias-Timon Poulakis) — <https://reapertips.com>
— which is itself derived from REAPER's stock `Default_6.0` theme.

The Reapertips theme is a **commercial product** and ships no license
file. Treat this directory as an internal fork: fine to use and modify
for our own rigs, **not** fine to publish or bundle into an FTS release
until we either get written permission from the Reapertips authors or
replace the borrowed artwork. `just reaper theme-pack` exists for local
distribution to our own machines, not for shipping.

## Dev loop

```sh
just reaper theme-install     # symlink into $REAPER_HOME/ColorThemes
just reaper theme-uninstall
just reaper theme-pack        # dist/FastTrackStudio.ReaperThemeZip
```

`theme-install` symlinks the *unpacked* folder — REAPER loads a theme
from `ColorThemes/<name>/` + `ColorThemes/<name>.ReaperTheme` exactly
like it loads a `.ReaperThemeZip`, so edits in this repo are live. Then
in REAPER, `Options > Themes > FastTrackStudio`, and while iterating:

| Action (search in the Actions list) | What it does |
|---|---|
| `Theme development: Reload theme` | re-reads `rtconfig.txt` + PNGs |
| `Theme development: Show theme tweak/configuration window` | live WALTER editor; writes back to `rtconfig.txt` |
| `Theme development: Show theme element/parameter under mouse cursor` | tells you which WALTER element you're hovering |
| `Options > Themes > Theme adjuster` | the end-user knob panel (see below) |

Reload does not pick up `.ReaperTheme` color changes reliably — re-select
the theme for those.

## What lives where

### `FastTrackStudio.ReaperTheme` — colors and fonts

Plain INI. `[color theme]` holds ~400 keys as **BGR integers** (not RGB
hex): `col_main_bg`, `col_arrangebg`, `col_tcp_text`, `col_env1..16`,
peak/waveform colors, etc. Negative values mean "unset / inherit". The
`[REAPER]` section holds `ui_img=FastTrackStudio` (which image folder to
use — **must match the folder name**) plus the base64 font blobs
(`tl_font`, `trans_font`, `user_font0..15`, …). Fonts are Fira Sans and
Roboto here; the blobs are platform-encoded, so edit them through
REAPER's color/font dialogs rather than by hand.

### `rtconfig.txt` — globals + WALTER

Two halves:

1. **Globals** (top of file, before `; ---- WALTER ----`): non-WALTER
   switches — `version 6.0`, `use_pngs 1`, `tcp_heights 4 26 61 61`,
   `mcp_min_height 215`, `misc_dpi_translate 134 150` (which DPI
   threshold loads the `150/` images), `adjuster_script "…lua"`.
2. **WALTER** — Window Arrangement Logic Template Engine for REAPER. A
   declarative layout language: every UI element gets a rectangle
   `[x y w h ls ts rs bs]` (position, size, then four edge-attachment
   factors that make it stretch with its parent).

Key WALTER constructs used in this file:

- `set <element> <rect>` — place an element. `clear <element>` hides it.
- `def` / `macro` — preprocessor substitution and reusable parameterized
  blocks; this theme leans on macros heavily to build the TCP/MCP rows.
- `define_parameter <name> '<label>' <default> <min> <max>` — exposes a
  knob to the **Theme Adjuster**. That's the mechanism behind every
  "Global: Tint track name", "Transport: Docked height" toggle.
- `Layout "A" … EndLayout` — named layouts, selectable per track. The
  `150%_A` / `200%_A` variants take a second argument (`"150"`) naming
  the image subfolder they read from. `GlobalLayout` promotes a layout
  into the global list.
- Expressions: `w<100 [0 0 10 10] [20 20 10 10]` — conditionals on
  predefined scalars (`w`, `h`, `recarm`, `folderstate`, `tracknch`,
  `os_type`, …). Operators `< > <= >= == != &`, plus `+ - * /`.
- Element namespaces: `tcp.*`, `mcp.*`, `master.tcp.*`, `master.mcp.*`,
  `envcp.*`, `trans.*`, `item.*`.

### Images

Filename prefixes map to element groups: `tcp_`, `mcp_`, `envcp_`,
`transport_`, `track_`, `item_`, `meter_`, `toolbar_`, `gen_`. Buttons
are three-state sprites stacked in one PNG (normal / hover / pressed).
Stretchable images carry marker pixels: **magenta** `#FF00FF` marks the
non-stretched corner regions (nine-slice guides), **yellow** `#FFFF00`
marks the outer extent. `150/` and `200/` hold the same filenames at
1.5x and 2x; item images may instead use a `_hidpi` suffix.

`custom_*.png` are this theme's own additions, referenced from
`rtconfig.txt` via WALTER's `custom` command (REAPER 7+), which is how
elements REAPER doesn't natively theme get drawn.

## Reference

- [WALTER reference](https://www.reaper.fm/sdk/walter/walter.php) — the
  official command/variable list. Terse but complete.
- [Theme images](https://www.reaper.fm/sdk/walter/images.php) — every
  image filename REAPER looks for, with dimensions.
- [WALTER: A themer's guide](https://www.houseofwhitetie.com/reaper/walter_themers_guide.pdf)
  (White Tie) — the actual tutorial; read this one first.
- [Customizing the REAPER 6 theme](https://reaper.blog/2020/01/custom-reaper-6-theme/)
  — The REAPER Blog's practical walkthrough.
- [WALTER: Customisation in REAPER](https://www.soundonsound.com/techniques/walter-customisation-reaper)
  — Sound On Sound overview.
- [Reapertips Theme — complete guide](https://www.reapertips.com/post/reapertips-theme-complete-guide)
  — documents the adjuster parameters this fork inherits.
