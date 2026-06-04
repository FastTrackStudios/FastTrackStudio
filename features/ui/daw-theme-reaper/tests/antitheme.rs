//! Integration test against the real White Tie Anti-Theme (the REAPER 7
//! default theme rebuilt human-readable) — our first import target.
//!
//! Looks for the unpacked theme via `$REAPER_ANTITHEME_DIR` or the
//! reaper-theme repo's extraction path; **skips** (passes) when absent so CI
//! without the theme corpus stays green.

use daw_theme_reaper::ReaperTheme;

fn antitheme() -> Option<ReaperTheme> {
    let candidates = [
        std::env::var("REAPER_ANTITHEME_DIR").ok(),
        Some("/home/cody/Development/FastTrackStudio/reaper-theme/extracted/antitheme".to_string()),
    ];
    for dir in candidates.into_iter().flatten() {
        if std::path::Path::new(&dir).is_dir()
            && let Ok(theme) = ReaperTheme::load_dir(&dir)
        {
            return Some(theme);
        }
    }
    eprintln!("anti-theme not found — skipping");
    None
}

#[test]
fn loads_palette_params_and_images() {
    let Some(theme) = antitheme() else { return };

    // Palette: a few known [color theme] keys decode.
    assert!(
        theme.palette.len() > 100,
        "palette has {}",
        theme.palette.len()
    );
    let arrange = theme.palette.color("col_arrangebg").expect("col_arrangebg");
    assert_eq!((arrange.r, arrange.g, arrange.b), (0x45, 0x45, 0x45));
    assert!(theme.palette.color("col_seltrack").is_some());

    // rtconfig: version global + the documented adjuster knobs.
    assert!(theme.rtconfig.global_f32("version").is_some());
    let names: Vec<&str> = theme
        .rtconfig
        .params
        .iter()
        .map(|p| p.name.as_str())
        .collect();
    assert!(names.contains(&"textBrightness"), "params: {names:?}");
    assert!(
        names.contains(&"customColorDepthParam"),
        "params: {names:?}"
    );

    // Images: the track/gen button vocabulary is present.
    for name in [
        "track_mute_off",
        "track_mute_on",
        "track_solo_off",
        "gen_mute_off",
    ] {
        assert!(theme.images.has(name), "missing image {name}");
    }
}

#[test]
fn slices_anti_theme_buttons_and_faders() {
    let Some(theme) = antitheme() else { return };

    // track_mute_off: plain 60×20 → three 20×20 states.
    let mute = theme
        .images
        .button3("track_mute_off")
        .expect("track_mute_off");
    assert_eq!(mute.normal.dimensions(), (20, 20));
    assert_eq!(mute.pressed.dimensions(), (20, 20));

    // mcp_io: pink-lined 62×34 (3N+2 rule) — left line + lone lower-right
    // corner → content 60 wide → three 20px states.
    let io = theme.images.button3("mcp_io").expect("mcp_io");
    assert_eq!(io.normal.dimensions(), (20, 33));

    // mcp_volbg: full marker ring, 26×22 → 24×20 content with 9-slice margins.
    let volbg = theme.images.load("mcp_volbg").expect("mcp_volbg");
    assert_eq!(volbg.image.dimensions(), (24, 20));
    assert!(volbg.markers.fixed_left > 0 && volbg.markers.fixed_right > 0);

    // mcp_volthumb: right-line markers → vertical fixed caps.
    let thumb = theme.images.load("mcp_volthumb").expect("mcp_volthumb");
    assert_eq!(thumb.image.dimensions(), (23, 53));
    assert!(thumb.markers.fixed_top > 0 && thumb.markers.fixed_bottom > 0);

    // Meter strips (the general meter vocabulary the Anti-Theme uses).
    assert!(theme.images.has("meter_strip_v"));
    assert!(theme.images.has("meter_bg_v"));
}
