//! Render a small project through `ProjectRenderer`, exercising:
//! - Item playback (with fade-in/out)
//! - Track volume / pan / mute / solo
//! - Track-to-track sends
//! - parent_send_enabled
//!
//! No cpal involved — pure number-crunching, so this test works the
//! same on native and WASM.

#![cfg(feature = "decode")]

use daw_proto::midi::Midi;
use daw_proto::primitives::Duration as ProtoDuration;
use daw_proto::project::ProjectContext;
use daw_proto::{ItemRef, Items, ProjectInfo, RouteRef, RouteType, Routing, TrackRef, Tracks};
use daw_standalone::audio_engine::DecodedAudio;
use daw_standalone::audio_engine::materialize::attach_audio_source;
use daw_standalone::audio_engine::render::ProjectRenderer;
use daw_standalone::sync::Standalone;

const SAMPLE_RATE: u32 = 48_000;

fn seeded() -> (Standalone, String) {
    let daw = Standalone::new();
    let guid = daw.seed_project(ProjectInfo {
        guid: "p".into(),
        name: "p".into(),
        path: String::new(),
    });
    (daw, guid)
}

/// Synth a 1s constant-amplitude mono buffer at value `v`.
fn const_audio(v: f32) -> DecodedAudio {
    let frames = SAMPLE_RATE as usize;
    DecodedAudio {
        samples: vec![v; frames],
        channels: 1,
        sample_rate: SAMPLE_RATE,
    }
}

fn create_item_with_audio(
    daw: &Standalone,
    project_guid: &str,
    track_guid: &str,
    start_seconds: f64,
    length_seconds: f64,
    audio: DecodedAudio,
) -> (String, String) {
    let ctx = ProjectContext::Project(project_guid.to_string());
    // Make a MIDI item then convert to audio so we can wire a take
    // GUID we control.
    let loc = Midi::create_midi_item(
        daw,
        ctx.clone(),
        TrackRef::Guid(track_guid.to_string()),
        start_seconds,
        start_seconds + length_seconds,
    )
    .expect("midi item created");
    let item_guid = match &loc.item {
        ItemRef::Guid(g) => g.clone(),
        _ => panic!(),
    };
    // Flip the active take to audio + attach our synthetic source.
    let active =
        daw_proto::Takes::get_active_take(daw, ctx.clone(), ItemRef::Guid(item_guid.clone()))
            .unwrap();
    daw.write_project(project_guid, |p| {
        for tl in p.takes.values_mut() {
            for t in tl.takes.iter_mut() {
                if t.guid == active.guid {
                    t.is_midi = false;
                    t.source_type = daw_proto::item::SourceType::Audio;
                    t.source_file_path = None; // already decoded
                }
            }
        }
    });
    attach_audio_source(daw, project_guid, &active.guid, audio);
    (item_guid, active.guid)
}

fn rms_l(buf: &daw_standalone::audio_engine::render::StereoBuffer) -> f32 {
    let mut s = 0.0;
    for i in 0..buf.frames {
        let x = buf.samples[i * 2] as f64;
        s += x * x;
    }
    ((s / buf.frames.max(1) as f64).sqrt()) as f32
}

fn rms_r(buf: &daw_standalone::audio_engine::render::StereoBuffer) -> f32 {
    let mut s = 0.0;
    for i in 0..buf.frames {
        let x = buf.samples[i * 2 + 1] as f64;
        s += x * x;
    }
    ((s / buf.frames.max(1) as f64).sqrt()) as f32
}

#[test]
fn renders_single_item_to_master() {
    let (daw, guid) = seeded();
    let ctx = ProjectContext::Current;
    let t = Tracks::add(&daw, ctx, "T", None).unwrap();
    create_item_with_audio(&daw, &guid, &t, 0.0, 1.0, const_audio(0.5));

    let r = ProjectRenderer::new(&daw, &guid, SAMPLE_RATE);
    // 0.5 s of audio.
    let block = r.render_block(0, (SAMPLE_RATE / 2) as usize);
    // Constant 0.5 → after center pan + unity volume the L/R each
    // see 0.5 * sqrt(0.5) = ~0.354.
    let target = 0.5 * (0.5_f32).sqrt();
    assert!(
        (rms_l(&block) - target).abs() < 0.05,
        "L rms={}, target={target}",
        rms_l(&block)
    );
    assert!(
        (rms_r(&block) - target).abs() < 0.05,
        "R rms={}, target={target}",
        rms_r(&block)
    );
}

#[test]
fn track_pan_routes_to_correct_side() {
    let (daw, guid) = seeded();
    let ctx = ProjectContext::Current;
    let t = Tracks::add(&daw, ctx.clone(), "T", None).unwrap();
    create_item_with_audio(&daw, &guid, &t, 0.0, 1.0, const_audio(0.5));
    Tracks::set_pan(&daw, ctx, TrackRef::Guid(t), 1.0).unwrap(); // hard right

    let block =
        ProjectRenderer::new(&daw, &guid, SAMPLE_RATE).render_block(0, SAMPLE_RATE as usize / 2);
    assert!(rms_l(&block) < 0.01, "L should be silent on hard-right pan");
    assert!(
        rms_r(&block) > 0.4,
        "R should be loud, got {}",
        rms_r(&block)
    );
}

#[test]
fn muted_track_contributes_silence() {
    let (daw, guid) = seeded();
    let ctx = ProjectContext::Current;
    let t = Tracks::add(&daw, ctx.clone(), "T", None).unwrap();
    create_item_with_audio(&daw, &guid, &t, 0.0, 1.0, const_audio(0.5));
    Tracks::set_muted(&daw, ctx, TrackRef::Guid(t), true).unwrap();

    let block =
        ProjectRenderer::new(&daw, &guid, SAMPLE_RATE).render_block(0, SAMPLE_RATE as usize / 4);
    assert!(rms_l(&block) < 0.001);
    assert!(rms_r(&block) < 0.001);
}

#[test]
fn solo_isolates_track() {
    let (daw, guid) = seeded();
    let ctx = ProjectContext::Current;
    let a = Tracks::add(&daw, ctx.clone(), "A", None).unwrap();
    let b = Tracks::add(&daw, ctx.clone(), "B", None).unwrap();
    create_item_with_audio(&daw, &guid, &a, 0.0, 1.0, const_audio(0.5));
    create_item_with_audio(&daw, &guid, &b, 0.0, 1.0, const_audio(0.5));

    Tracks::set_soloed(&daw, ctx, TrackRef::Guid(a), true).unwrap();
    let block =
        ProjectRenderer::new(&daw, &guid, SAMPLE_RATE).render_block(0, SAMPLE_RATE as usize / 4);
    // Only A contributes; B's content is suppressed.
    let l = rms_l(&block);
    let target = 0.5 * (0.5_f32).sqrt();
    assert!((l - target).abs() < 0.05, "solo L rms={l}, target={target}");
}

#[test]
fn fade_in_attenuates_block_start() {
    let (daw, guid) = seeded();
    let ctx = ProjectContext::Current;
    let t = Tracks::add(&daw, ctx.clone(), "T", None).unwrap();
    let (item, _take) = create_item_with_audio(&daw, &guid, &t, 0.0, 1.0, const_audio(1.0));
    // 200ms linear fade-in.
    Items::set_fade_in(
        &daw,
        ctx,
        ItemRef::Guid(item),
        ProtoDuration::from_seconds(0.2),
        daw_proto::item::FadeShape::Linear,
    )
    .unwrap();

    // Render 100ms at the very start — fade environment is 0 → 0.5.
    let r = ProjectRenderer::new(&daw, &guid, SAMPLE_RATE);
    let early = r.render_block(0, SAMPLE_RATE as usize / 10); // 100ms
    // Render 100ms past the fade-in — full level.
    let later = r.render_block((SAMPLE_RATE as u64) * 3 / 10, SAMPLE_RATE as usize / 10); // start at 300ms

    let target = (0.5_f32).sqrt(); // gain 1.0 then center pan
    assert!(
        rms_l(&early) < rms_l(&later),
        "fade-in early ({}) should be quieter than past-fade ({})",
        rms_l(&early),
        rms_l(&later)
    );
    assert!(
        rms_l(&later) > target * 0.8,
        "past-fade L should approach unity"
    );
}

#[test]
fn send_routes_audio_into_destination_bus() {
    let (daw, guid) = seeded();
    let ctx = ProjectContext::Current;
    let src = Tracks::add(&daw, ctx.clone(), "Src", None).unwrap();
    let bus = Tracks::add(&daw, ctx.clone(), "Bus", None).unwrap();
    create_item_with_audio(&daw, &guid, &src, 0.0, 1.0, const_audio(1.0));

    // Disable Src's parent send so it ONLY reaches master via Bus.
    Routing::set_parent_send_enabled(&daw, ctx.clone(), TrackRef::Guid(src.clone()), false)
        .unwrap();

    // Without the send: master is silent (Src has no parent send, Bus
    // has no items).
    let r = ProjectRenderer::new(&daw, &guid, SAMPLE_RATE);
    let pre = r.render_block(0, SAMPLE_RATE as usize / 4);
    assert!(
        rms_l(&pre) < 0.001 && rms_r(&pre) < 0.001,
        "expected silent master without send"
    );

    // Add send Src → Bus.
    Routing::add_send(
        &daw,
        ctx.clone(),
        TrackRef::Guid(src.clone()),
        TrackRef::Guid(bus.clone()),
    )
    .unwrap();

    let post = r.render_block(0, SAMPLE_RATE as usize / 4);
    assert!(
        rms_l(&post) > 0.1,
        "send should bring audio to master via Bus"
    );

    // Mute the send → master goes silent again.
    Routing::set_muted(
        &daw,
        ctx,
        daw_proto::RouteLocation {
            track: TrackRef::Guid(src),
            route_type: RouteType::Send,
            route: RouteRef::Index(0),
        },
        true,
    )
    .unwrap();
    let muted = r.render_block(0, SAMPLE_RATE as usize / 4);
    assert!(rms_l(&muted) < 0.001, "muted send should be silent");
}

#[test]
fn parent_send_disabled_excludes_track_from_master() {
    let (daw, guid) = seeded();
    let ctx = ProjectContext::Current;
    let t = Tracks::add(&daw, ctx.clone(), "T", None).unwrap();
    create_item_with_audio(&daw, &guid, &t, 0.0, 1.0, const_audio(1.0));
    Routing::set_parent_send_enabled(&daw, ctx, TrackRef::Guid(t), false).unwrap();

    let block =
        ProjectRenderer::new(&daw, &guid, SAMPLE_RATE).render_block(0, SAMPLE_RATE as usize / 4);
    assert!(rms_l(&block) < 0.001);
}

#[test]
fn item_position_shifts_into_block() {
    let (daw, guid) = seeded();
    let ctx = ProjectContext::Current;
    let t = Tracks::add(&daw, ctx, "T", None).unwrap();
    // Item starts at 0.5s, lasts 0.5s.
    create_item_with_audio(&daw, &guid, &t, 0.5, 0.5, const_audio(1.0));

    let r = ProjectRenderer::new(&daw, &guid, SAMPLE_RATE);
    // Render first 0.25s — silent.
    let early = r.render_block(0, SAMPLE_RATE as usize / 4);
    assert!(rms_l(&early) < 0.001, "early block should be silent");
    // Render 0.5-0.75s — audible.
    let mid = r.render_block(SAMPLE_RATE as u64 / 2, SAMPLE_RATE as usize / 4);
    assert!(
        rms_l(&mid) > 0.3,
        "mid block should be audible: {}",
        rms_l(&mid)
    );
}
