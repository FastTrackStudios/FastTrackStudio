//! Offline audio self-test for the CSS 1st Violins rig — no audio device, no
//! MIDI. Loads the real library, warms a note, triggers it, renders offline,
//! and reports whether any audio came out. Diagnoses "MIDI works but silent".
//!
//! ```text
//! cargo run -p signal-sampler --example strings_selftest
//! ```

use std::path::PathBuf;

use signal_sampler::SamplerRig;

const CSS_ROOT: &str =
    "/run/media/AudioHaven/Sampled/Orchestral/Cinematic Series/Cinematic Studio Strings";
const CSS_CONFIG: &str =
    "/run/media/Development/FastTrackStudio/sample-collector/specs/cinematic-strings.styx";
const ID: &str = "strings_1v";

fn main() -> eyre::Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "signal_sampler=debug".into()),
        )
        .with_ansi(false)
        .init();

    let css_root = PathBuf::from(CSS_ROOT);
    let spec = css_root
        .join("_patches")
        .join("1st Violins")
        .join("library.styx");
    let config = PathBuf::from(CSS_CONFIG);

    println!("zones spec : {}  (exists: {})", spec.display(), spec.exists());
    println!("config spec: {}  (exists: {})", config.display(), config.exists());

    let rig = SamplerRig::new_offline_with_cache_budget(48_000, Some(6 * 1024 * 1024 * 1024));
    rig.load_instrument_with_config(ID, &config, &spec, &css_root, "1st Violins", "Mix")?;
    rig.set_solo_mic(ID, Some("Mix".into()));
    rig.set_articulation(ID, "Nonvib");
    rig.cc(ID, 1, 90);
    rig.cc(ID, 2, 90);
    println!("articulation: {:?}", rig.articulation(ID));

    let note = 60u8;
    let w = rig.warm_note(ID, note);
    println!(
        "warm note {note}: loaded={} failed={} bytes={}",
        w.loaded, w.failed, w.bytes
    );

    rig.note_on(ID, note, 100);
    let frames = 48_000usize; // 1 s stereo
    let mut out = vec![0.0f32; frames * 2];
    rig.render_offline(&mut out)?;

    let peak = out.iter().fold(0f32, |m, &s| m.max(s.abs()));
    let rms = (out.iter().map(|&s| s * s).sum::<f32>() / out.len() as f32).sqrt();
    println!(
        "after note_on: voices={}  peak={peak:.5}  rms={rms:.6}",
        rig.active_voices(ID)
    );

    if peak > 0.0 {
        println!("✅ direct note_on path produces audio.");
    } else {
        println!("❌ direct note_on path SILENT — see warm/voice counts above.");
    }

    // Exercise the LIVE routing path: rig.midi_message → bank.midi_message,
    // routed by channel. This is what hardware MIDI hits in the live rig; it was
    // dropping notes when no MIDI-channel mapping existed.
    rig.panic(ID);
    let note2 = 67u8; // G4 (a recorded odd key)
    rig.warm_note(ID, note2);
    rig.midi_message(0, 0x90, note2, 100); // note-on, channel 0
    let mut out2 = vec![0.0f32; frames * 2];
    rig.render_offline(&mut out2)?;
    let peak2 = out2.iter().fold(0f32, |m, &s| m.max(s.abs()));
    println!(
        "live routing (midi_message ch0): voices={}  peak={peak2:.5}",
        rig.active_voices(ID)
    );

    if peak > 0.0 && peak2 > 0.0 {
        println!("\n✅ BOTH paths produce audio. If the live rig is still silent,");
        println!("   the issue is output-device routing (check the OUTPUT meter / qpwgraph).");
    } else if peak > 0.0 {
        println!("\n⚠️  Engine works but the LIVE channel-routing path is silent —");
        println!("   that's the bug that mutes the hardware-MIDI rig.");
    } else {
        println!("\n❌ SILENT — no audio from the engine. See warm/voice counts above.");
    }
    Ok(())
}
