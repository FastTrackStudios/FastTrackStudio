//! Export a sampled folder as a DecentSampler `.dspreset`.
//!
//! Purpose is diagnostic: play the *same WAV files* through an independent,
//! known-good sampler. If a pack sounds wrong in both, the recordings are at
//! fault; if it sounds right in DecentSampler, the problem is our engine. That
//! separation is otherwise very hard to get at.
//!
//! The mapping is almost 1:1 with [`ZoneSpec`], because both formats describe
//! the same thing — a sample placed at a key range × velocity range with
//! optional loop points. Notably `loopCrossfade` is in **frames** in both, so
//! it transfers without conversion.
//!
//! | `ZoneSpec`   | DecentSampler   |
//! |--------------|-----------------|
//! | `file`       | `path`          |
//! | `root_key`   | `rootNote`      |
//! | `key_min`    | `loNote`        |
//! | `key_max`    | `hiNote`        |
//! | `vel_min`    | `loVel`         |
//! | `vel_max`    | `hiVel`         |
//! | `loop_start` | `loopStart`     |
//! | `loop_end`   | `loopEnd`       |
//! | `loop_xfade` | `loopCrossfade` |
//! | `gain_db`    | `volume` (dB)   |
//! | `tune_cents` | `tuning` (semitones) |
//!
//! Format reference: <https://decentsampler-developers-guide.readthedocs.io/en/latest/the-groups-element.html>

use std::fmt::Write as _;
use std::path::{Path, PathBuf};

use eyre::{Result, WrapErr, eyre};
use signal_sampler::spec::LibrarySpec;

/// Gain, in dB, that cancels DecentSampler's velocity tracking at the velocity
/// a zone was actually recorded at.
///
/// With `ampVelTrack="1"` the engine multiplies amplitude by `velocity/127`. A
/// zone recorded at velocity `V` already *sounds* like `V`, so left alone it
/// would be scaled down a second time. Pre-boosting it by `127/V` makes the two
/// cancel exactly at `V` — so the recorded velocity plays back at its recorded
/// level, and velocities either side of it ramp smoothly instead of stepping.
///
/// ```text
///   level
///     │        ┌───── layer 3 ─────
///     │   ┌────┘ ramp between layers
///     │ ──┘ layer 2
///     └──────────────────────────► velocity
/// ```
fn velocity_compensation_db(recorded_velocity: u8) -> f32 {
    let v = recorded_velocity.max(1) as f32;
    20.0 * (127.0 / v).log10()
}

/// Render the `.dspreset` XML for a library spec.
///
/// `amp_vel_track` is written explicitly rather than left to the engine's
/// default, because that default is undocumented.
///
/// - **0** — velocity does not affect volume. Each layer plays at exactly its
///   recorded level, so dynamics are stepped: every velocity inside a band
///   sounds identical.
/// - **1** — volume tracks velocity continuously, and each zone is pre-boosted
///   by [`velocity_compensation_db`] so the tracking cancels at the velocity it
///   was recorded at. Dynamics become smooth *between* the sampled layers,
///   which is what makes a small number of layers playable.
pub fn render(spec: &LibrarySpec, amp_vel_track: f32) -> String {
    let mut s = String::new();
    let _ = writeln!(s, r#"<?xml version="1.0" encoding="UTF-8"?>"#);
    let _ = writeln!(
        s,
        r#"<!-- {} — exported from a .signalpack by fts signal sample export-decent -->"#,
        escape(&spec.name)
    );
    let _ = writeln!(s, r#"<DecentSampler minVersion="1.0.0">"#);
    let _ = writeln!(
        s,
        r#"  <ui width="812" height="375" bgColor="FF1F1F1F"></ui>"#
    );
    let _ = writeln!(s, r#"  <groups ampVelTrack="{amp_vel_track}">"#);
    let _ = writeln!(s, r#"    <group>"#);

    for z in &spec.zones {
        let mut attrs = format!(
            r#"path="{}" rootNote="{}" loNote="{}" hiNote="{}" loVel="{}" hiVel="{}""#,
            escape(&z.file),
            z.root_key,
            z.key_min,
            z.key_max,
            z.vel_min,
            z.vel_max,
        );
        // Only emit a loop when there is one. `loopEnd` defaults to the end of
        // the file, so writing loopStart=0/loopEnd=0 would not mean "no loop" —
        // it would mean something else entirely.
        if z.loop_end > z.loop_start + 1 {
            let _ = write!(
                attrs,
                r#" loopEnabled="true" loopStart="{}" loopEnd="{}""#,
                z.loop_start, z.loop_end
            );
            if z.loop_xfade > 0 {
                let _ = write!(attrs, r#" loopCrossfade="{}""#, z.loop_xfade);
            }
        }
        // The zone's own gain, plus whatever is needed to cancel velocity
        // tracking at the velocity this zone was recorded at. `vel_max` is that
        // velocity: the sampler strikes each band at its ceiling.
        let gain_db = z.gain_db + amp_vel_track * velocity_compensation_db(z.vel_max);
        if gain_db.abs() > 0.001 {
            let _ = write!(attrs, r#" volume="{gain_db:.2}dB""#);
        }
        if z.tune_cents != 0.0 {
            // DecentSampler tunes in semitones; we store cents.
            let _ = write!(attrs, r#" tuning="{}""#, z.tune_cents / 100.0);
        }
        if z.pan != 0.0 {
            let _ = write!(attrs, r#" pan="{}""#, z.pan * 100.0);
        }
        let _ = writeln!(s, "      <sample {attrs} />");
    }

    let _ = writeln!(s, r#"    </group>"#);
    let _ = writeln!(s, r#"  </groups>"#);
    let _ = writeln!(s, r#"</DecentSampler>"#);
    s
}

/// Export the `library.styx` in `samples_dir` as a `.dspreset` beside it.
///
/// The preset is written *into* the sample folder so every `path` is a bare
/// filename — no copying several hundred megabytes of audio just to audition
/// it, and nothing to keep in sync.
pub fn export(samples_dir: &Path, out: Option<PathBuf>, amp_vel_track: f32) -> Result<PathBuf> {
    let styx = samples_dir.join("library.styx");
    if !styx.exists() {
        return Err(eyre!(
            "no library.styx in {} — export needs the sampled folder, not the pack \
             (the pack's audio is FLAC inside the container; DecentSampler needs \
             the WAVs on disk)",
            samples_dir.display()
        ));
    }
    let spec = LibrarySpec::from_file(&styx)
        .map_err(|e| eyre!("parse {}: {e}", styx.display()))?;
    if spec.zones.is_empty() {
        return Err(eyre!(
            "{} has no zones — only zone-mode libraries can be exported",
            styx.display()
        ));
    }

    let path = out.unwrap_or_else(|| samples_dir.join(format!("{}.dspreset", spec.name)));
    std::fs::write(&path, render(&spec, amp_vel_track))
        .wrap_err_with(|| format!("write {}", path.display()))?;
    Ok(path)
}

/// Escape text for an XML attribute value.
fn escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&apos;")
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Build a spec by writing and parsing real styx, rather than constructing
    /// `LibrarySpec` field by field. That keeps these tests working when the
    /// struct gains fields, and exercises the same parse the export does.
    fn spec_with(zone_bodies: &[String]) -> LibrarySpec {
        let styx = format!(
            r#"name    "Whistle"
version "1.0"
vendor  "Korg"

sections ({{
  id           main
  label        "Whistle"
  note_grid    ()
  lowest_note  "C4"
  highest_note "C4"
}})

mics ({{
  id    Main
  label Main
  kind  blended
}})

dynamics {{
  short_note_controller velocity
}}

articulations ({{
  id       main
  label    "Main"
  kind     @Sustain
  dynamics ("127")
  rr       1
  dyn_ctrl velocity
}})

zones (
{}
)
"#,
            zone_bodies.join("\n")
        );
        // Unique per call: tests run in parallel, and two cases that happen to
        // build the same styx would otherwise share a directory and delete it
        // out from under each other.
        static SEQ: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
        let dir = std::env::temp_dir().join(format!(
            "fts-decent-test-{}-{}",
            std::process::id(),
            SEQ.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
        ));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("library.styx");
        std::fs::write(&path, styx).unwrap();
        let spec = LibrarySpec::from_file(&path).expect("styx should parse");
        std::fs::remove_dir_all(&dir).ok();
        spec
    }

    fn zone(file: &str) -> String {
        format!(
            r#"  {{
    file         "{file}"
    key_min      59
    key_max      61
    root_key     60
    vel_min      86
    vel_max      127
    rr_index     0
    mic          Main
    articulation main
  }}"#
        )
    }

    fn looped_zone(file: &str) -> String {
        format!(
            r#"  {{
    file         "{file}"
    key_min      59
    key_max      61
    root_key     60
    vel_min      86
    vel_max      127
    rr_index     0
    loop_start   88800
    loop_end     136800
    loop_xfade   7200
    mic          Main
    articulation main
  }}"#
        )
    }

    fn tuned_zone(file: &str) -> String {
        format!(
            r#"  {{
    file         "{file}"
    key_min      59
    key_max      61
    root_key     60
    vel_min      86
    vel_max      127
    rr_index     0
    tune_cents   50.0
    mic          Main
    articulation main
  }}"#
        )
    }

    #[test]
    fn zone_maps_onto_decentsampler_attributes() {
        let xml = render(&spec_with(&[zone("a.wav")]), 0.0);
        assert!(xml.contains(r#"path="a.wav""#));
        assert!(xml.contains(r#"rootNote="60""#));
        assert!(xml.contains(r#"loNote="59""#));
        assert!(xml.contains(r#"hiNote="61""#));
        assert!(xml.contains(r#"loVel="86""#));
        assert!(xml.contains(r#"hiVel="127""#));
    }

    #[test]
    fn an_unlooped_zone_writes_no_loop_attributes() {
        // loopEnd defaults to end-of-file, so emitting zeros would not read as
        // "no loop" — it must be omitted entirely.
        let xml = render(&spec_with(&[zone("a.wav")]), 0.0);
        assert!(!xml.contains("loopEnabled"), "got: {xml}");
        assert!(!xml.contains("loopStart"));
    }

    #[test]
    fn a_looped_zone_writes_loop_points_and_crossfade() {
        let xml = render(&spec_with(&[looped_zone("a.wav")]), 0.0);
        assert!(xml.contains(r#"loopEnabled="true""#));
        assert!(xml.contains(r#"loopStart="88800""#));
        assert!(xml.contains(r#"loopEnd="136800""#));
        assert!(xml.contains(r#"loopCrossfade="7200""#));
    }

    #[test]
    fn velocity_compensation_cancels_tracking_at_the_recorded_velocity() {
        // ampVelTrack=1 scales amplitude by v/127. A zone recorded at v must be
        // pre-boosted by 127/v so the two cancel exactly there.
        for v in [22u8, 43, 64, 85, 106, 127] {
            let boost_db = velocity_compensation_db(v);
            let net = 10f32.powf(boost_db / 20.0) * (v as f32 / 127.0);
            assert!(
                (net - 1.0).abs() < 1e-4,
                "velocity {v}: net gain {net}, expected 1.0"
            );
        }
    }

    #[test]
    fn the_loudest_layer_needs_no_compensation() {
        assert!(velocity_compensation_db(127).abs() < 1e-6);
    }

    #[test]
    fn quieter_layers_are_boosted_more() {
        assert!(velocity_compensation_db(22) > velocity_compensation_db(64));
        assert!(velocity_compensation_db(64) > velocity_compensation_db(127));
    }

    #[test]
    fn interpolating_export_writes_compensating_volume() {
        let xml = render(&spec_with(&[zone("a.wav")]), 1.0);
        assert!(xml.contains(r#"ampVelTrack="1""#), "got: {xml}");
        // The test zone's band tops out at 127, so no boost is needed there.
        assert!(!xml.contains("volume="), "got: {xml}");
    }

    #[test]
    fn stepped_export_writes_no_volume_compensation() {
        // With tracking off, every layer must play at exactly its recorded
        // level — compensating would make the soft layers wrong.
        let xml = render(&spec_with(&[zone("a.wav")]), 0.0);
        assert!(!xml.contains("volume="), "got: {xml}");
    }

    #[test]
    fn velocity_tracking_is_written_explicitly() {
        // The engine default is undocumented; a velocity-layered library that
        // ALSO scales volume by velocity applies its dynamics twice.
        let xml = render(&spec_with(&[zone("a.wav")]), 0.0);
        assert!(xml.contains(r#"ampVelTrack="0""#), "got: {xml}");
    }

    #[test]
    fn tuning_converts_cents_to_semitones() {
        let xml = render(&spec_with(&[tuned_zone("a.wav")]), 0.0);
        assert!(xml.contains(r#"tuning="0.5""#), "got: {xml}");
    }

    #[test]
    fn filenames_with_xml_metacharacters_are_escaped() {
        let xml = render(&spec_with(&[zone("a&c.wav")]), 0.0);
        assert!(xml.contains(r#"path="a&amp;c.wav""#), "got: {xml}");
    }

    #[test]
    fn every_zone_becomes_a_sample_element() {
        let zones: Vec<String> = (0..5).map(|i| zone(&format!("s{i}.wav"))).collect();
        let xml = render(&spec_with(&zones), 0.0);
        assert_eq!(xml.matches("<sample ").count(), 5);
    }
}
