//! Load an external plugin (CLAP / VST3), prepare it, render one MIDI-driven
//! block, and report what happened — the smoke test for hosting third-party
//! instruments (e.g. Pianoteq) inside Signal.
//!
//! ```text
//! cargo run -p signal-plugin-host --features vst3-host --example load_plugin -- \
//!     "$HOME/.vst3/Pianoteq 9.vst3"
//! ```

use signal_plugin_host::{HostedPlugin, PluginMidiEvent};

fn main() {
    let path = std::env::args()
        .nth(1)
        .expect("usage: load_plugin <path-to-.clap-or-.vst3>");

    let mut plugin = match HostedPlugin::load(&path) {
        Ok(Some(p)) => p,
        Ok(None) => {
            eprintln!("{path}: resolved to the synthetic backend (nothing to host)");
            std::process::exit(1);
        }
        Err(e) => {
            eprintln!("{path}: load failed: {e}");
            std::process::exit(1);
        }
    };

    let d = plugin.descriptor().clone();
    println!("loaded  : {} — {} ({:?})", d.name, d.vendor, d.format);
    println!("id      : {}", d.id);

    if let Err(e) = plugin.prepare(48_000.0, 512) {
        eprintln!("prepare failed: {e}");
        std::process::exit(1);
    }
    let params = plugin.params();
    println!("params  : {}", params.len());
    // `--params` dumps the full parameter surface (id, range, default,
    // current value, display text) as TSV — the Omnisphere calibration path.
    if std::env::args().any(|a| a == "--params") {
        for p in &params {
            let value = plugin.param_value(p.id).unwrap_or(p.default);
            let text = plugin.value_to_text(p.id, value).unwrap_or_default();
            println!(
                "PARAM\t{}\t{}\t{}\t{}\t{}\t{}\t{}",
                p.id, p.name, p.min, p.max, p.default, value, text
            );
        }
    } else {
        for p in params.iter().take(8) {
            println!("  [{:>5}] {}", p.id, p.name);
        }
    }
    println!("latency : {} frames", plugin.latency());

    // Render half a second of C4 and report the output level — proves the
    // instrument actually makes sound under our host.
    let note_on = [PluginMidiEvent {
        offset: 0,
        message: daw::service::MidiMessage::note_on(0, 60, 100),
    }];
    let mut inter = vec![0.0f32; 512 * 2];
    let mut peak = 0.0f32;
    for block in 0..50 {
        let midi: &[PluginMidiEvent] = if block == 0 { &note_on } else { &[] };
        inter.iter_mut().for_each(|s| *s = 0.0);
        if let Err(e) = plugin.process_interleaved(&mut inter, midi, &[]) {
            eprintln!("process failed: {e}");
            std::process::exit(1);
        }
        peak = peak.max(inter.iter().fold(0.0f32, |m, s| m.max(s.abs())));
    }
    println!("peak    : {peak:.4} ({})", if peak > 1e-4 { "AUDIBLE" } else { "silent" });
}
