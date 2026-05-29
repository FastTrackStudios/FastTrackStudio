//! Manual CLAP GUI launcher.
//!
//! Run from a graphical session:
//!
//! ```bash
//! cargo run -p daw-standalone --features clap-host --example clap_gui -- \
//!   /path/to/lsp-plugins.clap
//! ```

#[cfg(feature = "clap-host")]
use std::path::PathBuf;
#[cfg(feature = "clap-host")]
use std::time::Duration;

#[cfg(feature = "clap-host")]
use daw_standalone::audio_engine::plugin_host::ClapHost;

#[cfg(feature = "clap-host")]
fn main() {
    let mut args = std::env::args_os().skip(1);
    let path = args
        .next()
        .map(PathBuf::from)
        .or_else(|| std::env::var_os("DAW_TEST_LSP_CLAP_BUNDLE").map(PathBuf::from))
        .expect("usage: clap_gui /path/to/plugin.clap");
    let hold_secs = std::env::var("DAW_TEST_CLAP_GUI_HOLD_SECS")
        .ok()
        .and_then(|s| s.parse::<u64>().ok())
        .unwrap_or(30);

    let host = ClapHost::default();
    let descriptors = host.list_in_bundle(&path).expect("bundle should load");
    let wanted_terms = ["parametric", "equalizer", "x32", "stereo"];
    let plugin_index = descriptors
        .iter()
        .position(|d| {
            let haystack = format!("{} {}", d.id, d.name).to_ascii_lowercase();
            wanted_terms.iter().all(|term| haystack.contains(term))
        })
        .unwrap_or_else(|| {
            panic!(
                "no Parametric Equalizer x32 Stereo descriptor found in {}; descriptors: {:?}",
                path.display(),
                descriptors
                    .iter()
                    .map(|d| format!("{} ({})", d.name, d.id))
                    .collect::<Vec<_>>()
            )
        });

    let descriptor = &descriptors[plugin_index];
    eprintln!(
        "opening descriptor #{plugin_index}: '{}' id='{}' from {}",
        descriptor.name,
        descriptor.id,
        path.display()
    );

    let mut plugin = host
        .load(&path, plugin_index)
        .expect("plugin should instantiate");
    assert!(plugin.has_gui(), "plugin does not expose clap gui");
    plugin.open_gui_floating().expect("CLAP GUI should open");

    eprintln!("GUI is open for {hold_secs}s");
    std::thread::sleep(Duration::from_secs(hold_secs));
    plugin.close_gui();
}

#[cfg(not(feature = "clap-host"))]
fn main() {
    eprintln!("enable --features clap-host");
    std::process::exit(2);
}
