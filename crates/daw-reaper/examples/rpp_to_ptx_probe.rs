//! Generate a minimal RPP isolating ONE feature, convert backwards via the
//! PT Reaper Converter (`--convert generated.rpp result.ptx`), and report
//! the PTX block-type counts.
//!
//! Usage: cargo run -p daw-reaper --example rpp_to_ptx_probe -- <probe>
//!
//! Probes: baseline | mute | color | solo | folder | vol | pan | notes
//!
//! Requires `scripts/pt-convert.sh` to work (voyager reachable, app installed).
//!
//! Generates RPP into `/tmp/probe_<name>.rpp`, asks the converter to produce
//! `/tmp/probe_<name>.ptx`, then dumps the block counts. Compare against
//! `baseline` to see which block types appear/grow per feature.

use dawfile_protools::raw_block::RawBlock;
use dawfile_reaper::RppSerialize;
use std::collections::BTreeMap;
use std::process::Command;

fn build_rpp(probe: &str) -> String {
    use dawfile_reaper::builder::ReaperProjectBuilder;
    let mut b = ReaperProjectBuilder::new().sample_rate(48000);

    match probe {
        "baseline" => {
            b = b.track("ProbeTrack", |t| t);
        }
        "mute" => {
            b = b.track("ProbeTrack", |t| t.muted());
        }
        "color" => {
            b = b.track("ProbeTrack", |t| t.color(0xd86e41));
        }
        "solo" => {
            b = b.track("ProbeTrack", |t| t.soloed());
        }
        "folder" => {
            b = b
                .track("ProbeFolder", |t| t.folder_start())
                .track("Child", |t| t.folder_end(1));
        }
        "vol" => {
            b = b.track("ProbeTrack", |t| t.volume(0.5));
        }
        "pan" => {
            b = b.track("ProbeTrack", |t| t.pan(0.5));
        }
        "two_tracks" => {
            b = b.track("Alpha", |t| t).track("Beta", |t| t);
        }
        "marker" => {
            b = b.track("ProbeTrack", |t| t).marker(1, 1.0, "M");
        }
        "send" => {
            b = b.track("Source", |t| t).track("Dest", |t| t.receive(0));
        }
        "folder3" => {
            // Folder with 3 children — to map the children list in 0x200d
            b = b
                .track("Parent", |t| t.folder_start())
                .track("Child1", |t| t)
                .track("Child2", |t| t)
                .track("Child3", |t| t.folder_end(1));
        }
        "item_basic" => {
            // Track with one item
            b = b.track("ProbeTrack", |t| t.item(0.0, 1.0, |i| i.name("Clip")));
        }
        _ => panic!("unknown probe: {probe}"),
    }

    b.build().to_rpp_string()
}

fn run_converter(rpp_path: &str, ptx_path: &str) -> Result<(), Box<dyn std::error::Error>> {
    // Resolve repo root: crates/daw-reaper/examples/ → ../../..
    let manifest = env!("CARGO_MANIFEST_DIR");
    let script = std::path::Path::new(manifest)
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("scripts/pt-convert.sh");
    let out = Command::new(&script).args([rpp_path, ptx_path]).output()?;
    if !out.status.success() {
        return Err(format!(
            "convert failed: {}\nstderr: {}",
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        )
        .into());
    }
    eprintln!("{}", String::from_utf8_lossy(&out.stdout));
    Ok(())
}

fn block_summary(ptx_path: &str) -> Result<BTreeMap<u16, usize>, Box<dyn std::error::Error>> {
    let raw = std::fs::read(ptx_path)?;
    let session = dawfile_protools::parse_raw(raw)?;
    let mut counts: BTreeMap<u16, usize> = BTreeMap::new();
    fn walk(blocks: &[RawBlock], counts: &mut BTreeMap<u16, usize>) {
        for b in blocks {
            *counts.entry(b.content_type_raw).or_default() += 1;
            walk(&b.children, counts);
        }
    }
    walk(&session.blocks, &mut counts);
    Ok(counts)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let probe = std::env::args().nth(1).expect("probe name");

    let tmp = std::env::temp_dir();
    let rpp = tmp.join(format!("probe_{probe}.rpp"));
    let ptx = tmp.join(format!("probe_{probe}.ptx"));

    let rpp_content = build_rpp(&probe);
    std::fs::write(&rpp, &rpp_content)?;
    eprintln!("wrote {} ({} bytes)", rpp.display(), rpp_content.len());

    run_converter(rpp.to_str().unwrap(), ptx.to_str().unwrap())?;

    let ptx_size = std::fs::metadata(&ptx)?.len();
    eprintln!("got {} ({} bytes)", ptx.display(), ptx_size);

    let counts = block_summary(ptx.to_str().unwrap())?;
    println!("=== block counts for probe={probe} ({}b) ===", ptx_size);
    for (ct, n) in &counts {
        println!("  0x{ct:04x} × {n:>3}");
    }
    Ok(())
}
