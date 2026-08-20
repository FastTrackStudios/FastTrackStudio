//! Sample many patches unattended.
//!
//! Selection is by **set-list slot**, not bank/program. In Set List mode a
//! Program Change selects a slot, and the slot carries whatever bank, program,
//! transpose and effects it was configured with. That avoids reproducing the
//! instrument's Bank Select MSB/LSB table — the part of driving a synth over
//! MIDI most likely to be silently wrong — and it samples the patch *as the
//! set list actually uses it*, which is the thing being reproduced.
//!
//! A batch is a long job (tens of minutes per patch), so it is built to be
//! interrupted: an entry whose pack already exists is skipped, and one failure
//! never stops the rest.

use std::path::{Path, PathBuf};
use std::time::Duration;

use eyre::{Result, WrapErr, bail};

use crate::config::AutoSampleConfig;
use crate::midi::Instrument;

/// One patch to sample.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BatchEntry {
    /// Set-list slot to select with a Program Change.
    pub slot: u8,
    /// Name for the output folder and pack.
    pub name: String,
}

/// Parse a batch list: one `slot  name` per line.
///
/// `#` starts a comment. The name may contain spaces — everything after the
/// first whitespace run is the name, so `17  Whistle` and
/// `68  Brekdown Wurli` both work.
pub fn parse_list(text: &str) -> Result<Vec<BatchEntry>> {
    let mut out = Vec::new();
    for (i, raw) in text.lines().enumerate() {
        let line = raw.split('#').next().unwrap_or("").trim();
        if line.is_empty() {
            continue;
        }
        let (slot, name) = line
            .split_once(char::is_whitespace)
            .ok_or_else(|| eyre::eyre!("line {}: expected `<slot> <name>`, got {raw:?}", i + 1))?;
        let slot: u8 = slot
            .parse()
            .wrap_err_with(|| format!("line {}: bad slot {slot:?}", i + 1))?;
        let name = name.trim();
        if name.is_empty() {
            bail!("line {}: missing name", i + 1);
        }
        out.push(BatchEntry {
            slot,
            name: name.to_string(),
        });
    }
    if out.is_empty() {
        bail!("batch list is empty");
    }
    Ok(out)
}

/// What happened to one entry.
#[derive(Debug)]
pub enum Outcome {
    Sampled { samples: usize, pack: PathBuf },
    /// Its pack already existed, so it was left alone.
    Skipped,
    Failed(String),
}

/// One entry's result.
#[derive(Debug)]
pub struct BatchResult {
    pub entry: BatchEntry,
    pub outcome: Outcome,
}

/// Everything a batch needs beyond the per-patch grid settings.
pub struct BatchConfig {
    pub entries: Vec<BatchEntry>,
    /// Directory that receives `<name>/` and `<name>.signalpack` per entry.
    pub out_root: PathBuf,
    /// How long to wait after a Program Change before sampling. A Kronos slot
    /// change loads samples and rebuilds effects; sampling too early captures
    /// the previous patch.
    pub patch_settle_ms: u32,
    /// Template for each patch's run — grid, timing, routing. `name`,
    /// `out_dir` and `pack_path` are replaced per entry.
    pub template: AutoSampleConfig,
    /// Print what would happen without touching the instrument.
    pub dry_run: bool,
    /// Search each finished patch for its best loop points. Costs a couple of
    /// seconds per patch and is the difference between a usable loop and an
    /// audible one.
    pub search: Option<crate::reloop::SearchRange>,
    /// Also write a DecentSampler `.dspreset` beside each patch's samples.
    pub decent: bool,
    /// Velocity-to-volume tracking for those presets.
    pub amp_vel_track: f32,
    /// Stop after this many patches are sampled in this run. Lets a long job be
    /// taken in sittings — the next run resumes where this one stopped.
    pub limit: Option<usize>,
    /// Re-sample patches that already have a pack, instead of skipping them.
    ///
    /// Without this, "re-sample this patch" means "delete its pack first" — you
    /// have to destroy the existing recording *before* finding out whether the
    /// new settings are any better. The old pack is left untouched until the
    /// new one is built, so a failed or abandoned re-run costs nothing.
    pub force: bool,
}

/// Filesystem-safe folder name.
fn sanitize(name: &str) -> String {
    name.chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() || c == '-' || c == ' ' {
                c
            } else {
                '_'
            }
        })
        .collect::<String>()
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
}

/// Where an entry's output goes.
pub fn paths_for(out_root: &Path, name: &str) -> (PathBuf, PathBuf) {
    let safe = sanitize(name);
    (
        out_root.join(&safe),
        out_root.join(format!("{safe}.signalpack")),
    )
}

/// Run the whole batch, one patch at a time.
pub fn run(config: &BatchConfig) -> Result<Vec<BatchResult>> {
    std::fs::create_dir_all(&config.out_root)
        .wrap_err_with(|| format!("create {}", config.out_root.display()))?;

    if config.dry_run {
        let mut out = Vec::new();
        for entry in &config.entries {
            let (dir, pack) = paths_for(&config.out_root, &entry.name);
            println!(
                "slot {:>3} → PC {:<3} → {}  [{}]",
                entry.slot,
                entry.slot,
                dir.display(),
                if pack.exists() && !config.force {
                    "skip"
                } else if pack.exists() {
                    "RE-SAMPLE (pack exists, --force)"
                } else {
                    "sample"
                }
            );
            out.push(BatchResult {
                entry: entry.clone(),
                outcome: Outcome::Skipped,
            });
        }
        return Ok(out);
    }

    let done_already = config
        .entries
        .iter()
        .filter(|e| paths_for(&config.out_root, &e.name).1.exists())
        .count();
    let todo = config.entries.len() - done_already;
    tracing::info!(
        total = config.entries.len(),
        done = done_already,
        remaining = todo,
        limit = config.limit.map(|l| l.to_string()).unwrap_or("none".into()),
        "batch starting"
    );

    let mut results = Vec::new();
    let mut sampled_this_run = 0usize;
    for (i, entry) in config.entries.iter().enumerate() {
        if config.limit.is_some_and(|l| sampled_this_run >= l) {
            tracing::info!(
                "reached the {} patch limit for this run — rerun the same command to continue",
                config.limit.unwrap_or(0)
            );
            break;
        }
        let (out_dir, pack_path) = paths_for(&config.out_root, &entry.name);

        // Resume: a finished pack means this patch is done, unless `force`
        // asks for it again. The existing pack stays in place until the new one
        // overwrites it, so nothing is lost if the re-run fails.
        if pack_path.exists() && !config.force {
            tracing::info!(
                "[{}/{}] slot {} {} — pack exists, skipping",
                i + 1,
                config.entries.len(),
                entry.slot,
                entry.name
            );
            results.push(BatchResult {
                entry: entry.clone(),
                outcome: Outcome::Skipped,
            });
            continue;
        }

        tracing::info!(
            "[{}/{}] slot {} → {}",
            i + 1,
            config.entries.len(),
            entry.slot,
            entry.name
        );

        // Select the slot. The instrument is opened and closed per patch so a
        // failure mid-batch can't leave a note held for the rest of the run —
        // `Instrument`'s Drop sends All Notes Off.
        let select = (|| -> Result<()> {
            let mut instrument =
                Instrument::open(&config.template.midi.port, config.template.midi.channel)?;
            instrument.program_change(entry.slot)?;
            std::thread::sleep(Duration::from_millis(config.patch_settle_ms as u64));
            Ok(())
        })();
        if let Err(e) = select {
            results.push(BatchResult {
                entry: entry.clone(),
                outcome: Outcome::Failed(format!("select slot {}: {e}", entry.slot)),
            });
            continue;
        }

        let mut patch_config = clone_template(&config.template);
        patch_config.name = entry.name.clone();
        patch_config.out_dir = out_dir.clone();
        // Build the pack AFTER the loop search, not during sampling: the search
        // rewrites the spec, and packing twice would re-encode every sample to
        // FLAC for nothing.
        patch_config.pack_path = None;

        // One patch failing (silent slot, unplugged cable) must not end a job
        // that may have hours left.
        match crate::session::run(&patch_config) {
            Ok(report) => {
                let samples = report.recorded.len();
                match finish(config, &out_dir, &pack_path, report.sample_rate) {
                    Ok(()) => {
                        sampled_this_run += 1;
                        tracing::info!(
                            samples,
                            skipped = report.skipped.len(),
                            "[{}/{}] {} done",
                            i + 1,
                            config.entries.len(),
                            entry.name
                        );
                        results.push(BatchResult {
                            entry: entry.clone(),
                            outcome: Outcome::Sampled {
                                samples,
                                pack: pack_path,
                            },
                        });
                    }
                    Err(e) => {
                        // The audio is on disk; only the packaging failed. No
                        // pack means the next run redoes this patch.
                        tracing::error!("[{}/{}] {} packaging failed: {e}", i + 1, config.entries.len(), entry.name);
                        results.push(BatchResult {
                            entry: entry.clone(),
                            outcome: Outcome::Failed(format!("packaging: {e}")),
                        });
                    }
                }
            }
            Err(e) => {
                tracing::error!("[{}/{}] {} failed: {e}", i + 1, config.entries.len(), entry.name);
                results.push(BatchResult {
                    entry: entry.clone(),
                    outcome: Outcome::Failed(e.to_string()),
                });
            }
        }
    }
    Ok(results)
}

/// Loop-search, pack and (optionally) export one finished patch.
///
/// Ordering matters: the search rewrites `library.styx`, so the pack must be
/// built from the updated spec, and the `.dspreset` written from it too.
fn finish(
    config: &BatchConfig,
    out_dir: &Path,
    pack_path: &Path,
    sample_rate: u32,
) -> Result<()> {
    if let Some(range) = config.search {
        let report = crate::reloop::run_with_search(
            out_dir,
            &config.template.loop_policy,
            true,
            sample_rate,
            Some(range),
        )?;
        tracing::info!(
            relooped = report.relooped,
            mean_seam = report.mean_score.map(|s| format!("{s:.4}")),
            "loop search"
        );
    }

    let spec = signal_sampler::spec::LibrarySpec::from_file(&out_dir.join("library.styx"))
        .map_err(|e| eyre::eyre!("re-read spec: {e}"))?;
    let paths: Vec<PathBuf> = spec.zones.iter().map(|z| out_dir.join(&z.file)).collect();
    signal_sampler::engine::cache::create_signal_pack(
        pack_path,
        &out_dir.join("library.styx"),
        out_dir,
        paths.iter().map(|p| p.as_path()),
    )
    .map_err(|e| eyre::eyre!("build pack: {e}"))?;

    if config.decent {
        crate::decent::export(out_dir, None, config.amp_vel_track)?;
    }
    Ok(())
}

fn clone_template(t: &AutoSampleConfig) -> AutoSampleConfig {
    AutoSampleConfig {
        name: t.name.clone(),
        vendor: t.vendor.clone(),
        grid: t.grid.clone(),
        timing: t.timing.clone(),
        midi: t.midi.clone(),
        audio: t.audio.clone(),
        loops: t.loops,
        resume_samples: t.resume_samples,
        loop_policy: t.loop_policy,
        probe_search: t.probe_search,
        out_dir: t.out_dir.clone(),
        pack_path: t.pack_path.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn list_parses_slot_and_name() {
        let list = parse_list("17 Whistle\n68  Brekdown Wurli\n").unwrap();
        assert_eq!(
            list,
            vec![
                BatchEntry {
                    slot: 17,
                    name: "Whistle".into()
                },
                BatchEntry {
                    slot: 68,
                    name: "Brekdown Wurli".into()
                },
            ]
        );
    }

    #[test]
    fn comments_and_blank_lines_are_ignored() {
        let list = parse_list("# patches\n\n17 Whistle  # the GM one\n").unwrap();
        assert_eq!(list.len(), 1);
        assert_eq!(list[0].name, "Whistle");
    }

    #[test]
    fn a_malformed_line_names_its_line_number() {
        let err = parse_list("17 Whistle\nnope\n").unwrap_err().to_string();
        assert!(err.contains("line 2"), "got: {err}");
    }

    #[test]
    fn empty_list_is_rejected() {
        assert!(parse_list("# only comments\n").is_err());
    }

    #[test]
    fn names_become_filesystem_safe_without_collapsing_distinct_patches() {
        let root = Path::new("/tmp/packs");
        let (dir, pack) = paths_for(root, "Sirus Piano/Wurlitzer");
        assert_eq!(dir, root.join("Sirus Piano_Wurlitzer"));
        assert_eq!(pack, root.join("Sirus Piano_Wurlitzer.signalpack"));

        // Two genuinely different patches must not land on the same path.
        let (a, _) = paths_for(root, "Mandolin Pick");
        let (b, _) = paths_for(root, "Mandolin Trem & Str");
        assert_ne!(a, b);
    }

    #[test]
    fn slot_numbers_outside_a_program_change_are_rejected() {
        // Program Change carries 0-127; a slot above that cannot be selected.
        assert!(parse_list("300 Nope\n").is_err());
    }
}
