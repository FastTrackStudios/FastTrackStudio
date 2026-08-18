use std::path::Path;

use anyhow::{Context, Result};

use crate::analyzer::{self, Analyzer, BlockContext, RunContext};
use crate::config::{AnalyzerType, MeasureConfig, SignalType};
use crate::grid::{self, GridPoint};
use crate::host::LoadedPlugin;
use crate::signal;

/// Progress callback: (current_run, total_runs, grid_point).
pub type ProgressFn = Box<dyn FnMut(usize, usize, &GridPoint)>;

/// The measurement engine: ties together config, grid, host, signal, and analyzers.
pub struct MeasurementEngine {
    config: MeasureConfig,
    analyzers: Vec<(AnalyzerType, Box<dyn Analyzer>)>,
    progress: Option<ProgressFn>,
}

impl MeasurementEngine {
    pub fn new(config: MeasureConfig) -> Self {
        let analyzers: Vec<(AnalyzerType, Box<dyn Analyzer>)> = config
            .analyzers
            .iter()
            .map(|&kind| (kind, analyzer::create_analyzer(kind, &config)))
            .collect();

        Self {
            config,
            analyzers,
            progress: None,
        }
    }

    pub fn set_progress(&mut self, f: ProgressFn) {
        self.progress = Some(f);
    }

    /// Run the full measurement grid and write results to `output_dir`.
    pub fn run(&mut self, output_dir: &Path) -> Result<()> {
        std::fs::create_dir_all(output_dir)?;

        // Load plugin and resolve parameters
        let mut plugin = LoadedPlugin::load(
            &self.config.plugin_path,
            self.config.plugin_index,
            self.config.sample_rate,
            self.config.block_size,
        )
        .context("failed to load plugin")?;

        let plugin_params = plugin.params();
        let resolved =
            grid::resolve_params(&self.config.parameter_buckets, &plugin_params, &mut plugin)?;

        // Generate grid from resolved parameter values
        let grid_points = grid::generate_grid(&self.config, &resolved);
        let total = grid_points.len();

        eprintln!(
            "Running {} measurement points ({} parameter combos x {} gain levels)",
            total,
            if self.config.input_gain_buckets_db.is_empty() {
                1
            } else {
                self.config.input_gain_buckets_db.len()
            },
            if self.config.parameter_buckets.is_empty() {
                1
            } else {
                grid_points.len()
                    / if self.config.input_gain_buckets_db.is_empty() {
                        1
                    } else {
                        self.config.input_gain_buckets_db.len()
                    }
            }
        );

        let sr = self.config.sample_rate as f32;
        let warmup_samples = (self.config.warmup_seconds * sr) as usize;
        let total_samples = (self.config.seconds * sr) as usize + warmup_samples;

        for point in &grid_points {
            if let Some(ref mut progress) = self.progress {
                progress(point.index, total, point);
            }

            eprintln!(
                "[{}/{}] gain={:.1}dB params={:?}",
                point.index + 1,
                total,
                point.input_gain_db,
                point.params
            );

            // Generate input signal with gain applied
            let gain = 10.0f32.powf(point.input_gain_db / 20.0);
            let raw_signal = self.generate_signal(total_samples);
            let input: Vec<f32> = raw_signal.iter().map(|&s| s * gain).collect();

            // Build param overrides: resolve names to IDs
            let param_overrides: Vec<(u32, f64)> = point
                .params
                .iter()
                .map(|(name, value)| {
                    let r = resolved.iter().find(|r| r.name == *name).unwrap();
                    (r.id, *value)
                })
                .collect();

            // Process through plugin (fresh activate/deactivate per run, stereo)
            let (full_output_l, full_output_r) = plugin.process_stereo(&input, &param_overrides)?;

            // Discard warmup
            let input_trimmed = &input[warmup_samples..];
            let output_trimmed = &full_output_l[warmup_samples..];
            let output_r_trimmed = &full_output_r[warmup_samples..];

            // Prepare per-run analyzer state
            for (_, a) in &mut self.analyzers {
                a.prepare_run(point);
            }

            // Feed blocks to analyzers
            let block_size = self.config.block_size as usize;
            let mut offset = 0;
            while offset < input_trimmed.len() {
                let end = (offset + block_size).min(input_trimmed.len());
                let block_in = &input_trimmed[offset..end];
                let block_out = &output_trimmed[offset..end];
                let block_out_r = &output_r_trimmed[offset..end];

                let ctx = BlockContext {
                    input: block_in,
                    output: block_out,
                    output_r: block_out_r,
                    sample_rate: self.config.sample_rate,
                };

                for (_, a) in &mut self.analyzers {
                    a.process_block(&ctx);
                }
                offset = end;
            }

            // End run
            let run_ctx = RunContext {
                grid_point: point,
                full_input: input_trimmed,
                full_output: output_trimmed,
                full_output_r: output_r_trimmed,
                sample_rate: self.config.sample_rate,
            };
            for (_, a) in &mut self.analyzers {
                a.end_run(&run_ctx);
            }
        }

        // Write all results
        for (kind, a) in &self.analyzers {
            a.write_results(output_dir)
                .with_context(|| format!("failed to write {:?} results", kind))?;
        }

        eprintln!("Results written to {}", output_dir.display());
        Ok(())
    }

    fn generate_signal(&self, length: usize) -> Vec<f32> {
        let sr = self.config.sample_rate as f32;
        match self.config.signal_type {
            SignalType::Sine => signal::sine(self.config.sine_frequency, sr, length),
            SignalType::Noise => signal::white_noise(length, 42),
            SignalType::Sweep => signal::sweep(
                self.config.sweep_start_hz,
                self.config.sweep_end_hz,
                sr,
                length,
            ),
        }
    }
}
