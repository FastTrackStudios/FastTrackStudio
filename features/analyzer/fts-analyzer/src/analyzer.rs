use std::path::Path;
use std::sync::Arc;

use rustfft::num_complex::Complex;
use rustfft::{Fft, FftPlanner};

use crate::analysis;
use crate::grid::GridPoint;

/// Context passed to analyzers for each processing block.
pub struct BlockContext<'a> {
    pub input: &'a [f32],
    pub output: &'a [f32],
    /// Right channel output (same input is sent to both L and R).
    pub output_r: &'a [f32],
    pub sample_rate: f64,
}

/// Context passed to analyzers at the end of each run (one grid point).
pub struct RunContext<'a> {
    pub grid_point: &'a GridPoint,
    pub full_input: &'a [f32],
    pub full_output: &'a [f32],
    /// Right channel output.
    pub full_output_r: &'a [f32],
    pub sample_rate: f64,
}

/// Trait for measurement analyzers that accumulate data across blocks/runs.
pub trait Analyzer {
    /// Called before each run to set up per-run state.
    fn prepare_run(&mut self, point: &GridPoint);

    /// Called once per audio block during processing.
    fn process_block(&mut self, ctx: &BlockContext);

    /// Called at the end of each run (after all blocks for one grid point).
    fn end_run(&mut self, ctx: &RunContext);

    /// Write results to the output directory.
    fn write_results(&self, output_dir: &Path) -> anyhow::Result<()>;
}

// ---------------------------------------------------------------------------
// Hann window helper
// ---------------------------------------------------------------------------

#[inline]
fn hann(i: usize, n: usize) -> f32 {
    0.5 * (1.0 - (2.0 * std::f32::consts::PI * i as f32 / (n - 1) as f32).cos())
}

// ---------------------------------------------------------------------------
// RawCsv — streams time-domain samples
// ---------------------------------------------------------------------------

pub struct RawCsvAnalyzer {
    rows: Vec<RawCsvRow>,
    current_point: Option<GridPoint>,
}

struct RawCsvRow {
    run_index: usize,
    sample_index: usize,
    input: f32,
    output: f32,
}

impl Default for RawCsvAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

impl RawCsvAnalyzer {
    pub fn new() -> Self {
        Self {
            rows: Vec::new(),
            current_point: None,
        }
    }
}

impl Analyzer for RawCsvAnalyzer {
    fn prepare_run(&mut self, point: &GridPoint) {
        self.current_point = Some(point.clone());
    }

    fn process_block(&mut self, ctx: &BlockContext) {
        let run_index = self.current_point.as_ref().map(|p| p.index).unwrap_or(0);
        let base = self.rows.len();
        for (i, (&inp, &out)) in ctx.input.iter().zip(ctx.output.iter()).enumerate() {
            self.rows.push(RawCsvRow {
                run_index,
                sample_index: base + i,
                input: inp,
                output: out,
            });
        }
    }

    fn end_run(&mut self, _ctx: &RunContext) {}

    fn write_results(&self, output_dir: &Path) -> anyhow::Result<()> {
        let path = output_dir.join("raw.csv");
        let mut wtr = csv::Writer::from_path(&path)?;
        wtr.write_record(["run", "sample", "input", "output"])?;
        for row in &self.rows {
            wtr.write_record(&[
                row.run_index.to_string(),
                row.sample_index.to_string(),
                row.input.to_string(),
                row.output.to_string(),
            ])?;
        }
        wtr.flush()?;
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// RmsPeak — per-run RMS and peak levels (stereo)
// ---------------------------------------------------------------------------

pub struct RmsPeakAnalyzer {
    rows: Vec<RmsPeakRow>,
}

struct RmsPeakRow {
    run_index: usize,
    input_gain_db: f32,
    params: Vec<(String, f64)>,
    input_rms_db: f32,
    input_peak_db: f32,
    output_l_rms_db: f32,
    output_l_peak_db: f32,
    output_r_rms_db: f32,
    output_r_peak_db: f32,
    gain_reduction_db: f32,
}

impl Default for RmsPeakAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

impl RmsPeakAnalyzer {
    pub fn new() -> Self {
        Self { rows: Vec::new() }
    }
}

impl Analyzer for RmsPeakAnalyzer {
    fn prepare_run(&mut self, _point: &GridPoint) {}

    fn process_block(&mut self, _ctx: &BlockContext) {}

    fn end_run(&mut self, ctx: &RunContext) {
        self.rows.push(RmsPeakRow {
            run_index: ctx.grid_point.index,
            input_gain_db: ctx.grid_point.input_gain_db,
            params: ctx.grid_point.params.clone(),
            input_rms_db: analysis::rms_db(ctx.full_input),
            input_peak_db: analysis::peak_db(ctx.full_input),
            output_l_rms_db: analysis::rms_db(ctx.full_output),
            output_l_peak_db: analysis::peak_db(ctx.full_output),
            output_r_rms_db: analysis::rms_db(ctx.full_output_r),
            output_r_peak_db: analysis::peak_db(ctx.full_output_r),
            gain_reduction_db: analysis::gain_reduction_db(ctx.full_input, ctx.full_output),
        });
    }

    fn write_results(&self, output_dir: &Path) -> anyhow::Result<()> {
        let path = output_dir.join("grid_rms_peak.csv");
        let mut wtr = csv::Writer::from_path(&path)?;

        // Build header
        let param_names: Vec<&str> = if let Some(first) = self.rows.first() {
            first.params.iter().map(|(n, _)| n.as_str()).collect()
        } else {
            return Ok(());
        };

        let mut header: Vec<String> = vec!["run".into(), "input_gain_db".into()];
        for name in &param_names {
            header.push(name.to_string());
        }
        header.extend([
            "input_rms_db".into(),
            "input_peak_db".into(),
            "output_l_rms_db".into(),
            "output_l_peak_db".into(),
            "output_r_rms_db".into(),
            "output_r_peak_db".into(),
            "gain_reduction_db".into(),
        ]);
        wtr.write_record(&header)?;

        for row in &self.rows {
            let mut record: Vec<String> = vec![
                row.run_index.to_string(),
                format!("{:.2}", row.input_gain_db),
            ];
            for (_, val) in &row.params {
                record.push(format!("{:.4}", val));
            }
            record.extend([
                format!("{:.2}", row.input_rms_db),
                format!("{:.2}", row.input_peak_db),
                format!("{:.2}", row.output_l_rms_db),
                format!("{:.2}", row.output_l_peak_db),
                format!("{:.2}", row.output_r_rms_db),
                format!("{:.2}", row.output_r_peak_db),
                format!("{:.2}", row.gain_reduction_db),
            ]);
            wtr.write_record(&record)?;
        }
        wtr.flush()?;
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// TransferCurve — maps input level → output level in bins
// ---------------------------------------------------------------------------

const TRANSFER_BINS: usize = 512;
const TRANSFER_DB_MIN: f32 = -96.0;
const TRANSFER_DB_MAX: f32 = 0.0;

pub struct TransferCurveAnalyzer {
    runs: Vec<TransferCurveRun>,
}

struct TransferCurveRun {
    run_index: usize,
    input_gain_db: f32,
    params: Vec<(String, f64)>,
    bin_sum: Vec<f64>,
    bin_count: Vec<u32>,
}

impl Default for TransferCurveAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

impl TransferCurveAnalyzer {
    pub fn new() -> Self {
        Self { runs: Vec::new() }
    }
}

impl Analyzer for TransferCurveAnalyzer {
    fn prepare_run(&mut self, point: &GridPoint) {
        self.runs.push(TransferCurveRun {
            run_index: point.index,
            input_gain_db: point.input_gain_db,
            params: point.params.clone(),
            bin_sum: vec![0.0; TRANSFER_BINS],
            bin_count: vec![0; TRANSFER_BINS],
        });
    }

    fn process_block(&mut self, ctx: &BlockContext) {
        let run = match self.runs.last_mut() {
            Some(r) => r,
            None => return,
        };

        // Process sample-by-sample into bins
        for (&inp, &out) in ctx.input.iter().zip(ctx.output.iter()) {
            let in_abs = inp.abs();
            if in_abs < 1e-10 {
                continue;
            }
            let in_db = 20.0 * in_abs.log10();
            let out_db = 20.0 * out.abs().max(1e-10).log10();

            if !(TRANSFER_DB_MIN..=TRANSFER_DB_MAX).contains(&in_db) {
                continue;
            }

            let bin = ((in_db - TRANSFER_DB_MIN) / (TRANSFER_DB_MAX - TRANSFER_DB_MIN)
                * (TRANSFER_BINS - 1) as f32) as usize;
            let bin = bin.min(TRANSFER_BINS - 1);

            run.bin_sum[bin] += out_db as f64;
            run.bin_count[bin] += 1;
        }
    }

    fn end_run(&mut self, _ctx: &RunContext) {}

    fn write_results(&self, output_dir: &Path) -> anyhow::Result<()> {
        let path = output_dir.join("grid_transfer_curves.csv");
        let mut wtr = csv::Writer::from_path(&path)?;

        let param_names: Vec<&str> = if let Some(first) = self.runs.first() {
            first.params.iter().map(|(n, _)| n.as_str()).collect()
        } else {
            return Ok(());
        };

        let mut header: Vec<String> = vec!["run".into(), "input_gain_db".into()];
        for name in &param_names {
            header.push(name.to_string());
        }
        header.extend(["input_db".into(), "output_db".into()]);
        wtr.write_record(&header)?;

        for run in &self.runs {
            for bin in 0..TRANSFER_BINS {
                if run.bin_count[bin] == 0 {
                    continue;
                }
                let in_db = TRANSFER_DB_MIN
                    + (TRANSFER_DB_MAX - TRANSFER_DB_MIN) * bin as f32 / (TRANSFER_BINS - 1) as f32;
                let out_db = run.bin_sum[bin] / run.bin_count[bin] as f64;

                let mut record: Vec<String> = vec![
                    run.run_index.to_string(),
                    format!("{:.2}", run.input_gain_db),
                ];
                for (_, val) in &run.params {
                    record.push(format!("{:.4}", val));
                }
                record.extend([format!("{:.2}", in_db), format!("{:.4}", out_db)]);
                wtr.write_record(&record)?;
            }
        }
        wtr.flush()?;
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// LinearResponse — FFT-based frequency response (Welch averaged)
// ---------------------------------------------------------------------------

pub struct LinearResponseAnalyzer {
    fft_size: usize,
    fft: Arc<dyn Fft<f32>>,
    runs: Vec<LinearResponseRun>,
    // Per-run streaming state
    input_buf: Vec<f32>,
    output_buf: Vec<f32>,
}

struct LinearResponseRun {
    run_index: usize,
    input_gain_db: f32,
    params: Vec<(String, f64)>,
    sample_rate: f64,
    /// Welch accumulator: sum of |X(k)|² across windows.
    sum_input_mag_sq: Vec<f64>,
    /// Welch accumulator: sum of |Y(k)|² across windows.
    sum_output_mag_sq: Vec<f64>,
    /// Number of averaged windows.
    num_windows: usize,
}

impl LinearResponseAnalyzer {
    pub fn new(fft_size: usize) -> Self {
        let mut planner = FftPlanner::new();
        let fft = planner.plan_fft_forward(fft_size);
        Self {
            fft_size,
            fft,
            runs: Vec::new(),
            input_buf: Vec::new(),
            output_buf: Vec::new(),
        }
    }

    /// Process one FFT window worth of data through the Welch accumulator.
    fn process_welch_window(&mut self) {
        let run = match self.runs.last_mut() {
            Some(r) => r,
            None => return,
        };

        let fft_size = self.fft_size;
        let num_bins = fft_size / 2;

        // Initialize accumulators on first window
        if run.sum_input_mag_sq.is_empty() {
            run.sum_input_mag_sq = vec![0.0; num_bins];
            run.sum_output_mag_sq = vec![0.0; num_bins];
        }

        // Apply Hann window and FFT the input
        let mut x_buf: Vec<Complex<f32>> = (0..fft_size)
            .map(|i| Complex::new(self.input_buf[i] * hann(i, fft_size), 0.0))
            .collect();
        self.fft.process(&mut x_buf);

        // Apply Hann window and FFT the output
        let mut y_buf: Vec<Complex<f32>> = (0..fft_size)
            .map(|i| Complex::new(self.output_buf[i] * hann(i, fft_size), 0.0))
            .collect();
        self.fft.process(&mut y_buf);

        // Accumulate |X(k)|² and |Y(k)|²
        for k in 0..num_bins {
            let x_mag = x_buf[k].norm() as f64;
            let y_mag = y_buf[k].norm() as f64;
            run.sum_input_mag_sq[k] += x_mag * x_mag;
            run.sum_output_mag_sq[k] += y_mag * y_mag;
        }
        run.num_windows += 1;
    }
}

impl Analyzer for LinearResponseAnalyzer {
    fn prepare_run(&mut self, point: &GridPoint) {
        self.input_buf.clear();
        self.output_buf.clear();
        self.runs.push(LinearResponseRun {
            run_index: point.index,
            input_gain_db: point.input_gain_db,
            params: point.params.clone(),
            sample_rate: 0.0, // set from first block
            sum_input_mag_sq: Vec::new(),
            sum_output_mag_sq: Vec::new(),
            num_windows: 0,
        });
    }

    fn process_block(&mut self, ctx: &BlockContext) {
        // Record sample rate from first block
        if let Some(run) = self.runs.last_mut()
            && run.sample_rate == 0.0
        {
            run.sample_rate = ctx.sample_rate;
        }

        // Accumulate samples into buffers
        for (&inp, &out) in ctx.input.iter().zip(ctx.output.iter()) {
            self.input_buf.push(inp);
            self.output_buf.push(out);

            // When we have a full window, process it
            if self.input_buf.len() >= self.fft_size {
                self.process_welch_window();
                // Drain the processed window (non-overlapping)
                self.input_buf.drain(..self.fft_size);
                self.output_buf.drain(..self.fft_size);
            }
        }
    }

    fn end_run(&mut self, _ctx: &RunContext) {
        // Discard any leftover samples that don't fill a complete window
        self.input_buf.clear();
        self.output_buf.clear();
    }

    fn write_results(&self, output_dir: &Path) -> anyhow::Result<()> {
        let path = output_dir.join("grid_linear_response.csv");
        let mut wtr = csv::Writer::from_path(&path)?;

        let param_names: Vec<&str> = if let Some(first) = self.runs.first() {
            first.params.iter().map(|(n, _)| n.as_str()).collect()
        } else {
            return Ok(());
        };

        let mut header: Vec<String> = vec!["run".into(), "input_gain_db".into()];
        for name in &param_names {
            header.push(name.to_string());
        }
        header.extend(["frequency_hz".into(), "magnitude_db".into()]);
        wtr.write_record(&header)?;

        for run in &self.runs {
            if run.num_windows == 0 {
                continue;
            }

            let num_bins = self.fft_size / 2;
            let bin_hz = run.sample_rate as f32 / self.fft_size as f32;
            let n = run.num_windows as f64;

            for k in 0..num_bins {
                let freq = k as f32 * bin_hz;
                let mag_in = (run.sum_input_mag_sq[k] / n).sqrt();
                let mag_out = (run.sum_output_mag_sq[k] / n).sqrt();

                let h_db = if mag_in > 1e-10 {
                    20.0 * (mag_out / mag_in).log10()
                } else {
                    continue;
                };

                if !h_db.is_finite() {
                    continue;
                }

                let mut record: Vec<String> = vec![
                    run.run_index.to_string(),
                    format!("{:.2}", run.input_gain_db),
                ];
                for (_, val) in &run.params {
                    record.push(format!("{:.4}", val));
                }
                record.extend([format!("{:.2}", freq), format!("{:.4}", h_db as f32)]);
                wtr.write_record(&record)?;
            }
        }
        wtr.flush()?;
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// THD — Total Harmonic Distortion (streaming, time-series output)
// ---------------------------------------------------------------------------

/// Maximum number of harmonic columns in the CSV output.
const THD_MAX_HARMONIC_COLUMNS: usize = 10;

pub struct ThdAnalyzer {
    sine_frequency: f32,
    fft_size: usize,
    fft: Arc<dyn Fft<f32>>,
    runs: Vec<ThdRun>,
    // Per-run streaming state
    output_buf: Vec<f32>,
    samples_seen: usize,
}

struct ThdMeasurement {
    centre_sample: usize,
    thd_percent: f64,
    fundamental_db: f64,
    /// Per-harmonic levels in dB (H2, H3, ..., up to dynamic cap).
    harmonic_levels_db: Vec<f64>,
}

struct ThdRun {
    run_index: usize,
    input_gain_db: f32,
    params: Vec<(String, f64)>,
    measurements: Vec<ThdMeasurement>,
}

impl ThdAnalyzer {
    pub fn new(sine_frequency: f32, fft_size: usize) -> Self {
        let mut planner = FftPlanner::new();
        let fft = planner.plan_fft_forward(fft_size);
        Self {
            sine_frequency,
            fft_size,
            fft,
            runs: Vec::new(),
            output_buf: Vec::new(),
            samples_seen: 0,
        }
    }

    /// Process one FFT window of output data, compute THD measurement.
    fn process_thd_window(&mut self, sample_rate: f64) {
        let run = match self.runs.last_mut() {
            Some(r) => r,
            None => return,
        };

        let fft_size = self.fft_size;
        let bin_width = sample_rate as f32 / fft_size as f32;
        let fundamental_bin = (self.sine_frequency / bin_width).round() as usize;

        if fundamental_bin == 0 || fundamental_bin >= fft_size / 2 {
            return;
        }

        // Dynamic harmonic cap: avoid bins above Nyquist
        let max_harmonic = (THD_MAX_HARMONIC_COLUMNS).min((fft_size / 2) / fundamental_bin);
        if max_harmonic < 2 {
            return;
        }

        // Apply Hann window and FFT
        let mut buffer: Vec<Complex<f32>> = (0..fft_size)
            .map(|i| Complex::new(self.output_buf[i] * hann(i, fft_size), 0.0))
            .collect();
        self.fft.process(&mut buffer);

        // Fundamental magnitude
        let fundamental_mag = buffer[fundamental_bin].norm() as f64;
        let fundamental_db = if fundamental_mag > 1e-10 {
            20.0 * fundamental_mag.log10()
        } else {
            f64::NEG_INFINITY
        };

        // Harmonic powers
        let mut harmonic_sum_sq = 0.0f64;
        let mut harmonic_levels_db = Vec::with_capacity(max_harmonic - 1);

        for h in 2..=max_harmonic {
            let bin = fundamental_bin * h;
            let mag = if bin < fft_size / 2 {
                buffer[bin].norm() as f64
            } else {
                0.0
            };
            harmonic_sum_sq += mag * mag;
            let h_db = if mag > 1e-10 {
                20.0 * mag.log10()
            } else {
                f64::NEG_INFINITY
            };
            harmonic_levels_db.push(h_db);
        }

        let thd_percent = if fundamental_mag > 1e-10 {
            (harmonic_sum_sq.sqrt() / fundamental_mag) * 100.0
        } else {
            0.0
        };

        let centre_sample = self.samples_seen - fft_size / 2;

        run.measurements.push(ThdMeasurement {
            centre_sample,
            thd_percent,
            fundamental_db,
            harmonic_levels_db,
        });
    }
}

impl Analyzer for ThdAnalyzer {
    fn prepare_run(&mut self, point: &GridPoint) {
        self.output_buf.clear();
        self.samples_seen = 0;
        self.runs.push(ThdRun {
            run_index: point.index,
            input_gain_db: point.input_gain_db,
            params: point.params.clone(),
            measurements: Vec::new(),
        });
    }

    fn process_block(&mut self, ctx: &BlockContext) {
        for &out in ctx.output.iter() {
            self.output_buf.push(out);
            self.samples_seen += 1;

            if self.output_buf.len() >= self.fft_size {
                self.process_thd_window(ctx.sample_rate);
                self.output_buf.drain(..self.fft_size);
            }
        }
    }

    fn end_run(&mut self, _ctx: &RunContext) {
        self.output_buf.clear();
    }

    fn write_results(&self, output_dir: &Path) -> anyhow::Result<()> {
        let path = output_dir.join("grid_thd.csv");
        let mut wtr = csv::Writer::from_path(&path)?;

        let param_names: Vec<&str> = if let Some(first) = self.runs.first() {
            first.params.iter().map(|(n, _)| n.as_str()).collect()
        } else {
            return Ok(());
        };

        let mut header: Vec<String> = vec!["run".into(), "input_gain_db".into()];
        for name in &param_names {
            header.push(name.to_string());
        }
        header.push("centre_sample".into());
        header.push("fundamental_db".into());
        header.push("thd_percent".into());
        for h in 2..=THD_MAX_HARMONIC_COLUMNS {
            header.push(format!("h{}_db", h));
        }
        wtr.write_record(&header)?;

        for run in &self.runs {
            for m in &run.measurements {
                let mut record: Vec<String> = vec![
                    run.run_index.to_string(),
                    format!("{:.2}", run.input_gain_db),
                ];
                for (_, val) in &run.params {
                    record.push(format!("{:.4}", val));
                }
                record.push(m.centre_sample.to_string());
                record.push(format!("{:.2}", m.fundamental_db));
                record.push(format!("{:.6}", m.thd_percent));
                // Write harmonic levels, padding to fixed column count
                for i in 0..(THD_MAX_HARMONIC_COLUMNS - 1) {
                    let h_db = m
                        .harmonic_levels_db
                        .get(i)
                        .copied()
                        .unwrap_or(f64::NEG_INFINITY);
                    record.push(if h_db.is_finite() {
                        format!("{:.2}", h_db)
                    } else {
                        "-inf".into()
                    });
                }
                wtr.write_record(&record)?;
            }
        }
        wtr.flush()?;
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// GainReduction — per-sample gain reduction time series
// ---------------------------------------------------------------------------

/// Per-sample gain reduction analyzer. Computes `output_db - input_db` for each
/// sample, producing a time series of gain reduction. Used for compressor curve
/// analysis with pulse-tone test signals.
pub struct GainReductionAnalyzer {
    sample_rate: f64,
    runs: Vec<GainReductionRun>,
}

struct GainReductionRun {
    run_index: usize,
    input_gain_db: f32,
    params: Vec<(String, f64)>,
    /// Per-sample gain reduction in dB (negative = compression).
    samples: Vec<f32>,
}

impl GainReductionAnalyzer {
    pub fn new(sample_rate: f64) -> Self {
        Self {
            sample_rate,
            runs: Vec::new(),
        }
    }
}

impl Analyzer for GainReductionAnalyzer {
    fn prepare_run(&mut self, point: &GridPoint) {
        self.runs.push(GainReductionRun {
            run_index: point.index,
            input_gain_db: point.input_gain_db,
            params: point.params.clone(),
            samples: Vec::new(),
        });
    }

    fn process_block(&mut self, ctx: &BlockContext) {
        let run = match self.runs.last_mut() {
            Some(r) => r,
            None => return,
        };

        for (&inp, &out) in ctx.input.iter().zip(ctx.output.iter()) {
            let in_abs = inp.abs();
            let out_abs = out.abs();

            let gr = if in_abs > 1e-10 {
                // gain reduction = 20*log10(|out|/|in|)
                20.0 * (out_abs.max(1e-10) / in_abs).log10()
            } else {
                0.0 // silence — no meaningful gain reduction
            };

            run.samples.push(gr);
        }
    }

    fn end_run(&mut self, _ctx: &RunContext) {}

    fn write_results(&self, output_dir: &Path) -> anyhow::Result<()> {
        let path = output_dir.join("gain_reduction.csv");
        let mut wtr = csv::Writer::from_path(&path)?;

        let param_names: Vec<&str> = if let Some(first) = self.runs.first() {
            first.params.iter().map(|(n, _)| n.as_str()).collect()
        } else {
            return Ok(());
        };

        let mut header: Vec<String> = vec!["run".into(), "input_gain_db".into()];
        for name in &param_names {
            header.push(name.to_string());
        }
        header.extend(["time_ms".into(), "gain_reduction_db".into()]);
        wtr.write_record(&header)?;

        let ms_per_sample = 1000.0 / self.sample_rate;

        for run in &self.runs {
            // Downsample for CSV: write every Nth sample to keep file size reasonable
            // At 48kHz, 1 sample ≈ 0.02ms. Writing every 4th gives ~0.08ms resolution.
            let step = 4;
            for (i, &gr) in run.samples.iter().enumerate().step_by(step) {
                let time_ms = i as f64 * ms_per_sample;
                let mut record: Vec<String> = vec![
                    run.run_index.to_string(),
                    format!("{:.2}", run.input_gain_db),
                ];
                for (_, val) in &run.params {
                    record.push(format!("{:.4}", val));
                }
                record.extend([format!("{:.4}", time_ms), format!("{:.4}", gr)]);
                wtr.write_record(&record)?;
            }
        }
        wtr.flush()?;
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// Factory
// ---------------------------------------------------------------------------

/// Create an analyzer by type, used by the engine.
pub fn create_analyzer(
    kind: crate::config::AnalyzerType,
    config: &crate::config::MeasureConfig,
) -> Box<dyn Analyzer> {
    use crate::config::AnalyzerType::*;
    match kind {
        RawCsv => Box::new(RawCsvAnalyzer::new()),
        RmsPeak => Box::new(RmsPeakAnalyzer::new()),
        TransferCurve => Box::new(TransferCurveAnalyzer::new()),
        LinearResponse => Box::new(LinearResponseAnalyzer::new(config.fft_size)),
        Thd => Box::new(ThdAnalyzer::new(config.sine_frequency, config.fft_size)),
        GainReduction => Box::new(GainReductionAnalyzer::new(config.sample_rate)),
    }
}
