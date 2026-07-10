//! DDSP training: fit the additive **+ filtered-noise** synth to a real note by
//! gradient descent against a multi-resolution STFT-magnitude loss (Engel et
//! al. 2020).
//!
//! - Harmonic part: sum of damped sinusoids; per-partial amplitude + two-stage
//!   decay are learned (frequencies fixed from analysis).
//! - Noise part: a filtered-noise body. Because noise is uncorrelated with the
//!   harmonics, it adds in the POWER domain — the model magnitude is
//!   `sqrt(harmonic_mag² + noise_power)`, and `noise_power` is a learnable
//!   per-band envelope × a learnable temporal decay. So the optimizer fills the
//!   inter-partial body with noise (correct) instead of distorting harmonics.
//!
//! Both parts are learned jointly. The fitted params export to the same
//! (Partial, Residual) representation the WASM engine plays.

use anyhow::Result;
use candle_core::{DType, Device, Tensor, Var, D};
use candle_nn::{AdamW, Optimizer, ParamsAdamW};

use crate::analyze::{Partial, Residual, RES_BANDS};

/// Precomputed real-DFT basis + window + noise band matrix for one resolution.
struct Stft {
    cos: Tensor,      // [fft, nbins]
    sin: Tensor,      // [fft, nbins]
    win: Tensor,      // [fft]
    band_mat: Tensor, // [RES_BANDS, nbins] — maps band gains → per-bin noise shape
    fft: usize,
    hop: usize,
    sr: f32,
}

fn band_edges(sr: f32) -> Vec<f32> {
    let hi = (sr / 2.0).min(16_000.0);
    (0..=RES_BANDS)
        .map(|i| 50.0 * (hi / 50.0).powf(i as f32 / RES_BANDS as f32))
        .collect()
}

impl Stft {
    fn new(fft: usize, sr: f32, dev: &Device) -> Result<Self> {
        let nbins = fft / 2 + 1;
        let mut cos = vec![0f32; fft * nbins];
        let mut sin = vec![0f32; fft * nbins];
        for n in 0..fft {
            for k in 0..nbins {
                let a = -2.0 * std::f32::consts::PI * (n as f32) * (k as f32) / fft as f32;
                cos[n * nbins + k] = a.cos();
                sin[n * nbins + k] = a.sin();
            }
        }
        let win: Vec<f32> = (0..fft)
            .map(|i| (std::f32::consts::PI * i as f32 / (fft as f32 - 1.0)).sin().powi(2))
            .collect();
        // band matrix: row b = 1 at bins whose frequency falls in band b
        let edges = band_edges(sr);
        let mut band = vec![0f32; RES_BANDS * nbins];
        for k in 0..nbins {
            let f = k as f32 * sr / fft as f32;
            if f < edges[0] || f >= edges[RES_BANDS] {
                continue;
            }
            let mut b = 0;
            while b + 1 < RES_BANDS && f >= edges[b + 1] {
                b += 1;
            }
            band[b * nbins + k] = 1.0;
        }
        Ok(Self {
            cos: Tensor::from_vec(cos, (fft, nbins), dev)?,
            sin: Tensor::from_vec(sin, (fft, nbins), dev)?,
            win: Tensor::from_vec(win, fft, dev)?,
            band_mat: Tensor::from_vec(band, (RES_BANDS, nbins), dev)?,
            fft,
            hop: fft / 4,
            sr,
        })
    }

    /// Complex-magnitude spectrogram of a signal [T] → [n_frames, nbins].
    fn mag(&self, sig: &Tensor) -> Result<Tensor> {
        let t = sig.dim(0)?;
        let n_frames = (t.saturating_sub(self.fft)) / self.hop + 1;
        let mut frames = Vec::with_capacity(n_frames);
        for i in 0..n_frames {
            frames.push(sig.narrow(0, i * self.hop, self.fft)?.broadcast_mul(&self.win)?);
        }
        let frames = Tensor::stack(&frames, 0)?;
        let re = frames.matmul(&self.cos)?;
        let im = frames.matmul(&self.sin)?;
        Ok((re.sqr()? + im.sqr()?)?.sqrt()?)
    }

    /// Noise magnitude [n_frames, nbins] from a band-gain vector [1,RES_BANDS],
    /// a level scalar, and a decay scalar, over `n_frames` frames.
    fn noise_mag(
        &self,
        band_gain: &Tensor,
        level: &Tensor,
        decay: &Tensor,
        n_frames: usize,
        dev: &Device,
    ) -> Result<Tensor> {
        // spectral shape [1, nbins]
        let spectral = band_gain.matmul(&self.band_mat)?;
        // temporal [n_frames, 1]
        let tf: Vec<f32> = (0..n_frames)
            .map(|f| (f * self.hop + self.fft / 2) as f32 / self.sr)
            .collect();
        let tf = Tensor::from_vec(tf, (n_frames, 1), dev)?;
        let temporal = tf
            .broadcast_mul(&decay.reshape((1, 1))?)?
            .affine(-1.0, 0.0)?
            .exp()?
            .broadcast_mul(&level.reshape((1, 1))?)?; // [n_frames,1]
        Ok(temporal.broadcast_mul(&spectral)?) // [n_frames, nbins]
    }
}

fn rms_norm(x: &Tensor) -> Result<Tensor> {
    let rms = (x.sqr()?.mean_all()? + 1e-9)?.sqrt()?;
    Ok(x.broadcast_div(&rms)?)
}

fn log_floor(mag: &Tensor) -> Result<Tensor> {
    Ok((mag + 1e-4)?.log()?)
}

struct MultiStft {
    stfts: Vec<Stft>,
    targets: Vec<Tensor>,
    target_rms: f32,
    dev: Device,
}

impl MultiStft {
    fn new(target: &Tensor, sr: f32, dev: &Device) -> Result<Self> {
        // RMS-normalize the target so the loss optimizes SHAPE, not gain. Keep
        // the RMS so the fitted noise level can be rescaled back to real units.
        let target_rms = (target.sqr()?.mean_all()? + 1e-9)?.sqrt()?.to_scalar::<f32>()?;
        let target = rms_norm(target)?;
        let sizes = [512usize, 1024, 2048, 4096];
        let mut stfts = Vec::new();
        let mut targets = Vec::new();
        for &s in &sizes {
            let st = Stft::new(s, sr, dev)?;
            targets.push(log_floor(&st.mag(&target)?)?);
            stfts.push(st);
        }
        Ok(Self {
            stfts,
            targets,
            target_rms,
            dev: dev.clone(),
        })
    }

    /// Loss for harmonic audio + noise params. Model magnitude combines them in
    /// the power domain: sqrt(harm² + noise²).
    fn loss(
        &self,
        harm: &Tensor,
        band_gain: &Tensor,
        level: &Tensor,
        decay: &Tensor,
    ) -> Result<Tensor> {
        let harm = rms_norm(harm)?;
        let mut total: Option<Tensor> = None;
        for (st, tgt) in self.stfts.iter().zip(&self.targets) {
            let hm = st.mag(&harm)?;
            let n_frames = hm.dim(0)?;
            let nm = st.noise_mag(band_gain, level, decay, n_frames, &self.dev)?;
            let mag = (hm.sqr()? + nm.sqr()?)?.sqrt()?;
            let d = (log_floor(&mag)? - tgt)?.abs()?.mean_all()?;
            total = Some(match total {
                Some(t) => (t + d)?,
                None => d,
            });
        }
        Ok(total.unwrap())
    }
}

/// Fit a note's harmonic + noise parameters to `target` via DDSP.
/// Returns (fitted partials, fitted residual, loss0, loss1).
#[allow(clippy::too_many_arguments)]
pub fn fit_note(
    init: &[Partial],
    init_res: &Residual,
    target: &[f32],
    real: &[f32],
    sr: u32,
    steps: usize,
    verbose: bool,
) -> Result<(Vec<Partial>, Residual, f32, f32)> {
    let dev = Device::Cpu;
    let t_len = target.len();
    let tgt = Tensor::from_vec(target.to_vec(), t_len, &dev)?;
    let loss_fn = MultiStft::new(&tgt, sr as f32, &dev)?;

    let time = Tensor::from_vec(
        (0..t_len).map(|i| i as f32 / sr as f32).collect::<Vec<_>>(),
        (t_len, 1),
        &dev,
    )?;
    let k = init.len();
    let freqs = Tensor::from_vec(init.iter().map(|p| p.freq).collect::<Vec<_>>(), (1, k), &dev)?;
    let phase0 = Tensor::from_vec(
        (0..k).map(|i| (i as f32 * 0.61803399).fract() * std::f32::consts::TAU).collect::<Vec<_>>(),
        (1, k),
        &dev,
    )?;
    let sinp = time
        .broadcast_mul(&freqs)?
        .affine(std::f32::consts::TAU as f64, 0.0)?
        .broadcast_add(&phase0)?
        .sin()?; // [T,K]

    // learnable harmonic params (log/logit space)
    let v = |f: &dyn Fn(&Partial) -> f32| -> Result<Var> {
        Ok(Var::from_vec(init.iter().map(f).collect::<Vec<_>>(), (1, k), &dev)?)
    };
    let log_amp = v(&|p| p.amp.max(1e-6).ln())?;
    let log_fast = v(&|p| p.decay_fast.max(1e-3).ln())?;
    let log_slow = v(&|p| p.decay_slow.max(1e-3).ln())?;
    let logit_mix = v(&|p| logit(p.mix))?;

    // learnable noise params: band gains + level + decay (init from analysis)
    let nb = init_res.band_gain.len().max(1);
    let log_band = Var::from_vec(
        init_res.band_gain.iter().map(|&g| g.max(1e-4).ln()).collect::<Vec<_>>(),
        (1, nb),
        &dev,
    )?;
    // level in the RMS-normalized target's units
    let init_level = (init_res.level.max(1e-5) / loss_fn.target_rms).max(1e-4);
    let log_level = Var::from_vec(vec![init_level.ln()], 1, &dev)?;
    let log_ndecay = Var::from_vec(vec![init_res.decay.max(0.05).ln()], 1, &dev)?;

    let nt = time.affine(-1.0, 0.0)?; // [T,1]
    let render_harm = |la: &Var, lf: &Var, ls: &Var, lm: &Var| -> Result<Tensor> {
        let amp = la.exp()?;
        let mix = candle_nn::ops::sigmoid(lm.as_tensor())?;
        let ef = nt.broadcast_mul(&lf.exp()?)?.exp()?;
        let es = nt.broadcast_mul(&ls.exp()?)?.exp()?;
        let env = ef.broadcast_mul(&mix)?.add(&es.broadcast_mul(&(1.0 - &mix)?)?)?;
        Ok(env.broadcast_mul(&amp)?.mul(&sinp)?.sum(D::Minus1)?)
    };

    let mut opt = AdamW::new(
        vec![
            log_amp.clone(), log_fast.clone(), log_slow.clone(), logit_mix.clone(),
            log_band.clone(), log_level.clone(), log_ndecay.clone(),
        ],
        ParamsAdamW { lr: 0.05, ..Default::default() },
    )?;

    let eval = |la: &Var, lf: &Var, ls: &Var, lm: &Var, lb: &Var, ll: &Var, ld: &Var| -> Result<Tensor> {
        let harm = render_harm(la, lf, ls, lm)?;
        loss_fn.loss(&harm, &lb.exp()?, &ll.exp()?, &ld.exp()?)
    };

    let g1 = |t: &Var| -> Result<Vec<f32>> { Ok(t.exp()?.flatten_all()?.to_vec1::<f32>()?) };
    let extract = |la: &Var, lf: &Var, ls: &Var, lm: &Var| -> Result<Vec<Partial>> {
        let amp = g1(la)?;
        let fast = g1(lf)?;
        let slow = g1(ls)?;
        let mixv = candle_nn::ops::sigmoid(lm.as_tensor())?.flatten_all()?.to_vec1::<f32>()?;
        Ok(init.iter().enumerate().map(|(i, p)| Partial {
            k: p.k, freq: p.freq, amp: amp[i], decay_fast: fast[i], decay_slow: slow[i], mix: mixv[i],
        }).collect())
    };

    let loss0 = eval(&log_amp, &log_fast, &log_slow, &logit_mix, &log_band, &log_level, &log_ndecay)?
        .to_scalar::<f32>()?;

    // Validation-based early stopping AGAINST an accuracy proxy (cheap
    // single-scale LSD): keep the harmonic params that score best on the real
    // sample. Start from the init — so a cell can NEVER get worse than baseline
    // (if training only overfits, we return the init params).
    let qv = QuickVal::new(real, sr, 0.6);
    let vlen = qv.val_len();
    let mut best_partials = init.to_vec();
    let mut best_lsd = qv.score(&render_additive(&best_partials, sr, vlen));
    let lsd0 = best_lsd;

    for step in 0..steps {
        let loss = eval(&log_amp, &log_fast, &log_slow, &logit_mix, &log_band, &log_level, &log_ndecay)?;
        opt.backward_step(&loss)?;
        if step % 20 == 0 || step + 1 == steps {
            let cur = extract(&log_amp, &log_fast, &log_slow, &logit_mix)?;
            let lsd = qv.score(&render_additive(&cur, sr, vlen));
            if lsd < best_lsd {
                best_lsd = lsd;
                best_partials = cur;
            }
            if verbose {
                eprintln!("  step {step:4}  loss {:.4}  vlsd {lsd:.2}  best {best_lsd:.2}",
                    loss.to_scalar::<f32>()?);
            }
        }
    }
    let fitted = best_partials;

    let band_gain = g1(&log_band)?;
    // The loss RMS-normalizes the harmonic, so the learned noise level is
    // RELATIVE to unit-RMS harmonic — the renderer must normalize the harmonic
    // to unit RMS before adding the noise at this level (do NOT rescale here).
    let level = log_level.exp()?.flatten_all()?.to_vec1::<f32>()?[0];
    let decay = log_ndecay.exp()?.flatten_all()?.to_vec1::<f32>()?[0];
    let fitted_res = Residual {
        band_gain,
        band_hz: init_res.band_hz.clone(),
        decay,
        level,
    };
    let _ = loss0;
    Ok((fitted, fitted_res, lsd0, best_lsd))
}

fn logit(x: f32) -> f32 {
    let x = x.clamp(1e-4, 1.0 - 1e-4);
    (x / (1.0 - x)).ln()
}

/// Lightweight single-scale LSD validator for early-stopping DURING training:
/// one pre-planned 2048-pt FFT over a short window, target precomputed once.
/// Cheap enough to call every N steps × many parallel cells (the full
/// multi-resolution `accuracy_lsd` is ~50× heavier — use it only for reports).
pub struct QuickVal {
    fft: std::sync::Arc<dyn rustfft::Fft<f32>>,
    win: Vec<f32>,
    size: usize,
    hop: usize,
    val_len: usize,
    tgt: Vec<Vec<f32>>,
}

impl QuickVal {
    pub fn new(real: &[f32], sr: u32, secs: f32) -> Self {
        let size = 2048usize;
        let hop = 512usize;
        let val_len = ((secs * sr as f32) as usize).min(real.len());
        let mut planner = rustfft::FftPlanner::new();
        let fft = planner.plan_fft_forward(size);
        let win: Vec<f32> = (0..size)
            .map(|i| (std::f32::consts::PI * i as f32 / (size as f32 - 1.0)).sin().powi(2))
            .collect();
        let qv = Self {
            fft,
            win,
            size,
            hop,
            val_len,
            tgt: Vec::new(),
        };
        let tgt = qv.frames(&real[..val_len]);
        Self { tgt, ..qv }
    }

    fn frames(&self, sig: &[f32]) -> Vec<Vec<f32>> {
        use rustfft::num_complex::Complex;
        let peak = sig.iter().cloned().fold(0.0f32, |m, x| m.max(x.abs())).max(1e-9);
        let mut out = Vec::new();
        let mut pos = 0;
        while pos + self.size <= sig.len() {
            let mut buf: Vec<Complex<f32>> = (0..self.size)
                .map(|i| Complex::new(sig[pos + i] / peak * self.win[i], 0.0))
                .collect();
            self.fft.process(&mut buf);
            out.push(
                buf[..self.size / 2]
                    .iter()
                    .map(|c| (20.0 * (c.norm()).log10()).max(-80.0))
                    .collect(),
            );
            pos += self.hop;
        }
        out
    }

    pub fn val_len(&self) -> usize {
        self.val_len
    }

    /// LSD (dB) of `model` (same length as the validation window) vs the target.
    pub fn score(&self, model: &[f32]) -> f32 {
        let mf = self.frames(model);
        let n = mf.len().min(self.tgt.len());
        if n == 0 {
            return 1e3;
        }
        let (mut acc, mut cnt) = (0.0f64, 0.0f64);
        for f in 0..n {
            for k in 0..mf[f].len() {
                let d = (mf[f][k] - self.tgt[f][k]) as f64;
                acc += d * d;
                cnt += 1.0;
            }
        }
        (acc / cnt.max(1.0)).sqrt() as f32
    }
}

/// Pure additive render of the (two-stage) partials — matches the DDSP harmonic
/// model exactly (no jitter/attack), for A/B WAVs and LSD scoring.
pub fn render_additive(partials: &[Partial], sr: u32, n: usize) -> Vec<f32> {
    let mut out = vec![0.0f32; n];
    let dt = 1.0 / sr as f32;
    for p in partials {
        if p.freq <= 0.0 || p.freq >= sr as f32 / 2.0 {
            continue;
        }
        let w = std::f32::consts::TAU * p.freq / sr as f32;
        let phase0 = (p.k as f32 * 0.61803399).fract() * std::f32::consts::TAU;
        for (i, o) in out.iter_mut().enumerate() {
            let t = i as f32 * dt;
            let env = p.amp
                * (p.mix * (-p.decay_fast * t).exp() + (1.0 - p.mix) * (-p.decay_slow * t).exp());
            *o += env * (w * i as f32 + phase0).sin();
        }
    }
    out
}
