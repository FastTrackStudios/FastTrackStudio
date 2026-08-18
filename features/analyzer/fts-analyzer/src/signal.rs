//! Generate test signals for plugin analysis.

/// A single-sample impulse (1.0 at sample 0, then silence).
pub fn impulse(length: usize) -> Vec<f32> {
    let mut buf = vec![0.0f32; length];
    if !buf.is_empty() {
        buf[0] = 1.0;
    }
    buf
}

/// A sine wave at the given frequency.
pub fn sine(freq_hz: f32, sample_rate: f32, length: usize) -> Vec<f32> {
    (0..length)
        .map(|i| {
            let t = i as f32 / sample_rate;
            (2.0 * std::f32::consts::PI * freq_hz * t).sin()
        })
        .collect()
}

/// A sine wave at a given amplitude in dBFS.
pub fn sine_db(freq_hz: f32, sample_rate: f32, length: usize, db: f32) -> Vec<f32> {
    let gain = 10.0f32.powf(db / 20.0);
    sine(freq_hz, sample_rate, length)
        .into_iter()
        .map(|s| s * gain)
        .collect()
}

/// White noise (uniform distribution, range -1..1).
pub fn white_noise(length: usize, seed: u64) -> Vec<f32> {
    // Simple xorshift64 for reproducible noise
    let mut state = seed;
    (0..length)
        .map(|_| {
            state ^= state << 13;
            state ^= state >> 7;
            state ^= state << 17;
            // Map to -1..1
            (state as i64 as f32) / (i64::MAX as f32)
        })
        .collect()
}

/// A step function: silence for `silent_samples`, then full-scale for the rest.
/// Useful for measuring compressor attack/release.
pub fn step(silent_samples: usize, total_length: usize, amplitude_db: f32) -> Vec<f32> {
    let gain = 10.0f32.powf(amplitude_db / 20.0);
    (0..total_length)
        .map(|i| if i < silent_samples { 0.0 } else { gain })
        .collect()
}

/// Waveform shape for test signal generation.
#[derive(Debug, Clone, Copy)]
pub enum Waveform {
    Sine,
    Square,
    Saw,
}

/// Generate an amplitude-pulsing tone for compressor analysis.
///
/// Produces a carrier tone at `freq_hz` whose amplitude alternates between
/// `gain_high_db` and `gain_low_db` with configurable timing. This is the
/// same principle used by Delta Expose: the compressor reacts to the level
/// changes, and gain reduction is measured sample-by-sample.
///
/// Returns the generated signal.
#[allow(clippy::too_many_arguments)]
pub fn pulse_tone(
    freq_hz: f32,
    gain_high_db: f32,
    gain_low_db: f32,
    time_high_ms: f32,
    time_low_ms: f32,
    waveform: Waveform,
    sample_rate: f32,
    length: usize,
) -> Vec<f32> {
    let high_lin = 10.0f32.powf(gain_high_db / 20.0);
    let low_lin = 10.0f32.powf(gain_low_db / 20.0);
    let high_samples = (time_high_ms * sample_rate / 1000.0) as usize;
    let low_samples = (time_low_ms * sample_rate / 1000.0) as usize;
    let cycle_len = high_samples + low_samples;

    let mut phase = 0.0f64;
    let phase_inc = 2.0 * std::f64::consts::PI * freq_hz as f64 / sample_rate as f64;

    (0..length)
        .map(|i| {
            // Determine if we're in the high or low portion of the cycle
            let pos_in_cycle = if cycle_len > 0 { i % cycle_len } else { 0 };
            let gain = if pos_in_cycle < high_samples {
                high_lin
            } else {
                low_lin
            };

            // Generate carrier waveform
            let carrier = match waveform {
                Waveform::Sine => phase.sin() as f32,
                Waveform::Square => {
                    if (phase % (2.0 * std::f64::consts::PI)) < std::f64::consts::PI {
                        1.0
                    } else {
                        -1.0
                    }
                }
                Waveform::Saw => {
                    let t = (phase % (2.0 * std::f64::consts::PI)) / (2.0 * std::f64::consts::PI);
                    (2.0 * t - 1.0) as f32
                }
            };

            phase += phase_inc;

            carrier * gain
        })
        .collect()
}

/// A swept sine (chirp) from `start_hz` to `end_hz`.
pub fn sweep(start_hz: f32, end_hz: f32, sample_rate: f32, length: usize) -> Vec<f32> {
    let duration = length as f32 / sample_rate;
    let mut phase = 0.0f64;
    (0..length)
        .map(|i| {
            let t = i as f32 / sample_rate;
            let frac = t / duration;
            let freq = start_hz + (end_hz - start_hz) * frac;
            phase += (2.0 * std::f64::consts::PI * freq as f64) / sample_rate as f64;
            phase.sin() as f32
        })
        .collect()
}
