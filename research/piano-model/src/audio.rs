//! FLAC decode → mono f32 PCM.

use std::path::Path;

use anyhow::{Context, Result};

pub struct Audio {
    pub samples: Vec<f32>, // mono, downmixed
    pub sr: u32,
}

/// Load any WAV (int 16/24/32 or float) → mono f32.
pub fn load_wav(path: &Path) -> Result<Audio> {
    let mut reader = hound::WavReader::open(path)
        .with_context(|| format!("open wav {}", path.display()))?;
    let spec = reader.spec();
    let ch = spec.channels.max(1) as usize;
    let sr = spec.sample_rate;
    let interleaved: Vec<f32> = match spec.sample_format {
        hound::SampleFormat::Float => reader.samples::<f32>().map(|s| s.unwrap_or(0.0)).collect(),
        hound::SampleFormat::Int => {
            let scale = 1.0f32 / ((1i64 << (spec.bits_per_sample - 1)) as f32);
            reader.samples::<i32>().map(|s| s.unwrap_or(0) as f32 * scale).collect()
        }
    };
    let mut mono = Vec::with_capacity(interleaved.len() / ch);
    for frame in interleaved.chunks(ch) {
        mono.push(frame.iter().sum::<f32>() / ch as f32);
    }
    Ok(Audio { samples: mono, sr })
}

/// Load by extension: .wav → WAV, else FLAC.
pub fn load_any(path: &Path) -> Result<Audio> {
    match path.extension().and_then(|e| e.to_str()) {
        Some("wav") | Some("WAV") => load_wav(path),
        _ => load_mono(path),
    }
}

pub fn load_mono(path: &Path) -> Result<Audio> {
    let mut reader = claxon::FlacReader::open(path)
        .with_context(|| format!("open flac {}", path.display()))?;
    let info = reader.streaminfo();
    let ch = info.channels as usize;
    let sr = info.sample_rate;
    let bits = info.bits_per_sample;
    let scale = 1.0f32 / ((1i64 << (bits - 1)) as f32);

    // Interleaved i32 samples across channels → mono average.
    let mut mono = Vec::with_capacity(info.samples.unwrap_or(0) as usize);
    let mut acc = 0i64;
    let mut c = 0usize;
    for s in reader.samples() {
        acc += s? as i64;
        c += 1;
        if c == ch {
            mono.push((acc as f32 / ch as f32) * scale);
            acc = 0;
            c = 0;
        }
    }
    Ok(Audio { samples: mono, sr })
}
