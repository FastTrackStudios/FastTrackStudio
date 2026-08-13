//! Writing takes to disk as 24-bit stereo WAV.
//!
//! 24-bit rather than 32-bit float because the pack builder re-encodes to
//! FLAC-i24 anyway — writing float first would cost disk and a lossy narrowing
//! later, with no headroom benefit for material that is already normalised
//! below full scale.

use std::path::Path;

use eyre::{Result, WrapErr};

use crate::capture::Take;

/// Largest magnitude representable in 24-bit signed PCM.
const I24_MAX: f32 = 8_388_607.0;

/// Write `take` to `path` as 24-bit stereo PCM.
pub fn write(path: &Path, take: &Take, sample_rate: u32) -> Result<()> {
    let spec = hound::WavSpec {
        channels: 2,
        sample_rate,
        bits_per_sample: 24,
        sample_format: hound::SampleFormat::Int,
    };
    let mut writer = hound::WavWriter::create(path, spec)
        .wrap_err_with(|| format!("create {}", path.display()))?;

    for i in 0..take.frames() {
        writer.write_sample(to_i24(take.left[i]))?;
        writer.write_sample(to_i24(take.right[i]))?;
    }
    writer
        .finalize()
        .wrap_err_with(|| format!("finalize {}", path.display()))?;
    Ok(())
}

/// Clamp to [-1, 1] and scale to 24-bit. Clamping matters: an interface driven
/// into clipping returns values past full scale, and letting those wrap would
/// turn a hot note into loud noise.
fn to_i24(sample: f32) -> i32 {
    (sample.clamp(-1.0, 1.0) * I24_MAX) as i32
}

/// Read a stereo WAV back as `(left, right)` in -1..=1.
///
/// Accepts whatever bit depth the file carries, not just the 24-bit this module
/// writes — a folder may hold samples from another source, and failing on those
/// would be a surprise rather than a safeguard.
pub fn read(path: &Path) -> Result<(Vec<f32>, Vec<f32>)> {
    let mut reader = hound::WavReader::open(path)
        .wrap_err_with(|| format!("open {}", path.display()))?;
    let spec = reader.spec();
    let channels = spec.channels.max(1) as usize;

    let scale = |bits: u16| 1.0f32 / ((1i64 << (bits - 1)) - 1) as f32;
    let samples: Vec<f32> = match spec.sample_format {
        hound::SampleFormat::Float => reader
            .samples::<f32>()
            .collect::<Result<Vec<_>, _>>()
            .wrap_err_with(|| format!("read {}", path.display()))?,
        hound::SampleFormat::Int => {
            let s = scale(spec.bits_per_sample);
            reader
                .samples::<i32>()
                .map(|v| v.map(|v| v as f32 * s))
                .collect::<Result<Vec<_>, _>>()
                .wrap_err_with(|| format!("read {}", path.display()))?
        }
    };

    let frames = samples.len() / channels;
    let mut left = Vec::with_capacity(frames);
    let mut right = Vec::with_capacity(frames);
    for f in samples.chunks_exact(channels) {
        left.push(f[0]);
        // Mono files feed the same signal to both sides rather than erroring.
        right.push(if channels > 1 { f[1] } else { f[0] });
    }
    Ok((left, right))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn full_scale_maps_to_the_24_bit_extremes() {
        assert_eq!(to_i24(1.0), 8_388_607);
        assert_eq!(to_i24(-1.0), -8_388_607);
        assert_eq!(to_i24(0.0), 0);
    }

    #[test]
    fn over_full_scale_clamps_instead_of_wrapping() {
        assert_eq!(to_i24(2.5), 8_388_607);
        assert_eq!(to_i24(-2.5), -8_388_607);
    }

    #[test]
    fn trim_end_drops_decayed_silence_but_keeps_the_guard() {
        let mut take = Take {
            left: vec![0.5, 0.5, 0.0001, 0.0001, 0.0001, 0.0001],
            right: vec![0.5, 0.5, 0.0001, 0.0001, 0.0001, 0.0001],
        };
        take.trim_end(0.01, 2);
        assert_eq!(take.frames(), 4, "2 loud frames + 2 frames of guard");
    }

    #[test]
    fn trim_end_leaves_a_wholly_silent_take_alone() {
        // Nothing crossed the threshold — truncating to zero would hide the
        // problem; the caller should reject it as silent instead.
        let mut take = Take {
            left: vec![0.0; 8],
            right: vec![0.0; 8],
        };
        take.trim_end(0.01, 2);
        assert_eq!(take.frames(), 8);
    }

    #[test]
    fn fade_out_ends_at_silence_and_leaves_the_head_untouched() {
        let mut take = Take {
            left: vec![1.0; 10],
            right: vec![1.0; 10],
        };
        take.fade_out(4);
        assert_eq!(take.left[5], 1.0, "before the fade");
        assert_eq!(take.left[9], 0.0, "last frame is silent");
        assert!(take.left[7] < take.left[6], "monotonically decreasing");
    }

    #[test]
    fn round_trips_through_a_real_file() {
        let dir = std::env::temp_dir().join("fts-auto-sampler-wav-test");
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("probe.wav");

        let take = Take {
            left: vec![0.0, 0.5, -0.5],
            right: vec![0.0, 0.25, -0.25],
        };
        write(&path, &take, 48_000).unwrap();

        let reader = hound::WavReader::open(&path).unwrap();
        assert_eq!(reader.spec().channels, 2);
        assert_eq!(reader.spec().sample_rate, 48_000);
        assert_eq!(reader.spec().bits_per_sample, 24);
        assert_eq!(reader.len(), 6, "3 frames × 2 channels");

        std::fs::remove_file(&path).ok();
    }
}
