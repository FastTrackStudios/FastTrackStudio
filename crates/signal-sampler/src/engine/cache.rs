//! Sample cache — loads WAV files into RAM as f32 stereo interleaved buffers.
//!
//! All samples are normalised to f32 on load. Stereo files stay stereo;
//! mono files are stored as mono. The caller decides how to mix channels.
//!
//! Loaded buffers are reference-counted so multiple voices can share one
//! allocation without copying.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::SamplerError;

// ── Loaded sample data ────────────────────────────────────────────────────────

/// A fully decoded WAV file.
#[derive(Debug, Clone)]
pub struct SampleData {
    /// PCM data — f32, normalised to [-1.0, 1.0].
    /// For stereo: interleaved L/R pairs. For mono: plain samples.
    pub frames: Arc<Vec<f32>>,
    pub channels: u16,
    pub sample_rate: u32,
    /// Total number of sample frames (frames = samples / channels).
    pub num_frames: usize,
}

impl SampleData {
    /// Read one stereo frame (or duplicate mono → stereo). Returns (L, R).
    #[inline]
    pub fn frame(&self, frame_idx: usize) -> (f32, f32) {
        let base = frame_idx * self.channels as usize;
        if base >= self.frames.len() {
            return (0.0, 0.0);
        }
        match self.channels {
            1 => {
                let s = self.frames[base];
                (s, s)
            }
            _ => {
                let l = self.frames[base];
                let r = self.frames[(base + 1).min(self.frames.len() - 1)];
                (l, r)
            }
        }
    }
}

// ── Cache ─────────────────────────────────────────────────────────────────────

/// Thread-safe sample cache.
///
/// Samples are keyed by absolute path. Once loaded, the `Arc<SampleData>` is
/// held for the lifetime of the cache — there is no eviction.
pub struct SampleCache {
    loaded: HashMap<PathBuf, Arc<SampleData>>,
}

impl SampleCache {
    pub fn new() -> Self {
        Self { loaded: HashMap::new() }
    }

    /// Get a loaded sample, loading from disk on first access.
    pub fn get(&mut self, path: &Path) -> Result<Arc<SampleData>, SamplerError> {
        if let Some(entry) = self.loaded.get(path) {
            return Ok(Arc::clone(entry));
        }
        let data = load_wav(path)?;
        let arc = Arc::new(data);
        self.loaded.insert(path.to_owned(), Arc::clone(&arc));
        Ok(arc)
    }

    /// Pre-load a set of paths. Errors are logged and skipped.
    pub fn preload<'a>(&mut self, paths: impl Iterator<Item = &'a Path>) {
        for path in paths {
            if let Err(e) = self.get(path) {
                tracing::warn!("cache: failed to preload {}: {e}", path.display());
            }
        }
    }

    /// Number of samples currently in cache.
    pub fn len(&self) -> usize { self.loaded.len() }

    pub fn is_empty(&self) -> bool { self.loaded.is_empty() }
}

// ── WAV loader ────────────────────────────────────────────────────────────────

fn load_wav(path: &Path) -> Result<SampleData, SamplerError> {
    let mut reader = hound::WavReader::open(path)
        .map_err(|e| SamplerError::Io(std::io::Error::new(std::io::ErrorKind::Other, e.to_string())))?;

    let spec = reader.spec();
    let channels = spec.channels;
    let sample_rate = spec.sample_rate;

    let frames: Vec<f32> = match spec.sample_format {
        hound::SampleFormat::Float => reader
            .samples::<f32>()
            .map(|s| s.map_err(|e| SamplerError::Io(
                std::io::Error::new(std::io::ErrorKind::Other, e.to_string())
            )))
            .collect::<Result<_, _>>()?,
        hound::SampleFormat::Int => {
            let max = (1i64 << (spec.bits_per_sample - 1)) as f32;
            reader
                .samples::<i32>()
                .map(|s| s.map(|v| v as f32 / max)
                    .map_err(|e| SamplerError::Io(
                        std::io::Error::new(std::io::ErrorKind::Other, e.to_string())
                    )))
                .collect::<Result<_, _>>()?
        }
    };

    let num_frames = frames.len() / channels as usize;
    Ok(SampleData {
        frames: Arc::new(frames),
        channels,
        sample_rate,
        num_frames,
    })
}
