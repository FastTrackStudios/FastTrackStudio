//! Live audio engine — cpal output stream with test tone through the processing chain.

use std::sync::{Arc, Mutex};

use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};

use crate::chain::ProcessingChain;

fn output_device_name(device: &cpal::Device) -> String {
    device
        .description()
        .map(|description| description.name().to_string())
        .unwrap_or_else(|_| "unknown".into())
}

/// Generates one sample of a sawtooth-approximation test tone at the given phase.
///
/// Sums 8 harmonics of a 110 Hz fundamental (110, 220, 330 … 880 Hz).
fn test_tone_sample(phase: f64) -> f64 {
    let mut s = 0.0f64;
    let harmonics = [1.0f64, 0.5, 0.33, 0.25, 0.167, 0.125, 0.1, 0.083];
    for (i, &amp) in harmonics.iter().enumerate() {
        s += amp * (2.0 * std::f64::consts::PI * (i + 1) as f64 * phase).sin();
    }
    s * 0.15 // scale to prevent clipping
}

/// Live audio engine: keeps a cpal output stream alive.
///
/// Drop this struct to stop the stream.
pub struct LiveAudioEngine {
    _stream: Option<cpal::Stream>,
}

impl LiveAudioEngine {
    /// Construct a disabled engine. Useful when native audio output is owned
    /// by another subsystem, or when startup should continue without audio.
    pub fn disabled() -> Self {
        Self { _stream: None }
    }

    /// Start a cpal output stream that feeds a test tone through `chain`.
    pub fn new(chain: ProcessingChain) -> Self {
        match Self::try_new(chain) {
            Ok(engine) => engine,
            Err(err) => {
                tracing::warn!("signal-audio: audio disabled: {err}");
                Self::disabled()
            }
        }
    }

    /// Try to start a cpal output stream that feeds a test tone through `chain`.
    ///
    /// Only f32 output is currently supported. Device/configuration failures
    /// are returned so applications can keep running without crashing.
    pub fn try_new(chain: ProcessingChain) -> Result<Self, String> {
        let host = cpal::default_host();

        let device = match host.default_output_device() {
            Some(d) => d,
            None => {
                return Err("no default output device available".into());
            }
        };

        let config = match device.default_output_config() {
            Ok(c) => c,
            Err(e) => {
                return Err(format!("could not get default output config: {e}"));
            }
        };

        let sample_rate = config.sample_rate() as f64;
        let channels = config.channels() as usize;

        tracing::info!(
            "signal-audio: opening output stream — device={:?}, sr={sample_rate}, ch={channels}",
            output_device_name(&device),
        );

        const FREQ_HZ: f64 = 110.0;
        let phase_inc = FREQ_HZ / sample_rate;
        let phase = Arc::new(Mutex::new(0.0f64));

        let stream = match config.sample_format() {
            cpal::SampleFormat::F32 => {
                let phase = Arc::clone(&phase);
                let chain = chain.clone();
                device
                    .build_output_stream(
                        &config.into(),
                        move |data: &mut [f32], _: &cpal::OutputCallbackInfo| {
                            let frame_count = data.len() / channels;
                            let mut left = vec![0.0f64; frame_count];
                            let mut right = vec![0.0f64; frame_count];

                            // Generate test tone
                            {
                                let mut ph = match phase.try_lock() {
                                    Ok(p) => p,
                                    Err(_) => {
                                        // Lock held elsewhere — fill with silence this block
                                        data.fill(0.0);
                                        return;
                                    }
                                };
                                for i in 0..frame_count {
                                    let s = test_tone_sample(*ph);
                                    left[i] = s;
                                    right[i] = s;
                                    *ph += phase_inc;
                                    if *ph >= 1.0 {
                                        *ph -= 1.0;
                                    }
                                }
                            }

                            // Process through the live chain.
                            chain.process(&mut left, &mut right);

                            // Interleave f64 → f32 output
                            for i in 0..frame_count {
                                let l = left[i] as f32;
                                let r = right[i] as f32;
                                for ch in 0..channels {
                                    data[i * channels + ch] = if ch == 0 { l } else { r };
                                }
                            }
                        },
                        move |err| {
                            tracing::warn!("signal-audio: stream error: {err}");
                        },
                        None,
                    )
                    .map_err(|e| format!("failed to build f32 output stream: {e}"))?
            }
            fmt => {
                return Err(format!(
                    "unsupported sample format {fmt:?}; only f32 is supported"
                ));
            }
        };

        stream
            .play()
            .map_err(|e| format!("failed to start audio stream: {e}"))?;
        tracing::info!("signal-audio: stream started");

        Ok(Self {
            _stream: Some(stream),
        })
    }
}
