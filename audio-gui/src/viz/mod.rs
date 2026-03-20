//! Audio visualizations — waveform displays, transfer curves.

pub mod transfer_curve;
pub mod waveform;

pub use transfer_curve::TransferCurve;
pub use waveform::{PeakWaveform, WaveformDisplay};
