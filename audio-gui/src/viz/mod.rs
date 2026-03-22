//! Audio visualizations — waveform displays, transfer curves, EQ graphs.

pub mod eq_graph;
pub mod transfer_curve;
pub mod waveform;

pub use eq_graph::{
    get_band_color, get_band_fill_color, q_to_slope_db, slope_db_to_q, EqBand, EqBandShape,
    EqGraph, StereoMode, MAX_BANDS,
};
pub use transfer_curve::TransferCurve;
pub use waveform::{PeakWaveform, WaveformDisplay};
