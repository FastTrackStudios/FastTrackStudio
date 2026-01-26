//! DSP algorithms for FastTrackStudio.
//!
//! This module provides digital signal processing algorithms including:
//! - Parametric EQ with Pro-Q 4 compatible parameters
//! - Filter design and coefficient calculation
//! - FFT-based spectrum analysis
//! - EQ graph rendering (with `render` feature)

pub mod eq;

pub use eq::params::{
    BandParams, BandShape, BandStatus, ChannelMode, DynamicsParams, EqParams, GlobalParams,
    ProcessingMode, SideChainParams, SpectralParams,
};

#[cfg(feature = "render")]
pub use eq::render::{
    EqColor, EqGraphColors, EqGraphConfig, EqGraphRenderer, EqPaintCommand, LineCap, LineJoin,
    TextAnchor, commands_to_svg,
};
