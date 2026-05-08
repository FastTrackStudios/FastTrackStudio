//! Processing chain — EQ + Compressor with shared-state access.

use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::{Arc, Mutex};

use comp_dsp::CompChain;
use eq_dsp::EqChain;
use eq_dsp::filter_type::FilterType;
use fts_dsp::{AudioConfig, Processor};

/// Shared EQ + Compressor processing chain.
///
/// All fields are Arc-wrapped so the chain can be cheaply cloned and
/// shared between the audio thread (cpal callback) and the UI thread.
///
/// `PartialEq` uses Arc pointer equality so Dioxus prop diffing works.
#[derive(Clone)]
pub struct ProcessingChain {
    pub eq: Arc<Mutex<EqChain>>,
    pub comp: Arc<Mutex<CompChain>>,
    /// Current gain reduction stored as f32 bits in an AtomicU32.
    pub gr_db: Arc<AtomicU32>,
}

impl PartialEq for ProcessingChain {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.eq, &other.eq)
    }
}

impl ProcessingChain {
    /// Create a new chain initialised at the given sample rate.
    ///
    /// Default bands:
    /// - Band 0: 80 Hz Highpass, order 2
    /// - Band 1: 500 Hz Peak, ±0 dB
    /// - Band 2: 8 kHz High Shelf, ±0 dB
    ///
    /// Default compressor: threshold −18 dB, ratio 4:1, attack 10 ms, release 100 ms.
    pub fn new(sample_rate: f64) -> Self {
        let config = AudioConfig {
            sample_rate,
            max_buffer_size: 512,
        };

        let mut eq = EqChain::new();
        eq.update(config);

        // Band 0: 80 Hz Highpass order 2
        let idx = eq.add_band();
        if let Some(band) = eq.band_mut(idx) {
            band.filter_type = FilterType::Highpass;
            band.freq_hz = 80.0;
            band.gain_db = 0.0;
            band.q = 0.707;
            band.order = 2;
            band.enabled = true;
        }
        eq.update_band(idx);

        // Band 1: 500 Hz Peak
        let idx = eq.add_band();
        if let Some(band) = eq.band_mut(idx) {
            band.filter_type = FilterType::Peak;
            band.freq_hz = 500.0;
            band.gain_db = 0.0;
            band.q = 1.0;
            band.order = 2;
            band.enabled = true;
        }
        eq.update_band(idx);

        // Band 2: 8 kHz High Shelf
        let idx = eq.add_band();
        if let Some(band) = eq.band_mut(idx) {
            band.filter_type = FilterType::HighShelf;
            band.freq_hz = 8000.0;
            band.gain_db = 0.0;
            band.q = 0.707;
            band.order = 2;
            band.enabled = true;
        }
        eq.update_band(idx);

        let mut comp = CompChain::new();
        comp.comp.threshold_db = -18.0;
        comp.comp.ratio = 4.0;
        comp.comp.attack_ms = 10.0;
        comp.comp.release_ms = 100.0;
        comp.comp.knee_db = 4.0;
        comp.update(config);

        Self {
            eq: Arc::new(Mutex::new(eq)),
            comp: Arc::new(Mutex::new(comp)),
            gr_db: Arc::new(AtomicU32::new(0)),
        }
    }

    /// Read the current gain reduction in dB (positive = reducing).
    pub fn gain_reduction_db(&self) -> f32 {
        f32::from_bits(self.gr_db.load(Ordering::Relaxed))
    }

    /// Process a stereo buffer through the EQ then compressor chain.
    ///
    /// Uses `try_lock` on each stage so the UI thread can access parameters
    /// without blocking the audio callback.
    pub fn process(&self, left: &mut [f64], right: &mut [f64]) {
        if let Ok(mut eq) = self.eq.try_lock() {
            eq.process(left, right);
        }

        if let Ok(mut comp) = self.comp.try_lock() {
            comp.process(left, right);
            let gr = comp.comp.last_gr_db[0].max(comp.comp.last_gr_db[1]) as f32;
            self.gr_db.store(gr.to_bits(), Ordering::Relaxed);
        }
    }
}
