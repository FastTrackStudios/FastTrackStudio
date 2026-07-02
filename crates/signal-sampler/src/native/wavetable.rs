//! Native **Wavetable** oscillator — the `Native` implementation of
//! `BlockType::Wavetable` (Omnisphere "Synth mode", Nord synth waves).
//!
//! v1 is a morphing band-limited oscillator rather than a table reader: a
//! `shape` param (0..1) crossfades sine → triangle → saw → square, with
//! PolyBLEP correction on the discontinuous waves. This stands in for the
//! 638 Omnisphere wavetables until table extraction/recreation lands — the
//! SHAPE-morph behavior matches, the exact spectra don't yet.
//!
//! Polyphonic with per-voice ADSR, same pattern as
//! [`NativeOscillator`](crate::native_osc::NativeOscillator).

use signal_plugin_host::{
    PluginDescriptor, PluginError, PluginEvents, PluginFormat, PluginInstance, PluginParamInfo,
};

use super::adsr::{Adsr, AdsrParams};

/// PolyBLEP residual for a discontinuity at phase 0 (t in 0..1, dt = inc).
#[inline]
fn poly_blep(t: f32, dt: f32) -> f32 {
    if t < dt {
        let x = t / dt;
        x + x - x * x - 1.0
    } else if t > 1.0 - dt {
        let x = (t - 1.0) / dt;
        x * x + x + x + 1.0
    } else {
        0.0
    }
}

/// One cycle of each base wave at `phase`, band-limited where needed.
#[inline]
fn waves(phase: f32, dt: f32) -> [f32; 4] {
    let sine = (core::f32::consts::TAU * phase).sin();
    let tri = 4.0 * (phase - 0.5).abs() - 1.0;
    let saw = 2.0 * phase - 1.0 - poly_blep(phase, dt);
    let mut sq = if phase < 0.5 { 1.0 } else { -1.0 };
    sq += poly_blep(phase, dt);
    sq -= poly_blep((phase + 0.5).fract(), dt);
    [sine, tri, saw, sq]
}

/// Morph 0..1 across sine → triangle → saw → square.
#[inline]
fn morph(phase: f32, dt: f32, shape: f32) -> f32 {
    let w = waves(phase, dt);
    let x = shape.clamp(0.0, 1.0) * 3.0;
    let i = (x as usize).min(2);
    let frac = x - i as f32;
    w[i] * (1.0 - frac) + w[i + 1] * frac
}

struct Voice {
    note: u8,
    phase: f32,
    inc: f32,
    amp: f32,
    env: Adsr,
}

/// A polyphonic morphing wavetable oscillator.
pub struct NativeWavetable {
    sample_rate: f32,
    /// 0..1 morph across sine → tri → saw → square (param 0).
    shape: f32,
    voices: Vec<Voice>,
    prepared: bool,
}

impl NativeWavetable {
    pub fn new(sample_rate: u32) -> Self {
        Self {
            sample_rate: sample_rate.max(1) as f32,
            shape: 2.0 / 3.0, // saw — the classic synth default
            voices: Vec::new(),
            prepared: false,
        }
    }

    #[must_use]
    pub fn with_shape(mut self, shape: f32) -> Self {
        self.shape = shape.clamp(0.0, 1.0);
        self
    }

    pub fn active_voices(&self) -> usize {
        self.voices.len()
    }

    fn note_on(&mut self, note: u8, velocity: u8) {
        if velocity == 0 {
            return self.note_off(note);
        }
        let freq = 440.0 * 2f32.powf((note as f32 - 69.0) / 12.0);
        let amp = (velocity as f32 / 127.0) * 0.15;
        if let Some(v) = self.voices.iter_mut().find(|v| v.note == note) {
            v.inc = freq / self.sample_rate;
            v.amp = amp;
            v.env.note_on();
        } else {
            let mut env = Adsr::new(self.sample_rate, AdsrParams::default());
            env.note_on();
            self.voices.push(Voice {
                note,
                phase: 0.0,
                inc: freq / self.sample_rate,
                amp,
                env,
            });
        }
    }

    fn note_off(&mut self, note: u8) {
        for v in self.voices.iter_mut().filter(|v| v.note == note) {
            v.env.note_off();
        }
    }

    fn apply_midi(&mut self, message: &daw::service::MidiMessage) {
        use daw::service::MidiMessage;
        match *message {
            MidiMessage::NoteOn { note, velocity, .. } => self.note_on(note, velocity),
            MidiMessage::NoteOff { note, .. } => self.note_off(note),
            _ => {}
        }
    }
}

impl PluginInstance for NativeWavetable {
    fn descriptor(&self) -> PluginDescriptor {
        PluginDescriptor {
            id: "signal.native.wavetable".into(),
            name: "Wavetable".into(),
            vendor: "Signal".into(),
            version: String::new(),
            format: PluginFormat::Synthetic,
        }
    }

    fn params(&mut self) -> Vec<PluginParamInfo> {
        vec![PluginParamInfo {
            id: 0,
            name: "shape".into(),
            min: 0.0,
            max: 1.0,
            default: 2.0 / 3.0,
        }]
    }
    fn param_value(&mut self, id: u32) -> Option<f64> {
        (id == 0).then_some(self.shape as f64)
    }
    fn value_to_text(&mut self, _id: u32, _value: f64) -> Option<String> {
        None
    }
    fn text_to_value(&mut self, _id: u32, _text: &str) -> Option<f64> {
        None
    }
    fn latency(&mut self) -> u32 {
        0
    }

    fn prepare(&mut self, sample_rate: f64, _block_size: u32) -> Result<(), PluginError> {
        let new_sr = sample_rate.max(1.0) as f32;
        if (new_sr - self.sample_rate).abs() > f32::EPSILON {
            let ratio = self.sample_rate / new_sr;
            for v in &mut self.voices {
                v.inc *= ratio;
                v.env.set_sample_rate(new_sr);
            }
            self.sample_rate = new_sr;
        }
        self.prepared = true;
        Ok(())
    }

    fn is_prepared(&self) -> bool {
        self.prepared
    }

    fn process_block(
        &mut self,
        _in_l: &[f32],
        _in_r: &[f32],
        out_l: &mut [f32],
        out_r: &mut [f32],
        events: &PluginEvents<'_>,
    ) -> Result<(), PluginError> {
        for &(id, value) in events.params {
            if id == 0 {
                self.shape = (value as f32).clamp(0.0, 1.0);
            }
        }
        for ev in events.midi {
            self.apply_midi(&ev.message);
        }
        let frames = out_l.len().min(out_r.len());
        for f in 0..frames {
            let mut s = 0.0f32;
            for v in &mut self.voices {
                s += morph(v.phase, v.inc, self.shape) * v.amp * v.env.tick();
                v.phase += v.inc;
                if v.phase >= 1.0 {
                    v.phase -= 1.0;
                }
            }
            out_l[f] = s;
            out_r[f] = s;
        }
        self.voices.retain(|v| !v.env.is_idle());
        Ok(())
    }

    fn deactivate(&mut self) {
        self.prepared = false;
        self.voices.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use signal_plugin_host::PluginMidiEvent;

    fn render(shape: f32, n: usize) -> Vec<f32> {
        let mut osc = NativeWavetable::new(48_000).with_shape(shape);
        osc.prepare(48_000.0, n as u32).unwrap();
        let (inl, inr) = (vec![0.0; n], vec![0.0; n]);
        let (mut outl, mut outr) = (vec![0.0; n], vec![0.0; n]);
        let midi = [PluginMidiEvent {
            offset: 0,
            message: daw::service::MidiMessage::note_on(0, 69, 100),
        }];
        let ev = PluginEvents {
            params: &[],
            midi: &midi,
            note_expressions: &[],
        };
        osc.process_block(&inl, &inr, &mut outl, &mut outr, &ev).unwrap();
        outl
    }

    fn rms(b: &[f32]) -> f32 {
        (b.iter().map(|s| s * s).sum::<f32>() / b.len() as f32).sqrt()
    }

    /// High-frequency content proxy: RMS of the first difference.
    fn hf(b: &[f32]) -> f32 {
        let d: Vec<f32> = b.windows(2).map(|w| w[1] - w[0]).collect();
        rms(&d)
    }

    #[test]
    fn all_shapes_are_audible() {
        for shape in [0.0, 1.0 / 3.0, 2.0 / 3.0, 1.0] {
            let out = render(shape, 4_096);
            assert!(rms(&out) > 1e-3, "shape {shape} audible");
        }
    }

    #[test]
    fn saw_is_brighter_than_sine() {
        let sine = render(0.0, 4_096);
        let saw = render(2.0 / 3.0, 4_096);
        assert!(
            hf(&saw) > hf(&sine) * 2.0,
            "saw carries harmonics: sine hf={} saw hf={}",
            hf(&sine),
            hf(&saw)
        );
    }

    #[test]
    fn square_is_band_limited_enough() {
        // A naive square at high pitch aliases badly; PolyBLEP keeps the
        // waveform bounded (no huge overshoot).
        let mut osc = NativeWavetable::new(48_000).with_shape(1.0);
        osc.prepare(48_000.0, 2_048).unwrap();
        let (inl, inr) = (vec![0.0; 2_048], vec![0.0; 2_048]);
        let (mut outl, mut outr) = (vec![0.0; 2_048], vec![0.0; 2_048]);
        let midi = [PluginMidiEvent {
            offset: 0,
            message: daw::service::MidiMessage::note_on(0, 108, 100), // C8
        }];
        let ev = PluginEvents {
            params: &[],
            midi: &midi,
            note_expressions: &[],
        };
        osc.process_block(&inl, &inr, &mut outl, &mut outr, &ev).unwrap();
        let peak = outl.iter().fold(0.0f32, |m, s| m.max(s.abs()));
        assert!(peak < 0.4, "bounded output at C8, peak={peak}");
    }
}
