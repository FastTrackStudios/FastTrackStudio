//! Control-rate **modulation sources** — the runtime behind the ModMatrix
//! (keys-rig roadmap §2, Omnisphere compat §7).
//!
//! Sources produce one value per render block (block-rate control). Three
//! families:
//! - [`ControlLfo`] — free-running LFO.
//! - [`ControlEnv`] — an [`Adsr`] gated by the incoming note stream (any
//!   note-on retriggers; the envelope releases when the last note lifts).
//! - [`MidiMod`] — MIDI performance controllers (mod wheel, aftertouch,
//!   pitch bend, last note-on velocity, arbitrary CC).
//!
//! A [`ModSource`] wraps one of them; `tick(events, frames)` advances it
//! through one block and returns its current value. LFO/bend are bipolar
//! (−1..+1); envelopes/wheel/velocity are unipolar (0..1).

use signal_plugin_host::PluginEvents;

use super::adsr::{Adsr, AdsrParams};

/// LFO waveform (control-rate).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LfoWave {
    Sine,
    Triangle,
    Saw,
    Square,
}

/// A free-running control LFO. Bipolar output −1..+1.
#[derive(Clone, Copy, Debug)]
pub struct ControlLfo {
    pub wave: LfoWave,
    pub rate_hz: f32,
    phase: f32,
}

impl ControlLfo {
    pub fn new(wave: LfoWave, rate_hz: f32) -> Self {
        Self {
            wave,
            rate_hz,
            phase: 0.0,
        }
    }

    /// Advance by `frames` at `sample_rate`; returns the value at the block
    /// start (one value per block — block-rate control).
    fn tick(&mut self, frames: usize, sample_rate: f32) -> f32 {
        let v = match self.wave {
            LfoWave::Sine => (core::f32::consts::TAU * self.phase).sin(),
            LfoWave::Triangle => 4.0 * (self.phase - 0.5).abs() - 1.0,
            LfoWave::Saw => 2.0 * self.phase - 1.0,
            LfoWave::Square => {
                if self.phase < 0.5 {
                    1.0
                } else {
                    -1.0
                }
            }
        };
        self.phase += self.rate_hz * frames as f32 / sample_rate.max(1.0);
        self.phase -= self.phase.floor();
        v
    }
}

/// A note-gated control envelope: retriggers on any note-on, releases when
/// the last held note lifts. Unipolar 0..1.
#[derive(Clone, Copy, Debug)]
pub struct ControlEnv {
    env: Adsr,
    held: u32,
}

impl ControlEnv {
    pub fn new(sample_rate: f32, params: AdsrParams) -> Self {
        Self {
            env: Adsr::new(sample_rate, params),
            held: 0,
        }
    }

    fn tick(&mut self, events: &PluginEvents<'_>, frames: usize) -> f32 {
        use daw::service::MidiMessage;
        for ev in events.midi {
            match ev.message {
                MidiMessage::NoteOn { velocity, .. } if velocity > 0 => {
                    self.held += 1;
                    self.env.note_on();
                }
                MidiMessage::NoteOn { .. } | MidiMessage::NoteOff { .. } => {
                    self.held = self.held.saturating_sub(1);
                    if self.held == 0 {
                        self.env.note_off();
                    }
                }
                _ => {}
            }
        }
        // Advance through the block; block-rate consumers take the end value.
        let mut v = 0.0;
        for _ in 0..frames {
            v = self.env.tick();
        }
        v
    }
}

/// MIDI performance sources.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MidiMod {
    /// Mod wheel (CC1), 0..1.
    Wheel,
    /// Channel aftertouch, 0..1.
    Aftertouch,
    /// Pitch bend, −1..+1.
    Bender,
    /// Velocity of the most recent note-on, 0..1.
    Velocity,
    /// Note number of the most recent note-on, 0..1 (keytracking).
    Key,
    /// Sample-and-hold random, redrawn per note-on, −1..+1.
    Random,
    /// Alternates 0 / 1 on each note-on.
    Alt,
    /// Always 1.0 (route depth = a constant offset).
    Constant,
    /// An arbitrary CC, 0..1.
    Cc(u8),
}

/// One compiled modulation source.
pub struct ModSource {
    kind: SourceKind,
    sample_rate: f32,
    /// Last computed value (also the held state for MIDI sources).
    value: f32,
}

enum SourceKind {
    Lfo(ControlLfo),
    Env(ControlEnv),
    Midi(MidiMod),
}

impl ModSource {
    pub fn lfo(lfo: ControlLfo, sample_rate: f32) -> Self {
        Self {
            kind: SourceKind::Lfo(lfo),
            sample_rate,
            value: 0.0,
        }
    }

    pub fn env(env: ControlEnv, sample_rate: f32) -> Self {
        Self {
            kind: SourceKind::Env(env),
            sample_rate,
            value: 0.0,
        }
    }

    pub fn midi(m: MidiMod) -> Self {
        Self {
            kind: SourceKind::Midi(m),
            sample_rate: 0.0,
            value: if m == MidiMod::Constant { 1.0 } else { 0.0 },
        }
    }

    /// Map a MIDI source name (ours or Omnisphere's) to a [`MidiMod`].
    pub fn midi_by_name(name: &str) -> Option<MidiMod> {
        Some(match name.to_ascii_lowercase().as_str() {
            "wheel" | "mod wheel" | "modwheel" => MidiMod::Wheel,
            "after" | "aftertouch" | "pressure" => MidiMod::Aftertouch,
            "bender" | "bend" | "pitchbend" => MidiMod::Bender,
            "velo" | "velocity" => MidiMod::Velocity,
            "key" | "keytrack" => MidiMod::Key,
            "random" | "random2" | "random unipolar" => MidiMod::Random,
            "alt" => MidiMod::Alt,
            "constant" | "bias1" | "bias2" => MidiMod::Constant,
            other => {
                let n = other.strip_prefix("cc")?.parse().ok()?;
                MidiMod::Cc(n)
            }
        })
    }

    pub fn set_sample_rate(&mut self, sample_rate: f32) {
        self.sample_rate = sample_rate;
        if let SourceKind::Env(e) = &mut self.kind {
            e.env.set_sample_rate(sample_rate);
        }
    }

    /// Advance through one block; returns the source's current value.
    pub fn tick(&mut self, events: &PluginEvents<'_>, frames: usize) -> f32 {
        self.value = match &mut self.kind {
            SourceKind::Lfo(l) => l.tick(frames, self.sample_rate),
            SourceKind::Env(e) => e.tick(events, frames),
            SourceKind::Midi(m) => {
                use daw::service::MidiMessage;
                let mut v = self.value;
                for ev in events.midi {
                    match (*m, &ev.message) {
                        (MidiMod::Wheel, MidiMessage::ControlChange { controller, value, .. })
                            if *controller == 1 =>
                        {
                            v = *value as f32 / 127.0;
                        }
                        (MidiMod::Cc(n), MidiMessage::ControlChange { controller, value, .. })
                            if *controller == n =>
                        {
                            v = *value as f32 / 127.0;
                        }
                        (MidiMod::Aftertouch, MidiMessage::ChannelPressure { pressure, .. }) => {
                            v = *pressure as f32 / 127.0;
                        }
                        (MidiMod::Bender, MidiMessage::PitchBend { value, .. }) => {
                            // −8192..8191 → −1..+1.
                            v = *value as f32 / 8192.0;
                        }
                        (MidiMod::Velocity, MidiMessage::NoteOn { velocity, .. })
                            if *velocity > 0 =>
                        {
                            v = *velocity as f32 / 127.0;
                        }
                        (MidiMod::Key, MidiMessage::NoteOn { note, velocity, .. })
                            if *velocity > 0 =>
                        {
                            v = *note as f32 / 127.0;
                        }
                        (MidiMod::Random, MidiMessage::NoteOn { velocity, .. })
                            if *velocity > 0 =>
                        {
                            // Redraw from a running hash of the previous value.
                            let bits = (v.to_bits() ^ 0x9E37_79B9).wrapping_mul(0xC2B2_AE35);
                            v = ((bits >> 8) as f32 / (u32::MAX >> 8) as f32) * 2.0 - 1.0;
                        }
                        (MidiMod::Alt, MidiMessage::NoteOn { velocity, .. })
                            if *velocity > 0 =>
                        {
                            v = if v > 0.5 { 0.0 } else { 1.0 };
                        }
                        _ => {}
                    }
                }
                v
            }
        };
        self.value
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use signal_plugin_host::PluginMidiEvent;

    fn no_events() -> PluginEvents<'static> {
        PluginEvents {
            params: &[],
            midi: &[],
            note_expressions: &[],
        }
    }

    #[test]
    fn lfo_cycles_bipolar() {
        // 1 Hz at 48 kHz, 512-frame blocks: sweep one second, track extremes.
        let mut src = ModSource::lfo(ControlLfo::new(LfoWave::Sine, 1.0), 48_000.0);
        let (mut lo, mut hi) = (f32::MAX, f32::MIN);
        for _ in 0..(48_000 / 512 + 1) {
            let v = src.tick(&no_events(), 512);
            lo = lo.min(v);
            hi = hi.max(v);
        }
        assert!(hi > 0.9 && lo < -0.9, "full bipolar swing, got {lo}..{hi}");
    }

    #[test]
    fn env_gates_on_notes() {
        let mut src = ModSource::env(
            ControlEnv::new(48_000.0, crate::native::AdsrParams::default()),
            48_000.0,
        );
        let on = [PluginMidiEvent {
            offset: 0,
            message: daw::service::MidiMessage::note_on(0, 60, 100),
        }];
        let ev_on = PluginEvents {
            params: &[],
            midi: &on,
            note_expressions: &[],
        };
        let v = src.tick(&ev_on, 4_800); // 100 ms — past the 3 ms attack
        assert!(v > 0.5, "gated envelope rises, v={v}");
        let off = [PluginMidiEvent {
            offset: 0,
            message: daw::service::MidiMessage::note_off(0, 60, 0),
        }];
        let ev_off = PluginEvents {
            params: &[],
            midi: &off,
            note_expressions: &[],
        };
        let mut v = 1.0;
        for _ in 0..40 {
            let e = if v == 1.0 { &ev_off } else { &no_events() };
            v = src.tick(e, 4_800);
            if v < 0.01 {
                break;
            }
        }
        assert!(v < 0.01, "released envelope decays, v={v}");
    }

    #[test]
    fn wheel_tracks_cc1() {
        let mut src = ModSource::midi(MidiMod::Wheel);
        let cc = [PluginMidiEvent {
            offset: 0,
            message: daw::service::MidiMessage::ControlChange {
                channel: 0,
                controller: 1,
                value: 64,
            },
        }];
        let ev = PluginEvents {
            params: &[],
            midi: &cc,
            note_expressions: &[],
        };
        let v = src.tick(&ev, 64);
        assert!((v - 64.0 / 127.0).abs() < 1e-3);
        // Holds its value across empty blocks.
        assert_eq!(src.tick(&no_events(), 64), v);
    }
}
