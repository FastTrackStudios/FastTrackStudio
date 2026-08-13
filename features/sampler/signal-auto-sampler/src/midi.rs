//! The MIDI side: strike notes on the instrument under test.

use eyre::{Result, WrapErr};
use midicore_midir::{MidiOutput, output_ports};
use midicore_proto::{
    Channel, ControllerNumber, ControllerValue, KeyNumber, MidiEvent, PortSelector, ProgramNumber,
    Velocity,
};

/// CC 123 — All Notes Off. Releases held notes, but lets their release tails
/// ring out naturally.
const ALL_NOTES_OFF: u8 = 123;

/// CC 120 — All Sound Off. Cuts everything immediately, including release
/// tails, which All Notes Off deliberately does not.
const ALL_SOUND_OFF: u8 = 120;

/// CC 64 — sustain pedal. A latched pedal holds notes through All Notes Off on
/// many instruments, so clearing it is part of getting to real silence.
const SUSTAIN_PEDAL: u8 = 64;

/// An open MIDI output aimed at one channel of one instrument.
pub struct Instrument {
    out: MidiOutput,
    channel: Channel,
    /// Notes currently held, so an early exit can release exactly those.
    held: Vec<u8>,
}

impl Instrument {
    /// Open the output port whose name contains `port` (case-insensitive);
    /// an empty `port` takes the default. `channel` is 1-16 as labelled on the
    /// instrument's front panel.
    pub fn open(port: &str, channel: u8) -> Result<Self> {
        let selector = if port.is_empty() {
            PortSelector::Default
        } else {
            PortSelector::NameContains(port.to_string())
        };
        let out = MidiOutput::open(selector).wrap_err_with(|| {
            let available = output_ports();
            if available.is_empty() {
                format!("no MIDI output ports found (looking for {port:?})")
            } else {
                format!(
                    "no MIDI output matching {port:?} — available: {}",
                    available.join(", ")
                )
            }
        })?;
        Ok(Self {
            out,
            // Front-panel 1-16 → wire 0-15.
            channel: Channel::new(channel.saturating_sub(1)),
            held: Vec::new(),
        })
    }

    /// Select a program (patch) on the instrument.
    pub fn program_change(&mut self, program: u8) -> Result<()> {
        self.out.send_event(&MidiEvent::ProgramChange {
            channel: self.channel,
            program: ProgramNumber::new(program),
        })
    }

    /// Strike a note.
    pub fn note_on(&mut self, note: u8, velocity: u8) -> Result<()> {
        self.out.send_event(&MidiEvent::NoteOn {
            channel: self.channel,
            key: KeyNumber::new(note),
            velocity: Velocity::new(velocity),
        })?;
        self.held.push(note);
        Ok(())
    }

    /// Release a note.
    pub fn note_off(&mut self, note: u8) -> Result<()> {
        self.out.send_event(&MidiEvent::NoteOff {
            channel: self.channel,
            key: KeyNumber::new(note),
            velocity: Velocity::new(0),
        })?;
        self.held.retain(|&n| n != note);
        Ok(())
    }

    /// Cut all sound immediately, on every channel.
    ///
    /// Stronger than [`silence`](Self::silence): it releases the sustain pedal
    /// first (a latched pedal holds notes through All Notes Off on many
    /// instruments), then sends All Sound Off, which kills release tails rather
    /// than letting them ring. Sent on all 16 channels because a stuck note
    /// need not be on the channel we are driving.
    ///
    /// Used before measuring the noise floor: starting a run against a ringing
    /// instrument poisons the measurement and aborts at calibration.
    pub fn panic(&mut self) -> Result<()> {
        self.held.clear();
        for ch in 0..16u8 {
            let channel = Channel::new(ch);
            for (controller, value) in [
                (SUSTAIN_PEDAL, 0),
                (ALL_NOTES_OFF, 0),
                (ALL_SOUND_OFF, 0),
            ] {
                self.out.send_event(&MidiEvent::ControlChange {
                    channel,
                    controller: ControllerNumber::new(controller),
                    value: ControllerValue::new(value),
                })?;
            }
        }
        Ok(())
    }

    /// Release everything, belt and braces: explicit note-offs for the notes we
    /// know are held, then All Notes Off for anything we don't.
    pub fn silence(&mut self) -> Result<()> {
        for note in std::mem::take(&mut self.held) {
            let _ = self.out.send_event(&MidiEvent::NoteOff {
                channel: self.channel,
                key: KeyNumber::new(note),
                velocity: Velocity::new(0),
            });
        }
        self.out.send_event(&MidiEvent::ControlChange {
            channel: self.channel,
            controller: ControllerNumber::new(ALL_NOTES_OFF),
            value: ControllerValue::new(0),
        })
    }
}

impl Drop for Instrument {
    fn drop(&mut self) {
        // A panic or Ctrl-C mid-note must not leave the hardware sounding.
        let _ = self.silence();
    }
}
