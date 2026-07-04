//! Typed MIDI events and their wire (byte) codec.
//!
//! The types here are the ergonomic core of the crate: newtypes that make
//! illegal values unrepresentable (channels are `0..=15`, data bytes are
//! `0..=127`), a [`MidiEvent`] enum covering the channel-voice and common
//! system messages, and round-tripping to/from raw MIDI bytes.
//!
//! `no_std + alloc` clean — only [`MidiEvent::SysEx`] needs `alloc` (for its
//! variable-length payload) and is gated behind the `alloc` feature.

use core::fmt;

use facet::Facet;

/// A MIDI channel, `0..=15` (displayed to humans as `1..=16`).
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Facet)]
pub struct Channel(u8);

impl Channel {
    /// Construct from a raw `0..=15` value, clamping out-of-range input.
    pub const fn new(raw: u8) -> Self {
        Self(if raw > 15 { 15 } else { raw })
    }
    /// Construct from a 0-based value, returning `None` if `>= 16`.
    pub const fn try_new(raw: u8) -> Option<Self> {
        if raw < 16 { Some(Self(raw)) } else { None }
    }
    /// The raw `0..=15` value used on the wire.
    pub const fn index(self) -> u8 {
        self.0
    }
    /// The human-facing `1..=16` channel number.
    pub const fn number(self) -> u8 {
        self.0 + 1
    }
}

impl fmt::Debug for Channel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Channel({})", self.number())
    }
}

/// A 7-bit data value, `0..=127` — the shape of note numbers, velocities,
/// controller numbers, and controller values on the wire.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Facet)]
pub struct U7(u8);

impl U7 {
    pub const MIN: U7 = U7(0);
    pub const MAX: U7 = U7(127);

    /// Construct, clamping to `0..=127`.
    pub const fn new(raw: u8) -> Self {
        Self(if raw > 127 { 127 } else { raw })
    }
    /// Construct, returning `None` if `> 127` (i.e. the status bit is set).
    pub const fn try_new(raw: u8) -> Option<Self> {
        if raw < 128 { Some(Self(raw)) } else { None }
    }
    pub const fn get(self) -> u8 {
        self.0
    }
}

impl fmt::Debug for U7 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A 14-bit pitch-bend value, `0..=16383`, center `8192`.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Facet)]
pub struct PitchBend(u16);

impl PitchBend {
    pub const CENTER: PitchBend = PitchBend(8192);

    /// Construct, clamping to `0..=16383`.
    pub const fn new(raw: u16) -> Self {
        Self(if raw > 16383 { 16383 } else { raw })
    }
    pub const fn get(self) -> u16 {
        self.0
    }
    /// Signed offset from center, `-8192..=8191`.
    pub const fn offset(self) -> i16 {
        self.0 as i16 - 8192
    }
    /// The `(lsb, msb)` 7-bit halves as they appear on the wire.
    pub const fn to_bytes(self) -> (U7, U7) {
        (U7(self.0 as u8 & 0x7f), U7((self.0 >> 7) as u8 & 0x7f))
    }
    /// Reassemble from the wire `(lsb, msb)` pair.
    pub const fn from_bytes(lsb: U7, msb: U7) -> Self {
        Self(((msb.0 as u16) << 7) | lsb.0 as u16)
    }
}

impl fmt::Debug for PitchBend {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "PitchBend({:+})", self.offset())
    }
}

/// A parsed MIDI event.
///
/// Channel-voice variants carry their [`Channel`]; system messages don't.
/// This is deliberately a flat, exhaustive enum so callers can `match`
/// without reaching for raw status bytes.
#[derive(Clone, Debug, PartialEq, Eq, Facet)]
#[repr(u8)]
pub enum MidiEvent {
    /// Note released. (A `NoteOn` with velocity 0 decodes to this.)
    NoteOff { channel: Channel, note: U7, velocity: U7 },
    /// Note struck.
    NoteOn { channel: Channel, note: U7, velocity: U7 },
    /// Per-note (polyphonic) aftertouch.
    PolyAftertouch { channel: Channel, note: U7, pressure: U7 },
    /// Control change (CC).
    ControlChange { channel: Channel, controller: U7, value: U7 },
    /// Program (patch) change.
    ProgramChange { channel: Channel, program: U7 },
    /// Channel-wide aftertouch.
    ChannelPressure { channel: Channel, pressure: U7 },
    /// Pitch bend.
    PitchBend { channel: Channel, bend: PitchBend },
    /// System-realtime: timing clock (24 per quarter note).
    Clock,
    /// System-realtime: start.
    Start,
    /// System-realtime: continue.
    Continue,
    /// System-realtime: stop.
    Stop,
    /// System-realtime: active sensing.
    ActiveSensing,
    /// System-realtime: reset.
    Reset,
    /// System-exclusive payload (without the enclosing `0xF0`/`0xF7`).
    SysEx(Vec<u8>),
}

/// Error decoding a MIDI byte stream.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DecodeError {
    /// The slice was empty or a message was truncated mid-way.
    Truncated,
    /// The leading byte was a data byte, not a status byte (running status is
    /// not resolvable without prior context — decode with [`Decoder`] for that).
    UnexpectedDataByte,
    /// A status byte this codec does not model (e.g. MTC quarter-frame).
    Unsupported(u8),
}

impl MidiEvent {
    /// The channel this event targets, if it is a channel-voice message.
    pub const fn channel(&self) -> Option<Channel> {
        match self {
            MidiEvent::NoteOff { channel, .. }
            | MidiEvent::NoteOn { channel, .. }
            | MidiEvent::PolyAftertouch { channel, .. }
            | MidiEvent::ControlChange { channel, .. }
            | MidiEvent::ProgramChange { channel, .. }
            | MidiEvent::ChannelPressure { channel, .. }
            | MidiEvent::PitchBend { channel, .. } => Some(*channel),
            _ => None,
        }
    }

    /// Decode a single event from the front of `bytes`, returning the event and
    /// the number of bytes consumed. Does not handle running status; feed a
    /// [`Decoder`] for streams that use it.
    pub fn decode(bytes: &[u8]) -> Result<(MidiEvent, usize), DecodeError> {
        let status = *bytes.first().ok_or(DecodeError::Truncated)?;
        if status < 0x80 {
            return Err(DecodeError::UnexpectedDataByte);
        }

        // System messages (0xF0..=0xFF) have no channel nibble.
        if status >= 0xF0 {
            return match status {
                0xF8 => Ok((MidiEvent::Clock, 1)),
                0xFA => Ok((MidiEvent::Start, 1)),
                0xFB => Ok((MidiEvent::Continue, 1)),
                0xFC => Ok((MidiEvent::Stop, 1)),
                0xFE => Ok((MidiEvent::ActiveSensing, 1)),
                0xFF => Ok((MidiEvent::Reset, 1)),
                0xF0 => {
                    // SysEx runs until the 0xF7 terminator.
                    let end = bytes.iter().position(|&b| b == 0xF7).ok_or(DecodeError::Truncated)?;
                    Ok((MidiEvent::SysEx(bytes[1..end].to_vec()), end + 1))
                }
                other => Err(DecodeError::Unsupported(other)),
            };
        }

        let channel = Channel(status & 0x0F);
        let d1 = || bytes.get(1).copied().ok_or(DecodeError::Truncated).map(|b| U7::new(b));
        let d2 = || bytes.get(2).copied().ok_or(DecodeError::Truncated).map(|b| U7::new(b));

        match status & 0xF0 {
            0x80 => Ok((MidiEvent::NoteOff { channel, note: d1()?, velocity: d2()? }, 3)),
            0x90 => {
                let (note, velocity) = (d1()?, d2()?);
                // Note-on with velocity 0 is the canonical "note off".
                let ev = if velocity.get() == 0 {
                    MidiEvent::NoteOff { channel, note, velocity }
                } else {
                    MidiEvent::NoteOn { channel, note, velocity }
                };
                Ok((ev, 3))
            }
            0xA0 => Ok((MidiEvent::PolyAftertouch { channel, note: d1()?, pressure: d2()? }, 3)),
            0xB0 => Ok((MidiEvent::ControlChange { channel, controller: d1()?, value: d2()? }, 3)),
            0xC0 => Ok((MidiEvent::ProgramChange { channel, program: d1()? }, 2)),
            0xD0 => Ok((MidiEvent::ChannelPressure { channel, pressure: d1()? }, 2)),
            0xE0 => Ok((MidiEvent::PitchBend { channel, bend: PitchBend::from_bytes(d1()?, d2()?) }, 3)),
            other => Err(DecodeError::Unsupported(other)),
        }
    }

    /// Encode into `out`, returning the number of bytes written, or `None` if
    /// `out` is too small. `SysEx` includes its `0xF0`/`0xF7` framing.
    pub fn encode(&self, out: &mut [u8]) -> Option<usize> {
        fn put(out: &mut [u8], bytes: &[u8]) -> Option<usize> {
            if out.len() < bytes.len() {
                return None;
            }
            out[..bytes.len()].copy_from_slice(bytes);
            Some(bytes.len())
        }
        match self {
            MidiEvent::NoteOff { channel, note, velocity } => {
                put(out, &[0x80 | channel.0, note.0, velocity.0])
            }
            MidiEvent::NoteOn { channel, note, velocity } => {
                put(out, &[0x90 | channel.0, note.0, velocity.0])
            }
            MidiEvent::PolyAftertouch { channel, note, pressure } => {
                put(out, &[0xA0 | channel.0, note.0, pressure.0])
            }
            MidiEvent::ControlChange { channel, controller, value } => {
                put(out, &[0xB0 | channel.0, controller.0, value.0])
            }
            MidiEvent::ProgramChange { channel, program } => {
                put(out, &[0xC0 | channel.0, program.0])
            }
            MidiEvent::ChannelPressure { channel, pressure } => {
                put(out, &[0xD0 | channel.0, pressure.0])
            }
            MidiEvent::PitchBend { channel, bend } => {
                let (lsb, msb) = bend.to_bytes();
                put(out, &[0xE0 | channel.0, lsb.0, msb.0])
            }
            MidiEvent::Clock => put(out, &[0xF8]),
            MidiEvent::Start => put(out, &[0xFA]),
            MidiEvent::Continue => put(out, &[0xFB]),
            MidiEvent::Stop => put(out, &[0xFC]),
            MidiEvent::ActiveSensing => put(out, &[0xFE]),
            MidiEvent::Reset => put(out, &[0xFF]),
            MidiEvent::SysEx(payload) => {
                if out.len() < payload.len() + 2 {
                    return None;
                }
                out[0] = 0xF0;
                out[1..1 + payload.len()].copy_from_slice(payload);
                out[1 + payload.len()] = 0xF7;
                Some(payload.len() + 2)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn note_on_roundtrips() {
        let ev = MidiEvent::NoteOn {
            channel: Channel::new(0),
            note: U7::new(60),
            velocity: U7::new(100),
        };
        let mut buf = [0u8; 3];
        let n = ev.encode(&mut buf).unwrap();
        assert_eq!(&buf[..n], &[0x90, 60, 100]);
        assert_eq!(MidiEvent::decode(&buf[..n]).unwrap(), (ev, 3));
    }

    #[test]
    fn note_on_zero_velocity_is_note_off() {
        let (ev, _) = MidiEvent::decode(&[0x90, 60, 0]).unwrap();
        assert!(matches!(ev, MidiEvent::NoteOff { .. }));
    }

    #[test]
    fn pitch_bend_center_is_8192() {
        let (ev, _) = MidiEvent::decode(&[0xE0, 0x00, 0x40]).unwrap();
        match ev {
            MidiEvent::PitchBend { bend, .. } => {
                assert_eq!(bend.get(), 8192);
                assert_eq!(bend.offset(), 0);
            }
            _ => panic!("expected pitch bend"),
        }
    }

    #[test]
    fn channel_human_numbering() {
        assert_eq!(Channel::new(0).number(), 1);
        assert_eq!(Channel::new(15).number(), 16);
    }
}
