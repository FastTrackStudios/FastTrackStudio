//! Load the swept parameter table for realtime playback: per (note, velocity)
//! modal partials, with nearest-layer velocity lookup and note transposition
//! for any MIDI note outside the sampled set.

use std::collections::HashMap;
use std::path::Path;

use anyhow::Result;
use serde::Deserialize;

use crate::analyze::Partial;

/// One sampled cell. Extra fields in the JSON (f0, B, T60) are ignored.
#[derive(Deserialize)]
struct Rec {
    note: u8,
    vel: u8,
    peak_rms: f32,
    modal: Vec<Partial>,
}

pub struct Voicing {
    pub modal: Vec<Partial>,
    pub peak_rms: f32,
}

pub struct Table {
    /// note → velocity layers sorted ascending by vel.
    by_note: HashMap<u8, Vec<(u8, Voicing)>>,
    notes_sorted: Vec<u8>,
}

/// Equal-tempered frequency of a MIDI note.
fn midi_hz(note: u8) -> f32 {
    440.0 * 2f32.powf((note as f32 - 69.0) / 12.0)
}

impl Table {
    pub fn load(path: &Path) -> Result<Self> {
        let recs: Vec<Rec> = serde_json::from_str(&std::fs::read_to_string(path)?)?;
        let mut by_note: HashMap<u8, Vec<(u8, Voicing)>> = HashMap::new();
        for r in recs {
            by_note.entry(r.note).or_default().push((
                r.vel,
                Voicing {
                    modal: r.modal,
                    peak_rms: r.peak_rms,
                },
            ));
        }
        for layers in by_note.values_mut() {
            layers.sort_by_key(|(v, _)| *v);
        }
        let mut notes_sorted: Vec<u8> = by_note.keys().copied().collect();
        notes_sorted.sort_unstable();
        Ok(Self {
            by_note,
            notes_sorted,
        })
    }

    /// Look up the voicing for (note, velocity). Returns the voicing plus a
    /// frequency scale to transpose it if the exact note isn't sampled.
    pub fn lookup(&self, note: u8, vel: u8) -> Option<(&Voicing, f32)> {
        // nearest sampled note
        let src = if self.by_note.contains_key(&note) {
            note
        } else {
            *self
                .notes_sorted
                .iter()
                .min_by_key(|&&n| (n as i32 - note as i32).abs())?
        };
        let layers = self.by_note.get(&src)?;
        // nearest velocity layer
        let (_, v) = layers
            .iter()
            .min_by_key(|(lv, _)| (*lv as i32 - vel as i32).abs())?;
        let scale = midi_hz(note) / midi_hz(src);
        Some((v, scale))
    }

    pub fn note_range(&self) -> (u8, u8) {
        (
            self.notes_sorted.first().copied().unwrap_or(0),
            self.notes_sorted.last().copied().unwrap_or(127),
        )
    }
}
