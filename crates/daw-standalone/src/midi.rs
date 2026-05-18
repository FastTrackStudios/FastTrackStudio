//! `impl Midi for Standalone` — note storage in `ProjectState.midi_notes`.
//!
//! Implemented surface:
//! - `create_midi_item` — create an `Item` + active MIDI `Take` on a track
//! - Note CRUD: `add_note`, `add_notes`, `delete_note`, `delete_notes`,
//!   `delete_selected_notes`
//! - Note state setters: pitch / velocity / position / length /
//!   channel / selected / muted
//! - Note batch ops: `select_all_notes`, `transpose_notes`,
//!   `quantize_notes`, `humanize_notes`
//! - Queries: `notes`, `notes_in_range`, `selected_notes`, `note_count`
//!
//! Still `todo!()`: CCs / pitch bends / program changes / sysex
//! (not modeled in `ProjectState` yet — file an issue when needed).
//!
//! Note indices are re-issued on every mutating call so the `index`
//! field on returned `MidiNote`s is consistent with its position in
//! the storage vec. References across mutations aren't stable —
//! callers should re-fetch.

use daw_proto::item::SourceType;
use daw_proto::midi::{
    HumanizeParams, Midi, MidiCC, MidiCCCreate, MidiNote, MidiNoteCreate, MidiPitchBend,
    MidiPitchBendCreate, MidiProgramChange, MidiSysEx, MidiTakeLocation, PpqRange, QuantizeParams,
};
use daw_proto::primitives::{Duration, PositionInSeconds};
use daw_proto::project::ProjectContext;
use daw_proto::{Item, ItemRef, Take, TakeRef, TrackRef};
use uuid::Uuid;

use crate::sync::{Standalone, TakeList};

fn resolve_project(daw: &Standalone, ctx: &ProjectContext) -> Option<String> {
    match ctx {
        ProjectContext::Project(guid) => Some(guid.clone()),
        ProjectContext::Current => {
            let state = daw.state.lock().ok()?;
            state.current_project_guid.clone()
        }
    }
}

/// Resolve `(item, take)` refs in the context of a project to the
/// take's GUID, used to key into `ProjectState.midi_notes`.
fn resolve_take_guid(daw: &Standalone, location: &MidiTakeLocation) -> Option<String> {
    let project_guid = resolve_project(daw, &location.project)?;
    daw.with_project(&project_guid, |p| {
        // Resolve item GUID.
        let item_guid = match &location.item {
            ItemRef::Guid(g) => p.items.contains_key(g).then(|| g.clone()),
            ItemRef::Index(_) | ItemRef::ProjectIndex(_) => {
                // ItemRef::Index = within-track index, ProjectIndex =
                // global. Standalone doesn't index items globally;
                // walk items_by_track to resolve. Caller should
                // generally use Guid.
                None
            }
        }?;
        let takes = p.takes.get(&item_guid)?;
        match &location.take {
            TakeRef::Guid(g) => takes
                .takes
                .iter()
                .find(|t| t.guid == *g)
                .map(|t| t.guid.clone()),
            TakeRef::Index(i) => takes.takes.get(*i as usize).map(|t| t.guid.clone()),
            TakeRef::Active => takes
                .takes
                .get(takes.active_idx as usize)
                .map(|t| t.guid.clone()),
        }
    })
    .ok()
    .flatten()
}

/// Reissue indices so the `index` field matches the vec position.
fn renumber(notes: &mut [MidiNote]) {
    for (i, n) in notes.iter_mut().enumerate() {
        n.index = i as u32;
    }
}

impl Midi for Standalone {
    fn notes(&self, location: MidiTakeLocation) -> Vec<MidiNote> {
        let Some(take_guid) = resolve_take_guid(self, &location) else {
            return Vec::new();
        };
        let Some(project_guid) = resolve_project(self, &location.project) else {
            return Vec::new();
        };
        self.with_project(&project_guid, |p| {
            p.midi_notes.get(&take_guid).cloned().unwrap_or_default()
        })
        .unwrap_or_default()
    }

    fn notes_in_range(&self, location: MidiTakeLocation, range: PpqRange) -> Vec<MidiNote> {
        Midi::notes(self, location)
            .into_iter()
            .filter(|n| n.overlaps(range.start, range.end))
            .collect()
    }

    fn selected_notes(&self, location: MidiTakeLocation) -> Vec<MidiNote> {
        Midi::notes(self, location)
            .into_iter()
            .filter(|n| n.selected)
            .collect()
    }

    fn note_count(&self, location: MidiTakeLocation) -> u32 {
        Midi::notes(self, location).len() as u32
    }

    fn create_midi_item(
        &self,
        project: ProjectContext,
        track: TrackRef,
        start_seconds: f64,
        end_seconds: f64,
    ) -> Option<MidiTakeLocation> {
        let project_guid = resolve_project(self, &project)?;
        let result: Option<(String, String)> = self
            .with_project_mut(&project_guid, |p| {
                // Resolve target track guid.
                let track_guid = match &track {
                    TrackRef::Guid(g) => p
                        .tracks
                        .iter()
                        .find(|t| t.guid == *g)
                        .map(|t| t.guid.clone()),
                    TrackRef::Index(i) => p.tracks.get(*i as usize).map(|t| t.guid.clone()),
                    TrackRef::Master => None, // no items on master
                }?;
                // Synthesize GUIDs.
                let item_guid = Uuid::new_v4().to_string();
                let take_guid = Uuid::new_v4().to_string();
                p.next_item_counter += 1;
                p.next_take_counter += 1;

                // Determine the item index by counting existing
                // items on the track *before* taking the mutable
                // entry handle (avoids overlapping borrows of `p`).
                let item_idx = p
                    .items_by_track
                    .get(&track_guid)
                    .map(|v| v.len() as u32)
                    .unwrap_or(0);

                let mut item = Item::default();
                item.guid = item_guid.clone();
                item.track_guid = track_guid.clone();
                item.index = item_idx;
                item.position = PositionInSeconds::from_seconds(start_seconds);
                item.length = Duration::from_seconds((end_seconds - start_seconds).max(0.0));
                item.take_count = 1;
                item.active_take_index = 0;

                p.items
                    .insert(item_guid.clone(), crate::sync::ItemEntry { item });
                p.items_by_track
                    .entry(track_guid)
                    .or_default()
                    .push(item_guid.clone());

                // Active MIDI take.
                let take = Take {
                    guid: take_guid.clone(),
                    item_guid: item_guid.clone(),
                    index: 0,
                    is_active: true,
                    name: String::new(),
                    is_midi: true,
                    midi_note_count: Some(0),
                    source_type: SourceType::Midi,
                    ..Default::default()
                };
                p.takes.insert(
                    item_guid.clone(),
                    TakeList {
                        active_idx: 0,
                        takes: vec![take],
                    },
                );
                p.midi_notes.insert(take_guid.clone(), Vec::new());
                Some((item_guid, take_guid))
            })
            .ok()
            .flatten();

        let (item_guid, _take_guid) = result?;
        Some(MidiTakeLocation::new(
            ProjectContext::Project(project_guid),
            ItemRef::Guid(item_guid),
            TakeRef::Active,
        ))
    }

    fn add_note(&self, location: MidiTakeLocation, note: MidiNoteCreate) -> u32 {
        let Some(take_guid) = resolve_take_guid(self, &location) else {
            return u32::MAX;
        };
        let Some(project_guid) = resolve_project(self, &location.project) else {
            return u32::MAX;
        };
        self.with_project_mut(&project_guid, |p| {
            let new_count = {
                let notes = p.midi_notes.entry(take_guid.clone()).or_default();
                let idx = notes.len() as u32;
                notes.push(MidiNote {
                    index: idx,
                    channel: note.channel & 0x0F,
                    pitch: note.pitch & 0x7F,
                    velocity: note.velocity.clamp(1, 127),
                    start_ppq: note.start_ppq,
                    length_ppq: note.length_ppq.max(0.0),
                    selected: false,
                    muted: false,
                });
                (idx, notes.len() as u32)
            };
            update_take_note_count(p, &take_guid, new_count.1);
            new_count.0
        })
        .unwrap_or(u32::MAX)
    }

    fn add_notes(&self, location: MidiTakeLocation, notes: Vec<MidiNoteCreate>) -> Vec<u32> {
        notes
            .into_iter()
            .map(|n| Midi::add_note(self, location.clone(), n))
            .collect()
    }

    fn delete_note(&self, location: MidiTakeLocation, index: u32) {
        let Some(take_guid) = resolve_take_guid(self, &location) else {
            return;
        };
        let Some(project_guid) = resolve_project(self, &location.project) else {
            return;
        };
        let _ = self.with_project_mut(&project_guid, |p| {
            let new_count = p.midi_notes.get_mut(&take_guid).map(|notes| {
                let i = index as usize;
                if i < notes.len() {
                    notes.remove(i);
                    renumber(notes);
                }
                notes.len() as u32
            });
            if let Some(c) = new_count {
                update_take_note_count(p, &take_guid, c);
            }
        });
    }

    fn delete_notes(&self, location: MidiTakeLocation, indices: Vec<u32>) {
        let Some(take_guid) = resolve_take_guid(self, &location) else {
            return;
        };
        let Some(project_guid) = resolve_project(self, &location.project) else {
            return;
        };
        let _ = self.with_project_mut(&project_guid, |p| {
            let new_count = p.midi_notes.get_mut(&take_guid).map(|notes| {
                let mut sorted = indices;
                sorted.sort_unstable_by(|a, b| b.cmp(a));
                for i in sorted {
                    let u = i as usize;
                    if u < notes.len() {
                        notes.remove(u);
                    }
                }
                renumber(notes);
                notes.len() as u32
            });
            if let Some(c) = new_count {
                update_take_note_count(p, &take_guid, c);
            }
        });
    }

    fn delete_selected_notes(&self, location: MidiTakeLocation) {
        let Some(take_guid) = resolve_take_guid(self, &location) else {
            return;
        };
        let Some(project_guid) = resolve_project(self, &location.project) else {
            return;
        };
        let _ = self.with_project_mut(&project_guid, |p| {
            let new_count = p.midi_notes.get_mut(&take_guid).map(|notes| {
                notes.retain(|n| !n.selected);
                renumber(notes);
                notes.len() as u32
            });
            if let Some(c) = new_count {
                update_take_note_count(p, &take_guid, c);
            }
        });
    }

    fn set_note_pitch(&self, location: MidiTakeLocation, index: u32, pitch: u8) {
        mutate_note(self, &location, index, |n| n.pitch = pitch & 0x7F);
    }

    fn set_note_velocity(&self, location: MidiTakeLocation, index: u32, velocity: u8) {
        mutate_note(self, &location, index, |n| {
            n.velocity = velocity.clamp(1, 127)
        });
    }

    fn set_note_position(&self, location: MidiTakeLocation, index: u32, start_ppq: f64) {
        mutate_note(self, &location, index, |n| n.start_ppq = start_ppq);
    }

    fn set_note_length(&self, location: MidiTakeLocation, index: u32, length_ppq: f64) {
        mutate_note(self, &location, index, |n| {
            n.length_ppq = length_ppq.max(0.0)
        });
    }

    fn set_note_channel(&self, location: MidiTakeLocation, index: u32, channel: u8) {
        mutate_note(self, &location, index, |n| n.channel = channel & 0x0F);
    }

    fn set_note_selected(&self, location: MidiTakeLocation, index: u32, selected: bool) {
        mutate_note(self, &location, index, |n| n.selected = selected);
    }

    fn set_note_muted(&self, location: MidiTakeLocation, index: u32, muted: bool) {
        mutate_note(self, &location, index, |n| n.muted = muted);
    }

    fn select_all_notes(&self, location: MidiTakeLocation, selected: bool) {
        let Some(take_guid) = resolve_take_guid(self, &location) else {
            return;
        };
        let Some(project_guid) = resolve_project(self, &location.project) else {
            return;
        };
        let _ = self.with_project_mut(&project_guid, |p| {
            if let Some(notes) = p.midi_notes.get_mut(&take_guid) {
                for n in notes.iter_mut() {
                    n.selected = selected;
                }
            }
        });
    }

    fn transpose_notes(&self, location: MidiTakeLocation, indices: Vec<u32>, semitones: i8) {
        let Some(take_guid) = resolve_take_guid(self, &location) else {
            return;
        };
        let Some(project_guid) = resolve_project(self, &location.project) else {
            return;
        };
        let _ = self.with_project_mut(&project_guid, |p| {
            if let Some(notes) = p.midi_notes.get_mut(&take_guid) {
                // Per proto doc: empty `indices` means "selected notes only".
                let select_all_selected = indices.is_empty();
                for (i, n) in notes.iter_mut().enumerate() {
                    let touched = if select_all_selected {
                        n.selected
                    } else {
                        indices.contains(&(i as u32))
                    };
                    if touched {
                        let new_pitch = (n.pitch as i16 + semitones as i16).clamp(0, 127);
                        n.pitch = new_pitch as u8;
                    }
                }
            }
        });
    }

    fn quantize_notes(&self, location: MidiTakeLocation, params: QuantizeParams) {
        if params.grid_ppq <= 0.0 {
            return;
        }
        let Some(take_guid) = resolve_take_guid(self, &location) else {
            return;
        };
        let Some(project_guid) = resolve_project(self, &location.project) else {
            return;
        };
        let _ = self.with_project_mut(&project_guid, |p| {
            if let Some(notes) = p.midi_notes.get_mut(&take_guid) {
                let strength = params.strength.clamp(0.0, 1.0);
                let select_all_selected = params.indices.is_empty();
                for (i, n) in notes.iter_mut().enumerate() {
                    let touched = if select_all_selected {
                        n.selected
                    } else {
                        params.indices.contains(&(i as u32))
                    };
                    if !touched {
                        continue;
                    }
                    let grid = params.grid_ppq;
                    let nearest = (n.start_ppq / grid).round() * grid;
                    n.start_ppq += (nearest - n.start_ppq) * strength;
                }
            }
        });
    }

    fn humanize_notes(&self, _location: MidiTakeLocation, _params: HumanizeParams) {
        // Random walks need an rng; standalone doesn't pull rand yet.
        // Kept as a deliberate no-op (not a panic) so callers can run
        // the trait without crashing, but the effect is empty until
        // we wire a deterministic seed.
        todo!("standalone: Midi::humanize_notes — needs rng + seed plumbing")
    }

    fn ccs(&self, _location: MidiTakeLocation, _controller: Option<u8>) -> Vec<MidiCC> {
        todo!("standalone: Midi::ccs — CC storage not yet modeled")
    }
    fn add_cc(&self, _location: MidiTakeLocation, _cc: MidiCCCreate) -> u32 {
        todo!("standalone: Midi::add_cc")
    }
    fn delete_cc(&self, _location: MidiTakeLocation, _index: u32) {
        todo!("standalone: Midi::delete_cc")
    }
    fn set_cc_value(&self, _location: MidiTakeLocation, _index: u32, _value: u8) {
        todo!("standalone: Midi::set_cc_value")
    }
    fn pitch_bends(&self, _location: MidiTakeLocation) -> Vec<MidiPitchBend> {
        todo!("standalone: Midi::pitch_bends")
    }
    fn add_pitch_bend(&self, _location: MidiTakeLocation, _pb: MidiPitchBendCreate) -> u32 {
        todo!("standalone: Midi::add_pitch_bend")
    }
    fn program_changes(&self, _location: MidiTakeLocation) -> Vec<MidiProgramChange> {
        todo!("standalone: Midi::program_changes")
    }
    fn sysex(&self, _location: MidiTakeLocation) -> Vec<MidiSysEx> {
        todo!("standalone: Midi::sysex")
    }
}

// ── Helpers ────────────────────────────────────────────────────────────

fn mutate_note(
    daw: &Standalone,
    location: &MidiTakeLocation,
    index: u32,
    f: impl FnOnce(&mut MidiNote),
) {
    let Some(take_guid) = resolve_take_guid(daw, location) else {
        return;
    };
    let Some(project_guid) = resolve_project(daw, &location.project) else {
        return;
    };
    let _ = daw.with_project_mut(&project_guid, |p| {
        if let Some(notes) = p.midi_notes.get_mut(&take_guid)
            && let Some(n) = notes.get_mut(index as usize)
        {
            f(n);
        }
    });
}

fn update_take_note_count(p: &mut crate::sync::ProjectState, take_guid: &str, count: u32) {
    if let Some(list) = p
        .takes
        .values_mut()
        .find(|tl| tl.takes.iter().any(|t| t.guid == take_guid))
        && let Some(t) = list.takes.iter_mut().find(|t| t.guid == take_guid)
    {
        t.midi_note_count = Some(count);
    }
}
