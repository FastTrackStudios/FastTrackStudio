//! Turning a [`Grid`] into the concrete list of notes to strike and the key /
//! velocity ranges each recording is then responsible for.
//!
//! This is the part that makes the pack *authoritative*: we choose the note and
//! velocity, so the resulting zone needs no filename parsing and no guessing.

use crate::config::Grid;

/// One recording to make: strike `note` at `velocity`, and the resulting sample
/// covers `key_min..=key_max` × `vel_min..=vel_max`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Cell {
    /// The note actually struck — becomes the zone's `root_key`.
    pub note: u8,
    /// The velocity actually struck.
    pub velocity: u8,
    /// Lowest key this sample covers (inclusive).
    pub key_min: u8,
    /// Highest key this sample covers (inclusive).
    pub key_max: u8,
    /// Lowest velocity this sample covers (inclusive).
    pub vel_min: u8,
    /// Highest velocity this sample covers (inclusive).
    pub vel_max: u8,
}

/// The sampled root notes, low to high.
///
/// Always includes `low_note`, and always includes `high_note` even when the
/// interval doesn't divide the range evenly — otherwise the top of the keyboard
/// would be covered by a sample stretched further than the interval allows.
pub fn roots(grid: &Grid) -> Vec<u8> {
    let step = grid.note_interval.max(1);
    let mut out = Vec::new();
    let mut n = grid.low_note;
    while n <= grid.high_note {
        out.push(n);
        match n.checked_add(step) {
            Some(next) => n = next,
            None => break,
        }
    }
    if out.last() != Some(&grid.high_note) {
        out.push(grid.high_note);
    }
    out
}

/// Velocity bands, low to high, as `(struck_velocity, vel_min, vel_max)`.
///
/// Each band is struck at its **top** velocity, matching the "velocity ceiling"
/// convention the styx `dynamics` list already uses. The lowest band always
/// starts at 0 and the highest always ends at 127, so no incoming velocity can
/// land in a gap and produce silence.
pub fn velocity_bands(grid: &Grid) -> Vec<(u8, u8, u8)> {
    let layers = grid.velocity_layers.max(1) as u32;
    let lo = grid.low_velocity.min(grid.high_velocity).max(1) as u32;
    let hi = grid.high_velocity.max(grid.low_velocity) as u32;

    let mut out = Vec::with_capacity(layers as usize);
    let mut prev_top = 0u32;
    for i in 0..layers {
        // Evenly spaced ceilings across [lo, hi]; single-layer collapses to hi.
        let struck = if layers == 1 {
            hi
        } else {
            lo + (hi - lo) * (i + 1) / layers
        };
        let vel_min = if i == 0 { 0 } else { prev_top + 1 };
        let vel_max = if i == layers - 1 { 127 } else { struck };
        // A degenerate range (layers > usable velocity span) would produce
        // vel_min > vel_max, which matches nothing. Clamp instead of emitting
        // an inert zone.
        let vel_max = vel_max.max(vel_min);
        out.push((
            struck.clamp(1, 127) as u8,
            vel_min.min(127) as u8,
            vel_max.min(127) as u8,
        ));
        prev_top = struck;
    }
    out
}

/// Every cell to record, in sampling order (note-major, then velocity).
pub fn cells(grid: &Grid) -> Vec<Cell> {
    let roots = roots(grid);
    let bands = velocity_bands(grid);
    let mut out = Vec::with_capacity(roots.len() * bands.len());

    for (i, &note) in roots.iter().enumerate() {
        // Key coverage runs to the midpoint between neighbouring roots, so a
        // sample is transposed at most half an interval in either direction.
        // Pitching a sample up and down equally sounds better than only ever
        // stretching it upward from the root below.
        // One above where the previous root's coverage stopped — the same
        // midpoint expression as `key_max` below, plus one. Deriving both from
        // one formula is what makes the ranges exactly contiguous: computing
        // them independently produced an overlap at the midpoint.
        let key_min = if i == 0 {
            grid.low_note
        } else {
            let prev = roots[i - 1];
            prev + (note - prev) / 2 + 1
        };
        let key_max = if i + 1 == roots.len() {
            grid.high_note
        } else {
            let next = roots[i + 1];
            note + (next - note) / 2
        };

        for &(velocity, vel_min, vel_max) in &bands {
            out.push(Cell {
                note,
                velocity,
                key_min,
                key_max,
                vel_min,
                vel_max,
            });
        }
    }
    out
}

/// Note name for filenames and logs — `61` → `Cs4`.
///
/// Uses `s` rather than `#` because these end up in filenames, where `#` is
/// legal but needs quoting in most shells.
pub fn note_name(note: u8) -> String {
    const NAMES: [&str; 12] = [
        "C", "Cs", "D", "Ds", "E", "F", "Fs", "G", "Gs", "A", "As", "B",
    ];
    let octave = note as i32 / 12 - 1;
    format!("{}{}", NAMES[note as usize % 12], octave)
}

/// Note name in the form the spec parser accepts — `61` → `C#4`.
///
/// `signal_sampler::midi::note_name_to_midi` matches `C#4`/`Db4`, not `Cs4`, so
/// the styx `lowest_note`/`highest_note` fields must use this spelling.
pub fn spec_note_name(note: u8) -> String {
    const NAMES: [&str; 12] = [
        "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B",
    ];
    let octave = note as i32 / 12 - 1;
    format!("{}{}", NAMES[note as usize % 12], octave)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn grid() -> Grid {
        Grid {
            low_note: 36,
            high_note: 48,
            note_interval: 4,
            low_velocity: 1,
            high_velocity: 127,
            velocity_layers: 3,
        }
    }

    #[test]
    fn roots_span_the_range_and_include_the_top() {
        assert_eq!(roots(&grid()), vec![36, 40, 44, 48]);
    }

    #[test]
    fn uneven_interval_still_reaches_the_highest_note() {
        let g = Grid {
            note_interval: 5,
            ..grid()
        };
        // 36, 41, 46, then 48 appended so the top key is not over-stretched.
        assert_eq!(roots(&g), vec![36, 41, 46, 48]);
    }

    #[test]
    fn key_ranges_are_contiguous_and_cover_the_whole_span() {
        let cells = cells(&grid());
        let mut by_root: Vec<(u8, u8, u8)> = cells
            .iter()
            .map(|c| (c.note, c.key_min, c.key_max))
            .collect();
        by_root.dedup();

        assert_eq!(by_root.first().unwrap().1, 36, "starts at low_note");
        assert_eq!(by_root.last().unwrap().2, 48, "ends at high_note");
        for pair in by_root.windows(2) {
            assert_eq!(
                pair[1].1,
                pair[0].2 + 1,
                "no gap or overlap between {pair:?}"
            );
        }
    }

    #[test]
    fn velocity_bands_cover_0_to_127_without_gaps() {
        let bands = velocity_bands(&grid());
        assert_eq!(bands.len(), 3);
        assert_eq!(bands[0].1, 0, "softest band catches velocity 0");
        assert_eq!(bands.last().unwrap().2, 127, "hardest band reaches 127");
        for pair in bands.windows(2) {
            assert_eq!(pair[1].1, pair[0].2 + 1, "no gap between {pair:?}");
        }
    }

    #[test]
    fn single_velocity_layer_covers_everything() {
        let g = Grid {
            velocity_layers: 1,
            ..grid()
        };
        let bands = velocity_bands(&g);
        assert_eq!(bands.len(), 1);
        assert_eq!((bands[0].1, bands[0].2), (0, 127));
    }

    #[test]
    fn chromatic_interval_gives_one_root_per_key() {
        let g = Grid {
            note_interval: 1,
            velocity_layers: 1,
            ..grid()
        };
        let cells = cells(&g);
        assert_eq!(cells.len(), 13);
        for c in &cells {
            assert_eq!(
                (c.key_min, c.key_max),
                (c.note, c.note),
                "chromatic zones cover exactly their root"
            );
        }
    }

    #[test]
    fn note_names_read_the_way_a_keyboard_is_labelled() {
        assert_eq!(note_name(60), "C4");
        assert_eq!(note_name(21), "A0");
        assert_eq!(note_name(108), "C8");
        assert_eq!(note_name(61), "Cs4", "filename form avoids '#'");
    }

    #[test]
    fn spec_note_names_use_the_spelling_the_parser_accepts() {
        assert_eq!(spec_note_name(61), "C#4");
        assert_eq!(spec_note_name(60), "C4");
        assert_eq!(spec_note_name(21), "A0");
    }
}
