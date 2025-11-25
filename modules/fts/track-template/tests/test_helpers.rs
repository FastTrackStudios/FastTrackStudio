//! Helper functions for real multitrack tests

use track_template::{Template, Track};
use naming_convention::{TrackName, format_track_name_default};

/// Helper to create a track name and format it
pub fn create_track_name(
    group_prefix: &str,
    sub_type: Option<&str>,
    track_type: Option<&str>,
    performer: Option<&str>,
    arrangement: Option<&str>,
    channel: Option<&str>,
    multi_mic: Option<&str>,
    increment: Option<&str>,
    section: Option<&str>,
    layers: Option<&str>,
) -> String {
    let mut track_name = TrackName::new();
    track_name.group_prefix = Some(group_prefix.to_string());
    if let Some(st) = sub_type {
        track_name.sub_type = Some(vec![st.to_string()]);
    }
    if let Some(tt) = track_type {
        track_name.track_type = Some(tt.to_string());
    }
    if let Some(p) = performer {
        track_name.performer = Some(p.to_string());
    }
    if let Some(a) = arrangement {
        track_name.arrangement = Some(a.to_string());
    }
    if let Some(c) = channel {
        track_name.channel = Some(c.to_string());
    }
    if let Some(mm) = multi_mic {
        track_name.multi_mic = Some(vec![mm.to_string()]);
    }
    if let Some(inc) = increment {
        track_name.increment = Some(inc.to_string());
    }
    if let Some(sec) = section {
        track_name.section = Some(sec.to_string());
    }
    if let Some(l) = layers {
        track_name.layers = Some(l.to_string());
    }
    format_track_name_default(&track_name)
}

/// Helper to add a track to a template
pub fn add_track(template: &mut Template, formatted_name: &str, parent: Option<&str>) {
    let mut track = Track::new(formatted_name);
    
    if let Some(p) = parent {
        track.set_parent(p);
    }
    
    template.add_track(track);
}

