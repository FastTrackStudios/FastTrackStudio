//! The REAPER guide — how FastTrackStudio drives REAPER, grounded in the
//! real `fasttrackstudio` keybind profile (transport.styx, tracks.styx).
//! A test below guards every Shortcut block against the embedded profile,
//! so config edits can't silently strand the guide.

use super::{Guide, GuideBlock, GuideSection, SectionKind, prose, see_all, shortcut, step};

/// Build the REAPER guide.
pub fn reaper_guide() -> Guide {
    Guide {
        id: "reaper",
        title: "REAPER Guide",
        intro: "Drive REAPER the FastTrackStudio way — a modal, keyboard-first \
                input layer over a stock REAPER install. This guide walks the \
                essentials; the /input page is the full searchable reference.",
        sections: vec![intro_section(), transport_section(), tracks_section()],
    }
}

fn intro_section() -> GuideSection {
    GuideSection {
        kind: SectionKind::Concept,
        id: "input-layer",
        title: "The input layer",
        body: vec![
            prose(
                "FastTrackStudio replaces REAPER's flat action list with a \
                 structured input layer. A profile is a complete keyboard \
                 personality — the canonical one is fasttrackstudio, with \
                 Logic, Pro Tools, and Ableton flavors for muscle-memory \
                 migration. Profiles are plain styx files, organized by what \
                 you're doing: transport, navigation, editing, tracks, and so \
                 on.\n\n\
                 Single keys do the frequent things (r records, space plays). \
                 Chords add modifiers the way you'd expect (Ctrl+S saves). And \
                 prefix keys open which-key overlays — press a prefix like n \
                 and a menu of follow-up keys appears, so whole families of \
                 actions hang off one mnemonic key. You never memorize blindly; \
                 the overlay teaches you as you go.",
            ),
            prose(
                "Everything below uses the fasttrackstudio profile. Keep the \
                 full reference open in another tab while you learn — it has \
                 every binding, an interactive keyboard map, and all the other \
                 profiles.",
            ),
            see_all("", "Browse the full shortcut reference"),
        ],
    }
}

fn transport_section() -> GuideSection {
    GuideSection {
        kind: SectionKind::Input,
        id: "transport",
        title: "Transport",
        body: vec![
            prose(
                "The transport is the first thing to get under your fingers — \
                 everything else in a session happens around play, stop, and \
                 record.",
            ),
            step("Start playback with Space. Press it again to stop — the edit cursor returns to where playback started, so repeated takes audition the same spot."),
            shortcut("<space>", "Play / Stop"),
            step("When you want to stop without losing your place, pause instead: Shift+Space halts playback and keeps the play cursor where it is."),
            shortcut("<S-space>", "Play / Pause"),
            step("With a time selection active, Ctrl+Shift+Space plays but skips over the selection — perfect for checking an edit by hearing straight through the splice."),
            shortcut("<C-S-space>", "Play (skip time selection)"),
            step("Jump home anytime: Enter sends the cursor to the start of the project."),
            shortcut("<enter>", "Go to start of project"),
            step("Arm and roll: r toggles recording. Combined with Space-to-stop, a take is just r, perform, Space."),
            shortcut("r", "Toggle recording"),
            step("Navigate by markers while the transport runs: , and . hop to the previous and next marker."),
            shortcut(",", "Go to previous marker"),
            shortcut(".", "Go to next marker"),
            see_all("transport", "See every Transport shortcut"),
        ],
    }
}

fn tracks_section() -> GuideSection {
    GuideSection {
        kind: SectionKind::Input,
        id: "tracks",
        title: "Tracks",
        body: vec![
            prose(
                "Track work is where the which-key layer shines: one plain \
                 chord for the raw insert, and two prefix menus — n for the \
                 track manager, Shift+N for creating fully-configured session \
                 tracks.",
            ),
            step("The plain insert is a chord: Ctrl+T drops a new empty track below the selection, exactly like stock REAPER."),
            shortcut("<C-t>", "Insert new track"),
            step("Press n to open the Track Manager menu. The overlay lists every follow-up key — each letter is mnemonic for the entity being added."),
            shortcut("n n", "New blank track"),
            shortcut("n d", "Duplicate selected tracks"),
            step("The same menu drives the FTS Session track manager: a adds an arrangement, c a channel, l a layer, m a multi-mic group, p a performer."),
            shortcut("n a", "Add arrangement"),
            shortcut("n c", "Add channel"),
            step("Press Shift+N to create categorized session tracks — named, routed, and colored for their role. Keep Shift held and tap letters to create several in a row."),
            shortcut("<S-n> d", "Create drum kit"),
            shortcut("<S-n> l", "Create lead vocals"),
            shortcut("<S-n> g", "Create electric guitar"),
            shortcut("<S-n> b", "Create bass guitar"),
            shortcut("<S-n> p", "Create piano"),
            step("Branches nest: s inside the Shift+N menu opens a synth submenu — arp, bass, lead, pad."),
            shortcut("<S-n> s a", "Create synth arp"),
            see_all("tracks", "See every Tracks shortcut"),
        ],
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use input_config_proto::{SectionConfig, WhichKeyEntryDef};

    use super::*;
    use crate::components::input_tutorial::embedded::PROFILES;

    /// Collect every leaf sequence of a which-key tree as `"prefix k [k…]"`.
    fn walk(prefix: &str, entries: &[WhichKeyEntryDef], out: &mut HashSet<String>) {
        for e in entries {
            let seq = format!("{prefix} {}", e.key);
            if e.action.is_some() {
                out.insert(seq.clone());
            }
            if let Some(children) = &e.children {
                walk(&seq, children, out);
            }
        }
    }

    /// Every Shortcut block in the REAPER guide must exist in the embedded
    /// fasttrackstudio profile — either as a direct binding or as a
    /// which-key leaf sequence. Guards the guide against config drift.
    #[test]
    fn reaper_guide_shortcuts_exist_in_fasttrackstudio_profile() {
        let profile = PROFILES
            .iter()
            .find(|p| p.id == "fasttrackstudio")
            .expect("fasttrackstudio profile embedded");

        let mut valid: HashSet<String> = HashSet::new();
        for s in profile.sections {
            let config: SectionConfig = match facet_styx::from_str(s.styx) {
                Ok(c) => c,
                Err(_) => continue,
            };
            for b in config.bindings() {
                valid.insert(b.keys.clone());
            }
            for tree in config.which_key() {
                walk(&tree.prefix, &tree.entries, &mut valid);
            }
        }
        assert!(!valid.is_empty(), "profile produced no key sequences");

        let mut checked = 0;
        for section in reaper_guide().sections {
            for block in section.body {
                if let GuideBlock::Shortcut { keys, .. } = block {
                    checked += 1;
                    assert!(
                        valid.contains(&keys),
                        "guide shortcut {keys:?} (section {:?}) does not exist \
                         in the fasttrackstudio profile — update the guide or \
                         the config",
                        section.id,
                    );
                }
            }
        }
        assert!(checked > 0, "guide has no shortcut blocks to check");
    }
}
