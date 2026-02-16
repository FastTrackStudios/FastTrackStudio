//! Action definitions for the Visibility Manager.
//!
//! Each action maps to a dynamic-template top-level group. The `LocalActionBinder`
//! trait is implemented by reaper-extension to toggle REAPER track visibility.

actions_proto::define_actions! {
    pub visibility_manager_actions {
        prefix: "fts.visibility_manager",
        title: "Visibility Manager",

        // ── Per-group toggles (matches dynamic-template default_config order) ──
        TOGGLE_DRUMS = "toggle_drums" {
            name: "Toggle Drums",
            description: "Toggle visibility of all Drums tracks",
            category: View,
        }
        TOGGLE_PERCUSSION = "toggle_percussion" {
            name: "Toggle Percussion",
            description: "Toggle visibility of all Percussion tracks",
            category: View,
        }
        TOGGLE_BASS = "toggle_bass" {
            name: "Toggle Bass",
            description: "Toggle visibility of all Bass tracks",
            category: View,
        }
        TOGGLE_GUITARS = "toggle_guitars" {
            name: "Toggle Guitars",
            description: "Toggle visibility of all Guitars tracks",
            category: View,
        }
        TOGGLE_KEYS = "toggle_keys" {
            name: "Toggle Keys",
            description: "Toggle visibility of all Keys tracks",
            category: View,
        }
        TOGGLE_SYNTHS = "toggle_synths" {
            name: "Toggle Synths",
            description: "Toggle visibility of all Synths tracks",
            category: View,
        }
        TOGGLE_HORNS = "toggle_horns" {
            name: "Toggle Horns",
            description: "Toggle visibility of all Horns tracks",
            category: View,
        }
        TOGGLE_HARMONICA = "toggle_harmonica" {
            name: "Toggle Harmonica",
            description: "Toggle visibility of all Harmonica tracks",
            category: View,
        }
        TOGGLE_VOCALS = "toggle_vocals" {
            name: "Toggle Vocals",
            description: "Toggle visibility of all Vocals tracks",
            category: View,
        }
        TOGGLE_CHOIR = "toggle_choir" {
            name: "Toggle Choir",
            description: "Toggle visibility of all Choir tracks",
            category: View,
        }
        TOGGLE_ORCHESTRA = "toggle_orchestra" {
            name: "Toggle Orchestra",
            description: "Toggle visibility of all Orchestra tracks",
            category: View,
        }
        TOGGLE_SFX = "toggle_sfx" {
            name: "Toggle SFX",
            description: "Toggle visibility of all SFX tracks",
            category: View,
        }
        TOGGLE_GUIDE = "toggle_guide" {
            name: "Toggle Guide",
            description: "Toggle visibility of all Guide tracks",
            category: View,
        }
        TOGGLE_REFERENCE = "toggle_reference" {
            name: "Toggle Reference",
            description: "Toggle visibility of all Reference tracks",
            category: View,
        }

        // ── Global operations ───────────────────────────────────────
        SHOW_ALL = "show_all" {
            name: "Show All",
            description: "Show all tracks (reset visibility)",
            category: View,
        }
        HIDE_ALL = "hide_all" {
            name: "Hide All",
            description: "Hide all group tracks",
            category: View,
        }
        REBUILD_CACHE = "rebuild_cache" {
            name: "Rebuild Cache",
            description: "Rebuild the track-to-group classification cache",
            category: Dev,
            group: "Dev",
        }
    }
}
