//! Sync domain action definitions.
//!
//! Declares all REAPER actions owned by the sync extension. Both the
//! extension binary and integration tests import from here — single
//! source of truth.
//!
//! The dotted action ID (e.g. `fts.sync.toggle_link`) is the canonical
//! identifier. Use `StaticActionId::to_command_id()` to get the REAPER
//! command name (`FTS_SYNC_TOGGLE_LINK`).

actions_proto::define_actions! {
    /// Sync domain action ID constants.
    pub sync_actions {
        prefix: "fts.sync",
        title: "Sync",

        // ── Ableton Link ────────────────────────────────────────────────

        TOGGLE_LINK = "toggle_link" {
            name: "Toggle Ableton Link",
            description: "Enable/disable Ableton Link sync (toggles between Puppet and Off)",
            category: Dev,
        }

        LINK_PUPPET = "link_puppet" {
            name: "Link Puppet Mode",
            description: "Enable Ableton Link in Puppet mode (follow Link session)",
            category: Dev,
        }

        LINK_MASTER = "link_master" {
            name: "Link Master Mode",
            description: "Enable Ableton Link in Master mode (drive Link session)",
            category: Dev,
        }

        LINK_OFF = "link_off" {
            name: "Link Off",
            description: "Disable Ableton Link sync",
            category: Dev,
        }

        // ── Setlist sync ────────────────────────────────────────────────

        SETLIST_TOGGLE = "setlist_toggle" {
            name: "Toggle Setlist Sync",
            description: "Enable/disable live sync between song tabs and the combined setlist",
            category: Dev,
        }
    }
}
