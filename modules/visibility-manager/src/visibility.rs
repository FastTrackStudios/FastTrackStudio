//! Track visibility operations
//!
//! Defines visibility state and operations for tracks.

use serde::{Deserialize, Serialize};

/// What visibility target to affect
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
pub enum VisibilityTarget {
    /// Track Control Panel (arrange view)
    #[default]
    TCP,
    /// Mixer Control Panel
    MCP,
    /// Both TCP and MCP
    Both,
}

impl VisibilityTarget {
    pub fn affects_tcp(&self) -> bool {
        matches!(self, VisibilityTarget::TCP | VisibilityTarget::Both)
    }

    pub fn affects_mcp(&self) -> bool {
        matches!(self, VisibilityTarget::MCP | VisibilityTarget::Both)
    }
}

/// Track visibility state
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrackVisibility {
    /// Track index in the project
    pub track_index: usize,

    /// Whether track is visible in TCP (arrange view)
    pub tcp_visible: bool,

    /// Whether track is visible in MCP (mixer)
    pub mcp_visible: bool,

    /// Track height in TCP (if supported)
    pub tcp_height: Option<i32>,

    /// Track height in MCP (if supported)
    pub mcp_height: Option<i32>,

    /// Whether track folder is collapsed in TCP
    pub tcp_collapsed: bool,

    /// Whether track folder is collapsed in MCP
    pub mcp_collapsed: bool,
}

impl TrackVisibility {
    /// Create a new visibility state for a track
    pub fn new(track_index: usize) -> Self {
        Self {
            track_index,
            tcp_visible: true,
            mcp_visible: true,
            tcp_height: None,
            mcp_height: None,
            tcp_collapsed: false,
            mcp_collapsed: false,
        }
    }

    /// Create visibility state from a DAW track
    pub fn from_track(track_index: usize, track: &daw::tracks::Track) -> Self {
        Self {
            track_index,
            tcp_visible: track.show_in_track_list,
            mcp_visible: track.show_in_mixer,
            tcp_height: track.track_height,
            mcp_height: None, // MCP height not in Track struct
            tcp_collapsed: matches!(
                track.folder_state_tcp,
                Some(daw::tracks::api::folder::TcpFolderState::Collapsed)
            ),
            mcp_collapsed: matches!(
                track.folder_state_mcp,
                Some(daw::tracks::api::folder::McpFolderState::Collapsed)
            ),
        }
    }

    /// Check if track is visible for the given target
    pub fn is_visible(&self, target: VisibilityTarget) -> bool {
        match target {
            VisibilityTarget::TCP => self.tcp_visible,
            VisibilityTarget::MCP => self.mcp_visible,
            VisibilityTarget::Both => self.tcp_visible && self.mcp_visible,
        }
    }

    /// Set visibility for the given target
    pub fn set_visible(&mut self, target: VisibilityTarget, visible: bool) {
        if target.affects_tcp() {
            self.tcp_visible = visible;
        }
        if target.affects_mcp() {
            self.mcp_visible = visible;
        }
    }

    /// Show the track for the given target
    pub fn show(&mut self, target: VisibilityTarget) {
        self.set_visible(target, true);
    }

    /// Hide the track for the given target
    pub fn hide(&mut self, target: VisibilityTarget) {
        self.set_visible(target, false);
    }

    /// Toggle visibility for the given target
    pub fn toggle(&mut self, target: VisibilityTarget) -> bool {
        let new_state = !self.is_visible(target);
        self.set_visible(target, new_state);
        new_state
    }

    /// Check if track folder is collapsed
    pub fn is_collapsed(&self, target: VisibilityTarget) -> bool {
        match target {
            VisibilityTarget::TCP => self.tcp_collapsed,
            VisibilityTarget::MCP => self.mcp_collapsed,
            VisibilityTarget::Both => self.tcp_collapsed && self.mcp_collapsed,
        }
    }

    /// Set folder collapse state
    pub fn set_collapsed(&mut self, target: VisibilityTarget, collapsed: bool) {
        if target.affects_tcp() {
            self.tcp_collapsed = collapsed;
        }
        if target.affects_mcp() {
            self.mcp_collapsed = collapsed;
        }
    }
}

/// A batch of visibility changes to apply
#[derive(Debug, Clone, Default)]
pub struct VisibilityChanges {
    /// Tracks to show
    pub show: Vec<usize>,
    /// Tracks to hide
    pub hide: Vec<usize>,
    /// Tracks to collapse
    pub collapse: Vec<usize>,
    /// Tracks to expand
    pub expand: Vec<usize>,
    /// Target for the changes
    pub target: VisibilityTarget,
}

impl VisibilityChanges {
    pub fn new(target: VisibilityTarget) -> Self {
        Self {
            target,
            ..Default::default()
        }
    }

    pub fn show_track(mut self, index: usize) -> Self {
        self.show.push(index);
        self
    }

    pub fn hide_track(mut self, index: usize) -> Self {
        self.hide.push(index);
        self
    }

    pub fn show_tracks(mut self, indices: impl IntoIterator<Item = usize>) -> Self {
        self.show.extend(indices);
        self
    }

    pub fn hide_tracks(mut self, indices: impl IntoIterator<Item = usize>) -> Self {
        self.hide.extend(indices);
        self
    }

    pub fn collapse_track(mut self, index: usize) -> Self {
        self.collapse.push(index);
        self
    }

    pub fn expand_track(mut self, index: usize) -> Self {
        self.expand.push(index);
        self
    }

    pub fn is_empty(&self) -> bool {
        self.show.is_empty()
            && self.hide.is_empty()
            && self.collapse.is_empty()
            && self.expand.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_visibility_state() {
        let mut vis = TrackVisibility::new(0);
        assert!(vis.tcp_visible);
        assert!(vis.mcp_visible);

        vis.hide(VisibilityTarget::TCP);
        assert!(!vis.tcp_visible);
        assert!(vis.mcp_visible);

        vis.show(VisibilityTarget::Both);
        assert!(vis.tcp_visible);
        assert!(vis.mcp_visible);
    }

    #[test]
    fn test_visibility_toggle() {
        let mut vis = TrackVisibility::new(0);
        assert!(vis.tcp_visible);

        let new_state = vis.toggle(VisibilityTarget::TCP);
        assert!(!new_state);
        assert!(!vis.tcp_visible);
    }

    #[test]
    fn test_visibility_target() {
        assert!(VisibilityTarget::TCP.affects_tcp());
        assert!(!VisibilityTarget::TCP.affects_mcp());

        assert!(!VisibilityTarget::MCP.affects_tcp());
        assert!(VisibilityTarget::MCP.affects_mcp());

        assert!(VisibilityTarget::Both.affects_tcp());
        assert!(VisibilityTarget::Both.affects_mcp());
    }
}
