//! Display implementation for Template
//!
//! Provides tree-based display of track hierarchies with support for showing modes.

use crate::smart_template::core::models::template::Template;
use crate::smart_template::utils::track_helpers::TrackExt;
use daw::tracks::Track;
use std::fmt;

/// Display mode for templates
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TemplateDisplayMode {
    /// Show full hierarchy with track types
    Full,
    /// Show hierarchy with mode information
    WithModes,
    /// Show only track names (minimal)
    Minimal,
}

impl Template {
    /// Format the template as a tree with the given prefix
    fn fmt_tree_recursive(
        &self,
        f: &mut fmt::Formatter,
        track: &Track,
        prefix: &str,
        is_last: bool,
        show_modes: bool,
        visited: &mut std::collections::HashSet<usize>,
    ) -> fmt::Result {
        // Find the index of this track to track if we've visited it
        let track_idx = self.tracks.iter().position(|t| 
            std::ptr::eq(t, track) || 
            (t.name == track.name && t.track_type() == track.track_type() && t.parent_name() == track.parent_name())
        );
        
        if let Some(idx) = track_idx {
            if visited.contains(&idx) {
                // Already visited this track, skip to avoid infinite recursion
                return Ok(());
            }
            visited.insert(idx);
        }
        
        // Draw the tree connector
        let connector = if is_last { "└── " } else { "├── " };
        write!(f, "{}{}", prefix, connector)?;
        
        // Track name
        write!(f, "{}", track.name)?;
        
        // Track type (only if not already in the name)
        if let Some(track_type) = track.track_type() {
            let type_suffix = format!(" ({})", track_type);
            if !track.name.ends_with(&type_suffix) {
                write!(f, " ({})", track_type)?;
            }
        }
        
        // Show modes if requested
        let modes = track.get_modes();
        if show_modes && !modes.is_empty() {
            let mode_strs: Vec<&str> = modes.iter().map(|m| m.as_str()).collect();
            write!(f, " [{}]", mode_strs.join(", "))?;
        }
        
        writeln!(f)?;
        
        // Calculate prefix for children
        let child_prefix = if is_last {
            format!("{}    ", prefix)
        } else {
            format!("{}│   ", prefix)
        };
        
        // Get children of this track - match by name and prefer matching track type for disambiguation
        let children: Vec<&Track> = self.tracks
            .iter()
            .filter(|t| {
                if let Some(parent_name) = t.parent_name() {
                    if parent_name == track.name {
                        // Additional check: if parent has a type, prefer children that match the parent's type context
                        // For now, just match by name - the visited set will prevent infinite loops
                        return true;
                    }
                }
                false
            })
            .collect();
        
        // Format children recursively
        for (i, child) in children.iter().enumerate() {
            let is_last_child = i == children.len() - 1;
            self.fmt_tree_recursive(f, child, &child_prefix, is_last_child, show_modes, visited)?;
        }
        
        Ok(())
    }
    
    /// Get root tracks (tracks without parents)
    fn get_roots(&self) -> Vec<&Track> {
        self.tracks
            .iter()
            .filter(|track| track.parent_name().is_none())
            .collect()
    }
}

impl fmt::Display for Template {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let roots = self.get_roots();
        
        if roots.is_empty() {
            writeln!(f, "(empty template)")?;
            return Ok(());
        }
        
        // Show template name
        writeln!(f, "Template: {}", self.name)?;
        writeln!(f)?;
        
        // Track visited tracks to prevent infinite recursion
        let mut visited = std::collections::HashSet::new();
        
        // Format each root track and its children
        for (i, root) in roots.iter().enumerate() {
            let is_last = i == roots.len() - 1;
            self.fmt_tree_recursive(f, root, "", is_last, false, &mut visited)?;
        }
        
        Ok(())
    }
}

impl Template {
    /// Display the template with a specific display mode
    pub fn display_with_mode(&self, mode: TemplateDisplayMode) -> String {
        match mode {
            TemplateDisplayMode::Full => format!("{}", self),
            TemplateDisplayMode::WithModes => self.display_with_modes(),
            TemplateDisplayMode::Minimal => self.display_minimal(),
        }
    }
    
    /// Display the template showing mode information
    fn display_with_modes(&self) -> String {
        let mut output = String::new();
        output.push_str(&format!("Template: {} (with modes)\n\n", self.name));
        
        let roots = self.get_roots();
        let mut visited = std::collections::HashSet::new();
        for (i, root) in roots.iter().enumerate() {
            let is_last = i == roots.len() - 1;
            self.fmt_tree_recursive_internal(&mut output, root, "", is_last, true, &mut visited);
        }
        
        output
    }
    
    /// Display the template in minimal format (names only)
    fn display_minimal(&self) -> String {
        let mut output = String::new();
        output.push_str(&format!("Template: {}\n\n", self.name));
        
        let roots = self.get_roots();
        let mut visited = std::collections::HashSet::new();
        for (i, root) in roots.iter().enumerate() {
            let is_last = i == roots.len() - 1;
            self.fmt_tree_recursive_minimal(&mut output, root, "", is_last, &mut visited);
        }
        
        output
    }
    
    /// Internal helper for formatting with modes
    fn fmt_tree_recursive_internal(
        &self,
        output: &mut String,
        track: &Track,
        prefix: &str,
        is_last: bool,
        show_modes: bool,
        visited: &mut std::collections::HashSet<usize>,
    ) {
        // Find the index of this track to track if we've visited it
        let track_idx = self.tracks.iter().position(|t| 
            std::ptr::eq(t, track) || 
            (t.name == track.name && t.track_type() == track.track_type() && t.parent_name() == track.parent_name())
        );
        
        if let Some(idx) = track_idx {
            if visited.contains(&idx) {
                // Already visited this track, skip to avoid infinite recursion
                return;
            }
            visited.insert(idx);
        }
        let connector = if is_last { "└── " } else { "├── " };
        output.push_str(&format!("{}{}", prefix, connector));
        output.push_str(&track.name);
        
        // Track type (only if not already in the name)
        if let Some(track_type) = track.track_type() {
            let type_suffix = format!(" ({})", track_type);
            if !track.name.ends_with(&type_suffix) {
                output.push_str(&format!(" ({})", track_type));
            }
        }
        
        let modes = track.get_modes();
        if show_modes && !modes.is_empty() {
            let mode_strs: Vec<&str> = modes.iter().map(|m| m.as_str()).collect();
            output.push_str(&format!(" [{}]", mode_strs.join(", ")));
        }
        
        output.push('\n');
        
        let child_prefix = if is_last {
            format!("{}    ", prefix)
        } else {
            format!("{}│   ", prefix)
        };
        
        let children: Vec<&Track> = self.tracks
            .iter()
            .filter(|t| t.parent_name().map(|p| p == track.name).unwrap_or(false))
            .collect();
        
        for (i, child) in children.iter().enumerate() {
            let is_last_child = i == children.len() - 1;
            self.fmt_tree_recursive_internal(output, child, &child_prefix, is_last_child, show_modes, visited);
        }
    }
    
    /// Internal helper for minimal formatting
    fn fmt_tree_recursive_minimal(
        &self,
        output: &mut String,
        track: &Track,
        prefix: &str,
        is_last: bool,
        visited: &mut std::collections::HashSet<usize>,
    ) {
        // Find the index of this track to track if we've visited it
        let track_idx = self.tracks.iter().position(|t| 
            std::ptr::eq(t, track) || 
            (t.name == track.name && t.track_type() == track.track_type() && t.parent_name() == track.parent_name())
        );
        
        if let Some(idx) = track_idx {
            if visited.contains(&idx) {
                // Already visited this track, skip to avoid infinite recursion
                return;
            }
            visited.insert(idx);
        }
        let connector = if is_last { "└── " } else { "├── " };
        output.push_str(&format!("{}{}{}\n", prefix, connector, track.name));
        
        let child_prefix = if is_last {
            format!("{}    ", prefix)
        } else {
            format!("{}│   ", prefix)
        };
        
        let children: Vec<&Track> = self.tracks
            .iter()
            .filter(|t| t.parent_name().map(|p| p == track.name).unwrap_or(false))
            .collect();
        
        for (i, child) in children.iter().enumerate() {
            let is_last_child = i == children.len() - 1;
            self.fmt_tree_recursive_minimal(output, child, &child_prefix, is_last_child, visited);
        }
    }
}
