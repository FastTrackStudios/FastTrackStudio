//! Tab Navigation
//!
//! Handles navigation between REAPER project tabs, including:
//! - Finding current tab index
//! - Switching to next/previous tab
//! - Switching to specific tab by index

use reaper_high::Project;
use reaper_high::Reaper;
use reaper_medium::{CommandId, ProjectContext, ProjectRef};
use tracing::{debug, info, trace, warn};

/// Handles navigation between project tabs
#[derive(Debug)]
pub struct TabNavigator {
    /// Cached tab count (updated periodically)
    tab_count: usize,
}

impl TabNavigator {
    pub fn new() -> Self {
        Self { tab_count: 0 }
    }

    /// Check if a project tab should be excluded (e.g., FTS-ROUTING, FTS_Routing, etc.)
    /// Handles case-insensitive matching and both hyphens and underscores
    fn is_excluded_tab(file_path: Option<&std::path::Path>) -> bool {
        if let Some(path) = file_path {
            // Get filename without extension
            if let Some(file_name) = path.file_stem().and_then(|s| s.to_str()) {
                // Normalize the filename: convert to uppercase and replace underscores with hyphens
                let normalized = file_name.to_uppercase().replace('_', "-");
                if normalized == "FTS-ROUTING" {
                    return true;
                }
            }
        }
        false
    }

    /// Find next non-excluded tab (skipping FTS-ROUTING)
    fn find_next_valid_tab(start_from: usize, direction: i32) -> Option<usize> {
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();
        let mut tab_count = 0;

        // Count total tabs first
        for i in 0..128u32 {
            if medium_reaper.enum_projects(ProjectRef::Tab(i), 0).is_some() {
                tab_count += 1;
            } else {
                break;
            }
        }

        if tab_count <= 1 {
            return None;
        }

        // Search for next valid tab
        let mut current = start_from;
        let mut checked = 0;

        while checked < tab_count {
            // Move to next/previous tab index (with wrapping)
            if direction > 0 {
                current = (current + 1) % tab_count;
            } else {
                current = if current > 0 {
                    current - 1
                } else {
                    tab_count - 1
                };
            }

            // Check if this tab is valid
            if let Some(result) = medium_reaper.enum_projects(ProjectRef::Tab(current as u32), 0) {
                let file_path = result.file_path.as_ref().map(|p| p.as_std_path());
                if !Self::is_excluded_tab(file_path.as_deref()) {
                    trace!(tab_index = current, "Found next valid tab");
                    return Some(current);
                } else {
                    trace!(tab_index = current, "Skipping excluded tab (FTS-ROUTING)");
                }
            }

            checked += 1;
        }

        None
    }

    /// Get total number of project tabs (excluding FTS-ROUTING)
    pub fn get_tab_count(&mut self) -> usize {
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();
        let mut count = 0;
        let mut excluded_count = 0;

        for i in 0..128u32 {
            if let Some(result) = medium_reaper.enum_projects(ProjectRef::Tab(i), 0) {
                let file_path = result.file_path.as_ref().map(|p| p.as_std_path());
                if Self::is_excluded_tab(file_path.as_deref()) {
                    excluded_count += 1;
                } else {
                    count += 1;
                }
            } else {
                break;
            }
        }

        self.tab_count = count;
        trace!(
            tab_count = count,
            excluded_tabs = excluded_count,
            "Tab count updated"
        );
        count
    }

    /// Get current active tab index
    pub fn get_current_tab_index(&self) -> Option<usize> {
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();

        if let Some(current_result) = medium_reaper.enum_projects(ProjectRef::Current, 0) {
            let current_project = current_result.project;
            let current_name = current_result
                .file_path
                .as_ref()
                .and_then(|p| p.as_std_path().file_stem())
                .and_then(|s| s.to_str())
                .map(|s| s.to_string())
                .unwrap_or_else(|| "unsaved".to_string());

            // Find which tab index matches current project
            for i in 0..128u32 {
                if let Some(result) = medium_reaper.enum_projects(ProjectRef::Tab(i), 0) {
                    if result.project == current_project {
                        trace!(tab_index = i, project_name = %current_name, "Current tab identified");
                        return Some(i as usize);
                    }
                } else {
                    break;
                }
            }
        }

        trace!("No current tab found");
        None
    }

    /// Switch to next tab using REAPER action (skipping FTS-ROUTING)
    pub fn switch_next(&self) -> anyhow::Result<()> {
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();

        let current_tab = self
            .get_current_tab_index()
            .ok_or_else(|| anyhow::anyhow!("No current tab"))?;

        let next_tab = Self::find_next_valid_tab(current_tab, 1)
            .ok_or_else(|| anyhow::anyhow!("No valid next tab found"))?;

        // Get tab info for logging
        let next_tab_info = if let Some(result) =
            medium_reaper.enum_projects(ProjectRef::Tab(next_tab as u32), 0)
        {
            result
                .file_path
                .as_ref()
                .and_then(|p| p.as_std_path().file_stem())
                .and_then(|s| s.to_str())
                .map(|s| s.to_string())
                .unwrap_or_else(|| format!("Tab {}", next_tab))
        } else {
            format!("Tab {}", next_tab)
        };

        // Switch to the target tab
        self.switch_to_tab(next_tab)?;

        info!(
            from_tab = current_tab,
            to_tab = next_tab,
            project_name = %next_tab_info,
            "Switched to next tab"
        );

        Ok(())
    }

    /// Switch to previous tab using REAPER action (skipping FTS-ROUTING)
    pub fn switch_prev(&self) -> anyhow::Result<()> {
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();

        let current_tab = self
            .get_current_tab_index()
            .ok_or_else(|| anyhow::anyhow!("No current tab"))?;

        let prev_tab = Self::find_next_valid_tab(current_tab, -1)
            .ok_or_else(|| anyhow::anyhow!("No valid previous tab found"))?;

        // Get tab info for logging
        let prev_tab_info = if let Some(result) =
            medium_reaper.enum_projects(ProjectRef::Tab(prev_tab as u32), 0)
        {
            result
                .file_path
                .as_ref()
                .and_then(|p| p.as_std_path().file_stem())
                .and_then(|s| s.to_str())
                .map(|s| s.to_string())
                .unwrap_or_else(|| format!("Tab {}", prev_tab))
        } else {
            format!("Tab {}", prev_tab)
        };

        // Switch to the target tab
        self.switch_to_tab(prev_tab)?;

        info!(
            from_tab = current_tab,
            to_tab = prev_tab,
            project_name = %prev_tab_info,
            "Switched to previous tab"
        );

        Ok(())
    }

    /// Switch to a specific tab by index using REAPER actions
    pub fn switch_to_tab(&self, tab_index: usize) -> anyhow::Result<()> {
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();

        // Convert usize to u32 for tab index
        let tab_index_u32 = tab_index
            .try_into()
            .map_err(|_| anyhow::anyhow!("Tab index {} too large", tab_index))?;

        // Verify tab exists and get info
        let tab_result = medium_reaper
            .enum_projects(ProjectRef::Tab(tab_index_u32), 0)
            .ok_or_else(|| anyhow::anyhow!("Tab {} not found", tab_index))?;

        // Check if this is an excluded tab
        let file_path = tab_result.file_path.as_ref().map(|p| p.as_std_path());
        if Self::is_excluded_tab(file_path.as_deref()) {
            warn!(
                tab_index,
                "Attempted to switch to excluded tab (FTS-ROUTING)"
            );
            return Err(anyhow::anyhow!(
                "Cannot switch to excluded tab (FTS-ROUTING)"
            ));
        }

        let tab_name = tab_result
            .file_path
            .as_ref()
            .and_then(|p| p.as_std_path().file_stem())
            .and_then(|s| s.to_str())
            .map(|s| s.to_string())
            .unwrap_or_else(|| format!("Tab {}", tab_index));

        debug!(tab_index, project_name = %tab_name, "Switching to tab");

        // Get current tab index
        let current_idx = self
            .get_current_tab_index()
            .ok_or_else(|| anyhow::anyhow!("No current tab"))?;

        // If already on the target tab, nothing to do
        if current_idx == tab_index {
            trace!(tab_index, project_name = %tab_name, "Already on target tab");
            return Ok(());
        }

        // Calculate how many tabs to move (need to count all tabs, not just valid ones)
        let mut total_tab_count = 0;
        for i in 0..128u32 {
            if medium_reaper.enum_projects(ProjectRef::Tab(i), 0).is_some() {
                total_tab_count += 1;
            } else {
                break;
            }
        }

        if total_tab_count <= 1 {
            return Err(anyhow::anyhow!("Only one tab available"));
        }

        // Calculate distance (forward and backward) using actual tab indices
        let forward_dist = if tab_index > current_idx {
            tab_index - current_idx
        } else {
            (total_tab_count - current_idx) + tab_index
        };

        let backward_dist = if tab_index < current_idx {
            current_idx - tab_index
        } else {
            current_idx + (total_tab_count - tab_index)
        };

        // Use the shorter path
        let (action_id, distance) = if forward_dist <= backward_dist {
            (40861u32, forward_dist) // Action 40861: Next project tab
        } else {
            (40862u32, backward_dist) // Action 40862: Previous project tab
        };

        // Execute the action the required number of times
        for _ in 0..distance {
            let cmd_id = CommandId::new(action_id);
            medium_reaper.main_on_command_ex(cmd_id, 0, ProjectContext::CurrentProject);
        }

        info!(
            from_tab = current_idx,
            to_tab = tab_index,
            project_name = %tab_name,
            distance,
            "Switched to tab"
        );

        Ok(())
    }

    /// Get project for a specific tab index
    pub fn get_project_at(&self, tab_index: usize) -> Option<Project> {
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();

        // Convert usize to u32 for tab index
        let tab_index_u32 = tab_index.try_into().ok()?;
        if let Some(result) = medium_reaper.enum_projects(ProjectRef::Tab(tab_index_u32), 0) {
            Some(Project::new(result.project))
        } else {
            None
        }
    }
}
