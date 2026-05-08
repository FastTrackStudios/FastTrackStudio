//! REAPER UI Service Implementation

use daw_proto::{UiService, UserInputResult};
use reaper_high::Reaper;
use std::path::PathBuf;

use crate::main_thread;

/// REAPER UI service implementation
#[derive(Clone)]
pub struct ReaperUi;

impl ReaperUi {
    pub fn new() -> Self {
        Self
    }
}

impl Default for ReaperUi {
    fn default() -> Self {
        Self::new()
    }
}

impl UiService for ReaperUi {
    async fn get_user_inputs(
        &self,
        title: String,
        prompts: Vec<String>,
        defaults: Vec<String>,
    ) -> Option<UserInputResult> {
        main_thread::query(move || {
            let medium = Reaper::get().medium_reaper();

            // REAPER's GetUserInputs takes up to 16 fields
            if prompts.len() > 16 || prompts.len() != defaults.len() {
                return None;
            }

            let num_inputs = prompts.len() as u32;

            // Build the caption string (CSV format: "field1,field2,field3")
            let captions = prompts.join(",");

            // Build initial values string (also CSV)
            let initial_csv = defaults.join(",");

            // Call GetUserInputs
            let result = medium.get_user_inputs(
                title,
                num_inputs,
                captions,
                initial_csv,
                512, // max buffer size per field
            );

            match result {
                Some(values_str) => {
                    // Parse the returned CSV
                    let values: Vec<String> = values_str
                        .to_str()
                        .split(',')
                        .map(|s| s.to_string())
                        .collect();

                    Some(UserInputResult { ok: true, values })
                }
                None => {
                    // User cancelled
                    Some(UserInputResult {
                        ok: false,
                        values: vec![],
                    })
                }
            }
        })
        .await
        .flatten()
    }

    async fn browse_for_file(
        &self,
        title: String,
        initial_dir: Option<PathBuf>,
        filter: Option<String>,
    ) -> Option<PathBuf> {
        main_thread::query(move || {
            if !reaper_low::Swell::is_available_globally() {
                return None;
            }
            let swell = reaper_low::Swell::get();
            crate::safe_wrappers::ui::browse_for_open_file(
                swell,
                &title,
                initial_dir.as_deref(),
                filter.as_deref(),
            )
        })
        .await
        .flatten()
    }

    async fn browse_for_save_file(
        &self,
        title: String,
        initial_dir: Option<PathBuf>,
        default_name: String,
        filter: Option<String>,
    ) -> Option<PathBuf> {
        main_thread::query(move || {
            if !reaper_low::Swell::is_available_globally() {
                return None;
            }
            let swell = reaper_low::Swell::get();
            crate::safe_wrappers::ui::browse_for_save_file(
                swell,
                &title,
                initial_dir.as_deref(),
                &default_name,
                filter.as_deref(),
            )
        })
        .await
        .flatten()
    }

    async fn browse_for_directory(
        &self,
        title: String,
        initial_dir: Option<PathBuf>,
    ) -> Option<PathBuf> {
        main_thread::query(move || {
            if !reaper_low::Swell::is_available_globally() {
                return None;
            }
            let swell = reaper_low::Swell::get();
            crate::safe_wrappers::ui::browse_for_directory(swell, &title, initial_dir.as_deref())
        })
        .await
        .flatten()
    }

    async fn set_prevent_ui_refresh(&self, prevent: bool) {
        main_thread::run(move || {
            let low = Reaper::get().medium_reaper().low();
            low.PreventUIRefresh(if prevent { 1 } else { 0 });
        });
    }
}
