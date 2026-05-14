//! UI service trait — dialogs and refresh control.

use super::UserInputResult;
use std::path::PathBuf;
use vox::service;

#[service]
pub trait UiService {
    /// Multi-input dialog. Returns `None` if cancelled.
    async fn get_user_inputs(
        &self,
        title: String,
        prompts: Vec<String>,
        defaults: Vec<String>,
    ) -> Option<UserInputResult>;

    /// File picker for an existing file.
    async fn browse_for_file(
        &self,
        title: String,
        initial_dir: Option<PathBuf>,
        filter: Option<String>,
    ) -> Option<PathBuf>;

    /// File picker for "save as".
    async fn browse_for_save_file(
        &self,
        title: String,
        initial_dir: Option<PathBuf>,
        default_name: String,
        filter: Option<String>,
    ) -> Option<PathBuf>;

    /// Directory picker.
    async fn browse_for_directory(
        &self,
        title: String,
        initial_dir: Option<PathBuf>,
    ) -> Option<PathBuf>;

    /// Temporarily suspend UI refresh for batch operations.
    async fn set_prevent_ui_refresh(&self, prevent: bool);
}
