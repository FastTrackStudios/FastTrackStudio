//! Installation plan — validated configuration for the install process.

use std::path::PathBuf;

/// Pinned REAPER version for this release of the installer.
pub const REAPER_VERSION: &str = "7.30";

/// Default library download URL — points to the latest GitHub Release asset.
/// Update this when cutting a new library release.
pub const DEFAULT_LIBRARY_URL: &str =
    "https://github.com/FastTrackStudios/fts-library/releases/latest/download/fts-library.tar.gz";

#[derive(Clone, PartialEq)]
pub struct InstallPlan {
    /// Root installation directory (e.g. `~/Music/FastTrackStudio/`).
    pub install_root: PathBuf,
    /// Path to `FTS-Control.app` (found alongside the installer on the DMG).
    pub fts_control_source: Option<PathBuf>,
    /// URL to download the library archive (presets, FXChains, etc.).
    pub library_url: String,
}

impl InstallPlan {
    /// Create a default plan for this machine.
    pub fn default_for_machine() -> Self {
        Self {
            install_root: default_install_root(),
            fts_control_source: find_fts_control_app(),
            library_url: DEFAULT_LIBRARY_URL.to_string(),
        }
    }

    /// Validate the plan before starting installation.
    pub fn validate(&self) -> Result<(), Vec<String>> {
        let mut errors = Vec::new();

        // Check parent directory is writable
        if let Some(parent) = self.install_root.parent() {
            if parent.exists() && std::fs::metadata(parent).map(|m| m.permissions().readonly()).unwrap_or(true) {
                errors.push(format!("Directory {} is not writable", parent.display()));
            }
        }

        if errors.is_empty() { Ok(()) } else { Err(errors) }
    }

    /// URL for downloading REAPER for this platform/arch.
    pub fn reaper_download_url(&self) -> String {
        let version_slug = REAPER_VERSION.replace('.', "");
        let arch = if cfg!(target_arch = "aarch64") { "arm64" } else { "x86_64" };

        if cfg!(target_os = "macos") {
            format!("https://www.reaper.fm/files/7.x/reaper{version_slug}_macos_{arch}.dmg")
        } else {
            format!("https://www.reaper.fm/files/7.x/reaper{version_slug}_linux_{arch}.tar.xz")
        }
    }

    pub fn reaper_dir(&self) -> PathBuf {
        self.install_root.join("Reaper")
    }

    pub fn library_dir(&self) -> PathBuf {
        self.install_root.join("Library")
    }
}

fn default_install_root() -> PathBuf {
    dirs_home()
        .map(|h| h.join("Music/FastTrackStudio"))
        .unwrap_or_else(|| PathBuf::from("/tmp/FastTrackStudio"))
}

/// Look for FTS-Control.app adjacent to the running installer binary.
fn find_fts_control_app() -> Option<PathBuf> {
    let exe = std::env::current_exe().ok()?;
    // Inside FTS Installer.app/Contents/MacOS/fts-installer
    let app_bundle = exe.parent()?.parent()?.parent()?;
    let sibling = app_bundle.parent()?.join("FTS Control.app");
    sibling.exists().then_some(sibling)
}

fn dirs_home() -> Option<PathBuf> {
    std::env::var_os("HOME").map(PathBuf::from)
}
