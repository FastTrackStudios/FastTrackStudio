//! Configuration for local SQLite storage.

use std::path::{Path, PathBuf};

/// Configuration for the local SQLite database.
#[derive(Debug, Clone)]
pub struct LocalConfig {
    /// Path to the SQLite database file.
    pub database_path: PathBuf,
    /// Enable WAL mode for better concurrent read performance.
    pub wal_mode: bool,
    /// Busy timeout in milliseconds.
    pub busy_timeout_ms: u64,
}

impl LocalConfig {
    /// Create config with an explicit database path.
    pub fn new(path: impl Into<PathBuf>) -> Self {
        Self {
            database_path: path.into(),
            wal_mode: true,
            busy_timeout_ms: 5000,
        }
    }

    /// Create config using the FastTrackStudio music directory.
    ///
    /// Resolves to: `~/Music/FastTrackStudio/signal.db`
    ///
    /// This is the primary storage location so presets persist across
    /// projects and are easy to find alongside other music production data.
    pub fn default_path() -> Result<Self, crate::error::StorageError> {
        let home = dirs::home_dir().ok_or_else(|| {
            crate::error::StorageError::Config("could not determine home directory".to_string())
        })?;
        let db_path = home.join("Music").join("FastTrackStudio").join("signal.db");
        Ok(Self::new(db_path))
    }

    /// Create config using the platform-appropriate data directory (legacy).
    ///
    /// Resolves to:
    /// - Linux: `~/.local/share/signal/signal.db`
    /// - macOS: `~/Library/Application Support/signal/signal.db`
    /// - Windows: `C:\Users\<user>\AppData\Roaming\signal\signal.db`
    ///
    /// Kept for backward compatibility and testing. New code should use
    /// [`default_path`](Self::default_path) instead.
    pub fn platform_data_path() -> Result<Self, crate::error::StorageError> {
        let data_dir = dirs::data_dir().ok_or_else(|| {
            crate::error::StorageError::Config(
                "could not determine platform data directory".to_string(),
            )
        })?;
        let db_path = data_dir.join("signal").join("signal.db");
        Ok(Self::new(db_path))
    }

    /// Build the SQLite connection URL.
    pub fn connection_url(&self) -> String {
        format!("sqlite://{}?mode=rwc", self.database_path.display())
    }

    /// Get the parent directory of the database file.
    pub fn database_dir(&self) -> &Path {
        self.database_path.parent().unwrap_or(Path::new("."))
    }
}

impl Default for LocalConfig {
    fn default() -> Self {
        Self::new("signal.db")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn connection_url_includes_rwc_mode() {
        let config = LocalConfig::new("/tmp/test.db");
        let url = config.connection_url();
        assert!(url.contains("sqlite://"));
        assert!(url.contains("/tmp/test.db"));
        assert!(url.contains("mode=rwc"));
    }

    #[test]
    fn default_path_resolves_to_fast_track_studio() {
        let config = LocalConfig::default_path();
        assert!(config.is_ok());
        let config = config.unwrap();
        let path_str = config.database_path.to_string_lossy();
        assert!(
            path_str.contains("FastTrackStudio"),
            "expected FastTrackStudio in path, got: {path_str}"
        );
        assert!(
            path_str.ends_with("signal.db"),
            "expected signal.db suffix, got: {path_str}"
        );
    }

    #[test]
    fn platform_data_path_resolves() {
        // Legacy path should still work
        let config = LocalConfig::platform_data_path();
        assert!(config.is_ok());
        let config = config.unwrap();
        assert!(config.database_path.to_string_lossy().contains("signal"));
    }

    #[test]
    fn wal_mode_enabled_by_default() {
        let config = LocalConfig::new("/tmp/test.db");
        assert!(config.wal_mode);
    }

    #[test]
    fn database_dir_returns_parent() {
        let config = LocalConfig::new("/foo/bar/signal.db");
        assert_eq!(config.database_dir(), Path::new("/foo/bar"));
    }
}
