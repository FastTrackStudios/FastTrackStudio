//! Storage-agnostic project provider system.
//!
//! The `ProjectProvider` trait abstracts how projects and tasks are stored,
//! allowing the same task management logic to work across:
//!
//! - Local filesystem (NFS mounts, local directories)
//! - Obsidian vaults (personal task files)
//! - S3-compatible object storage (AWS S3, MinIO, R2)
//! - WebDAV / Nextcloud file shares
//! - Git repositories
//!
//! Each provider reads/writes the same `project.md` + `tasks/*.md` format,
//! just on different storage backends.
//!
//! # Architecture
//!
//! ```text
//! ProjectRegistry
//!   ├── LocalProvider      /mnt/starcommand/Projects/
//!   ├── VaultProvider      ~/Documents/The Observatory/TaskNotes/
//!   ├── S3Provider         s3://fasttrack-projects/
//!   ├── WebDavProvider     https://cloud.example.com/remote.php/dav/
//!   └── GitProvider        github.com/FastTrackStudios/projects
//! ```

mod traits;
mod local;
mod vault;
mod registry;
mod s3;
mod webdav;
mod nextcloud;
pub mod nextcloud_sync;
pub mod github;
pub mod talk;
pub mod mail;
pub mod channel;
#[cfg(feature = "server")]
pub mod mail_watch;

pub use traits::*;
pub use local::LocalProvider;
pub use vault::VaultProvider;
pub use registry::ProjectRegistry;
pub use s3::{S3Provider, S3Config};
pub use webdav::{
    WebDavConfig, WebDavEntry, WebDavLock, WebDavProvider, WebDavPutOptions, WebDavResourceKind,
};
pub mod mock;
pub use mock::MockProvider;
pub use nextcloud::{NextcloudProvider, NextcloudConfig};
pub use github::{GitHubSync, GitHubConfig, GitHubSyncResult};
pub use talk::{Message as TalkMessage, Room as TalkRoom, TalkClient, TalkConfig};
pub use channel::{
    ChannelConversation, ChannelMessage, ChannelSendMessageRequest, CommunicationChannelProvider,
};
pub use mail::{
    MailAccount, MailAttachment, MailClient, MailConfig, MailMessage, MailMessageDetail, MailTag,
    Mailbox,
};
#[cfg(feature = "server")]
pub use mail_watch::{ImapWatchConfig, WatchEvent, watch_idle};

#[cfg(test)]
#[derive(Debug, Clone)]
pub(crate) struct LiveNextcloudCredentials {
    pub url: String,
    pub username: String,
    pub password: String,
    pub projects_path: String,
    pub calendar: String,
    pub event_calendar: Option<String>,
}

#[cfg(test)]
pub(crate) fn live_nextcloud_credentials() -> LiveNextcloudCredentials {
    let file_cfg = load_nextcloud_test_config();
    let url = env_or_toml("NEXTCLOUD_URL", &file_cfg, "url")
        .unwrap_or_else(|| "https://cloud.starcommand.live".to_string());
    let username = env_or_toml("NEXTCLOUD_USER", &file_cfg, "username")
        .or_else(|| env_or_toml("NEXTCLOUD_USERNAME", &file_cfg, "username"))
        .unwrap_or_else(|| "codywright".to_string());
    let password = std::env::var("NEXTCLOUD_PASSWORD")
        .ok()
        .filter(|value| !value.is_empty())
        .or_else(|| read_secret_file_var("NEXTCLOUD_PASSWORD_FILE"))
        .or_else(|| env_or_toml("NEXTCLOUD_PASSWORD", &file_cfg, "password"))
        .or_else(|| {
            toml_string(&file_cfg, "password_file")
                .and_then(|path| std::fs::read_to_string(path).ok())
                .map(|value| value.trim().to_string())
        })
        .filter(|value| !value.is_empty())
        .expect(
            "live Nextcloud tests require NEXTCLOUD_PASSWORD, NEXTCLOUD_PASSWORD_FILE, \
             or [nextcloud].password_file in TASK_NEXTCLOUD_CONFIG / ~/.config/task/nextcloud.toml",
        );
    let projects_path = env_or_toml("NEXTCLOUD_PROJECTS_PATH", &file_cfg, "projects_path")
        .unwrap_or_else(|| "Projects/".to_string());
    let calendar = env_or_toml("NEXTCLOUD_CALENDAR", &file_cfg, "calendar")
        .unwrap_or_else(|| "tasks".to_string())
        .to_ascii_lowercase();
    let event_calendar = env_or_toml("NEXTCLOUD_EVENT_CALENDAR", &file_cfg, "event_calendar")
        .or_else(|| env_or_toml("NEXTCLOUD_EVENTS_CALENDAR", &file_cfg, "events_calendar"))
        .map(|value| value.to_ascii_lowercase());

    LiveNextcloudCredentials {
        url,
        username,
        password,
        projects_path,
        calendar,
        event_calendar,
    }
}

#[cfg(test)]
fn load_nextcloud_test_config() -> Option<toml::Value> {
    let path = std::env::var("TASK_NEXTCLOUD_CONFIG")
        .ok()
        .map(std::path::PathBuf::from)
        .or_else(|| {
            std::env::var("HOME")
                .ok()
                .map(|home| std::path::PathBuf::from(home).join(".config/task/nextcloud.toml"))
        })?;
    std::fs::read_to_string(path)
        .ok()
        .and_then(|content| content.parse::<toml::Value>().ok())
}

#[cfg(test)]
fn env_or_toml(env: &str, cfg: &Option<toml::Value>, key: &str) -> Option<String> {
    std::env::var(env)
        .ok()
        .filter(|value| !value.is_empty())
        .or_else(|| toml_string(cfg, key))
}

#[cfg(test)]
fn toml_string(cfg: &Option<toml::Value>, key: &str) -> Option<String> {
    cfg.as_ref()
        .and_then(|value| value.get("nextcloud"))
        .and_then(|value| value.get(key))
        .and_then(|value| value.as_str())
        .map(str::to_string)
}

#[cfg(test)]
fn read_secret_file_var(env: &str) -> Option<String> {
    let path = std::env::var(env).ok()?;
    std::fs::read_to_string(path)
        .ok()
        .map(|value| value.trim().to_string())
}
