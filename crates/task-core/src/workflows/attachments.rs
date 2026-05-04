//! File attachments — universal, Nextcloud WebDAV compatible.
//!
//! Any entity can have file attachments. Files live in the project folder
//! structure and are referenced by relative path. Nextcloud WebDAV serves
//! them for preview/download.
//!
//! ## Storage
//! Attachments are listed in the entity's frontmatter:
//! ```yaml
//! attachments:
//!   - path: "bounces/Sunrise Mix v2.wav"
//!     type: audio
//!     size_bytes: 52428800
//!     uploaded_by: cody
//!   - path: "artwork/cover-draft.png"
//!     type: image
//!     size_bytes: 2048000
//! ```

use chrono::NaiveDateTime;
use facet::Facet;

/// A file attached to an entity.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Attachment {
    /// Relative path within the project folder.
    pub path: String,
    /// File type hint for preview rendering.
    pub file_type: FileType,
    /// File size in bytes.
    pub size_bytes: Option<u64>,
    /// Who uploaded/added this file.
    pub uploaded_by: Option<String>,
    /// When the file was added.
    pub uploaded_at: Option<NaiveDateTime>,
    /// Optional version number (for versioned deliverables).
    pub version: Option<u32>,
    /// Duration in seconds (for audio/video).
    pub duration_seconds: Option<u32>,
    /// Sample rate (for audio).
    pub sample_rate: Option<u32>,
    /// Resolution (for images/video, e.g. "1920x1080").
    pub resolution: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
#[repr(u8)]
pub enum FileType {
    #[default]
    Unknown,
    Audio,
    Video,
    Image,
    Document,
    Archive,
    Project, // DAW session, Premiere project, etc.
}

impl FileType {
    /// Detect file type from extension.
    pub fn from_extension(ext: &str) -> Self {
        match ext.to_lowercase().as_str() {
            "wav" | "mp3" | "flac" | "aiff" | "aif" | "ogg" | "m4a" | "opus" => Self::Audio,
            "mp4" | "mov" | "avi" | "mkv" | "webm" | "m4v" | "mxf" => Self::Video,
            "jpg" | "jpeg" | "png" | "gif" | "webp" | "svg" | "tiff" | "bmp" => Self::Image,
            "pdf" | "doc" | "docx" | "txt" | "md" | "rtf" | "csv" | "xlsx" => Self::Document,
            "zip" | "tar" | "gz" | "rar" | "7z" => Self::Archive,
            "rpp" | "als" | "logic" | "ptx" | "cpr" | "prproj" | "aep" | "drp" => Self::Project,
            _ => Self::Unknown,
        }
    }

    pub fn label(&self) -> &'static str {
        match self {
            Self::Audio => "Audio",
            Self::Video => "Video",
            Self::Image => "Image",
            Self::Document => "Document",
            Self::Archive => "Archive",
            Self::Project => "Project File",
            Self::Unknown => "File",
        }
    }

    /// Whether this type supports inline preview in the browser.
    pub fn previewable(&self) -> bool {
        matches!(self, Self::Audio | Self::Video | Self::Image)
    }
}

/// Format file size for display.
pub fn format_size(bytes: u64) -> String {
    if bytes < 1024 {
        format!("{bytes} B")
    } else if bytes < 1024 * 1024 {
        format!("{:.1} KB", bytes as f64 / 1024.0)
    } else if bytes < 1024 * 1024 * 1024 {
        format!("{:.1} MB", bytes as f64 / (1024.0 * 1024.0))
    } else {
        format!("{:.2} GB", bytes as f64 / (1024.0 * 1024.0 * 1024.0))
    }
}
