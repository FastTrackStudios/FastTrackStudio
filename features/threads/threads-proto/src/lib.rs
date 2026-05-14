pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

pub mod anchor;
pub use anchor::{Anchor, Rect, resolve_text_quote};

/// Kinds a Comment can take. Stored as a `String` field to allow tolerant
/// decode of values added in later versions; UI maps unknown kinds to a
/// neutral rendering.
pub const THREAD_KINDS: &[&str] = &["discussion", "action", "question", "decision", "praise"];

/// Statuses an action-kind thread can take.
///
/// **Logseq compatibility**: when exporting threads to a Logseq-flavored
/// markdown vault, these values map to Logseq's inline status markers as:
/// `open` → `TODO`, `in-progress` → `DOING`, `done` → `DONE`,
/// `wont-do` → `CANCELED`.
pub const ACTION_STATUSES: &[&str] = &["open", "in-progress", "done", "wont-do"];

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "comments", repo)]
pub struct Comment {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub entity_id: Uuid,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::EntityType"))]
    pub entity_type: String,

    #[architect(filterable, sortable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "fake::faker::name::en::Name()"))]
    pub author: String,

    #[architect(fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::CommentBody"))]
    pub body: String,

    #[cfg_attr(feature = "fake", dummy(faker = "0i64..600000"))]
    pub time_start_ms: Option<i64>,

    #[cfg_attr(feature = "fake", dummy(faker = "0i64..600000"))]
    pub time_end_ms: Option<i64>,

    #[architect(filterable)]
    pub reply_to: Option<Uuid>,

    #[architect(filterable)]
    pub resolved: bool,

    #[cfg_attr(feature = "fake", dummy(faker = "fake::faker::name::en::Name()"))]
    pub resolved_by: Option<String>,

    #[architect(json)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "(fake::faker::internet::en::Username(), 0..4)")
    )]
    pub mentions: Vec<String>,

    #[architect(json)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ThreadsTags"))]
    pub tags: Vec<String>,

    /// Thread kind — see [`THREAD_KINDS`]. Defaults to "discussion".
    #[architect(filterable, exclude(create), on_create = String::from("discussion"))]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ThreadKind"))]
    pub kind: String,

    /// When kind == "action". See [`ACTION_STATUSES`].
    #[architect(filterable, exclude(create), on_create = None)]
    #[cfg_attr(feature = "fake", dummy(expr = "None"))]
    pub action_status: Option<String>,

    #[architect(exclude(create), on_create = None)]
    #[cfg_attr(feature = "fake", dummy(expr = "None"))]
    pub action_assignee: Option<String>,

    #[architect(exclude(create), on_create = None)]
    #[cfg_attr(feature = "fake", dummy(expr = "None"))]
    pub action_priority: Option<String>,

    #[architect(exclude(create), on_create = None)]
    #[cfg_attr(feature = "fake", dummy(expr = "None"))]
    pub action_due_date: Option<DateTime<Utc>>,

    /// Set when this thread spawned a real project Task; the new task's id.
    #[architect(exclude(create), on_create = None)]
    #[cfg_attr(feature = "fake", dummy(expr = "None"))]
    pub spawned_task_id: Option<Uuid>,

    /// Set on body edits — distinct from `updated_at`, which CRDT touches
    /// on every write.
    #[architect(exclude(create), on_create = None)]
    #[cfg_attr(feature = "fake", dummy(expr = "None"))]
    pub edited_at: Option<DateTime<Utc>>,

    /// Soft-delete marker. Body becomes empty, marker remains so anchored
    /// locations don't collapse and reply trees stay readable.
    #[architect(filterable, exclude(create), on_create = false)]
    #[cfg_attr(feature = "fake", dummy(expr = "false"))]
    pub deleted: bool,

    #[architect(exclude(create), on_create = None)]
    #[cfg_attr(feature = "fake", dummy(expr = "None"))]
    pub deleted_by: Option<String>,

    /// Serialized [`Anchor`] JSON. `None` means whole-entity (see legacy
    /// `entity_id` / `entity_type` columns).
    #[cfg_attr(feature = "fake", dummy(expr = "None"))]
    pub anchor_json: Option<String>,

    #[architect(exclude(create, update), on_create = Utc::now(), sortable)]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "reactions", repo)]
pub struct Reaction {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub entity_id: Uuid,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::EntityType"))]
    pub entity_type: String,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::Emoji"))]
    pub emoji: String,

    #[architect(filterable)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::internet::en::Username()")
    )]
    pub user: String,

    #[architect(exclude(create, update), on_create = Utc::now(), sortable)]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "attachments", repo)]
pub struct Attachment {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub owner_id: Uuid,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::EntityType"))]
    pub owner_type: String,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::AttachmentSource"))]
    pub source: String,

    #[architect(fulltext, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::AttachmentPath"))]
    pub path: String,

    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Sentence(1..3)")
    )]
    pub label: Option<String>,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::MimeType"))]
    pub mime: Option<String>,

    #[cfg_attr(feature = "fake", dummy(faker = "1000i64..10_000_000"))]
    pub size_bytes: Option<i64>,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::Checksum"))]
    pub checksum: Option<String>,

    #[architect(filterable)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::internet::en::Username()")
    )]
    pub uploader: Option<String>,

    #[architect(json)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ThreadsTags"))]
    pub tags: Vec<String>,

    /// High-level kind: "audio" | "video" | "image" | "file". Distinct from
    /// the legacy `source` (where the bytes came from) and `mime`.
    #[architect(filterable, exclude(create), on_create = String::from("file"))]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::AttachmentKind"))]
    pub kind: String,

    #[architect(exclude(create), on_create = None)]
    #[cfg_attr(feature = "fake", dummy(expr = "None"))]
    pub duration_ms: Option<i64>,

    #[architect(exclude(create), on_create = None)]
    #[cfg_attr(feature = "fake", dummy(expr = "None"))]
    pub width: Option<i32>,

    #[architect(exclude(create), on_create = None)]
    #[cfg_attr(feature = "fake", dummy(expr = "None"))]
    pub height: Option<i32>,

    /// HTTP-fetchable URL for the blob bytes (server-relative or absolute).
    #[architect(exclude(create), on_create = None)]
    #[cfg_attr(feature = "fake", dummy(expr = "None"))]
    pub blob_url: Option<String>,

    /// Loro map key when bytes are stored inline in the doc. Mutually
    /// exclusive with `blob_url` in practice; rarely set (large media
    /// belongs out-of-band).
    #[architect(exclude(create), on_create = None)]
    #[cfg_attr(feature = "fake", dummy(expr = "None"))]
    pub blob_loro_key: Option<String>,

    /// Pre-computed waveform peaks for the audio scrubber (JSON-serialized).
    #[architect(exclude(create), on_create = None)]
    #[cfg_attr(feature = "fake", dummy(expr = "None"))]
    pub waveform_json: Option<String>,

    /// ASR transcript — reserved for future Whisper integration.
    #[architect(exclude(create), on_create = None)]
    #[cfg_attr(feature = "fake", dummy(expr = "None"))]
    pub transcript: Option<String>,

    /// User-visible title; distinct from the existing `label` (free-form
    /// description) — `title` is the display name in lists.
    #[architect(exclude(create), on_create = None)]
    #[cfg_attr(feature = "fake", dummy(expr = "None"))]
    pub title: Option<String>,

    #[architect(exclude(create, update), on_create = Utc::now(), sortable)]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum ThreadsServiceError {
    #[error("not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait ThreadsService {
    async fn resolve_thread(&self, comment_id: Uuid) -> Result<(), ThreadsServiceError>;
}

#[cfg(feature = "fake")]
pub mod fakers {
    use fake::Dummy;
    use fake::rand::{Rng, seq::IndexedRandom};

    fn pick<R: Rng + ?Sized>(rng: &mut R, values: &[&str]) -> String {
        (*values.choose(rng).unwrap()).to_string()
    }

    pub struct EntityType;
    impl Dummy<EntityType> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &EntityType, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "task",
                    "project",
                    "invoice",
                    "comment",
                    "inventory",
                    "recipe",
                    "event",
                    "message",
                    "agent_run",
                ],
            )
        }
    }

    pub struct CommentBody;
    impl Dummy<CommentBody> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &CommentBody, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "Nice work on this!",
                    "Can we revisit the edge case?",
                    "Resolved — see commit.",
                    "Question: should this also handle null?",
                    "LGTM, ship it.",
                    "I'll take a look this afternoon.",
                    "Mentioning for visibility.",
                    "Found a regression — see test.",
                    "+1 to the approach.",
                    "Filed a follow-up ticket.",
                ],
            )
        }
    }

    pub struct Emoji;
    impl Dummy<Emoji> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &Emoji, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "+1", "-1", "tada", "heart", "rocket", "eyes", "fire", "wave", "thinking",
                    "check",
                ],
            )
        }
    }

    pub struct AttachmentSource;
    impl Dummy<AttachmentSource> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &AttachmentSource, rng: &mut R) -> Self {
            pick(
                rng,
                &["upload", "dropbox", "gdrive", "s3", "local", "url-import"],
            )
        }
    }

    pub struct AttachmentPath;
    impl Dummy<AttachmentPath> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &AttachmentPath, rng: &mut R) -> Self {
            const NAMES: &[&str] = &[
                "report.pdf",
                "screenshot.png",
                "stems.zip",
                "mockup.fig",
                "contract.docx",
                "spreadsheet.xlsx",
                "demo.mp4",
                "reference.wav",
            ];
            let folder: u32 = rng.random_range(1..1000);
            format!("attachments/{}/{}", folder, NAMES.choose(rng).unwrap())
        }
    }

    pub struct MimeType;
    impl Dummy<MimeType> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &MimeType, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "image/png",
                    "image/jpeg",
                    "application/pdf",
                    "audio/wav",
                    "audio/mpeg",
                    "video/mp4",
                    "application/zip",
                    "text/plain",
                    "application/json",
                    "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
                ],
            )
        }
    }

    pub struct Checksum;
    impl Dummy<Checksum> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &Checksum, rng: &mut R) -> Self {
            // 64-hex SHA-256 shape
            let a: u128 = rng.random_range(0..u128::MAX);
            let b: u128 = rng.random_range(0..u128::MAX);
            format!("{:032x}{:032x}", a, b)
        }
    }

    pub struct ThreadKind;
    impl Dummy<ThreadKind> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &ThreadKind, rng: &mut R) -> Self {
            // Bias heavily toward "discussion" — the common case in real usage.
            pick(
                rng,
                &[
                    "discussion",
                    "discussion",
                    "discussion",
                    "discussion",
                    "action",
                    "action",
                    "question",
                    "decision",
                    "praise",
                ],
            )
        }
    }

    pub struct AttachmentKind;
    impl Dummy<AttachmentKind> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &AttachmentKind, rng: &mut R) -> Self {
            pick(rng, &["file", "image", "audio", "video"])
        }
    }

    pub struct ThreadsTags;
    impl Dummy<ThreadsTags> for Vec<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &ThreadsTags, rng: &mut R) -> Self {
            const POOL: &[&str] = &[
                "review",
                "feedback",
                "question",
                "blocker",
                "nit",
                "follow-up",
                "design",
                "engineering",
            ];
            let n = rng.random_range(1..=3usize);
            POOL.choose_multiple(rng, n)
                .map(|s| s.to_string())
                .collect()
        }
    }
}
