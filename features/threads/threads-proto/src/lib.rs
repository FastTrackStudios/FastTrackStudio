pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

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

    #[cfg_attr(
        feature = "fake",
        dummy(faker = "(fake::faker::internet::en::Username(), 0..4)")
    )]
    pub mentions: Vec<String>,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ThreadsTags"))]
    pub tags: Vec<String>,

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

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ThreadsTags"))]
    pub tags: Vec<String>,

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
