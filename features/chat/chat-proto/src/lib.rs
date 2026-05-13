//! `chat-proto` — wire contract for the chat feature.

pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

// ── Channel ───────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "channels", repo)]
pub struct Channel {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ChannelName"))]
    pub name: String,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ChannelKind"))]
    pub kind: String,

    #[architect(fulltext)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Sentence(3..10)")
    )]
    pub topic: Option<String>,

    #[architect(filterable)]
    pub project_id: Option<Uuid>,

    #[architect(filterable)]
    pub archived: bool,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ChatTags"))]
    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── Message ───────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "messages", repo)]
pub struct Message {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable)]
    pub channel_id: Uuid,

    #[architect(filterable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "fake::faker::name::en::Name()"))]
    pub author: String,

    #[architect(fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::MessageBody"))]
    pub body: String,

    #[architect(filterable)]
    pub reply_to: Option<Uuid>,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
    pub edited_at: Option<DateTime<Utc>>,

    #[architect(filterable)]
    pub deleted: bool,

    #[cfg_attr(
        feature = "fake",
        dummy(faker = "(fake::faker::internet::en::Username(), 0..4)")
    )]
    pub mentions: Vec<String>,

    #[cfg_attr(
        feature = "fake",
        dummy(faker = "(fake::faker::filesystem::en::FileName(), 0..3)")
    )]
    pub attachment_ids: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── ChannelMember ─────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "channel_members", repo)]
pub struct ChannelMember {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub channel_id: Uuid,

    #[architect(filterable)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::internet::en::Username()")
    )]
    pub user: String,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::MemberRole"))]
    pub role: String,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
    pub joined_at: DateTime<Utc>,

    pub last_read_message_id: Option<Uuid>,

    #[architect(filterable)]
    pub muted: bool,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── ChatService ───────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum ChatServiceError {
    #[error("not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait ChatService {
    async fn mark_read(&self, member_id: Uuid, message_id: Uuid) -> Result<(), ChatServiceError>;
}

#[cfg(feature = "fake")]
pub mod fakers {
    use chrono::{DateTime, Duration, Utc};
    use fake::Dummy;
    use fake::rand::{Rng, seq::IndexedRandom};

    fn pick<R: Rng + ?Sized>(rng: &mut R, values: &[&str]) -> String {
        (*values.choose(rng).unwrap()).to_string()
    }

    pub struct ChannelName;
    impl Dummy<ChannelName> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &ChannelName, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "general",
                    "engineering",
                    "design",
                    "random",
                    "announcements",
                    "support-tickets",
                    "ops",
                    "product",
                    "marketing",
                    "watercooler",
                    "incidents",
                    "releases",
                ],
            )
        }
    }

    pub struct ChannelKind;
    impl Dummy<ChannelKind> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &ChannelKind, rng: &mut R) -> Self {
            pick(rng, &["public", "private", "dm", "group-dm"])
        }
    }

    pub struct MessageBody;
    impl Dummy<MessageBody> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &MessageBody, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "Sounds good, let's ship it.",
                    "Can you take a look when you get a chance?",
                    "PR is ready for review.",
                    "Heads up — deploy at 3pm.",
                    "Thanks!",
                    "+1",
                    "I'll grab lunch and be back at 1.",
                    "Anyone seen the staging env logs?",
                    "Pushed a fix, mind re-running CI?",
                    "Let's pair on this tomorrow.",
                    "Standup in 5",
                    "Just merged. Watch for regressions.",
                ],
            )
        }
    }

    pub struct MemberRole;
    impl Dummy<MemberRole> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &MemberRole, rng: &mut R) -> Self {
            pick(rng, &["owner", "admin", "member", "guest"])
        }
    }

    pub struct RecentDateTime;
    impl Dummy<RecentDateTime> for DateTime<Utc> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &RecentDateTime, rng: &mut R) -> Self {
            Utc::now() - Duration::minutes(rng.random_range(0..43_200))
        }
    }

    pub struct ChatTags;
    impl Dummy<ChatTags> for Vec<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &ChatTags, rng: &mut R) -> Self {
            const POOL: &[&str] = &["team", "social", "support", "announce", "alerts", "archive"];
            let n = rng.random_range(1..=3usize);
            POOL.choose_multiple(rng, n)
                .map(|s| s.to_string())
                .collect()
        }
    }
}
