pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "emails", repo)]
pub struct Email {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::MessageId"))]
    pub message_id: String,

    #[architect(filterable, sortable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::EmailSubject"))]
    pub subject: String,

    #[architect(filterable, fulltext)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::internet::en::FreeEmail()")
    )]
    pub from_addr: String,

    #[cfg_attr(
        feature = "fake",
        dummy(faker = "(fake::faker::internet::en::FreeEmail(), 1..4)")
    )]
    pub to_addrs: Vec<String>,

    #[cfg_attr(
        feature = "fake",
        dummy(faker = "(fake::faker::internet::en::FreeEmail(), 0..3)")
    )]
    pub cc_addrs: Vec<String>,

    #[cfg_attr(
        feature = "fake",
        dummy(faker = "(fake::faker::internet::en::FreeEmail(), 0..2)")
    )]
    pub bcc_addrs: Vec<String>,

    #[architect(fulltext)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Paragraph(2..5)")
    )]
    pub body: Option<String>,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
    pub received_at: DateTime<Utc>,

    #[architect(filterable)]
    pub read: bool,

    #[architect(filterable)]
    pub starred: bool,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::Folder"))]
    pub folder: Option<String>,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ThreadId"))]
    pub thread_id: Option<String>,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::EmailTags"))]
    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum EmailServiceError {
    #[error("not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait EmailService {
    async fn mark_read(&self, email_id: Uuid) -> Result<(), EmailServiceError>;
}

#[cfg(feature = "fake")]
pub mod fakers {
    use chrono::{DateTime, Duration, Utc};
    use fake::Dummy;
    use fake::rand::{Rng, seq::IndexedRandom};

    fn pick<R: Rng + ?Sized>(rng: &mut R, values: &[&str]) -> String {
        (*values.choose(rng).unwrap()).to_string()
    }

    pub struct MessageId;
    impl Dummy<MessageId> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &MessageId, rng: &mut R) -> Self {
            let a: u64 = rng.random_range(0..u64::MAX);
            let b: u64 = rng.random_range(0..u64::MAX);
            format!("<{:016x}.{:016x}@mail.example.com>", a, b)
        }
    }

    pub struct EmailSubject;
    impl Dummy<EmailSubject> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &EmailSubject, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "Re: invoice for March",
                    "Action required: password reset",
                    "Your weekly digest",
                    "Welcome aboard",
                    "Meeting notes — sprint planning",
                    "Fwd: Q3 roadmap",
                    "Quick question about the proposal",
                    "Your order has shipped",
                    "Reminder: 1:1 tomorrow at 2pm",
                    "Newsletter — issue #42",
                    "Out of office: returning Monday",
                    "Re: contract for review",
                    "Heads up: deploy window tonight",
                ],
            )
        }
    }

    pub struct Folder;
    impl Dummy<Folder> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &Folder, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "INBOX",
                    "Sent",
                    "Archive",
                    "Trash",
                    "Spam",
                    "Drafts",
                    "Important",
                ],
            )
        }
    }

    pub struct ThreadId;
    impl Dummy<ThreadId> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &ThreadId, rng: &mut R) -> Self {
            let a: u64 = rng.random_range(0..u64::MAX);
            format!("thread-{:016x}", a)
        }
    }

    pub struct RecentDateTime;
    impl Dummy<RecentDateTime> for DateTime<Utc> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &RecentDateTime, rng: &mut R) -> Self {
            Utc::now() - Duration::minutes(rng.random_range(0..43_200))
        }
    }

    pub struct EmailTags;
    impl Dummy<EmailTags> for Vec<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &EmailTags, rng: &mut R) -> Self {
            const POOL: &[&str] = &[
                "important",
                "follow-up",
                "client",
                "internal",
                "newsletter",
                "receipt",
                "support",
                "promotion",
            ];
            let n = rng.random_range(1..=3usize);
            POOL.choose_multiple(rng, n)
                .map(|s| s.to_string())
                .collect()
        }
    }
}
