pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "meetings", repo)]
pub struct Meeting {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::MeetingTitle"))]
    pub name: String,

    #[architect(filterable)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::internet::en::Username()")
    )]
    pub host_user: Option<String>,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::AroundNowDateTime"))]
    pub scheduled_at: DateTime<Utc>,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
    pub started_at: Option<DateTime<Utc>>,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
    pub ended_at: Option<DateTime<Utc>>,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::MeetingStatus"))]
    pub status: String,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecordingUrl"))]
    pub recording_url: Option<String>,

    #[architect(fulltext)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Paragraph(1..3)")
    )]
    pub notes: Option<String>,

    #[architect(json)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "(fake::faker::name::en::Name(), 2..8)")
    )]
    pub participants: Vec<String>,

    #[architect(json)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::MeetingTags"))]
    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum ConferenceServiceError {
    #[error("not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait ConferenceService {
    async fn end_meeting(&self, meeting_id: Uuid) -> Result<(), ConferenceServiceError>;
}

#[cfg(feature = "fake")]
pub mod fakers {
    use chrono::{DateTime, Duration, Utc};
    use fake::Dummy;
    use fake::rand::{Rng, seq::IndexedRandom};

    fn pick<R: Rng + ?Sized>(rng: &mut R, values: &[&str]) -> String {
        (*values.choose(rng).unwrap()).to_string()
    }

    pub struct MeetingTitle;
    impl Dummy<MeetingTitle> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &MeetingTitle, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "Weekly Standup",
                    "Sprint Planning",
                    "Sprint Retro",
                    "1:1 Sync",
                    "Design Review",
                    "Roadmap Sync",
                    "All Hands",
                    "Client Kickoff",
                    "Architecture Review",
                    "Post-mortem",
                    "Demo Day",
                    "Quarterly Review",
                ],
            )
        }
    }

    pub struct MeetingStatus;
    impl Dummy<MeetingStatus> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &MeetingStatus, rng: &mut R) -> Self {
            pick(rng, &["scheduled", "in-progress", "completed", "cancelled"])
        }
    }

    pub struct RecordingUrl;
    impl Dummy<RecordingUrl> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &RecordingUrl, rng: &mut R) -> Self {
            const HOSTS: &[&str] = &[
                "zoom.us/rec",
                "meet.google.com/recording",
                "drive.google.com/file",
                "storage.example.com/recordings",
            ];
            let host = HOSTS.choose(rng).unwrap();
            let id: u64 = rng.random_range(100_000_000..9_999_999_999);
            format!("https://{}/{}", host, id)
        }
    }

    pub struct AroundNowDateTime;
    impl Dummy<AroundNowDateTime> for DateTime<Utc> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &AroundNowDateTime, rng: &mut R) -> Self {
            let offset_min: i64 = rng.random_range(-86_400..86_400);
            Utc::now() + Duration::minutes(offset_min)
        }
    }

    pub struct RecentDateTime;
    impl Dummy<RecentDateTime> for DateTime<Utc> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &RecentDateTime, rng: &mut R) -> Self {
            Utc::now() - Duration::minutes(rng.random_range(0..43_200))
        }
    }

    pub struct MeetingTags;
    impl Dummy<MeetingTags> for Vec<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &MeetingTags, rng: &mut R) -> Self {
            const POOL: &[&str] = &[
                "standup",
                "planning",
                "review",
                "demo",
                "external",
                "internal",
                "client",
                "recurring",
                "ad-hoc",
            ];
            let n = rng.random_range(1..=3usize);
            POOL.choose_multiple(rng, n)
                .map(|s| s.to_string())
                .collect()
        }
    }
}
