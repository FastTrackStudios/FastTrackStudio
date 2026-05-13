pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "agent_runs", repo)]
pub struct AgentRun {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::AgentRunName"))]
    pub name: String,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::AgentKind"))]
    pub kind: String,

    #[architect(fulltext)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Paragraph(1..3)")
    )]
    pub prompt: String,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::AgentStatus"))]
    pub status: String,

    #[architect(filterable)]
    pub task_id: Option<Uuid>,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
    pub started_at: Option<DateTime<Utc>>,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
    pub completed_at: Option<DateTime<Utc>>,

    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Paragraph(1..3)")
    )]
    pub result: Option<String>,

    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Sentence(3..10)")
    )]
    pub error_message: Option<String>,

    #[cfg_attr(feature = "fake", dummy(faker = "500u32..200_000"))]
    pub tokens_used: Option<u32>,

    #[cfg_attr(feature = "fake", dummy(faker = "1u32..500"))]
    pub cost_cents: Option<u32>,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::AgentTags"))]
    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum AgentServiceError {
    #[error("not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait AgentService {
    async fn cancel(&self, run_id: Uuid) -> Result<(), AgentServiceError>;
}

#[cfg(feature = "fake")]
pub mod fakers {
    use chrono::{DateTime, Duration, Utc};
    use fake::Dummy;
    use fake::rand::{Rng, seq::IndexedRandom};

    fn pick<R: Rng + ?Sized>(rng: &mut R, values: &[&str]) -> String {
        (*values.choose(rng).unwrap()).to_string()
    }

    pub struct AgentRunName;
    impl Dummy<AgentRunName> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &AgentRunName, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "Summarize meeting notes",
                    "Triage inbox",
                    "Draft release notes",
                    "Generate test cases",
                    "Refactor module",
                    "Translate documentation",
                    "Classify support tickets",
                    "Plan project milestones",
                    "Write commit message",
                    "Code review pass",
                ],
            )
        }
    }

    pub struct AgentKind;
    impl Dummy<AgentKind> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &AgentKind, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "chat",
                    "completion",
                    "tool-use",
                    "embedding",
                    "summarization",
                    "classification",
                    "code-gen",
                ],
            )
        }
    }

    pub struct AgentStatus;
    impl Dummy<AgentStatus> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &AgentStatus, rng: &mut R) -> Self {
            pick(
                rng,
                &["queued", "running", "completed", "failed", "cancelled"],
            )
        }
    }

    pub struct RecentDateTime;
    impl Dummy<RecentDateTime> for DateTime<Utc> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &RecentDateTime, rng: &mut R) -> Self {
            Utc::now() - Duration::days(rng.random_range(0..90))
        }
    }

    pub struct AgentTags;
    impl Dummy<AgentTags> for Vec<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &AgentTags, rng: &mut R) -> Self {
            const POOL: &[&str] = &[
                "gpt-4",
                "gpt-5",
                "claude",
                "experimental",
                "production",
                "background",
                "user-initiated",
                "scheduled",
            ];
            let n = rng.random_range(1..=3usize);
            POOL.choose_multiple(rng, n)
                .map(|s| s.to_string())
                .collect()
        }
    }
}
