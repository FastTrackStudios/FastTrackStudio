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
//
// A `Message` belongs to *either* a `Channel` (human chat) *or* an
// `AgentConversation` (AI chat), never both. The XOR is enforced at
// the service layer — proto only documents it. `channel_id` and
// `agent_conversation_id` are both `Option<Uuid>`; exactly one is
// expected to be `Some`.

pub const MESSAGE_ROLES: &[&str] = &["user", "assistant", "system", "tool"];
pub const FINISH_REASONS: &[&str] = &["stop", "length", "tool_call", "content_filter", "error"];

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "messages", repo)]
pub struct Message {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    /// Owning channel (human chat). XOR with `agent_conversation_id`.
    /// Service layer enforces "exactly one of the two is Some"; this
    /// crate just stores whatever is written.
    #[architect(filterable, sortable)]
    pub channel_id: Option<Uuid>,

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

    // ── AI chat extensions ───────────────────────────────────────────
    //
    // Phase A: demo-only fields. The service layer will populate these
    // when a Message hangs off an `AgentConversation`. Human chat rows
    // leave them all `None` / default.
    /// One of `MESSAGE_ROLES`. Unknown values are stored verbatim.
    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::MaybeRole"))]
    pub role: Option<String>,

    /// Model id (e.g. "claude-opus-4-7"). Filterable so the UI can
    /// scope a view by model.
    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::MaybeModelName"))]
    pub model: Option<String>,

    /// Extended-thinking output for reasoning models (Claude
    /// `thinking`, OpenAI `reasoning`). Stored as plain text.
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::MaybeReasoning"))]
    pub reasoning: Option<String>,

    /// Tool calls + tool results, JSON-encoded. v1 keeps this opaque
    /// to avoid a hard schema commitment; the UI parses as needed.
    pub tool_calls_json: Option<String>,

    /// One of `FINISH_REASONS`. `None` while streaming.
    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::MaybeFinishReason"))]
    pub finish_reason: Option<String>,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::MaybeTokens"))]
    pub tokens_input: Option<u32>,
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::MaybeTokens"))]
    pub tokens_output: Option<u32>,
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::MaybeCost"))]
    pub cost_cents: Option<u32>,

    /// `true` while the assistant is still streaming. Flips to `false`
    /// on `Done`. Demo seeds set this to `false`.
    #[architect(filterable)]
    pub streaming: bool,

    /// Owning AI conversation. XOR with `channel_id`. See doc on
    /// `channel_id` above.
    #[architect(filterable)]
    pub agent_conversation_id: Option<Uuid>,

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

    // ── AI-chat fakers ────────────────────────────────────────────
    //
    // These produce realistically-shaped Option<...> values for the
    // new Message AI fields. Each rolls a coin; `None` is the more
    // common case so seeded data still looks like a normal channel
    // conversation by default.

    pub struct AssistantBody;
    impl Dummy<AssistantBody> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &AssistantBody, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "Sure — here's a quick rundown of the trade-offs.",
                    "Looking at the code, I think we should pull this into its own module.",
                    "I'll start by enumerating the failure modes, then propose a fix for each.",
                    "Good question. The short answer is yes, but with caveats.",
                    "Let me think about that. The key constraint is the CRDT merge order.",
                    "Done — applied the patch and ran the test suite.",
                    "I see the bug. The off-by-one is in the pagination cursor.",
                ],
            )
        }
    }

    pub struct ReasoningBlurb;
    impl Dummy<ReasoningBlurb> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &ReasoningBlurb, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "The user is asking about CRDT convergence. I should focus on the merge semantics first, then the API surface.",
                    "Two interpretations here. I'll go with the more specific one since it matches the surrounding context.",
                    "Need to double-check that the fakers are gated behind the `fake` feature — otherwise we leak `fake` into wasm builds.",
                    "This is a refactor question. The right answer probably depends on whether the caller already holds a lock.",
                ],
            )
        }
    }

    pub struct ModelName;
    impl Dummy<ModelName> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &ModelName, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "claude-opus-4-7",
                    "claude-sonnet-4-6",
                    "claude-haiku-4-5",
                    "gpt-4o",
                    "gpt-4o-mini",
                    "gemma3:4b",
                    "qwen2.5-coder:32b",
                    "mock",
                ],
            )
        }
    }

    pub struct MaybeRole;
    impl Dummy<MaybeRole> for Option<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &MaybeRole, rng: &mut R) -> Self {
            if rng.random_bool(0.3) {
                Some(pick(rng, super::MESSAGE_ROLES))
            } else {
                None
            }
        }
    }

    pub struct MaybeModelName;
    impl Dummy<MaybeModelName> for Option<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &MaybeModelName, rng: &mut R) -> Self {
            if rng.random_bool(0.3) {
                Some(String::dummy_with_rng(&ModelName, rng))
            } else {
                None
            }
        }
    }

    pub struct MaybeReasoning;
    impl Dummy<MaybeReasoning> for Option<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &MaybeReasoning, rng: &mut R) -> Self {
            if rng.random_bool(0.15) {
                Some(String::dummy_with_rng(&ReasoningBlurb, rng))
            } else {
                None
            }
        }
    }

    pub struct MaybeFinishReason;
    impl Dummy<MaybeFinishReason> for Option<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &MaybeFinishReason, rng: &mut R) -> Self {
            if rng.random_bool(0.3) {
                Some(pick(rng, super::FINISH_REASONS))
            } else {
                None
            }
        }
    }

    pub struct MaybeTokens;
    impl Dummy<MaybeTokens> for Option<u32> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &MaybeTokens, rng: &mut R) -> Self {
            if rng.random_bool(0.3) {
                Some(rng.random_range(20..8000u32))
            } else {
                None
            }
        }
    }

    pub struct MaybeCost;
    impl Dummy<MaybeCost> for Option<u32> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &MaybeCost, rng: &mut R) -> Self {
            if rng.random_bool(0.3) {
                Some(rng.random_range(1..200u32))
            } else {
                None
            }
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
