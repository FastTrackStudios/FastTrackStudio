//! `person-proto` — wire contract for the `person` feature.
//!
//! Three top-level entities:
//!
//! - `Person` — an individual (engineer, producer, client, collaborator)
//! - `Client` — a company / individual / label that contracts work
//! - `Team`   — a grouping of people
//!
//! Each is a separate `architect::Entity` with its own Repo trait.

pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

// ── Person ────────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "people", repo)]
pub struct Person {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "fake::faker::name::en::Name()"))]
    pub name: String,

    #[architect(filterable, fulltext)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::internet::en::FreeEmail()")
    )]
    pub email: Option<String>,

    #[architect(filterable)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::phone_number::en::PhoneNumber()")
    )]
    pub phone: Option<String>,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "fake::faker::job::en::Title()"))]
    pub role: Option<String>,

    #[architect(filterable)]
    pub client_id: Option<Uuid>,

    #[architect(filterable)]
    pub team_id: Option<Uuid>,

    #[architect(fulltext)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Sentence(3..8)")
    )]
    pub notes: Option<String>,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::PersonTags"))]
    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── Client ────────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "clients", repo)]
pub struct Client {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::company::en::CompanyName()")
    )]
    pub name: String,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ClientKind"))]
    pub kind: String,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::Website"))]
    pub website: Option<String>,

    #[architect(filterable)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::internet::en::FreeEmail()")
    )]
    pub contact_email: Option<String>,

    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::address::en::StreetName()")
    )]
    pub address: Option<String>,

    #[architect(filterable)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::address::en::CountryCode()")
    )]
    pub country_code: Option<String>,

    #[cfg_attr(feature = "fake", dummy(faker = "5000u32..30000"))]
    pub default_billable_rate_cents: Option<u32>,

    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Sentence(3..8)")
    )]
    pub notes: Option<String>,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ClientTags"))]
    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── Team ──────────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "teams", repo)]
pub struct Team {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::TeamName"))]
    pub name: String,

    #[architect(fulltext)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Paragraph(1..3)")
    )]
    pub description: Option<String>,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "fake::faker::name::en::Name()"))]
    pub owner: Option<String>,

    #[architect(filterable)]
    pub archived: bool,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::TeamTags"))]
    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── PersonService ─────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum PersonServiceError {
    #[error("not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait PersonService {
    /// Attach a person to a client (set `client_id`).
    async fn attach_to_client(
        &self,
        person_id: Uuid,
        client_id: Uuid,
    ) -> Result<(), PersonServiceError>;
}

#[cfg(feature = "fake")]
pub mod fakers {
    use fake::Dummy;
    use fake::rand::{Rng, seq::IndexedRandom};

    fn pick<R: Rng + ?Sized>(rng: &mut R, values: &[&str]) -> String {
        (*values.choose(rng).unwrap()).to_string()
    }

    pub struct ClientKind;
    impl Dummy<ClientKind> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &ClientKind, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "company",
                    "individual",
                    "label",
                    "agency",
                    "non-profit",
                    "government",
                    "startup",
                ],
            )
        }
    }

    pub struct Website;
    impl Dummy<Website> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &Website, rng: &mut R) -> Self {
            const TLDS: &[&str] = &["com", "io", "co", "net", "org", "studio", "agency"];
            const NAMES: &[&str] = &[
                "northwind",
                "contoso",
                "acme",
                "globex",
                "umbrella",
                "soylent",
                "initech",
                "stark",
                "wayne",
                "wonka",
            ];
            format!(
                "https://www.{}.{}",
                NAMES.choose(rng).unwrap(),
                TLDS.choose(rng).unwrap()
            )
        }
    }

    pub struct TeamName;
    impl Dummy<TeamName> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &TeamName, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "Platform",
                    "Growth",
                    "Design",
                    "Engineering",
                    "Product",
                    "Operations",
                    "Marketing",
                    "Customer Success",
                    "Studio Team",
                    "Mastering",
                    "A&R",
                ],
            )
        }
    }

    pub struct PersonTags;
    impl Dummy<PersonTags> for Vec<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &PersonTags, rng: &mut R) -> Self {
            const POOL: &[&str] = &[
                "internal",
                "external",
                "vip",
                "vendor",
                "lead",
                "collaborator",
                "freelancer",
                "contractor",
            ];
            let n = rng.random_range(1..=3usize);
            POOL.choose_multiple(rng, n)
                .map(|s| s.to_string())
                .collect()
        }
    }

    pub struct ClientTags;
    impl Dummy<ClientTags> for Vec<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &ClientTags, rng: &mut R) -> Self {
            const POOL: &[&str] = &[
                "active",
                "retainer",
                "one-off",
                "enterprise",
                "small-biz",
                "high-touch",
                "self-serve",
                "vip",
            ];
            let n = rng.random_range(1..=3usize);
            POOL.choose_multiple(rng, n)
                .map(|s| s.to_string())
                .collect()
        }
    }

    pub struct TeamTags;
    impl Dummy<TeamTags> for Vec<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &TeamTags, rng: &mut R) -> Self {
            const POOL: &[&str] = &[
                "core",
                "platform",
                "product",
                "engineering",
                "design",
                "remote",
                "on-site",
            ];
            let n = rng.random_range(1..=3usize);
            POOL.choose_multiple(rng, n)
                .map(|s| s.to_string())
                .collect()
        }
    }
}
