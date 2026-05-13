//! `location-proto` — wire contract for physical places.
//!
//! Studios, venues, offices, client sites. Cross-cutting: assets and
//! people live somewhere; events happen somewhere.

pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "locations", repo)]
pub struct Location {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::company::en::CompanyName()")
    )]
    pub name: String,

    /// Free-form category: `"studio"`, `"venue"`, `"office"`,
    /// `"client-site"`, etc.
    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::LocationKind"))]
    pub kind: Option<String>,

    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::address::en::StreetName()")
    )]
    pub address1: Option<String>,
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::address::en::SecondaryAddress()")
    )]
    pub address2: Option<String>,
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::address::en::CityName()")
    )]
    pub city: Option<String>,
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::address::en::StateName()")
    )]
    pub state: Option<String>,
    #[cfg_attr(feature = "fake", dummy(faker = "fake::faker::address::en::ZipCode()"))]
    pub postal_code: Option<String>,

    /// ISO 3166-1 alpha-2.
    #[architect(filterable)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::address::en::CountryCode()")
    )]
    pub country_code: Option<String>,

    #[cfg_attr(feature = "fake", dummy(faker = "fake::faker::name::en::Name()"))]
    pub contact_name: Option<String>,
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::internet::en::FreeEmail()")
    )]
    pub contact_email: Option<String>,

    /// Self-FK for sub-locations (Studio A → Live Room).
    #[architect(filterable)]
    pub parent_id: Option<Uuid>,

    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Sentence(3..8)")
    )]
    pub notes: Option<String>,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::LocationTags"))]
    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum LocationServiceError {
    #[error("not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait LocationService {
    /// Re-parent a location under a different parent.
    async fn reparent(
        &self,
        location_id: Uuid,
        new_parent_id: Option<Uuid>,
    ) -> Result<(), LocationServiceError>;
}

#[cfg(feature = "fake")]
pub mod fakers {
    use fake::Dummy;
    use fake::rand::{Rng, seq::IndexedRandom};

    fn pick<R: Rng + ?Sized>(rng: &mut R, values: &[&str]) -> String {
        (*values.choose(rng).unwrap()).to_string()
    }

    pub struct LocationKind;
    impl Dummy<LocationKind> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &LocationKind, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "studio",
                    "venue",
                    "office",
                    "client-site",
                    "warehouse",
                    "home",
                    "rehearsal-space",
                    "co-working",
                ],
            )
        }
    }

    pub struct LocationTags;
    impl Dummy<LocationTags> for Vec<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &LocationTags, rng: &mut R) -> Self {
            const POOL: &[&str] = &[
                "primary",
                "remote",
                "indoor",
                "outdoor",
                "soundproofed",
                "rental",
                "owned",
                "shared",
            ];
            let n = rng.random_range(1..=3usize);
            POOL.choose_multiple(rng, n)
                .map(|s| s.to_string())
                .collect()
        }
    }
}
