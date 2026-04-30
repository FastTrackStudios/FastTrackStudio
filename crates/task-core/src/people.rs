use chrono::{DateTime, NaiveDate, Utc};
use facet::Facet;

use crate::{CalendarEvent, Project, Task};

#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct ProviderRef {
    pub provider: String,
    pub account: Option<String>,
    pub collection: Option<String>,
    pub href: Option<String>,
    pub etag: Option<String>,
    pub uid: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct ContactMethod {
    pub kind: String,
    pub value: String,
    pub label: Option<String>,
    pub primary: bool,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Person {
    pub id: Option<String>,
    pub display_name: String,
    pub given_name: Option<String>,
    pub family_name: Option<String>,
    pub organization: Option<String>,
    pub title: Option<String>,
    #[facet(default)]
    pub contact_methods: Vec<ContactMethod>,
    #[facet(default)]
    pub provider_refs: Vec<ProviderRef>,
    pub notes: Option<String>,
    pub follow_up_on: Option<NaiveDate>,
    pub last_contacted_at: Option<DateTime<Utc>>,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct OrganizationRecord {
    pub id: Option<String>,
    pub name: String,
    #[facet(default)]
    pub people: Vec<String>,
    #[facet(default)]
    pub contact_methods: Vec<ContactMethod>,
    #[facet(default)]
    pub provider_refs: Vec<ProviderRef>,
    pub notes: Option<String>,
    pub follow_up_on: Option<NaiveDate>,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Relationship {
    pub from: String,
    pub to: String,
    pub kind: String,
    pub since: Option<NaiveDate>,
    pub notes: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct CommunicationRef {
    pub kind: String,
    pub external_id: String,
    pub summary: Option<String>,
    pub occurred_at: Option<DateTime<Utc>>,
    pub provider: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Facet)]
pub struct ProviderConflictField {
    pub field: String,
    pub local_value: Option<String>,
    pub remote_value: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Facet)]
pub struct ProviderConflict {
    pub entity_type: String,
    pub entity_id: String,
    pub provider: String,
    pub account: Option<String>,
    pub collection: Option<String>,
    pub href: Option<String>,
    pub uid: Option<String>,
    pub local_etag: Option<String>,
    pub remote_etag: Option<String>,
    #[facet(default)]
    pub fields: Vec<ProviderConflictField>,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct PersonContext {
    pub person: Person,
    #[facet(default)]
    pub tasks: Vec<Task>,
    #[facet(default)]
    pub projects: Vec<Project>,
    #[facet(default)]
    pub calendar_events: Vec<CalendarEvent>,
    #[facet(default)]
    pub communications: Vec<CommunicationRef>,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct OrganizationContext {
    pub organization: OrganizationRecord,
    #[facet(default)]
    pub people: Vec<Person>,
    #[facet(default)]
    pub tasks: Vec<Task>,
    #[facet(default)]
    pub projects: Vec<Project>,
    #[facet(default)]
    pub calendar_events: Vec<CalendarEvent>,
    #[facet(default)]
    pub communications: Vec<CommunicationRef>,
}
