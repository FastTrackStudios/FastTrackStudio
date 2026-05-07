//! SeaORM-backed [`PeopleService`] implementation.
//!
//! The `people` table holds locally-cached person records. List and
//! context operations originally pulled from CardDAV, so when no
//! provider is configured we serve people from the repo and skip the
//! cross-entity context joins (those need task/project/event repos and
//! per-person link tables that don't exist yet).
//!
//! Conflict detection is pure: it compares two snapshots of the same
//! provider-backed entity and reports the changed fields.

use std::sync::Arc;

use super::helpers::{convert_model, provider_not_configured};
use crate::people::{
    ContactMethod, ContactMethodList, OrganizationContext, OrganizationRecord, Person,
    PersonApiList, PersonContext, PersonRepo, ProviderConflict, ProviderConflictField, ProviderRef,
    ProviderRefList,
};
use crate::provider::NextcloudSync;
use crate::service::{CardDavContact, PeopleService, VaultError};

/// Typed requirements for [`PeopleServiceImpl`].
///
/// `provider = None` skips the CardDAV path; `addressbook=Some(..)` ops
/// then return `provider_not_configured` instead of hitting the network.
pub struct PeopleServiceDeps<R> {
    pub people_repo: R,
    pub provider: Option<Arc<NextcloudSync>>,
}

#[derive(Clone)]
pub struct PeopleServiceImpl<R> {
    people_repo: R,
    provider: Option<Arc<NextcloudSync>>,
}

impl<R> PeopleServiceImpl<R> {
    pub fn new(deps: PeopleServiceDeps<R>) -> Self {
        Self {
            people_repo: deps.people_repo,
            provider: deps.provider,
        }
    }
}

impl<R> PeopleServiceImpl<R>
where
    R: PersonRepo,
{
    async fn list_person_models(&self) -> Result<Vec<Person>, VaultError> {
        self.people_repo
            .list_persons(None, None, None, Some(10_000))
            .await
            .map_err(VaultError::ParseError)?
            .into_iter()
            .map(convert_model::<PersonApiList, Person>)
            .collect()
    }

    /// Resolve `addressbook` against discovery, then sync the collection and
    /// convert the returned vCards into [`Person`] records.
    async fn fetch_provider_people(
        &self,
        provider: &NextcloudSync,
        addressbook: &str,
    ) -> Result<Vec<Person>, VaultError> {
        let discovery = provider.discover_addressbooks().await?;
        let resolved = discovery
            .addressbooks
            .iter()
            .find(|book| {
                book.name.eq_ignore_ascii_case(addressbook)
                    || book
                        .display_name
                        .as_deref()
                        .map(|d| d.eq_ignore_ascii_case(addressbook))
                        .unwrap_or(false)
                    || book.href == addressbook
            })
            .map(|book| {
                if book.href.is_empty() {
                    book.name.clone()
                } else {
                    book.href.clone()
                }
            })
            .unwrap_or_else(|| addressbook.to_string());

        let collection = provider
            .sync_addressbook_collection(&resolved, None)
            .await?;
        Ok(collection
            .objects
            .into_iter()
            .filter(|object| !object.deleted)
            .filter_map(|object| {
                let href = object.href.clone();
                let etag = object.etag.clone();
                object
                    .contact
                    .map(|contact| person_from_card_contact(contact, &resolved, &href, etag))
            })
            .collect())
    }
}

impl<R> PeopleService for PeopleServiceImpl<R>
where
    R: PersonRepo,
{
    async fn list_people(&self, addressbook: Option<String>) -> Result<Vec<Person>, VaultError> {
        if let Some(book) = addressbook {
            let provider = self
                .provider
                .as_ref()
                .ok_or_else(|| provider_not_configured("list_people (addressbook scoped)"))?;
            return self.fetch_provider_people(provider, &book).await;
        }
        self.list_person_models().await
    }

    async fn list_organizations(
        &self,
        addressbook: Option<String>,
    ) -> Result<Vec<OrganizationRecord>, VaultError> {
        if let Some(book) = addressbook {
            let provider = self.provider.as_ref().ok_or_else(|| {
                provider_not_configured("list_organizations (addressbook scoped)")
            })?;
            let people = self.fetch_provider_people(provider, &book).await?;
            return Ok(group_people_by_organization(people));
        }
        // Group locally-cached people by their `organization` field.
        let mut groups = std::collections::BTreeMap::<String, OrganizationRecord>::new();
        for person in self.list_person_models().await? {
            let Some(org_name) = person
                .organization
                .as_ref()
                .map(|name| name.trim().to_string())
                .filter(|name| !name.is_empty())
            else {
                continue;
            };
            let entry = groups
                .entry(org_name.clone())
                .or_insert_with(|| OrganizationRecord {
                    name: org_name,
                    ..Default::default()
                });
            entry.people.push(person.display_name.clone());
        }
        Ok(groups.into_values().collect())
    }

    async fn person_context(
        &self,
        reference: String,
        addressbook: Option<String>,
    ) -> Result<Option<PersonContext>, VaultError> {
        let people = if let Some(book) = addressbook {
            let provider = self
                .provider
                .as_ref()
                .ok_or_else(|| provider_not_configured("person_context (addressbook scoped)"))?;
            self.fetch_provider_people(provider, &book).await?
        } else {
            self.list_person_models().await?
        };
        let person = people
            .into_iter()
            .find(|person| person_matches(person, &reference));
        Ok(person.map(|person| PersonContext {
            person,
            ..Default::default()
        }))
    }

    async fn organization_context(
        &self,
        reference: String,
        addressbook: Option<String>,
    ) -> Result<Option<OrganizationContext>, VaultError> {
        let people = if let Some(book) = addressbook {
            let provider = self.provider.as_ref().ok_or_else(|| {
                provider_not_configured("organization_context (addressbook scoped)")
            })?;
            self.fetch_provider_people(provider, &book).await?
        } else {
            self.list_person_models().await?
        };
        let members: Vec<Person> = people
            .into_iter()
            .filter(|person| {
                person
                    .organization
                    .as_deref()
                    .map(|name| name.eq_ignore_ascii_case(reference.trim()))
                    .unwrap_or(false)
            })
            .collect();
        if members.is_empty() {
            return Ok(None);
        }
        let organization = OrganizationRecord {
            name: reference,
            people: members
                .iter()
                .map(|person| person.display_name.clone())
                .collect::<Vec<_>>()
                .into(),
            ..Default::default()
        };
        Ok(Some(OrganizationContext {
            organization,
            people: members,
            ..Default::default()
        }))
    }

    async fn detect_person_conflict(
        &self,
        local: Person,
        remote: Person,
    ) -> Result<Option<ProviderConflict>, VaultError> {
        Ok(person_provider_conflicts(&local, &remote))
    }

    async fn detect_organization_conflict(
        &self,
        local: OrganizationRecord,
        remote: OrganizationRecord,
    ) -> Result<Option<ProviderConflict>, VaultError> {
        Ok(organization_provider_conflicts(&local, &remote))
    }
}

/// Convert a CardDAV vCard contact into a local [`Person`] record.
fn person_from_card_contact(
    contact: CardDavContact,
    collection: &str,
    href: &str,
    etag: Option<String>,
) -> Person {
    let display_name = contact.full_name.clone().unwrap_or_else(|| {
        match (
            contact.given_name.as_deref(),
            contact.family_name.as_deref(),
        ) {
            (Some(given), Some(family)) => format!("{given} {family}"),
            (Some(given), None) => given.to_string(),
            (None, Some(family)) => family.to_string(),
            (None, None) => contact.uid.clone().unwrap_or_else(|| "Unknown".to_string()),
        }
    });

    let mut methods: Vec<ContactMethod> = Vec::new();
    for (idx, email) in contact.emails.iter().enumerate() {
        methods.push(ContactMethod {
            kind: "email".into(),
            value: email.clone(),
            label: None,
            primary: idx == 0,
        });
    }
    for (idx, phone) in contact.phones.iter().enumerate() {
        methods.push(ContactMethod {
            kind: "phone".into(),
            value: phone.clone(),
            label: None,
            primary: contact.emails.is_empty() && idx == 0,
        });
    }
    for url in &contact.urls {
        methods.push(ContactMethod {
            kind: "url".into(),
            value: url.clone(),
            label: None,
            primary: false,
        });
    }

    let provider_ref = ProviderRef {
        provider: "nextcloud".to_string(),
        account: None,
        collection: Some(collection.to_string()),
        href: Some(href.to_string()),
        etag,
        uid: contact.uid.clone(),
    };

    Person {
        uuid: uuid::Uuid::new_v4(),
        id: contact.uid,
        display_name,
        given_name: contact.given_name,
        family_name: contact.family_name,
        organization: contact.organization,
        title: contact.title,
        contact_methods: ContactMethodList::from(methods),
        provider_refs: ProviderRefList::from(vec![provider_ref]),
        notes: contact.note,
        follow_up_on: None,
        last_contacted_at: None,
    }
}

/// Group a list of people by their `organization` field.
fn group_people_by_organization(people: Vec<Person>) -> Vec<OrganizationRecord> {
    let mut groups = std::collections::BTreeMap::<String, OrganizationRecord>::new();
    for person in people {
        let Some(org_name) = person
            .organization
            .as_ref()
            .map(|name| name.trim().to_string())
            .filter(|name| !name.is_empty())
        else {
            continue;
        };
        let entry = groups
            .entry(org_name.clone())
            .or_insert_with(|| OrganizationRecord {
                name: org_name,
                ..Default::default()
            });
        entry.people.push(person.display_name.clone());
    }
    groups.into_values().collect()
}

fn person_matches(person: &Person, reference: &str) -> bool {
    let needle = reference.trim();
    person.display_name.eq_ignore_ascii_case(needle)
        || person.id.as_deref() == Some(needle)
        || person.uuid.to_string().eq_ignore_ascii_case(needle)
}

fn person_provider_conflicts(local: &Person, remote: &Person) -> Option<ProviderConflict> {
    let mut fields = Vec::new();
    push_string_field(
        &mut fields,
        "display_name",
        Some(&local.display_name),
        Some(&remote.display_name),
    );
    push_opt_field(
        &mut fields,
        "given_name",
        local.given_name.as_deref(),
        remote.given_name.as_deref(),
    );
    push_opt_field(
        &mut fields,
        "family_name",
        local.family_name.as_deref(),
        remote.family_name.as_deref(),
    );
    push_opt_field(
        &mut fields,
        "organization",
        local.organization.as_deref(),
        remote.organization.as_deref(),
    );
    push_opt_field(
        &mut fields,
        "title",
        local.title.as_deref(),
        remote.title.as_deref(),
    );
    push_opt_field(
        &mut fields,
        "notes",
        local.notes.as_deref(),
        remote.notes.as_deref(),
    );
    push_contact_methods_field(
        &mut fields,
        &local.contact_methods.0,
        &remote.contact_methods.0,
    );

    if fields.is_empty() {
        return None;
    }

    let provider_ref = local
        .provider_refs
        .first()
        .or_else(|| remote.provider_refs.first());
    Some(build_conflict(
        "person",
        local.uuid.to_string(),
        provider_ref,
        fields,
    ))
}

fn organization_provider_conflicts(
    local: &OrganizationRecord,
    remote: &OrganizationRecord,
) -> Option<ProviderConflict> {
    let mut fields = Vec::new();
    push_string_field(&mut fields, "name", Some(&local.name), Some(&remote.name));
    push_opt_field(
        &mut fields,
        "notes",
        local.notes.as_deref(),
        remote.notes.as_deref(),
    );
    push_contact_methods_field(
        &mut fields,
        &local.contact_methods.0,
        &remote.contact_methods.0,
    );

    if fields.is_empty() {
        return None;
    }

    let provider_ref = local
        .provider_refs
        .first()
        .or_else(|| remote.provider_refs.first());
    Some(build_conflict(
        "organization",
        local.id.clone().unwrap_or_else(|| local.name.clone()),
        provider_ref,
        fields,
    ))
}

fn build_conflict(
    entity_type: &str,
    entity_id: String,
    provider_ref: Option<&ProviderRef>,
    fields: Vec<ProviderConflictField>,
) -> ProviderConflict {
    ProviderConflict {
        entity_type: entity_type.to_string(),
        entity_id,
        provider: provider_ref.map(|r| r.provider.clone()).unwrap_or_default(),
        account: provider_ref.and_then(|r| r.account.clone()),
        collection: provider_ref.and_then(|r| r.collection.clone()),
        href: provider_ref.and_then(|r| r.href.clone()),
        uid: provider_ref.and_then(|r| r.uid.clone()),
        local_etag: provider_ref.and_then(|r| r.etag.clone()),
        remote_etag: None,
        fields,
    }
}

fn push_string_field(
    out: &mut Vec<ProviderConflictField>,
    field: &str,
    local: Option<&str>,
    remote: Option<&str>,
) {
    if local != remote {
        out.push(ProviderConflictField {
            field: field.into(),
            local_value: local.map(str::to_string),
            remote_value: remote.map(str::to_string),
        });
    }
}

fn push_opt_field(
    out: &mut Vec<ProviderConflictField>,
    field: &str,
    local: Option<&str>,
    remote: Option<&str>,
) {
    if local != remote {
        out.push(ProviderConflictField {
            field: field.into(),
            local_value: local.map(str::to_string),
            remote_value: remote.map(str::to_string),
        });
    }
}

fn push_contact_methods_field(
    out: &mut Vec<ProviderConflictField>,
    local: &[ContactMethod],
    remote: &[ContactMethod],
) {
    let local_serialized = serialize_contact_methods(local);
    let remote_serialized = serialize_contact_methods(remote);
    if local_serialized != remote_serialized {
        out.push(ProviderConflictField {
            field: "contact_methods".into(),
            local_value: Some(local_serialized),
            remote_value: Some(remote_serialized),
        });
    }
}

fn serialize_contact_methods(methods: &[ContactMethod]) -> String {
    let mut sorted: Vec<String> = methods
        .iter()
        .map(|m| format!("{}:{}", m.kind, m.value))
        .collect();
    sorted.sort();
    sorted.join(",")
}
