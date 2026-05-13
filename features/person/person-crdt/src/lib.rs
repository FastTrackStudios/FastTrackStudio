//! Loro-backed source-of-truth for the `person` feature. Three
//! entities, three `EntityCrdt` impls, three `*RepoLoro` newtypes.

use architect::{Page, RepoError, SortOrder};
use chrono::Utc;
use crdt::EntityCrdt;
use crdt::codec::{
    read_bool, read_dt, read_opt_str, read_opt_u32, read_opt_uuid, read_str, read_string_list,
    read_uuid, write_bool, write_dt, write_opt_str, write_opt_string_list, write_opt_u32,
    write_opt_uuid, write_str, write_string_list, write_uuid,
};
use loro::LoroMap;
use person_proto::{
    Client, ClientCreate, ClientList, ClientRepo, ClientUpdate, Person, PersonCreate, PersonList,
    PersonRepo, PersonUpdate, Team, TeamCreate, TeamList, TeamRepo, TeamUpdate,
};
use uuid::Uuid;

pub use crdt::{CrdtDoc, LoroRepo};

// ── Person ────────────────────────────────────────────────────────────

pub struct PersonEntity;

#[derive(Clone)]
pub struct PersonRepoLoro {
    inner: LoroRepo<PersonEntity>,
}

impl PersonRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<PersonEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for PersonEntity {
    type Wire = Person;
    type Create = PersonCreate;
    type Update = PersonUpdate;
    type List = PersonList;

    const ROOT: &'static str = "people";

    fn id(w: &Person) -> Uuid {
        w.id
    }

    fn from_create(input: PersonCreate) -> Person {
        let now = Utc::now();
        Person {
            id: Uuid::new_v4(),
            name: input.name,
            email: input.email,
            phone: input.phone,
            role: input.role,
            client_id: input.client_id,
            team_id: input.team_id,
            notes: input.notes,
            tags: input.tags,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &Person) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_str(m, "name", &e.name)?;
        write_opt_str(m, "email", e.email.as_deref())?;
        write_opt_str(m, "phone", e.phone.as_deref())?;
        write_opt_str(m, "role", e.role.as_deref())?;
        write_opt_uuid(m, "client_id", e.client_id)?;
        write_opt_uuid(m, "team_id", e.team_id)?;
        write_opt_str(m, "notes", e.notes.as_deref())?;
        write_string_list(m, "tags", &e.tags)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Person, RepoError> {
        Ok(Person {
            id: read_uuid(m, "id")?,
            name: read_str(m, "name")?,
            email: read_opt_str(m, "email")?,
            phone: read_opt_str(m, "phone")?,
            role: read_opt_str(m, "role")?,
            client_id: read_opt_uuid(m, "client_id")?,
            team_id: read_opt_uuid(m, "team_id")?,
            notes: read_opt_str(m, "notes")?,
            tags: read_string_list(m, "tags")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: PersonUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.name {
            write_str(m, "name", &v)?;
        }
        if let Some(v) = u.email {
            write_opt_str(m, "email", v.as_deref())?;
        }
        if let Some(v) = u.phone {
            write_opt_str(m, "phone", v.as_deref())?;
        }
        if let Some(v) = u.role {
            write_opt_str(m, "role", v.as_deref())?;
        }
        if let Some(v) = u.client_id {
            write_opt_uuid(m, "client_id", v)?;
        }
        if let Some(v) = u.team_id {
            write_opt_uuid(m, "team_id", v)?;
        }
        if let Some(v) = u.notes {
            write_opt_str(m, "notes", v.as_deref())?;
        }
        if let Some(v) = u.tags {
            write_opt_string_list(m, "tags", Some(&v))?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(items: &mut [Person], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "name" => items.sort_by(|a, b| a.name.cmp(&b.name)),
            "created_at" => items.sort_by(|a, b| a.created_at.cmp(&b.created_at)),
            other => {
                return Err(RepoError::InvalidInput(format!(
                    "unsortable field: {other}"
                )));
            }
        }
        if matches!(order, SortOrder::Desc) {
            items.reverse();
        }
        Ok(())
    }

    fn build_list(items: Vec<Person>, total: u32, page: Page) -> PersonList {
        PersonList { items, total, page }
    }
}

impl PersonRepo for PersonRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Person, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<PersonList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: PersonCreate) -> Result<Person, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: PersonUpdate) -> Result<Person, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── Client ────────────────────────────────────────────────────────────

pub struct ClientEntity;

#[derive(Clone)]
pub struct ClientRepoLoro {
    inner: LoroRepo<ClientEntity>,
}

impl ClientRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<ClientEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for ClientEntity {
    type Wire = Client;
    type Create = ClientCreate;
    type Update = ClientUpdate;
    type List = ClientList;

    const ROOT: &'static str = "clients";

    fn id(w: &Client) -> Uuid {
        w.id
    }

    fn from_create(input: ClientCreate) -> Client {
        let now = Utc::now();
        Client {
            id: Uuid::new_v4(),
            name: input.name,
            kind: input.kind,
            website: input.website,
            contact_email: input.contact_email,
            address: input.address,
            country_code: input.country_code,
            default_billable_rate_cents: input.default_billable_rate_cents,
            notes: input.notes,
            tags: input.tags,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &Client) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_str(m, "name", &e.name)?;
        write_str(m, "kind", &e.kind)?;
        write_opt_str(m, "website", e.website.as_deref())?;
        write_opt_str(m, "contact_email", e.contact_email.as_deref())?;
        write_opt_str(m, "address", e.address.as_deref())?;
        write_opt_str(m, "country_code", e.country_code.as_deref())?;
        write_opt_u32(
            m,
            "default_billable_rate_cents",
            e.default_billable_rate_cents,
        )?;
        write_opt_str(m, "notes", e.notes.as_deref())?;
        write_string_list(m, "tags", &e.tags)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Client, RepoError> {
        Ok(Client {
            id: read_uuid(m, "id")?,
            name: read_str(m, "name")?,
            kind: read_str(m, "kind")?,
            website: read_opt_str(m, "website")?,
            contact_email: read_opt_str(m, "contact_email")?,
            address: read_opt_str(m, "address")?,
            country_code: read_opt_str(m, "country_code")?,
            default_billable_rate_cents: read_opt_u32(m, "default_billable_rate_cents")?,
            notes: read_opt_str(m, "notes")?,
            tags: read_string_list(m, "tags")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: ClientUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.name {
            write_str(m, "name", &v)?;
        }
        if let Some(v) = u.kind {
            write_str(m, "kind", &v)?;
        }
        if let Some(v) = u.website {
            write_opt_str(m, "website", v.as_deref())?;
        }
        if let Some(v) = u.contact_email {
            write_opt_str(m, "contact_email", v.as_deref())?;
        }
        if let Some(v) = u.address {
            write_opt_str(m, "address", v.as_deref())?;
        }
        if let Some(v) = u.country_code {
            write_opt_str(m, "country_code", v.as_deref())?;
        }
        if let Some(v) = u.default_billable_rate_cents {
            write_opt_u32(m, "default_billable_rate_cents", v)?;
        }
        if let Some(v) = u.notes {
            write_opt_str(m, "notes", v.as_deref())?;
        }
        if let Some(v) = u.tags {
            write_opt_string_list(m, "tags", Some(&v))?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(items: &mut [Client], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "name" => items.sort_by(|a, b| a.name.cmp(&b.name)),
            "created_at" => items.sort_by(|a, b| a.created_at.cmp(&b.created_at)),
            other => {
                return Err(RepoError::InvalidInput(format!(
                    "unsortable field: {other}"
                )));
            }
        }
        if matches!(order, SortOrder::Desc) {
            items.reverse();
        }
        Ok(())
    }

    fn build_list(items: Vec<Client>, total: u32, page: Page) -> ClientList {
        ClientList { items, total, page }
    }
}

impl ClientRepo for ClientRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Client, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<ClientList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: ClientCreate) -> Result<Client, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: ClientUpdate) -> Result<Client, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── Team ──────────────────────────────────────────────────────────────

pub struct TeamEntity;

#[derive(Clone)]
pub struct TeamRepoLoro {
    inner: LoroRepo<TeamEntity>,
}

impl TeamRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<TeamEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for TeamEntity {
    type Wire = Team;
    type Create = TeamCreate;
    type Update = TeamUpdate;
    type List = TeamList;

    const ROOT: &'static str = "teams";

    fn id(w: &Team) -> Uuid {
        w.id
    }

    fn from_create(input: TeamCreate) -> Team {
        let now = Utc::now();
        Team {
            id: Uuid::new_v4(),
            name: input.name,
            description: input.description,
            owner: input.owner,
            archived: input.archived,
            tags: input.tags,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &Team) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_str(m, "name", &e.name)?;
        write_opt_str(m, "description", e.description.as_deref())?;
        write_opt_str(m, "owner", e.owner.as_deref())?;
        write_bool(m, "archived", e.archived)?;
        write_string_list(m, "tags", &e.tags)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Team, RepoError> {
        Ok(Team {
            id: read_uuid(m, "id")?,
            name: read_str(m, "name")?,
            description: read_opt_str(m, "description")?,
            owner: read_opt_str(m, "owner")?,
            archived: read_bool(m, "archived")?,
            tags: read_string_list(m, "tags")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: TeamUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.name {
            write_str(m, "name", &v)?;
        }
        if let Some(v) = u.description {
            write_opt_str(m, "description", v.as_deref())?;
        }
        if let Some(v) = u.owner {
            write_opt_str(m, "owner", v.as_deref())?;
        }
        if let Some(v) = u.archived {
            write_bool(m, "archived", v)?;
        }
        if let Some(v) = u.tags {
            write_opt_string_list(m, "tags", Some(&v))?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(items: &mut [Team], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "name" => items.sort_by(|a, b| a.name.cmp(&b.name)),
            "created_at" => items.sort_by(|a, b| a.created_at.cmp(&b.created_at)),
            other => {
                return Err(RepoError::InvalidInput(format!(
                    "unsortable field: {other}"
                )));
            }
        }
        if matches!(order, SortOrder::Desc) {
            items.reverse();
        }
        Ok(())
    }

    fn build_list(items: Vec<Team>, total: u32, page: Page) -> TeamList {
        TeamList { items, total, page }
    }
}

impl TeamRepo for TeamRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Team, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<TeamList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: TeamCreate) -> Result<Team, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: TeamUpdate) -> Result<Team, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}
