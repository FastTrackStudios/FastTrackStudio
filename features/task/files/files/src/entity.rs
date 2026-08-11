//! The Vault mapping for Files' two curated version entities (issue
//! #261): [`NamedVersion`] and [`ProjectVersion`].
//!
//! ADR 0001: "Named Versions and Project Versions are Vault entities,
//! not engine constructs ... the version store knows nothing about
//! names." So they live exactly where every other Task entity lives —
//! a markdown page with YAML frontmatter under the org vault,
//! discovered by a live filesystem scan — and everything generic (the
//! frontmatter split, the lenient YAML readers, the slug rule, CRUD)
//! comes from `vault-entity`, the shared slice layer. That is also
//! what makes them replicate: they are vault files, so the existing
//! vault sync carries them offline-first and any device re-resolves
//! them against its own copy of the same root.
//!
//! What stays here is only the field mapping. The frontmatter is built
//! key-by-key rather than by serializing the wire model, because the
//! wire model carries two fields a page must not repeat: `path` (the
//! page's own location — the file system already says it) and `note`
//! (the markdown body).

use chrono::{DateTime, Utc};
use files_proto::{NamedVersion, ProjectVersion};
use uuid::Uuid;
use vault::VaultPage;
use vault_entity::error::{ParseError, WriteError};
use vault_entity::store::VaultEntity;
use vault_entity::{frontmatter, yaml};

/// Vault folder holding every Files version entity, per root:
/// `Files/<root-slug>/versions/…` and
/// `Files/<root-slug>/project-versions/…`.
pub(crate) const FILES_FOLDER: &str = "Files";
pub(crate) const NAMED_SUBFOLDER: &str = "versions";
pub(crate) const PROJECT_SUBFOLDER: &str = "project-versions";

/// Vault mapping marker for [`NamedVersion`].
pub struct NamedVersions;

/// Vault mapping marker for [`ProjectVersion`].
pub struct ProjectVersions;

fn uuid_at(map: &serde_yaml::Mapping, key: &str, alt: &str) -> Option<Uuid> {
    yaml::str_at(map, key)
        .or_else(|| yaml::str_at(map, alt))
        .and_then(|s| Uuid::parse_str(&s).ok())
}

fn hex_at(map: &serde_yaml::Mapping, key: &str, alt: &str) -> Option<String> {
    yaml::str_at(map, key)
        .or_else(|| yaml::str_at(map, alt))
        .filter(|s| !s.is_empty())
}

impl VaultEntity for NamedVersions {
    type Model = NamedVersion;

    const TYPE: &'static str = "files-named-version";
    /// Only a fallback: real paths are per-root and built by
    /// [`crate::versions`] from the root's own name.
    const DEFAULT_FOLDER: &'static str = FILES_FOLDER;

    fn id(m: &NamedVersion) -> Uuid {
        m.id
    }
    fn set_id(m: &mut NamedVersion, id: Uuid) {
        m.id = id;
    }
    fn path(m: &NamedVersion) -> &str {
        &m.path
    }
    fn set_path(m: &mut NamedVersion, path: String) {
        m.path = path;
    }
    fn name(m: &NamedVersion) -> &str {
        &m.name
    }

    fn on_create(m: &mut NamedVersion, now: DateTime<Utc>) {
        if m.created_at.timestamp() == 0 {
            m.created_at = now;
        }
    }

    fn from_page(page: &VaultPage) -> Result<NamedVersion, ParseError> {
        let (map, body) = frontmatter::mapping(&page.raw).ok_or(ParseError::NoFrontmatter)?;
        let root_id = uuid_at(&map, "rootId", "root_id").ok_or_else(|| {
            ParseError::Field("named version is missing required `rootId`".into())
        })?;
        let commit_id = hex_at(&map, "commitId", "commit_id").ok_or_else(|| {
            ParseError::Field("named version is missing required `commitId`".into())
        })?;
        Ok(NamedVersion {
            id: uuid_at(&map, "id", "id")
                .unwrap_or_else(|| Uuid::new_v5(&Uuid::NAMESPACE_URL, page.rel_path.as_bytes())),
            path: page.rel_path.clone(),
            name: yaml::str_at(&map, "name").unwrap_or_else(|| page.basename.clone()),
            root_id,
            // A pre-resolution page may carry only the commit id; the
            // change id is recoverable from the store, so accept it.
            change_id: hex_at(&map, "changeId", "change_id").unwrap_or_default(),
            commit_id,
            note: body.trim_start_matches('\n').to_string(),
            created_at: yaml::timestamp_at(&map, "dateCreated").unwrap_or_else(Utc::now),
        })
    }

    fn to_markdown(m: &NamedVersion) -> Result<String, WriteError> {
        let mut map = serde_yaml::Mapping::new();
        map.insert("id".into(), m.id.to_string().into());
        map.insert("name".into(), m.name.clone().into());
        map.insert("rootId".into(), m.root_id.to_string().into());
        map.insert("changeId".into(), m.change_id.clone().into());
        map.insert("commitId".into(), m.commit_id.clone().into());
        map.insert("dateCreated".into(), m.created_at.to_rfc3339().into());
        frontmatter::document(Self::TYPE, &map, &m.note)
    }
}

impl VaultEntity for ProjectVersions {
    type Model = ProjectVersion;

    const TYPE: &'static str = "files-project-version";
    const DEFAULT_FOLDER: &'static str = FILES_FOLDER;

    fn id(m: &ProjectVersion) -> Uuid {
        m.id
    }
    fn set_id(m: &mut ProjectVersion, id: Uuid) {
        m.id = id;
    }
    fn path(m: &ProjectVersion) -> &str {
        &m.path
    }
    fn set_path(m: &mut ProjectVersion, path: String) {
        m.path = path;
    }
    /// A Project Version's human name is its number plus any label —
    /// `v2` / `v2 — Client remix`. Only used for the default filename,
    /// which [`crate::versions`] overrides anyway.
    fn name(m: &ProjectVersion) -> &str {
        m.label.as_deref().unwrap_or("project version")
    }

    fn on_create(m: &mut ProjectVersion, now: DateTime<Utc>) {
        if m.started_at.timestamp() == 0 {
            m.started_at = now;
        }
    }

    fn from_page(page: &VaultPage) -> Result<ProjectVersion, ParseError> {
        let (map, _body) = frontmatter::mapping(&page.raw).ok_or(ParseError::NoFrontmatter)?;
        let root_id = uuid_at(&map, "rootId", "root_id").ok_or_else(|| {
            ParseError::Field("project version is missing required `rootId`".into())
        })?;
        let number = yaml::i64_at(&map, "number")
            .and_then(|n| u32::try_from(n).ok())
            .ok_or_else(|| {
                ParseError::Field("project version is missing required `number`".into())
            })?;
        Ok(ProjectVersion {
            id: uuid_at(&map, "id", "id")
                .unwrap_or_else(|| Uuid::new_v5(&Uuid::NAMESPACE_URL, page.rel_path.as_bytes())),
            path: page.rel_path.clone(),
            root_id,
            number,
            label: yaml::str_at(&map, "label").filter(|s| !s.is_empty()),
            change_id: hex_at(&map, "changeId", "change_id").unwrap_or_default(),
            commit_id: hex_at(&map, "commitId", "commit_id").unwrap_or_default(),
            started_at: yaml::timestamp_at(&map, "dateCreated").unwrap_or_else(Utc::now),
        })
    }

    fn to_markdown(m: &ProjectVersion) -> Result<String, WriteError> {
        let mut map = serde_yaml::Mapping::new();
        map.insert("id".into(), m.id.to_string().into());
        map.insert("rootId".into(), m.root_id.to_string().into());
        map.insert("number".into(), i64::from(m.number).into());
        if let Some(label) = &m.label {
            map.insert("label".into(), label.clone().into());
        }
        map.insert("changeId".into(), m.change_id.clone().into());
        map.insert("commitId".into(), m.commit_id.clone().into());
        map.insert("dateCreated".into(), m.started_at.to_rfc3339().into());
        frontmatter::document(Self::TYPE, &map, "")
    }
}
