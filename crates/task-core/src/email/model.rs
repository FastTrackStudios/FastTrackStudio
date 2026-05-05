//! Email reference — pointer to an email in Nextcloud Mail.
//!
//! We don't mirror email bodies or attachments locally. An [`EmailRef`] is a
//! pointer: enough metadata to display the email in our UI and re-fetch it
//! from Nextcloud Mail on demand. Attachments are already in Nextcloud's
//! storage; reconstructing the URL from `account_id` + `nc_db_id` is cheap.
//!
//! # Routing model
//!
//! Emails arrive in Nextcloud Mail (or ProtonMail via IMAP). Our bot
//! ("Jarvis", typically) reads tags and forwarding-address hints and
//! decides which task or project each email belongs to, then calls
//! `task email link` to record the association.
//!
//! Projects and tasks declare which signals belong to them via
//! `email_aliases` (inbound addresses like `montreal-album@proj.example`)
//! and `email_tags` (mail-client labels like `project:montreal-album`).
//! Those are just strings in YAML — the bot decides how to interpret them.

use chrono::{DateTime, Utc};
use crudcrate::EntityToModels;
use facet::Facet;
use sea_orm::entity::prelude::*;
use sea_orm::sea_query::{ArrayType, ColumnType, Nullable, Value, ValueType, ValueTypeErr};
use sea_orm::{ColIdx, QueryResult, TryGetError, TryGetable};
use serde::{Deserialize, Serialize};
use std::ops::{Deref, DerefMut};
use utoipa::ToSchema;
use uuid::Uuid;

#[derive(Debug, Clone, PartialEq, Facet, Serialize, Deserialize, ToSchema)]
#[facet(transparent)]
#[serde(transparent)]
pub struct EmailStringList(pub Vec<String>);

impl Default for EmailStringList {
    fn default() -> Self {
        Self(Vec::new())
    }
}

impl Deref for EmailStringList {
    type Target = Vec<String>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for EmailStringList {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<Vec<String>> for EmailStringList {
    fn from(value: Vec<String>) -> Self {
        Self(value)
    }
}

impl From<EmailStringList> for Value {
    fn from(value: EmailStringList) -> Self {
        Value::Json(Some(Box::new(
            serde_json::to_value(value.0).unwrap_or(serde_json::Value::Array(Vec::new())),
        )))
    }
}

impl Nullable for EmailStringList {
    fn null() -> Value {
        Value::Json(None)
    }
}

impl TryGetable for EmailStringList {
    fn try_get_by<I: ColIdx>(res: &QueryResult, idx: I) -> Result<Self, TryGetError> {
        let value: serde_json::Value = res.try_get_by(idx)?;
        let items = serde_json::from_value(value).map_err(|err| {
            TryGetError::DbErr(sea_orm::DbErr::Type(format!(
                "failed to deserialize JSON array: {err}"
            )))
        })?;
        Ok(Self(items))
    }
}

impl ValueType for EmailStringList {
    fn try_from(value: Value) -> Result<Self, ValueTypeErr> {
        match value {
            Value::Json(Some(value)) => serde_json::from_value(*value)
                .map(Self)
                .map_err(|_| ValueTypeErr),
            _ => Err(ValueTypeErr),
        }
    }

    fn type_name() -> String {
        stringify!(EmailStringList).to_string()
    }

    fn array_type() -> ArrayType {
        ArrayType::Json
    }

    fn column_type() -> ColumnType {
        ColumnType::Json
    }
}

/// A reference to an email that lives in Nextcloud Mail (or any IMAP server
/// NC Mail has configured). Stored on [`Task`](crate::task::Task) and
/// [`Project`](crate::project::Project) frontmatter under `emails:`.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Default,
    Facet,
    DeriveEntityModel,
    EntityToModels,
    Serialize,
    Deserialize,
    ToSchema,
)]
#[sea_orm(table_name = "email_refs")]
#[crudcrate(
    api_struct = "EmailRefApi",
    generate_vox_service,
    name_singular = "email reference",
    name_plural = "email references"
)]
pub struct Model {
    #[facet(default)]
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(
        primary_key,
        exclude(create),
        on_create = uuid::Uuid::new_v4()
    )]
    pub uuid: Uuid,

    /// RFC 2822 `Message-ID` — the canonical, survives-folder-moves key.
    /// Store with or without the angle brackets; resolve functions should
    /// tolerate both.
    #[crudcrate(filterable, sortable, fulltext)]
    pub message_id: String,

    #[crudcrate(filterable, sortable, fulltext)]
    pub subject: String,
    /// "Display Name <addr@example.com>" when available, otherwise just
    /// the bare address.
    #[crudcrate(filterable, sortable, fulltext)]
    pub from: String,

    /// All recipient addresses (To only; not CC).
    #[facet(default)]
    pub to: EmailStringList,

    /// When the email was sent.
    #[crudcrate(filterable, sortable)]
    pub date: DateTime<Utc>,

    /// First ~200 chars of the body, for list-view previews. Optional —
    /// not every sync pipeline populates this.
    pub snippet: Option<String>,

    // ── Re-fetch hints ─────────────────────────────────────────────
    /// Nextcloud Mail account id. Lets us scope re-fetches without
    /// scanning every account.
    #[crudcrate(filterable, sortable)]
    pub account_id: Option<i64>,

    /// Mailbox/folder name at link time. Hint only — emails can move.
    pub mailbox: Option<String>,

    /// IMAP UID within the mailbox at link time. Invalidates on move or
    /// UIDVALIDITY change — re-resolve via Message-ID when stale.
    pub imap_uid: Option<u32>,

    /// Nextcloud Mail's database row id. Fastest way to fetch; also
    /// invalidates on folder moves.
    pub nc_db_id: Option<i64>,

    // ── UI hints ───────────────────────────────────────────────────
    /// Whether the email has any attachments. Populated at link time.
    #[facet(default)]
    #[crudcrate(filterable)]
    pub has_attachments: bool,

    /// Number of attachments.
    #[facet(default)]
    pub attachment_count: u32,

    // ── Provenance ─────────────────────────────────────────────────
    /// Who linked the email. "jarvis" for bot-driven links, a human
    /// username for manual links, "auto" for internal rule-based.
    pub linked_by: Option<String>,

    #[crudcrate(sortable)]
    pub linked_at: Option<DateTime<Utc>>,

    /// Free-form tags our users add on top of whatever the mail client
    /// tagged. Distinct from `Project.email_tags` (matcher input) —
    /// these are categorization output.
    #[facet(default)]
    pub user_tags: EmailStringList,
}

pub type EmailRef = Model;

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {}

impl EmailRef {
    /// Return the message id without angle brackets.
    pub fn bare_message_id(&self) -> &str {
        self.message_id
            .strip_prefix('<')
            .and_then(|s| s.strip_suffix('>'))
            .unwrap_or(&self.message_id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bare_message_id_strips_brackets() {
        let e = EmailRef {
            message_id: "<abc@example.com>".into(),
            ..Default::default()
        };
        assert_eq!(e.bare_message_id(), "abc@example.com");

        let e = EmailRef {
            message_id: "abc@example.com".into(),
            ..Default::default()
        };
        assert_eq!(e.bare_message_id(), "abc@example.com");
    }
}
