//! Team member model — supports claimed and unclaimed (placeholder) accounts.
//!
//! ## Account lifecycle:
//!
//! 1. **Placeholder** — created by a team admin when assigning someone who
//!    isn't on the platform yet. Has a username, name, role, but no auth
//!    account. Can be @mentioned, assigned tasks, listed in personnel.
//!    Comments attributed to them show as "James (unclaimed)".
//!
//! 2. **Invited** — an email invitation has been sent. The placeholder
//!    gains an `invite_token` and `invite_email`.
//!
//! 3. **Claimed** — the person signs up (or signs in with existing account)
//!    and claims the placeholder. All assignments, comments, mentions,
//!    and history merge into their real account. The placeholder username
//!    becomes an alias.
//!
//! 4. **Active** — normal authenticated team member.
//!
//! ## Nextcloud integration:
//! When Nextcloud is connected, claimed members sync with Nextcloud users.
//! Placeholder members can still exist without Nextcloud accounts.

use crudcrate::EntityToModels;
use facet::Facet;
use sea_orm::entity::prelude::*;
use serde::{Deserialize, Serialize};
use utoipa::ToSchema;
use uuid::Uuid;

use crate::task::StringList;

/// Account claim status.
#[derive(
    Debug, Clone, PartialEq, Eq, Facet, Serialize, Deserialize, ToSchema, EnumIter, DeriveActiveEnum,
)]
#[sea_orm(rs_type = "String", db_type = "String(StringLen::N(32))")]
#[repr(u8)]
#[derive(Default)]
pub enum AccountStatus {
    /// No auth account — placeholder created by admin.
    #[sea_orm(string_value = "placeholder")]
    #[default]
    Placeholder,
    /// Invitation sent but not yet accepted.
    #[sea_orm(string_value = "invited")]
    Invited,
    /// Account claimed by a real user.
    #[sea_orm(string_value = "claimed")]
    Claimed,
    /// Disabled / deactivated.
    #[sea_orm(string_value = "disabled")]
    Disabled,
}

/// A team member — either a real user or a placeholder awaiting claim.
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
#[sea_orm(table_name = "team_members")]
#[crudcrate(
    api_struct = "TeamMemberApi",
    generate_vox_service,
    name_singular = "team member",
    name_plural = "team members"
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

    /// Internal username / handle (e.g. "james"). Immutable once created.
    /// Used for @mentions, assignments, and URL slugs.
    #[crudcrate(filterable, sortable, fulltext)]
    pub username: String,

    /// Display name (e.g. "James Rodriguez").
    #[crudcrate(filterable, sortable, fulltext)]
    pub name: String,

    /// Role in the organization (e.g. "Session Drummer").
    #[crudcrate(filterable, sortable)]
    pub role: String,

    /// Department (e.g. "music", "engineering", "events").
    #[crudcrate(filterable, sortable)]
    pub department: String,

    /// Email — may be empty for placeholders.
    #[crudcrate(filterable, sortable)]
    pub email: String,

    /// Current account status.
    #[crudcrate(filterable, sortable)]
    pub status: AccountStatus,

    /// Auth user ID — set when the account is claimed.
    /// Links to better-auth's `AuthUser.id`.
    pub auth_id: Option<String>,

    /// Invite token — set when invitation is sent, cleared on claim.
    pub invite_token: Option<String>,

    /// The username of who created this placeholder.
    pub created_by: Option<String>,

    /// Nextcloud user ID — for sync with Nextcloud APIs.
    #[crudcrate(filterable, sortable)]
    pub nextcloud_id: Option<String>,

    /// Avatar URL or path.
    pub avatar: Option<String>,

    /// Short bio or notes about this person.
    pub bio: Option<String>,

    /// Usernames that this member was previously known as
    /// (from merged placeholder accounts).
    #[facet(default)]
    pub aliases: StringList,
}

pub type TeamMember = Model;

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {}

impl TeamMember {
    /// Create a new placeholder team member.
    pub fn placeholder(username: &str, name: &str, role: &str, department: &str) -> Self {
        Self {
            username: username.to_string(),
            name: name.to_string(),
            role: role.to_string(),
            department: department.to_string(),
            status: AccountStatus::Placeholder,
            ..Default::default()
        }
    }

    /// Create an invited member (placeholder + email).
    pub fn invited(username: &str, name: &str, role: &str, department: &str, email: &str) -> Self {
        let mut m = Self::placeholder(username, name, role, department);
        m.email = email.to_string();
        m.status = AccountStatus::Invited;
        m
    }

    /// Create a fully claimed member.
    pub fn claimed(
        username: &str,
        name: &str,
        role: &str,
        department: &str,
        email: &str,
        auth_id: &str,
    ) -> Self {
        Self {
            username: username.to_string(),
            name: name.to_string(),
            role: role.to_string(),
            department: department.to_string(),
            email: email.to_string(),
            status: AccountStatus::Claimed,
            auth_id: Some(auth_id.to_string()),
            ..Default::default()
        }
    }

    /// Claim this placeholder with a real auth account.
    /// Merges the placeholder identity into the real account.
    pub fn claim(&mut self, auth_id: &str, email: &str) {
        self.auth_id = Some(auth_id.to_string());
        if self.email.is_empty() {
            self.email = email.to_string();
        }
        self.status = AccountStatus::Claimed;
        self.invite_token = None;
    }

    /// Merge another username into this account (when a placeholder
    /// gets claimed by someone who already has an account).
    pub fn merge_alias(&mut self, old_username: &str) {
        if !self.aliases.contains(&old_username.to_string()) {
            self.aliases.push(old_username.to_string());
        }
    }

    pub fn is_placeholder(&self) -> bool {
        self.status == AccountStatus::Placeholder
    }

    pub fn is_claimed(&self) -> bool {
        self.status == AccountStatus::Claimed
    }

    pub fn is_invited(&self) -> bool {
        self.status == AccountStatus::Invited
    }

    /// Display label — name + "(unclaimed)" for placeholders.
    pub fn display_label(&self) -> String {
        match self.status {
            AccountStatus::Placeholder => format!("{} (unclaimed)", self.name),
            AccountStatus::Invited => format!("{} (invited)", self.name),
            AccountStatus::Disabled => format!("{} (disabled)", self.name),
            AccountStatus::Claimed => self.name.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn placeholder_lifecycle() {
        // Admin creates a placeholder for the session drummer
        let mut james =
            TeamMember::placeholder("james", "James Rodriguez", "Session Drummer", "music");
        assert!(james.is_placeholder());
        assert_eq!(james.display_label(), "James Rodriguez (unclaimed)");
        assert!(james.auth_id.is_none());

        // Admin sends an invitation
        james.email = "james@example.com".to_string();
        james.status = AccountStatus::Invited;
        james.invite_token = Some("inv_abc123".to_string());
        assert!(james.is_invited());

        // James clicks the invite link and signs up
        james.claim("auth_user_xyz", "james@example.com");
        assert!(james.is_claimed());
        assert_eq!(james.auth_id.as_deref(), Some("auth_user_xyz"));
        assert!(james.invite_token.is_none());
        assert_eq!(james.display_label(), "James Rodriguez");
    }

    #[test]
    fn merge_existing_account() {
        // "james-temp" was a placeholder assigned to tasks
        let mut real_james = TeamMember::claimed(
            "james",
            "James Rodriguez",
            "Session Drummer",
            "music",
            "james@example.com",
            "auth_123",
        );

        // Merge the old placeholder username
        real_james.merge_alias("james-temp");
        assert_eq!(real_james.aliases.as_slice(), ["james-temp"]);

        // Now any reference to "james-temp" in tasks/comments
        // resolves to this account
    }
}
