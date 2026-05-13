//! SeaORM-backed database adapter for better-auth.
//!
//! Implements all 10 `*Ops` traits from `better_auth_core::adapters` using
//! SeaORM with SQLite, so auth data (users, sessions, orgs, etc.) is persisted
//! to the same SQLite database as task/project data.

use async_trait::async_trait;
use chrono::Utc;
use sea_orm::prelude::*;
use sea_orm::*;
use uuid::Uuid;

use better_auth_core::adapters::*;
use better_auth_core::error::{AuthError, AuthResult};
use better_auth_core::types::*;

use crate::entities::*;

/// SeaORM adapter for better-auth, backed by SQLite.
#[derive(Clone)]
pub struct SeaOrmAuthAdapter {
    db: DatabaseConnection,
}

impl SeaOrmAuthAdapter {
    pub fn new(db: DatabaseConnection) -> Self {
        Self { db }
    }
}

// ── UserOps ─────────────────────────────────────────────────────────────────

#[async_trait]
impl UserOps for SeaOrmAuthAdapter {
    type User = auth_user::Model;

    async fn create_user(&self, user: CreateUser) -> AuthResult<Self::User> {
        let now = Utc::now();
        let id = user.id.unwrap_or_else(|| Uuid::new_v4().to_string());
        let model = auth_user::ActiveModel {
            id: Set(id),
            email: Set(user.email),
            name: Set(user.name),
            image: Set(user.image),
            email_verified: Set(user.email_verified.unwrap_or(false)),
            username: Set(user.username),
            display_username: Set(user.display_username),
            role: Set(user.role),
            metadata: Set(user.metadata.unwrap_or(serde_json::json!({}))),
            two_factor_enabled: Set(false),
            banned: Set(false),
            ban_reason: Set(None),
            ban_expires: Set(None),
            created_at: Set(now),
            updated_at: Set(now),
        };
        let res = model.insert(&self.db).await.map_err(db_err)?;
        Ok(res)
    }

    async fn get_user_by_id(&self, id: &str) -> AuthResult<Option<Self::User>> {
        auth_user::Entity::find_by_id(id.to_string())
            .one(&self.db)
            .await
            .map_err(db_err)
    }

    async fn get_user_by_email(&self, email: &str) -> AuthResult<Option<Self::User>> {
        auth_user::Entity::find()
            .filter(auth_user::Column::Email.eq(email))
            .one(&self.db)
            .await
            .map_err(db_err)
    }

    async fn get_user_by_username(&self, username: &str) -> AuthResult<Option<Self::User>> {
        auth_user::Entity::find()
            .filter(auth_user::Column::Username.eq(username))
            .one(&self.db)
            .await
            .map_err(db_err)
    }

    async fn update_user(&self, id: &str, update: UpdateUser) -> AuthResult<Self::User> {
        let existing = self
            .get_user_by_id(id)
            .await?
            .ok_or(AuthError::UserNotFound)?;
        let mut model: auth_user::ActiveModel = existing.into();
        if let Some(email) = update.email {
            model.email = Set(Some(email));
        }
        if let Some(name) = update.name {
            model.name = Set(Some(name));
        }
        if let Some(image) = update.image {
            model.image = Set(Some(image));
        }
        if let Some(ev) = update.email_verified {
            model.email_verified = Set(ev);
        }
        if let Some(username) = update.username {
            model.username = Set(Some(username));
        }
        if let Some(du) = update.display_username {
            model.display_username = Set(Some(du));
        }
        if let Some(role) = update.role {
            model.role = Set(Some(role));
        }
        if let Some(banned) = update.banned {
            model.banned = Set(banned);
        }
        if let Some(reason) = update.ban_reason {
            model.ban_reason = Set(Some(reason));
        }
        if let Some(expires) = update.ban_expires {
            model.ban_expires = Set(Some(expires));
        }
        if let Some(tfa) = update.two_factor_enabled {
            model.two_factor_enabled = Set(tfa);
        }
        if let Some(meta) = update.metadata {
            model.metadata = Set(meta);
        }
        model.updated_at = Set(Utc::now());
        model.update(&self.db).await.map_err(db_err)
    }

    async fn delete_user(&self, id: &str) -> AuthResult<()> {
        auth_user::Entity::delete_by_id(id.to_string())
            .exec(&self.db)
            .await
            .map_err(db_err)?;
        Ok(())
    }

    async fn list_users(&self, params: ListUsersParams) -> AuthResult<(Vec<Self::User>, usize)> {
        let mut query = auth_user::Entity::find();

        if let (Some(field), Some(value)) = (&params.search_field, &params.search_value) {
            match field.as_str() {
                "email" => {
                    query = query.filter(auth_user::Column::Email.contains(value));
                }
                "name" => {
                    query = query.filter(auth_user::Column::Name.contains(value));
                }
                "username" => {
                    query = query.filter(auth_user::Column::Username.contains(value));
                }
                _ => {}
            }
        }

        let total = query.clone().count(&self.db).await.map_err(db_err)? as usize;

        if let Some(offset) = params.offset {
            query = query.offset(offset as u64);
        }
        if let Some(limit) = params.limit {
            query = query.limit(limit as u64);
        }

        let users = query.all(&self.db).await.map_err(db_err)?;
        Ok((users, total))
    }
}

// ── SessionOps ──────────────────────────────────────────────────────────────

#[async_trait]
impl SessionOps for SeaOrmAuthAdapter {
    type Session = auth_session::Model;

    async fn create_session(&self, session: CreateSession) -> AuthResult<Self::Session> {
        let now = Utc::now();
        let id = Uuid::new_v4().to_string();
        let token = Uuid::new_v4().to_string();
        let model = auth_session::ActiveModel {
            id: Set(id),
            expires_at: Set(session.expires_at),
            token: Set(token),
            created_at: Set(now),
            updated_at: Set(now),
            ip_address: Set(session.ip_address),
            user_agent: Set(session.user_agent),
            user_id: Set(session.user_id),
            impersonated_by: Set(session.impersonated_by),
            active_organization_id: Set(session.active_organization_id),
            active: Set(true),
        };
        model.insert(&self.db).await.map_err(db_err)
    }

    async fn get_session(&self, token: &str) -> AuthResult<Option<Self::Session>> {
        auth_session::Entity::find()
            .filter(auth_session::Column::Token.eq(token))
            .one(&self.db)
            .await
            .map_err(db_err)
    }

    async fn get_user_sessions(&self, user_id: &str) -> AuthResult<Vec<Self::Session>> {
        auth_session::Entity::find()
            .filter(auth_session::Column::UserId.eq(user_id))
            .all(&self.db)
            .await
            .map_err(db_err)
    }

    async fn update_session_expiry(
        &self,
        token: &str,
        expires_at: chrono::DateTime<Utc>,
    ) -> AuthResult<()> {
        auth_session::Entity::update_many()
            .col_expr(auth_session::Column::ExpiresAt, Expr::value(expires_at))
            .col_expr(auth_session::Column::UpdatedAt, Expr::value(Utc::now()))
            .filter(auth_session::Column::Token.eq(token))
            .exec(&self.db)
            .await
            .map_err(db_err)?;
        Ok(())
    }

    async fn delete_session(&self, token: &str) -> AuthResult<()> {
        auth_session::Entity::delete_many()
            .filter(auth_session::Column::Token.eq(token))
            .exec(&self.db)
            .await
            .map_err(db_err)?;
        Ok(())
    }

    async fn delete_user_sessions(&self, user_id: &str) -> AuthResult<()> {
        auth_session::Entity::delete_many()
            .filter(auth_session::Column::UserId.eq(user_id))
            .exec(&self.db)
            .await
            .map_err(db_err)?;
        Ok(())
    }

    async fn delete_expired_sessions(&self) -> AuthResult<usize> {
        let res = auth_session::Entity::delete_many()
            .filter(auth_session::Column::ExpiresAt.lt(Utc::now()))
            .exec(&self.db)
            .await
            .map_err(db_err)?;
        Ok(res.rows_affected as usize)
    }

    async fn update_session_active_organization(
        &self,
        token: &str,
        organization_id: Option<&str>,
    ) -> AuthResult<Self::Session> {
        let session = self
            .get_session(token)
            .await?
            .ok_or(AuthError::SessionNotFound)?;
        let mut model: auth_session::ActiveModel = session.into();
        model.active_organization_id = Set(organization_id.map(|s| s.to_string()));
        model.updated_at = Set(Utc::now());
        model.update(&self.db).await.map_err(db_err)
    }
}

// ── AccountOps ──────────────────────────────────────────────────────────────

#[async_trait]
impl AccountOps for SeaOrmAuthAdapter {
    type Account = auth_account::Model;

    async fn create_account(&self, account: CreateAccount) -> AuthResult<Self::Account> {
        let now = Utc::now();
        let id = Uuid::new_v4().to_string();
        let model = auth_account::ActiveModel {
            id: Set(id),
            account_id: Set(account.account_id),
            provider_id: Set(account.provider_id),
            user_id: Set(account.user_id),
            access_token: Set(account.access_token),
            refresh_token: Set(account.refresh_token),
            id_token: Set(account.id_token),
            access_token_expires_at: Set(account.access_token_expires_at),
            refresh_token_expires_at: Set(account.refresh_token_expires_at),
            scope: Set(account.scope),
            password: Set(account.password),
            created_at: Set(now),
            updated_at: Set(now),
        };
        model.insert(&self.db).await.map_err(db_err)
    }

    async fn get_account(
        &self,
        provider: &str,
        provider_account_id: &str,
    ) -> AuthResult<Option<Self::Account>> {
        auth_account::Entity::find()
            .filter(auth_account::Column::ProviderId.eq(provider))
            .filter(auth_account::Column::AccountId.eq(provider_account_id))
            .one(&self.db)
            .await
            .map_err(db_err)
    }

    async fn get_user_accounts(&self, user_id: &str) -> AuthResult<Vec<Self::Account>> {
        auth_account::Entity::find()
            .filter(auth_account::Column::UserId.eq(user_id))
            .all(&self.db)
            .await
            .map_err(db_err)
    }

    async fn update_account(&self, id: &str, update: UpdateAccount) -> AuthResult<Self::Account> {
        let existing = auth_account::Entity::find_by_id(id.to_string())
            .one(&self.db)
            .await
            .map_err(db_err)?
            .ok_or_else(|| AuthError::NotFound("account".into()))?;
        let mut model: auth_account::ActiveModel = existing.into();
        if let Some(at) = update.access_token {
            model.access_token = Set(Some(at));
        }
        if let Some(rt) = update.refresh_token {
            model.refresh_token = Set(Some(rt));
        }
        if let Some(ate) = update.access_token_expires_at {
            model.access_token_expires_at = Set(Some(ate));
        }
        if let Some(rte) = update.refresh_token_expires_at {
            model.refresh_token_expires_at = Set(Some(rte));
        }
        model.updated_at = Set(Utc::now());
        model.update(&self.db).await.map_err(db_err)
    }

    async fn delete_account(&self, id: &str) -> AuthResult<()> {
        auth_account::Entity::delete_by_id(id.to_string())
            .exec(&self.db)
            .await
            .map_err(db_err)?;
        Ok(())
    }
}

// ── VerificationOps ─────────────────────────────────────────────────────────

#[async_trait]
impl VerificationOps for SeaOrmAuthAdapter {
    type Verification = auth_verification::Model;

    async fn create_verification(&self, v: CreateVerification) -> AuthResult<Self::Verification> {
        let now = Utc::now();
        let id = Uuid::new_v4().to_string();
        let model = auth_verification::ActiveModel {
            id: Set(id),
            identifier: Set(v.identifier),
            value: Set(v.value),
            expires_at: Set(v.expires_at),
            created_at: Set(now),
            updated_at: Set(now),
        };
        model.insert(&self.db).await.map_err(db_err)
    }

    async fn get_verification(
        &self,
        identifier: &str,
        value: &str,
    ) -> AuthResult<Option<Self::Verification>> {
        auth_verification::Entity::find()
            .filter(auth_verification::Column::Identifier.eq(identifier))
            .filter(auth_verification::Column::Value.eq(value))
            .one(&self.db)
            .await
            .map_err(db_err)
    }

    async fn get_verification_by_value(
        &self,
        value: &str,
    ) -> AuthResult<Option<Self::Verification>> {
        auth_verification::Entity::find()
            .filter(auth_verification::Column::Value.eq(value))
            .one(&self.db)
            .await
            .map_err(db_err)
    }

    async fn get_verification_by_identifier(
        &self,
        identifier: &str,
    ) -> AuthResult<Option<Self::Verification>> {
        auth_verification::Entity::find()
            .filter(auth_verification::Column::Identifier.eq(identifier))
            .one(&self.db)
            .await
            .map_err(db_err)
    }

    async fn consume_verification(
        &self,
        identifier: &str,
        value: &str,
    ) -> AuthResult<Option<Self::Verification>> {
        let found = self.get_verification(identifier, value).await?;
        if let Some(ref v) = found {
            auth_verification::Entity::delete_by_id(v.id.clone())
                .exec(&self.db)
                .await
                .map_err(db_err)?;
        }
        Ok(found)
    }

    async fn delete_verification(&self, id: &str) -> AuthResult<()> {
        auth_verification::Entity::delete_by_id(id.to_string())
            .exec(&self.db)
            .await
            .map_err(db_err)?;
        Ok(())
    }

    async fn delete_expired_verifications(&self) -> AuthResult<usize> {
        let res = auth_verification::Entity::delete_many()
            .filter(auth_verification::Column::ExpiresAt.lt(Utc::now()))
            .exec(&self.db)
            .await
            .map_err(db_err)?;
        Ok(res.rows_affected as usize)
    }
}

// ── OrganizationOps ─────────────────────────────────────────────────────────

#[async_trait]
impl OrganizationOps for SeaOrmAuthAdapter {
    type Organization = auth_organization::Model;

    async fn create_organization(&self, org: CreateOrganization) -> AuthResult<Self::Organization> {
        let now = Utc::now();
        let id = org.id.unwrap_or_else(|| Uuid::new_v4().to_string());
        let model = auth_organization::ActiveModel {
            id: Set(id),
            name: Set(org.name),
            slug: Set(org.slug),
            logo: Set(org.logo),
            metadata: Set(org.metadata),
            created_at: Set(now),
            updated_at: Set(now),
        };
        model.insert(&self.db).await.map_err(db_err)
    }

    async fn get_organization_by_id(&self, id: &str) -> AuthResult<Option<Self::Organization>> {
        auth_organization::Entity::find_by_id(id.to_string())
            .one(&self.db)
            .await
            .map_err(db_err)
    }

    async fn get_organization_by_slug(&self, slug: &str) -> AuthResult<Option<Self::Organization>> {
        auth_organization::Entity::find()
            .filter(auth_organization::Column::Slug.eq(slug))
            .one(&self.db)
            .await
            .map_err(db_err)
    }

    async fn update_organization(
        &self,
        id: &str,
        update: UpdateOrganization,
    ) -> AuthResult<Self::Organization> {
        let existing = self
            .get_organization_by_id(id)
            .await?
            .ok_or_else(|| AuthError::NotFound("organization".into()))?;
        let mut model: auth_organization::ActiveModel = existing.into();
        if let Some(name) = update.name {
            model.name = Set(name);
        }
        if let Some(slug) = update.slug {
            model.slug = Set(slug);
        }
        if let Some(logo) = update.logo {
            model.logo = Set(Some(logo));
        }
        if let Some(meta) = update.metadata {
            model.metadata = Set(Some(meta));
        }
        model.updated_at = Set(Utc::now());
        model.update(&self.db).await.map_err(db_err)
    }

    async fn delete_organization(&self, id: &str) -> AuthResult<()> {
        auth_organization::Entity::delete_by_id(id.to_string())
            .exec(&self.db)
            .await
            .map_err(db_err)?;
        Ok(())
    }

    async fn list_user_organizations(&self, user_id: &str) -> AuthResult<Vec<Self::Organization>> {
        // Join members → organizations where member.user_id = user_id
        let members = auth_member::Entity::find()
            .filter(auth_member::Column::UserId.eq(user_id))
            .all(&self.db)
            .await
            .map_err(db_err)?;

        let org_ids: Vec<String> = members.into_iter().map(|m| m.organization_id).collect();
        if org_ids.is_empty() {
            return Ok(vec![]);
        }

        auth_organization::Entity::find()
            .filter(auth_organization::Column::Id.is_in(org_ids))
            .all(&self.db)
            .await
            .map_err(db_err)
    }
}

// ── MemberOps ───────────────────────────────────────────────────────────────

#[async_trait]
impl MemberOps for SeaOrmAuthAdapter {
    type Member = auth_member::Model;

    async fn create_member(&self, member: CreateMember) -> AuthResult<Self::Member> {
        let now = Utc::now();
        let id = Uuid::new_v4().to_string();
        let model = auth_member::ActiveModel {
            id: Set(id),
            organization_id: Set(member.organization_id),
            user_id: Set(member.user_id),
            role: Set(member.role),
            created_at: Set(now),
        };
        model.insert(&self.db).await.map_err(db_err)
    }

    async fn get_member(
        &self,
        organization_id: &str,
        user_id: &str,
    ) -> AuthResult<Option<Self::Member>> {
        auth_member::Entity::find()
            .filter(auth_member::Column::OrganizationId.eq(organization_id))
            .filter(auth_member::Column::UserId.eq(user_id))
            .one(&self.db)
            .await
            .map_err(db_err)
    }

    async fn get_member_by_id(&self, id: &str) -> AuthResult<Option<Self::Member>> {
        auth_member::Entity::find_by_id(id.to_string())
            .one(&self.db)
            .await
            .map_err(db_err)
    }

    async fn update_member_role(&self, member_id: &str, role: &str) -> AuthResult<Self::Member> {
        let existing = self
            .get_member_by_id(member_id)
            .await?
            .ok_or_else(|| AuthError::NotFound("member".into()))?;
        let mut model: auth_member::ActiveModel = existing.into();
        model.role = Set(role.to_string());
        model.update(&self.db).await.map_err(db_err)
    }

    async fn delete_member(&self, member_id: &str) -> AuthResult<()> {
        auth_member::Entity::delete_by_id(member_id.to_string())
            .exec(&self.db)
            .await
            .map_err(db_err)?;
        Ok(())
    }

    async fn list_organization_members(
        &self,
        organization_id: &str,
    ) -> AuthResult<Vec<Self::Member>> {
        auth_member::Entity::find()
            .filter(auth_member::Column::OrganizationId.eq(organization_id))
            .all(&self.db)
            .await
            .map_err(db_err)
    }

    async fn count_organization_members(&self, organization_id: &str) -> AuthResult<usize> {
        let count = auth_member::Entity::find()
            .filter(auth_member::Column::OrganizationId.eq(organization_id))
            .count(&self.db)
            .await
            .map_err(db_err)?;
        Ok(count as usize)
    }

    async fn count_organization_owners(&self, organization_id: &str) -> AuthResult<usize> {
        let count = auth_member::Entity::find()
            .filter(auth_member::Column::OrganizationId.eq(organization_id))
            .filter(auth_member::Column::Role.eq("owner"))
            .count(&self.db)
            .await
            .map_err(db_err)?;
        Ok(count as usize)
    }
}

// ── InvitationOps ───────────────────────────────────────────────────────────

#[async_trait]
impl InvitationOps for SeaOrmAuthAdapter {
    type Invitation = auth_invitation::Model;

    async fn create_invitation(&self, inv: CreateInvitation) -> AuthResult<Self::Invitation> {
        let id = Uuid::new_v4().to_string();
        let now = Utc::now();
        let model = auth_invitation::ActiveModel {
            id: Set(id),
            organization_id: Set(inv.organization_id),
            email: Set(inv.email),
            role: Set(inv.role),
            status: Set("pending".to_string()),
            inviter_id: Set(inv.inviter_id),
            expires_at: Set(inv.expires_at),
            created_at: Set(now),
        };
        model.insert(&self.db).await.map_err(db_err)
    }

    async fn get_invitation_by_id(&self, id: &str) -> AuthResult<Option<Self::Invitation>> {
        auth_invitation::Entity::find_by_id(id.to_string())
            .one(&self.db)
            .await
            .map_err(db_err)
    }

    async fn get_pending_invitation(
        &self,
        organization_id: &str,
        email: &str,
    ) -> AuthResult<Option<Self::Invitation>> {
        auth_invitation::Entity::find()
            .filter(auth_invitation::Column::OrganizationId.eq(organization_id))
            .filter(auth_invitation::Column::Email.eq(email))
            .filter(auth_invitation::Column::Status.eq("pending"))
            .one(&self.db)
            .await
            .map_err(db_err)
    }

    async fn update_invitation_status(
        &self,
        id: &str,
        status: InvitationStatus,
    ) -> AuthResult<Self::Invitation> {
        let existing = self
            .get_invitation_by_id(id)
            .await?
            .ok_or_else(|| AuthError::NotFound("invitation".into()))?;
        let mut model: auth_invitation::ActiveModel = existing.into();
        model.status = Set(status.to_string());
        model.update(&self.db).await.map_err(db_err)
    }

    async fn list_organization_invitations(
        &self,
        organization_id: &str,
    ) -> AuthResult<Vec<Self::Invitation>> {
        auth_invitation::Entity::find()
            .filter(auth_invitation::Column::OrganizationId.eq(organization_id))
            .all(&self.db)
            .await
            .map_err(db_err)
    }

    async fn list_user_invitations(&self, email: &str) -> AuthResult<Vec<Self::Invitation>> {
        auth_invitation::Entity::find()
            .filter(auth_invitation::Column::Email.eq(email))
            .all(&self.db)
            .await
            .map_err(db_err)
    }
}

// ── TwoFactorOps ────────────────────────────────────────────────────────────

#[async_trait]
impl TwoFactorOps for SeaOrmAuthAdapter {
    type TwoFactor = auth_two_factor::Model;

    async fn create_two_factor(&self, tf: CreateTwoFactor) -> AuthResult<Self::TwoFactor> {
        let now = Utc::now();
        let id = Uuid::new_v4().to_string();
        let model = auth_two_factor::ActiveModel {
            id: Set(id),
            secret: Set(tf.secret),
            backup_codes: Set(tf.backup_codes),
            user_id: Set(tf.user_id),
            created_at: Set(now),
            updated_at: Set(now),
        };
        model.insert(&self.db).await.map_err(db_err)
    }

    async fn get_two_factor_by_user_id(
        &self,
        user_id: &str,
    ) -> AuthResult<Option<Self::TwoFactor>> {
        auth_two_factor::Entity::find()
            .filter(auth_two_factor::Column::UserId.eq(user_id))
            .one(&self.db)
            .await
            .map_err(db_err)
    }

    async fn update_two_factor_backup_codes(
        &self,
        user_id: &str,
        backup_codes: &str,
    ) -> AuthResult<Self::TwoFactor> {
        let existing = self
            .get_two_factor_by_user_id(user_id)
            .await?
            .ok_or_else(|| AuthError::NotFound("two_factor".into()))?;
        let mut model: auth_two_factor::ActiveModel = existing.into();
        model.backup_codes = Set(Some(backup_codes.to_string()));
        model.updated_at = Set(Utc::now());
        model.update(&self.db).await.map_err(db_err)
    }

    async fn delete_two_factor(&self, user_id: &str) -> AuthResult<()> {
        auth_two_factor::Entity::delete_many()
            .filter(auth_two_factor::Column::UserId.eq(user_id))
            .exec(&self.db)
            .await
            .map_err(db_err)?;
        Ok(())
    }
}

// ── ApiKeyOps ───────────────────────────────────────────────────────────────

#[async_trait]
impl ApiKeyOps for SeaOrmAuthAdapter {
    type ApiKey = auth_api_key::Model;

    async fn create_api_key(&self, input: CreateApiKey) -> AuthResult<Self::ApiKey> {
        let now = Utc::now().to_rfc3339();
        let id = Uuid::new_v4().to_string();
        let model = auth_api_key::ActiveModel {
            id: Set(id),
            name: Set(input.name),
            start: Set(input.start),
            prefix: Set(input.prefix),
            key_hash: Set(input.key_hash),
            user_id: Set(input.user_id),
            refill_interval: Set(input.refill_interval),
            refill_amount: Set(input.refill_amount),
            last_refill_at: Set(None),
            enabled: Set(input.enabled),
            rate_limit_enabled: Set(input.rate_limit_enabled),
            rate_limit_time_window: Set(input.rate_limit_time_window),
            rate_limit_max: Set(input.rate_limit_max),
            request_count: Set(None),
            remaining: Set(input.remaining),
            last_request: Set(None),
            expires_at: Set(input.expires_at),
            created_at: Set(now.clone()),
            updated_at: Set(now),
            permissions: Set(input.permissions),
            metadata: Set(input.metadata),
        };
        model.insert(&self.db).await.map_err(db_err)
    }

    async fn get_api_key_by_id(&self, id: &str) -> AuthResult<Option<Self::ApiKey>> {
        auth_api_key::Entity::find_by_id(id.to_string())
            .one(&self.db)
            .await
            .map_err(db_err)
    }

    async fn get_api_key_by_hash(&self, hash: &str) -> AuthResult<Option<Self::ApiKey>> {
        auth_api_key::Entity::find()
            .filter(auth_api_key::Column::KeyHash.eq(hash))
            .one(&self.db)
            .await
            .map_err(db_err)
    }

    async fn list_api_keys_by_user(&self, user_id: &str) -> AuthResult<Vec<Self::ApiKey>> {
        auth_api_key::Entity::find()
            .filter(auth_api_key::Column::UserId.eq(user_id))
            .all(&self.db)
            .await
            .map_err(db_err)
    }

    async fn update_api_key(&self, id: &str, update: UpdateApiKey) -> AuthResult<Self::ApiKey> {
        let existing = self
            .get_api_key_by_id(id)
            .await?
            .ok_or_else(|| AuthError::NotFound("api_key".into()))?;
        let mut model: auth_api_key::ActiveModel = existing.into();
        if let Some(name) = update.name {
            model.name = Set(Some(name));
        }
        if let Some(enabled) = update.enabled {
            model.enabled = Set(enabled);
        }
        if let Some(remaining) = update.remaining {
            model.remaining = Set(Some(remaining));
        }
        if let Some(last_request) = update.last_request {
            model.last_request = Set(last_request);
        }
        if let Some(request_count) = update.request_count {
            model.request_count = Set(Some(request_count));
        }
        if let Some(last_refill) = update.last_refill_at {
            model.last_refill_at = Set(last_refill);
        }
        if let Some(permissions) = update.permissions {
            model.permissions = Set(Some(permissions));
        }
        if let Some(metadata) = update.metadata {
            model.metadata = Set(Some(metadata));
        }
        model.updated_at = Set(Utc::now().to_rfc3339());
        model.update(&self.db).await.map_err(db_err)
    }

    async fn delete_api_key(&self, id: &str) -> AuthResult<()> {
        auth_api_key::Entity::delete_by_id(id.to_string())
            .exec(&self.db)
            .await
            .map_err(db_err)?;
        Ok(())
    }

    async fn delete_expired_api_keys(&self) -> AuthResult<usize> {
        // API key expires_at is a TEXT field — compare as string
        let now = Utc::now().to_rfc3339();
        let res = auth_api_key::Entity::delete_many()
            .filter(auth_api_key::Column::ExpiresAt.is_not_null())
            .filter(auth_api_key::Column::ExpiresAt.lt(&now))
            .exec(&self.db)
            .await
            .map_err(db_err)?;
        Ok(res.rows_affected as usize)
    }
}

// ── PasskeyOps ──────────────────────────────────────────────────────────────

#[async_trait]
impl PasskeyOps for SeaOrmAuthAdapter {
    type Passkey = auth_passkey::Model;

    async fn create_passkey(&self, input: CreatePasskey) -> AuthResult<Self::Passkey> {
        let now = Utc::now();
        let id = Uuid::new_v4().to_string();
        let model = auth_passkey::ActiveModel {
            id: Set(id),
            name: Set(input.name),
            public_key: Set(input.public_key),
            user_id: Set(input.user_id),
            credential_id: Set(input.credential_id),
            counter: Set(input.counter as i64),
            device_type: Set(input.device_type),
            backed_up: Set(input.backed_up),
            transports: Set(input.transports),
            created_at: Set(now),
        };
        model.insert(&self.db).await.map_err(db_err)
    }

    async fn get_passkey_by_id(&self, id: &str) -> AuthResult<Option<Self::Passkey>> {
        auth_passkey::Entity::find_by_id(id.to_string())
            .one(&self.db)
            .await
            .map_err(db_err)
    }

    async fn get_passkey_by_credential_id(
        &self,
        credential_id: &str,
    ) -> AuthResult<Option<Self::Passkey>> {
        auth_passkey::Entity::find()
            .filter(auth_passkey::Column::CredentialId.eq(credential_id))
            .one(&self.db)
            .await
            .map_err(db_err)
    }

    async fn list_passkeys_by_user(&self, user_id: &str) -> AuthResult<Vec<Self::Passkey>> {
        auth_passkey::Entity::find()
            .filter(auth_passkey::Column::UserId.eq(user_id))
            .all(&self.db)
            .await
            .map_err(db_err)
    }

    async fn update_passkey_counter(&self, id: &str, counter: u64) -> AuthResult<Self::Passkey> {
        let existing = self
            .get_passkey_by_id(id)
            .await?
            .ok_or_else(|| AuthError::NotFound("passkey".into()))?;
        let mut model: auth_passkey::ActiveModel = existing.into();
        model.counter = Set(counter as i64);
        model.update(&self.db).await.map_err(db_err)
    }

    async fn update_passkey_name(&self, id: &str, name: &str) -> AuthResult<Self::Passkey> {
        let existing = self
            .get_passkey_by_id(id)
            .await?
            .ok_or_else(|| AuthError::NotFound("passkey".into()))?;
        let mut model: auth_passkey::ActiveModel = existing.into();
        model.name = Set(name.to_string());
        model.update(&self.db).await.map_err(db_err)
    }

    async fn delete_passkey(&self, id: &str) -> AuthResult<()> {
        auth_passkey::Entity::delete_by_id(id.to_string())
            .exec(&self.db)
            .await
            .map_err(db_err)?;
        Ok(())
    }
}

// ── Helpers ─────────────────────────────────────────────────────────────────

fn db_err(e: DbErr) -> AuthError {
    AuthError::Database(better_auth_core::error::DatabaseError::Query(e.to_string()))
}
