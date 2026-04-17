//! SeaORM entities for better-auth tables.
//!
//! Each submodule defines a SeaORM entity (`Model`, `Column`, `Entity`,
//! `Relation`, `ActiveModel`) and implements the corresponding better-auth
//! entity trait (`AuthUser`, `AuthSession`, etc.) plus its `Meta` trait.

pub mod auth_user {
    use sea_orm::entity::prelude::*;
    use serde::{Deserialize, Serialize};

    #[derive(Clone, Debug, PartialEq, DeriveEntityModel, Serialize, Deserialize)]
    #[sea_orm(table_name = "auth_users")]
    pub struct Model {
        #[sea_orm(primary_key, auto_increment = false)]
        pub id: String,
        pub email: Option<String>,
        pub name: Option<String>,
        pub email_verified: bool,
        pub image: Option<String>,
        pub created_at: chrono::DateTime<chrono::Utc>,
        pub updated_at: chrono::DateTime<chrono::Utc>,
        pub username: Option<String>,
        pub display_username: Option<String>,
        pub two_factor_enabled: bool,
        pub role: Option<String>,
        pub banned: bool,
        pub ban_reason: Option<String>,
        pub ban_expires: Option<chrono::DateTime<chrono::Utc>>,
        #[sea_orm(column_type = "Json")]
        pub metadata: serde_json::Value,
    }

    #[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
    pub enum Relation {}

    impl ActiveModelBehavior for ActiveModel {}

    impl better_auth_core::entity::AuthUser for Model {
        fn id(&self) -> &str { &self.id }
        fn email(&self) -> Option<&str> { self.email.as_deref() }
        fn name(&self) -> Option<&str> { self.name.as_deref() }
        fn email_verified(&self) -> bool { self.email_verified }
        fn image(&self) -> Option<&str> { self.image.as_deref() }
        fn created_at(&self) -> chrono::DateTime<chrono::Utc> { self.created_at }
        fn updated_at(&self) -> chrono::DateTime<chrono::Utc> { self.updated_at }
        fn username(&self) -> Option<&str> { self.username.as_deref() }
        fn display_username(&self) -> Option<&str> { self.display_username.as_deref() }
        fn two_factor_enabled(&self) -> bool { self.two_factor_enabled }
        fn role(&self) -> Option<&str> { self.role.as_deref() }
        fn banned(&self) -> bool { self.banned }
        fn ban_reason(&self) -> Option<&str> { self.ban_reason.as_deref() }
        fn ban_expires(&self) -> Option<chrono::DateTime<chrono::Utc>> { self.ban_expires }
        fn metadata(&self) -> &serde_json::Value { &self.metadata }
    }

    impl better_auth_core::entity::AuthUserMeta for Model {
        fn table() -> &'static str { "auth_users" }
    }
}

pub mod auth_session {
    use sea_orm::entity::prelude::*;
    use serde::{Deserialize, Serialize};

    #[derive(Clone, Debug, PartialEq, DeriveEntityModel, Serialize, Deserialize)]
    #[sea_orm(table_name = "auth_sessions")]
    pub struct Model {
        #[sea_orm(primary_key, auto_increment = false)]
        pub id: String,
        pub expires_at: chrono::DateTime<chrono::Utc>,
        pub token: String,
        pub created_at: chrono::DateTime<chrono::Utc>,
        pub updated_at: chrono::DateTime<chrono::Utc>,
        pub ip_address: Option<String>,
        pub user_agent: Option<String>,
        pub user_id: String,
        pub impersonated_by: Option<String>,
        pub active_organization_id: Option<String>,
        pub active: bool,
    }

    #[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
    pub enum Relation {}

    impl ActiveModelBehavior for ActiveModel {}

    impl better_auth_core::entity::AuthSession for Model {
        fn id(&self) -> &str { &self.id }
        fn expires_at(&self) -> chrono::DateTime<chrono::Utc> { self.expires_at }
        fn token(&self) -> &str { &self.token }
        fn created_at(&self) -> chrono::DateTime<chrono::Utc> { self.created_at }
        fn updated_at(&self) -> chrono::DateTime<chrono::Utc> { self.updated_at }
        fn ip_address(&self) -> Option<&str> { self.ip_address.as_deref() }
        fn user_agent(&self) -> Option<&str> { self.user_agent.as_deref() }
        fn user_id(&self) -> &str { &self.user_id }
        fn impersonated_by(&self) -> Option<&str> { self.impersonated_by.as_deref() }
        fn active_organization_id(&self) -> Option<&str> { self.active_organization_id.as_deref() }
        fn active(&self) -> bool { self.active }
    }

    impl better_auth_core::entity::AuthSessionMeta for Model {
        fn table() -> &'static str { "auth_sessions" }
    }
}

pub mod auth_account {
    use sea_orm::entity::prelude::*;
    use serde::{Deserialize, Serialize};

    #[derive(Clone, Debug, PartialEq, DeriveEntityModel, Serialize, Deserialize)]
    #[sea_orm(table_name = "auth_accounts")]
    pub struct Model {
        #[sea_orm(primary_key, auto_increment = false)]
        pub id: String,
        pub account_id: String,
        pub provider_id: String,
        pub user_id: String,
        pub access_token: Option<String>,
        pub refresh_token: Option<String>,
        pub id_token: Option<String>,
        pub access_token_expires_at: Option<chrono::DateTime<chrono::Utc>>,
        pub refresh_token_expires_at: Option<chrono::DateTime<chrono::Utc>>,
        pub scope: Option<String>,
        pub password: Option<String>,
        pub created_at: chrono::DateTime<chrono::Utc>,
        pub updated_at: chrono::DateTime<chrono::Utc>,
    }

    #[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
    pub enum Relation {}

    impl ActiveModelBehavior for ActiveModel {}

    impl better_auth_core::entity::AuthAccount for Model {
        fn id(&self) -> &str { &self.id }
        fn account_id(&self) -> &str { &self.account_id }
        fn provider_id(&self) -> &str { &self.provider_id }
        fn user_id(&self) -> &str { &self.user_id }
        fn access_token(&self) -> Option<&str> { self.access_token.as_deref() }
        fn refresh_token(&self) -> Option<&str> { self.refresh_token.as_deref() }
        fn id_token(&self) -> Option<&str> { self.id_token.as_deref() }
        fn access_token_expires_at(&self) -> Option<chrono::DateTime<chrono::Utc>> { self.access_token_expires_at }
        fn refresh_token_expires_at(&self) -> Option<chrono::DateTime<chrono::Utc>> { self.refresh_token_expires_at }
        fn scope(&self) -> Option<&str> { self.scope.as_deref() }
        fn password(&self) -> Option<&str> { self.password.as_deref() }
        fn created_at(&self) -> chrono::DateTime<chrono::Utc> { self.created_at }
        fn updated_at(&self) -> chrono::DateTime<chrono::Utc> { self.updated_at }
    }

    impl better_auth_core::entity::AuthAccountMeta for Model {
        fn table() -> &'static str { "auth_accounts" }
    }
}

pub mod auth_organization {
    use sea_orm::entity::prelude::*;
    use serde::{Deserialize, Serialize};

    #[derive(Clone, Debug, PartialEq, DeriveEntityModel, Serialize, Deserialize)]
    #[sea_orm(table_name = "auth_organizations")]
    pub struct Model {
        #[sea_orm(primary_key, auto_increment = false)]
        pub id: String,
        pub name: String,
        pub slug: String,
        pub logo: Option<String>,
        #[sea_orm(column_type = "Json", nullable)]
        pub metadata: Option<serde_json::Value>,
        pub created_at: chrono::DateTime<chrono::Utc>,
        pub updated_at: chrono::DateTime<chrono::Utc>,
    }

    #[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
    pub enum Relation {}

    impl ActiveModelBehavior for ActiveModel {}

    impl better_auth_core::entity::AuthOrganization for Model {
        fn id(&self) -> &str { &self.id }
        fn name(&self) -> &str { &self.name }
        fn slug(&self) -> &str { &self.slug }
        fn logo(&self) -> Option<&str> { self.logo.as_deref() }
        fn metadata(&self) -> Option<&serde_json::Value> { self.metadata.as_ref() }
        fn created_at(&self) -> chrono::DateTime<chrono::Utc> { self.created_at }
        fn updated_at(&self) -> chrono::DateTime<chrono::Utc> { self.updated_at }
    }

    impl better_auth_core::entity::AuthOrganizationMeta for Model {
        fn table() -> &'static str { "auth_organizations" }
    }
}

pub mod auth_member {
    use sea_orm::entity::prelude::*;
    use serde::{Deserialize, Serialize};

    #[derive(Clone, Debug, PartialEq, DeriveEntityModel, Serialize, Deserialize)]
    #[sea_orm(table_name = "auth_members")]
    pub struct Model {
        #[sea_orm(primary_key, auto_increment = false)]
        pub id: String,
        pub organization_id: String,
        pub user_id: String,
        pub role: String,
        pub created_at: chrono::DateTime<chrono::Utc>,
    }

    #[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
    pub enum Relation {}

    impl ActiveModelBehavior for ActiveModel {}

    impl better_auth_core::entity::AuthMember for Model {
        fn id(&self) -> &str { &self.id }
        fn organization_id(&self) -> &str { &self.organization_id }
        fn user_id(&self) -> &str { &self.user_id }
        fn role(&self) -> &str { &self.role }
        fn created_at(&self) -> chrono::DateTime<chrono::Utc> { self.created_at }
    }

    impl better_auth_core::entity::AuthMemberMeta for Model {
        fn table() -> &'static str { "auth_members" }
    }
}

pub mod auth_invitation {
    use sea_orm::entity::prelude::*;
    use serde::{Deserialize, Serialize};

    #[derive(Clone, Debug, PartialEq, DeriveEntityModel, Serialize, Deserialize)]
    #[sea_orm(table_name = "auth_invitations")]
    pub struct Model {
        #[sea_orm(primary_key, auto_increment = false)]
        pub id: String,
        pub organization_id: String,
        pub email: String,
        pub role: String,
        pub status: String,
        pub inviter_id: String,
        pub expires_at: chrono::DateTime<chrono::Utc>,
        pub created_at: chrono::DateTime<chrono::Utc>,
    }

    #[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
    pub enum Relation {}

    impl ActiveModelBehavior for ActiveModel {}

    impl Model {
        /// Parse the stored status string into an `InvitationStatus`.
        fn parsed_status(&self) -> better_auth_core::types::InvitationStatus {
            better_auth_core::types::InvitationStatus::from(self.status.clone())
        }
    }

    impl better_auth_core::entity::AuthInvitation for Model {
        fn id(&self) -> &str { &self.id }
        fn organization_id(&self) -> &str { &self.organization_id }
        fn email(&self) -> &str { &self.email }
        fn role(&self) -> &str { &self.role }
        fn status(&self) -> &better_auth_core::types::InvitationStatus {
            // The trait requires `&InvitationStatus` with lifetime tied to `&self`,
            // but we store a String in the DB. We leak a Box to produce a
            // `&'static` reference. This is safe because InvitationStatus is a
            // small enum and invitations are not created in hot loops.
            Box::leak(Box::new(self.parsed_status()))
        }
        fn inviter_id(&self) -> &str { &self.inviter_id }
        fn expires_at(&self) -> chrono::DateTime<chrono::Utc> { self.expires_at }
        fn created_at(&self) -> chrono::DateTime<chrono::Utc> { self.created_at }
    }

    impl better_auth_core::entity::AuthInvitationMeta for Model {
        fn table() -> &'static str { "auth_invitations" }
    }
}

pub mod auth_verification {
    use sea_orm::entity::prelude::*;
    use serde::{Deserialize, Serialize};

    #[derive(Clone, Debug, PartialEq, DeriveEntityModel, Serialize, Deserialize)]
    #[sea_orm(table_name = "auth_verifications")]
    pub struct Model {
        #[sea_orm(primary_key, auto_increment = false)]
        pub id: String,
        pub identifier: String,
        pub value: String,
        pub expires_at: chrono::DateTime<chrono::Utc>,
        pub created_at: chrono::DateTime<chrono::Utc>,
        pub updated_at: chrono::DateTime<chrono::Utc>,
    }

    #[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
    pub enum Relation {}

    impl ActiveModelBehavior for ActiveModel {}

    impl better_auth_core::entity::AuthVerification for Model {
        fn id(&self) -> &str { &self.id }
        fn identifier(&self) -> &str { &self.identifier }
        fn value(&self) -> &str { &self.value }
        fn expires_at(&self) -> chrono::DateTime<chrono::Utc> { self.expires_at }
        fn created_at(&self) -> chrono::DateTime<chrono::Utc> { self.created_at }
        fn updated_at(&self) -> chrono::DateTime<chrono::Utc> { self.updated_at }
    }

    impl better_auth_core::entity::AuthVerificationMeta for Model {
        fn table() -> &'static str { "auth_verifications" }
    }
}

pub mod auth_two_factor {
    use sea_orm::entity::prelude::*;
    use serde::{Deserialize, Serialize};

    #[derive(Clone, Debug, PartialEq, DeriveEntityModel, Serialize, Deserialize)]
    #[sea_orm(table_name = "auth_two_factor")]
    pub struct Model {
        #[sea_orm(primary_key, auto_increment = false)]
        pub id: String,
        pub secret: String,
        pub backup_codes: Option<String>,
        pub user_id: String,
        pub created_at: chrono::DateTime<chrono::Utc>,
        pub updated_at: chrono::DateTime<chrono::Utc>,
    }

    #[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
    pub enum Relation {}

    impl ActiveModelBehavior for ActiveModel {}

    impl better_auth_core::entity::AuthTwoFactor for Model {
        fn id(&self) -> &str { &self.id }
        fn secret(&self) -> &str { &self.secret }
        fn backup_codes(&self) -> Option<&str> { self.backup_codes.as_deref() }
        fn user_id(&self) -> &str { &self.user_id }
        fn created_at(&self) -> chrono::DateTime<chrono::Utc> { self.created_at }
        fn updated_at(&self) -> chrono::DateTime<chrono::Utc> { self.updated_at }
    }

    impl better_auth_core::entity::AuthTwoFactorMeta for Model {
        fn table() -> &'static str { "auth_two_factor" }
    }
}

pub mod auth_api_key {
    use sea_orm::entity::prelude::*;
    use serde::{Deserialize, Serialize};

    #[derive(Clone, Debug, PartialEq, DeriveEntityModel, Serialize, Deserialize)]
    #[sea_orm(table_name = "auth_api_keys")]
    pub struct Model {
        #[sea_orm(primary_key, auto_increment = false)]
        pub id: String,
        pub name: Option<String>,
        pub start: Option<String>,
        pub prefix: Option<String>,
        #[sea_orm(column_name = "key")]
        pub key_hash: String,
        pub user_id: String,
        pub refill_interval: Option<i64>,
        pub refill_amount: Option<i64>,
        pub last_refill_at: Option<String>,
        pub enabled: bool,
        pub rate_limit_enabled: bool,
        pub rate_limit_time_window: Option<i64>,
        pub rate_limit_max: Option<i64>,
        pub request_count: Option<i64>,
        pub remaining: Option<i64>,
        pub last_request: Option<String>,
        pub expires_at: Option<String>,
        pub created_at: String,
        pub updated_at: String,
        pub permissions: Option<String>,
        pub metadata: Option<String>,
    }

    #[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
    pub enum Relation {}

    impl ActiveModelBehavior for ActiveModel {}

    impl better_auth_core::entity::AuthApiKey for Model {
        fn id(&self) -> &str { &self.id }
        fn name(&self) -> Option<&str> { self.name.as_deref() }
        fn start(&self) -> Option<&str> { self.start.as_deref() }
        fn prefix(&self) -> Option<&str> { self.prefix.as_deref() }
        fn key_hash(&self) -> &str { &self.key_hash }
        fn user_id(&self) -> &str { &self.user_id }
        fn refill_interval(&self) -> Option<i64> { self.refill_interval }
        fn refill_amount(&self) -> Option<i64> { self.refill_amount }
        fn last_refill_at(&self) -> Option<&str> { self.last_refill_at.as_deref() }
        fn enabled(&self) -> bool { self.enabled }
        fn rate_limit_enabled(&self) -> bool { self.rate_limit_enabled }
        fn rate_limit_time_window(&self) -> Option<i64> { self.rate_limit_time_window }
        fn rate_limit_max(&self) -> Option<i64> { self.rate_limit_max }
        fn request_count(&self) -> Option<i64> { self.request_count }
        fn remaining(&self) -> Option<i64> { self.remaining }
        fn last_request(&self) -> Option<&str> { self.last_request.as_deref() }
        fn expires_at(&self) -> Option<&str> { self.expires_at.as_deref() }
        fn created_at(&self) -> &str { &self.created_at }
        fn updated_at(&self) -> &str { &self.updated_at }
        fn permissions(&self) -> Option<&str> { self.permissions.as_deref() }
        fn metadata(&self) -> Option<&str> { self.metadata.as_deref() }
    }

    impl better_auth_core::entity::AuthApiKeyMeta for Model {
        fn table() -> &'static str { "auth_api_keys" }
    }
}

pub mod auth_passkey {
    use sea_orm::entity::prelude::*;
    use serde::{Deserialize, Serialize};

    #[derive(Clone, Debug, PartialEq, DeriveEntityModel, Serialize, Deserialize)]
    #[sea_orm(table_name = "auth_passkeys")]
    pub struct Model {
        #[sea_orm(primary_key, auto_increment = false)]
        pub id: String,
        pub name: String,
        pub public_key: String,
        pub user_id: String,
        pub credential_id: String,
        pub counter: i64,
        pub device_type: String,
        pub backed_up: bool,
        pub transports: Option<String>,
        pub created_at: chrono::DateTime<chrono::Utc>,
    }

    #[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
    pub enum Relation {}

    impl ActiveModelBehavior for ActiveModel {}

    impl better_auth_core::entity::AuthPasskey for Model {
        fn id(&self) -> &str { &self.id }
        fn name(&self) -> &str { &self.name }
        fn public_key(&self) -> &str { &self.public_key }
        fn user_id(&self) -> &str { &self.user_id }
        fn credential_id(&self) -> &str { &self.credential_id }
        fn counter(&self) -> u64 { self.counter as u64 }
        fn device_type(&self) -> &str { &self.device_type }
        fn backed_up(&self) -> bool { self.backed_up }
        fn transports(&self) -> Option<&str> { self.transports.as_deref() }
        fn created_at(&self) -> chrono::DateTime<chrono::Utc> { self.created_at }
    }

    impl better_auth_core::entity::AuthPasskeyMeta for Model {
        fn table() -> &'static str { "auth_passkeys" }
    }
}
