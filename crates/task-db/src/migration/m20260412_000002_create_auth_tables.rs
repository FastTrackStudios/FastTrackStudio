use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        // ── Auth Users ──────────────────────────────────────────────
        manager
            .create_table(
                Table::create()
                    .table(AuthUsers::Table)
                    .if_not_exists()
                    .col(ColumnDef::new(AuthUsers::Id).string().not_null().primary_key())
                    .col(ColumnDef::new(AuthUsers::Email).string().unique_key())
                    .col(ColumnDef::new(AuthUsers::Name).string())
                    .col(ColumnDef::new(AuthUsers::Image).string())
                    .col(ColumnDef::new(AuthUsers::EmailVerified).boolean().not_null().default(false))
                    .col(ColumnDef::new(AuthUsers::CreatedAt).timestamp_with_time_zone().not_null())
                    .col(ColumnDef::new(AuthUsers::UpdatedAt).timestamp_with_time_zone().not_null())
                    .col(ColumnDef::new(AuthUsers::Metadata).json().not_null().default("{}"))
                    .col(ColumnDef::new(AuthUsers::Username).string().unique_key())
                    .col(ColumnDef::new(AuthUsers::DisplayUsername).string())
                    .col(ColumnDef::new(AuthUsers::TwoFactorEnabled).boolean().not_null().default(false))
                    .col(ColumnDef::new(AuthUsers::Role).string())
                    .col(ColumnDef::new(AuthUsers::Banned).boolean().not_null().default(false))
                    .col(ColumnDef::new(AuthUsers::BanReason).string())
                    .col(ColumnDef::new(AuthUsers::BanExpires).timestamp_with_time_zone())
                    .to_owned(),
            )
            .await?;

        // ── Auth Sessions ───────────────────────────────────────────
        manager
            .create_table(
                Table::create()
                    .table(AuthSessions::Table)
                    .if_not_exists()
                    .col(ColumnDef::new(AuthSessions::Id).string().not_null().primary_key())
                    .col(ColumnDef::new(AuthSessions::ExpiresAt).timestamp_with_time_zone().not_null())
                    .col(ColumnDef::new(AuthSessions::Token).string().not_null().unique_key())
                    .col(ColumnDef::new(AuthSessions::CreatedAt).timestamp_with_time_zone().not_null())
                    .col(ColumnDef::new(AuthSessions::UpdatedAt).timestamp_with_time_zone().not_null())
                    .col(ColumnDef::new(AuthSessions::IpAddress).string())
                    .col(ColumnDef::new(AuthSessions::UserAgent).string())
                    .col(ColumnDef::new(AuthSessions::UserId).string().not_null())
                    .col(ColumnDef::new(AuthSessions::ImpersonatedBy).string())
                    .col(ColumnDef::new(AuthSessions::ActiveOrganizationId).string())
                    .col(ColumnDef::new(AuthSessions::Active).boolean().not_null().default(true))
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_auth_sessions_user_id")
                    .table(AuthSessions::Table)
                    .col(AuthSessions::UserId)
                    .to_owned(),
            )
            .await?;

        // ── Auth Accounts ───────────────────────────────────────────
        manager
            .create_table(
                Table::create()
                    .table(AuthAccounts::Table)
                    .if_not_exists()
                    .col(ColumnDef::new(AuthAccounts::Id).string().not_null().primary_key())
                    .col(ColumnDef::new(AuthAccounts::AccountId).string().not_null())
                    .col(ColumnDef::new(AuthAccounts::ProviderId).string().not_null())
                    .col(ColumnDef::new(AuthAccounts::UserId).string().not_null())
                    .col(ColumnDef::new(AuthAccounts::AccessToken).string())
                    .col(ColumnDef::new(AuthAccounts::RefreshToken).string())
                    .col(ColumnDef::new(AuthAccounts::IdToken).string())
                    .col(ColumnDef::new(AuthAccounts::AccessTokenExpiresAt).timestamp_with_time_zone())
                    .col(ColumnDef::new(AuthAccounts::RefreshTokenExpiresAt).timestamp_with_time_zone())
                    .col(ColumnDef::new(AuthAccounts::Scope).string())
                    .col(ColumnDef::new(AuthAccounts::Password).string())
                    .col(ColumnDef::new(AuthAccounts::CreatedAt).timestamp_with_time_zone().not_null())
                    .col(ColumnDef::new(AuthAccounts::UpdatedAt).timestamp_with_time_zone().not_null())
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_auth_accounts_provider_account")
                    .table(AuthAccounts::Table)
                    .col(AuthAccounts::ProviderId)
                    .col(AuthAccounts::AccountId)
                    .unique()
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_auth_accounts_user_id")
                    .table(AuthAccounts::Table)
                    .col(AuthAccounts::UserId)
                    .to_owned(),
            )
            .await?;

        // ── Auth Organizations ──────────────────────────────────────
        manager
            .create_table(
                Table::create()
                    .table(AuthOrganizations::Table)
                    .if_not_exists()
                    .col(ColumnDef::new(AuthOrganizations::Id).string().not_null().primary_key())
                    .col(ColumnDef::new(AuthOrganizations::Name).string().not_null())
                    .col(ColumnDef::new(AuthOrganizations::Slug).string().not_null().unique_key())
                    .col(ColumnDef::new(AuthOrganizations::Logo).string())
                    .col(ColumnDef::new(AuthOrganizations::Metadata).json())
                    .col(ColumnDef::new(AuthOrganizations::CreatedAt).timestamp_with_time_zone().not_null())
                    .col(ColumnDef::new(AuthOrganizations::UpdatedAt).timestamp_with_time_zone().not_null())
                    .to_owned(),
            )
            .await?;

        // ── Auth Members ────────────────────────────────────────────
        manager
            .create_table(
                Table::create()
                    .table(AuthMembers::Table)
                    .if_not_exists()
                    .col(ColumnDef::new(AuthMembers::Id).string().not_null().primary_key())
                    .col(ColumnDef::new(AuthMembers::OrganizationId).string().not_null())
                    .col(ColumnDef::new(AuthMembers::UserId).string().not_null())
                    .col(ColumnDef::new(AuthMembers::Role).string().not_null().default("member"))
                    .col(ColumnDef::new(AuthMembers::CreatedAt).timestamp_with_time_zone().not_null())
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_auth_members_org_user")
                    .table(AuthMembers::Table)
                    .col(AuthMembers::OrganizationId)
                    .col(AuthMembers::UserId)
                    .unique()
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_auth_members_organization_id")
                    .table(AuthMembers::Table)
                    .col(AuthMembers::OrganizationId)
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_auth_members_user_id")
                    .table(AuthMembers::Table)
                    .col(AuthMembers::UserId)
                    .to_owned(),
            )
            .await?;

        // ── Auth Invitations ────────────────────────────────────────
        manager
            .create_table(
                Table::create()
                    .table(AuthInvitations::Table)
                    .if_not_exists()
                    .col(ColumnDef::new(AuthInvitations::Id).string().not_null().primary_key())
                    .col(ColumnDef::new(AuthInvitations::OrganizationId).string().not_null())
                    .col(ColumnDef::new(AuthInvitations::Email).string().not_null())
                    .col(ColumnDef::new(AuthInvitations::Role).string().not_null().default("member"))
                    .col(ColumnDef::new(AuthInvitations::Status).string().not_null().default("pending"))
                    .col(ColumnDef::new(AuthInvitations::InviterId).string().not_null())
                    .col(ColumnDef::new(AuthInvitations::ExpiresAt).timestamp_with_time_zone().not_null())
                    .col(ColumnDef::new(AuthInvitations::CreatedAt).timestamp_with_time_zone().not_null())
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_auth_invitations_organization_id")
                    .table(AuthInvitations::Table)
                    .col(AuthInvitations::OrganizationId)
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_auth_invitations_email")
                    .table(AuthInvitations::Table)
                    .col(AuthInvitations::Email)
                    .to_owned(),
            )
            .await?;

        // ── Auth Verifications ──────────────────────────────────────
        manager
            .create_table(
                Table::create()
                    .table(AuthVerifications::Table)
                    .if_not_exists()
                    .col(ColumnDef::new(AuthVerifications::Id).string().not_null().primary_key())
                    .col(ColumnDef::new(AuthVerifications::Identifier).string().not_null())
                    .col(ColumnDef::new(AuthVerifications::Value).string().not_null())
                    .col(ColumnDef::new(AuthVerifications::ExpiresAt).timestamp_with_time_zone().not_null())
                    .col(ColumnDef::new(AuthVerifications::CreatedAt).timestamp_with_time_zone().not_null())
                    .col(ColumnDef::new(AuthVerifications::UpdatedAt).timestamp_with_time_zone().not_null())
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_auth_verifications_identifier_value")
                    .table(AuthVerifications::Table)
                    .col(AuthVerifications::Identifier)
                    .col(AuthVerifications::Value)
                    .to_owned(),
            )
            .await?;

        // ── Auth Two Factor ─────────────────────────────────────────
        manager
            .create_table(
                Table::create()
                    .table(AuthTwoFactor::Table)
                    .if_not_exists()
                    .col(ColumnDef::new(AuthTwoFactor::Id).string().not_null().primary_key())
                    .col(ColumnDef::new(AuthTwoFactor::Secret).string().not_null())
                    .col(ColumnDef::new(AuthTwoFactor::BackupCodes).string())
                    .col(ColumnDef::new(AuthTwoFactor::UserId).string().not_null().unique_key())
                    .col(ColumnDef::new(AuthTwoFactor::CreatedAt).timestamp_with_time_zone().not_null())
                    .col(ColumnDef::new(AuthTwoFactor::UpdatedAt).timestamp_with_time_zone().not_null())
                    .to_owned(),
            )
            .await?;

        // ── Auth API Keys ───────────────────────────────────────────
        manager
            .create_table(
                Table::create()
                    .table(AuthApiKeys::Table)
                    .if_not_exists()
                    .col(ColumnDef::new(AuthApiKeys::Id).string().not_null().primary_key())
                    .col(ColumnDef::new(AuthApiKeys::Name).string())
                    .col(ColumnDef::new(AuthApiKeys::Start).string())
                    .col(ColumnDef::new(AuthApiKeys::Prefix).string())
                    .col(ColumnDef::new(AuthApiKeys::Key).string().not_null())
                    .col(ColumnDef::new(AuthApiKeys::UserId).string().not_null())
                    .col(ColumnDef::new(AuthApiKeys::RefillInterval).big_integer())
                    .col(ColumnDef::new(AuthApiKeys::RefillAmount).big_integer())
                    .col(ColumnDef::new(AuthApiKeys::LastRefillAt).string())
                    .col(ColumnDef::new(AuthApiKeys::Enabled).boolean().not_null().default(true))
                    .col(ColumnDef::new(AuthApiKeys::RateLimitEnabled).boolean().not_null().default(false))
                    .col(ColumnDef::new(AuthApiKeys::RateLimitTimeWindow).big_integer())
                    .col(ColumnDef::new(AuthApiKeys::RateLimitMax).big_integer())
                    .col(ColumnDef::new(AuthApiKeys::RequestCount).big_integer())
                    .col(ColumnDef::new(AuthApiKeys::Remaining).big_integer())
                    .col(ColumnDef::new(AuthApiKeys::LastRequest).string())
                    .col(ColumnDef::new(AuthApiKeys::ExpiresAt).string())
                    .col(ColumnDef::new(AuthApiKeys::CreatedAt).string().not_null())
                    .col(ColumnDef::new(AuthApiKeys::UpdatedAt).string().not_null())
                    .col(ColumnDef::new(AuthApiKeys::Permissions).string())
                    .col(ColumnDef::new(AuthApiKeys::Metadata).string())
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_auth_api_keys_user_id")
                    .table(AuthApiKeys::Table)
                    .col(AuthApiKeys::UserId)
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_auth_api_keys_key")
                    .table(AuthApiKeys::Table)
                    .col(AuthApiKeys::Key)
                    .unique()
                    .to_owned(),
            )
            .await?;

        // ── Auth Passkeys ───────────────────────────────────────────
        manager
            .create_table(
                Table::create()
                    .table(AuthPasskeys::Table)
                    .if_not_exists()
                    .col(ColumnDef::new(AuthPasskeys::Id).string().not_null().primary_key())
                    .col(ColumnDef::new(AuthPasskeys::Name).string().not_null())
                    .col(ColumnDef::new(AuthPasskeys::PublicKey).string().not_null())
                    .col(ColumnDef::new(AuthPasskeys::UserId).string().not_null())
                    .col(ColumnDef::new(AuthPasskeys::CredentialId).string().not_null().unique_key())
                    .col(ColumnDef::new(AuthPasskeys::Counter).big_integer().not_null().default(0))
                    .col(ColumnDef::new(AuthPasskeys::DeviceType).string().not_null())
                    .col(ColumnDef::new(AuthPasskeys::BackedUp).boolean().not_null().default(false))
                    .col(ColumnDef::new(AuthPasskeys::Transports).string())
                    .col(ColumnDef::new(AuthPasskeys::CreatedAt).timestamp_with_time_zone().not_null())
                    .to_owned(),
            )
            .await?;

        Ok(())
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager.drop_table(Table::drop().table(AuthPasskeys::Table).to_owned()).await?;
        manager.drop_table(Table::drop().table(AuthApiKeys::Table).to_owned()).await?;
        manager.drop_table(Table::drop().table(AuthTwoFactor::Table).to_owned()).await?;
        manager.drop_table(Table::drop().table(AuthVerifications::Table).to_owned()).await?;
        manager.drop_table(Table::drop().table(AuthInvitations::Table).to_owned()).await?;
        manager.drop_table(Table::drop().table(AuthMembers::Table).to_owned()).await?;
        manager.drop_table(Table::drop().table(AuthOrganizations::Table).to_owned()).await?;
        manager.drop_table(Table::drop().table(AuthAccounts::Table).to_owned()).await?;
        manager.drop_table(Table::drop().table(AuthSessions::Table).to_owned()).await?;
        manager.drop_table(Table::drop().table(AuthUsers::Table).to_owned()).await?;
        Ok(())
    }
}

// ── Table identifiers ───────────────────────────────────────────────────────

#[derive(DeriveIden)]
enum AuthUsers {
    Table, Id, Email, Name, Image, EmailVerified, CreatedAt, UpdatedAt,
    Metadata, Username, DisplayUsername, TwoFactorEnabled, Role, Banned,
    BanReason, BanExpires,
}

#[derive(DeriveIden)]
enum AuthSessions {
    Table, Id, ExpiresAt, Token, CreatedAt, UpdatedAt, IpAddress,
    UserAgent, UserId, ImpersonatedBy, ActiveOrganizationId, Active,
}

#[derive(DeriveIden)]
enum AuthAccounts {
    Table, Id, AccountId, ProviderId, UserId, AccessToken, RefreshToken,
    IdToken, AccessTokenExpiresAt, RefreshTokenExpiresAt, Scope, Password,
    CreatedAt, UpdatedAt,
}

#[derive(DeriveIden)]
enum AuthOrganizations {
    Table, Id, Name, Slug, Logo, Metadata, CreatedAt, UpdatedAt,
}

#[derive(DeriveIden)]
enum AuthMembers {
    Table, Id, OrganizationId, UserId, Role, CreatedAt,
}

#[derive(DeriveIden)]
enum AuthInvitations {
    Table, Id, OrganizationId, Email, Role, Status, InviterId, ExpiresAt,
    CreatedAt,
}

#[derive(DeriveIden)]
enum AuthVerifications {
    Table, Id, Identifier, Value, ExpiresAt, CreatedAt, UpdatedAt,
}

#[derive(DeriveIden)]
enum AuthTwoFactor {
    Table, Id, Secret, BackupCodes, UserId, CreatedAt, UpdatedAt,
}

#[derive(DeriveIden)]
enum AuthApiKeys {
    Table, Id, Name, Start, Prefix, Key, UserId, RefillInterval,
    RefillAmount, LastRefillAt, Enabled, RateLimitEnabled,
    RateLimitTimeWindow, RateLimitMax, RequestCount, Remaining,
    LastRequest, ExpiresAt, CreatedAt, UpdatedAt, Permissions, Metadata,
}

#[derive(DeriveIden)]
enum AuthPasskeys {
    Table, Id, Name, PublicKey, UserId, CredentialId, Counter, DeviceType,
    BackedUp, Transports, CreatedAt,
}
