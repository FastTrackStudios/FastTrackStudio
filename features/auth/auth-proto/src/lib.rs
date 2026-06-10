//! Wire contract for architect-auth.

pub use architect;

// r[impl auth.core.entities.single-source]
pub mod account;
pub mod api_key;
pub mod audit_event;
pub mod invitation;
pub mod member;
pub mod organization;
pub mod organization_role;
pub mod passkey;
pub mod session;
pub mod team;
pub mod team_member;
pub mod two_factor;
pub mod user;
pub mod verification;

// r[verify auth.core.entities.single-source]
pub use account::{
    AuthAccount, AuthAccountCreate, AuthAccountList, AuthAccountRepo, AuthAccountUpdate,
};
pub use api_key::{AuthApiKey, AuthApiKeyCreate, AuthApiKeyList, AuthApiKeyRepo, AuthApiKeyUpdate};
pub use audit_event::{
    AuthAuditEventRecord, AuthAuditEventRecordCreate, AuthAuditEventRecordList,
    AuthAuditEventRecordRepo, AuthAuditEventRecordUpdate,
};
pub use invitation::{
    AuthInvitation, AuthInvitationCreate, AuthInvitationList, AuthInvitationRepo,
    AuthInvitationUpdate, InvitationStatus,
};
pub use member::{AuthMember, AuthMemberCreate, AuthMemberList, AuthMemberRepo, AuthMemberUpdate};
pub use organization::{
    AuthOrganization, AuthOrganizationCreate, AuthOrganizationList, AuthOrganizationRepo,
    AuthOrganizationUpdate,
};
pub use organization_role::{
    AuthOrganizationRole, AuthOrganizationRoleCreate, AuthOrganizationRoleList,
    AuthOrganizationRoleRepo, AuthOrganizationRoleUpdate,
};
pub use passkey::{
    AuthPasskey, AuthPasskeyCreate, AuthPasskeyList, AuthPasskeyRepo, AuthPasskeyUpdate,
};
pub use session::{
    AuthSession, AuthSessionCreate, AuthSessionList, AuthSessionRepo, AuthSessionUpdate,
};
pub use team::{AuthTeam, AuthTeamCreate, AuthTeamList, AuthTeamRepo, AuthTeamUpdate};
pub use team_member::{
    AuthTeamMember, AuthTeamMemberCreate, AuthTeamMemberList, AuthTeamMemberRepo,
    AuthTeamMemberUpdate,
};
pub use two_factor::{
    AuthTwoFactor, AuthTwoFactorCreate, AuthTwoFactorList, AuthTwoFactorRepo, AuthTwoFactorUpdate,
};
pub use user::{AuthUser, AuthUserCreate, AuthUserList, AuthUserRepo, AuthUserUpdate};
pub use verification::{
    AuthVerification, AuthVerificationCreate, AuthVerificationList, AuthVerificationRepo,
    AuthVerificationUpdate,
};

// r[impl auth.core.errors-stable]
// r[verify auth.core.errors-stable]
#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum AuthFlowError {
    #[error("invalid credentials")]
    InvalidCredentials,
    #[error("session expired")]
    SessionExpired,
    #[error("verification required")]
    VerificationRequired,
    #[error("two-factor required")]
    TwoFactorRequired,
    #[error("permission denied")]
    PermissionDenied,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[derive(Clone, Debug, PartialEq, ::facet::Facet)]
pub struct SignInEmailPassword {
    pub email: String,
    pub password: String,
    pub ip_address: Option<String>,
    pub user_agent: Option<String>,
}

#[derive(Clone, Debug, PartialEq, ::facet::Facet)]
pub struct AuthSessionBundle {
    pub user: AuthUser,
    pub session: AuthSession,
    pub token: String,
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait AuthService {
    async fn sign_in_email_password(
        &self,
        input: SignInEmailPassword,
    ) -> Result<AuthSessionBundle, AuthFlowError>;

    async fn current_session(&self, token: String) -> Result<AuthSessionBundle, AuthFlowError>;

    async fn sign_out(&self, token: String) -> Result<(), AuthFlowError>;
}
