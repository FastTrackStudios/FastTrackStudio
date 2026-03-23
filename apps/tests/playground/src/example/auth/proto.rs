//! Auth proto — domain types and service trait

use facet::Facet;

#[derive(Debug, Clone, PartialEq, Facet)]
pub struct SessionToken {
    pub token: String,
    pub user_id: String,
    pub user_email: String,
    pub user_name: String,
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Facet)]
pub enum AuthResult {
    Success { session: SessionToken } = 0,
    Failed { message: String } = 1,
}

#[vox::service]
pub trait AuthService {
    async fn sign_up(&self, email: String, password: String, name: String) -> AuthResult;
    async fn sign_in(&self, email: String, password: String) -> AuthResult;
    async fn validate_session(&self, token: String) -> Option<SessionToken>;
    async fn sign_out(&self, token: String);
}
