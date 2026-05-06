//! Server-side `ConversationService` implementation.
//!
//! Every operation requires a channel provider (Nextcloud Talk, Matrix, …)
//! that is not wired into this codebase. We return a uniform
//! `VaultError::IoError("provider not configured: <op>")` so clients get
//! a clean per-operation error rather than the generic "Unknown Vox
//! service requested" 5xx.

use crate::provider::{ChannelConversation, ChannelMessage, ChannelSendMessageRequest};
use crate::service::{ConversationService, VaultError};

fn provider_not_configured(op: &str) -> VaultError {
    VaultError::IoError(format!("provider not configured: {op}"))
}

#[derive(Clone, Default)]
pub struct ConversationServiceImpl;

impl ConversationServiceImpl {
    pub fn new() -> Self {
        Self
    }
}

impl ConversationService for ConversationServiceImpl {
    async fn list_conversations(&self) -> Result<Vec<ChannelConversation>, VaultError> {
        Err(provider_not_configured("list_conversations"))
    }

    async fn recent_messages(
        &self,
        _conversation_id: String,
        _limit: u32,
    ) -> Result<Vec<ChannelMessage>, VaultError> {
        Err(provider_not_configured("recent_messages"))
    }

    async fn send_message(
        &self,
        _request: ChannelSendMessageRequest,
    ) -> Result<ChannelMessage, VaultError> {
        Err(provider_not_configured("send_message"))
    }
}
