//! Server-side `ConversationService` implementation.
//!
//! When a `CommunicationChannelProvider` (e.g. `TalkClient`) is injected,
//! every op delegates to it. With no provider configured we return a
//! uniform `VaultError::IoError("provider not configured: <op>")` so
//! clients get a clean per-operation error rather than the generic
//! "Unknown Vox service requested" 5xx.

use std::sync::Arc;

use crate::provider::{
    ChannelConversation, ChannelMessage, ChannelSendMessageRequest, CommunicationChannelProvider,
};
use crate::service::{ConversationService, VaultError};

fn provider_not_configured(op: &str) -> VaultError {
    VaultError::IoError(format!("provider not configured: {op}"))
}

#[derive(Clone, Default)]
pub struct ConversationServiceImpl {
    provider: Option<Arc<dyn CommunicationChannelProvider>>,
}

impl ConversationServiceImpl {
    /// Build the service with no provider configured.
    pub fn new() -> Self {
        Self { provider: None }
    }

    /// Build the service backed by a live channel provider (Talk, Matrix, …).
    pub fn new_with_provider(provider: Arc<dyn CommunicationChannelProvider>) -> Self {
        Self {
            provider: Some(provider),
        }
    }

    /// Build the service from a possibly-absent shared provider.
    pub fn with_shared_provider(provider: Option<Arc<dyn CommunicationChannelProvider>>) -> Self {
        Self { provider }
    }
}

impl ConversationService for ConversationServiceImpl {
    async fn list_conversations(&self) -> Result<Vec<ChannelConversation>, VaultError> {
        match &self.provider {
            Some(p) => p.list_conversations().await,
            None => Err(provider_not_configured("list_conversations")),
        }
    }

    async fn recent_messages(
        &self,
        conversation_id: String,
        limit: u32,
    ) -> Result<Vec<ChannelMessage>, VaultError> {
        match &self.provider {
            Some(p) => p.recent_messages(&conversation_id, limit).await,
            None => Err(provider_not_configured("recent_messages")),
        }
    }

    async fn send_message(
        &self,
        request: ChannelSendMessageRequest,
    ) -> Result<ChannelMessage, VaultError> {
        match &self.provider {
            Some(p) => p.send_message(request).await,
            None => Err(provider_not_configured("send_message")),
        }
    }
}
