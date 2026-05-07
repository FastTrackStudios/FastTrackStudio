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

/// Typed requirements for [`ConversationServiceImpl`].
#[derive(Default)]
pub struct ConversationServiceDeps {
    pub provider: Option<Arc<dyn CommunicationChannelProvider>>,
}

#[derive(Clone, Default)]
pub struct ConversationServiceImpl {
    provider: Option<Arc<dyn CommunicationChannelProvider>>,
}

impl ConversationServiceImpl {
    pub fn new(deps: ConversationServiceDeps) -> Self {
        Self {
            provider: deps.provider,
        }
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
