use async_trait::async_trait;
use facet::Facet;

use crate::service::VaultError;

#[derive(Debug, Clone, PartialEq, Eq, Default, Facet)]
pub struct ChannelConversation {
    pub provider: String,
    pub account: Option<String>,
    pub id: String,
    pub name: String,
    pub kind: String,
    pub participant_count: u32,
    pub last_activity: Option<i64>,
    pub last_message: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Facet)]
pub struct ChannelMessage {
    pub provider: String,
    pub account: Option<String>,
    pub conversation_id: String,
    pub id: String,
    pub actor_id: String,
    pub actor_type: String,
    pub actor_display_name: String,
    pub timestamp: i64,
    pub body: String,
    pub reply_to: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Facet)]
pub struct ChannelSendMessageRequest {
    pub conversation_id: String,
    pub body: String,
    pub reply_to: Option<String>,
}

#[async_trait]
pub trait CommunicationChannelProvider: Send + Sync {
    fn provider_id(&self) -> &'static str;

    fn account_id(&self) -> Option<&str> {
        None
    }

    async fn list_conversations(&self) -> Result<Vec<ChannelConversation>, VaultError>;

    async fn recent_messages(
        &self,
        conversation_id: &str,
        limit: u32,
    ) -> Result<Vec<ChannelMessage>, VaultError>;

    async fn send_message(
        &self,
        request: ChannelSendMessageRequest,
    ) -> Result<ChannelMessage, VaultError>;
}
