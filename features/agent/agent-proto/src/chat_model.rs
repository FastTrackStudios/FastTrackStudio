//! `ChatModel` — provider-neutral trait for streaming chat
//! completions. Phase A scaffolding: real implementations
//! (Anthropic, OpenAI, Ollama, mock) live in sibling crates and
//! register themselves with `ChatModelRegistry` at server boot.
//!
//! v1 limitation: tool execution is plumbed through `ToolSchema` +
//! `ChatStreamChunk::ToolCall`/`ToolResult` but the actual tool
//! dispatcher is out of scope here — the server wires that up.

use async_trait::async_trait;
use futures_core::stream::BoxStream;

use crate::ModelInfo;
use crate::integration::IntegrationError;

#[async_trait]
pub trait ChatModel: Send + Sync + 'static {
    /// Stable plugin key (e.g. "anthropic", "ollama", "mock"). Used
    /// as the `ChatModelRegistry` index.
    fn provider(&self) -> &'static str;

    /// Models this provider can serve. Used by the registry to route
    /// a `model_id` to a provider via `for_model`.
    fn models(&self) -> Vec<ModelInfo>;

    async fn complete_stream(
        &self,
        req: ChatStreamRequest,
    ) -> Result<BoxStream<'static, Result<ChatStreamChunk, IntegrationError>>, IntegrationError>;
}

#[derive(Clone, Debug)]
pub struct ChatStreamRequest {
    pub model: String,
    pub system_prompt: Option<String>,
    pub messages: Vec<ChatTurn>,
    /// 700 = 0.7. Keeping millis lets us avoid f32 round-trip pain
    /// on the wire.
    pub temperature_milli: i32,
    pub max_tokens: Option<i32>,
    pub tools: Vec<ToolSchema>,
    /// Ask the provider to emit `ChatStreamChunk::ReasoningDelta` for
    /// reasoning models. Ignored by providers that don't support it.
    pub stream_reasoning: bool,
}

#[derive(Clone, Debug)]
pub struct ChatTurn {
    /// Matches `chat_proto::MESSAGE_ROLES`.
    pub role: String,
    pub content: String,
}

#[derive(Clone, Debug)]
pub struct ToolSchema {
    pub name: String,
    pub description: String,
    /// JSON-schema document. v1 keeps this opaque — concrete plugins
    /// parse/validate as needed.
    pub parameters_json: String,
}

#[derive(Clone, Debug)]
pub enum ChatStreamChunk {
    Delta(String),
    ReasoningDelta(String),
    ToolCall {
        id: String,
        name: String,
        arguments_json: String,
    },
    ToolResult {
        id: String,
        result_json: String,
    },
    Usage {
        tokens_in: u32,
        tokens_out: u32,
        cost_cents: u32,
    },
    Done {
        /// One of `chat_proto::FINISH_REASONS`.
        finish_reason: String,
    },
}

/// Registry of available `ChatModel` implementations. The server
/// selects an implementor by `req.model` (resolved via
/// `for_model`, which probes each registered provider's `models()`).
#[derive(Default)]
pub struct ChatModelRegistry {
    providers: std::collections::HashMap<&'static str, std::sync::Arc<dyn ChatModel>>,
}

impl ChatModelRegistry {
    pub fn new() -> Self {
        Self {
            providers: Default::default(),
        }
    }

    /// Keyed by `m.provider()`. Last write wins, matching
    /// `IntegrationRegistry` semantics — handy in tests.
    pub fn register(&mut self, m: std::sync::Arc<dyn ChatModel>) {
        self.providers.insert(m.provider(), m);
    }

    pub fn get(&self, provider: &str) -> Option<std::sync::Arc<dyn ChatModel>> {
        self.providers.get(provider).cloned()
    }

    /// Probe each registered provider's `models()` for one whose
    /// `id == model_id`. Pick the first match. v1 limitation:
    /// O(providers × models) — fine for the small static catalogs
    /// plugins return.
    pub fn for_model(&self, model_id: &str) -> Option<std::sync::Arc<dyn ChatModel>> {
        for provider in self.providers.values() {
            if provider.models().iter().any(|m| m.id == model_id) {
                return Some(provider.clone());
            }
        }
        None
    }

    pub fn all_models(&self) -> Vec<ModelInfo> {
        self.providers.values().flat_map(|p| p.models()).collect()
    }

    pub fn providers(&self) -> Vec<&'static str> {
        self.providers.keys().copied().collect()
    }
}
