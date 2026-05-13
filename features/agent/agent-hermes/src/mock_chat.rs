//! Deterministic, scripted `ChatModel` impl — server-side counterpart
//! to `MockChatModel`. Used by the demo + integration tests when no
//! real provider plugin is configured.
//!
//! Stream timeline (`mock-reasoning`):
//!
//! - if `req.stream_reasoning`, yield 3 `ReasoningDelta` chunks (~30ms apart).
//! - yield 8 `Delta` chunks (50ms apart) spelling out a response
//!   derived from the last user message.
//! - yield one `Usage{tokens_in=142, tokens_out=98, cost_cents=1}`.
//! - yield `Done{finish_reason: "stop"}`.

use std::time::Duration;

use agent_proto::{
    ModelInfo,
    chat_model::{ChatModel, ChatStreamChunk, ChatStreamRequest},
    integration::IntegrationError,
};
use async_trait::async_trait;
use futures_core::stream::BoxStream;
use futures_util::stream::StreamExt;

pub struct MockChatModel;

#[async_trait]
impl ChatModel for MockChatModel {
    fn provider(&self) -> &'static str {
        "mock"
    }

    fn models(&self) -> Vec<ModelInfo> {
        vec![
            ModelInfo {
                id: "mock-fast".into(),
                provider: "mock".into(),
                display: "Mock — fast".into(),
                reasoning: false,
                context_tokens: 32_000,
                input_cost_milli: 0,
                output_cost_milli: 0,
            },
            ModelInfo {
                id: "mock-reasoning".into(),
                provider: "mock".into(),
                display: "Mock — reasoning".into(),
                reasoning: true,
                context_tokens: 128_000,
                input_cost_milli: 0,
                output_cost_milli: 0,
            },
        ]
    }

    async fn complete_stream(
        &self,
        req: ChatStreamRequest,
    ) -> Result<BoxStream<'static, Result<ChatStreamChunk, IntegrationError>>, IntegrationError>
    {
        let last_user = req
            .messages
            .iter()
            .rev()
            .find(|m| m.role == "user")
            .map(|m| m.content.clone())
            .unwrap_or_else(|| "your request".to_string());
        let trimmed: String = last_user.chars().take(60).collect();
        let body = format!(
            "Got it. Here's how I'd approach \"{trimmed}\":\n\n\
             1. Start by mapping the surface area.\n\
             2. Identify the smallest behavioural slice.\n\
             3. Land that slice end-to-end first.\n\
             4. Loop back for the edge cases.\n\n\
             Let me know which step you'd like to dig into."
        );

        let chunk_count: usize = 8;
        let mut deltas: Vec<String> = Vec::with_capacity(chunk_count);
        let chars: Vec<char> = body.chars().collect();
        let chunk_size = chars.len().div_ceil(chunk_count).max(1);
        for piece in chars.chunks(chunk_size) {
            deltas.push(piece.iter().collect::<String>());
        }
        while deltas.len() < chunk_count {
            deltas.push(String::new());
        }

        let reasoning_pieces: Vec<&'static str> = if req.stream_reasoning
            && req.model.contains("reasoning")
        {
            vec![
                "Let me think about this step by step.\n",
                "First, I'll parse what the user actually asked for and what shape of answer fits.\n",
                "Then I'll structure a numbered plan they can act on incrementally.\n",
            ]
        } else {
            Vec::new()
        };

        let s = async_stream::stream! {
            for r in reasoning_pieces {
                tokio::time::sleep(Duration::from_millis(30)).await;
                yield Ok(ChatStreamChunk::ReasoningDelta(r.to_string()));
            }
            for d in deltas {
                tokio::time::sleep(Duration::from_millis(50)).await;
                yield Ok(ChatStreamChunk::Delta(d));
            }
            yield Ok(ChatStreamChunk::Usage {
                tokens_in: 142,
                tokens_out: 98,
                cost_cents: 1,
            });
            yield Ok(ChatStreamChunk::Done {
                finish_reason: "stop".to_string(),
            });
        };

        Ok(s.boxed())
    }
}
