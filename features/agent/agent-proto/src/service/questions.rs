//! Mid-turn structured questions from the agent.

use crate::error::AgentError;
use crate::question::{QuestionAnswer, QuestionRequest};

#[architect::rpc]
pub trait Questions {
    fn list_pending_questions(&self, session_id: &str) -> Result<Vec<QuestionRequest>, AgentError>;
    fn answer_question(
        &self,
        request_id: &str,
        answers: Vec<QuestionAnswer>,
    ) -> Result<QuestionRequest, AgentError>;
}
