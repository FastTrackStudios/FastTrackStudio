//! Kanban boards. Mostly Hermes territory — Codex doesn't
//! implement this.

use crate::error::AgentError;
use crate::kanban::{Board, BoardFilter, BoardView, Card, CardComment, CardLink};

#[architect::rpc]
pub trait Kanban {
    fn list_boards(&self) -> Result<Vec<Board>, AgentError>;
    fn upsert_board(&self, board: Board) -> Result<Board, AgentError>;
    fn remove_board(&self, board_id: &str) -> Result<(), AgentError>;
    fn read_board(&self, board_id: &str, filter: BoardFilter) -> Result<BoardView, AgentError>;

    fn upsert_card(&self, card: Card) -> Result<Card, AgentError>;
    fn remove_card(&self, card_id: &str) -> Result<(), AgentError>;
    fn read_card(&self, card_id: &str) -> Result<Card, AgentError>;
    fn claim_card(&self, card_id: &str, handle: &str) -> Result<Card, AgentError>;
    fn move_card(&self, card_id: &str, new_column: &str) -> Result<Card, AgentError>;
    fn link_card_to_session(&self, card_id: &str, session_id: &str) -> Result<Card, AgentError>;

    fn add_card_link(&self, parent_id: &str, child_id: &str) -> Result<CardLink, AgentError>;
    fn remove_card_link(&self, parent_id: &str, child_id: &str) -> Result<(), AgentError>;
    fn list_card_links(&self, board_id: &str) -> Result<Vec<CardLink>, AgentError>;

    fn add_card_comment(
        &self,
        card_id: &str,
        author: &str,
        text: &str,
    ) -> Result<CardComment, AgentError>;
    fn list_card_comments(&self, card_id: &str) -> Result<Vec<CardComment>, AgentError>;
}
