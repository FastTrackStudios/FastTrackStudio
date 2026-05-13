//! AI-chat reusable components — companion to the human-chat
//! `MessageRow`/`MessageList` family above. All dumb: data + callbacks
//! in, RSX out.
//!
//! Layout mirrors `agent-ui::hermes_kit` and `timer-ui::solidtime`:
//! one submodule per visual unit + a `common.rs` for shared helpers.

pub mod actions;
pub mod bubble;
pub mod common;
pub mod composer;
pub mod markdown;
pub mod model_picker;
pub mod reasoning;
pub mod sidebar;
pub mod streaming;
pub mod tool_call;

pub use actions::{MessageAction, MessageActions, MessageActionsProps};
pub use bubble::{MessageBubble, MessageBubbleProps};
pub use common::{
    ConvGroup, format_cost, format_token_count, group_conversations_by_day, relative_time,
    role_avatar_letter, role_label,
};
pub use composer::{MessageComposer, MessageComposerProps, SendIntent};
pub use markdown::Markdown;
pub use model_picker::{ModelPicker, ModelPickerProps};
pub use reasoning::{ReasoningPanel, ReasoningPanelProps};
pub use sidebar::{ConversationSidebar, ConversationSidebarProps};
pub use streaming::StreamingIndicator;
pub use tool_call::{ToolCallCard, ToolCallStatus, ToolCallView, parse_tool_calls};
