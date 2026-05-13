//! Whiteboard component — Logseq-style 2D canvas where Blocks live as
//! references (not copies). A whiteboard is a Page with `ext == "canvas"`;
//! its Blocks carry `CanvasNode` in `canvas_node_json`.

pub mod agent_diagram;
pub mod coords;
pub mod hit;
pub mod selection;
pub mod whiteboard;
pub mod widgets;

pub use whiteboard::{CanvasMode, Whiteboard, WhiteboardProps};
