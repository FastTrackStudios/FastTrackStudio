//! Per-shape widget renderers for the canvas. Each module is one
//! shape family — drawn primitives, references, connectors, groups,
//! assets. Widgets render inside the world-transformed inner div, so
//! sizes are in world units (no zoom math needed inside).

pub mod asset;
pub mod block_ref;
pub mod connector;
pub mod group;
pub mod page_ref;
pub mod shape_pen;
pub mod shape_rect;
