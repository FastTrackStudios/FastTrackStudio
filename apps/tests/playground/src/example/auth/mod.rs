//! Auth feature — complete vertical slice
//!
//! Proto (types + service trait) → Live (better-auth) → Control (typestate) → UI

pub mod control;
pub mod live;
pub mod proto;
pub mod ui;
