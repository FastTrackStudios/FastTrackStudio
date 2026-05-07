#![allow(clippy::needless_lifetimes, clippy::manual_strip)]
#![allow(unused_imports)]

use crate::*;

#[derive(Subcommand)]
pub(crate) enum OperatingCommands {
    /// Show the derived life/business operating model
    Model {
        #[arg(long)]
        json: bool,
    },
}
