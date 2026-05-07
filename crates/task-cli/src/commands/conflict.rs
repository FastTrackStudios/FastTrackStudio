#![allow(clippy::needless_lifetimes, clippy::manual_strip)]
#![allow(unused_imports)]

use crate::*;

#[derive(Subcommand)]
pub(crate) enum ConflictCommands {
    /// List conflicts
    List {
        /// Include already-resolved conflicts
        #[arg(long)]
        all: bool,
        #[arg(long, short = 'n', default_value = "50")]
        limit: u32,
        #[arg(long)]
        json: bool,
    },
    /// Resolve a conflict by id
    Resolve {
        conflict_id: i64,
        /// How it was resolved — free-form tag (e.g. "picked-winning",
        /// "picked-losing", "merged", "ignored")
        #[arg(long, default_value = "resolved")]
        how: String,
    },
}
