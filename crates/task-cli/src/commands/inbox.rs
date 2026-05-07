#![allow(clippy::needless_lifetimes, clippy::manual_strip)]
#![allow(unused_imports)]

use crate::*;

#[derive(Subcommand)]
pub(crate) enum InboxCommands {
    /// List untriaged inbox captures
    List {
        /// Output as JSON
        #[arg(long)]
        json: bool,
    },
    /// Show daily review buckets
    Daily {
        /// Output as JSON
        #[arg(long)]
        json: bool,
    },
    /// Show weekly review buckets
    Weekly {
        /// Output as JSON
        #[arg(long)]
        json: bool,
    },
    /// Show monthly review buckets
    Monthly {
        /// Output as JSON
        #[arg(long)]
        json: bool,
    },
    /// Show review buckets scoped to a project
    Project {
        name: String,
        /// Output as JSON
        #[arg(long)]
        json: bool,
    },
    /// Promote/classify an inbox capture
    Promote {
        /// Task id or title
        reference: String,
        /// commitment, idea, task, waiting, reference, someday
        #[arg(long)]
        kind: Option<String>,
        #[arg(long)]
        project: Option<String>,
        #[arg(long)]
        status: Option<String>,
        #[arg(long)]
        assignee: Option<String>,
        #[arg(long)]
        due: Option<String>,
        #[arg(long)]
        scheduled: Option<String>,
        #[arg(long = "tag")]
        add_tags: Vec<String>,
        /// Output as JSON
        #[arg(long)]
        json: bool,
    },
}
