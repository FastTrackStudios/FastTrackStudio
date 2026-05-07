#![allow(clippy::needless_lifetimes, clippy::manual_strip)]
#![allow(unused_imports)]

use crate::*;

#[derive(Subcommand)]
pub(crate) enum PeopleCommands {
    /// List CardDAV-backed people
    List {
        #[arg(long)]
        addressbook: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// List CardDAV-backed organizations
    Orgs {
        #[arg(long)]
        addressbook: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Show one person with related tasks/projects/events
    Show {
        reference: String,
        #[arg(long)]
        addressbook: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Show one organization with related people/tasks/projects/events
    Org {
        reference: String,
        #[arg(long)]
        addressbook: Option<String>,
        #[arg(long)]
        json: bool,
    },
}
