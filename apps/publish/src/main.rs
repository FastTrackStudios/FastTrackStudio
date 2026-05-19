//! `task-publish` — Quartz-equivalent static-site generator for
//! Task-architect `.loro` vaults.
//!
//! Phase 1 ships:
//! - Read a `.loro` snapshot file
//! - List every page + its blocks
//! - Render each page to standalone HTML in `out/<slug>.html`
//! - Build an `index.html` listing all pages
//! - Drop a single `assets/style.css` for theming
//!
//! Phases 2+ (wikilinks resolution, backlinks, search, graph)
//! are tracked in `plans/vault-publisher.md`.
//!
//! ## Usage
//!
//! ```sh
//! task-publish ./vault.loro --out ./public
//! ```

use clap::Parser;
use eyre::{Context, Result};
use std::path::PathBuf;

use publish_core as components; // re-export shared engine
mod feeds;
mod graph;
use publish_core::parser as inline;
mod render;
mod search;
mod site;
mod source;
use publish_core::syntax;
mod tags;

#[derive(Parser, Debug)]
#[command(name = "task-publish")]
#[command(about = "Build a static doc site from a Task-architect .loro vault")]
struct Args {
    /// Path to the `.loro` snapshot file (the file Export emits).
    input: PathBuf,
    /// Output directory. Created if missing. Existing files are
    /// overwritten — wipe it first if you want a clean build.
    #[arg(short, long, default_value = "public")]
    out: PathBuf,
    /// Optional site title (shown in the header + browser tab).
    #[arg(long, default_value = "Vault")]
    title: String,
    /// Absolute origin where the site will be deployed (used in
    /// sitemap.xml + RSS). Without trailing slash. Optional —
    /// when omitted, sitemap/RSS are skipped.
    #[arg(long)]
    base_url: Option<String>,
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env().unwrap_or_else(|_| "info".into()),
        )
        .init();
    let args = Args::parse();
    let snap = tokio::fs::read(&args.input)
        .await
        .with_context(|| format!("reading snapshot at {}", args.input.display()))?;
    tracing::info!(bytes = snap.len(), "loaded snapshot");
    site::build(&snap, &args.out, &args.title, args.base_url.as_deref()).await?;
    tracing::info!(out = %args.out.display(), "site written");
    Ok(())
}
