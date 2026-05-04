//! File I/O for reading and writing workflow types from `.md` files.
//!
//! The pattern matches `Vault::parse_task_from_md` / `Vault::render_task_file`:
//! - **Read:** split frontmatter from body, deserialize YAML via `facet_yaml::from_str`
//! - **Write:** serialize to YAML via `facet_yaml::to_string`, combine with body

use super::download::*;
use super::event::*;
use super::output::*;

// ── Helpers ─────────────────────────────────────────────────────────────────

/// Split `---\nFRONTMATTER\n---\nBODY` into `(frontmatter, body)`.
///
/// Same logic as `Vault::split_frontmatter` but available without the
/// `server` feature gate.
fn split_frontmatter(content: &str) -> Option<(&str, &str)> {
    let content = content.trim_start();
    if !content.starts_with("---") {
        return None;
    }
    let rest = &content[3..];
    let end = rest.find("\n---")?;
    let frontmatter = rest[..end].trim_start_matches('\n');
    let body = &rest[end + 4..];
    let body = body.trim_start_matches('\n');
    Some((frontmatter, body))
}

// ── Generic ─────────────────────────────────────────────────────────────────

/// Generic parse: deserialize any `Facet` type from `.md` content.
///
/// Splits the YAML frontmatter from the body and deserializes the
/// frontmatter into `T`.
pub fn parse_frontmatter<T: for<'a> facet::Facet<'a>>(content: &str) -> Option<T> {
    let (frontmatter, _body) = split_frontmatter(content)?;
    facet_yaml::from_str::<T>(frontmatter).ok()
}

/// Generic render: serialize any `Facet` type to frontmatter + body.
///
/// Produces `---\n{yaml}---\n{body}`.
pub fn render_frontmatter<T: for<'a> facet::Facet<'a>>(
    data: &T,
    body: &str,
) -> Result<String, RenderError> {
    let yaml = facet_yaml::to_string(data).map_err(|e| RenderError(e.to_string()))?;
    let yaml = yaml.strip_prefix("---\n").unwrap_or(&yaml);
    Ok(format!("---\n{}---\n{}", yaml, body))
}

/// Error returned when rendering frontmatter fails.
#[derive(Debug, thiserror::Error)]
#[error("render error: {0}")]
pub struct RenderError(pub String);

#[cfg(feature = "server")]
impl From<RenderError> for crate::service::VaultError {
    fn from(e: RenderError) -> Self {
        crate::service::VaultError::ParseError(e.0)
    }
}

// ── Read ────────────────────────────────────────────────────────────────────

/// Parse an `event.md` file.
pub fn parse_event(content: &str) -> Option<Event> {
    parse_frontmatter::<Event>(content)
}

/// Parse a `performance.md` file.
pub fn parse_performance(content: &str) -> Option<Performance> {
    parse_frontmatter::<Performance>(content)
}

/// Parse a `setlist.md` file.
pub fn parse_setlist(content: &str) -> Option<Setlist> {
    parse_frontmatter::<Setlist>(content)
}

/// Parse a `stage-plot.md` file.
pub fn parse_stage_plot(content: &str) -> Option<StagePlot> {
    parse_frontmatter::<StagePlot>(content)
}

/// Parse an `input-list.md` file.
pub fn parse_input_list(content: &str) -> Option<InputList> {
    parse_frontmatter::<InputList>(content)
}

/// Parse a `run-of-show.md` file.
pub fn parse_run_of_show(content: &str) -> Option<RunOfShow> {
    parse_frontmatter::<RunOfShow>(content)
}

/// Parse an `outputs.md` file.
pub fn parse_output_manifest(content: &str) -> Option<OutputManifest> {
    parse_frontmatter::<OutputManifest>(content)
}

/// Parse a `portal.md` file.
pub fn parse_download_portal(content: &str) -> Option<DownloadPortal> {
    parse_frontmatter::<DownloadPortal>(content)
}

/// Parse a `venue.md` file.
pub fn parse_venue(content: &str) -> Option<Venue> {
    parse_frontmatter::<Venue>(content)
}

/// Parse a `budget.md` file.
pub fn parse_budget(content: &str) -> Option<Budget> {
    parse_frontmatter::<Budget>(content)
}

// ── Write ───────────────────────────────────────────────────────────────────

/// Render an event to `.md` file content.
pub fn render_event(event: &Event, body: &str) -> Result<String, RenderError> {
    render_frontmatter(event, body)
}

/// Render a performance to `.md` file content.
pub fn render_performance(perf: &Performance, body: &str) -> Result<String, RenderError> {
    render_frontmatter(perf, body)
}

/// Render a setlist to `.md` file content.
pub fn render_setlist(setlist: &Setlist, body: &str) -> Result<String, RenderError> {
    render_frontmatter(setlist, body)
}

/// Render a stage plot to `.md` file content.
pub fn render_stage_plot(plot: &StagePlot, body: &str) -> Result<String, RenderError> {
    render_frontmatter(plot, body)
}

/// Render an input list to `.md` file content.
pub fn render_input_list(list: &InputList, body: &str) -> Result<String, RenderError> {
    render_frontmatter(list, body)
}

/// Render a run-of-show to `.md` file content.
pub fn render_run_of_show(ros: &RunOfShow, body: &str) -> Result<String, RenderError> {
    render_frontmatter(ros, body)
}

/// Render an output manifest to `.md` file content.
pub fn render_output_manifest(
    manifest: &OutputManifest,
    body: &str,
) -> Result<String, RenderError> {
    render_frontmatter(manifest, body)
}

/// Render a download portal to `.md` file content.
pub fn render_download_portal(portal: &DownloadPortal, body: &str) -> Result<String, RenderError> {
    render_frontmatter(portal, body)
}

/// Render a venue to `.md` file content.
pub fn render_venue(venue: &Venue, body: &str) -> Result<String, RenderError> {
    render_frontmatter(venue, body)
}

/// Render a budget to `.md` file content.
pub fn render_budget(budget: &Budget, body: &str) -> Result<String, RenderError> {
    render_frontmatter(budget, body)
}
