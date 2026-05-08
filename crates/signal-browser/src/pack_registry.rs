//! Filesystem-direct pack discovery — no DB.
//!
//! Walks a library root, opens each `.signalpack` header (no audio decode),
//! and produces [`PackEntry`] values containing everything the browser needs
//! to render a tag-filterable list. Convertible to [`ColumnItem`] so the
//! same `signal-browser` consumers (Dioxus collection_browser, ratatui TUI)
//! can render packs without knowing they came from disk.
//!
//! Cheap: opening a pack header is a small `read_exact` of the index
//! footer. Scanning ~5,000 packs takes well under a second.

use std::path::{Path, PathBuf};

use rayon::prelude::*;
use signal::tagging::{StructuredTag, TagSet};
use signal_sampler::{LibrarySpec, read_pack_header};

use crate::types::{ColumnItem, DetailData};

/// Lightweight summary of a single `.signalpack` on disk.
#[derive(Clone, Debug)]
pub struct PackEntry {
    /// Absolute path to the pack file.
    pub path: PathBuf,
    /// `LibrarySpec.name`, falling back to the file stem.
    pub name: String,
    /// `LibrarySpec.instrument`. Empty when not classified.
    pub instrument: String,
    /// `LibrarySpec.category`. Empty when not classified.
    pub category: String,
    /// `LibrarySpec.style`.
    pub style: Vec<String>,
    /// Structured tags materialised from `LibrarySpec.tags`.
    pub tags: TagSet,
    /// Vendor (`LibrarySpec.vendor`).
    pub vendor: String,
    /// Path component groups for tree-style display: e.g.
    /// `["Drum Kits", "Stylus RMX", "RMX Grooves"]`. Last segment is the
    /// pack's immediate parent folder. Empty when the pack sits at `root`.
    pub folder: Vec<String>,
    /// Total samples in the pack body.
    pub sample_count: usize,
    /// Pack file size on disk in bytes.
    pub size_bytes: u64,
}

impl PackEntry {
    /// Convert to a `ColumnItem` for rendering in the multi-column browser.
    pub fn to_column_item(&self) -> ColumnItem {
        let subtitle = match (&self.instrument, &self.category) {
            (i, c) if !i.is_empty() && !c.is_empty() => Some(format!("{i} · {c}")),
            (i, _) if !i.is_empty() => Some(i.clone()),
            (_, c) if !c.is_empty() => Some(c.clone()),
            _ => None,
        };
        let badge = (!self.vendor.is_empty()).then(|| self.vendor.clone());
        ColumnItem {
            id: self.path.to_string_lossy().into_owned(),
            name: self.name.clone(),
            subtitle,
            badge,
            metadata: None,
            structured_tags: self.tags.clone(),
            detail: DetailData::default(),
            tag: None,
            folder: (!self.folder.is_empty()).then(|| self.folder.join(" / ")),
        }
    }
}

/// Scan `root` recursively for `.signalpack` files, opening each header.
/// Returns `PackEntry` records sorted by path for stable display order.
///
/// Errors during individual pack reads are logged and skipped — partial
/// failures shouldn't kill the browser.
pub fn scan_packs(root: &Path) -> Vec<PackEntry> {
    let paths: Vec<PathBuf> = walkdir::WalkDir::new(root)
        .follow_links(false)
        .max_depth(12)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file())
        .map(|e| e.into_path())
        .filter(|p| p.extension().is_some_and(|e| e == "signalpack"))
        .collect();

    let mut entries: Vec<PackEntry> = paths
        .par_iter()
        .filter_map(|path| build_entry(path, root).ok())
        .collect();
    entries.sort_by(|a, b| a.path.cmp(&b.path));
    entries
}

fn build_entry(path: &Path, root: &Path) -> Result<PackEntry, signal_sampler::SamplerError> {
    let header = read_pack_header(path)?;
    let spec: LibrarySpec = header.spec;
    let name = if spec.name.is_empty() {
        path.file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("?")
            .to_string()
    } else {
        spec.name.clone()
    };
    let mut tags = spec.tag_set();
    // Round out the tag set with anything implied but not explicit so the
    // filter chips work uniformly across older and newer packs.
    if !spec.instrument.is_empty() && tag_missing(&tags, "instrument", &spec.instrument) {
        tags.insert(StructuredTag::new(
            signal::tagging::TagCategory::Instrument,
            spec.instrument.clone(),
        ));
    }
    if !spec.vendor.is_empty() && tag_missing(&tags, "vendor", &spec.vendor) {
        tags.insert(StructuredTag::new(
            signal::tagging::TagCategory::Vendor,
            spec.vendor.clone(),
        ));
    }

    let folder = path
        .parent()
        .and_then(|p| p.strip_prefix(root).ok())
        .map(|rel| {
            rel.components()
                .filter_map(|c| c.as_os_str().to_str().map(|s| s.to_string()))
                .filter(|s| !s.is_empty())
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();

    Ok(PackEntry {
        path: path.to_path_buf(),
        name,
        instrument: spec.instrument,
        category: spec.category,
        style: spec.style,
        tags,
        vendor: spec.vendor,
        folder,
        sample_count: header.sample_count,
        size_bytes: header.size_bytes,
    })
}

fn tag_missing(tags: &TagSet, category_str: &str, value: &str) -> bool {
    !tags
        .values()
        .any(|t| t.category.as_str() == category_str && t.value == value)
}

/// Filter packs by an instrument substring (case-insensitive).
/// Empty `q` returns all entries unchanged.
pub fn filter_instrument<'a>(entries: &'a [PackEntry], q: &str) -> Vec<&'a PackEntry> {
    if q.is_empty() {
        return entries.iter().collect();
    }
    let q = q.to_ascii_lowercase();
    entries
        .iter()
        .filter(|e| e.instrument.to_ascii_lowercase().contains(&q))
        .collect()
}

/// Filter packs by category (case-insensitive exact match).
pub fn filter_category<'a>(entries: &'a [PackEntry], category: &str) -> Vec<&'a PackEntry> {
    if category.is_empty() {
        return entries.iter().collect();
    }
    let q = category.to_ascii_lowercase();
    entries
        .iter()
        .filter(|e| e.category.to_ascii_lowercase() == q)
        .collect()
}

/// Free-text search across name + folder + tags.
pub fn search<'a>(entries: &'a [PackEntry], q: &str) -> Vec<&'a PackEntry> {
    if q.is_empty() {
        return entries.iter().collect();
    }
    let q = q.to_ascii_lowercase();
    entries
        .iter()
        .filter(|e| {
            e.name.to_ascii_lowercase().contains(&q)
                || e.folder.iter().any(|s| s.to_ascii_lowercase().contains(&q))
                || e.instrument.to_ascii_lowercase().contains(&q)
                || e.category.to_ascii_lowercase().contains(&q)
                || e.tags
                    .values()
                    .any(|t| t.value.to_ascii_lowercase().contains(&q))
        })
        .collect()
}
