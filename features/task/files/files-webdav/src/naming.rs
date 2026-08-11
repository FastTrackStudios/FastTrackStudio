//! Mapping a File Root onto the URL segment a file manager shows as a
//! folder name.
//!
//! A mounted root has to *look* like the project it is, so the segment
//! is the root's name — not its uuid. Two constraints make that less
//! trivial than it sounds:
//!
//! - Root names are free text and need not be unique, but a collection
//!   cannot hold two children with the same name. Colliding names are
//!   therefore *all* disambiguated with a short id suffix, so a segment
//!   never depends on which root happened to be created first.
//! - Finder and Explorer treat names case-insensitively, so collision
//!   detection folds case even though the segments themselves keep it.
//!
//! A root's uuid is always accepted as an alternative segment
//! ([`resolve`]), which gives scripts and tests a stable address that
//! survives a rename.

use std::collections::HashMap;

use files_proto::FileRootInfo;
use uuid::Uuid;

/// Characters that must not appear in a path segment — the separator
/// and NUL are structurally impossible, the rest are the Windows
/// reserved set (Explorer refuses to create or display them, and this
/// bridge exists for Explorer).
const RESERVED: &[char] = &['/', '\\', ':', '*', '?', '"', '<', '>', '|'];

/// The segment `root` would get if its name were unique.
fn sanitize(root: &FileRootInfo) -> String {
    let cleaned: String = root
        .name
        .chars()
        .map(|c| {
            if c.is_control() || RESERVED.contains(&c) {
                '-'
            } else {
                c
            }
        })
        .collect();
    let trimmed = cleaned.trim();
    // `.`/`..` would be eaten by path normalization, and an empty
    // segment is unaddressable — fall back to the id, which is always
    // a legal segment.
    if trimmed.is_empty() || trimmed == "." || trimmed == ".." {
        root.id.to_string()
    } else {
        trimmed.to_string()
    }
}

/// Short, stable disambiguator for colliding names.
fn short_id(id: Uuid) -> String {
    id.simple().to_string()[..8].to_string()
}

/// Every visible root paired with the URL segment it is addressed by.
/// Order follows `roots`.
#[must_use]
pub fn segments(roots: &[FileRootInfo]) -> Vec<(String, FileRootInfo)> {
    let mut counts: HashMap<String, usize> = HashMap::new();
    for root in roots {
        *counts.entry(sanitize(root).to_lowercase()).or_default() += 1;
    }
    roots
        .iter()
        .map(|root| {
            let base = sanitize(root);
            let segment = if counts.get(&base.to_lowercase()).copied().unwrap_or(0) > 1 {
                format!("{base} ({})", short_id(root.id))
            } else {
                base
            };
            (segment, root.clone())
        })
        .collect()
}

/// Resolve one URL segment against the visible roots: its uuid, or the
/// name segment [`segments`] assigned it (case-insensitively, matching
/// how the mounting OS compares names).
#[must_use]
pub fn resolve(roots: &[FileRootInfo], segment: &str) -> Option<FileRootInfo> {
    if let Ok(id) = Uuid::parse_str(segment)
        && let Some(root) = roots.iter().find(|r| r.id == id)
    {
        return Some(root.clone());
    }
    segments(roots)
        .into_iter()
        .find(|(seg, _)| seg.eq_ignore_ascii_case(segment))
        .map(|(_, root)| root)
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::Utc;
    use files_proto::RootFlavor;

    fn root(name: &str) -> FileRootInfo {
        FileRootInfo {
            id: Uuid::new_v4(),
            name: name.to_string(),
            path: format!("/tmp/{name}"),
            flavor: RootFlavor::Media,
            created_at: Utc::now(),
        }
    }

    #[test]
    fn unique_names_keep_their_name() {
        let roots = vec![root("El Artisa"), root("Dr Jaramillo")];
        let segs = segments(&roots);
        assert_eq!(segs[0].0, "El Artisa");
        assert_eq!(segs[1].0, "Dr Jaramillo");
    }

    #[test]
    fn colliding_names_are_all_suffixed() {
        let roots = vec![root("Mix"), root("mix")];
        let segs = segments(&roots);
        assert!(segs[0].0.starts_with("Mix ("), "{}", segs[0].0);
        assert!(segs[1].0.starts_with("mix ("), "{}", segs[1].0);
        assert_ne!(segs[0].0, segs[1].0);
        // Both remain resolvable by their own segment.
        for (seg, expected) in &segs {
            assert_eq!(resolve(&roots, seg).unwrap().id, expected.id);
        }
    }

    #[test]
    fn reserved_characters_are_replaced() {
        let roots = vec![root("A/B:C")];
        assert_eq!(segments(&roots)[0].0, "A-B-C");
    }

    #[test]
    fn a_root_is_always_addressable_by_uuid() {
        let roots = vec![root("Mix")];
        let by_id = resolve(&roots, &roots[0].id.to_string()).expect("uuid segment resolves");
        assert_eq!(by_id.id, roots[0].id);
    }

    #[test]
    fn an_unnameable_root_falls_back_to_its_id() {
        let roots = vec![root("   ")];
        assert_eq!(segments(&roots)[0].0, roots[0].id.to_string());
    }
}
