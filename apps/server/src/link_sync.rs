//! Auto-sync note `[[wikilinks]]` → `note → verse` typed links, driven by
//! vault change events. Keeps the knowledge graph live as notes are saved
//! — the always-on counterpart to the `links` crate's `sync_vault_links`
//! example (which also handles video clips; here we sync verse refs, the
//! common case).

use std::path::{Path, PathBuf};
use std::sync::OnceLock;

use links::{Confidence, LinksService, NodeKind, NodeRef, Relation, Store, TypedLink, Visibility};
use regex::Regex;
use scripture::VerseRange;
use tokio::sync::broadcast;
use vault_proto::VaultEvent;

/// Provenance tag for links this sync owns — so a re-sync replaces only
/// its own links, never user-authored ones. Matches `sync_vault_links`.
const SOURCE_REF: &str = "vault-link-sync";

/// Mirrors `vault_obsidian::obsidian_parse::LINK_REGEX` (copied so the
/// server doesn't pull the obsidian feature just for the pattern):
/// g1 = target, g2 = `#^block`, g3 = `#heading`, g4 = `|alias`.
const LINK_REGEX: &str =
    r"\[\[([^\]\|#\^\r\n]+)(?:#\^([a-zA-Z0-9\-]+)|#([^\]\|\r\n]+))?(?:\|([^\]\r\n]+))?\]\]";

fn link_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(LINK_REGEX).expect("LINK_REGEX is valid"))
}

fn is_md(path: &str) -> bool {
    Path::new(path)
        .extension()
        .is_some_and(|e| e.eq_ignore_ascii_case("md"))
}

/// Delete this note's previously auto-synced links.
fn remove_synced(store: &Store, src: &NodeRef) {
    if let Ok(links) = store.links_for(src.clone()) {
        for l in links {
            if l.provenance.source_ref == SOURCE_REF && &l.source == src {
                let _ = store.delete(&l.id);
            }
        }
    }
}

/// Replace a note's auto-synced verse links from its current content.
fn sync_note(store: &Store, rel_path: &str, content: &str) {
    let src = NodeRef::new(NodeKind::Note, rel_path);
    remove_synced(store, &src);

    let mut seen = std::collections::HashSet::new();
    for cap in link_re().captures_iter(content) {
        // Skip anchored links (`#^block` / `#heading`) — not verse refs.
        if cap.get(2).is_some() || cap.get(3).is_some() {
            continue;
        }
        let target = cap.get(1).map_or("", |m| m.as_str().trim());
        let Ok(range) = VerseRange::parse(target) else {
            continue;
        };
        let osis = range.osis();
        if !seen.insert(osis.clone()) {
            continue;
        }
        let mut link = TypedLink::new(
            src.clone(),
            NodeRef::verse(osis),
            Relation::Mentions,
            Confidence::Certain,
        );
        link.visibility = Visibility::Private; // prose notes are the private layer
        link.provenance.created_by = SOURCE_REF.to_string();
        link.provenance.source_ref = SOURCE_REF.to_string();
        link.provenance.derived = true;
        let _ = store.create(link);
    }
}

/// Subscribe to the vault's change broadcast and keep `note → verse` links
/// in sync as notes are saved (`Put`) or removed (`Delete`).
pub fn spawn(store: Store, vault_root: PathBuf, mut rx: broadcast::Receiver<VaultEvent>) {
    tokio::spawn(async move {
        loop {
            match rx.recv().await {
                Ok(VaultEvent::Put { path, .. }) if is_md(&path) => {
                    if let Ok(content) = std::fs::read_to_string(vault_root.join(&path)) {
                        sync_note(&store, &path, &content);
                    }
                }
                Ok(VaultEvent::Delete { path }) if is_md(&path) => {
                    remove_synced(&store, &NodeRef::new(NodeKind::Note, path));
                }
                Ok(_) => {}
                Err(broadcast::error::RecvError::Closed) => break,
                Err(broadcast::error::RecvError::Lagged(_)) => {}
            }
        }
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn syncs_verse_wikilinks_deduped_and_replaceable() {
        let dir = tempfile::tempdir().unwrap();
        let store = Store::open(dir.path().join("links.jsonl"));
        let src = NodeRef::new(NodeKind::Note, "Notes/study.md");

        sync_note(
            &store,
            "Notes/study.md",
            "See [[John 3:16]] and again [[John 3:16]], plus [[Romans 5:8]], \
             a plain [[Some Note]], and a block ref [[John 3:16#^abc]].",
        );
        let mut targets: Vec<String> = store
            .links_for(src.clone())
            .unwrap()
            .iter()
            .map(|l| l.target.to_token())
            .collect();
        targets.sort();
        // Deduped, verses only (note ref + block-anchored ref dropped).
        assert_eq!(targets, vec!["verse:John.3.16", "verse:Rom.5.8"]);

        // Re-sync with fewer refs replaces the prior set (stale removed).
        sync_note(&store, "Notes/study.md", "Only [[John 3:16]] now.");
        let after = store.links_for(src).unwrap();
        assert_eq!(after.len(), 1);
        assert_eq!(after[0].target.to_token(), "verse:John.3.16");
    }
}
