//! [`NodeRef`] — a universal reference to anything a link can touch.
//!
//! A node is a verse, a vault note, a wiki page, a topic, an entity
//! (person/place), a block, or an external URL. The `id` is interpreted
//! per [`NodeKind`] (an OSIS verse id, a vault path, a topic slug, …).
//! The canonical string form is `kind:id` (e.g. `verse:John.3.16`,
//! `note:Journal/2026-06-16.md`, `topic:money`).

use facet::Facet;
use serde::{Deserialize, Serialize};

/// What kind of thing a [`NodeRef`] points at.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Facet)]
#[repr(u8)]
pub enum NodeKind {
    /// A Bible verse — `id` is the OSIS reference (`John.3.16`).
    Verse,
    /// A vault note — `id` is the vault-relative path.
    Note,
    /// A wiki page — `id` is the wiki-relative path.
    Wiki,
    /// A topic / tag — `id` is the topic slug (`money`).
    Topic,
    /// An entity (person, place, thing) — `id` is its stable id.
    Entity,
    /// A block within a note — `id` is the block uuid.
    Block,
    /// An external resource — `id` is the URL.
    External,
}

impl NodeKind {
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Verse => "verse",
            Self::Note => "note",
            Self::Wiki => "wiki",
            Self::Topic => "topic",
            Self::Entity => "entity",
            Self::Block => "block",
            Self::External => "external",
        }
    }

    #[must_use]
    pub fn parse(s: &str) -> Option<Self> {
        Some(match s {
            "verse" => Self::Verse,
            "note" => Self::Note,
            "wiki" => Self::Wiki,
            "topic" => Self::Topic,
            "entity" => Self::Entity,
            "block" => Self::Block,
            "external" => Self::External,
            _ => return None,
        })
    }
}

/// A reference to one node in the knowledge graph.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Facet)]
pub struct NodeRef {
    pub kind: NodeKind,
    pub id: String,
}

impl NodeRef {
    #[must_use]
    pub fn new(kind: NodeKind, id: impl Into<String>) -> Self {
        Self {
            kind,
            id: id.into(),
        }
    }

    /// A verse node (`id` = OSIS).
    #[must_use]
    pub fn verse(osis: impl Into<String>) -> Self {
        Self::new(NodeKind::Verse, osis)
    }

    /// Canonical `kind:id` string.
    #[must_use]
    pub fn to_token(&self) -> String {
        format!("{}:{}", self.kind.as_str(), self.id)
    }

    /// Parse a `kind:id` token.
    #[must_use]
    pub fn parse(token: &str) -> Option<Self> {
        let (kind, id) = token.split_once(':')?;
        Some(Self::new(NodeKind::parse(kind)?, id))
    }
}

impl std::fmt::Display for NodeRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.to_token())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn token_round_trips() {
        let n = NodeRef::verse("John.3.16");
        assert_eq!(n.to_token(), "verse:John.3.16");
        assert_eq!(NodeRef::parse("verse:John.3.16"), Some(n));
        assert_eq!(
            NodeRef::parse("note:Journal/2026.md"),
            Some(NodeRef::new(NodeKind::Note, "Journal/2026.md"))
        );
        assert_eq!(NodeRef::parse("bogus"), None);
        assert_eq!(NodeRef::parse("nope:x"), None);
    }
}
