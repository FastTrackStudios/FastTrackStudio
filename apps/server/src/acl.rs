//! Phase 4 — ACL frontmatter parser.
//!
//! Per `plans/decentralized-foundation.md` §14.5, the canonical
//! shape on a project-doc root page is:
//!
//! ```yaml
//! acl:
//!   read: ["@org-members", "[[Cody]]"]
//!   write: ["[[Cody]]", "[[Alice]]"]
//!   admin: ["[[Cody]]"]
//! ```
//!
//! Entries may be a symbolic group (prefixed `@`) or a wiki-link
//! (`[[Name]]` — resolved via the basename index to an auth user
//! id). The parser is pure — wiki-link resolution + group expansion
//! happens at access-check time in a later layer.

use serde::Deserialize;
use thiserror::Error;

/// Symbolic groups recognized by the access check.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Group {
    OrgMembers,
    OrgAdmins,
    Everyone,
    Anyone,
    /// Anything the parser didn't recognize. Kept as-is for forward
    /// compatibility (the access check ignores unknown groups).
    Other(String),
}

impl Group {
    fn parse(s: &str) -> Self {
        match s {
            "@org-members" => Self::OrgMembers,
            "@org-admins" => Self::OrgAdmins,
            "@everyone" => Self::Everyone,
            "@anyone" => Self::Anyone,
            _ => Self::Other(s.to_string()),
        }
    }
}

/// One entry in an ACL list — either a symbolic group or a wiki-link.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AclEntry {
    Group(Group),
    /// Inner string is the wiki-link target name without the `[[ ]]`.
    /// Example: `[[Cody]]` → `"Cody"`.
    WikiLink(String),
}

impl AclEntry {
    fn parse(s: &str) -> Self {
        let trimmed = s.trim();
        if let Some(stripped) = trimmed
            .strip_prefix("[[")
            .and_then(|s| s.strip_suffix("]]"))
        {
            Self::WikiLink(stripped.trim().to_string())
        } else if trimmed.starts_with('@') {
            Self::Group(Group::parse(trimmed))
        } else {
            // Bare literal — treat as wiki-link target for tolerance.
            Self::WikiLink(trimmed.to_string())
        }
    }
}

/// Parsed `acl:` block.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Acl {
    pub read: Vec<AclEntry>,
    pub write: Vec<AclEntry>,
    pub admin: Vec<AclEntry>,
}

#[derive(Debug, Error)]
pub enum AclError {
    #[error("yaml: {0}")]
    Yaml(#[from] serde_yaml::Error),
}

#[derive(Debug, Deserialize)]
struct AclRaw {
    #[serde(default)]
    read: Vec<String>,
    #[serde(default)]
    write: Vec<String>,
    #[serde(default)]
    admin: Vec<String>,
}

#[derive(Debug, Deserialize)]
struct Frontmatter {
    acl: Option<AclRaw>,
}

impl Acl {
    /// Parse the `acl:` block out of a YAML frontmatter blob. If no
    /// `acl:` key is present, returns an empty `Acl` — i.e. nothing
    /// is granted, which the access check should treat as
    /// admin-only by default.
    pub fn parse_frontmatter(yaml: &str) -> Result<Self, AclError> {
        let fm: Frontmatter = serde_yaml::from_str(yaml)?;
        let raw = fm.acl.unwrap_or(AclRaw {
            read: vec![],
            write: vec![],
            admin: vec![],
        });
        Ok(Self {
            read: raw.read.iter().map(|s| AclEntry::parse(s)).collect(),
            write: raw.write.iter().map(|s| AclEntry::parse(s)).collect(),
            admin: raw.admin.iter().map(|s| AclEntry::parse(s)).collect(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_full_example() {
        let yaml = r#"
acl:
  read: ["@org-members", "[[Cody]]"]
  write: ["[[Cody]]", "[[Alice]]"]
  admin: ["[[Cody]]"]
"#;
        let acl = Acl::parse_frontmatter(yaml).expect("parse");
        assert_eq!(acl.read.len(), 2);
        assert_eq!(acl.read[0], AclEntry::Group(Group::OrgMembers));
        assert_eq!(acl.read[1], AclEntry::WikiLink("Cody".into()));
        assert_eq!(acl.write[0], AclEntry::WikiLink("Cody".into()));
        assert_eq!(acl.write[1], AclEntry::WikiLink("Alice".into()));
        assert_eq!(acl.admin, vec![AclEntry::WikiLink("Cody".into())]);
    }

    #[test]
    fn parses_empty_yaml() {
        let acl = Acl::parse_frontmatter("").expect("parse");
        assert!(acl.read.is_empty());
        assert!(acl.write.is_empty());
        assert!(acl.admin.is_empty());
    }

    #[test]
    fn parses_partial_block() {
        let acl = Acl::parse_frontmatter("acl:\n  read: [\"@anyone\"]\n").expect("parse");
        assert_eq!(acl.read, vec![AclEntry::Group(Group::Anyone)]);
        assert!(acl.write.is_empty());
    }

    #[test]
    fn unknown_group_preserved() {
        let acl = Acl::parse_frontmatter("acl:\n  admin: [\"@studio-leads\"]\n").expect("parse");
        assert_eq!(
            acl.admin,
            vec![AclEntry::Group(Group::Other("@studio-leads".into()))]
        );
    }
}
