//! Location/Venue and Space entities — reusable place records.
//!
//! Locations live as markdown notes (`locations/<Name>.md`) with YAML
//! frontmatter. Each location can contain bookable spaces (rooms,
//! stages, studios) as nested entries in the frontmatter.
//!
//! Calendar events reference locations and optional spaces via
//! `[[WikiLink]]` while preserving the freeform iCalendar `LOCATION`
//! text for interop.
//!
//! Venue defaults (stage plot, input list, patch list, setlist,
//! personnel, schedule, files, diagrams) can be inherited and
//! overridden per event/date.

use chrono::{DateTime, Utc};
use facet::Facet;

/// A reusable venue/location record.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Location {
    /// Stable identifier (UUID v4), auto-generated on create.
    pub id: Option<String>,

    /// Human-readable name, also used as the file stem.
    pub name: String,

    // ── Address ─────────────────────────────────────────────────────
    pub address1: Option<String>,
    pub address2: Option<String>,
    pub city: Option<String>,
    pub state: Option<String>,
    pub postal_code: Option<String>,
    /// ISO 3166-1 alpha-2 country code (e.g. "US", "GB").
    pub country_code: Option<String>,

    // ── Contact ─────────────────────────────────────────────────────
    pub contact_name: Option<String>,
    pub contact_email: Option<String>,
    pub contact_phone: Option<String>,

    // ── Access & logistics ──────────────────────────────────────────
    /// Free-text access/directions notes (gate codes, buzzer, etc.).
    pub access_notes: Option<String>,
    /// Parking and load-in instructions.
    pub parking_load_in: Option<String>,
    /// Network/Wi-Fi and power availability notes.
    pub network_power: Option<String>,

    // ── Venue type ──────────────────────────────────────────────────
    /// Freeform type tag: studio, live-venue, church, rehearsal, remote, etc.
    pub venue_type: Option<String>,

    // ── Defaults ────────────────────────────────────────────────────
    /// Default files associated with this venue (stage plot, input list, etc.).
    /// Each entry is a relative path or WikiLink to a file in the vault.
    #[facet(default)]
    pub default_files: Vec<VenueDefault>,

    // ── Bookable spaces ─────────────────────────────────────────────
    /// Sub-locations within this venue (rooms, stages, studios).
    #[facet(default)]
    pub spaces: Vec<Space>,

    // ── Tags ────────────────────────────────────────────────────────
    #[facet(default)]
    pub tags: Vec<String>,

    // ── Bookkeeping ─────────────────────────────────────────────────
    pub date_created: Option<DateTime<Utc>>,
    pub date_modified: Option<DateTime<Utc>>,
    pub deleted_at: Option<DateTime<Utc>>,

    /// Markdown body (notes, directions, etc.). Not serialized to frontmatter.
    #[facet(skip)]
    #[facet(default)]
    pub body: String,
}

/// A bookable sub-location inside a venue (room, stage, studio, booth).
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Space {
    /// Stable identifier (UUID v4).
    pub id: Option<String>,

    /// Human-readable name (e.g. "Studio A", "Main Stage", "Green Room").
    pub name: String,

    /// Optional capacity.
    pub capacity: Option<u32>,

    /// Free-text notes (gear available, dimensions, etc.).
    pub notes: Option<String>,

    /// Space-specific default files (overrides or extends venue defaults).
    #[facet(default)]
    pub default_files: Vec<VenueDefault>,

    #[facet(default)]
    pub tags: Vec<String>,
}

/// A default file/resource associated with a venue or space.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct VenueDefault {
    /// What kind of default: stage_plot, input_list, patch_list,
    /// setlist, personnel, schedule, diagram, file, etc.
    pub kind: String,

    /// Path or WikiLink to the file.
    pub path: String,

    /// Optional label for display.
    pub label: Option<String>,
}

impl Location {
    pub fn is_deleted(&self) -> bool {
        self.deleted_at.is_some()
    }

    /// Find a space by name (case-insensitive).
    pub fn find_space(&self, name: &str) -> Option<&Space> {
        let lower = name.to_lowercase();
        self.spaces.iter().find(|s| s.name.to_lowercase() == lower)
    }

    /// Find a space by id.
    pub fn find_space_by_id(&self, id: &str) -> Option<&Space> {
        self.spaces.iter().find(|s| s.id.as_deref() == Some(id))
    }

    /// Collect effective default files for a space, with space defaults
    /// overriding venue defaults of the same kind.
    pub fn effective_defaults(&self, space_name: Option<&str>) -> Vec<&VenueDefault> {
        let space = space_name.and_then(|n| self.find_space(n));
        match space {
            Some(sp) => {
                let mut result: Vec<&VenueDefault> = sp.default_files.iter().collect();
                let space_kinds: Vec<&str> =
                    sp.default_files.iter().map(|d| d.kind.as_str()).collect();
                for d in &self.default_files {
                    if !space_kinds.contains(&d.kind.as_str()) {
                        result.push(d);
                    }
                }
                result
            }
            None => self.default_files.iter().collect(),
        }
    }
}

pub fn render_location_body(location: &Location) -> String {
    use std::fmt::Write;

    let mut out = String::new();
    let _ = writeln!(out, "# {}", location.name);
    let _ = writeln!(out);
    if let Some(venue_type) = &location.venue_type {
        let _ = writeln!(out, "**Type:** {venue_type}");
    }
    if let Some(address1) = &location.address1 {
        let _ = writeln!(out, "**Address:** {address1}");
    }
    if location.city.is_some() || location.state.is_some() || location.postal_code.is_some() {
        let locality = [
            location.city.as_deref(),
            location.state.as_deref(),
            location.postal_code.as_deref(),
        ]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>()
        .join(", ");
        let _ = writeln!(out, "**Locality:** {locality}");
    }
    if let Some(contact) = &location.contact_name {
        let _ = writeln!(out, "**Contact:** {contact}");
    }
    if let Some(email) = &location.contact_email {
        let _ = writeln!(out, "**Email:** {email}");
    }
    if let Some(phone) = &location.contact_phone {
        let _ = writeln!(out, "**Phone:** {phone}");
    }
    if !location.spaces.is_empty() {
        let _ = writeln!(out);
        let _ = writeln!(out, "## Spaces");
        for space in &location.spaces {
            let _ = writeln!(
                out,
                "- {}{}",
                space.name,
                space
                    .capacity
                    .map(|capacity| format!(" ({capacity})"))
                    .unwrap_or_default()
            );
        }
    }
    if !location.default_files.is_empty() {
        let _ = writeln!(out);
        let _ = writeln!(out, "## Defaults");
        for default in &location.default_files {
            let label = default.label.as_deref().unwrap_or(&default.kind);
            let _ = writeln!(out, "- {}: {}", label, default.path);
        }
    }
    if !location.body.is_empty() {
        let _ = writeln!(out);
        let _ = write!(out, "{}", location.body);
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_location_is_empty() {
        let loc = Location::default();
        assert_eq!(loc.name, "");
        assert!(loc.id.is_none());
        assert!(loc.spaces.is_empty());
        assert!(loc.default_files.is_empty());
        assert!(!loc.is_deleted());
    }

    #[test]
    fn find_space_case_insensitive() {
        let loc = Location {
            name: "Test Venue".into(),
            spaces: vec![
                Space {
                    name: "Studio A".into(),
                    ..Default::default()
                },
                Space {
                    name: "Green Room".into(),
                    ..Default::default()
                },
            ],
            ..Default::default()
        };
        assert!(loc.find_space("studio a").is_some());
        assert!(loc.find_space("STUDIO A").is_some());
        assert!(loc.find_space("Studio A").is_some());
        assert!(loc.find_space("nonexistent").is_none());
    }

    #[test]
    fn find_space_by_id_works() {
        let loc = Location {
            name: "Test Venue".into(),
            spaces: vec![Space {
                id: Some("abc-123".into()),
                name: "Main Stage".into(),
                ..Default::default()
            }],
            ..Default::default()
        };
        assert!(loc.find_space_by_id("abc-123").is_some());
        assert!(loc.find_space_by_id("other").is_none());
    }

    #[test]
    fn effective_defaults_venue_only() {
        let loc = Location {
            name: "Venue".into(),
            default_files: vec![
                VenueDefault {
                    kind: "stage_plot".into(),
                    path: "stage.pdf".into(),
                    label: None,
                },
                VenueDefault {
                    kind: "input_list".into(),
                    path: "inputs.md".into(),
                    label: None,
                },
            ],
            ..Default::default()
        };
        let defaults = loc.effective_defaults(None);
        assert_eq!(defaults.len(), 2);
    }

    #[test]
    fn effective_defaults_space_overrides_venue() {
        let loc = Location {
            name: "Venue".into(),
            default_files: vec![
                VenueDefault {
                    kind: "stage_plot".into(),
                    path: "venue-stage.pdf".into(),
                    label: None,
                },
                VenueDefault {
                    kind: "input_list".into(),
                    path: "venue-inputs.md".into(),
                    label: None,
                },
            ],
            spaces: vec![Space {
                name: "Main Stage".into(),
                default_files: vec![VenueDefault {
                    kind: "stage_plot".into(),
                    path: "main-stage.pdf".into(),
                    label: Some("Main Stage Plot".into()),
                }],
                ..Default::default()
            }],
            ..Default::default()
        };
        let defaults = loc.effective_defaults(Some("Main Stage"));
        assert_eq!(defaults.len(), 2);
        // Space's stage_plot overrides venue's
        let stage = defaults.iter().find(|d| d.kind == "stage_plot").unwrap();
        assert_eq!(stage.path, "main-stage.pdf");
        // Venue's input_list is inherited
        let inputs = defaults.iter().find(|d| d.kind == "input_list").unwrap();
        assert_eq!(inputs.path, "venue-inputs.md");
    }

    #[test]
    fn effective_defaults_nonexistent_space_returns_venue() {
        let loc = Location {
            name: "Venue".into(),
            default_files: vec![VenueDefault {
                kind: "diagram".into(),
                path: "layout.png".into(),
                label: None,
            }],
            ..Default::default()
        };
        let defaults = loc.effective_defaults(Some("No Such Room"));
        assert_eq!(defaults.len(), 1);
        assert_eq!(defaults[0].kind, "diagram");
    }

    #[test]
    fn location_roundtrip_yaml() {
        let loc = Location {
            id: Some("uuid-1".into()),
            name: "First Baptist Church".into(),
            address1: Some("123 Main St".into()),
            city: Some("Nashville".into()),
            state: Some("TN".into()),
            postal_code: Some("37201".into()),
            country_code: Some("US".into()),
            contact_name: Some("John Doe".into()),
            contact_email: Some("john@example.com".into()),
            venue_type: Some("church".into()),
            spaces: vec![Space {
                id: Some("space-1".into()),
                name: "Sanctuary".into(),
                capacity: Some(500),
                notes: Some("Main worship space".into()),
                ..Default::default()
            }],
            default_files: vec![VenueDefault {
                kind: "stage_plot".into(),
                path: "[[FBC Stage Plot]]".into(),
                label: Some("Default stage plot".into()),
            }],
            tags: vec!["church".into(), "nashville".into()],
            ..Default::default()
        };

        let yaml = facet_yaml::to_string(&loc).expect("serialize");
        let parsed: Location = facet_yaml::from_str(&yaml).expect("deserialize");

        assert_eq!(parsed.id, loc.id);
        assert_eq!(parsed.name, loc.name);
        assert_eq!(parsed.address1, loc.address1);
        assert_eq!(parsed.city, loc.city);
        assert_eq!(parsed.venue_type, loc.venue_type);
        assert_eq!(parsed.spaces.len(), 1);
        assert_eq!(parsed.spaces[0].name, "Sanctuary");
        assert_eq!(parsed.spaces[0].capacity, Some(500));
        assert_eq!(parsed.default_files.len(), 1);
        assert_eq!(parsed.default_files[0].kind, "stage_plot");
        assert_eq!(parsed.tags, loc.tags);
    }

    #[test]
    fn space_roundtrip_yaml() {
        let space = Space {
            id: Some("s-1".into()),
            name: "Studio B".into(),
            capacity: Some(8),
            notes: Some("Isolation booth + control room".into()),
            default_files: vec![VenueDefault {
                kind: "patch_list".into(),
                path: "studio-b-patch.md".into(),
                label: None,
            }],
            tags: vec!["recording".into()],
        };

        let yaml = facet_yaml::to_string(&space).expect("serialize");
        let parsed: Space = facet_yaml::from_str(&yaml).expect("deserialize");

        assert_eq!(parsed.name, "Studio B");
        assert_eq!(parsed.capacity, Some(8));
        assert_eq!(parsed.default_files.len(), 1);
    }
}
