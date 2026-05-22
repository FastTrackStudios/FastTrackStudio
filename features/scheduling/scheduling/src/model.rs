//! Frontmatter-kind discriminator.
//!
//! Every scheduling markdown file carries `type: scheduling-*` in
//! frontmatter so the scanner can route the page to the right
//! parser without inspecting other fields. Mirrors how `task`
//! discriminates with `type: task`.

/// Which scheduling entity a markdown page represents.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FrontmatterKind {
    DayTemplate,
    EventType,
    Schedule,
    Booking,
}

impl FrontmatterKind {
    pub const fn frontmatter_value(self) -> &'static str {
        match self {
            Self::DayTemplate => "scheduling-day-template",
            Self::EventType => "scheduling-event-type",
            Self::Schedule => "scheduling-schedule",
            Self::Booking => "scheduling-booking",
        }
    }
}

/// Return the kind a frontmatter `type` string maps to, if any.
pub fn frontmatter_kind(ty: &str) -> Option<FrontmatterKind> {
    Some(match ty {
        "scheduling-day-template" => FrontmatterKind::DayTemplate,
        "scheduling-event-type" => FrontmatterKind::EventType,
        "scheduling-schedule" => FrontmatterKind::Schedule,
        "scheduling-booking" => FrontmatterKind::Booking,
        _ => return None,
    })
}
