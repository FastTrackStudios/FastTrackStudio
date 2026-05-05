use facet::Facet;

/// Canonical production stages for a FastTrackAudio music project.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Facet)]
#[repr(u8)]
pub enum MusicProjectStep {
    #[default]
    Writing,
    Recording,
    Production,
    Editing,
    Mixing,
    Mastering,
    Approved,
    Released,
}

impl MusicProjectStep {
    /// Ordered list of all workflow stages.
    pub const fn all() -> [Self; 8] {
        [
            Self::Writing,
            Self::Recording,
            Self::Production,
            Self::Editing,
            Self::Mixing,
            Self::Mastering,
            Self::Approved,
            Self::Released,
        ]
    }

    /// Advance to the next workflow stage, if any.
    pub const fn next(self) -> Option<Self> {
        match self {
            Self::Writing => Some(Self::Recording),
            Self::Recording => Some(Self::Production),
            Self::Production => Some(Self::Editing),
            Self::Editing => Some(Self::Mixing),
            Self::Mixing => Some(Self::Mastering),
            Self::Mastering => Some(Self::Approved),
            Self::Approved => Some(Self::Released),
            Self::Released => None,
        }
    }

    /// Move one step backward.
    pub const fn previous(self) -> Option<Self> {
        match self {
            Self::Writing => None,
            Self::Recording => Some(Self::Writing),
            Self::Production => Some(Self::Recording),
            Self::Editing => Some(Self::Production),
            Self::Mixing => Some(Self::Editing),
            Self::Mastering => Some(Self::Mixing),
            Self::Approved => Some(Self::Mastering),
            Self::Released => Some(Self::Approved),
        }
    }

    /// True when the project has reached a finished state.
    pub const fn is_complete(self) -> bool {
        matches!(self, Self::Approved | Self::Released)
    }
}

impl std::fmt::Display for MusicProjectStep {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Writing => "Writing",
            Self::Recording => "Recording",
            Self::Production => "Production",
            Self::Editing => "Editing",
            Self::Mixing => "Mixing",
            Self::Mastering => "Mastering",
            Self::Approved => "Approved",
            Self::Released => "Released",
        })
    }
}
