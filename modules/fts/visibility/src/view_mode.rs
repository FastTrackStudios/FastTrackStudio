//! View mode for group visibility behavior

/// Defines how multiple groups interact when activated
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ViewMode {
    /// Multiple groups can be active simultaneously
    /// Each group independently shows/hides its tracks
    Toggle,
    
    /// Only one group can be active at a time
    /// Activating a group hides all other groups' tracks first
    Exclusive,
    
    /// Similar to Exclusive but more subtle
    /// All tracks minimized, only active group unfolded
    LimitedExclusive,
}

impl ViewMode {
    pub fn as_str(&self) -> &'static str {
        match self {
            ViewMode::Toggle => "Toggle",
            ViewMode::Exclusive => "Exclusive",
            ViewMode::LimitedExclusive => "LimitedExclusive",
        }
    }
    
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "Toggle" => Some(ViewMode::Toggle),
            "Exclusive" => Some(ViewMode::Exclusive),
            "LimitedExclusive" => Some(ViewMode::LimitedExclusive),
            _ => None,
        }
    }
}

impl Default for ViewMode {
    fn default() -> Self {
        ViewMode::Toggle
    }
}

