//! Section Definition
//!
//! Represents a section with its type and optional numbering

use super::section_type::SectionType;

/// Represents a section with its type and optional number
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Section {
    pub section_type: SectionType,
    pub number: Option<u32>,
    pub split_letter: Option<char>, // For consecutive same sections (1a, 1b, etc.)
    pub measure_count: Option<usize>, // Specified measure count
    pub is_subsection: bool,        // True if prefixed with ^ (e.g., ^Band-In)
}

impl Section {
    pub fn new(section_type: SectionType) -> Self {
        Self {
            section_type,
            number: None,
            split_letter: None,
            measure_count: None,
            is_subsection: false,
        }
    }

    pub fn with_subsection(mut self, is_subsection: bool) -> Self {
        self.is_subsection = is_subsection;
        self
    }

    pub fn with_measure_count(mut self, count: usize) -> Self {
        self.measure_count = Some(count);
        self
    }

    /// Get a display name for this section
    pub fn display_name(&self) -> String {
        let base = self.section_type.full_name();
        let prefix = if self.is_subsection { "^" } else { "" };

        match (self.number, self.split_letter) {
            (Some(n), Some(l)) => format!("{}{} {}{}", prefix, base, n, l),
            (Some(n), None) => format!("{}{} {}", prefix, base, n),
            (None, _) => format!("{}{}", prefix, base),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_section() {
        let section = Section::new(SectionType::Verse);
        assert_eq!(section.section_type, SectionType::Verse);
        assert_eq!(section.number, None);
        assert_eq!(section.split_letter, None);
        assert_eq!(section.measure_count, None);
    }

    #[test]
    fn test_with_measure_count() {
        let section = Section::new(SectionType::Chorus).with_measure_count(8);
        assert_eq!(section.measure_count, Some(8));
    }

    #[test]
    fn test_display_name_no_number() {
        let section = Section::new(SectionType::Intro);
        assert_eq!(section.display_name(), "Intro");
    }

    #[test]
    fn test_display_name_with_number() {
        let mut section = Section::new(SectionType::Verse);
        section.number = Some(1);
        assert_eq!(section.display_name(), "Verse 1");
    }

    #[test]
    fn test_display_name_with_split_letter() {
        let mut section = Section::new(SectionType::Verse);
        section.number = Some(1);
        section.split_letter = Some('a');
        assert_eq!(section.display_name(), "Verse 1a");
    }

    #[test]
    fn test_section_clone() {
        let section1 = Section::new(SectionType::Bridge).with_measure_count(4);
        let section2 = section1.clone();
        assert_eq!(section1, section2);
    }
}
