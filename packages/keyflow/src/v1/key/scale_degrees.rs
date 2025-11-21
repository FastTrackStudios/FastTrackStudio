//! Nashville-style scale degree helpers (key-agnostic for now)

use crate::primitives::intervals::{Interval, SemInterval};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScaleDegree {
    One,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
}

impl ScaleDegree {
    pub fn from_semantic_interval(sem: SemInterval) -> Option<Self> {
        match sem {
            SemInterval::Root => Some(ScaleDegree::One),
            SemInterval::Second => Some(ScaleDegree::Two),
            SemInterval::Third => Some(ScaleDegree::Three),
            SemInterval::Fourth => Some(ScaleDegree::Four),
            SemInterval::Fifth => Some(ScaleDegree::Five),
            SemInterval::Sixth => Some(ScaleDegree::Six),
            SemInterval::Seventh => Some(ScaleDegree::Seven),
            SemInterval::Ninth => Some(ScaleDegree::Two),
            SemInterval::Eleventh => Some(ScaleDegree::Four),
            SemInterval::Thirteenth => Some(ScaleDegree::Six),
        }
    }

    pub fn numeric(self) -> u8 {
        match self {
            ScaleDegree::One => 1,
            ScaleDegree::Two => 2,
            ScaleDegree::Three => 3,
            ScaleDegree::Four => 4,
            ScaleDegree::Five => 5,
            ScaleDegree::Six => 6,
            ScaleDegree::Seven => 7,
        }
    }

    pub fn annotate_interval(i: Interval) -> (Self, &'static str) {
        let sem = i.to_semantic_interval();
        let deg = Self::from_semantic_interval(sem).unwrap_or(ScaleDegree::One);
        // accidental string for degree: b, #, etc. Based on semitone offset within semantic class
        let accidental = match i {
            Interval::MinorSecond | Interval::FlatNinth => "b",
            Interval::AugmentedSecond | Interval::SharpNinth => "#",
            Interval::MinorThird => "b",
            Interval::AugmentedThird => "#",
            Interval::AugmentedFourth | Interval::DiminishedFifth => "#", // 4# / b5
            Interval::AugmentedFifth | Interval::MinorSixth => "#",       // 5# / b6
            Interval::DiminishedSeventh => "bb",
            Interval::AugmentedSixth => "#",
            Interval::AugmentedSeventh => "#",
            _ => "",
        };
        (deg, accidental)
    }
}

/// Convert an Interval to a scale-degree string limited to 1..=7
/// Folds 9→2, 11→4, 13→6 and preserves accidentals (b/#/bb) relative to the degree.
pub fn interval_to_degree_string(i: Interval) -> String {
    let (deg, accidental) = ScaleDegree::annotate_interval(i);
    format!("{}{}", accidental, deg.numeric())
}

/// Build a scale-degree string (1..7) with an alteration amount
/// alter: -2=bb, -1=b, 0=natural, +1=#, +2=##
pub fn degree_with_alteration(degree: u8, alter: i8) -> Option<String> {
    if !(1..=7).contains(&degree) { return None; }
    let acc = match alter {
        -2 => "bb",
        -1 => "b",
        0 => "",
        1 => "#",
        2 => "##",
        _ => return None,
    };
    Some(format!("{}{}", acc, degree))
}



