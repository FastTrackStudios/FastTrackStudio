use std::collections::HashMap;

use crate::primitives::{
    intervals::Interval,
    note::{Note, NoteLiteral},
};
use crate::chord::chord::{
    ChordQuality, SeventhType, Extension
};
use super::expressions::{BassExp, OmitExp};

use super::expression::Exp;
use crate::parsing::common::ParserError;


#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Ast {
    pub(crate) root: Note,
    pub(crate) bass: Option<Note>,
    pub(crate) expressions: Vec<Exp>,
    pub(crate) intervals: Vec<Interval>,
    pub(crate) is_sus: bool,
    pub(crate) is_power: bool,
    pub(crate) errors: Vec<ParserError>,
}

impl Ast {
    fn set_intervals(&mut self) {
        self.expressions.sort();
        self.expressions.iter().for_each(|e| match e {
            Exp::Minor(min) => min.execute(&mut self.intervals, &self.expressions),
            Exp::Dim7(dim) => dim.execute(&mut self.intervals, &self.expressions),
            Exp::Dim(dim) => dim.execute(&mut self.intervals, &self.expressions),
            Exp::HalfDim(half) => half.execute(&mut self.intervals, &self.expressions),
            Exp::Sus(sus) => {
                sus.execute(&mut self.intervals);
                self.is_sus = true;
            }
            Exp::Maj(maj) => maj.execute(&mut self.intervals, &self.expressions),
            Exp::Extension(ext) => {
                ext.execute(&mut self.intervals, &mut self.is_sus, &self.expressions)
            }
            Exp::Add(add) => add.execute(&mut self.intervals),
            Exp::Aug(aug) => aug.execute(&mut self.intervals, &self.expressions),
            Exp::SlashBass(bass) => self.bass = Some(bass.note.clone()),
            Exp::Alt(alt) => alt.execute(&mut self.intervals),
            Exp::Power(pw) => {
                if self.expressions.len() != 1 {
                    self.errors.push(ParserError::Chord(crate::parsing::chord::ChordParserError::InvalidPowerExpression));
                } else {
                    pw.execute(&mut self.intervals);
                    self.is_power = true;
                }
            }
            Exp::Bass(_) => (),
            _ => (),
        });

        self.add_third();
        self.add_five();
        self.intervals.sort_by_key(|i| i.st());
    }

    fn add_third(&mut self) {
        // Don't add third if we already have one, or if it's suspended, or if it's a power chord
        let has_third = self.intervals.contains(&Interval::MajorThird) || self.intervals.contains(&Interval::MinorThird);
        let has_omit_third = self.expressions.iter().any(|exp| {
            matches!(exp, Exp::Omit(OmitExp { interval: Interval::MajorThird, .. }))
        });
        
        if !has_third && !self.is_sus && !self.is_power && !has_omit_third {
            self.intervals.push(Interval::MajorThird);
        }
    }

    fn add_five(&mut self) {
        if !self.intervals.contains(&Interval::DiminishedFifth)
            && !self.intervals.contains(&Interval::PerfectFifth)
            && !self.intervals.contains(&Interval::AugmentedFifth)
            && !self.intervals.contains(&Interval::FlatThirteenth)
            && !self.expressions.iter().any(|exp| {
                matches!(
                    exp,
                    Exp::Omit(OmitExp {
                        interval: Interval::PerfectFifth,
                        ..
                    }) | Exp::Bass(BassExp)
                )
            })
        {
            self.intervals.push(Interval::PerfectFifth);
        }
    }

    /// Checks if there are any three consecutive semitones, which are illegal.
    fn validate_semitones(&mut self) -> bool {
        let mut is_valid = true;
        let mut count = 0u16;
        let mut intervals = [None; 12];

        for s in self.intervals.iter() {
            let pos = s.st() % 12;
            count |= 1 << pos;
            intervals[pos as usize] = Some(s);
        }

        for i in 0..12 {
            let a = (i + 1) % 12;
            let b = (i + 2) % 12;
            if (count & (1 << i) != 0) && (count & (1 << a) != 0) && (count & (1 << b) != 0) {
                is_valid = false;
                self.errors
                    .push(ParserError::Chord(crate::parsing::chord::ChordParserError::ThreeConsecutiveSemitones(vec![
                        format!("{}", intervals[i].unwrap()),
                        format!("{}", intervals[a].unwrap()),
                        format!("{}", intervals[b].unwrap()),
                    ])));
            }
        }

        is_valid
    }

    fn has_inconsistent_extension(&self, int: &Interval, matches: Vec<&Interval>) -> bool {
        for i in matches {
            if self.intervals.contains(i) && self.intervals.contains(int) {
                return true;
            }
        }
        false
    }

    /// Finds illegal extensions combinations (for example 9 and b9/#9)
    fn has_inconsistent_extensions(&mut self) -> bool {
        if self.has_inconsistent_extension(
            &Interval::Ninth,
            vec![&Interval::FlatNinth, &Interval::SharpNinth],
        ) {
            self.errors.push(ParserError::Chord(crate::parsing::chord::ChordParserError::InconsistentExtension(
                Interval::Ninth.to_string(),
            )));
            return true;
        }
        if self.has_inconsistent_extension(&Interval::Eleventh, vec![&Interval::SharpEleventh]) {
            self.errors.push(ParserError::Chord(crate::parsing::chord::ChordParserError::InconsistentExtension(
                Interval::Eleventh.to_string(),
            )));
            return true;
        }
        if self.has_inconsistent_extension(&Interval::Thirteenth, vec![&Interval::FlatThirteenth]) {
            self.errors.push(ParserError::Chord(crate::parsing::chord::ChordParserError::InconsistentExtension(
                Interval::Thirteenth.to_string(),
            )));
            return true;
        }
        if self.has_inconsistent_extension(&Interval::MajorSixth, vec![&Interval::MinorSixth]) {
            self.errors.push(ParserError::Chord(crate::parsing::chord::ChordParserError::InconsistentExtension(
                Interval::MajorSixth.to_string(),
            )));
            return true;
        }
        if self.has_inconsistent_extension(&Interval::MajorThird, vec![&Interval::MinorThird]) {
            self.errors.push(ParserError::Chord(crate::parsing::chord::ChordParserError::InconsistentExtension(
                Interval::MajorThird.to_string(),
            )));
            return true;
        }
        false
    }

    /// Validates extensions finding for duplicates and incosistencies.
    fn validate_extensions(&mut self) -> bool {
        let mut ext_count = [0; 24];
        let filtered = self
            .expressions
            .iter()
            .filter(|exp| matches!(exp, Exp::Extension(_)));
        for ext in filtered {
            if let Exp::Extension(ext) = ext {
                let index = ext.interval.st() as usize;
                match ext.interval {
                    Interval::MinorSecond
                    | Interval::MajorSecond
                    | Interval::MinorThird
                    | Interval::MajorThird
                    | Interval::DiminishedSeventh
                    | Interval::MajorSeventh => {
                        self.errors.push(ParserError::Chord(crate::parsing::chord::ChordParserError::InvalidExtension(ext.pos)));
                        return false;
                    }
                    _ => (),
                }
                if ext_count[index] > 0 {
                    self.errors.push(ParserError::Chord(crate::parsing::chord::ChordParserError::DuplicateExtension(ext.pos)));
                    return false;
                }
                ext_count[index] += 1;
            }
        }
        !self.has_inconsistent_extensions()
    }

    /// Validates expressions both individually and finding illegal duplicates
    fn validate_expressions(&mut self) -> bool {
        let mut is_valid = true;
        let mut target_pos;
        let mut counts: HashMap<u32, usize> = HashMap::new();
        for exp in &self.expressions {
            (is_valid, target_pos) = exp.validate();
            if !is_valid {
                self.errors
                    .push(ParserError::Chord(crate::parsing::chord::ChordParserError::WrongExpressionTarget(target_pos)));
                return false;
            }
            let key = match exp {
                Exp::Extension(_) | Exp::Add(_) | Exp::Omit(_) => u32::MAX,
                _ => exp.priority(),
            };
            *counts.entry(key).or_insert(0) += 1;
        }

        for (key, count) in counts {
            if key < u32::MAX && count > 1 {
                self.errors
                    .push(ParserError::Chord(crate::parsing::chord::ChordParserError::DuplicateModifier(Exp::from_priority(key))));
                return false;
            }
        }
        is_valid
    }

    /// Analizes expressions and intervals finding inconsistencies.  
    /// If any inconcistence is found, self.errors is populated and false is returned.
    fn is_valid(&mut self) -> bool {
        let valid_exp = self.validate_expressions();
        let valid_ext = self.validate_extensions();
        let valid_sem = self.validate_semitones();
        valid_exp && valid_ext && valid_sem && self.errors.is_empty()
    }


    pub fn get_descriptor(&mut self, name: &str) -> String {
        let modifier_str = match &self.root.modifier {
            Some(m) => m.to_string(),
            None => "".to_string(),
        };
        name.replace(&format!("{}{}", self.root.literal, modifier_str), "")
    }

    pub(crate) fn build_chord(&mut self, _name: &str) -> Result<crate::chord::chord::ChordData, crate::parsing::common::ParserErrors> {
        self.set_intervals();

        if !self.is_valid() {
            return Err(crate::parsing::common::ParserErrors::new(self.errors.clone()));
        }

        // Analyze the chord structure to determine quality, seventh, and extensions
        let chord_quality = self.determine_chord_quality();
        let seventh_type = self.determine_seventh_type();
        let extensions = self.determine_extensions();

        // Build the chord using a helper function that creates a ChordData
        let chord_data = self.build_chord_data(chord_quality, seventh_type, extensions);

        Ok(chord_data)
    }

    fn build_chord_data(
        &self,
        _quality: ChordQuality,
        _seventh: Option<SeventhType>,
        _extensions: Vec<Extension>,
    ) -> crate::chord::chord::ChordData {
        // Use the intervals that were built by the expressions
        let mut intervals = self.intervals.clone();
        
        // Remove the Unison interval as it's not needed in the final chord data
        intervals.retain(|&interval| interval != Interval::Unison);

        // Apply other modifiers (omissions, etc.)
        // Note: SusExp is already processed during execution, so we don't need to process it again here
        for exp in &self.expressions {
            match exp {
                Exp::Omit(omit_exp) => {
                    intervals.retain(|&interval| interval != omit_exp.interval);
                },
                _ => {}
            }
        }

        // Create ChordData directly
        crate::chord::chord::ChordData {
            root: self.root.clone(),
            intervals,
            bass: self.bass.clone(),
        }
    }



    fn determine_chord_quality(&self) -> ChordQuality {
        // Check expressions for quality indicators
        for exp in &self.expressions {
            match exp {
                Exp::Power(_) => return ChordQuality::Power,
                Exp::Minor(_) => return ChordQuality::Minor,
                Exp::Dim(_) | Exp::Dim7(_) => return ChordQuality::Diminished,
                Exp::Aug(_) => return ChordQuality::Augmented,
                Exp::Maj(_) => return ChordQuality::Major,
                _ => {}
            }
        }

        // Check intervals for quality indicators
        if self.intervals.contains(&Interval::MinorThird) {
            ChordQuality::Minor
        } else if self.intervals.contains(&Interval::DiminishedFifth) {
            ChordQuality::Diminished
        } else if self.intervals.contains(&Interval::AugmentedFifth) {
            ChordQuality::Augmented
        } else {
            ChordQuality::Major // Default to major
        }
    }

    fn determine_seventh_type(&self) -> Option<SeventhType> {
        if self.intervals.contains(&Interval::MajorSeventh) {
            Some(SeventhType::Major7)
        } else if self.intervals.contains(&Interval::MinorSeventh) {
            // Check if it's dominant (major chord with minor 7th) or minor 7th
            if self.determine_chord_quality() == ChordQuality::Major {
                Some(SeventhType::Dominant7)
            } else {
                Some(SeventhType::Minor7)
            }
        } else if self.intervals.contains(&Interval::DiminishedSeventh) {
            Some(SeventhType::Diminished7)
        } else {
            None
        }
    }

    fn determine_extensions(&self) -> Vec<Extension> {
        let mut extensions = Vec::new();
        
        // Check for extensions (9th, 11th, 13th)
        if self.intervals.contains(&Interval::Ninth) {
            extensions.push(Extension::Ninth);
        }
        if self.intervals.contains(&Interval::Eleventh) {
            extensions.push(Extension::Eleventh);
        }
        if self.intervals.contains(&Interval::Thirteenth) {
            extensions.push(Extension::Thirteenth);
        }

        // Check for add extensions
        for exp in &self.expressions {
            if let Exp::Add(add_exp) = exp {
                match add_exp.interval {
                    Interval::Ninth => extensions.push(Extension::Ninth),
                    Interval::Eleventh => extensions.push(Extension::Eleventh),
                    Interval::Thirteenth => extensions.push(Extension::Thirteenth),
                    Interval::MajorSixth => extensions.push(Extension::Ninth), // 6th chords are handled differently
                    _ => {}
                }
            }
        }

        extensions
    }

}

impl Default for Ast {
    fn default() -> Ast {
        Ast {
            root: Note::new(NoteLiteral::C, None),
            bass: None,
            expressions: Vec::new(),
            intervals: vec![Interval::Unison],
            is_sus: false,
            is_power: false,
            errors: Vec::new(),
        }
    }
}
