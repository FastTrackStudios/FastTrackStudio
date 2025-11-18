//! Chord duration and rhythm parsing
//!
//! Implements various rhythm notation systems for chords:
//! - Slash syntax (/, //, ///)
//! - Lily-inspired syntax (_4, _8., _2)
//! - Push/Pull notation ('C, C')
//! - Ties and rests (r, s, ~)

use crate::parsing::{ParseError, Token, TokenType};
use crate::time::{MusicalDuration, TimeSignature};
use tracing::instrument;

/// Rhythm notation for a chord
#[derive(Debug, Clone, PartialEq)]
pub enum ChordRhythm {
    /// Default - one bar (implied if no rhythm specified)
    Default,
    /// Slash notation - each slash is one beat (/, //, ///, ////)
    Slashes(u8),
    /// Lily-inspired duration syntax (_4, _8, _2, _1, _4., etc.)
    Lily {
        duration: LilySyntax,
        dotted: bool,
        multiplier: Option<u16>, // For s1*8 syntax
        tied: bool,              // For ~ syntax
    },
    /// Rest (silent) - r4, r8, etc.
    Rest {
        duration: LilySyntax,
        dotted: bool,
        multiplier: Option<u16>,
    },
    /// Space/tacet (invisible) - s4, s8, etc.
    Space {
        duration: LilySyntax,
        dotted: bool,
        multiplier: Option<u16>,
    },
    /// Push notation - anticipate/play earlier ('C, ''C, '''C)
    Push(PushPullAmount),
    /// Pull notation - delay/play later (C', C'', C''')
    Pull(PushPullAmount),
}

/// Lily-inspired duration values
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LilySyntax {
    Whole,        // 1
    Half,         // 2
    Quarter,      // 4
    Eighth,       // 8
    Sixteenth,    // 16
    ThirtySecond, // 32
}

impl LilySyntax {
    /// Get the numeric value (1, 2, 4, 8, 16, 32)
    pub fn value(&self) -> u8 {
        match self {
            LilySyntax::Whole => 1,
            LilySyntax::Half => 2,
            LilySyntax::Quarter => 4,
            LilySyntax::Eighth => 8,
            LilySyntax::Sixteenth => 16,
            LilySyntax::ThirtySecond => 32,
        }
    }

    /// Parse from number string ("1", "2", "4", "8", "16", "32")
    pub fn from_number(s: &str) -> Option<Self> {
        match s {
            "1" => Some(LilySyntax::Whole),
            "2" => Some(LilySyntax::Half),
            "4" => Some(LilySyntax::Quarter),
            "8" => Some(LilySyntax::Eighth),
            "16" => Some(LilySyntax::Sixteenth),
            "32" => Some(LilySyntax::ThirtySecond),
            _ => None,
        }
    }
}

/// Push/Pull amount (number of apostrophes)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PushPullAmount {
    Eighth,       // ' - one apostrophe
    Sixteenth,    // '' - two apostrophes
    ThirtySecond, // ''' - three apostrophes
}

impl PushPullAmount {
    pub fn from_count(count: u8) -> Option<Self> {
        match count {
            1 => Some(PushPullAmount::Eighth),
            2 => Some(PushPullAmount::Sixteenth),
            3 => Some(PushPullAmount::ThirtySecond),
            _ => None,
        }
    }
}

impl ChordRhythm {
    /// Parse chord rhythm from tokens following the chord
    /// Returns (rhythm, tokens_consumed)
    /// Returns error if no valid rhythm notation is found
    #[instrument(level = "debug", skip(tokens), fields(token_count = tokens.len()))]
    pub fn parse(tokens: &[Token]) -> Result<(Self, usize), ParseError> {
        if tokens.is_empty() {
            return Err(ParseError::NoValidParser {
                context: "No rhythm tokens found".to_string(),
            });
        }

        // Check for push notation (leading apostrophes before we get here are handled by chord parser)
        // This handles trailing apostrophes for pull notation
        if let TokenType::Apostrophe = tokens[0].token_type {
            let count = Self::count_apostrophes(tokens);
            if let Some(amount) = PushPullAmount::from_count(count as u8) {
                return Ok((ChordRhythm::Pull(amount), count));
            }
        }

        // Check for underscore (Lily syntax) or slash syntax
        match &tokens[0].token_type {
            TokenType::Underscore => {
                // Lily duration: _4, _8., _2, etc.
                let consumed = 1;
                let (rhythm, tokens_used) = Self::parse_lily_duration(&tokens[consumed..])?;
                Ok((rhythm, consumed + tokens_used))
            }

            TokenType::Slash => {
                // Slash notation: /, //, ///, ////, or with dots: /., //., etc.
                let slash_count = Self::count_slashes(tokens);

                // Check if there's a dot after the slashes for dotted rhythm
                let has_dot = if slash_count < tokens.len() {
                    matches!(tokens[slash_count].token_type, TokenType::Dot)
                } else {
                    false
                };

                if has_dot {
                    // Convert dotted slashes to Lily notation
                    // / = quarter note, so /. = dotted quarter
                    // // = half note, so //. = dotted half
                    // etc.
                    let duration = match slash_count {
                        1 => LilySyntax::Quarter,
                        2 => LilySyntax::Half,
                        3 => LilySyntax::Half, // Three quarters, treat as dotted half
                        4 => LilySyntax::Whole,
                        _ => LilySyntax::Quarter,
                    };
                    Ok((
                        ChordRhythm::Lily {
                            duration,
                            dotted: true,
                            multiplier: None,
                            tied: false,
                        },
                        slash_count + 1, // +1 for the dot
                    ))
                } else {
                    Ok((ChordRhythm::Slashes(slash_count as u8), slash_count))
                }
            }

            TokenType::Letter('r') | TokenType::Letter('s') => {
                // Rest or Space: r4, s8, r_4., s1*8
                Self::parse_rest_or_space(tokens)
            }

            _ => {
                // No rhythm tokens recognized
                Err(ParseError::NoValidParser {
                    context: "No valid rhythm notation found".to_string(),
                })
            }
        }
    }

    /// Parse Lily duration after underscore: _4, _8., _2~, etc.
    /// `tokens` should be the slice starting AFTER the underscore
    fn parse_lily_duration(tokens: &[Token]) -> Result<(Self, usize), ParseError> {
        let mut consumed = 0;

        if tokens.is_empty() {
            return Err(ParseError::NoValidParser {
                context: "Expected duration after underscore".to_string(),
            });
        }

        // Parse the duration number
        let duration = match &tokens[consumed].token_type {
            TokenType::Number(n) => {
                LilySyntax::from_number(n).ok_or(ParseError::NoValidParser {
                    context: format!("Invalid Lily duration: {}", n),
                })?
            }
            _ => {
                return Err(ParseError::NoValidParser {
                    context: "Expected duration number after underscore".to_string(),
                });
            }
        };
        consumed += 1;

        // Check for dot
        let dotted = if consumed < tokens.len() {
            if matches!(tokens[consumed].token_type, TokenType::Dot) {
                consumed += 1;
                true
            } else {
                false
            }
        } else {
            false
        };

        // Check for tie (~)
        let tied = if consumed < tokens.len() {
            if matches!(tokens[consumed].token_type, TokenType::Tilde) {
                consumed += 1;
                true
            } else {
                false
            }
        } else {
            false
        };

        // Check for multiplier (*8)
        let multiplier = if consumed < tokens.len() {
            if matches!(tokens[consumed].token_type, TokenType::Asterisk) {
                consumed += 1;
                if consumed < tokens.len() {
                    if let TokenType::Number(n) = &tokens[consumed].token_type {
                        consumed += 1;
                        Some(n.parse::<u16>().unwrap_or(1))
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };

        Ok((
            ChordRhythm::Lily {
                duration,
                dotted,
                multiplier,
                tied,
            },
            consumed,
        ))
    }

    /// Parse rest (r) or space (s): r4, s8, r_4., s1*8
    fn parse_rest_or_space(tokens: &[Token]) -> Result<(Self, usize), ParseError> {
        let mut consumed = 0;

        let is_rest = matches!(tokens[0].token_type, TokenType::Letter('r'));
        consumed += 1;

        // Check for underscore (optional)
        let has_underscore = if consumed < tokens.len() {
            matches!(tokens[consumed].token_type, TokenType::Underscore)
        } else {
            false
        };

        if has_underscore {
            consumed += 1;
        }

        // Parse duration number
        if consumed >= tokens.len() {
            return Err(ParseError::NoValidParser {
                context: "Expected duration after rest/space".to_string(),
            });
        }

        let duration = match &tokens[consumed].token_type {
            TokenType::Number(n) => {
                LilySyntax::from_number(n).ok_or(ParseError::NoValidParser {
                    context: format!("Invalid duration: {}", n),
                })?
            }
            _ => {
                return Err(ParseError::NoValidParser {
                    context: "Expected duration number".to_string(),
                });
            }
        };
        consumed += 1;

        // Check for dot
        let dotted = if consumed < tokens.len() {
            if matches!(tokens[consumed].token_type, TokenType::Dot) {
                consumed += 1;
                true
            } else {
                false
            }
        } else {
            false
        };

        // Check for multiplier (*8)
        let multiplier = if consumed < tokens.len() {
            if matches!(tokens[consumed].token_type, TokenType::Asterisk) {
                consumed += 1;
                if consumed < tokens.len() {
                    if let TokenType::Number(n) = &tokens[consumed].token_type {
                        consumed += 1;
                        Some(n.parse::<u16>().unwrap_or(1))
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };

        if is_rest {
            Ok((
                ChordRhythm::Rest {
                    duration,
                    dotted,
                    multiplier,
                },
                consumed,
            ))
        } else {
            Ok((
                ChordRhythm::Space {
                    duration,
                    dotted,
                    multiplier,
                },
                consumed,
            ))
        }
    }

    /// Count consecutive slashes
    fn count_slashes(tokens: &[Token]) -> usize {
        let mut count = 0;
        for token in tokens {
            if matches!(token.token_type, TokenType::Slash) {
                count += 1;
            } else {
                break;
            }
        }
        count
    }

    /// Count consecutive apostrophes
    fn count_apostrophes(tokens: &[Token]) -> usize {
        let mut count = 0;
        for token in tokens {
            if matches!(token.token_type, TokenType::Apostrophe) {
                count += 1;
            } else {
                break;
            }
        }
        count
    }

    /// Convert to MusicalDuration based on time signature
    pub fn to_duration(&self, time_sig: TimeSignature) -> MusicalDuration {
        match self {
            ChordRhythm::Default => {
                // Default is one full measure
                MusicalDuration::new(1, 0, 0)
            }
            ChordRhythm::Slashes(count) => {
                // Each slash is one beat (in the time signature denominator)
                MusicalDuration::from_beats(*count as f64, time_sig)
            }
            ChordRhythm::Lily {
                duration,
                dotted,
                multiplier,
                ..
            } => {
                let base_beats = Self::lily_to_beats(*duration, *dotted, time_sig);
                let mult = multiplier.unwrap_or(1) as f64;
                let total_beats = base_beats * mult;
                MusicalDuration::from_beats(total_beats, time_sig)
            }
            ChordRhythm::Rest {
                duration,
                dotted,
                multiplier,
            } => {
                let base_beats = Self::lily_to_beats(*duration, *dotted, time_sig);
                let mult = multiplier.unwrap_or(1) as f64;
                let total_beats = base_beats * mult;
                MusicalDuration::from_beats(total_beats, time_sig)
            }
            ChordRhythm::Space {
                duration,
                dotted,
                multiplier,
            } => {
                let base_beats = Self::lily_to_beats(*duration, *dotted, time_sig);
                let mult = multiplier.unwrap_or(1) as f64;
                let total_beats = base_beats * mult;
                MusicalDuration::from_beats(total_beats, time_sig)
            }
            ChordRhythm::Push(amount) | ChordRhythm::Pull(amount) => {
                // Push/pull adjusts by subdivision amount
                let subdivision_beats = match amount {
                    PushPullAmount::Eighth => 0.5,
                    PushPullAmount::Sixteenth => 0.25,
                    PushPullAmount::ThirtySecond => 0.125,
                };
                // For now, return the subdivision amount
                // TODO: This should adjust the chord's position, not just duration
                MusicalDuration::from_beats(subdivision_beats, time_sig)
            }
        }
    }

    /// Convert Lily syntax duration to beats
    fn lily_to_beats(duration: LilySyntax, dotted: bool, time_sig: TimeSignature) -> f64 {
        // The denominator tells us what note value gets one beat
        // For 4/4: quarter note = 1 beat
        // For 6/8: eighth note = 1 beat

        // First, calculate how many 32nd notes this duration has
        let thirty_seconds = match duration {
            LilySyntax::Whole => 32.0,
            LilySyntax::Half => 16.0,
            LilySyntax::Quarter => 8.0,
            LilySyntax::Eighth => 4.0,
            LilySyntax::Sixteenth => 2.0,
            LilySyntax::ThirtySecond => 1.0,
        };

        let actual_thirty_seconds = if dotted {
            thirty_seconds * 1.5
        } else {
            thirty_seconds
        };

        // Now convert to beats based on the time signature denominator
        // denominator = 4 means quarter note = 1 beat (8 thirty-seconds)
        // denominator = 8 means eighth note = 1 beat (4 thirty-seconds)
        let thirty_seconds_per_beat = 32.0 / time_sig.denominator as f64;

        actual_thirty_seconds / thirty_seconds_per_beat
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parsing::Lexer;

    #[test]
    fn test_parse_slash_rhythm() {
        let mut lexer = Lexer::new("////".to_string());
        let tokens = lexer.tokenize();
        let (rhythm, consumed) = ChordRhythm::parse(&tokens).unwrap();

        assert_eq!(rhythm, ChordRhythm::Slashes(4));
        assert_eq!(consumed, 4);
    }

    #[test]
    fn test_parse_lilypond_quarter() {
        let mut lexer = Lexer::new("_4".to_string());
        let tokens = lexer.tokenize();
        let (rhythm, consumed) = ChordRhythm::parse(&tokens).unwrap();

        match rhythm {
            ChordRhythm::Lily {
                duration, dotted, ..
            } => {
                assert_eq!(duration, LilySyntax::Quarter);
                assert!(!dotted);
            }
            _ => panic!("Expected Lily rhythm"),
        }
        assert_eq!(consumed, 2);
    }

    #[test]
    fn test_parse_lilypond_dotted() {
        let mut lexer = Lexer::new("_4.".to_string());
        let tokens = lexer.tokenize();
        let (rhythm, _consumed) = ChordRhythm::parse(&tokens).unwrap();

        match rhythm {
            ChordRhythm::Lily {
                duration, dotted, ..
            } => {
                assert_eq!(duration, LilySyntax::Quarter);
                assert!(dotted);
            }
            _ => panic!("Expected Lily rhythm"),
        }
    }

    #[test]
    fn test_parse_rest() {
        let mut lexer = Lexer::new("r4".to_string());
        let tokens = lexer.tokenize();
        let (rhythm, _) = ChordRhythm::parse(&tokens).unwrap();

        match rhythm {
            ChordRhythm::Rest { duration, .. } => {
                assert_eq!(duration, LilySyntax::Quarter);
            }
            _ => panic!("Expected Rest rhythm"),
        }
    }

    #[test]
    fn test_parse_space_with_multiplier() {
        let mut lexer = Lexer::new("s1*8".to_string());
        let tokens = lexer.tokenize();
        let (rhythm, _) = ChordRhythm::parse(&tokens).unwrap();

        match rhythm {
            ChordRhythm::Space {
                duration,
                multiplier,
                ..
            } => {
                assert_eq!(duration, LilySyntax::Whole);
                assert_eq!(multiplier, Some(8));
            }
            _ => panic!("Expected Space rhythm with multiplier"),
        }
    }

    #[test]
    fn test_no_rhythm_returns_error() {
        let tokens = vec![];
        let result = ChordRhythm::parse(&tokens);

        assert!(result.is_err());
    }
}
