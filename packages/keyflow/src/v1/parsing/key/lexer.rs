//! Key signature lexer for parsing key signature strings

use crate::parsing::common::token::{Token, TokenType};
use std::{iter::Peekable, str::Chars};

pub struct KeyLexer {
    tokens: Vec<Token>,
    current: usize,
    input_len: usize,
}

impl KeyLexer {
    pub fn new() -> Self {
        Self {
            tokens: Vec::new(),
            current: 0,
            input_len: 0,
        }
    }

    pub fn scan_tokens(&mut self, source: &str) -> Vec<Token> {
        self.input_len = source.len();
        let mut iter = source.chars().peekable();
        
        while !self.is_at_end() {
            self.scan_token(&mut iter);
        }
        
        self.add_token(TokenType::Eof, self.current + 1, 0);
        let res = self.tokens.clone();
        
        
        self.tokens.clear();
        self.current = 0;
        res
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.input_len
    }

    fn scan_token(&mut self, chars: &mut Peekable<Chars>) {
        let c = self.advance(chars);
        match c {
            None => (),
            Some(c) => match c {
                ' ' => (), // Skip whitespace
                ',' => self.add_token(TokenType::Comma, self.current, 1),
                '[' => self.add_token(TokenType::LBracket, self.current, 1),
                ']' => self.add_token(TokenType::RBracket, self.current, 1),
                c => {
                    if c.is_numeric() {
                        let pos = self.current;
                        let mut literal = String::from(c);
                        while let Some(&next) = chars.peek() {
                            if next.is_numeric() {
                                literal.push(self.advance(chars).unwrap());
                            } else {
                                break;
                            }
                        }
                        let len = literal.len();
                        self.add_token(TokenType::Extension(literal), pos, len);
                    } else if self.is_alphabetic(&c) {
                        // Check if this could be a note with accidental first
                        if matches!(c, 'A'..='G' | 'a'..='g') {
                            let pos = self.current;
                            let mut note_str = String::from(c);
                            
                            // Check if next character is a sharp or flat
                            if let Some(&next) = chars.peek() {
                                if matches!(next, '#' | '♯' | 'b' | '♭') {
                                    note_str.push(self.advance(chars).unwrap());
                                    // Try to parse as note name
                                    if let Some(note_token) = self.parse_note_name(&note_str) {
                                        self.add_token(note_token, pos, note_str.len());
                                    } else {
                                        // If not a valid note, treat as regular alphabetic string
                                        let mut literal = String::from(c);
                                        literal.push(note_str.chars().nth(1).unwrap());
                                        self.parse_string(&literal, pos);
                                    }
                                } else {
                                    // No accidental, treat as regular alphabetic string
                                    let mut literal = String::from(c);
                                    while let Some(&next) = chars.peek() {
                                        if self.is_alphabetic(&next) {
                                            literal.push(self.advance(chars).unwrap());
                                        } else {
                                            break;
                                        }
                                    }
                                    self.parse_string(&literal, pos);
                                }
                            } else {
                                // No next character, treat as regular alphabetic string
                                let mut literal = String::from(c);
                                while let Some(&next) = chars.peek() {
                                    if self.is_alphabetic(&next) {
                                        literal.push(self.advance(chars).unwrap());
                                    } else {
                                        break;
                                    }
                                }
                                self.parse_string(&literal, pos);
                            }
                        } else {
                            // Not a note letter, treat as regular alphabetic string
                            let pos = self.current;
                            let mut literal = String::from(c);
                            while let Some(&next) = chars.peek() {
                                if self.is_alphabetic(&next) {
                                    literal.push(self.advance(chars).unwrap());
                                } else {
                                    break;
                                }
                            }
                            self.parse_string(&literal, pos);
                        }
                    } else {
                        // Handle other characters (like #, b, etc.)
                        self.parse_note_with_accidental(c, chars);
                    }
                }
            },
        }
    }

    fn parse_string(&mut self, s: &str, pos: usize) {
        // First check if it's a note name
        if let Some(note_token) = self.parse_note_name(s) {
            self.add_token(note_token, pos, s.len());
            return;
        }

        // Then check if it's a scale name
        if let Some(scale_token) = self.parse_scale_name(s) {
            self.add_token(scale_token, pos, s.len());
            return;
        }

        // If neither, it's illegal
        self.add_token(TokenType::Illegal, pos, s.len());
    }

    fn parse_note_with_accidental(&mut self, c: char, chars: &mut Peekable<Chars>) {
        // Check if this could be a note with accidental
        if matches!(c, 'A'..='G' | 'a'..='g') {
            let pos = self.current;
            let mut note_str = String::from(c);
            
            // Check if next character is a sharp or flat
            if let Some(&next) = chars.peek() {
                if matches!(next, '#' | '♯' | 'b' | '♭') {
                    note_str.push(self.advance(chars).unwrap());
                }
            }
            
            // Try to parse as note name
            if let Some(note_token) = self.parse_note_name(&note_str) {
                self.add_token(note_token, pos, note_str.len());
                return;
            }
        }
        
        // If not a note, treat as illegal
        self.add_token(TokenType::Illegal, self.current, 1);
    }

    fn parse_note_name(&self, s: &str) -> Option<TokenType> {
        match s.to_uppercase().as_str() {
            "A" => Some(TokenType::Note("A".to_string())),
            "B" => Some(TokenType::Note("B".to_string())),
            "C" => Some(TokenType::Note("C".to_string())),
            "D" => Some(TokenType::Note("D".to_string())),
            "E" => Some(TokenType::Note("E".to_string())),
            "F" => Some(TokenType::Note("F".to_string())),
            "G" => Some(TokenType::Note("G".to_string())),
            "A#" | "A♯" => Some(TokenType::Note("A#".to_string())),
            "B#" | "B♯" => Some(TokenType::Note("B#".to_string())),
            "C#" | "C♯" => Some(TokenType::Note("C#".to_string())),
            "D#" | "D♯" => Some(TokenType::Note("D#".to_string())),
            "E#" | "E♯" => Some(TokenType::Note("E#".to_string())),
            "F#" | "F♯" => Some(TokenType::Note("F#".to_string())),
            "G#" | "G♯" => Some(TokenType::Note("G#".to_string())),
            "AB" | "A♭" => Some(TokenType::Note("Ab".to_string())),
            "BB" | "B♭" => Some(TokenType::Note("Bb".to_string())),
            "CB" | "C♭" => Some(TokenType::Note("Cb".to_string())),
            "DB" | "D♭" => Some(TokenType::Note("Db".to_string())),
            "EB" | "E♭" => Some(TokenType::Note("Eb".to_string())),
            "FB" | "F♭" => Some(TokenType::Note("Fb".to_string())),
            "GB" | "G♭" => Some(TokenType::Note("Gb".to_string())),
            _ => None,
        }
    }

    fn parse_scale_name(&self, s: &str) -> Option<TokenType> {
        match s.to_lowercase().as_str() {
            // Diatonic modes
            "major" | "ionian" => Some(TokenType::ScaleName("major".to_string())),
            "minor" | "aeolian" => Some(TokenType::ScaleName("minor".to_string())),
            "dorian" => Some(TokenType::ScaleName("dorian".to_string())),
            "phrygian" => Some(TokenType::ScaleName("phrygian".to_string())),
            "lydian" => Some(TokenType::ScaleName("lydian".to_string())),
            "mixolydian" => Some(TokenType::ScaleName("mixolydian".to_string())),
            "locrian" => Some(TokenType::ScaleName("locrian".to_string())),
            
            // Melodic minor modes
            "melodicminor" | "melodic_minor" => Some(TokenType::ScaleName("melodicminor".to_string())),
            "dorianb2" | "dorian_b2" => Some(TokenType::ScaleName("dorianb2".to_string())),
            "lydianaugmented" | "lydian_augmented" => Some(TokenType::ScaleName("lydianaugmented".to_string())),
            "lydiandominant" | "lydian_dominant" => Some(TokenType::ScaleName("lydiandominant".to_string())),
            "mixolydianb6" | "mixolydian_b6" => Some(TokenType::ScaleName("mixolydianb6".to_string())),
            "aeolianb5" | "aeolian_b5" => Some(TokenType::ScaleName("aeolianb5".to_string())),
            "superlocrian" | "super_locrian" | "altered" => Some(TokenType::ScaleName("superlocrian".to_string())),
            
            // Harmonic minor modes
            "harmonicminor" | "harmonic_minor" => Some(TokenType::ScaleName("harmonicminor".to_string())),
            "locriannat6" | "locrian_nat_6" => Some(TokenType::ScaleName("locriannat6".to_string())),
            "ionianaugmented" | "ionian_augmented" => Some(TokenType::ScaleName("ionianaugmented".to_string())),
            "romaniandorian" | "romanian_dorian" => Some(TokenType::ScaleName("romaniandorian".to_string())),
            "phrygiandominant" | "phrygian_dominant" => Some(TokenType::ScaleName("phrygiandominant".to_string())),
            "lydian#9" | "lydian_sharp_9" => Some(TokenType::ScaleName("lydian#9".to_string())),
            "ultralocrian" | "ultra_locrian" => Some(TokenType::ScaleName("ultralocrian".to_string())),
            
            _ => None,
        }
    }

    fn is_alphabetic(&self, c: &char) -> bool {
        c.is_ascii_alphabetic()
    }

    fn advance(&mut self, chars: &mut Peekable<Chars>) -> Option<char> {
        if let Some(c) = chars.next() {
            self.current += 1;
            Some(c)
        } else {
            None
        }
    }

    fn add_token(&mut self, token_type: TokenType, pos: usize, len: usize) {
        self.tokens.push(Token::new(token_type, pos, len));
    }
}
