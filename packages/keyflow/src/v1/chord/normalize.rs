use super::chord::{ChordStruct, ChordQualityMarker, SeventhMarker, ExtensionMarker};
use crate::primitives::intervals::Interval;
use colored::*;

/// Normalize a chord to its display string representation
pub fn normalize_chord<Q: ChordQualityMarker, S: SeventhMarker, E: ExtensionMarker>(chord: &ChordStruct<Q, S, E>) -> String {
    normalize_chord_internal(chord, false)
}

pub fn normalize_chord_colored<Q: ChordQualityMarker, S: SeventhMarker, E: ExtensionMarker>(chord: &ChordStruct<Q, S, E>) -> String {
    normalize_chord_internal(chord, true)
}

fn normalize_chord_internal<Q: ChordQualityMarker, S: SeventhMarker, E: ExtensionMarker>(chord: &ChordStruct<Q, S, E>, use_colors: bool) -> String {
    let mut res = if use_colors {
        chord.root.to_string().bright_cyan().to_string()
    } else {
        chord.root.to_string()
    };
    
    // Handle power chords
    if chord.intervals.len() == 1 && chord.intervals.contains(&Interval::PerfectFifth) {
        if use_colors {
            res.push_str(&"5".bright_magenta().to_string());
        } else {
            res.push('5');
        }
        if let Some(bass) = &chord.bass {
            res.push('/');
            if use_colors {
                res.push_str(&bass.to_string().bright_green().to_string());
            } else {
                res.push_str(&bass.to_string());
            }
        }
        return res;
    }
    
    // Handle 6th chords specially - they handle their own quality
    let has_sixth = chord.intervals.contains(&Interval::MajorSixth);
    let has_ninth = chord.intervals.contains(&Interval::Ninth);
    let has_major_third = chord.intervals.contains(&Interval::MajorThird);
    let has_minor_third = chord.intervals.contains(&Interval::MinorThird);
    
    // Check if this is a suspended chord first
    // A chord is suspended if it has a 2nd or 4th but NO third
    let has_third = chord.intervals.contains(&Interval::MajorThird) || 
                    chord.intervals.contains(&Interval::MinorThird);
    let has_sus = (chord.intervals.contains(&Interval::PerfectFourth) ||
                   chord.intervals.contains(&Interval::MajorSecond)) && !has_third;
    
    if has_sus {
        // Suspended chord - handle normally, 6th will be added as "add6"
        // Add modifier (7, 9, 11, 13) - this is the main extension
        let modifier = get_modifier(chord);
        if let Some(modifier_interval) = modifier {
            if use_colors {
                res.push_str(&modifier_interval.to_chord_notation().bright_magenta().to_string());
            } else {
                res.push_str(&modifier_interval.to_chord_notation());
            }
        }
        // Add "sus" to indicate it's a suspended chord
        // For sus2, be explicit. For sus4, just show "sus" (default)
        if chord.intervals.contains(&Interval::MajorSecond) {
            if use_colors {
                res.push_str(&"sus2".bright_blue().to_string());
            } else {
                res.push_str("sus2");
            }
        } else {
            // Default to sus (which is sus4)
            if use_colors {
                res.push_str(&"sus".bright_blue().to_string());
            } else {
                res.push_str("sus");
            }
        }
    } else {
        // Check for "add" chords (chords with 2nd/4th AND third)
        let has_add_second = chord.intervals.contains(&Interval::MajorSecond) && has_third;
        let has_add_fourth = chord.intervals.contains(&Interval::PerfectFourth) && has_third;
        
        if has_add_second || has_add_fourth {
            // This is an "add" chord - handle the base quality first
            let base_quality = determine_base_quality(chord);
            if use_colors {
                res.push_str(&base_quality.bright_yellow().to_string());
            } else {
                res.push_str(&base_quality);
            }
            
            // Add modifier (7, 9, 11, 13) - this is the main extension
            let modifier = get_modifier(chord);
            if let Some(modifier_interval) = modifier {
                if use_colors {
                    res.push_str(&modifier_interval.to_chord_notation().bright_magenta().to_string());
                } else {
                    res.push_str(&modifier_interval.to_chord_notation());
                }
            }
            
            // Add the "add" notation
            if has_add_second {
                if use_colors {
                    res.push_str(&"(add2)".bright_blue().to_string());
                } else {
                    res.push_str("(add2)");
                }
            } else if has_add_fourth {
                if use_colors {
                    res.push_str(&"(add4)".bright_blue().to_string());
                } else {
                    res.push_str("(add4)");
                }
            }
        } else if has_sixth {
        // Only handle 6th chords if it's not a suspended chord
        // 6th chords handle their own quality, don't call determine_base_quality
        if has_ninth {
            // 6/9 chord
            if has_major_third && !has_minor_third {
                // Major 6/9 chord - show as "6/9" (no "maj" prefix)
                if use_colors {
                    res.push_str(&"6/9".bright_magenta().to_string());
                } else {
                    res.push_str("6/9");
                }
            } else if has_minor_third && !has_major_third {
                // Minor 6/9 chord - show as "min6/9"
                if use_colors {
                    res.push_str(&"m".bright_yellow().to_string());
                    res.push_str(&"6/9".bright_magenta().to_string());
                } else {
                    res.push_str("m6/9");
                }
            } else {
                // Just 6/9 - show as "6/9"
                if use_colors {
                    res.push_str(&"6/9".bright_magenta().to_string());
                } else {
                    res.push_str("6/9");
                }
            }
        } else {
            // Just 6th chord
            if has_major_third && !has_minor_third {
                // Major 6th chord - show as "maj6"
                if use_colors {
                    res.push_str(&"maj".bright_yellow().to_string());
                    res.push_str(&"6".bright_magenta().to_string());
                } else {
                    res.push_str("maj6");
                }
            } else if has_minor_third && !has_major_third {
                // Minor 6th chord - show as "min6"
                if use_colors {
                    res.push_str(&"m".bright_yellow().to_string());
                    res.push_str(&"6".bright_magenta().to_string());
                } else {
                    res.push_str("m6");
                }
            } else {
                // Just 6th - show as "6"
                if use_colors {
                    res.push_str(&"6".bright_magenta().to_string());
                } else {
                    res.push_str("6");
                }
            }
        }
    } else {
        // Determine base chord quality (Major, Minor, Diminished, Augmented) for non-6th chords
        let base_quality = determine_base_quality(chord);
        if use_colors {
            res.push_str(&base_quality.bright_yellow().to_string());
        } else {
            res.push_str(&base_quality);
        }

        // Check if this is an altered chord (has both b5 and #5) - handle before modifier
        let is_altered = chord.intervals.contains(&Interval::DiminishedFifth) && 
                        chord.intervals.contains(&Interval::AugmentedFifth);
        
        if is_altered {
            // Use "alt" shorthand for altered chords - skip modifier
            if use_colors {
                res.push_str(&"alt".bright_red().to_string());
            } else {
                res.push_str("alt");
            }
        } else {
            // Add modifier (7, 9, 11, 13) - this is the main extension
            let modifier = get_modifier(chord);
            if let Some(modifier_interval) = modifier {
            let modifier_str = modifier_interval.to_chord_notation();
            // Special handling for "maj" in major 7th chords
            if modifier_str == "maj7" {
                if use_colors {
                    res.push_str(&"maj".bright_yellow().to_string());
                    res.push_str(&"7".bright_magenta().to_string());
                } else {
                    res.push_str(&modifier_str);
                }
            } else if modifier_str == "bb7" && chord.intervals.contains(&Interval::DiminishedFifth) {
                // Special handling for diminished 7th chords - show as "7" not "bb7"
                if use_colors {
                    res.push_str(&"7".bright_magenta().to_string());
                } else {
                    res.push_str("7");
                }
            } else if chord.intervals.contains(&Interval::MajorSeventh) && 
                      (modifier_str == "9" || modifier_str == "11" || modifier_str == "13") {
                // Special handling for major 7th chords with extensions - add "maj" prefix
                if use_colors {
                    res.push_str(&"maj".bright_yellow().to_string());
                    res.push_str(&modifier_str.bright_magenta().to_string());
                } else {
                    res.push_str("maj");
                    res.push_str(&modifier_str);
                }
            } else {
                if use_colors {
                    res.push_str(&modifier_str.bright_magenta().to_string());
                } else {
                    res.push_str(&modifier_str);
                }
            }
        }
        } // Close the else block for non-altered chords
    }
    }
    
    // Add sus if needed (only if not already handled by the main suspended block)
    // This is for cases like C7sus, where the 7th is the modifier and sus is an addition
    if !has_sus && (chord.intervals.contains(&Interval::PerfectFourth) ||
                    chord.intervals.contains(&Interval::MajorSecond)) &&
                    !(chord.intervals.contains(&Interval::MajorThird) ||
                      chord.intervals.contains(&Interval::MinorThird)) {
        // For sus2, be explicit. For sus4, just show "sus" (default)
        if chord.intervals.contains(&Interval::MajorSecond) {
            if use_colors {
                res.push_str(&"sus2".bright_blue().to_string());
            } else {
                res.push_str("sus2");
            }
        } else {
            // Default to sus (which is sus4)
            if use_colors {
                res.push_str(&"sus".bright_blue().to_string());
            } else {
                res.push_str("sus");
            }
        }
    }
    
    // Add alterations and adds in parentheses (only for non-altered chords)
    let is_altered = chord.intervals.contains(&Interval::DiminishedFifth) && 
                    chord.intervals.contains(&Interval::AugmentedFifth);
    
    if !is_altered {
        let alterations = if use_colors { get_alterations_colored(chord) } else { get_alterations(chord) };
        let adds = if use_colors { get_adds_colored(chord) } else { get_adds(chord) };
        let omits = if use_colors { get_omits_colored(chord) } else { get_omits(chord) };

        let mut extras = Vec::new();
        extras.extend(alterations);
        extras.extend(adds);
        extras.extend(omits);

        if !extras.is_empty() {
            res.push('(');
            res.push_str(&extras.join(","));
            res.push(')');
        }
    }
    
    // Add slash bass
    if let Some(bass) = &chord.bass {
        res.push('/');
        if use_colors {
            res.push_str(&bass.to_string().bright_green().to_string());
        } else {
            res.push_str(&bass.to_string());
        }
    }

    res
}

fn determine_base_quality<Q: ChordQualityMarker, S: SeventhMarker, E: ExtensionMarker>(chord: &ChordStruct<Q, S, E>) -> String {
    let is_minor = chord.intervals.contains(&Interval::MinorThird);
    let is_diminished = chord.intervals.contains(&Interval::DiminishedFifth);
    let is_augmented = chord.intervals.contains(&Interval::AugmentedFifth);
    
    // Handle power chords (just root + fifth, no third)
    let has_third = chord.intervals.contains(&Interval::MajorThird) || chord.intervals.contains(&Interval::MinorThird);
    let has_fifth = chord.intervals.contains(&Interval::PerfectFifth);
    let has_seventh = chord.intervals.contains(&Interval::MajorSeventh) ||
                     chord.intervals.contains(&Interval::MinorSeventh) ||
                     chord.intervals.contains(&Interval::DiminishedSeventh);
    let has_extensions = chord.intervals.iter().any(|&interval| {
        matches!(interval, 
            Interval::Ninth | Interval::FlatNinth | Interval::SharpNinth |
            Interval::Eleventh | Interval::SharpEleventh |
            Interval::Thirteenth | Interval::FlatThirteenth |
            Interval::MajorSixth)
    });
    
    if has_fifth && !has_third && !has_seventh && !has_extensions {
        return "5".to_string();
    }
    
    // Handle diminished chords first (minor third + diminished fifth, and no major third)
    // But only if it's not a minor 7th chord with flat 5th (which should be "min7" not "dim")
    if is_diminished && is_minor && !chord.intervals.contains(&Interval::MajorThird) {
        // If it has a minor 7th (not diminished 7th), it's a minor 7th chord with flat 5th
        if chord.intervals.contains(&Interval::MinorSeventh) {
            return "m".to_string();
        }
        // Otherwise it's a diminished chord (triad or diminished 7th)
        return "dim".to_string();
    }
    
    // Handle minor chords (including minor 7th chords with alterations)
    if is_minor {
        return "m".to_string();
    }
    
    // Handle augmented chords (major third + augmented fifth, and no minor third, no other alterations)
    if is_augmented && chord.intervals.contains(&Interval::MajorThird) && !is_minor {
        // Only pure augmented chords (no 7th, no extensions, no alterations)
        let has_alterations = chord.intervals.contains(&Interval::DiminishedFifth);
        
        if !has_seventh && !has_extensions && !has_alterations {
            return "+".to_string();
        }
    }
    
    // Major chords (including dominant) - just return empty string
    "".to_string()
}

/// Get the main modifier (7, 9, 11, 13) for the chord
fn get_modifier<Q: ChordQualityMarker, S: SeventhMarker, E: ExtensionMarker>(chord: &ChordStruct<Q, S, E>) -> Option<Interval> {
    let has_seventh = chord.intervals.contains(&Interval::MajorSeventh) ||
                     chord.intervals.contains(&Interval::MinorSeventh) ||
                     chord.intervals.contains(&Interval::DiminishedSeventh);
    let is_diminished = chord.intervals.contains(&Interval::DiminishedFifth);

    // For pure diminished chords (not minor 7th with flat 5th), look for extensions first, then sevenths
    if is_diminished && !chord.intervals.contains(&Interval::MinorSeventh) {
        // 13th implies 9th and 11th (only natural extensions count)
        if chord.intervals.contains(&Interval::Thirteenth) {
            let has_ninth = chord.intervals.contains(&Interval::Ninth);
            let has_eleventh = chord.intervals.contains(&Interval::Eleventh);
            if has_ninth && has_eleventh {
                return Some(Interval::Thirteenth);
            }
        }
        // 11th implies 9th (only natural extensions count)
        if chord.intervals.contains(&Interval::Eleventh) {
            let has_ninth = chord.intervals.contains(&Interval::Ninth);
            if has_ninth {
                return Some(Interval::Eleventh);
            }
        }
        // Just 9th (only natural extensions count)
        if chord.intervals.contains(&Interval::Ninth) {
            return Some(Interval::Ninth);
        }
        // Check for diminished seventh
        if chord.intervals.contains(&Interval::DiminishedSeventh) {
            return Some(Interval::DiminishedSeventh);
        }
        return None;
    }

    // For seventh chords, determine the main extension
    if has_seventh {
        // Check if this is a major 7th chord with extensions
        let has_major_seventh = chord.intervals.contains(&Interval::MajorSeventh);
        
        // 13th implies 9th and 11th (only natural extensions count)
        if chord.intervals.contains(&Interval::Thirteenth) {
            let has_ninth = chord.intervals.contains(&Interval::Ninth);
            let has_eleventh = chord.intervals.contains(&Interval::Eleventh);
            if has_ninth && has_eleventh {
                return Some(Interval::Thirteenth);
            }
        }
        // 11th implies 9th (only natural extensions count)
        if chord.intervals.contains(&Interval::Eleventh) {
            let has_ninth = chord.intervals.contains(&Interval::Ninth);
            if has_ninth {
                return Some(Interval::Eleventh);
            }
        }
        // Just 9th (only natural extensions count)
        if chord.intervals.contains(&Interval::Ninth) {
            return Some(Interval::Ninth);
        }
        // Default to 7th
        if has_major_seventh {
            return Some(Interval::MajorSeventh);
        } else if chord.intervals.contains(&Interval::MinorSeventh) {
            return Some(Interval::MinorSeventh);
        } else if chord.intervals.contains(&Interval::DiminishedSeventh) {
            return Some(Interval::DiminishedSeventh);
        }
        return None;
    }

    // For triads, look for 6th
    if chord.intervals.contains(&Interval::MajorSixth) {
        // Check if this is a major 6th chord (has major third)
        let is_major = chord.intervals.contains(&Interval::MajorThird);
        if is_major {
            // For major 6th chords, we need to return a special marker
            // We'll handle this in the display logic
            return Some(Interval::MajorSixth);
        } else {
            // For minor 6th chords, just return the 6th
            return Some(Interval::MajorSixth);
        }
    }

    None
}

/// Get alterations (b5, #5, b9, #9, #11, b13, etc.)
fn get_alterations<Q: ChordQualityMarker, S: SeventhMarker, E: ExtensionMarker>(chord: &ChordStruct<Q, S, E>) -> Vec<String> {
    let mut alterations = Vec::new();
    
    // Check if this is a pure diminished or augmented chord (base quality, not alterations)
    let is_pure_diminished = chord.intervals.contains(&Interval::DiminishedFifth) && 
                            chord.intervals.contains(&Interval::MinorThird) &&
                            !chord.intervals.contains(&Interval::MajorThird) &&
                            !chord.intervals.contains(&Interval::AugmentedFifth) &&
                            !chord.intervals.contains(&Interval::MinorSeventh) &&
                            !chord.intervals.contains(&Interval::MajorSeventh); // Pure diminished (triad or dim7), no min7 or maj7
    let is_pure_augmented = chord.intervals.contains(&Interval::AugmentedFifth) && 
                           chord.intervals.contains(&Interval::MajorThird) &&
                           !chord.intervals.contains(&Interval::MinorThird) &&
                           !chord.intervals.contains(&Interval::DiminishedFifth) &&
                           !chord.intervals.contains(&Interval::MinorSeventh) &&
                           !chord.intervals.contains(&Interval::MajorSeventh) &&
                           !chord.intervals.contains(&Interval::DiminishedSeventh); // Pure augmented triad, no 7th
    
    // 5th alterations (but not for pure diminished/augmented chords where it's part of the base quality)
    // Special case: altered chords can have both b5 and #5 simultaneously
    if chord.intervals.contains(&Interval::DiminishedFifth) && !is_pure_diminished {
        alterations.push("b5".to_string());
    }
    if chord.intervals.contains(&Interval::AugmentedFifth) && !is_pure_augmented {
        alterations.push("#5".to_string());
    }
    
    // 9th alterations
    if chord.intervals.contains(&Interval::FlatNinth) {
        alterations.push("b9".to_string());
    }
    if chord.intervals.contains(&Interval::SharpNinth) {
        alterations.push("#9".to_string());
    }
    
    // 11th alterations
    if chord.intervals.contains(&Interval::SharpEleventh) {
        alterations.push("#11".to_string());
    }
    
    // 13th alterations
    if chord.intervals.contains(&Interval::FlatThirteenth) {
        alterations.push("b13".to_string());
    }
    
    alterations
}

/// Get additions (add9, add11, add13, etc.)
fn get_adds<Q: ChordQualityMarker, S: SeventhMarker, E: ExtensionMarker>(chord: &ChordStruct<Q, S, E>) -> Vec<String> {
    let mut adds = Vec::new();
    let has_seventh = chord.intervals.contains(&Interval::MajorSeventh) ||
                     chord.intervals.contains(&Interval::MinorSeventh) ||
                     chord.intervals.contains(&Interval::DiminishedSeventh);

    // Check for add6 (6th without 7th, but not for pure 6th chords)
    let has_sixth = chord.intervals.contains(&Interval::MajorSixth);
    let has_major_third = chord.intervals.contains(&Interval::MajorThird);
    let has_minor_third = chord.intervals.contains(&Interval::MinorThird);
    let is_pure_sixth_chord = has_sixth && !has_seventh && 
                             ((has_major_third && !has_minor_third) || 
                              (has_minor_third && !has_major_third));
    
    if has_sixth && !has_seventh && !is_pure_sixth_chord {
        adds.push("add6".to_string());
    }

    // Check for incomplete extension sequences
    if has_seventh {
        // If we have 13th but missing 9th or 11th, it's an add13
        if chord.intervals.contains(&Interval::Thirteenth) {
            let has_ninth = chord.intervals.contains(&Interval::Ninth) ||
                           chord.intervals.contains(&Interval::FlatNinth) ||
                           chord.intervals.contains(&Interval::SharpNinth);
            let has_eleventh = chord.intervals.contains(&Interval::Eleventh) ||
                              chord.intervals.contains(&Interval::SharpEleventh);
            if !has_ninth || !has_eleventh {
                adds.push("add13".to_string());
            }
        }
        // If we have 11th but missing 9th, it's an add11 (but not for sharp 11th, which is an alteration)
        else if chord.intervals.contains(&Interval::Eleventh) {
            let has_ninth = chord.intervals.contains(&Interval::Ninth) ||
                           chord.intervals.contains(&Interval::FlatNinth) ||
                           chord.intervals.contains(&Interval::SharpNinth);
            if !has_ninth {
                adds.push("add11".to_string());
            }
        }
    } else {
        // For triads, any extension is an "add" (but not for 6/9 chords)
        let is_6_9_chord = has_sixth && chord.intervals.contains(&Interval::Ninth);
        if !is_6_9_chord && (chord.intervals.contains(&Interval::Ninth) ||
           chord.intervals.contains(&Interval::FlatNinth) ||
           chord.intervals.contains(&Interval::SharpNinth)) {
            adds.push("add9".to_string());
        }
        if chord.intervals.contains(&Interval::Eleventh) || chord.intervals.contains(&Interval::SharpEleventh) {
            adds.push("add11".to_string());
        }
        if chord.intervals.contains(&Interval::Thirteenth) {
            adds.push("add13".to_string());
        }
    }

    adds
}

/// Get omitted notes
fn get_omits<Q: ChordQualityMarker, S: SeventhMarker, E: ExtensionMarker>(chord: &ChordStruct<Q, S, E>) -> Vec<String> {
    let mut omits = Vec::new();
    
    let has_sus = chord.intervals.contains(&Interval::PerfectFourth) ||
                  chord.intervals.contains(&Interval::MajorSecond);
    let has_third = chord.intervals.contains(&Interval::MajorThird) || 
                   chord.intervals.contains(&Interval::MinorThird);
    
    // Check for omitted 3rd (but not for suspended chords or 11th chords)
    let has_eleventh = chord.intervals.contains(&Interval::Eleventh) || 
                      chord.intervals.contains(&Interval::SharpEleventh);
    if !has_third && !has_sus && !has_eleventh {
        omits.push("omit3".to_string());
    }
    
    // Check for omitted 5th
    if !chord.intervals.contains(&Interval::PerfectFifth) && 
       !chord.intervals.contains(&Interval::DiminishedFifth) && 
       !chord.intervals.contains(&Interval::AugmentedFifth) {
        omits.push("omit5".to_string());
    }
    
    omits
}

// Colored helper functions for the to_colored_string method
pub fn determine_chord_quality_colored<Q: ChordQualityMarker, S: SeventhMarker, E: ExtensionMarker>(chord: &ChordStruct<Q, S, E>) -> (String, bool) {
    let base_quality = determine_base_quality(chord);
    let has_sus = chord.intervals.contains(&Interval::PerfectFourth) ||
                  chord.intervals.contains(&Interval::MajorSecond);
    let has_third = chord.intervals.contains(&Interval::MajorThird) || 
                   chord.intervals.contains(&Interval::MinorThird);
    let is_sus = has_sus && !has_third;
    (base_quality, is_sus)
}

pub fn get_modifier_colored<Q: ChordQualityMarker, S: SeventhMarker, E: ExtensionMarker>(chord: &ChordStruct<Q, S, E>) -> Option<Interval> {
    get_modifier(chord)
}

pub fn get_alterations_colored<Q: ChordQualityMarker, S: SeventhMarker, E: ExtensionMarker>(chord: &ChordStruct<Q, S, E>) -> Vec<String> {
    get_alterations(chord).into_iter().map(|alt| alt.bright_red().to_string()).collect()
}

pub fn get_adds_colored<Q: ChordQualityMarker, S: SeventhMarker, E: ExtensionMarker>(chord: &ChordStruct<Q, S, E>) -> Vec<String> {
    get_adds(chord).into_iter().map(|add| add.bright_green().to_string()).collect()
}

pub fn get_omits_colored<Q: ChordQualityMarker, S: SeventhMarker, E: ExtensionMarker>(chord: &ChordStruct<Q, S, E>) -> Vec<String> {
    get_omits(chord).into_iter().map(|omit| omit.bright_yellow().to_string()).collect()
}
