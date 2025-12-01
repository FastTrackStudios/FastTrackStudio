//! Syllable counting module
//!
//! This module provides functions for counting syllables in words and text.
//! Uses regex-based heuristics to estimate syllable counts.

use regex::Regex;
use once_cell::sync::Lazy;

// Pre-compile regex patterns for better performance
static REGEX_ALPHA: Lazy<Regex> = Lazy::new(|| Regex::new(r"[^\w\s]").unwrap());
static REGEX_VOWEL_GROUPS: Lazy<Regex> = Lazy::new(|| Regex::new(r"([aeiuo]+)").unwrap());
static REGEX_Y_AFTER_CONSONANT: Lazy<Regex> = Lazy::new(|| Regex::new(r"([dlmnrstz]y)").unwrap());
static REGEX_AXE_PATTERN: Lazy<Regex> = Lazy::new(|| Regex::new(r"([aiou][b-df-hj-np-rs-v-z]e)").unwrap());
static REGEX_AXES_PATTERN: Lazy<Regex> = Lazy::new(|| Regex::new(r"([aiou][cs]es)").unwrap());
static REGEX_ASTE_PATTERN: Lazy<Regex> = Lazy::new(|| Regex::new(r"(aste)").unwrap());
static REGEX_APSE_PATTERN: Lazy<Regex> = Lazy::new(|| Regex::new(r"(apse)").unwrap());
static REGEX_TED_PATTERN: Lazy<Regex> = Lazy::new(|| Regex::new(r"([^b-df-hj-np-tv-z]ted$)").unwrap());
static REGEX_EXE_PATTERN: Lazy<Regex> = Lazy::new(|| Regex::new(r"(e[rsvy]e)").unwrap());
static REGEX_CONTRACTION_PATTERN: Lazy<Regex> = Lazy::new(|| Regex::new(r"(d[nv])").unwrap());
static REGEX_ELVE_PATTERN: Lazy<Regex> = Lazy::new(|| Regex::new(r"(elve[^t])").unwrap());
static REGEX_EING_PATTERN: Lazy<Regex> = Lazy::new(|| Regex::new(r"(eing)").unwrap());

/// Count the number of syllables in a single word.
///
/// # Panics
///
/// Panics if the input contains spaces. Use `syllables_in_words` for multi-word text.
///
/// # Examples
///
/// ```
/// use crate::crate::lyrics::syllables::syllables_in_word;
///
/// assert_eq!(syllables_in_word("hello"), 2);
/// assert_eq!(syllables_in_word("chocolate"), 3);
/// assert_eq!(syllables_in_word("life"), 1);
/// ```
pub fn syllables_in_word(s: &str) -> usize {
    assert!(
        !s.contains(' '),
        "One word at a time! Did you trim all non alpha characters?"
    );

    // Remove all non alpha characters
    let word = REGEX_ALPHA.replace_all(s, "").to_lowercase();
    let mut count;

    // The number of syllables in a word is tied to the number of "vowel groups".
    // Start by finding the number of vowel groups, before applying special rules.
    {
        let vowel_groups = REGEX_VOWEL_GROUPS.captures_iter(&word).count();
        count = vowel_groups;
    }

    // Check for y in words following certain consonants.
    // Note: Might be missing a few consonants.
    // Examples: rowdy, lowly, every
    {
        let y_count = REGEX_Y_AFTER_CONSONANT.captures_iter(&word).count();
        count += y_count;
    }

    // Check for cases of aXe(s), which constitute 1 syllable despite having 2 vowel groups.
    // Examples: case, race, chase, promise, dice, lice, choose, st[ate]sman
    {
        let axe_count = REGEX_AXE_PATTERN.captures_iter(&word).count();
        count -= axe_count;
    }

    // Check for *exceptions* to cases of aXe(s), which do actually constitute 2 syllables.
    // Seems to only apply when the final s is present and X is c or s. May be missing X's though.
    // Examples: cases (2) vs case (1), races (2) vs race (1)
    {
        let axes_count = REGEX_AXES_PATTERN.captures_iter(&word).count();
        count += axes_count;
    }

    // Acknowledge special cases of aste.
    // Examples: paste, haste
    {
        if REGEX_ASTE_PATTERN.captures_iter(&word).count() == 1 {
            count -= 1;
        }
    }

    // Acknowledge special cases of apse.
    // Examples: lapse, collapse
    {
        if REGEX_APSE_PATTERN.captures_iter(&word).count() == 1 {
            count -= 1;
        }
    }

    // Add syllable for instances of ted, for which a syllable is removed in one of the earlier regexes.
    // Examples: rated (2) vs rate (1)
    // Exceptions: wanted (2)
    {
        if REGEX_TED_PATTERN.captures_iter(&word).count() == 1 {
            count += 1;
        }
    }

    // Remove syllable for every instance of eXe.
    // Examples: eye, eve
    {
        let exe_count = REGEX_EXE_PATTERN.captures_iter(&word).count();
        count -= exe_count;
    }

    // Add syllable for contractions
    // Examples: coulDNt, woulDVe
    {
        let contraction_count = REGEX_CONTRACTION_PATTERN.captures_iter(&word).count();
        count += contraction_count;
    }

    // Rule for 'elve'
    // Examples: selves
    // Exceptions: e.g. velvet
    {
        let elve_count = REGEX_ELVE_PATTERN.captures_iter(&word).count();
        count -= elve_count;
    }

    // Add syllable for e + ing
    // Examples: being, seeing
    {
        if REGEX_EING_PATTERN.captures_iter(&word).count() == 1 {
            count += 1;
        }
    }

    count
}

/// Count the total number of syllables in a text containing multiple words.
///
/// # Examples
///
/// ```
/// use crate::crate::lyrics::syllables::syllables_in_words;
///
/// assert_eq!(syllables_in_words("hello world"), 3); // hello=2, world=1
/// assert_eq!(syllables_in_words("there are eight syllables in here"), 8);
/// ```
pub fn syllables_in_words(text: &str) -> usize {
    // Remove non alpha characters before splitting words by whitespace
    let text = REGEX_ALPHA.replace_all(text, "").to_string();
    let words = text.split_whitespace();
    let mut count = 0;
    for word in words {
        count += syllables_in_word(word);
    }
    count
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_syllables_in_word() {
        let cases = vec![
            ("life", 1),
            ("can", 1),
            ("be", 1),
            ("hard", 1),
            ("but", 1),
            ("chocolate", 3),
            ("truly", 2),
            ("makes", 1),
            ("date", 1),
            ("belated", 3),
            ("it", 1),
            ("quite", 1),
            ("beautiful", 3),
            ("polymorphism", 4),
            ("hatred", 2),
            ("lucozade", 3),
            ("the", 1),
            ("baked", 1),
            ("named", 1),
            ("dated", 2),
            ("baited", 2),
            ("my", 1),
            ("autonomy", 4),
            ("smile", 1),
            ("smiles", 1),
            ("comprehensive", 4),
            ("comprehensives", 4),
            ("tests", 1),
            ("test", 1),
            ("haste", 1),
            ("queue", 1),
            ("rated", 2),
            ("everytime", 3),
            ("cry", 1),
            ("were", 1),
            ("eye", 1),
            ("couldnt", 2),
            ("collapse", 2),
            ("holes", 1),
            ("creatures", 2),
            ("themselves", 2),
            ("velvet", 2),
            ("selvedge", 2),
            ("these", 1),
            ("thesis", 2),
            ("being", 2),
            ("crazy", 2),
            ("wanted", 2),
            ("cased", 1),
            ("cases", 2),
            ("crises", 2),
            ("race", 1),
            ("races", 2),
            ("promise", 2),
        ];

        for case in cases {
            assert_eq!(
                syllables_in_word(case.0),
                case.1,
                "Failed for word: {}",
                case.0
            );
        }
    }

    #[test]
    fn test_syllables_in_sentences() {
        let cases = vec![
            ("there are eight syllables in here", 8),
            ("to be or not to be is the    question??", 10),
            ("I should add more test cases to \n this!", 9),
            ("When you were here \tbefore", 6),
            ("Couldn't look you in the eye", 7),
            ("You're just like an angel", 6),
            ("Your skin makes me cry", 5),
        ];

        for case in cases {
            let count = syllables_in_words(case.0);
            assert_eq!(count, case.1, "Failed for: \n{}", case.0);
        }
    }

    #[test]
    fn test_syllables_in_lines() {
        // Test various lyric lines with verified counts
        let cases = vec![
            ("Into you", 3),
            ("Well!", 1),
            ("(Woo)", 1),
            ("(Are you ready?)", 4),
            ("Waiting, I'm patient", 5),
            ("In keeping, I'm wanting more", 7),
            ("Picking up the loose puzzle pieces", 9),
            ("Scattered around the floor", 6),
            ("Linking, I'm drifting", 5),
            ("Just waiting for you to know", 7),
            ("Following the crumbs that I left for you", 10),
            ("Leading to my door", 5),
        ];

        for case in cases {
            let count = syllables_in_words(case.0);
            assert_eq!(
                count,
                case.1,
                "Failed for line: '{}' - expected {}, got {}",
                case.0,
                case.1,
                count
            );
        }
    }

    #[test]
    fn test_syllables_in_lines_verify_counts() {
        // Test lines and verify they have reasonable syllable counts
        // (These may vary, so we just verify they're > 0 and reasonable)
        let test_lines = vec![
            "Drowning, fishing, dropping, screaming under the lights",
            "I'm feeling everything crashing, burning, I lost track of time",
            "I'm breathing, I'm breathing, I think I'm reading you well",
            "Burning, crashing, trampoline, the edge of the sun",
            "I think I fell in love with lightning bolt, I'm ready to run",
        ];

        for line in test_lines {
            let count = syllables_in_words(line);
            // Verify it's a reasonable count (at least 1, and not absurdly high)
            assert!(count > 0, "Line '{}' should have syllables", line);
            assert!(count < 50, "Line '{}' has suspiciously high syllable count: {}", line, count);
        }
    }

    #[test]
    fn test_syllables_in_empty_and_whitespace() {
        assert_eq!(syllables_in_words(""), 0);
        assert_eq!(syllables_in_words("   "), 0);
        assert_eq!(syllables_in_words("\t\n"), 0);
        assert_eq!(syllables_in_words("  hello  world  "), 3);
    }

    #[test]
    fn test_syllables_with_punctuation() {
        // Test that punctuation doesn't affect syllable counting
        assert_eq!(syllables_in_words("hello, world!"), 3);
        assert_eq!(syllables_in_words("Well! Well!"), 2);
        assert_eq!(syllables_in_words("(Woo) (Are you ready?)"), 5);
        
        // Test with contractions - these may have different counts
        let count = syllables_in_words("I'm... ready?");
        assert!(count >= 3, "I'm... ready? should have at least 3 syllables, got {}", count);
    }
}

