//! Default performer names for track name parsing
//! 
//! This module contains the list of known performer names that can be
//! extracted from track names. Based on the fts-naming-parser config.json.

/// Default list of performer names
/// 
/// These names are used to identify performers in track names.
/// The list includes common first names and initials.
pub const DEFAULT_PERFORMERS: &[&str] = &[
    "Cody",
    "John",
    "Sarah",
    "Mike",
    "Emily",
    "David",
    "Lisa",
    "Chris",
    "Anna",
    "Mark",
    "Bri",
    "Joshua",
    "Alex",
    "Sam",
    "Jordan",
    "Taylor",
    "Casey",
    "Morgan",
    "Jamie",
    "Riley",
    "Avery",
    "Quinn",
    "Matt",
    "Dan",
    "Tom",
    "Ben",
    "Jake",
    "Luke",
    "Nick",
    "Ryan",
    "Kyle",
    "Sean",
    "Kate",
    "Emma",
    "Mia",
    "Zoe",
    "Amy",
    "Jess",
    "Leah",
    "Maya",
    "Ella",
    "Grace",
    "James",
    "Will",
    "Jack",
    "Noah",
    "Liam",
    "Owen",
    "Ethan",
    "Mason",
    "Logan",
    "Lucas",
    "Sophia",
    "Olivia",
    "Ava",
    "Isabella",
    "Charlotte",
    "Amelia",
    "Harper",
    "Evelyn",
    "JD",
    "MJ",
    "AJ",
    "TJ",
    "CJ",
    "DJ",
    "RJ",
    "BJ",
    "PJ",
    "KJ",
    "Mac",
    "Max",
    "Ace",
    "Rex",
    "Zach",
    "Josh",
    "Rob",
    "Bob",
    "Joe",
    "Jim",
];

/// Check if a word matches any known performer name (case-insensitive)
pub fn is_performer_name(word: &str) -> bool {
    let word_lower = word.to_lowercase();
    DEFAULT_PERFORMERS.iter()
        .any(|performer| performer.to_lowercase() == word_lower)
}

/// Get the original case performer name if it matches (case-insensitive)
pub fn get_performer_name(word: &str) -> Option<&'static str> {
    let word_lower = word.to_lowercase();
    DEFAULT_PERFORMERS.iter()
        .find(|performer| performer.to_lowercase() == word_lower)
        .copied()
}

