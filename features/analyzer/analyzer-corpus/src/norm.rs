//! Collapsing chart rows down to song identities.
//!
//! A single song shows up as hundreds of chart rows — once per week per
//! chart it appears on — and the corpus needs one row per *song*, since
//! that is the unit that gets downloaded, separated and measured. So
//! every row is reduced to a `(title_norm, artist_norm)` key and rows
//! sharing a key are one song.
//!
//! The normalisation is deliberately conservative. Billboard is
//! internally consistent about how it writes a given release, so the
//! job is not fuzzy matching across sources — it is absorbing the few
//! ways Billboard itself varies:
//!
//! - featuring credits move between the title and the artist field, and
//!   change spelling (`Featuring` / `Feat.` / `ft.`),
//! - typographic punctuation drifts (curly vs straight apostrophes),
//! - accented characters appear both composed and stripped, which
//!   matters a great deal on the Latin chart.
//!
//! What it must NOT do is merge distinct songs. Parenthetical text is
//! only dropped when it is a credit — a title that is *mostly*
//! parenthetical, as plenty of hits are, has to survive intact, so a
//! parenthetical is never dropped if doing so would leave nothing.

/// Normalise a song title into a dedupe key.
pub fn title_key(title: &str) -> String {
    let t = strip_credit_parentheticals(&fold(title));
    squash(&t)
}

/// Normalise an artist credit into a dedupe key.
///
/// The whole credit is kept, featured artists included, because two
/// different featuring line-ups over the same backing track are two
/// different records with two different vocals — which is exactly what
/// this corpus measures.
pub fn artist_key(artist: &str) -> String {
    // The parts are already squashed, so they are joined directly —
    // squashing again would eat the separator and turn
    // `a & b` into `a b`, which then collides with an act genuinely
    // named "a b".
    split_credit(&fold(artist)).join(" & ")
}

/// The lead artist, with any featuring credit removed.
///
/// Kept alongside the full credit so the corpus can group by act
/// without collapsing distinct records together.
pub fn primary_artist(artist: &str) -> String {
    let folded = fold(artist);
    let parts = split_credit(&folded);
    let lead = parts.first().map(String::as_str).unwrap_or(&folded);
    squash(lead)
}

/// Lowercase, replace typographic punctuation with ASCII, and strip
/// diacritics so `Despacito` and `Despácito` land in the same bucket.
fn fold(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for ch in s.chars() {
        match ch {
            '\u{2018}' | '\u{2019}' | '\u{02BC}' | '`' => out.push('\''),
            '\u{201C}' | '\u{201D}' => out.push('"'),
            '\u{2010}'..='\u{2015}' | '\u{2212}' => out.push('-'),
            '\u{00A0}' | '\u{2007}' | '\u{202F}' => out.push(' '),
            '\u{2026}' => out.push_str("..."),
            _ => match deaccent(ch) {
                Some(rep) => out.push_str(rep),
                None => out.extend(ch.to_lowercase()),
            },
        }
    }
    out
}

/// Map the accented Latin-1/Latin-A characters Billboard actually uses
/// down to their ASCII base letter.
fn deaccent(ch: char) -> Option<&'static str> {
    let lower = ch.to_lowercase().next().unwrap_or(ch);
    Some(match lower {
        'á' | 'à' | 'â' | 'ä' | 'ã' | 'å' | 'ā' | 'ă' | 'ą' => "a",
        'é' | 'è' | 'ê' | 'ë' | 'ē' | 'ĕ' | 'ė' | 'ę' | 'ě' => "e",
        'í' | 'ì' | 'î' | 'ï' | 'ī' | 'į' => "i",
        'ó' | 'ò' | 'ô' | 'ö' | 'õ' | 'ø' | 'ō' | 'ő' => "o",
        'ú' | 'ù' | 'û' | 'ü' | 'ū' | 'ů' | 'ų' => "u",
        'ñ' | 'ń' | 'ņ' | 'ň' => "n",
        'ç' | 'ć' | 'č' => "c",
        'ý' | 'ÿ' => "y",
        'š' | 'ś' => "s",
        'ž' | 'ź' | 'ż' => "z",
        'ł' => "l",
        'đ' => "d",
        'ř' => "r",
        'ť' => "t",
        'ß' => "ss",
        'æ' => "ae",
        'œ' => "oe",
        _ => return None,
    })
}

/// The tokens Billboard uses to introduce a featured credit.
const CREDIT_MARKERS: &[&str] = &[
    " featuring ",
    " feat. ",
    " feat ",
    " ft. ",
    " ft ",
    " with ",
    " duet with ",
    " x ",
    " & ",
    " and ",
    ", ",
    " + ",
];

/// Split an already-folded artist credit into its constituent acts,
/// lead first.
fn split_credit(folded: &str) -> Vec<String> {
    let mut parts = vec![folded.to_string()];
    for marker in CREDIT_MARKERS {
        parts = parts
            .iter()
            .flat_map(|p| p.split(marker).map(str::to_string).collect::<Vec<_>>())
            .collect();
    }
    parts
        .into_iter()
        .map(|p| squash(&p))
        .filter(|p| !p.is_empty())
        .collect()
}

/// Drop parenthetical/bracketed groups that are only a credit, e.g.
/// `song (feat. someone)`.
///
/// A group is only removed when it *starts* with a credit marker, so a
/// title whose meaning lives inside its parentheses is untouched. If
/// removing everything would empty the title, the original is kept.
fn strip_credit_parentheticals(folded: &str) -> String {
    let mut out = String::with_capacity(folded.len());
    let mut depth = 0usize;
    let mut group = String::new();

    for ch in folded.chars() {
        match ch {
            '(' | '[' => {
                depth += 1;
                if depth == 1 {
                    group.clear();
                } else {
                    group.push(ch);
                }
            }
            ')' | ']' if depth > 0 => {
                depth -= 1;
                if depth == 0 {
                    if !is_credit_group(&group) {
                        out.push('(');
                        out.push_str(&group);
                        out.push(')');
                    }
                } else {
                    group.push(ch);
                }
            }
            _ if depth > 0 => group.push(ch),
            _ => out.push(ch),
        }
    }

    if squash(&out).is_empty() {
        folded.to_string()
    } else {
        out
    }
}

/// Does this parenthetical group read as a featuring/production credit
/// rather than part of the title?
fn is_credit_group(group: &str) -> bool {
    let g = group.trim();
    const PREFIXES: &[&str] = &[
        "feat.", "feat ", "featuring", "ft.", "ft ", "with ", "prod.", "prod ", "produced by",
    ];
    PREFIXES.iter().any(|p| g.starts_with(p))
}

/// Reduce to `[a-z0-9]` words joined by single spaces.
///
/// Apostrophes are *deleted* rather than treated as separators, so
/// `don't` becomes `dont` and not `don t` — otherwise every contraction
/// would key differently depending on whether the source wrote a curly
/// apostrophe, a straight one, or none at all.
fn squash(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut pending_space = false;
    for ch in s.chars() {
        if ch.is_ascii_alphanumeric() {
            if pending_space && !out.is_empty() {
                out.push(' ');
            }
            pending_space = false;
            out.push(ch);
        } else if ch != '\'' {
            pending_space = true;
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn folds_typography_and_accents() {
        assert_eq!(title_key("Don\u{2019}t Stop"), "dont stop");
        assert_eq!(title_key("Despácito"), "despacito");
        assert_eq!(artist_key("Beyoncé"), "beyonce");
    }

    #[test]
    fn credit_parentheticals_are_dropped_from_titles() {
        assert_eq!(title_key("Slow Down (feat. Someone)"), "slow down");
        assert_eq!(title_key("Slow Down [Featuring Someone]"), "slow down");
        assert_eq!(title_key("Slow Down (Prod. By Someone)"), "slow down");
    }

    #[test]
    fn meaningful_parentheticals_survive() {
        // A great many hits are titled this way; dropping the
        // parenthetical would merge genuinely different songs.
        assert_eq!(
            title_key("(Everything I Do) I Do It For You"),
            "everything i do i do it for you"
        );
        // Removing this one would leave nothing at all, so it stays.
        assert_eq!(title_key("(Reprise)"), "reprise");
    }

    #[test]
    fn featuring_credits_normalise_across_spellings() {
        let a = artist_key("Artist One Featuring Artist Two");
        assert_eq!(a, artist_key("Artist One Feat. Artist Two"));
        assert_eq!(a, artist_key("Artist One ft Artist Two"));
        assert_eq!(a, artist_key("Artist One & Artist Two"));
        assert_eq!(a, "artist one & artist two");
    }

    #[test]
    fn primary_artist_drops_the_feature() {
        assert_eq!(primary_artist("Artist One Featuring Artist Two"), "artist one");
        assert_eq!(primary_artist("Artist One"), "artist one");
    }

    #[test]
    fn distinct_songs_stay_distinct() {
        assert_ne!(title_key("Hello"), title_key("Hello Goodbye"));
        assert_ne!(
            artist_key("Artist One Featuring Artist Two"),
            artist_key("Artist One Featuring Artist Three")
        );
    }
}
