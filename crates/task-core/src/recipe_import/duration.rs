//! ISO 8601 duration parser narrowed to the shapes schema.org recipes
//! actually emit (`PT15M`, `PT1H`, `PT1H30M`, `P0DT15M`, …).
//!
//! Only the day, hour, minute and second fields are honored. Years/
//! months/weeks are rejected — they're effectively never present in
//! `prepTime` / `cookTime` and the unit conversion would be lossy. Any
//! input we don't recognize returns `None` so the caller can log a
//! warning and fall back to the existing recipe defaults.

/// Parse an ISO 8601 duration string and return the total length in
/// minutes. Returns `None` for malformed input or zero-length output.
///
/// Supported shape: `P[nD]T[nH][nM][nS]`. Seconds are floored away
/// (rounded down to the nearest minute). A bare `PT0M` or all-zero
/// payload returns `Some(0)`.
#[must_use]
pub fn parse_iso_duration(input: &str) -> Option<u32> {
    let trimmed = input.trim();
    if trimmed.is_empty() {
        return None;
    }
    let mut bytes = trimmed.as_bytes();
    if bytes.first() != Some(&b'P') && bytes.first() != Some(&b'p') {
        return None;
    }
    bytes = &bytes[1..];

    let mut days: u64 = 0;
    let mut hours: u64 = 0;
    let mut minutes: u64 = 0;
    let mut seconds: u64 = 0;
    let mut in_time = false;
    let mut current = String::new();

    for &b in bytes {
        let c = b as char;
        if c.is_ascii_digit() {
            current.push(c);
            continue;
        }
        match c.to_ascii_uppercase() {
            'T' => {
                if in_time || !current.is_empty() {
                    return None;
                }
                in_time = true;
            }
            'D' if !in_time => {
                days = current.parse().ok()?;
                current.clear();
            }
            'H' if in_time => {
                hours = current.parse().ok()?;
                current.clear();
            }
            'M' if in_time => {
                minutes = current.parse().ok()?;
                current.clear();
            }
            'S' if in_time => {
                seconds = current.parse().ok()?;
                current.clear();
            }
            // Years/months/weeks/anything else → reject.
            _ => return None,
        }
    }
    if !current.is_empty() {
        // Trailing digits without a unit suffix.
        return None;
    }
    let total_minutes = days
        .checked_mul(24 * 60)?
        .checked_add(hours.checked_mul(60)?)?
        .checked_add(minutes)?
        .checked_add(seconds / 60)?;
    u32::try_from(total_minutes).ok()
}

#[cfg(test)]
mod tests {
    use super::parse_iso_duration;

    #[test]
    fn pt15m() {
        assert_eq!(parse_iso_duration("PT15M"), Some(15));
    }

    #[test]
    fn pt1h() {
        assert_eq!(parse_iso_duration("PT1H"), Some(60));
    }

    #[test]
    fn pt1h30m() {
        assert_eq!(parse_iso_duration("PT1H30M"), Some(90));
    }

    #[test]
    fn pt2h45m30s_drops_seconds() {
        // 2*60 + 45 + 30/60 -> floor to 165
        assert_eq!(parse_iso_duration("PT2H45M30S"), Some(165));
    }

    #[test]
    fn pt2h45m59s_still_floors_to_165() {
        assert_eq!(parse_iso_duration("PT2H45M59S"), Some(165));
    }

    #[test]
    fn pt0m_zero() {
        assert_eq!(parse_iso_duration("PT0M"), Some(0));
    }

    #[test]
    fn day_prefix() {
        assert_eq!(parse_iso_duration("P1DT0M"), Some(24 * 60));
    }

    #[test]
    fn day_plus_minutes() {
        assert_eq!(parse_iso_duration("P0DT15M"), Some(15));
    }

    #[test]
    fn empty() {
        assert_eq!(parse_iso_duration(""), None);
    }

    #[test]
    fn missing_prefix() {
        assert_eq!(parse_iso_duration("15M"), None);
    }

    #[test]
    fn malformed_letters() {
        assert_eq!(parse_iso_duration("PTfooM"), None);
    }

    #[test]
    fn rejects_years() {
        assert_eq!(parse_iso_duration("P1Y"), None);
    }

    #[test]
    fn rejects_weeks() {
        assert_eq!(parse_iso_duration("P1W"), None);
    }

    #[test]
    fn case_insensitive_prefix() {
        assert_eq!(parse_iso_duration("pt15m"), Some(15));
    }

    #[test]
    fn trailing_digits_without_unit() {
        assert_eq!(parse_iso_duration("PT15"), None);
    }

    #[test]
    fn whitespace_trimmed() {
        assert_eq!(parse_iso_duration("  PT15M  "), Some(15));
    }
}
