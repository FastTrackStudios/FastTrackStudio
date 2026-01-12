//! Tempo extraction utilities
//!
//! Extracts tempo/BPM values from track names like "126bpm", "83.5BPM", etc.

/// Extract tempo in BPM from a string
///
/// Looks for patterns like:
/// - "126bpm"
/// - "83.5BPM"
/// - "120_bpm"
/// - "140 BPM"
///
/// Returns the tempo as f32 if found, None otherwise.
///
/// # Examples
///
/// ```
/// use dynamic_template::extract_tempo;
///
/// assert_eq!(extract_tempo("Track.126bpm.wav"), Some(126.0));
/// assert_eq!(extract_tempo("83.5BPM_song"), Some(83.5));
/// assert_eq!(extract_tempo("No tempo here"), None);
/// ```
pub fn extract_tempo(input: &str) -> Option<f32> {
    let lower = input.to_lowercase();

    // Find "bpm" in the string
    let bpm_idx = lower.find("bpm")?;

    // Look backwards from bpm to find the number
    let prefix = &input[..bpm_idx];

    // Find where the number ends (right before bpm or any separator before bpm)
    let prefix_trimmed = prefix.trim_end();

    // Find where the number starts by scanning backwards
    let mut num_start = prefix_trimmed.len();
    let mut num_end = prefix_trimmed.len();
    let mut found_digit = false;
    let mut has_decimal = false;

    for (i, c) in prefix_trimmed.char_indices().rev() {
        if c.is_ascii_digit() {
            num_start = i;
            if !found_digit {
                num_end = i + c.len_utf8();
            }
            found_digit = true;
        } else if c == '.' && found_digit && !has_decimal {
            // Allow one decimal point within the number
            num_start = i;
            has_decimal = true;
        } else if found_digit {
            // We've hit a non-digit after finding digits, stop
            break;
        }
        // Continue scanning backwards past separators when we haven't found digits yet
    }

    if !found_digit {
        return None;
    }

    // Extract the number substring
    let num_str = &prefix_trimmed[num_start..num_end];

    // Handle leading decimal point (e.g., ".5bpm" -> not valid)
    let num_str = num_str.trim_start_matches(|c: char| !c.is_ascii_digit());

    // Parse as f32
    num_str.parse::<f32>().ok()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extract_integer_tempo() {
        assert_eq!(extract_tempo("126bpm"), Some(126.0));
        assert_eq!(extract_tempo("120BPM"), Some(120.0));
        assert_eq!(extract_tempo("140 bpm"), Some(140.0));
        assert_eq!(extract_tempo("85_bpm"), Some(85.0));
    }

    #[test]
    fn extract_decimal_tempo() {
        assert_eq!(extract_tempo("83.5bpm"), Some(83.5));
        assert_eq!(extract_tempo("127.4BPM"), Some(127.4));
        assert_eq!(extract_tempo("99.99bpm"), Some(99.99));
    }

    #[test]
    fn extract_tempo_in_filename() {
        assert_eq!(extract_tempo("01.LV BECCA.TimeAfterTime.126bpm"), Some(126.0));
        assert_eq!(extract_tempo("Track_83.5BPM_v2.wav"), Some(83.5));
        assert_eq!(extract_tempo("Song.120bpm.stem.wav"), Some(120.0));
    }

    #[test]
    fn no_tempo_found() {
        assert_eq!(extract_tempo("Kick In.wav"), None);
        assert_eq!(extract_tempo("bpm"), None); // Just "bpm" without number
        assert_eq!(extract_tempo("Vocal Track"), None);
    }

    #[test]
    fn edge_cases() {
        // Number right before bpm
        assert_eq!(extract_tempo("abc123bpm"), Some(123.0));
        // Decimal at end
        assert_eq!(extract_tempo("120.bpm"), Some(120.0));
    }
}
