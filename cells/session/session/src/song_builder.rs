//! SongBuilder - Extract song structure from DAW projects
//!
//! Analyzes markers, regions, and tempo maps to build Song domain objects.
//!
//! ## Song Name Convention
//! Project names follow the format: "Title - Artist.rpp"
//! - The ".rpp" extension is stripped
//! - "Title - Artist" is parsed to extract song name and artist
//!
//! ## Section Detection
//! Sections can be built from either:
//! 1. **Regions**: Regions contained within a song region (preferred)
//! 2. **Markers**: When no regions exist, consecutive markers are used to define sections
//!    Each marker defines the start of a section, ending at the next marker.

use daw_control::Project;
use daw_proto::{Marker, Region};
use session_proto::{Section, SectionType, Song};
use tracing::debug;
use uuid::Uuid;

/// Builder for extracting Song structure from DAW projects
pub struct SongBuilder;

/// Helper to get seconds from Position
fn position_to_seconds(pos: &daw_proto::Position) -> f64 {
    pos.time.as_ref().map(|t| t.as_seconds()).unwrap_or(0.0)
}

impl SongBuilder {
    /// Build a Song from a DAW project using daw-control API
    ///
    /// Analyzes markers for SONGSTART/SONGEND, regions for sections,
    /// and tempo map for tempo/time signature information.
    pub async fn build(project: &Project) -> eyre::Result<Song> {
        debug!("SongBuilder::build for project {}", project.guid());

        // Get project info for the name
        let project_info = project.info().await?;
        let project_name = &project_info.name;

        // Parse song name and artist from project name
        let (song_name, _artist) = Self::parse_project_name(project_name);

        let markers = project.markers().all().await?;
        let regions = project.regions().all().await?;
        let tempo_map = project.tempo_map();

        // Find special markers
        let count_in_marker = markers.iter().find(|m| Self::is_count_in_marker(&m.name));
        let absolute_start_marker = markers.iter().find(|m| m.name == "=START");
        let songstart_marker = markers.iter().find(|m| Self::is_songstart_marker(&m.name));
        let songend_marker = markers.iter().find(|m| Self::is_songend_marker(&m.name));
        let absolute_end_marker = markers.iter().find(|m| m.name == "=END");

        // Debug: log found markers with positions
        debug!(
            "All markers: {:?}",
            markers
                .iter()
                .map(|m| (&m.name, position_to_seconds(&m.position)))
                .collect::<Vec<_>>()
        );
        debug!(
            "count_in_marker: {:?}",
            count_in_marker.map(|m| (&m.name, position_to_seconds(&m.position)))
        );
        debug!(
            "absolute_start_marker (=START): {:?}",
            absolute_start_marker.map(|m| (&m.name, position_to_seconds(&m.position)))
        );
        debug!(
            "songstart_marker: {:?}",
            songstart_marker.map(|m| (&m.name, position_to_seconds(&m.position)))
        );
        debug!(
            "songend_marker: {:?}",
            songend_marker.map(|m| (&m.name, position_to_seconds(&m.position)))
        );
        debug!(
            "absolute_end_marker (=END): {:?}",
            absolute_end_marker.map(|m| (&m.name, position_to_seconds(&m.position)))
        );

        // Legacy marker support
        // Note: =START is the absolute start (including count-in), not SONGSTART
        // Only use =SONGSTART as a fallback, not =START
        let start_marker =
            songstart_marker.or_else(|| markers.iter().find(|m| m.name.starts_with("=SONGSTART")));

        // Note: =END is the absolute end (including outro), not SONGEND
        // Only use =SONGEND as a fallback, not =END
        let end_marker =
            songend_marker.or_else(|| markers.iter().find(|m| m.name.starts_with("=SONGEND")));

        // Find the song region (if regions exist)
        let song_region = Self::find_song_region(&regions);

        // Determine song bounds
        let (start_seconds, songend_seconds, end_seconds, count_in_seconds) =
            if let (Some(start), Some(end)) = (start_marker, end_marker) {
                let song_start = position_to_seconds(&start.position);
                let song_end = position_to_seconds(&end.position);
                let absolute_end = absolute_end_marker
                    .map(|m| position_to_seconds(&m.position))
                    .unwrap_or(song_end);

                // Use COUNT-IN marker first, fall back to =START
                // COUNT-IN is more explicit about the count-in position
                let count_in = if let Some(ci_marker) = count_in_marker.or(absolute_start_marker) {
                    let ci_time = position_to_seconds(&ci_marker.position);
                    debug!(
                        "Count-in calculation: ci_marker={}, ci_time={}, song_start={}",
                        ci_marker.name, ci_time, song_start
                    );
                    if ci_time < song_start {
                        let duration = song_start - ci_time;
                        debug!("Count-in duration: {}", duration);
                        Some(duration)
                    } else {
                        debug!("Count-in marker is NOT before song_start, no count-in");
                        None
                    }
                } else {
                    debug!("No count-in or =START marker found");
                    None
                };

                debug!(
                    "Song bounds: start={}, songend={}, end={}, count_in={:?}",
                    song_start, song_end, absolute_end, count_in
                );
                (song_start, song_end, absolute_end, count_in)
            } else if let Some(ref song_region) = song_region {
                let end = song_region.time_range.end_seconds();
                let start = song_region.time_range.start_seconds();
                let count_in = count_in_marker.and_then(|m| {
                    let marker_time = position_to_seconds(&m.position);
                    if marker_time < start {
                        Some(start - marker_time)
                    } else {
                        None
                    }
                });
                (start, end, end, count_in)
            } else {
                // Fallback to entire project
                let start = markers
                    .iter()
                    .map(|m| position_to_seconds(&m.position))
                    .chain(regions.iter().map(|r| r.time_range.start_seconds()))
                    .min_by(|a: &f64, b: &f64| a.partial_cmp(b).unwrap())
                    .unwrap_or(0.0);

                let end = markers
                    .iter()
                    .map(|m| position_to_seconds(&m.position))
                    .chain(regions.iter().map(|r| r.time_range.end_seconds()))
                    .max_by(|a: &f64, b: &f64| a.partial_cmp(b).unwrap())
                    .unwrap_or(60.0);

                (start, end, end, None)
            };

        // Extract sections - prefer regions, fall back to markers
        let mut sections = if let Some(ref song_region) = song_region {
            Self::extract_sections_from_song_region(&regions, song_region)?
        } else if !regions.is_empty() {
            Self::extract_sections_from_regions(&regions, start_seconds, songend_seconds)?
        } else {
            // No regions - build sections from markers
            debug!("No regions found, building sections from markers");
            Self::build_sections_from_markers(&markers, start_seconds, songend_seconds)?
        };

        debug!("Extracted {} sections", sections.len());
        for (i, section) in sections.iter().enumerate() {
            debug!(
                "  Section[{}]: '{}' type={:?} start={:.3} end={:.3} duration={:.3}",
                i,
                section.name,
                section.section_type,
                section.start_seconds,
                section.end_seconds,
                section.end_seconds - section.start_seconds
            );
        }

        // Add Count-In section at the beginning if there's a count-in
        // IMPORTANT: Use the first section's start time as Count-In's end time to ensure
        // continuity (no gaps between Count-In and the first section). This handles cases
        // where markers and regions don't perfectly align due to tempo quantization.
        let song_start_seconds = if let Some(count_in_duration) = count_in_seconds {
            if count_in_duration > 0.0 {
                let count_in_start = start_seconds - count_in_duration;
                // Use first section's start time as count-in end (ensures no gap)
                let count_in_end = sections
                    .first()
                    .map(|s| s.start_seconds)
                    .unwrap_or(start_seconds);
                debug!(
                    "Adding Count-In section: start={:.3} end={:.3} (first_section_start={:.3}, marker_start={:.3})",
                    count_in_start, count_in_end, count_in_end, start_seconds
                );
                sections.insert(
                    0,
                    Section {
                        id: None,
                        name: "Count-In".to_string(),
                        comment: None,
                        section_type: SectionType::CountIn,
                        start_seconds: count_in_start,
                        end_seconds: count_in_end,
                        number: None,
                        color: None,
                    },
                );
                count_in_start
            } else {
                start_seconds
            }
        } else {
            start_seconds
        };

        // Add END section if there's a gap between SONGEND and =END
        // IMPORTANT: Use the last section's end time as END's start time to ensure
        // continuity (no gaps between the last content section and END). This handles
        // cases where markers and regions don't perfectly align due to tempo quantization.
        if end_seconds > songend_seconds + 0.01 {
            // Use last section's end time as END start (ensures no gap)
            let end_section_start = sections
                .last()
                .map(|s| s.end_seconds)
                .unwrap_or(songend_seconds);
            debug!(
                "Adding END section: start={:.3} end={:.3} (last_section_end={:.3}, marker_songend={:.3})",
                end_section_start, end_seconds, end_section_start, songend_seconds
            );
            sections.push(Section {
                id: None,
                name: "End".to_string(),
                comment: None,
                section_type: SectionType::End,
                start_seconds: end_section_start,
                end_seconds,
                number: None,
                color: None,
            });
        }

        // Log final sections list
        debug!("Final sections after adding Count-In/End:");
        for (i, section) in sections.iter().enumerate() {
            debug!(
                "  Final[{}]: '{}' start={:.3} end={:.3}",
                i, section.name, section.start_seconds, section.end_seconds
            );
        }

        // Get tempo and time signature at song start
        let tempo = tempo_map.tempo_at(start_seconds).await.ok();
        let time_sig = tempo_map
            .time_signature_at(start_seconds)
            .await
            .ok()
            .map(|(num, denom)| daw_proto::TimeSignature::new(num as u32, denom as u32));

        // Build measure positions if we have tempo and time signature
        let measure_positions = if let (Some(bpm), Some(ts)) = (tempo, time_sig) {
            Self::calculate_measure_positions(song_start_seconds, end_seconds, bpm, ts)
        } else {
            Vec::new()
        };

        Ok(Song {
            id: Uuid::new_v4().to_string(),
            name: song_name,
            project_guid: project.guid().to_string(),
            start_seconds: song_start_seconds,
            end_seconds,
            count_in_seconds,
            sections,
            tempo,
            time_signature: time_sig,
            measure_positions,
        })
    }

    /// Parse project name to extract song title and artist
    ///
    /// Format: "Title - Artist.rpp" or "Title - Artist"
    /// Returns: (song_name, Option<artist>)
    fn parse_project_name(name: &str) -> (String, Option<String>) {
        // Remove .rpp extension (case insensitive)
        let name = name
            .trim()
            .trim_end_matches(".rpp")
            .trim_end_matches(".RPP")
            .trim_end_matches(".Rpp");

        // Look for " - " separator (with spaces around dash)
        if let Some(sep_pos) = name.find(" - ") {
            let title = name[..sep_pos].trim();
            let artist = name[sep_pos + 3..].trim();

            if artist.is_empty() {
                (title.to_string(), None)
            } else {
                (title.to_string(), Some(artist.to_string()))
            }
        } else {
            (name.to_string(), None)
        }
    }

    /// Check if a marker name indicates a count-in marker
    /// Supports: COUNTIN, COUNT-IN, COUNT IN, count in, count-in, COUNT_IN, etc.
    fn is_count_in_marker(name: &str) -> bool {
        let normalized = name.to_uppercase().replace(['-', ' ', '_'], "");
        normalized == "COUNTIN"
    }

    /// Check if a marker name indicates a SONGSTART marker
    fn is_songstart_marker(name: &str) -> bool {
        let upper = name.to_uppercase();
        upper == "SONGSTART"
            || upper.starts_with("SONGSTART ")
            || upper == "SONG START"
            || upper.starts_with("SONG START ")
    }

    /// Check if a marker name indicates a SONGEND marker
    fn is_songend_marker(name: &str) -> bool {
        let upper = name.to_uppercase();
        upper == "SONGEND"
            || upper.starts_with("SONGEND ")
            || upper == "SONG END"
            || upper.starts_with("SONG END ")
    }

    /// Check if a marker is a special marker (not a section marker)
    fn is_special_marker(name: &str) -> bool {
        Self::is_count_in_marker(name)
            || Self::is_songstart_marker(name)
            || Self::is_songend_marker(name)
            || name == "=START"
            || name == "=END"
            || name.starts_with("=SONGSTART")
            || name.starts_with("=SONGEND")
    }

    /// Find the song region - the region that contains other regions (sections)
    fn find_song_region(regions: &[Region]) -> Option<&Region> {
        let mut best_region: Option<&Region> = None;
        let mut best_count = 0;

        for region in regions {
            let contained_count = regions
                .iter()
                .filter(|r| {
                    r.id != region.id
                        && r.time_range.start_seconds() >= region.time_range.start_seconds()
                        && r.time_range.end_seconds() <= region.time_range.end_seconds()
                })
                .count();

            if contained_count > best_count {
                best_count = contained_count;
                best_region = Some(region);
            }
        }

        if best_count > 0 { best_region } else { None }
    }

    /// Build sections from markers (when no regions exist)
    ///
    /// Each marker defines the start of a section, ending at the next marker.
    fn build_sections_from_markers(
        markers: &[Marker],
        song_start: f64,
        song_end: f64,
    ) -> eyre::Result<Vec<Section>> {
        // Filter to section markers within song bounds (excluding special markers)
        let mut section_markers: Vec<&Marker> = markers
            .iter()
            .filter(|m| {
                let pos = position_to_seconds(&m.position);
                pos >= song_start && pos < song_end && !Self::is_special_marker(&m.name)
            })
            .collect();

        // Sort by position
        section_markers.sort_by(|a, b| {
            position_to_seconds(&a.position)
                .partial_cmp(&position_to_seconds(&b.position))
                .unwrap_or(std::cmp::Ordering::Equal)
        });

        let mut sections = Vec::new();

        for (idx, marker) in section_markers.iter().enumerate() {
            let start = position_to_seconds(&marker.position);

            // End is at the next marker or song end
            let end = if idx + 1 < section_markers.len() {
                position_to_seconds(&section_markers[idx + 1].position)
            } else {
                song_end
            };

            let (section_type, number, clean_name, comment) =
                Self::parse_section_name(&marker.name);

            sections.push(Section {
                id: marker.id,
                name: clean_name,
                comment,
                section_type,
                start_seconds: start,
                end_seconds: end,
                number,
                color: marker.color,
            });
        }

        Ok(sections)
    }

    /// Extract sections from regions contained within the song region
    fn extract_sections_from_song_region(
        regions: &[Region],
        song_region: &Region,
    ) -> eyre::Result<Vec<Section>> {
        let song_start = song_region.time_range.start_seconds();
        let song_end = song_region.time_range.end_seconds();

        let mut sections: Vec<Section> = regions
            .iter()
            .filter(|r| {
                r.id != song_region.id
                    && r.time_range.start_seconds() >= song_start
                    && r.time_range.end_seconds() <= song_end
            })
            .map(|r| {
                let (section_type, number, clean_name, comment) = Self::parse_section_name(&r.name);
                Section {
                    id: r.id,
                    name: clean_name,
                    comment,
                    section_type,
                    start_seconds: r.time_range.start_seconds(),
                    end_seconds: r.time_range.end_seconds(),
                    number,
                    color: r.color,
                }
            })
            .collect();

        sections.sort_by(|a, b| {
            a.start_seconds
                .partial_cmp(&b.start_seconds)
                .unwrap_or(std::cmp::Ordering::Equal)
        });

        Ok(sections)
    }

    /// Extract sections from regions within song bounds (fallback when no song region)
    fn extract_sections_from_regions(
        regions: &[Region],
        start: f64,
        end: f64,
    ) -> eyre::Result<Vec<Section>> {
        let mut sections: Vec<Section> = regions
            .iter()
            .filter(|r| r.time_range.start_seconds() >= start && r.time_range.end_seconds() <= end)
            .filter(|r| !r.name.starts_with("SONG:") && r.name.to_uppercase() != "SONG")
            .map(|r| {
                let (section_type, number, clean_name, comment) = Self::parse_section_name(&r.name);
                Section {
                    id: r.id,
                    name: clean_name,
                    comment,
                    section_type,
                    start_seconds: r.time_range.start_seconds(),
                    end_seconds: r.time_range.end_seconds(),
                    number,
                    color: r.color,
                }
            })
            .collect();

        sections.sort_by(|a, b| {
            a.start_seconds
                .partial_cmp(&b.start_seconds)
                .unwrap_or(std::cmp::Ordering::Equal)
        });

        Ok(sections)
    }

    /// Calculate measure positions for the song
    fn calculate_measure_positions(
        start_seconds: f64,
        end_seconds: f64,
        bpm: f64,
        ts: daw_proto::TimeSignature,
    ) -> Vec<daw_proto::Position> {
        let beats_per_measure = ts.numerator() as f64;
        let seconds_per_beat = 60.0 / bpm;
        let measure_duration = beats_per_measure * seconds_per_beat;

        let song_duration = end_seconds - start_seconds;
        let measure_count = (song_duration / measure_duration).ceil() as i32;

        (0..measure_count)
            .map(|idx| {
                let time_seconds = start_seconds + (idx as f64 * measure_duration);
                daw_proto::Position::from_time(daw_proto::TimePosition::from_seconds(time_seconds))
            })
            .collect()
    }

    /// Parse section type, number, name (without comment), and optional comment from region/marker name
    ///
    /// Supports formats like:
    /// - "Verse 1" -> (Verse, Some(1), "Verse 1", None)
    /// - "Interlude C" -> (Instrumental, Some(3), "Interlude C", None) // C=3rd variant
    /// - `Interlude C "Woodwinds"` -> (Instrumental, Some(3), "Interlude C", Some("Woodwinds"))
    /// - `Chorus 2 "Big Build"` -> (Chorus, Some(2), "Chorus 2", Some("Big Build"))
    fn parse_section_name(name: &str) -> (SectionType, Option<u32>, String, Option<String>) {
        // First, extract any quoted comment
        let (name_without_comment, comment) = Self::extract_comment(name);

        let name_upper = name_without_comment.to_uppercase();
        let name_trimmed = name_upper.trim();

        let (type_part, number) = Self::extract_type_and_number(name_trimmed);
        let section_type = Self::parse_section_type(type_part);

        // Use the original name (without comment) preserving case
        let clean_name = name_without_comment.trim().to_string();

        (section_type, number, clean_name, comment)
    }

    /// Extract a quoted comment from a section name
    ///
    /// Looks for text in double quotes at the end of the name.
    /// Returns (name_without_comment, optional_comment)
    fn extract_comment(name: &str) -> (&str, Option<String>) {
        let name = name.trim();

        // Look for a quoted string at the end: `Something "Comment"`
        if let Some(last_quote) = name.rfind('"') {
            // Find the opening quote
            if let Some(open_quote) = name[..last_quote].rfind('"') {
                let comment = name[open_quote + 1..last_quote].trim();
                let name_part = name[..open_quote].trim();

                if !comment.is_empty() {
                    return (name_part, Some(comment.to_string()));
                }
            }
        }

        (name, None)
    }

    /// Extract the type part and optional number from a section name
    fn extract_type_and_number(name: &str) -> (&str, Option<u32>) {
        // Try "Type Number" format (e.g., "Verse 1", "CH 2")
        if let Some(last_space) = name.rfind(' ') {
            let potential_num = &name[last_space + 1..];
            if let Ok(num) = potential_num.parse::<u32>() {
                return (&name[..last_space], Some(num));
            }
            // Try single letter variant (A=1, B=2, C=3, etc.) for cases like "Interlude C"
            if potential_num.len() == 1 {
                let c = potential_num.chars().next().unwrap();
                if c.is_ascii_uppercase() {
                    // A=1, B=2, C=3, etc.
                    let num = (c as u32) - ('A' as u32) + 1;
                    return (&name[..last_space], Some(num));
                }
            }
        }

        // Try concatenated format (e.g., "V1", "CH2", "VS1A")
        // Handle letter suffix after number (e.g., "1A" -> 1)
        let mut num_start = name.len();
        let mut num_end = name.len();

        for (i, c) in name.chars().rev().enumerate() {
            let pos = name.len() - 1 - i;
            if c.is_ascii_digit() {
                num_start = pos;
                if num_end == name.len() {
                    num_end = pos + 1;
                }
            } else if num_end != name.len() {
                // Found non-digit after finding digits, stop
                break;
            }
        }

        if num_start < num_end {
            let num_str = &name[num_start..num_end];
            if let Ok(num) = num_str.parse::<u32>() {
                return (&name[..num_start], Some(num));
            }
        }

        (name, None)
    }

    /// Parse section type from the type part of the name
    fn parse_section_type(type_part: &str) -> SectionType {
        let s = type_part.trim().to_lowercase();

        // Handle pre/post modifiers
        if s.starts_with("pre-") || s.starts_with("pre ") {
            let rest = s.trim_start_matches("pre-").trim_start_matches("pre ");
            if rest == "chorus" || rest == "ch" || rest == "c" {
                return SectionType::PreChorus;
            }
        }

        match s.as_str() {
            // Count-in variations
            "count-in" | "countin" | "count in" | "count" => SectionType::CountIn,

            // Intro variations
            "intro" | "in" => SectionType::Intro,

            // Verse variations
            "verse" | "vs" | "v" => SectionType::Verse,

            // Pre-chorus variations
            "prechorus" | "pre-chorus" | "pre chorus" | "pc" => SectionType::PreChorus,

            // Chorus variations
            "chorus" | "ch" | "c" => SectionType::Chorus,

            // Bridge variations
            "bridge" | "br" | "b" => SectionType::Bridge,

            // Outro variations
            "outro" | "out" | "o" => SectionType::Outro,

            // Solo variations
            "solo" | "s" => SectionType::Solo,

            // Breakdown variations
            "breakdown" | "bd" | "build" => SectionType::Breakdown,

            // Instrumental variations
            "instrumental" | "inst" => SectionType::Instrumental,

            // Interlude variations
            "interlude" | "int" => SectionType::Interlude,

            // Vamp variations
            "vamp" | "vmp" => SectionType::Vamp,

            // End section
            "end" => SectionType::End,

            // Unknown - use Other with original name
            _ => SectionType::Other(type_part.to_string()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_project_name() {
        let (name, artist) = SongBuilder::parse_project_name("Cryin' - Mateus Asato.rpp");
        assert_eq!(name, "Cryin'");
        assert_eq!(artist, Some("Mateus Asato".to_string()));

        let (name, artist) = SongBuilder::parse_project_name("My Song.rpp");
        assert_eq!(name, "My Song");
        assert_eq!(artist, None);

        let (name, artist) = SongBuilder::parse_project_name("Another Song - The Artist");
        assert_eq!(name, "Another Song");
        assert_eq!(artist, Some("The Artist".to_string()));
    }

    #[test]
    fn test_parse_section_name() {
        // Basic section types with numbers
        let (section_type, number, name, comment) = SongBuilder::parse_section_name("Verse 1");
        assert_eq!(section_type, SectionType::Verse);
        assert_eq!(number, Some(1));
        assert_eq!(name, "Verse 1");
        assert_eq!(comment, None);

        let (section_type, number, name, comment) = SongBuilder::parse_section_name("V2");
        assert_eq!(section_type, SectionType::Verse);
        assert_eq!(number, Some(2));
        assert_eq!(name, "V2");
        assert_eq!(comment, None);

        let (section_type, number, name, comment) = SongBuilder::parse_section_name("VS 1A");
        assert_eq!(section_type, SectionType::Verse);
        assert_eq!(number, Some(1));
        assert_eq!(name, "VS 1A");
        assert_eq!(comment, None);

        let (section_type, number, name, comment) = SongBuilder::parse_section_name("VS 1B");
        assert_eq!(section_type, SectionType::Verse);
        assert_eq!(number, Some(1));
        assert_eq!(name, "VS 1B");
        assert_eq!(comment, None);

        let (section_type, number, name, comment) = SongBuilder::parse_section_name("CH 1");
        assert_eq!(section_type, SectionType::Chorus);
        assert_eq!(number, Some(1));
        assert_eq!(name, "CH 1");
        assert_eq!(comment, None);

        let (section_type, number, name, comment) = SongBuilder::parse_section_name("INST");
        assert_eq!(section_type, SectionType::Instrumental);
        assert_eq!(number, None);
        assert_eq!(name, "INST");
        assert_eq!(comment, None);

        let (section_type, number, name, comment) = SongBuilder::parse_section_name("GTR SOLO");
        assert_eq!(section_type, SectionType::Other("GTR SOLO".to_string()));
        assert_eq!(number, None);
        assert_eq!(name, "GTR SOLO");
        assert_eq!(comment, None);

        let (section_type, number, name, comment) = SongBuilder::parse_section_name("SYNTH SOLO");
        assert_eq!(section_type, SectionType::Other("SYNTH SOLO".to_string()));
        assert_eq!(number, None);
        assert_eq!(name, "SYNTH SOLO");
        assert_eq!(comment, None);
    }

    #[test]
    fn test_parse_section_name_with_comment() {
        // Section with quoted comment - "C" is interpreted as variant 3 (A=1, B=2, C=3)
        let (section_type, number, name, comment) =
            SongBuilder::parse_section_name(r#"Interlude C "Woodwinds""#);
        assert_eq!(section_type, SectionType::Interlude);
        assert_eq!(number, Some(3)); // C = 3rd variant
        assert_eq!(name, "Interlude C");
        assert_eq!(comment, Some("Woodwinds".to_string()));

        // Chorus with comment
        let (section_type, number, name, comment) =
            SongBuilder::parse_section_name(r#"Chorus 2 "Big Build""#);
        assert_eq!(section_type, SectionType::Chorus);
        assert_eq!(number, Some(2));
        assert_eq!(name, "Chorus 2");
        assert_eq!(comment, Some("Big Build".to_string()));

        // Bridge with descriptive comment
        let (section_type, number, name, comment) =
            SongBuilder::parse_section_name(r#"Bridge "Key Change to Eb""#);
        assert_eq!(section_type, SectionType::Bridge);
        assert_eq!(number, None);
        assert_eq!(name, "Bridge");
        assert_eq!(comment, Some("Key Change to Eb".to_string()));

        // Verse with instrument indication
        let (section_type, number, name, comment) =
            SongBuilder::parse_section_name(r#"Verse 1 "Guitar Solo""#);
        assert_eq!(section_type, SectionType::Verse);
        assert_eq!(number, Some(1));
        assert_eq!(name, "Verse 1");
        assert_eq!(comment, Some("Guitar Solo".to_string()));
    }

    #[test]
    fn test_is_special_marker() {
        assert!(SongBuilder::is_special_marker("COUNT-IN"));
        assert!(SongBuilder::is_special_marker("SONGSTART"));
        assert!(SongBuilder::is_special_marker("SONGEND"));
        assert!(SongBuilder::is_special_marker("=START"));
        assert!(SongBuilder::is_special_marker("=END"));
        assert!(!SongBuilder::is_special_marker("Intro"));
        assert!(!SongBuilder::is_special_marker("VS 1A"));
        assert!(!SongBuilder::is_special_marker("CH 1"));
    }
}
