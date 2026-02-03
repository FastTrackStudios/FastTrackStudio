//! SongBuilder - Extract song structure from DAW projects
//!
//! Analyzes markers, regions, and tempo maps to build Song domain objects.

use daw_control::Project;
use session_proto::{Section, SectionType, Song};
use tracing::info;
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
        info!("SongBuilder::build for project {}", project.guid());

        let markers = project.markers().all().await?;
        info!("  Found {} markers", markers.len());
        for marker in &markers {
            info!("    - {} @ {:.2}s", marker.name, marker.position_seconds());
        }

        let regions = project.regions().all().await?;
        info!("  Found {} regions", regions.len());
        for region in &regions {
            info!(
                "    - {} ({:.2}s - {:.2}s)",
                region.name,
                region.start_seconds(),
                region.end_seconds()
            );
        }

        let tempo_map = project.tempo_map();

        // Find markers according to the convention:
        // - COUNT-IN or =START: Absolute start of song timeline (including count-in)
        // - SONGSTART: Where the actual song content begins (after count-in)
        // - SONGEND: Where the song content ends
        // - =END: Absolute end (sections may continue past SONGEND until =END)

        // Find the absolute start (COUNT-IN or =START, whichever comes first)
        let count_in_marker = markers
            .iter()
            .find(|m| m.name.to_uppercase() == "COUNT-IN" || m.name.to_uppercase() == "COUNTIN");

        let absolute_start_marker = markers.iter().find(|m| m.name == "=START");

        // Find SONGSTART marker (where actual song content begins)
        let songstart_marker = markers.iter().find(|m| {
            m.name == "SONGSTART"
                || m.name.starts_with("SONGSTART ")
                || m.name.to_uppercase().starts_with("SONG START")
        });

        // Find SONGEND marker (where song content ends)
        let songend_marker = markers
            .iter()
            .find(|m| m.name == "SONGEND" || m.name.to_uppercase().starts_with("SONG END"));

        // Find =END marker (absolute end, sections may continue past SONGEND)
        let absolute_end_marker = markers.iter().find(|m| m.name == "=END");

        // For backwards compatibility, also check for legacy markers
        let start_marker = songstart_marker.or_else(|| {
            markers
                .iter()
                .find(|m| m.name.starts_with("=SONGSTART") || m.name.starts_with("=START"))
        });

        let end_marker = songend_marker.or_else(|| {
            markers
                .iter()
                .find(|m| m.name.starts_with("=SONGEND") || m.name.starts_with("=END"))
        });

        // Determine song bounds using the marker convention:
        // - start_seconds: SONGSTART position (where song content begins)
        // - songend_seconds: SONGEND position (where song content ends)
        // - end_seconds: =END position (absolute end, includes tail/reverb)
        // - count_in_seconds: Duration from =START/COUNT-IN to SONGSTART

        let (start_seconds, songend_seconds, end_seconds, song_name, count_in_seconds) =
            if let (Some(start), Some(end)) = (start_marker, end_marker) {
                let song_start = position_to_seconds(&start.position);
                let song_end = position_to_seconds(&end.position);

                // Absolute end is =END if available, otherwise same as SONGEND
                let absolute_end = absolute_end_marker
                    .map(|m| position_to_seconds(&m.position))
                    .unwrap_or(song_end);

                // Calculate count-in duration
                let count_in = if let Some(abs_start) = absolute_start_marker.or(count_in_marker) {
                    let abs_start_time = position_to_seconds(&abs_start.position);
                    if abs_start_time < song_start {
                        Some(song_start - abs_start_time)
                    } else {
                        None
                    }
                } else {
                    None
                };

                (
                    song_start,
                    song_end,
                    absolute_end,
                    Self::extract_song_name(&start.name),
                    count_in,
                )
            } else if let Some(song_region) = regions.iter().find(|r| {
                r.name.starts_with("SONG:")
                    || r.name.to_uppercase().starts_with("SONG ")
                    || r.name.to_uppercase() == "SONG"
            }) {
                // Fallback: use song region
                let end = song_region.time_range.end_seconds();
                (
                    song_region.time_range.start_seconds(),
                    end,
                    end,
                    Self::extract_song_name(&song_region.name),
                    None,
                )
            } else {
                // No song markers/regions found - use entire project
                // Find project bounds from all markers/regions
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
                    .unwrap_or(60.0); // Default 60 seconds if no content

                (start, end, end, "Untitled Song".to_string(), None)
            };

        // Extract sections from regions between SONGSTART and SONGEND
        let mut sections = Self::extract_sections(&regions, start_seconds, songend_seconds)?;

        // Add Count-In section at the beginning if there's a count-in
        // Also adjust the song's start_seconds to include the count-in
        let song_start_seconds = if let Some(count_in_duration) = count_in_seconds {
            if count_in_duration > 0.0 {
                let count_in_start = start_seconds - count_in_duration;
                sections.insert(
                    0,
                    Section {
                        id: None,
                        name: "Count-In".to_string(),
                        section_type: SectionType::CountIn,
                        start_seconds: count_in_start,
                        end_seconds: start_seconds,
                        number: None,
                        color: None,
                    },
                );
                count_in_start // Song now starts from count-in
            } else {
                start_seconds
            }
        } else {
            start_seconds
        };

        // Add hardcoded END section if there's a gap between SONGEND and =END
        if end_seconds > songend_seconds {
            sections.push(Section {
                id: None,
                name: "End".to_string(),
                section_type: SectionType::End,
                start_seconds: songend_seconds,
                end_seconds,
                number: None,
                color: None,
            });
        }

        // Get tempo and time signature at song start
        let tempo = tempo_map.tempo_at(start_seconds).await.ok();
        let time_sig = tempo_map
            .time_signature_at(start_seconds)
            .await
            .ok()
            .map(|(num, denom)| daw_proto::TimeSignature::new(num as u32, denom as u32));

        // Build measure positions if we have tempo and time signature
        // Use song_start_seconds to include count-in in measure positions
        let measure_positions = if let (Some(bpm), Some(ts)) = (tempo, time_sig) {
            Self::calculate_measure_positions(song_start_seconds, end_seconds, bpm, ts)
        } else {
            Vec::new()
        };

        Ok(Song {
            id: Uuid::new_v4().to_string(),
            name: song_name,
            project_guid: project.guid().to_string(),
            start_seconds: song_start_seconds, // Include count-in in song bounds
            end_seconds,
            count_in_seconds,
            sections,
            tempo,
            time_signature: time_sig,
            measure_positions,
        })
    }

    /// Extract song name from marker/region name
    ///
    /// Examples:
    /// - "SONGSTART: My Song" -> "My Song"
    /// - "=SONGSTART My Song" -> "My Song"
    /// - "=START My Song" -> "My Song"
    /// - "SONG: Title" -> "Title"
    fn extract_song_name(name: &str) -> String {
        // Remove common prefixes
        let name = name
            .trim()
            .trim_start_matches("=SONGSTART")
            .trim_start_matches("=SONGEND")
            .trim_start_matches("SONGSTART")
            .trim_start_matches("SONG START")
            .trim_start_matches("=START")
            .trim_start_matches("SONG:")
            .trim_start_matches("SONG")
            .trim_start_matches(':')
            .trim();

        if name.is_empty() {
            "Untitled Song".to_string()
        } else {
            name.to_string()
        }
    }

    /// Extract sections from regions within song bounds
    fn extract_sections(
        regions: &[daw_proto::Region],
        start: f64,
        end: f64,
    ) -> eyre::Result<Vec<Section>> {
        let mut sections: Vec<Section> = regions
            .iter()
            .filter(|r| {
                // Include regions that overlap with song bounds
                let r_start = r.time_range.start_seconds();
                let r_end = r.time_range.end_seconds();
                r_start < end && r_end > start
            })
            .filter(|r| {
                // Exclude the SONG region itself
                !r.name.starts_with("SONG:") && r.name.to_uppercase() != "SONG"
            })
            .map(|r| {
                let (section_type, number) = Self::parse_section_name(&r.name);
                let r_start = r.time_range.start_seconds();
                let r_end = r.time_range.end_seconds();
                Section {
                    id: r.id,
                    name: r.name.clone(),
                    section_type,
                    start_seconds: r_start.max(start),
                    end_seconds: r_end.min(end),
                    number,
                    color: r.color,
                }
            })
            .collect();

        // Sort sections by start position
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

    /// Parse section type and number from region name
    ///
    /// Examples:
    /// - "Verse 1" -> (Verse, Some(1))
    /// - "V2" -> (Verse, Some(2))
    /// - "Chorus" -> (Chorus, None)
    /// - "Bridge 1" -> (Bridge, Some(1))
    fn parse_section_name(name: &str) -> (SectionType, Option<u32>) {
        let name_upper = name.to_uppercase();

        // Try to extract number from end
        let (type_part, number) = if let Some(last_space) = name_upper.rfind(' ') {
            let potential_num = &name_upper[last_space + 1..];
            if let Ok(num) = potential_num.parse::<u32>() {
                (&name_upper[..last_space], Some(num))
            } else {
                (name_upper.as_str(), None)
            }
        } else {
            // Try to extract number from end of string (e.g., "V1")
            let last_char_num = name_upper
                .chars()
                .rev()
                .take_while(|c| c.is_ascii_digit())
                .collect::<String>()
                .chars()
                .rev()
                .collect::<String>();

            if !last_char_num.is_empty() {
                let type_len = name_upper.len() - last_char_num.len();
                let num = last_char_num.parse::<u32>().ok();
                (&name_upper[..type_len], num)
            } else {
                (name_upper.as_str(), None)
            }
        };

        let section_type = SectionType::parse(type_part.trim());

        (section_type, number)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_song_name() {
        assert_eq!(
            SongBuilder::extract_song_name("SONGSTART: My Song"),
            "My Song"
        );
        assert_eq!(
            SongBuilder::extract_song_name("=SONGSTART How Great is Our God"),
            "How Great is Our God"
        );
        assert_eq!(
            SongBuilder::extract_song_name("=START Test Track"),
            "Test Track"
        );
        assert_eq!(SongBuilder::extract_song_name("SONG: Title"), "Title");
        assert_eq!(SongBuilder::extract_song_name("SONGSTART"), "Untitled Song");
    }

    #[test]
    fn test_parse_section_name() {
        assert_eq!(
            SongBuilder::parse_section_name("Verse 1"),
            (SectionType::Verse, Some(1))
        );
        assert_eq!(
            SongBuilder::parse_section_name("V2"),
            (SectionType::Verse, Some(2))
        );
        assert_eq!(
            SongBuilder::parse_section_name("Chorus"),
            (SectionType::Chorus, None)
        );
        assert_eq!(
            SongBuilder::parse_section_name("Bridge 1"),
            (SectionType::Bridge, Some(1))
        );
        assert_eq!(
            SongBuilder::parse_section_name("C1"),
            (SectionType::Chorus, Some(1))
        );
    }
}
