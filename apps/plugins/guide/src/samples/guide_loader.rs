//! Guide sample loading logic
//!
//! Handles loading guide samples based on section types and managing the guide sample cache.

use std::collections::HashMap;
use std::error::Error;
use std::sync::{Arc, Mutex};
use symphonium::DecodedAudioF32;
use tracing::{info, warn};

use crate::SectionInfo;

/// Get the base path for guide samples from environment or use default
fn get_guide_base_path() -> String {
    std::env::var("FTS_GUIDE_SECTION_PATH").unwrap_or_else(|_| {
        // Default path - user should set FTS_GUIDE_SECTION_PATH environment variable
        "/Users/codywright/Music/Audiohaven/Sample Libraries/FTS-GUIDE/Section Guide/".to_string()
    })
}

/// List of all guide sample filenames to load
/// This includes all section guide files in the directory
const GUIDE_SAMPLE_FILES: &[&str] = &[
    "English Female - Acapella.wav",
    "English Female - Breakdown.wav",
    "English Female - Bridge.wav",
    "English Female - Bridge 1.wav",
    "English Female - Bridge 2.wav",
    "English Female - Bridge 3.wav",
    "English Female - Bridge 4.wav",
    "English Female - Chorus.wav",
    "English Female - Chorus 1.wav",
    "English Female - Chorus 2.wav",
    "English Female - Chorus 3.wav",
    "English Female - Chorus 4.wav",
    "English Female - Ending.wav",
    "English Female - Exhortation.wav",
    "English Female - Instrumental.wav",
    "English Female - Interlude.wav",
    "English Female - Intro.wav",
    "English Female - Outro.wav",
    "English Female - Post Chorus.wav",
    "English Female - Pre Chorus.wav",
    "English Female - Pre Chorus 1.wav",
    "English Female - Pre Chorus 2.wav",
    "English Female - Pre Chorus 3.wav",
    "English Female - Pre Chorus 4.wav",
    "English Female - Rap.wav",
    "English Female - Refrain.wav",
    "English Female - Solo.wav",
    "English Female - Tag.wav",
    "English Female - Turnaround.wav",
    "English Female - Vamp.wav",
    "English Female - Verse.wav",
    "English Female - Verse 1.wav",
    "English Female - Verse 2.wav",
    "English Female - Verse 3.wav",
    "English Female - Verse 4.wav",
    "English Female - Verse 5.wav",
    "English Female - Verse 6.wav",
];

/// Map a section to a guide file name
/// Returns the filename (without path) for the guide sample, or None if no mapping exists
/// NOTE: Always uses unnumbered filenames (e.g., "Chorus" not "Chorus 1")
pub fn section_to_guide_filename(
    section_type_name: &str,
    _section_number: Option<u32>,
) -> Option<String> {
    // Normalize section type name (capitalize first letter)
    let type_lower = section_type_name.to_lowercase();
    let type_capitalized = if let Some(first) = type_lower.chars().next() {
        first.to_uppercase().to_string() + &type_lower[1..]
    } else {
        type_lower
    };

    // Map section types to guide file names
    // Handle special cases first
    let base_type = match type_capitalized.as_str() {
        "Verse" | "Vs" => "Verse",
        "Chorus" | "Ch" => "Chorus",
        "Bridge" | "Br" => "Bridge",
        "Intro" => "Intro",
        "Outro" => "Outro",
        "Instrumental" => "Instrumental",
        "Pre-Chorus" | "Pre Chorus" | "Pre chorus" => "Pre Chorus",
        "Post-Chorus" | "Post Chorus" | "Post chorus" => "Post Chorus",
        "Breakdown" => "Breakdown",
        "Interlude" => "Interlude",
        "Tag" => "Tag",
        "Ending" => "Ending",
        "Solo" => "Solo",
        "Vamp" => "Vamp",
        "Turnaround" => "Turnaround",
        "Refrain" => "Refrain",
        "Rap" => "Rap",
        "Acapella" => "Acapella",
        "Exhortation" => "Exhortation",
        _ => return None, // Unknown section type
    };

    // Always use unnumbered filename (no section numbers)
    Some(format!("English Female - {base_type}.wav"))
}

/// Get guide sample key for a section
/// Key format: "{section_type}_{number}"
pub fn get_guide_key(section_type_name: &str, section_number: Option<u32>) -> String {
    let number_str = section_number
        .map(|n| n.to_string())
        .unwrap_or_else(|| "None".to_string());
    format!("{section_type_name}_{number_str}")
}

/// Guide sample loader
pub struct GuideSampleLoader;

impl GuideSampleLoader {
    /// Load ALL guide samples from the directory at initialization
    /// Similar to how click samples are loaded - all at once, cached statically
    pub fn load_all_samples(
        sample_rate: f32,
        guide_samples: &Arc<Mutex<HashMap<String, DecodedAudioF32>>>,
    ) {
        let base_path = get_guide_base_path();

        info!(
            sample_rate,
            guide_count = GUIDE_SAMPLE_FILES.len(),
            base_path = %base_path,
            "Loading all guide samples from directory"
        );

        let mut loaded_count = 0;
        let mut failed_count = 0;

        for filename in GUIDE_SAMPLE_FILES {
            // Check static cache first
            if let Some(cached_sample) =
                super::cache::SampleCache::get_guide_sample_from_cache(filename, sample_rate)
            {
                // Use cached sample
                let guide_key = Self::filename_to_key(filename);
                let mut guide_samples_guard = guide_samples.lock().unwrap();
                guide_samples_guard.insert(guide_key, cached_sample);
                loaded_count += 1;
                continue;
            }

            // Cache miss - load from disk
            let guide_path = format!("{base_path}{filename}");

            match super::loader::SampleLoader::load_file(&guide_path, sample_rate) {
                Ok(decoded_audio) => {
                    // Store in static cache
                    super::cache::SampleCache::cache_guide_sample(
                        (*filename).to_string(),
                        sample_rate,
                        decoded_audio.clone(),
                    );

                    // Store in instance HashMap with proper key
                    let guide_key = Self::filename_to_key(filename);
                    let guide_key_clone = guide_key.clone();
                    let mut guide_samples_guard = guide_samples.lock().unwrap();
                    guide_samples_guard.insert(guide_key, decoded_audio);
                    loaded_count += 1;

                    info!(
                        filename = %filename,
                        guide_key = %guide_key_clone,
                        "Loaded guide sample"
                    );
                }
                Err(e) => {
                    warn!(
                        filename = %filename,
                        error = %e,
                        "Failed to load guide sample"
                    );
                    failed_count += 1;
                }
            }
        }

        info!(
            loaded_count,
            failed_count,
            total_count = GUIDE_SAMPLE_FILES.len(),
            sample_rate,
            "Finished loading all guide samples"
        );
    }

    /// Convert a filename to a guide key for lookup
    /// Handles numbered variations (e.g., "Verse 1" -> "Verse_1")
    fn filename_to_key(filename: &str) -> String {
        // Remove "English Female - " prefix and ".wav" suffix
        let without_prefix = filename
            .strip_prefix("English Female - ")
            .unwrap_or(filename);
        let without_ext = without_prefix.strip_suffix(".wav").unwrap_or(without_prefix);

        // Split on space to separate type and number
        let parts: Vec<&str> = without_ext.split(' ').collect();
        if parts.len() == 1 {
            // No number (e.g., "Intro")
            format!("{}_None", parts[0])
        } else if parts.len() == 2 {
            // Has number (e.g., "Verse 1")
            format!("{}_{}", parts[0], parts[1])
        } else {
            // Multi-word type (e.g., "Pre Chorus 1")
            let section_type = parts[0..parts.len() - 1].join(" ");
            let number = parts[parts.len() - 1];
            format!("{section_type}_{number}")
        }
    }

    /// Load a guide sample for a section (legacy method, now uses cache)
    pub fn load_sample(
        section_info: &SectionInfo,
        sample_rate: f32,
        guide_samples: &Arc<Mutex<HashMap<String, DecodedAudioF32>>>,
    ) -> Result<(), Box<dyn Error>> {
        let filename = match section_to_guide_filename(
            &section_info.section_type_name,
            section_info.section_number,
        ) {
            Some(f) => f,
            None => return Ok(()), // No guide file for this section type
        };

        // Check static cache first
        if let Some(cached_sample) =
            super::cache::SampleCache::get_guide_sample_from_cache(&filename, sample_rate)
        {
            let guide_key =
                get_guide_key(&section_info.section_type_name, section_info.section_number);
            let mut guide_samples_guard = guide_samples.lock().unwrap();
            guide_samples_guard.insert(guide_key, cached_sample);
            return Ok(());
        }

        // Cache miss - load from disk
        let base_path = get_guide_base_path();
        let guide_path = format!("{base_path}{filename}");

        let decoded_audio = super::loader::SampleLoader::load_file(&guide_path, sample_rate)?;

        // Store in static cache
        super::cache::SampleCache::cache_guide_sample(
            filename.clone(),
            sample_rate,
            decoded_audio.clone(),
        );

        let guide_key =
            get_guide_key(&section_info.section_type_name, section_info.section_number);
        let mut guide_samples = guide_samples.lock().unwrap();
        guide_samples.insert(guide_key, decoded_audio);

        Ok(())
    }

    /// Load guide samples for multiple sections
    pub fn load_samples(
        sections: &[SectionInfo],
        sample_rate: f32,
        guide_samples: &Arc<Mutex<HashMap<String, DecodedAudioF32>>>,
    ) {
        for section_info in sections {
            if let Err(e) = Self::load_sample(section_info, sample_rate, guide_samples) {
                warn!(
                    section_name = %section_info.name,
                    error = %e,
                    "Failed to load guide sample"
                );
            } else {
                let guide_key =
                    get_guide_key(&section_info.section_type_name, section_info.section_number);
                if let Some(decoded_audio) = guide_samples.lock().unwrap().get(&guide_key) {
                    let filename = section_to_guide_filename(
                        &section_info.section_type_name,
                        section_info.section_number,
                    )
                    .unwrap_or_else(|| "unknown".to_string());
                    info!(
                        section_name = %section_info.name,
                        guide_filename = %filename,
                        frames = decoded_audio.frames(),
                        "Loaded guide sample"
                    );
                }
            }
        }
    }

    /// Load the "Ending" guide sample for SONGEND count-out
    pub fn load_ending_sample(
        sample_rate: f32,
        guide_samples: &Arc<Mutex<HashMap<String, DecodedAudioF32>>>,
    ) -> Result<(), Box<dyn Error>> {
        let base_path = get_guide_base_path();
        let ending_filename = "English Female - Ending.wav";
        let ending_path = format!("{base_path}{ending_filename}");

        let decoded_audio = super::loader::SampleLoader::load_file(&ending_path, sample_rate)?;
        let ending_key = "Ending_None".to_string();

        let mut guide_samples = guide_samples.lock().unwrap();
        let frame_count = decoded_audio.frames();
        guide_samples.insert(ending_key, decoded_audio);

        info!(
            frames = frame_count,
            "Loaded 'Ending' guide sample for SONGEND count-out"
        );
        Ok(())
    }
}
