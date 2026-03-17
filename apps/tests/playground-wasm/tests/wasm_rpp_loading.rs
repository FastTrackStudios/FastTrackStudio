//! WASM tests for RPP loading and audio file resolution.
//!
//! These test the RPP parser + audio resolver logic in WASM without actual
//! audio files (testing the parsing and filename matching path).

use wasm_bindgen_test::*;

wasm_bindgen_test_configure!(run_in_browser);

use daw::standalone::audio_engine::rpp_loader;

/// Minimal RPP with one WAVE item for testing
const MINI_RPP: &str = r#"<REAPER_PROJECT 0.1 "7.0/macOS-arm64" 0 0
  SAMPLERATE 48000 0 0
  <TRACK {AAAA-BBBB}
    NAME "Bass"
    <ITEM
      POSITION 2.5
      LENGTH 10.0
      SOFFS 0.5
      <SOURCE WAVE
        FILE "Media/bass.wav"
      >
    >
  >
  <TRACK {CCCC-DDDD}
    NAME "Vocals"
    <ITEM
      POSITION 0.0
      LENGTH 15.0
      SOFFS 0
      <SOURCE WAVE
        FILE "vocals.wav"
      >
    >
  >
>
"#;

#[wasm_bindgen_test]
fn list_audio_files_from_rpp() {
    let files = rpp_loader::list_audio_files(MINI_RPP).unwrap();
    assert_eq!(files.len(), 2);
    assert!(files.contains(&"Media/bass.wav".to_string()));
    assert!(files.contains(&"vocals.wav".to_string()));
}

#[wasm_bindgen_test]
fn load_rpp_with_no_audio_reports_failures() {
    let engine = match daw::standalone::audio_engine::AudioEngine::new() {
        Ok(e) => e,
        Err(_) => return, // Skip if no audio device in headless
    };

    let result = rpp_loader::load_rpp(&engine, MINI_RPP, |_path| {
        None // No files provided
    });

    let project = result.unwrap();
    assert_eq!(
        project.tracks.len(),
        0,
        "No tracks should load without audio files"
    );
    assert_eq!(
        project.failed.len(),
        2,
        "Both files should be in failed list"
    );
}

#[wasm_bindgen_test]
fn load_rpp_filename_matching() {
    // Simulate uploaded files with just filenames (no Media/ prefix)
    let mut uploaded: std::collections::HashMap<String, Vec<u8>> = std::collections::HashMap::new();
    // Empty bytes won't decode, but we can test the resolver gets called
    uploaded.insert("bass.wav".to_string(), vec![1, 2, 3]);
    uploaded.insert("vocals.wav".to_string(), vec![4, 5, 6]);

    let mut resolved = Vec::new();

    let engine = match daw::standalone::audio_engine::AudioEngine::new() {
        Ok(e) => e,
        Err(_) => return,
    };

    let _result = rpp_loader::load_rpp(&engine, MINI_RPP, |file_path| {
        // Try exact match
        if let Some(bytes) = uploaded.get(file_path) {
            resolved.push((file_path.to_string(), "exact".to_string()));
            return Some(bytes.clone());
        }
        // Try filename only
        let filename = file_path.rsplit(['/', '\\']).next().unwrap_or(file_path);
        if let Some(bytes) = uploaded.get(filename) {
            resolved.push((file_path.to_string(), format!("filename:{filename}")));
            return Some(bytes.clone());
        }
        resolved.push((file_path.to_string(), "not_found".to_string()));
        None
    });

    // "vocals.wav" should match exactly, "Media/bass.wav" should match via filename
    assert!(
        resolved
            .iter()
            .any(|(path, how)| path == "vocals.wav" && how == "exact"),
        "vocals.wav should exact-match. Got: {resolved:?}"
    );
    assert!(
        resolved
            .iter()
            .any(|(path, how)| path == "Media/bass.wav" && how.starts_with("filename")),
        "Media/bass.wav should match via filename stripping. Got: {resolved:?}"
    );
}

/// Test with the actual Vienna RPP text (no audio files — just parsing)
#[wasm_bindgen_test]
fn parse_vienna_rpp_in_wasm() {
    // Include the RPP at compile time so it's available in WASM
    let rpp_text = include_str!(
        "/Users/codywright/Music/Projects/Live Tracks/Just Friends/Vienna - Couch/Vienna - Couch.RPP"
    );

    let files = rpp_loader::list_audio_files(rpp_text).unwrap();
    assert!(!files.is_empty(), "Vienna RPP should reference audio files");

    // Should find the 6 stem files
    let wav_files: Vec<_> = files.iter().filter(|f| f.ends_with(".wav")).collect();
    assert!(
        wav_files.len() >= 6,
        "Expected at least 6 WAV files, got {}: {:?}",
        wav_files.len(),
        wav_files
    );

    // Verify specific files
    let has_bass = files.iter().any(|f| f.contains("Bass"));
    let has_vocals = files.iter().any(|f| f.contains("Vocals"));
    let has_drums = files.iter().any(|f| f.contains("Drums"));
    assert!(has_bass, "Should find Bass stem");
    assert!(has_vocals, "Should find Vocals stem");
    assert!(has_drums, "Should find Drums stem");
}
