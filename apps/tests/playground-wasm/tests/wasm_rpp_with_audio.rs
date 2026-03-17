//! Test RPP loading with actual audio data in WASM.
//!
//! Uses a minimal RPP and a synthetically generated WAV to test the full
//! pipeline: parse RPP → resolve files → decode audio → load into engine.

use wasm_bindgen_test::*;

wasm_bindgen_test_configure!(run_in_browser);

use daw::standalone::audio_engine::{AudioEngine, rpp_loader, test_tone, DecodedAudio};

/// Create a minimal WAV file from a DecodedAudio buffer.
fn encode_wav(audio: &DecodedAudio) -> Vec<u8> {
    let channels = audio.channels as u16;
    let sample_rate = audio.sample_rate;
    let bits_per_sample: u16 = 16;
    let byte_rate = sample_rate * channels as u32 * bits_per_sample as u32 / 8;
    let block_align = channels * bits_per_sample / 8;
    let data_size = audio.samples.len() as u32 * 2; // 16-bit = 2 bytes per sample

    let mut buf = Vec::with_capacity(44 + data_size as usize);

    // RIFF header
    buf.extend_from_slice(b"RIFF");
    buf.extend_from_slice(&(36 + data_size).to_le_bytes());
    buf.extend_from_slice(b"WAVE");

    // fmt chunk
    buf.extend_from_slice(b"fmt ");
    buf.extend_from_slice(&16u32.to_le_bytes()); // chunk size
    buf.extend_from_slice(&1u16.to_le_bytes()); // PCM format
    buf.extend_from_slice(&channels.to_le_bytes());
    buf.extend_from_slice(&sample_rate.to_le_bytes());
    buf.extend_from_slice(&byte_rate.to_le_bytes());
    buf.extend_from_slice(&block_align.to_le_bytes());
    buf.extend_from_slice(&bits_per_sample.to_le_bytes());

    // data chunk
    buf.extend_from_slice(b"data");
    buf.extend_from_slice(&data_size.to_le_bytes());
    for &sample in &audio.samples {
        let s16 = (sample.clamp(-1.0, 1.0) * 32767.0) as i16;
        buf.extend_from_slice(&s16.to_le_bytes());
    }

    buf
}

const TEST_RPP: &str = r#"<REAPER_PROJECT 0.1 "7.0" 0 0
  SAMPLERATE 44100 0 0
  <TRACK {TEST-TRACK-1}
    NAME "Sine"
    <ITEM
      POSITION 0.0
      LENGTH 1.0
      SOFFS 0
      <SOURCE WAVE
        FILE "sine.wav"
      >
    >
  >
  <TRACK {TEST-TRACK-2}
    NAME "Chord"
    <ITEM
      POSITION 0.5
      LENGTH 1.0
      SOFFS 0
      <SOURCE WAVE
        FILE "Media/chord.wav"
      >
    >
  >
>
"#;

#[wasm_bindgen_test]
fn full_rpp_load_with_synthetic_audio() {
    let engine = match AudioEngine::new() {
        Ok(e) => e,
        Err(_) => return, // Skip in headless if no audio device
    };

    // Generate test audio and encode as WAV
    let sine = test_tone::sine_wave(440.0, 2.0, 44100);
    let chord = test_tone::chord(&[261.63, 329.63, 392.0], 2.0, 44100);

    let sine_wav = encode_wav(&sine);
    let chord_wav = encode_wav(&chord);

    // Build the file map as the browser would provide it
    // (filenames only, no directory prefixes)
    let mut files: std::collections::HashMap<String, Vec<u8>> = std::collections::HashMap::new();
    files.insert("sine.wav".to_string(), sine_wav);
    files.insert("chord.wav".to_string(), chord_wav);

    let result = rpp_loader::load_rpp(&engine, TEST_RPP, |file_path| {
        // Exact match
        if let Some(bytes) = files.get(file_path) {
            return Some(bytes.clone());
        }
        // Filename-only match (strip directory)
        let filename = file_path.rsplit(['/', '\\']).next().unwrap_or(file_path);
        files.get(filename).cloned()
    });

    let project = result.unwrap();

    assert_eq!(project.tracks.len(), 2, "Should load 2 tracks");
    assert_eq!(project.failed.len(), 0, "No files should fail: {:?}", project.failed);
    assert_eq!(project.sample_rate, 44100);

    // Verify track details
    let sine_track = project.tracks.iter().find(|t| t.track_name == "Sine").unwrap();
    assert!((sine_track.position - 0.0).abs() < 0.01);
    assert!((sine_track.length - 1.0).abs() < 0.01);

    let chord_track = project.tracks.iter().find(|t| t.track_name == "Chord").unwrap();
    assert!((chord_track.position - 0.5).abs() < 0.01);
    assert!((chord_track.length - 1.0).abs() < 0.01);

    // Verify the engine has the tracks
    assert_eq!(engine.track_count(), 2);

    // Verify we can play
    engine.play();
    assert!(engine.is_playing());
    engine.stop();
}

#[wasm_bindgen_test]
fn rpp_load_partial_files_reports_missing() {
    let engine = match AudioEngine::new() {
        Ok(e) => e,
        Err(_) => return,
    };

    let sine = test_tone::sine_wave(440.0, 1.0, 44100);
    let sine_wav = encode_wav(&sine);

    // Only provide one of the two files
    let mut files: std::collections::HashMap<String, Vec<u8>> = std::collections::HashMap::new();
    files.insert("sine.wav".to_string(), sine_wav);
    // chord.wav is NOT provided

    let result = rpp_loader::load_rpp(&engine, TEST_RPP, |file_path| {
        if let Some(bytes) = files.get(file_path) {
            return Some(bytes.clone());
        }
        let filename = file_path.rsplit(['/', '\\']).next().unwrap_or(file_path);
        files.get(filename).cloned()
    });

    let project = result.unwrap();
    assert_eq!(project.tracks.len(), 1, "Only sine should load");
    assert_eq!(project.failed.len(), 1, "chord.wav should be in failed");
    assert!(project.failed[0].0.contains("chord.wav"));
}
