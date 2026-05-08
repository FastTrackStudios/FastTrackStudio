//! Integration tests for the Take service (#25).
//!
//! Round-trip coverage for the chunk/source pieces wired in #25:
//! - add take → preserve_pitch toggle → re-read survives
//! - source detection on a stock MIDI take
//! - set_source_file on a generated WAV → source_type flips Audio
//! - delete_take via action 40129 → take_count decreases by one
//!
//! Run with: `cargo xtask reaper-test -- reaper_takes`

use daw_proto::SourceType;
use reaper_test::reaper_test;
use std::fs;
use std::path::PathBuf;

/// Write a 0.1-second 44.1k mono silent PCM-16 WAV to `/tmp` and return
/// the path. Hand-rolled — we don't want a `hound`/`tempfile` dep just
/// for one test.
fn write_silence_wav(name: &str) -> PathBuf {
    let sample_rate: u32 = 44_100;
    let num_samples: u32 = 4_410; // 0.1s
    let bits_per_sample: u16 = 16;
    let channels: u16 = 1;
    let byte_rate = sample_rate * (bits_per_sample as u32 / 8) * channels as u32;
    let block_align = channels * bits_per_sample / 8;
    let data_size = num_samples * block_align as u32;
    let chunk_size = 36 + data_size;

    let mut buf: Vec<u8> = Vec::with_capacity(44 + data_size as usize);
    buf.extend_from_slice(b"RIFF");
    buf.extend_from_slice(&chunk_size.to_le_bytes());
    buf.extend_from_slice(b"WAVE");
    buf.extend_from_slice(b"fmt ");
    buf.extend_from_slice(&16u32.to_le_bytes()); // fmt chunk size
    buf.extend_from_slice(&1u16.to_le_bytes()); // PCM
    buf.extend_from_slice(&channels.to_le_bytes());
    buf.extend_from_slice(&sample_rate.to_le_bytes());
    buf.extend_from_slice(&byte_rate.to_le_bytes());
    buf.extend_from_slice(&block_align.to_le_bytes());
    buf.extend_from_slice(&bits_per_sample.to_le_bytes());
    buf.extend_from_slice(b"data");
    buf.extend_from_slice(&data_size.to_le_bytes());
    buf.extend(std::iter::repeat_n(0u8, data_size as usize));

    let path = std::env::temp_dir().join(format!("daw-take-test-{name}.wav"));
    fs::write(&path, &buf).expect("write tmp wav");
    path
}

#[reaper_test(isolated)]
async fn take_delete_decreases_count(ctx: &reaper_test::ReaperTestContext) -> eyre::Result<()> {
    let project = ctx.project().clone();
    let track = project.tracks().add("Take Delete Track", None).await?;
    let item = track
        .items()
        .add(
            daw_proto::PositionInSeconds::from_seconds(0.0),
            daw_proto::Duration::from_seconds(2.0),
        )
        .await?;

    // Items start with one take. Add a second take so we have something
    // to delete without leaving the item empty.
    item.takes().add().await?;
    let before = item.info().await?.take_count;
    assert!(
        before >= 2,
        "expected at least 2 takes after add(), got {before}"
    );

    // Delete take #1 (index 1) via the new chunk-action path.
    let take_to_kill = item
        .takes()
        .by_index(1)
        .await?
        .ok_or_else(|| eyre::eyre!("take #1 missing pre-delete"))?;
    take_to_kill.delete().await?;

    let after = item.info().await?.take_count;
    assert_eq!(
        after,
        before - 1,
        "delete_take should decrement take_count: {before} → {after}"
    );

    Ok(())
}

#[reaper_test(isolated)]
async fn take_preserve_pitch_round_trips(ctx: &reaper_test::ReaperTestContext) -> eyre::Result<()> {
    let project = ctx.project().clone();
    let track = project.tracks().add("Preserve Pitch Track", None).await?;
    let item = track
        .items()
        .add(
            daw_proto::PositionInSeconds::from_seconds(0.0),
            daw_proto::Duration::from_seconds(1.0),
        )
        .await?;
    let take = item.takes().active().await?;

    take.set_preserve_pitch(true).await?;
    let info_on = take.info().await?;
    assert!(
        info_on.preserve_pitch,
        "preserve_pitch should be true after set(true)"
    );

    take.set_preserve_pitch(false).await?;
    let info_off = take.info().await?;
    assert!(
        !info_off.preserve_pitch,
        "preserve_pitch should be false after set(false)"
    );

    Ok(())
}

#[reaper_test(isolated)]
async fn take_set_source_file_flips_to_audio(
    ctx: &reaper_test::ReaperTestContext,
) -> eyre::Result<()> {
    let wav = write_silence_wav("source_swap");
    let path_str = wav.to_string_lossy().into_owned();

    let project = ctx.project().clone();
    let track = project.tracks().add("Source Swap Track", None).await?;
    let item = track
        .items()
        .add(
            daw_proto::PositionInSeconds::from_seconds(0.0),
            daw_proto::Duration::from_seconds(0.5),
        )
        .await?;
    let take = item.takes().active().await?;

    take.set_source_file(&path_str).await?;
    let kind = take.source_type().await?;
    assert!(
        matches!(kind, SourceType::Audio),
        "after set_source_file('{path_str}'), source_type should be Audio, got {kind:?}"
    );

    let _ = fs::remove_file(&wav);
    Ok(())
}

#[reaper_test(isolated)]
async fn take_source_type_detected_for_default(
    ctx: &reaper_test::ReaperTestContext,
) -> eyre::Result<()> {
    let project = ctx.project().clone();
    let track = project.tracks().add("Source Type Track", None).await?;
    let item = track
        .items()
        .add(
            daw_proto::PositionInSeconds::from_seconds(0.0),
            daw_proto::Duration::from_seconds(1.0),
        )
        .await?;
    let take = item.takes().active().await?;

    // The detector must classify the source as something concrete —
    // never `Unknown`. The exact type depends on REAPER's default for
    // a freshly-added item (typically MIDI on Linux), so we accept
    // any of the live variants.
    let kind = take.source_type().await?;
    assert!(
        matches!(
            kind,
            SourceType::Empty | SourceType::Audio | SourceType::Midi | SourceType::Video
        ),
        "default take source_type should be detected, got {kind:?}"
    );

    Ok(())
}
