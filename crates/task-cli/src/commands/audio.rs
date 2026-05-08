//! `task audio` — audio-production workflow commands.

use crate::shared::{RemoteVoxConfig, remote_find_project_with_client};
use clap::{Args, Subcommand};
use task_core::service::{AddTrackRequest, SubmitMixRequest, TrackPatch};
use task_core::track::TrackApi;
use uuid::Uuid;

#[derive(Subcommand)]
pub(crate) enum AudioCommands {
    /// Track CRUD + status / metadata commands.
    Tracks {
        #[command(subcommand)]
        command: TrackCommands,
    },
    /// Mix workflow operations.
    Mix {
        #[command(subcommand)]
        command: MixCommands,
    },
}

#[derive(Subcommand)]
pub(crate) enum TrackCommands {
    /// List the tracks for a project, ordered by sequence.
    List {
        /// Project name or UUID.
        #[arg(long)]
        project: String,
        #[arg(long)]
        json: bool,
    },
    /// Append a new track to a project.
    Add(AddTrackArgs),
    /// Show one track.
    Show {
        /// Track UUID, or title (when --project is set).
        track: String,
        #[arg(long)]
        project: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Set the musical key.
    SetKey { track: String, key: String },
    /// Set the BPM.
    SetBpm { track: String, bpm: f64 },
    /// Set the duration in milliseconds.
    SetDuration { track: String, duration_ms: i64 },
    /// Set DAW session / stems / reference paths.
    SetPaths {
        track: String,
        #[arg(long)]
        daw: Option<String>,
        #[arg(long)]
        stems: Option<String>,
        #[arg(long)]
        reference: Option<String>,
    },
    /// Transition to a new status.
    Transition { track: String, new_status: String },
}

#[derive(Args)]
pub(crate) struct AddTrackArgs {
    /// Project name or UUID.
    #[arg(long)]
    project: String,
    #[arg(long)]
    title: String,
    #[arg(long)]
    sequence: Option<u32>,
    #[arg(long)]
    bpm: Option<f64>,
    #[arg(long)]
    key: Option<String>,
    #[arg(long)]
    artist: Option<String>,
}

#[derive(Subcommand)]
pub(crate) enum MixCommands {
    /// Submit a mix bounce, incrementing the revision counter and
    /// (optionally) recording an attachment row.
    Submit {
        track: String,
        #[arg(long)]
        notes: Option<String>,
        #[arg(long)]
        path: Option<String>,
        #[arg(long)]
        label: Option<String>,
        #[arg(long)]
        mime: Option<String>,
        #[arg(long)]
        uploader: Option<String>,
    },
    /// Approve the current mix — sets status to Approved and stamps
    /// approved_by / approved_at into the track's properties.
    Approve {
        track: String,
        #[arg(long)]
        actor: Option<String>,
    },
}

pub(crate) async fn run_remote_audio_command(
    remote: &RemoteVoxConfig,
    actor: Option<&str>,
    command: AudioCommands,
) -> eyre::Result<()> {
    let client = remote.audio().await?;
    match command {
        AudioCommands::Tracks { command } => run_tracks(remote, &client, command).await,
        AudioCommands::Mix { command } => run_mix(remote, &client, actor, command).await,
    }
}

async fn run_tracks(
    remote: &RemoteVoxConfig,
    client: &task_core::service::AudioProductionServiceClient,
    command: TrackCommands,
) -> eyre::Result<()> {
    match command {
        TrackCommands::List { project, json } => {
            let project_id = resolve_project_id(remote, &project).await?;
            let tracks = client
                .list_tracks(project_id)
                .await
                .map_err(|e| eyre::eyre!("list tracks: {e}"))?;
            print_tracks(&tracks, json)?;
        }
        TrackCommands::Add(args) => {
            let project_id = resolve_project_id(remote, &args.project).await?;
            let request = AddTrackRequest {
                project_id,
                title: args.title,
                sequence: args.sequence,
                bpm: args.bpm,
                key: args.key,
                artist: args.artist,
                created_by: None,
            };
            let track = client
                .add_track(request)
                .await
                .map_err(|e| eyre::eyre!("add track: {e}"))?;
            print_track(&track, false)?;
        }
        TrackCommands::Show {
            track,
            project,
            json,
        } => {
            let id = resolve_track_id(remote, client, &track, project.as_deref()).await?;
            let track = client
                .get_track(id)
                .await
                .map_err(|e| eyre::eyre!("get track: {e}"))?
                .ok_or_else(|| eyre::eyre!("track not found"))?;
            print_track(&track, json)?;
        }
        TrackCommands::SetKey { track, key } => {
            let id = resolve_track_id(remote, client, &track, None).await?;
            let updated = client
                .update_track(
                    id,
                    TrackPatch {
                        key: Some(key),
                        ..Default::default()
                    },
                )
                .await
                .map_err(|e| eyre::eyre!("update: {e}"))?;
            print_track(&updated, false)?;
        }
        TrackCommands::SetBpm { track, bpm } => {
            let id = resolve_track_id(remote, client, &track, None).await?;
            let updated = client
                .update_track(
                    id,
                    TrackPatch {
                        bpm: Some(bpm),
                        ..Default::default()
                    },
                )
                .await
                .map_err(|e| eyre::eyre!("update: {e}"))?;
            print_track(&updated, false)?;
        }
        TrackCommands::SetDuration { track, duration_ms } => {
            let id = resolve_track_id(remote, client, &track, None).await?;
            let updated = client
                .update_track(
                    id,
                    TrackPatch {
                        duration_ms: Some(duration_ms),
                        ..Default::default()
                    },
                )
                .await
                .map_err(|e| eyre::eyre!("update: {e}"))?;
            print_track(&updated, false)?;
        }
        TrackCommands::SetPaths {
            track,
            daw,
            stems,
            reference,
        } => {
            let id = resolve_track_id(remote, client, &track, None).await?;
            let updated = client
                .update_track(
                    id,
                    TrackPatch {
                        daw_session_path: daw,
                        stems_path: stems,
                        reference_url: reference,
                        ..Default::default()
                    },
                )
                .await
                .map_err(|e| eyre::eyre!("update: {e}"))?;
            print_track(&updated, false)?;
        }
        TrackCommands::Transition { track, new_status } => {
            let id = resolve_track_id(remote, client, &track, None).await?;
            let updated = client
                .transition_status(id, new_status)
                .await
                .map_err(|e| eyre::eyre!("transition: {e}"))?;
            print_track(&updated, false)?;
        }
    }
    Ok(())
}

async fn run_mix(
    remote: &RemoteVoxConfig,
    client: &task_core::service::AudioProductionServiceClient,
    actor: Option<&str>,
    command: MixCommands,
) -> eyre::Result<()> {
    match command {
        MixCommands::Submit {
            track,
            notes,
            path,
            label,
            mime,
            uploader,
        } => {
            let id = resolve_track_id(remote, client, &track, None).await?;
            let updated = client
                .submit_mix(SubmitMixRequest {
                    track_id: id,
                    notes,
                    mix_path: path,
                    mix_label: label,
                    mix_mime: mime,
                    uploader: uploader.or_else(|| actor.map(str::to_string)),
                })
                .await
                .map_err(|e| eyre::eyre!("submit_mix: {e}"))?;
            println!(
                "Mix submitted (rev {} → status {}).",
                updated.revision_number,
                updated.status.as_str()
            );
            print_track(&updated, false)?;
        }
        MixCommands::Approve { track, actor: who } => {
            let id = resolve_track_id(remote, client, &track, None).await?;
            let actor_value = who
                .or_else(|| actor.map(str::to_string))
                .ok_or_else(|| eyre::eyre!("--actor or --as-user is required to approve"))?;
            let updated = client
                .approve_mix(id, actor_value)
                .await
                .map_err(|e| eyre::eyre!("approve_mix: {e}"))?;
            println!("Approved.");
            print_track(&updated, false)?;
        }
    }
    Ok(())
}

async fn resolve_project_id(remote: &RemoteVoxConfig, reference: &str) -> eyre::Result<Uuid> {
    if let Ok(id) = Uuid::parse_str(reference) {
        return Ok(id);
    }
    let repo = remote.project_repo().await?;
    let project = remote_find_project_with_client(&repo, reference).await?;
    Ok(project.id)
}

async fn resolve_track_id(
    remote: &RemoteVoxConfig,
    client: &task_core::service::AudioProductionServiceClient,
    reference: &str,
    project: Option<&str>,
) -> eyre::Result<Uuid> {
    if let Ok(id) = Uuid::parse_str(reference) {
        return Ok(id);
    }
    let project_ref = project
        .ok_or_else(|| eyre::eyre!("track lookups by title require --project (or pass a UUID)"))?;
    let project_id = resolve_project_id(remote, project_ref).await?;
    let tracks = client
        .list_tracks(project_id)
        .await
        .map_err(|e| eyre::eyre!("list tracks: {e}"))?;
    tracks
        .into_iter()
        .find(|t| t.title.eq_ignore_ascii_case(reference))
        .map(|t| t.id)
        .ok_or_else(|| eyre::eyre!("track not found: {reference}"))
}

fn print_tracks(tracks: &[TrackApi], json: bool) -> eyre::Result<()> {
    if json {
        println!("{}", serde_json::to_string_pretty(tracks)?);
    } else if tracks.is_empty() {
        println!("(no tracks)");
    } else {
        for t in tracks {
            println!(
                "{:>3}. {:<32} {:<10} bpm={:<6} key={:<6} rev={}",
                t.sequence,
                t.title,
                t.status.as_str(),
                t.bpm.map(|n| format!("{n:.1}")).unwrap_or_default(),
                t.key.clone().unwrap_or_default(),
                t.revision_number,
            );
        }
    }
    Ok(())
}

fn print_track(track: &TrackApi, json: bool) -> eyre::Result<()> {
    if json {
        println!("{}", serde_json::to_string_pretty(track)?);
        return Ok(());
    }
    println!("Track {}", track.id);
    println!("  title:    {}", track.title);
    println!("  project:  {}", track.project_id);
    println!("  sequence: {}", track.sequence);
    println!("  status:   {}", track.status.as_str());
    println!("  revision: {}", track.revision_number);
    if let Some(bpm) = track.bpm {
        println!("  bpm:      {bpm}");
    }
    if let Some(key) = &track.key {
        println!("  key:      {key}");
    }
    if let Some(ts) = &track.time_signature {
        println!("  time_sig: {ts}");
    }
    if let Some(d) = track.duration_ms {
        println!("  duration: {d} ms");
    }
    if let Some(artist) = &track.artist {
        println!("  artist:   {artist}");
    }
    if let Some(p) = &track.daw_session_path {
        println!("  daw:      {p}");
    }
    if let Some(p) = &track.stems_path {
        println!("  stems:    {p}");
    }
    if let Some(p) = &track.reference_url {
        println!("  ref:      {p}");
    }
    if let Some(notes) = &track.notes {
        println!("  notes:    {notes}");
    }
    Ok(())
}
