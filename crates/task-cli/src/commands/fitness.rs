//! `task fit` / `task fitness` — fitness workflow commands.
//!
//! Catalog half of the fitness workflow: Exercise CRUD + Routine
//! templates with positioned exercises. Logged workouts come in a
//! follow-up bead.

use std::path::PathBuf;

use clap::Subcommand;
use task_core::body_measurement::BodyMeasurementApi;
use task_core::exercise::ExerciseApi;
use task_core::routine::RoutineApi;
use task_core::routine_exercise::RoutineExerciseApi;
use task_core::service::{
    AddRoutineExerciseRequest, BodyMeasurementTrendRequest, BodyMeasurementTrendView,
    CompleteWorkoutSessionRequest, CreateExerciseRequest, CreateRoutineRequest, ExercisePatch,
    FitnessServiceClient, ListBodyMeasurementsRequest, LogSetRequest, MetricTrend,
    RecordBodyMeasurementRequest, RoutineWithExercisesView, StartWorkoutSessionRequest,
    UpdateBodyMeasurementRequest, UpdateSetRequest, WorkoutSessionView,
};
use task_core::set_log::SetLogApi;
use uuid::Uuid;

use crate::shared::RemoteVoxConfig;

#[derive(Subcommand)]
pub(crate) enum FitnessCommands {
    /// Canonical exercise catalog.
    Exercise {
        #[command(subcommand)]
        command: ExerciseCommands,
    },
    /// Routine templates + ordered exercise lists.
    Routine {
        #[command(subcommand)]
        command: RoutineCommands,
    },
    /// Logged workouts — start a session, check off sets as you finish them.
    Session {
        #[command(subcommand)]
        command: SessionCommands,
    },
    /// Quick weight log — shorthand for `measure record --weight-kg <kg>`.
    Weigh {
        weight_kg: f64,
        /// Override the timestamp (ISO 8601). Defaults to now.
        #[arg(long = "at")]
        at: Option<String>,
        /// Body-fat percent at this measurement.
        #[arg(long = "bf")]
        body_fat_percent: Option<f32>,
        #[arg(long = "note")]
        note: Option<String>,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Body measurements (weight, BF%, circumferences, photos).
    Measure {
        #[command(subcommand)]
        command: MeasureCommands,
    },
}

#[derive(Subcommand)]
pub(crate) enum ExerciseCommands {
    List {
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        modality: Option<String>,
        #[arg(long)]
        muscle: Option<String>,
        #[arg(long)]
        json: bool,
    },
    Show {
        exercise: String,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        json: bool,
    },
    Create {
        #[arg(long)]
        name: String,
        #[arg(long)]
        slug: Option<String>,
        #[arg(long, default_value = "strength")]
        modality: String,
        #[arg(long)]
        muscle: Option<String>,
        #[arg(long)]
        equipment: Option<String>,
        #[arg(long = "alias", value_name = "ALIAS")]
        aliases: Vec<String>,
        #[arg(long = "secondary", value_name = "MUSCLE")]
        secondary_muscles: Vec<String>,
        #[arg(long)]
        body: Option<String>,
        #[arg(long = "media-url")]
        media_url: Option<String>,
        #[arg(long)]
        organization: Option<String>,
    },
    Update {
        id: String,
        #[arg(long)]
        body: Option<String>,
        #[arg(long = "alias", value_name = "ALIAS")]
        aliases: Vec<String>,
        #[arg(long)]
        muscle: Option<String>,
        #[arg(long)]
        equipment: Option<String>,
        #[arg(long)]
        modality: Option<String>,
    },
    Delete {
        id: String,
    },
}

#[derive(Subcommand)]
pub(crate) enum RoutineCommands {
    List {
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        category: Option<String>,
        #[arg(long)]
        json: bool,
    },
    Show {
        routine: String,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        json: bool,
    },
    Create {
        #[arg(long)]
        name: String,
        #[arg(long)]
        slug: Option<String>,
        #[arg(long)]
        description: Option<String>,
        #[arg(long)]
        category: Option<String>,
        #[arg(long)]
        difficulty: Option<String>,
        #[arg(long = "tag", value_name = "TAG")]
        tags: Vec<String>,
        #[arg(long = "est-minutes")]
        est_minutes: Option<u32>,
        #[arg(long)]
        organization: Option<String>,
    },
    Delete {
        id: String,
    },
    AddExercise {
        routine: String,
        #[arg(long, conflicts_with = "custom")]
        exercise: Option<String>,
        #[arg(long, conflicts_with = "exercise")]
        custom: Option<String>,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        sets: Option<u32>,
        #[arg(long)]
        reps: Option<u32>,
        #[arg(long = "weight-kg")]
        weight_kg: Option<f64>,
        #[arg(long)]
        rest: Option<u32>,
        #[arg(long)]
        rpe: Option<f32>,
        #[arg(long)]
        tempo: Option<String>,
        #[arg(long)]
        duration: Option<u32>,
        #[arg(long = "distance-m")]
        distance_m: Option<f64>,
        #[arg(long = "avg-hr")]
        avg_hr: Option<u32>,
        #[arg(long = "pace-s-per-km")]
        pace_s_per_km: Option<u32>,
        #[arg(long)]
        group: Option<String>,
        #[arg(long)]
        notes: Option<String>,
    },
    RemoveExercise {
        routine_exercise_id: String,
    },
    Reorder {
        routine: String,
        #[arg(long)]
        organization: Option<String>,
        ordered_ids: Vec<String>,
    },
}

pub(crate) async fn run_remote_fitness_command(
    remote: &RemoteVoxConfig,
    actor: Option<&str>,
    command: FitnessCommands,
) -> eyre::Result<()> {
    let client = remote.fitness().await?;
    match command {
        FitnessCommands::Exercise { command } => run_exercise(&client, actor, command).await,
        FitnessCommands::Routine { command } => run_routine(&client, actor, command).await,
        FitnessCommands::Session { command } => run_session(&client, actor, command).await,
        FitnessCommands::Weigh {
            weight_kg,
            at,
            body_fat_percent,
            note,
            organization,
            json,
        } => {
            let measured_at = parse_iso_at(at.as_deref())?;
            let saved = client
                .record_body_measurement(RecordBodyMeasurementRequest {
                    measured_at,
                    weight_kg: Some(weight_kg),
                    body_fat_percent,
                    notes: note,
                    organization,
                    created_by: actor.map(str::to_string),
                    ..Default::default()
                })
                .await
                .map_err(|e| eyre::eyre!("record_body_measurement: {e}"))?;
            print_measurement(&saved, json)
        }
        FitnessCommands::Measure { command } => run_measure(remote, &client, actor, command).await,
    }
}

// ── Exercise ────────────────────────────────────────────────────────

async fn resolve_exercise_id(
    client: &FitnessServiceClient,
    organization: Option<String>,
    reference: &str,
) -> eyre::Result<Uuid> {
    if let Ok(id) = Uuid::parse_str(reference) {
        return Ok(id);
    }
    let hit = client
        .find_exercise_by_slug_or_alias(organization.clone(), reference.to_string())
        .await
        .map_err(|e| eyre::eyre!("find_exercise_by_slug_or_alias: {e}"))?;
    if let Some(ex) = hit {
        return Ok(ex.id);
    }
    // Fall back to personal-org
    let hit = client
        .find_exercise_by_slug_or_alias(Some("personal".to_string()), reference.to_string())
        .await
        .map_err(|e| eyre::eyre!("find_exercise_by_slug_or_alias: {e}"))?;
    hit.map(|ex| ex.id)
        .ok_or_else(|| eyre::eyre!("exercise not found: {reference}"))
}

async fn resolve_routine_id(
    client: &FitnessServiceClient,
    organization: Option<String>,
    reference: &str,
) -> eyre::Result<Uuid> {
    if let Ok(id) = Uuid::parse_str(reference) {
        return Ok(id);
    }
    // Walk organization-scoped list, then personal-org, then global.
    for org in [organization, Some("personal".to_string()), None] {
        let routines = client
            .list_routines(org.clone(), None)
            .await
            .map_err(|e| eyre::eyre!("list_routines: {e}"))?;
        let needle = reference.to_lowercase();
        if let Some(r) = routines
            .iter()
            .find(|r| r.slug.to_lowercase() == needle || r.name.to_lowercase() == needle)
        {
            return Ok(r.id);
        }
    }
    Err(eyre::eyre!("routine not found: {reference}"))
}

async fn run_exercise(
    client: &FitnessServiceClient,
    actor: Option<&str>,
    command: ExerciseCommands,
) -> eyre::Result<()> {
    match command {
        ExerciseCommands::List {
            organization,
            modality,
            muscle,
            json,
        } => {
            let rows = client
                .list_exercises(organization, modality, muscle)
                .await
                .map_err(|e| eyre::eyre!("list_exercises: {e}"))?;
            print_exercises(&rows, json)?;
        }
        ExerciseCommands::Show {
            exercise,
            organization,
            json,
        } => {
            let id = resolve_exercise_id(client, organization, &exercise).await?;
            let item = client
                .get_exercise(id)
                .await
                .map_err(|e| eyre::eyre!("get_exercise: {e}"))?
                .ok_or_else(|| eyre::eyre!("exercise not found: {exercise}"))?;
            if json {
                println!("{}", serde_json::to_string_pretty(&item)?);
            } else {
                println!("{}  {}", item.id, item.name);
                println!("  slug:        {}", item.slug);
                println!("  modality:    {}", item.modality.as_str());
                if let Some(muscle) = &item.primary_muscle {
                    println!("  primary:     {muscle}");
                }
                if !item.secondary_muscles.is_empty() {
                    let v: Vec<&String> = item.secondary_muscles.iter().collect();
                    println!("  secondary:   {v:?}");
                }
                if let Some(eq) = &item.equipment {
                    println!("  equipment:   {eq}");
                }
                if !item.aliases.is_empty() {
                    let v: Vec<&String> = item.aliases.iter().collect();
                    println!("  aliases:     {v:?}");
                }
                if let Some(org) = &item.organization {
                    println!("  organization:{org}");
                }
                if !item.body_markdown.is_empty() {
                    println!("\n{}", item.body_markdown);
                }
            }
        }
        ExerciseCommands::Create {
            name,
            slug,
            modality,
            muscle,
            equipment,
            aliases,
            secondary_muscles,
            body,
            media_url,
            organization,
        } => {
            let saved = client
                .create_exercise(CreateExerciseRequest {
                    name,
                    slug,
                    aliases,
                    modality,
                    primary_muscle: muscle,
                    secondary_muscles,
                    equipment,
                    body_markdown: body,
                    media_url,
                    organization,
                    created_by: actor.map(str::to_string),
                })
                .await
                .map_err(|e| eyre::eyre!("create_exercise: {e}"))?;
            println!("Created exercise '{}' (id={})", saved.name, saved.id);
        }
        ExerciseCommands::Update {
            id,
            body,
            aliases,
            muscle,
            equipment,
            modality,
        } => {
            let id = Uuid::parse_str(&id).map_err(|e| eyre::eyre!("invalid UUID: {e}"))?;
            let patch = ExercisePatch {
                aliases: if aliases.is_empty() {
                    None
                } else {
                    Some(aliases)
                },
                body_markdown: body,
                primary_muscle: muscle,
                equipment,
                modality,
                ..Default::default()
            };
            let saved = client
                .update_exercise(id, patch)
                .await
                .map_err(|e| eyre::eyre!("update_exercise: {e}"))?;
            println!("Updated exercise '{}' (id={})", saved.name, saved.id);
        }
        ExerciseCommands::Delete { id } => {
            let id = Uuid::parse_str(&id).map_err(|e| eyre::eyre!("invalid UUID: {e}"))?;
            client
                .delete_exercise(id)
                .await
                .map_err(|e| eyre::eyre!("delete_exercise: {e}"))?;
            println!("Deleted exercise {id}");
        }
    }
    Ok(())
}

fn print_exercises(rows: &[ExerciseApi], json: bool) -> eyre::Result<()> {
    if json {
        println!("{}", serde_json::to_string_pretty(rows)?);
        return Ok(());
    }
    if rows.is_empty() {
        println!("(no exercises)");
        return Ok(());
    }
    for r in rows {
        println!(
            "{}  {}  [{}]  {}",
            r.id,
            r.name,
            r.modality.as_str(),
            r.primary_muscle.clone().unwrap_or_else(|| "-".to_string())
        );
    }
    Ok(())
}

// ── Routine ─────────────────────────────────────────────────────────

async fn run_routine(
    client: &FitnessServiceClient,
    actor: Option<&str>,
    command: RoutineCommands,
) -> eyre::Result<()> {
    match command {
        RoutineCommands::List {
            organization,
            category,
            json,
        } => {
            let rows = client
                .list_routines(organization, category)
                .await
                .map_err(|e| eyre::eyre!("list_routines: {e}"))?;
            print_routines(&rows, json)?;
        }
        RoutineCommands::Show {
            routine,
            organization,
            json,
        } => {
            let id = resolve_routine_id(client, organization, &routine).await?;
            let view = client
                .get_routine_with_exercises(id)
                .await
                .map_err(|e| eyre::eyre!("get_routine_with_exercises: {e}"))?
                .ok_or_else(|| eyre::eyre!("routine not found: {routine}"))?;
            print_routine_view(&view, json)?;
        }
        RoutineCommands::Create {
            name,
            slug,
            description,
            category,
            difficulty,
            tags,
            est_minutes,
            organization,
        } => {
            let saved = client
                .create_routine(CreateRoutineRequest {
                    name,
                    slug,
                    description,
                    body_markdown: None,
                    category,
                    estimated_duration_minutes: est_minutes,
                    difficulty,
                    tags,
                    organization,
                    created_by: actor.map(str::to_string),
                })
                .await
                .map_err(|e| eyre::eyre!("create_routine: {e}"))?;
            println!("Created routine '{}' (id={})", saved.name, saved.id);
        }
        RoutineCommands::Delete { id } => {
            let id = Uuid::parse_str(&id).map_err(|e| eyre::eyre!("invalid UUID: {e}"))?;
            client
                .delete_routine(id)
                .await
                .map_err(|e| eyre::eyre!("delete_routine: {e}"))?;
            println!("Deleted routine {id}");
        }
        RoutineCommands::AddExercise {
            routine,
            exercise,
            custom,
            organization,
            sets,
            reps,
            weight_kg,
            rest,
            rpe,
            tempo,
            duration,
            distance_m,
            avg_hr,
            pace_s_per_km,
            group,
            notes,
        } => {
            let routine_id = resolve_routine_id(client, organization.clone(), &routine).await?;
            let exercise_id = match exercise.as_deref() {
                Some(reference) => {
                    Some(resolve_exercise_id(client, organization.clone(), reference).await?)
                }
                None => None,
            };
            if exercise_id.is_none() && custom.is_none() {
                return Err(eyre::eyre!(
                    "either --exercise or --custom must be provided"
                ));
            }
            let req = AddRoutineExerciseRequest {
                routine_id,
                exercise_id,
                display_name: custom,
                group_label: group,
                target_sets: sets,
                target_reps: reps,
                target_weight_kg: weight_kg,
                target_rest_seconds: rest,
                target_rpe: rpe,
                tempo,
                target_duration_seconds: duration,
                target_distance_meters: distance_m,
                target_avg_hr: avg_hr,
                target_pace_seconds_per_km: pace_s_per_km,
                notes,
            };
            let row = client
                .add_routine_exercise(req)
                .await
                .map_err(|e| eyre::eyre!("add_routine_exercise: {e}"))?;
            println!(
                "Added '{}' to routine at position {} (id={})",
                row.display_name, row.position, row.id
            );
        }
        RoutineCommands::RemoveExercise {
            routine_exercise_id,
        } => {
            let id = Uuid::parse_str(&routine_exercise_id)
                .map_err(|e| eyre::eyre!("invalid UUID: {e}"))?;
            client
                .remove_routine_exercise(id)
                .await
                .map_err(|e| eyre::eyre!("remove_routine_exercise: {e}"))?;
            println!("Removed routine_exercise {id}");
        }
        RoutineCommands::Reorder {
            routine,
            organization,
            ordered_ids,
        } => {
            let routine_id = resolve_routine_id(client, organization, &routine).await?;
            let parsed: Vec<Uuid> = ordered_ids
                .iter()
                .map(|s| Uuid::parse_str(s).map_err(|e| eyre::eyre!("invalid UUID '{s}': {e}")))
                .collect::<eyre::Result<_>>()?;
            client
                .reorder_routine_exercises(routine_id, parsed)
                .await
                .map_err(|e| eyre::eyre!("reorder_routine_exercises: {e}"))?;
            println!("Reordered routine {routine_id}");
        }
    }
    Ok(())
}

fn print_routines(rows: &[RoutineApi], json: bool) -> eyre::Result<()> {
    if json {
        println!("{}", serde_json::to_string_pretty(rows)?);
        return Ok(());
    }
    if rows.is_empty() {
        println!("(no routines)");
        return Ok(());
    }
    for r in rows {
        println!(
            "{}  {}  [{}]  {}min",
            r.id,
            r.name,
            r.category.clone().unwrap_or_else(|| "-".to_string()),
            r.estimated_duration_minutes
                .map(|m| m.to_string())
                .unwrap_or_else(|| "-".to_string())
        );
    }
    Ok(())
}

fn print_routine_view(view: &RoutineWithExercisesView, json: bool) -> eyre::Result<()> {
    if json {
        let exercises: Vec<RoutineExerciseApi> = serde_json::from_str(&view.exercises_json)
            .map_err(|e| eyre::eyre!("decode exercises_json: {e}"))?;
        let payload = serde_json::json!({
            "routine": view.routine,
            "exercises": exercises,
        });
        println!("{}", serde_json::to_string_pretty(&payload)?);
        return Ok(());
    }
    let r = &view.routine;
    println!("{}  {}", r.id, r.name);
    if let Some(desc) = &r.description {
        println!("  {desc}");
    }
    if let Some(cat) = &r.category {
        println!("  category:    {cat}");
    }
    if let Some(diff) = &r.difficulty {
        println!("  difficulty:  {diff}");
    }
    if let Some(min) = r.estimated_duration_minutes {
        println!("  est:         {min} min");
    }
    if !r.tags.is_empty() {
        let v: Vec<&String> = r.tags.iter().collect();
        println!("  tags:        {v:?}");
    }
    let exercises: Vec<RoutineExerciseApi> = serde_json::from_str(&view.exercises_json)
        .map_err(|e| eyre::eyre!("decode exercises_json: {e}"))?;
    println!();
    let mut last_group: Option<String> = None;
    for ex in &exercises {
        if ex.group_label != last_group {
            if let Some(label) = &ex.group_label {
                println!("[{label}]");
            }
            last_group = ex.group_label.clone();
        }
        let prefix = format!("{:>2}.", ex.position + 1);
        let summary = format_exercise_targets(ex);
        println!("  {prefix} {}  {summary}", ex.display_name);
        if let Some(notes) = &ex.notes {
            println!("        ({notes})");
        }
    }
    Ok(())
}

fn format_exercise_targets(ex: &RoutineExerciseApi) -> String {
    // Strength: "3 × 8 @ 135 kg, rest 90s, RPE 7-8"
    if let (Some(sets), Some(reps)) = (ex.target_sets, ex.target_reps) {
        let mut parts = vec![format!("{sets} × {reps}")];
        if let Some(w) = ex.target_weight_kg {
            parts.push(format!("@ {w} kg"));
        }
        if let Some(rest) = ex.target_rest_seconds {
            parts.push(format!("rest {rest}s"));
        }
        if let Some(rpe) = ex.target_rpe {
            parts.push(format!("RPE {rpe}"));
        }
        if let Some(tempo) = &ex.tempo {
            parts.push(format!("tempo {tempo}"));
        }
        return parts.join(", ");
    }
    if let Some(sets) = ex.target_sets {
        if let Some(dur) = ex.target_duration_seconds {
            return format!("{sets} × {}", format_duration(dur));
        }
    }
    // Cardio: "30:00, 5.0 km, avg HR 150"
    let mut parts = Vec::new();
    if let Some(dur) = ex.target_duration_seconds {
        parts.push(format_duration(dur));
    }
    if let Some(meters) = ex.target_distance_meters {
        if meters >= 1000.0 {
            parts.push(format!("{:.1} km", meters / 1000.0));
        } else {
            parts.push(format!("{meters} m"));
        }
    }
    if let Some(hr) = ex.target_avg_hr {
        parts.push(format!("avg HR {hr}"));
    }
    if let Some(pace) = ex.target_pace_seconds_per_km {
        parts.push(format!("pace {}/km", format_duration(pace)));
    }
    if parts.is_empty() {
        "—".to_string()
    } else {
        parts.join(", ")
    }
}

fn format_duration(seconds: u32) -> String {
    let m = seconds / 60;
    let s = seconds % 60;
    format!("{m}:{s:02}")
}

// ── Session ─────────────────────────────────────────────────────────

#[derive(Subcommand)]
pub(crate) enum SessionCommands {
    /// Begin a workout session. With `--routine`, the session pre-populates
    /// planned sets matching the routine's targets so they're ready to check
    /// off. Without it, the session starts empty for ad-hoc lifting.
    Start {
        #[arg(long)]
        routine: Option<String>,
        #[arg(long)]
        label: Option<String>,
        #[arg(long = "bodyweight-kg")]
        bodyweight_kg: Option<f64>,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Render a session as a checkbox list. Pass "active" for the most-
    /// recent active session in your org, a UUID, or a fragment of the
    /// routine name to fuzzy-match.
    Show {
        session: String,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// List recent workout sessions.
    List {
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        status: Option<String>,
        #[arg(long)]
        limit: Option<u32>,
        #[arg(long)]
        json: bool,
    },
    /// Append a new set to a session. By default the set is marked done
    /// immediately; pass `--defer` to add it as a planned (unchecked) row.
    Log {
        session: String,
        #[arg(long, conflicts_with = "custom")]
        exercise: Option<String>,
        #[arg(long, conflicts_with = "exercise")]
        custom: Option<String>,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        reps: Option<u32>,
        #[arg(long = "weight-kg")]
        weight_kg: Option<f64>,
        #[arg(long)]
        duration: Option<u32>,
        #[arg(long = "distance-m")]
        distance_m: Option<f64>,
        #[arg(long = "avg-hr")]
        avg_hr: Option<u32>,
        #[arg(long = "pace-s-per-km")]
        pace_s_per_km: Option<u32>,
        #[arg(long)]
        rpe: Option<f32>,
        #[arg(long)]
        notes: Option<String>,
        #[arg(long)]
        defer: bool,
    },
    /// Tick a set as done.
    Check { set_log_id: String },
    /// Untick a previously-checked set.
    Uncheck { set_log_id: String },
    /// Update fields on an already-logged set.
    SetUpdate {
        set_log_id: String,
        #[arg(long)]
        reps: Option<u32>,
        #[arg(long = "weight-kg")]
        weight_kg: Option<f64>,
        #[arg(long)]
        duration: Option<u32>,
        #[arg(long = "distance-m")]
        distance_m: Option<f64>,
        #[arg(long = "avg-hr")]
        avg_hr: Option<u32>,
        #[arg(long = "pace-s-per-km")]
        pace_s_per_km: Option<u32>,
        #[arg(long)]
        rpe: Option<f32>,
        #[arg(long)]
        notes: Option<String>,
    },
    /// Delete a set row entirely.
    SetDelete { set_log_id: String },
    /// Mark a session complete.
    Complete {
        session: String,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        rpe: Option<f32>,
        #[arg(long)]
        notes: Option<String>,
        /// Skip the "all sets must be done" check.
        #[arg(long = "allow-incomplete")]
        allow_incomplete: bool,
    },
    /// Mark a session abandoned.
    Abandon {
        session: String,
        #[arg(long)]
        organization: Option<String>,
    },
}

async fn resolve_session_id(
    client: &FitnessServiceClient,
    organization: Option<String>,
    reference: &str,
) -> eyre::Result<Uuid> {
    if let Ok(id) = Uuid::parse_str(reference) {
        return Ok(id);
    }
    let lower = reference.trim().to_lowercase();
    if lower == "active" {
        let candidates = client
            .list_workout_sessions(organization.clone(), Some("active".to_string()), Some(1))
            .await
            .map_err(|e| eyre::eyre!("list_workout_sessions: {e}"))?;
        return candidates
            .into_iter()
            .next()
            .map(|s| s.id)
            .ok_or_else(|| eyre::eyre!("no active workout session found"));
    }
    let candidates = client
        .list_workout_sessions(organization.clone(), None, Some(50))
        .await
        .map_err(|e| eyre::eyre!("list_workout_sessions: {e}"))?;
    candidates
        .into_iter()
        .find(|s| s.routine_name_snapshot.to_lowercase().contains(&lower))
        .map(|s| s.id)
        .ok_or_else(|| eyre::eyre!("workout session not found: {reference}"))
}

async fn run_session(
    client: &FitnessServiceClient,
    actor: Option<&str>,
    command: SessionCommands,
) -> eyre::Result<()> {
    match command {
        SessionCommands::Start {
            routine,
            label,
            bodyweight_kg,
            organization,
            json,
        } => {
            let routine_id = match routine {
                Some(r) => Some(resolve_routine_id(client, organization.clone(), &r).await?),
                None => None,
            };
            let view = client
                .start_workout_session(StartWorkoutSessionRequest {
                    routine_id,
                    label,
                    bodyweight_kg,
                    organization,
                    created_by: actor.map(str::to_string),
                })
                .await
                .map_err(|e| eyre::eyre!("start_workout_session: {e}"))?;
            print_session(&view, json)?;
        }
        SessionCommands::Show {
            session,
            organization,
            json,
        } => {
            let id = resolve_session_id(client, organization, &session).await?;
            let view = client
                .get_workout_session(id)
                .await
                .map_err(|e| eyre::eyre!("get_workout_session: {e}"))?
                .ok_or_else(|| eyre::eyre!("workout session not found: {session}"))?;
            print_session(&view, json)?;
        }
        SessionCommands::List {
            organization,
            status,
            limit,
            json,
        } => {
            let rows = client
                .list_workout_sessions(organization, status, limit)
                .await
                .map_err(|e| eyre::eyre!("list_workout_sessions: {e}"))?;
            if json {
                println!("{}", serde_json::to_string_pretty(&rows)?);
            } else if rows.is_empty() {
                println!("(no sessions)");
            } else {
                for r in rows {
                    let when = r.started_at.format("%Y-%m-%d %H:%M");
                    println!(
                        "{}  [{}]  {when}  {}",
                        r.id,
                        r.status.as_str(),
                        r.routine_name_snapshot
                    );
                }
            }
        }
        SessionCommands::Log {
            session,
            exercise,
            custom,
            organization,
            reps,
            weight_kg,
            duration,
            distance_m,
            avg_hr,
            pace_s_per_km,
            rpe,
            notes,
            defer,
        } => {
            let session_id = resolve_session_id(client, organization.clone(), &session).await?;
            let exercise_id = match exercise.as_deref() {
                Some(reference) => {
                    Some(resolve_exercise_id(client, organization.clone(), reference).await?)
                }
                None => None,
            };
            if exercise_id.is_none() && custom.is_none() {
                return Err(eyre::eyre!(
                    "either --exercise or --custom must be provided"
                ));
            }
            let row = client
                .log_set(LogSetRequest {
                    workout_session_id: session_id,
                    exercise_id,
                    display_name: custom,
                    routine_exercise_id: None,
                    reps,
                    weight_kg,
                    duration_seconds: duration,
                    distance_meters: distance_m,
                    avg_hr,
                    pace_seconds_per_km: pace_s_per_km,
                    rpe,
                    notes,
                    defer,
                })
                .await
                .map_err(|e| eyre::eyre!("log_set: {e}"))?;
            println!(
                "Logged '{}' set #{} (id={})",
                row.exercise_name_snapshot,
                row.set_index + 1,
                row.id
            );
        }
        SessionCommands::Check { set_log_id } => {
            let id = Uuid::parse_str(&set_log_id).map_err(|e| eyre::eyre!("invalid UUID: {e}"))?;
            let row = client
                .mark_set_done(id, true)
                .await
                .map_err(|e| eyre::eyre!("mark_set_done: {e}"))?;
            println!(
                "[x] {} set #{}",
                row.exercise_name_snapshot,
                row.set_index + 1
            );
        }
        SessionCommands::Uncheck { set_log_id } => {
            let id = Uuid::parse_str(&set_log_id).map_err(|e| eyre::eyre!("invalid UUID: {e}"))?;
            let row = client
                .mark_set_done(id, false)
                .await
                .map_err(|e| eyre::eyre!("mark_set_done: {e}"))?;
            println!(
                "[ ] {} set #{}",
                row.exercise_name_snapshot,
                row.set_index + 1
            );
        }
        SessionCommands::SetUpdate {
            set_log_id,
            reps,
            weight_kg,
            duration,
            distance_m,
            avg_hr,
            pace_s_per_km,
            rpe,
            notes,
        } => {
            let id = Uuid::parse_str(&set_log_id).map_err(|e| eyre::eyre!("invalid UUID: {e}"))?;
            let row = client
                .update_set(UpdateSetRequest {
                    set_log_id: id,
                    reps,
                    weight_kg,
                    duration_seconds: duration,
                    distance_meters: distance_m,
                    avg_hr,
                    pace_seconds_per_km: pace_s_per_km,
                    rpe,
                    notes,
                })
                .await
                .map_err(|e| eyre::eyre!("update_set: {e}"))?;
            println!("Updated set #{} ({})", row.set_index + 1, row.id);
        }
        SessionCommands::SetDelete { set_log_id } => {
            let id = Uuid::parse_str(&set_log_id).map_err(|e| eyre::eyre!("invalid UUID: {e}"))?;
            client
                .delete_set(id)
                .await
                .map_err(|e| eyre::eyre!("delete_set: {e}"))?;
            println!("Deleted set {id}");
        }
        SessionCommands::Complete {
            session,
            organization,
            rpe,
            notes,
            allow_incomplete,
        } => {
            let id = resolve_session_id(client, organization, &session).await?;
            let saved = client
                .complete_workout_session(CompleteWorkoutSessionRequest {
                    id,
                    overall_rpe: rpe,
                    notes,
                    require_all_sets_done: !allow_incomplete,
                })
                .await
                .map_err(|e| eyre::eyre!("complete_workout_session: {e}"))?;
            println!(
                "Completed '{}' (id={})",
                saved.routine_name_snapshot, saved.id
            );
        }
        SessionCommands::Abandon {
            session,
            organization,
        } => {
            let id = resolve_session_id(client, organization, &session).await?;
            let saved = client
                .abandon_workout_session(id)
                .await
                .map_err(|e| eyre::eyre!("abandon_workout_session: {e}"))?;
            println!(
                "Abandoned '{}' (id={})",
                saved.routine_name_snapshot, saved.id
            );
        }
    }
    Ok(())
}

fn print_session(view: &WorkoutSessionView, json: bool) -> eyre::Result<()> {
    if json {
        // WorkoutSessionView is facet::Facet but not serde::Serialize;
        // assemble JSON by hand so callers can pipe / consume it.
        let sets: serde_json::Value =
            serde_json::from_str(&view.sets_json).unwrap_or(serde_json::Value::Array(Vec::new()));
        let payload = serde_json::json!({
            "session": view.session,
            "sets": sets,
            "sets_done": view.sets_done,
            "sets_total": view.sets_total,
            "total_volume_kg": view.total_volume_kg,
            "total_cardio_seconds": view.total_cardio_seconds,
        });
        println!("{}", serde_json::to_string_pretty(&payload)?);
        return Ok(());
    }
    let s = &view.session;
    let started = s.started_at.format("%H:%M");
    println!(
        "{}  ·  started {started}  ·  {}/{} sets done  ·  [{}]",
        s.routine_name_snapshot,
        view.sets_done,
        view.sets_total,
        s.status.as_str()
    );
    if view.total_volume_kg > 0.0 {
        println!("  volume: {:.1} kg", view.total_volume_kg);
    }
    if view.total_cardio_seconds > 0 {
        println!("  cardio: {}", format_duration(view.total_cardio_seconds));
    }
    if let Some(bw) = s.bodyweight_kg {
        println!("  bodyweight: {bw} kg");
    }

    let sets: Vec<SetLogApi> =
        serde_json::from_str(&view.sets_json).map_err(|e| eyre::eyre!("decode sets_json: {e}"))?;
    if sets.is_empty() {
        println!("\n  (no sets logged yet)");
        return Ok(());
    }

    // Group by exercise_name_snapshot, preserving first-occurrence order.
    let mut order: Vec<String> = Vec::new();
    let mut groups: std::collections::HashMap<String, Vec<SetLogApi>> =
        std::collections::HashMap::new();
    for set in sets {
        let key = set.exercise_name_snapshot.clone();
        if !groups.contains_key(&key) {
            order.push(key.clone());
        }
        groups.entry(key).or_default().push(set);
    }

    for name in &order {
        let group = groups.get(name).expect("group present");
        println!("\n  {name}");
        for set in group {
            let mark = if set.completed_at.is_some() {
                "[x]"
            } else {
                "[ ]"
            };
            let detail = format_set_detail(set);
            let id_suffix = if set.completed_at.is_none() {
                format!("    {}", set.id)
            } else {
                String::new()
            };
            println!("    {mark} {}   {detail}{id_suffix}", set.set_index + 1);
        }
    }
    if !s.notes.is_empty() {
        println!("\n  notes: {}", s.notes);
    }
    Ok(())
}

fn format_set_detail(set: &SetLogApi) -> String {
    let mut parts: Vec<String> = Vec::new();
    if set.reps.is_some() || set.weight_kg.is_some() {
        let reps = set
            .reps
            .map(|r| r.to_string())
            .unwrap_or_else(|| "_".to_string());
        let weight = set
            .weight_kg
            .map(|w| format!("{w} kg"))
            .unwrap_or_else(|| "_ kg".to_string());
        parts.push(format!("{reps} reps @ {weight}"));
    }
    if let Some(dur) = set.duration_seconds {
        parts.push(format_duration(dur));
    }
    if let Some(dist) = set.distance_meters {
        if dist >= 1000.0 {
            parts.push(format!("{:.2} km", dist / 1000.0));
        } else {
            parts.push(format!("{dist} m"));
        }
    }
    if let Some(hr) = set.avg_hr {
        parts.push(format!("HR {hr}"));
    }
    if let Some(pace) = set.pace_seconds_per_km {
        parts.push(format!("pace {}/km", format_duration(pace)));
    }
    if let Some(rpe) = set.rpe {
        parts.push(format!("RPE {rpe}"));
    }
    if let Some(notes) = &set.notes {
        if !notes.is_empty() {
            parts.push(format!("({notes})"));
        }
    }
    if parts.is_empty() {
        "—".to_string()
    } else {
        parts.join("   ")
    }
}

// ── Measure (body measurements) ─────────────────────────────────────

#[derive(Subcommand)]
pub(crate) enum MeasureCommands {
    /// Record a new body-measurement row. All fields are optional except
    /// the timestamp (defaults to now) — record only what you measured.
    Record {
        #[arg(long = "at")]
        at: Option<String>,
        #[arg(long = "weight-kg")]
        weight_kg: Option<f64>,
        #[arg(long = "bf")]
        body_fat_percent: Option<f32>,
        #[arg(long = "muscle-kg")]
        muscle_mass_kg: Option<f64>,
        #[arg(long = "water-pct")]
        water_percent: Option<f32>,
        #[arg(long)]
        neck: Option<f64>,
        #[arg(long)]
        chest: Option<f64>,
        #[arg(long)]
        waist: Option<f64>,
        #[arg(long)]
        hip: Option<f64>,
        #[arg(long = "left-thigh")]
        left_thigh: Option<f64>,
        #[arg(long = "right-thigh")]
        right_thigh: Option<f64>,
        #[arg(long = "left-arm")]
        left_arm: Option<f64>,
        #[arg(long = "right-arm")]
        right_arm: Option<f64>,
        #[arg(long = "left-calf")]
        left_calf: Option<f64>,
        #[arg(long = "right-calf")]
        right_calf: Option<f64>,
        #[arg(long = "resting-hr")]
        resting_hr: Option<u32>,
        /// Blood pressure as `<systolic>/<diastolic>` (e.g. `120/80`).
        #[arg(long = "bp")]
        bp: Option<String>,
        #[arg(long = "note")]
        note: Option<String>,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// List recent measurements (newest first).
    List {
        #[arg(long)]
        since: Option<String>,
        #[arg(long)]
        until: Option<String>,
        #[arg(long)]
        limit: Option<u32>,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Show one measurement by id.
    Show {
        id: String,
        #[arg(long)]
        json: bool,
    },
    /// Patch fields on an existing measurement (None fields = leave alone).
    Update {
        id: String,
        #[arg(long = "weight-kg")]
        weight_kg: Option<f64>,
        #[arg(long = "bf")]
        body_fat_percent: Option<f32>,
        #[arg(long)]
        waist: Option<f64>,
        #[arg(long)]
        chest: Option<f64>,
        #[arg(long)]
        hip: Option<f64>,
        #[arg(long = "note")]
        note: Option<String>,
    },
    /// Delete a measurement (drops attached photos as well).
    Delete { id: String },
    /// Compute trend metrics across a date window.
    Trend {
        #[arg(long)]
        since: Option<String>,
        #[arg(long)]
        until: Option<String>,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Photo subcommands — delegate to the attachment service with
    /// `owner_type = "body_measurement"`.
    Photo {
        #[command(subcommand)]
        command: PhotoCommands,
    },
}

#[derive(Subcommand)]
pub(crate) enum PhotoCommands {
    /// Upload a progress photo and attach it to a measurement row.
    Attach {
        id: String,
        path: PathBuf,
        #[arg(long)]
        label: Option<String>,
    },
    /// List photos hung off a measurement.
    List { id: String },
}

fn parse_iso_at(value: Option<&str>) -> eyre::Result<Option<chrono::DateTime<chrono::Utc>>> {
    let Some(raw) = value else {
        return Ok(None);
    };
    let parsed = chrono::DateTime::parse_from_rfc3339(raw)
        .map_err(|err| eyre::eyre!("invalid --at timestamp '{raw}': {err}"))?;
    Ok(Some(parsed.with_timezone(&chrono::Utc)))
}

fn parse_bp(raw: &str) -> eyre::Result<(u32, u32)> {
    let trimmed = raw.trim();
    let mut parts = trimmed.splitn(2, '/');
    let sys = parts
        .next()
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .ok_or_else(|| eyre::eyre!("--bp must be `<systolic>/<diastolic>` (got `{raw}`)"))?;
    let dia = parts
        .next()
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .ok_or_else(|| eyre::eyre!("--bp must be `<systolic>/<diastolic>` (got `{raw}`)"))?;
    let sys: u32 = sys
        .parse()
        .map_err(|err| eyre::eyre!("--bp systolic must be a non-negative integer: {err}"))?;
    let dia: u32 = dia
        .parse()
        .map_err(|err| eyre::eyre!("--bp diastolic must be a non-negative integer: {err}"))?;
    Ok((sys, dia))
}

async fn run_measure(
    remote: &RemoteVoxConfig,
    client: &FitnessServiceClient,
    actor: Option<&str>,
    command: MeasureCommands,
) -> eyre::Result<()> {
    match command {
        MeasureCommands::Record {
            at,
            weight_kg,
            body_fat_percent,
            muscle_mass_kg,
            water_percent,
            neck,
            chest,
            waist,
            hip,
            left_thigh,
            right_thigh,
            left_arm,
            right_arm,
            left_calf,
            right_calf,
            resting_hr,
            bp,
            note,
            organization,
            json,
        } => {
            let measured_at = parse_iso_at(at.as_deref())?;
            let (sys, dia) = match bp.as_deref() {
                Some(raw) => {
                    let (s, d) = parse_bp(raw)?;
                    (Some(s), Some(d))
                }
                None => (None, None),
            };
            let saved = client
                .record_body_measurement(RecordBodyMeasurementRequest {
                    measured_at,
                    weight_kg,
                    body_fat_percent,
                    muscle_mass_kg,
                    water_percent,
                    neck_cm: neck,
                    chest_cm: chest,
                    waist_cm: waist,
                    hip_cm: hip,
                    left_thigh_cm: left_thigh,
                    right_thigh_cm: right_thigh,
                    left_arm_cm: left_arm,
                    right_arm_cm: right_arm,
                    left_calf_cm: left_calf,
                    right_calf_cm: right_calf,
                    resting_hr,
                    blood_pressure_systolic: sys,
                    blood_pressure_diastolic: dia,
                    notes: note,
                    organization,
                    created_by: actor.map(str::to_string),
                })
                .await
                .map_err(|e| eyre::eyre!("record_body_measurement: {e}"))?;
            print_measurement(&saved, json)?;
        }
        MeasureCommands::List {
            since,
            until,
            limit,
            organization,
            json,
        } => {
            let rows = client
                .list_body_measurements(ListBodyMeasurementsRequest {
                    organization,
                    since: parse_iso_at(since.as_deref())?,
                    until: parse_iso_at(until.as_deref())?,
                    limit,
                })
                .await
                .map_err(|e| eyre::eyre!("list_body_measurements: {e}"))?;
            print_measurements(&rows, json)?;
        }
        MeasureCommands::Show { id, json } => {
            let parsed = Uuid::parse_str(&id).map_err(|e| eyre::eyre!("invalid UUID: {e}"))?;
            let item = client
                .get_body_measurement(parsed)
                .await
                .map_err(|e| eyre::eyre!("get_body_measurement: {e}"))?
                .ok_or_else(|| eyre::eyre!("body_measurement not found: {id}"))?;
            print_measurement(&item, json)?;
        }
        MeasureCommands::Update {
            id,
            weight_kg,
            body_fat_percent,
            waist,
            chest,
            hip,
            note,
        } => {
            let parsed = Uuid::parse_str(&id).map_err(|e| eyre::eyre!("invalid UUID: {e}"))?;
            let saved = client
                .update_body_measurement(UpdateBodyMeasurementRequest {
                    id: parsed,
                    weight_kg,
                    body_fat_percent,
                    waist_cm: waist,
                    chest_cm: chest,
                    hip_cm: hip,
                    notes: note,
                    ..Default::default()
                })
                .await
                .map_err(|e| eyre::eyre!("update_body_measurement: {e}"))?;
            println!(
                "Updated measurement {} (measured_at {})",
                saved.id, saved.measured_at
            );
        }
        MeasureCommands::Delete { id } => {
            let parsed = Uuid::parse_str(&id).map_err(|e| eyre::eyre!("invalid UUID: {e}"))?;
            client
                .delete_body_measurement(parsed)
                .await
                .map_err(|e| eyre::eyre!("delete_body_measurement: {e}"))?;
            println!("Deleted measurement {parsed} (and its photo attachments).");
        }
        MeasureCommands::Trend {
            since,
            until,
            organization,
            json,
        } => {
            let view = client
                .body_measurement_trend(BodyMeasurementTrendRequest {
                    organization,
                    since: parse_iso_at(since.as_deref())?,
                    until: parse_iso_at(until.as_deref())?,
                })
                .await
                .map_err(|e| eyre::eyre!("body_measurement_trend: {e}"))?;
            print_trend(&view, json)?;
        }
        MeasureCommands::Photo { command } => match command {
            PhotoCommands::Attach { id, path, label } => {
                let parsed = Uuid::parse_str(&id).map_err(|e| eyre::eyre!("invalid UUID: {e}"))?;
                let attachments = remote.attachment().await?;
                let bytes = std::fs::read(&path)
                    .map_err(|err| eyre::eyre!("read {}: {err}", path.display()))?;
                let basename = path
                    .file_name()
                    .and_then(|s| s.to_str())
                    .map(str::to_string)
                    .unwrap_or_else(|| "photo.bin".to_string());
                let remote_path = task_core::attachment::default_remote_path(
                    "body_measurement",
                    parsed,
                    &basename,
                );
                let attachment = attachments
                    .upload(task_core::service::AttachmentUploadRequest {
                        owner_type: "body_measurement".to_string(),
                        owner_id: parsed,
                        path: remote_path,
                        label,
                        mime: None,
                        bytes,
                        uploader: actor.map(str::to_string),
                        source: "nextcloud".to_string(),
                    })
                    .await
                    .map_err(|err| eyre::eyre!("upload failed: {err}"))?;
                println!(
                    "Attached {} -> {}",
                    attachment.label.as_deref().unwrap_or("(no label)"),
                    attachment.path
                );
            }
            PhotoCommands::List { id } => {
                let parsed = Uuid::parse_str(&id).map_err(|e| eyre::eyre!("invalid UUID: {e}"))?;
                let attachments = remote.attachment().await?;
                let rows = attachments
                    .list_for_entity("body_measurement".to_string(), parsed)
                    .await
                    .map_err(|err| eyre::eyre!("list failed: {err}"))?;
                if rows.is_empty() {
                    println!("(no photos)");
                } else {
                    for a in rows {
                        let mime = a.mime.as_deref().unwrap_or("?");
                        let label = a.label.as_deref().unwrap_or("(no label)");
                        println!("{}  {label}  [{mime}]  {}", a.id, a.path);
                    }
                }
            }
        },
    }
    Ok(())
}

fn print_measurement(item: &BodyMeasurementApi, json: bool) -> eyre::Result<()> {
    if json {
        println!("{}", serde_json::to_string_pretty(item)?);
        return Ok(());
    }
    let when = item.measured_at.format("%Y-%m-%d %H:%M");
    println!("{}  {when}", item.id);
    if let Some(v) = item.weight_kg {
        println!("  weight:        {v} kg");
    }
    if let Some(v) = item.body_fat_percent {
        println!("  body fat:      {v}%");
    }
    if let Some(v) = item.muscle_mass_kg {
        println!("  muscle mass:   {v} kg");
    }
    if let Some(v) = item.water_percent {
        println!("  water:         {v}%");
    }
    let circ = [
        ("neck", item.neck_cm),
        ("chest", item.chest_cm),
        ("waist", item.waist_cm),
        ("hip", item.hip_cm),
        ("left thigh", item.left_thigh_cm),
        ("right thigh", item.right_thigh_cm),
        ("left arm", item.left_arm_cm),
        ("right arm", item.right_arm_cm),
        ("left calf", item.left_calf_cm),
        ("right calf", item.right_calf_cm),
    ];
    for (label, v) in circ {
        if let Some(v) = v {
            println!("  {label:<13}{v} cm");
        }
    }
    if let Some(v) = item.resting_hr {
        println!("  resting HR:    {v} bpm");
    }
    if let (Some(s), Some(d)) = (item.blood_pressure_systolic, item.blood_pressure_diastolic) {
        println!("  blood pressure {s}/{d}");
    }
    if !item.notes.is_empty() {
        println!("  notes:         {}", item.notes);
    }
    if let Some(org) = &item.organization {
        println!("  organization:  {org}");
    }
    Ok(())
}

fn print_measurements(rows: &[BodyMeasurementApi], json: bool) -> eyre::Result<()> {
    if json {
        println!("{}", serde_json::to_string_pretty(rows)?);
        return Ok(());
    }
    if rows.is_empty() {
        println!("(no measurements)");
        return Ok(());
    }
    for r in rows {
        let when = r.measured_at.format("%Y-%m-%d %H:%M");
        let weight = r
            .weight_kg
            .map(|v| format!("{v:.1} kg"))
            .unwrap_or_else(|| "—".to_string());
        let bf = r
            .body_fat_percent
            .map(|v| format!("{v:.1}%"))
            .unwrap_or_else(|| "—".to_string());
        let waist = r
            .waist_cm
            .map(|v| format!("{v:.1} cm"))
            .unwrap_or_else(|| "—".to_string());
        println!("{}  {when}  weight={weight}  bf={bf}  waist={waist}", r.id);
    }
    Ok(())
}

fn print_trend(view: &BodyMeasurementTrendView, json: bool) -> eyre::Result<()> {
    if json {
        let payload = serde_json::json!({
            "measurement_count": view.measurement_count,
            "since": view.since,
            "until": view.until,
            "weight_kg": metric_json(view.weight_kg.as_ref()),
            "body_fat_percent": metric_json(view.body_fat_percent.as_ref()),
            "muscle_mass_kg": metric_json(view.muscle_mass_kg.as_ref()),
            "waist_cm": metric_json(view.waist_cm.as_ref()),
            "chest_cm": metric_json(view.chest_cm.as_ref()),
            "hip_cm": metric_json(view.hip_cm.as_ref()),
        });
        println!("{}", serde_json::to_string_pretty(&payload)?);
        return Ok(());
    }
    if view.measurement_count == 0 {
        println!("No measurements in range.");
        return Ok(());
    }
    let since = view.since.format("%Y-%m-%d");
    let until = view.until.format("%Y-%m-%d");
    println!(
        "Body trend  ·  {since} -> {until}  ·  {} measurements\n",
        view.measurement_count
    );
    let metrics: [(&str, Option<&MetricTrend>); 6] = [
        ("weight_kg", view.weight_kg.as_ref()),
        ("body_fat_pct", view.body_fat_percent.as_ref()),
        ("muscle_mass_kg", view.muscle_mass_kg.as_ref()),
        ("waist_cm", view.waist_cm.as_ref()),
        ("chest_cm", view.chest_cm.as_ref()),
        ("hip_cm", view.hip_cm.as_ref()),
    ];
    for (label, m) in metrics {
        if let Some(m) = m {
            println!(
                "  {label:<14}{first:.1} -> {last:.1}   Δ {delta:+.1}  ({pct:+.1}%)   range {min:.1}–{max:.1}   n={n}",
                first = m.first_value,
                last = m.last_value,
                delta = m.delta,
                pct = m.delta_percent,
                min = m.min_value,
                max = m.max_value,
                n = m.sample_count,
            );
        }
    }
    Ok(())
}

fn metric_json(m: Option<&MetricTrend>) -> serde_json::Value {
    match m {
        None => serde_json::Value::Null,
        Some(m) => serde_json::json!({
            "sample_count": m.sample_count,
            "first_value": m.first_value,
            "last_value": m.last_value,
            "min_value": m.min_value,
            "max_value": m.max_value,
            "mean_value": m.mean_value,
            "delta": m.delta,
            "delta_percent": m.delta_percent,
        }),
    }
}
