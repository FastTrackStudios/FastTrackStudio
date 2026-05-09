//! `task fit` / `task fitness` — fitness workflow commands.
//!
//! Catalog half of the fitness workflow: Exercise CRUD + Routine
//! templates with positioned exercises. Logged workouts come in a
//! follow-up bead.

use clap::Subcommand;
use task_core::exercise::ExerciseApi;
use task_core::routine::RoutineApi;
use task_core::routine_exercise::RoutineExerciseApi;
use task_core::service::{
    AddRoutineExerciseRequest, CreateExerciseRequest, CreateRoutineRequest, ExercisePatch,
    FitnessServiceClient, RoutineWithExercisesView,
};
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
