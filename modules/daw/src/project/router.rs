use crate::project::Project;
use crate::transport::{TransportActions, create_transport_http_router};
use axum::{Router, extract::State, http::StatusCode, response::Json, routing::get};
use std::sync::Arc;
use tokio::sync::Mutex;

/// Generic HTTP project router that composes domain routers (transport, audio, midi, etc.)
/// This router expects to receive individual project state, not a provider
pub fn create_project_http_router<T>() -> Router<Arc<Mutex<Project<T>>>>
where
    T: TransportActions + Send + Sync + 'static,
{
    Router::new()
        .route("/info", get(get_project_info))
        .nest("/transport", create_transport_http_router::<Project<T>>())
    // Add more domain routers here:
    // .nest("/audio", create_audio_router::<P>())
    // .nest("/midi", create_midi_router::<P>())
}

/// Get basic project information
async fn get_project_info<T>(
    State(project): State<Arc<Mutex<Project<T>>>>,
) -> Result<Json<ProjectInfo>, StatusCode> {
    let project = project.lock().await;

    Ok(Json(ProjectInfo {
        name: project.name().to_string(),
    }))
}

#[derive(serde::Serialize)]
struct ProjectInfo {
    name: String,
}
