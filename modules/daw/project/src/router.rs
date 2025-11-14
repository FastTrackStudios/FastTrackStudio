use crate::project::ProjectActions;
use axum::{Router, extract::State, http::StatusCode, response::Json, routing::get};
use std::sync::Arc;
use tokio::sync::Mutex;
use transport::{TransportActions, create_transport_http_router};

/// Generic HTTP project router that composes domain routers (transport, audio, midi, etc.)
/// This router expects to receive individual project state, not a provider
pub fn create_project_http_router<P>() -> Router<Arc<Mutex<P>>>
where
    P: ProjectActions + Send + Sync + 'static,
    P: TransportActions + Send + Sync + 'static,
{
    Router::new()
        .route("/info", get(get_project_info))
        .nest("/transport", create_transport_http_router::<P>())
    // Add more domain routers here:
    // .nest("/audio", create_audio_router::<P>())
    // .nest("/midi", create_midi_router::<P>())
}

/// Get basic project information
async fn get_project_info<P>(
    State(project): State<Arc<Mutex<P>>>,
) -> Result<Json<ProjectInfo>, StatusCode>
where
    P: ProjectActions + Send + Sync,
{
    let project = project.lock().await;

    Ok(Json(ProjectInfo {
        name: project.get_name().to_string(),
    }))
}

#[derive(serde::Serialize)]
struct ProjectInfo {
    name: String,
}
