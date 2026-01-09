use crate::primitives::TimeSignature;
use crate::transport::core::{RecordMode, Tempo, TransportActions};
use axum::{
    Router,
    extract::State,
    http::StatusCode,
    response::Json,
    routing::{get, post},
};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio::sync::Mutex;

// Request/Response types for HTTP endpoints
#[derive(Debug, Deserialize)]
pub struct SetTempoRequest {
    pub bpm: f64,
}

#[derive(Debug, Deserialize)]
pub struct SetTimeSignatureRequest {
    pub numerator: i32,
    pub denominator: i32,
}

#[derive(Debug, Deserialize)]
pub struct SetPositionRequest {
    pub seconds: f64,
}

#[derive(Debug, Deserialize)]
pub struct SetRecordModeRequest {
    pub mode: String,
}

#[derive(Debug, Serialize)]
pub struct TransportStatusResponse {
    pub is_playing: bool,
    pub is_recording: bool,
    pub tempo_bpm: f64,
    pub position_seconds: f64,
    pub time_signature: TimeSignatureResponse,
    pub record_mode: String,
}

#[derive(Debug, Serialize)]
pub struct TimeSignatureResponse {
    pub numerator: i32,
    pub denominator: i32,
}

#[derive(Debug, Serialize)]
pub struct TempoResponse {
    pub bpm: f64,
}

#[derive(Debug, Serialize)]
pub struct PositionResponse {
    pub seconds: f64,
}

#[derive(Debug, Serialize)]
pub struct ActionResponse {
    pub success: bool,
    pub message: String,
}

// Generic HTTP router that works with anything implementing TransportActions
pub fn create_transport_http_router<T>() -> Router<Arc<Mutex<T>>>
where
    T: TransportActions + Send + Sync + 'static,
{
    Router::new()
        // Playback control endpoints
        .route("/play", post(play_handler))
        .route("/pause", post(pause_handler))
        .route("/stop", post(stop_handler))
        .route("/play_pause", post(play_pause_handler))
        .route("/play_stop", post(play_stop_handler))
        // Recording control endpoints
        .route("/start_recording", post(start_recording_handler))
        .route("/stop_recording", post(stop_recording_handler))
        .route("/toggle_recording", post(toggle_recording_handler))
        // Configuration endpoints
        .route("/set_tempo", post(set_tempo_handler))
        .route("/set_time_signature", post(set_time_signature_handler))
        .route("/set_position", post(set_position_handler))
        .route("/set_record_mode", post(set_record_mode_handler))
        // Query endpoints
        .route("/status", get(status_handler))
        .route("/is_playing", get(is_playing_handler))
        .route("/is_recording", get(is_recording_handler))
        .route("/tempo", get(get_tempo_handler))
        .route("/time_signature", get(get_time_signature_handler))
        .route("/position", get(get_position_handler))
        .route("/record_mode", get(get_record_mode_handler))
        .route("/is_ready", get(is_ready_handler))
}

// Playback control handlers
async fn play_handler<T>(
    State(transport): State<Arc<Mutex<T>>>,
) -> Result<Json<ActionResponse>, (StatusCode, String)>
where
    T: TransportActions + Send + Sync,
{
    let mut transport = transport.lock().await;
    match transport.play() {
        Ok(message) => Ok(Json(ActionResponse {
            success: true,
            message,
        })),
        Err(e) => Err((StatusCode::INTERNAL_SERVER_ERROR, e.to_string())),
    }
}

async fn pause_handler<T>(
    State(transport): State<Arc<Mutex<T>>>,
) -> Result<Json<ActionResponse>, (StatusCode, String)>
where
    T: TransportActions + Send + Sync,
{
    let mut transport = transport.lock().await;
    match transport.pause() {
        Ok(message) => Ok(Json(ActionResponse {
            success: true,
            message,
        })),
        Err(e) => Err((StatusCode::INTERNAL_SERVER_ERROR, e.to_string())),
    }
}

async fn stop_handler<T>(
    State(transport): State<Arc<Mutex<T>>>,
) -> Result<Json<ActionResponse>, (StatusCode, String)>
where
    T: TransportActions + Send + Sync,
{
    let mut transport = transport.lock().await;
    match transport.stop() {
        Ok(message) => Ok(Json(ActionResponse {
            success: true,
            message,
        })),
        Err(e) => Err((StatusCode::INTERNAL_SERVER_ERROR, e.to_string())),
    }
}

async fn play_pause_handler<T>(
    State(transport): State<Arc<Mutex<T>>>,
) -> Result<Json<ActionResponse>, (StatusCode, String)>
where
    T: TransportActions + Send + Sync,
{
    let mut transport = transport.lock().await;
    match transport.play_pause() {
        Ok(message) => Ok(Json(ActionResponse {
            success: true,
            message,
        })),
        Err(e) => Err((StatusCode::INTERNAL_SERVER_ERROR, e.to_string())),
    }
}

async fn play_stop_handler<T>(
    State(transport): State<Arc<Mutex<T>>>,
) -> Result<Json<ActionResponse>, (StatusCode, String)>
where
    T: TransportActions + Send + Sync,
{
    let mut transport = transport.lock().await;
    match transport.play_stop() {
        Ok(message) => Ok(Json(ActionResponse {
            success: true,
            message,
        })),
        Err(e) => Err((StatusCode::INTERNAL_SERVER_ERROR, e.to_string())),
    }
}

// Recording control handlers
async fn start_recording_handler<T>(
    State(transport): State<Arc<Mutex<T>>>,
) -> Result<Json<ActionResponse>, (StatusCode, String)>
where
    T: TransportActions + Send + Sync,
{
    let mut transport = transport.lock().await;
    match transport.start_recording() {
        Ok(message) => Ok(Json(ActionResponse {
            success: true,
            message,
        })),
        Err(e) => Err((StatusCode::INTERNAL_SERVER_ERROR, e.to_string())),
    }
}

async fn stop_recording_handler<T>(
    State(transport): State<Arc<Mutex<T>>>,
) -> Result<Json<ActionResponse>, (StatusCode, String)>
where
    T: TransportActions + Send + Sync,
{
    let mut transport = transport.lock().await;
    match transport.stop_recording() {
        Ok(message) => Ok(Json(ActionResponse {
            success: true,
            message,
        })),
        Err(e) => Err((StatusCode::INTERNAL_SERVER_ERROR, e.to_string())),
    }
}

async fn toggle_recording_handler<T>(
    State(transport): State<Arc<Mutex<T>>>,
) -> Result<Json<ActionResponse>, (StatusCode, String)>
where
    T: TransportActions + Send + Sync,
{
    let mut transport = transport.lock().await;
    match transport.toggle_recording() {
        Ok(message) => Ok(Json(ActionResponse {
            success: true,
            message,
        })),
        Err(e) => Err((StatusCode::INTERNAL_SERVER_ERROR, e.to_string())),
    }
}

// Configuration handlers
async fn set_tempo_handler<T>(
    State(transport): State<Arc<Mutex<T>>>,
    Json(payload): Json<SetTempoRequest>,
) -> Result<Json<ActionResponse>, (StatusCode, String)>
where
    T: TransportActions + Send + Sync,
{
    let mut transport = transport.lock().await;
    let tempo = Tempo::new(payload.bpm);
    match transport.set_tempo(tempo) {
        Ok(message) => Ok(Json(ActionResponse {
            success: true,
            message,
        })),
        Err(e) => Err((StatusCode::BAD_REQUEST, e.to_string())),
    }
}

async fn set_time_signature_handler<T>(
    State(transport): State<Arc<Mutex<T>>>,
    Json(payload): Json<SetTimeSignatureRequest>,
) -> Result<Json<ActionResponse>, (StatusCode, String)>
where
    T: TransportActions + Send + Sync,
{
    let mut transport = transport.lock().await;
    let time_sig = TimeSignature::new(payload.numerator, payload.denominator);
    match transport.set_time_signature(time_sig) {
        Ok(message) => Ok(Json(ActionResponse {
            success: true,
            message,
        })),
        Err(e) => Err((StatusCode::BAD_REQUEST, e.to_string())),
    }
}

async fn set_position_handler<T>(
    State(transport): State<Arc<Mutex<T>>>,
    Json(payload): Json<SetPositionRequest>,
) -> Result<Json<ActionResponse>, (StatusCode, String)>
where
    T: TransportActions + Send + Sync,
{
    let mut transport = transport.lock().await;
    match transport.set_position(payload.seconds) {
        Ok(message) => Ok(Json(ActionResponse {
            success: true,
            message,
        })),
        Err(e) => Err((StatusCode::BAD_REQUEST, e.to_string())),
    }
}

async fn set_record_mode_handler<T>(
    State(transport): State<Arc<Mutex<T>>>,
    Json(payload): Json<SetRecordModeRequest>,
) -> Result<Json<ActionResponse>, (StatusCode, String)>
where
    T: TransportActions + Send + Sync,
{
    let mut transport = transport.lock().await;

    let record_mode = match payload.mode.to_lowercase().as_str() {
        "normal" => RecordMode::Normal,
        "time_selection" => RecordMode::TimeSelection,
        "item" => RecordMode::Item,
        _ => {
            return Err((
                StatusCode::BAD_REQUEST,
                "Invalid record mode. Valid options: normal, time_selection, item".to_string(),
            ));
        }
    };

    match transport.set_record_mode(record_mode) {
        Ok(message) => Ok(Json(ActionResponse {
            success: true,
            message,
        })),
        Err(e) => Err((StatusCode::BAD_REQUEST, e.to_string())),
    }
}

// Query handlers
async fn status_handler<T>(
    State(transport): State<Arc<Mutex<T>>>,
) -> Result<Json<TransportStatusResponse>, (StatusCode, String)>
where
    T: TransportActions + Send + Sync,
{
    let transport = transport.lock().await;

    let is_playing = transport
        .is_playing()
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?;
    let is_recording = transport
        .is_recording()
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?;
    let tempo = transport
        .get_tempo()
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?;
    let position = transport
        .get_position()
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?;
    let time_sig = transport
        .get_time_signature()
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?;
    let record_mode = transport
        .get_record_mode()
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?;

    let record_mode_str = match record_mode {
        RecordMode::Normal => "normal",
        RecordMode::TimeSelection => "time_selection",
        RecordMode::Item => "item",
    };

    Ok(Json(TransportStatusResponse {
        is_playing,
        is_recording,
        tempo_bpm: tempo.bpm,
        position_seconds: position,
        time_signature: TimeSignatureResponse {
            numerator: time_sig.numerator,
            denominator: time_sig.denominator,
        },
        record_mode: record_mode_str.to_string(),
    }))
}

async fn is_playing_handler<T>(
    State(transport): State<Arc<Mutex<T>>>,
) -> Result<Json<bool>, (StatusCode, String)>
where
    T: TransportActions + Send + Sync,
{
    let transport = transport.lock().await;
    let is_playing = transport
        .is_playing()
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?;
    Ok(Json(is_playing))
}

async fn is_recording_handler<T>(
    State(transport): State<Arc<Mutex<T>>>,
) -> Result<Json<bool>, (StatusCode, String)>
where
    T: TransportActions + Send + Sync,
{
    let transport = transport.lock().await;
    let is_recording = transport
        .is_recording()
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?;
    Ok(Json(is_recording))
}

async fn get_tempo_handler<T>(
    State(transport): State<Arc<Mutex<T>>>,
) -> Result<Json<TempoResponse>, (StatusCode, String)>
where
    T: TransportActions + Send + Sync,
{
    let transport = transport.lock().await;
    let tempo = transport
        .get_tempo()
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?;
    Ok(Json(TempoResponse { bpm: tempo.bpm }))
}

async fn get_time_signature_handler<T>(
    State(transport): State<Arc<Mutex<T>>>,
) -> Result<Json<TimeSignatureResponse>, (StatusCode, String)>
where
    T: TransportActions + Send + Sync,
{
    let transport = transport.lock().await;
    let time_sig = transport
        .get_time_signature()
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?;
    Ok(Json(TimeSignatureResponse {
        numerator: time_sig.numerator,
        denominator: time_sig.denominator,
    }))
}

async fn get_position_handler<T>(
    State(transport): State<Arc<Mutex<T>>>,
) -> Result<Json<PositionResponse>, (StatusCode, String)>
where
    T: TransportActions + Send + Sync,
{
    let transport = transport.lock().await;
    let position = transport
        .get_position()
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?;
    Ok(Json(PositionResponse { seconds: position }))
}

async fn get_record_mode_handler<T>(
    State(transport): State<Arc<Mutex<T>>>,
) -> Result<Json<String>, (StatusCode, String)>
where
    T: TransportActions + Send + Sync,
{
    let transport = transport.lock().await;
    let record_mode = transport
        .get_record_mode()
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?;

    let mode_str = match record_mode {
        RecordMode::Normal => "normal",
        RecordMode::TimeSelection => "time_selection",
        RecordMode::Item => "item",
    };

    Ok(Json(mode_str.to_string()))
}

async fn is_ready_handler<T>(
    State(transport): State<Arc<Mutex<T>>>,
) -> Result<Json<bool>, (StatusCode, String)>
where
    T: TransportActions + Send + Sync,
{
    let transport = transport.lock().await;
    let is_ready = transport
        .is_ready()
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?;
    Ok(Json(is_ready))
}
