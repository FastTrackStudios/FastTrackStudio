//! Unified service layer that works across tarpc (network RPC) and taurpc (desktop IPC)
//!
//! This library defines service traits that can be implemented once and used in:
//! - Network microservices (via tarpc)
//! - Desktop applications (via taurpc/Tauri)
//! - Direct function calls (monolith deployment)

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use thiserror::Error;

// Re-exports for convenience
#[cfg(feature = "network")]
pub use tarpc;

#[cfg(feature = "desktop")]
pub use taurpc;

// Common error types that work across all transport layers
#[derive(Debug, Error, Serialize, Deserialize)]
#[cfg_attr(feature = "desktop", derive(specta::Type))]
#[serde(tag = "type", content = "data")]
pub enum ServiceError {
    #[error("Not found: {resource}")]
    NotFound { resource: String },

    #[error("Validation error: {message}")]
    Validation { message: String },

    #[error("Internal error: {message}")]
    Internal { message: String },

    #[error("Permission denied: {action}")]
    PermissionDenied { action: String },

    #[error("Service unavailable: {service}")]
    ServiceUnavailable { service: String },
}

// Common data types
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "desktop", derive(specta::Type))]
#[cfg_attr(feature = "desktop", taurpc::ipc_type)]
pub struct User {
    pub id: String,
    pub username: String,
    pub email: String,
    pub created_at: chrono::DateTime<chrono::Utc>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "desktop", derive(specta::Type))]
#[cfg_attr(feature = "desktop", taurpc::ipc_type)]
pub struct Project {
    pub id: String,
    pub name: String,
    pub owner_id: String,
    pub collaborators: Vec<String>,
    pub created_at: chrono::DateTime<chrono::Utc>,
    pub updated_at: chrono::DateTime<chrono::Utc>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "desktop", derive(specta::Type))]
#[cfg_attr(feature = "desktop", taurpc::ipc_type)]
pub struct AudioTrack {
    pub id: String,
    pub project_id: String,
    pub name: String,
    pub file_path: String,
    pub duration_ms: u64,
    pub sample_rate: u32,
    pub channels: u16,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "desktop", derive(specta::Type))]
#[cfg_attr(feature = "desktop", taurpc::ipc_type)]
pub struct ProcessingJob {
    pub id: String,
    pub track_id: String,
    pub effects: Vec<AudioEffect>,
    pub status: JobStatus,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "desktop", derive(specta::Type))]
#[cfg_attr(feature = "desktop", taurpc::ipc_type)]
pub enum AudioEffect {
    Reverb { room_size: f32, damping: f32 },
    Delay { delay_ms: u32, feedback: f32 },
    Compressor { threshold: f32, ratio: f32 },
    EQ { bands: Vec<EQBand> },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "desktop", derive(specta::Type))]
#[cfg_attr(feature = "desktop", taurpc::ipc_type)]
pub struct EQBand {
    pub frequency: f32,
    pub gain: f32,
    pub q: f32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "desktop", derive(specta::Type))]
#[cfg_attr(feature = "desktop", taurpc::ipc_type)]
pub enum JobStatus {
    Pending,
    Processing { progress: u8 },
    Completed,
    Failed { error: String },
}

// Service trait definitions that work with both tarpc and taurpc

/// User management service
/// Works as both network RPC (tarpc) and desktop IPC (taurpc)
#[cfg_attr(feature = "network", tarpc::service)]
#[cfg_attr(feature = "desktop", taurpc::procedures(export_to = "../src/lib/user-bindings.ts"))]
pub trait UserService {
    async fn create_user(username: String, email: String) -> Result<User, ServiceError>;
    async fn get_user(id: String) -> Result<User, ServiceError>;
    async fn update_user(id: String, username: Option<String>, email: Option<String>) -> Result<User, ServiceError>;
    async fn delete_user(id: String) -> Result<(), ServiceError>;
    async fn list_users(limit: Option<u32>, offset: Option<u32>) -> Result<Vec<User>, ServiceError>;
    async fn authenticate(username: String, password: String) -> Result<String, ServiceError>; // Returns JWT token
}

/// Project management service
#[cfg_attr(feature = "network", tarpc::service)]
#[cfg_attr(feature = "desktop", taurpc::procedures(export_to = "../src/lib/project-bindings.ts"))]
pub trait ProjectService {
    async fn create_project(name: String, owner_id: String) -> Result<Project, ServiceError>;
    async fn get_project(id: String) -> Result<Project, ServiceError>;
    async fn update_project(id: String, name: Option<String>) -> Result<Project, ServiceError>;
    async fn delete_project(id: String) -> Result<(), ServiceError>;
    async fn list_projects(owner_id: String) -> Result<Vec<Project>, ServiceError>;
    async fn add_collaborator(project_id: String, user_id: String) -> Result<(), ServiceError>;
    async fn remove_collaborator(project_id: String, user_id: String) -> Result<(), ServiceError>;
}

/// Audio processing service
#[cfg_attr(feature = "network", tarpc::service)]
#[cfg_attr(feature = "desktop", taurpc::procedures(export_to = "../src/lib/audio-bindings.ts"))]
pub trait AudioService {
    async fn upload_track(project_id: String, name: String, file_data: Vec<u8>) -> Result<AudioTrack, ServiceError>;
    async fn get_track(id: String) -> Result<AudioTrack, ServiceError>;
    async fn delete_track(id: String) -> Result<(), ServiceError>;
    async fn list_tracks(project_id: String) -> Result<Vec<AudioTrack>, ServiceError>;
    async fn process_track(track_id: String, effects: Vec<AudioEffect>) -> Result<ProcessingJob, ServiceError>;
    async fn get_processing_job(job_id: String) -> Result<ProcessingJob, ServiceError>;

    // Event-based progress updates (works great with taurpc channels)
    #[cfg_attr(feature = "desktop", taurpc::procedure)]
    async fn subscribe_to_job_progress(job_id: String) -> Result<(), ServiceError>;
}

/// Real-time events for desktop UI
#[cfg(feature = "desktop")]
#[taurpc::procedures(path = "events", export_to = "../src/lib/event-bindings.ts")]
pub trait AudioEvents {
    #[taurpc(event)]
    async fn job_progress_updated(job_id: String, progress: u8);

    #[taurpc(event)]
    async fn job_completed(job_id: String, result_track_id: Option<String>);

    #[taurpc(event)]
    async fn job_failed(job_id: String, error: String);

    #[taurpc(event)]
    async fn track_uploaded(track: AudioTrack);
}

// Service implementation resolver that can choose between local, network, or desktop
pub enum ServiceResolver<T> {
    Local(T),
    #[cfg(feature = "network")]
    Network(Box<dyn std::future::Future<Output = Result<T, ServiceError>> + Send + Unpin>),
    #[cfg(feature = "desktop")]
    Desktop(T), // For taurpc, the client is generated
}

// Convenience macro for creating unified service implementations
#[macro_export]
macro_rules! impl_unified_service {
    ($service_trait:ident, $impl_type:ident) => {
        // Local implementation (for monolith)
        impl $service_trait for $impl_type {
            // Implementation methods here...
        }

        // Network RPC resolver (for microservices)
        #[cfg(feature = "network")]
        impl $service_trait for tarpc::client::NewClient<
            tarpc::client::Channel<
                std::sync::Arc<tarpc::ClientMessage<$service_trait>>,
                tarpc::Response<$service_trait>
            >
        > {
            // Delegate to tarpc client...
        }

        // Desktop IPC resolver (for Tauri)
        #[cfg(feature = "desktop")]
        #[taurpc::resolvers]
        impl $service_trait for $impl_type {
            // Delegate to taurpc...
        }
    };
}

// Configuration for service resolution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ServiceConfig {
    pub user_service: ServiceEndpoint,
    pub project_service: ServiceEndpoint,
    pub audio_service: ServiceEndpoint,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ServiceEndpoint {
    Local,
    Network { host: String, port: u16 },
    Desktop, // Resolved via IPC
}

impl Default for ServiceConfig {
    fn default() -> Self {
        Self {
            user_service: ServiceEndpoint::Local,
            project_service: ServiceEndpoint::Local,
            audio_service: ServiceEndpoint::Local,
        }
    }
}

// Utility functions for service discovery and connection
pub async fn connect_user_service(config: &ServiceConfig) -> Result<Box<dyn UserService>, ServiceError> {
    match &config.user_service {
        ServiceEndpoint::Local => {
            // Return local implementation
            todo!("Create local user service")
        }
        #[cfg(feature = "network")]
        ServiceEndpoint::Network { host, port } => {
            // Connect via tarpc
            use tarpc::{client, tokio_serde::formats::Json};

            let transport = tarpc::serde_transport::tcp::connect(
                format!("{}:{}", host, port),
                Json::default()
            ).await.map_err(|e| ServiceError::ServiceUnavailable {
                service: format!("user-service at {}:{}", host, port)
            })?;

            let client = UserServiceClient::new(client::Config::default(), transport).spawn();
            Ok(Box::new(client))
        }
        ServiceEndpoint::Desktop => {
            // This would be handled differently in the Tauri context
            Err(ServiceError::Internal {
                message: "Desktop endpoint not supported in this context".to_string()
            })
        }
    }
}

// Helper trait for unified async service calls
#[async_trait::async_trait]
pub trait UnifiedService<T> {
    async fn call(&self) -> Result<T, ServiceError>;
}

// Extension trait to add unified behavior to any service
pub trait ServiceExt {
    fn with_tracing(self) -> Self;
    fn with_retry(self, max_attempts: u32) -> Self;
    fn with_timeout(self, timeout: std::time::Duration) -> Self;
}

// Context that can be passed to all service calls (like tarpc::Context but universal)
#[derive(Debug, Clone)]
pub struct ServiceContext {
    pub correlation_id: String,
    pub user_id: Option<String>,
    pub timeout: Option<std::time::Duration>,
    pub metadata: HashMap<String, String>,
}

impl ServiceContext {
    pub fn new() -> Self {
        Self {
            correlation_id: uuid::Uuid::new_v4().to_string(),
            user_id: None,
            timeout: None,
            metadata: HashMap::new(),
        }
    }

    pub fn with_user(mut self, user_id: String) -> Self {
        self.user_id = Some(user_id);
        self
    }

    pub fn with_timeout(mut self, timeout: std::time::Duration) -> Self {
        self.timeout = Some(timeout);
        self
    }
}

impl Default for ServiceContext {
    fn default() -> Self {
        Self::new()
    }
}

// Export commonly used types
pub mod prelude {
    pub use super::{
        ServiceError, User, Project, AudioTrack, AudioEffect, ProcessingJob, JobStatus,
        UserService, ProjectService, AudioService, ServiceConfig, ServiceEndpoint, ServiceContext,
    };

    #[cfg(feature = "desktop")]
    pub use super::AudioEvents;

    #[cfg(feature = "network")]
    pub use tarpc::{context, client, server};

    #[cfg(feature = "desktop")]
    pub use taurpc::{Router, procedures, resolvers, ipc_type};
}
