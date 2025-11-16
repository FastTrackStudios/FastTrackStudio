//! Unified service demo library
//!
//! Demonstrates how to create service implementations that work with:
//! - tarpc (network RPC)
//! - taurpc (desktop IPC)
//! - direct function calls (monolith)

use services::prelude::*;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use uuid::Uuid;
use chrono::Utc;

// In-memory storage for demo purposes
pub type UserStorage = Arc<RwLock<HashMap<String, User>>>;
pub type ProjectStorage = Arc<RwLock<HashMap<String, Project>>>;
pub type TrackStorage = Arc<RwLock<HashMap<String, AudioTrack>>>;
pub type JobStorage = Arc<RwLock<HashMap<String, ProcessingJob>>>;

#[derive(Clone)]
pub struct AppState {
    pub users: UserStorage,
    pub projects: ProjectStorage,
    pub tracks: TrackStorage,
    pub jobs: JobStorage,
}

impl Default for AppState {
    fn default() -> Self {
        Self {
            users: Arc::new(RwLock::new(HashMap::new())),
            projects: Arc::new(RwLock::new(HashMap::new())),
            tracks: Arc::new(RwLock::new(HashMap::new())),
            jobs: Arc::new(RwLock::new(HashMap::new())),
        }
    }
}

// User service implementation
#[derive(Clone)]
pub struct UserServiceImpl {
    state: AppState,
}

impl UserServiceImpl {
    pub fn new(state: AppState) -> Self {
        Self { state }
    }
}

// Implement the unified UserService trait
#[cfg_attr(feature = "network", tarpc::server)]
#[cfg_attr(feature = "desktop", taurpc::resolvers)]
impl UserService for UserServiceImpl {
    async fn create_user(self, username: String, email: String) -> Result<User, ServiceError> {
        let user = User {
            id: Uuid::new_v4().to_string(),
            username: username.clone(),
            email: email.clone(),
            created_at: Utc::now(),
        };

        // Check if username already exists
        let users = self.state.users.read().await;
        if users.values().any(|u| u.username == username) {
            return Err(ServiceError::Validation {
                message: "Username already exists".to_string(),
            });
        }
        drop(users);

        // Insert new user
        let mut users = self.state.users.write().await;
        users.insert(user.id.clone(), user.clone());

        tracing::info!("Created user: {} ({})", username, user.id);
        Ok(user)
    }

    async fn get_user(self, id: String) -> Result<User, ServiceError> {
        let users = self.state.users.read().await;
        users.get(&id)
            .cloned()
            .ok_or_else(|| ServiceError::NotFound {
                resource: format!("User with id: {}", id),
            })
    }

    async fn update_user(
        self,
        id: String,
        username: Option<String>,
        email: Option<String>,
    ) -> Result<User, ServiceError> {
        let mut users = self.state.users.write().await;
        let user = users.get_mut(&id).ok_or_else(|| ServiceError::NotFound {
            resource: format!("User with id: {}", id),
        })?;

        if let Some(new_username) = username {
            user.username = new_username;
        }
        if let Some(new_email) = email {
            user.email = new_email;
        }

        let updated_user = user.clone();
        tracing::info!("Updated user: {}", id);
        Ok(updated_user)
    }

    async fn delete_user(self, id: String) -> Result<(), ServiceError> {
        let mut users = self.state.users.write().await;
        users.remove(&id).ok_or_else(|| ServiceError::NotFound {
            resource: format!("User with id: {}", id),
        })?;

        tracing::info!("Deleted user: {}", id);
        Ok(())
    }

    async fn list_users(
        self,
        limit: Option<u32>,
        offset: Option<u32>,
    ) -> Result<Vec<User>, ServiceError> {
        let users = self.state.users.read().await;
        let mut user_list: Vec<User> = users.values().cloned().collect();
        user_list.sort_by(|a, b| a.created_at.cmp(&b.created_at));

        let offset = offset.unwrap_or(0) as usize;
        let limit = limit.unwrap_or(100) as usize;

        Ok(user_list.into_iter().skip(offset).take(limit).collect())
    }

    async fn authenticate(self, username: String, password: String) -> Result<String, ServiceError> {
        let users = self.state.users.read().await;
        let user = users.values().find(|u| u.username == username)
            .ok_or_else(|| ServiceError::NotFound {
                resource: "User".to_string(),
            })?;

        // In a real implementation, you'd verify the password hash
        // For demo purposes, just accept any password
        tracing::info!("Authenticated user: {}", user.username);
        Ok(format!("jwt_token_for_{}", user.id))
    }
}

// Project service implementation
#[derive(Clone)]
pub struct ProjectServiceImpl {
    state: AppState,
}

impl ProjectServiceImpl {
    pub fn new(state: AppState) -> Self {
        Self { state }
    }
}

#[cfg_attr(feature = "network", tarpc::server)]
#[cfg_attr(feature = "desktop", taurpc::resolvers)]
impl ProjectService for ProjectServiceImpl {
    async fn create_project(self, name: String, owner_id: String) -> Result<Project, ServiceError> {
        // Verify owner exists
        let users = self.state.users.read().await;
        if !users.contains_key(&owner_id) {
            return Err(ServiceError::NotFound {
                resource: format!("User with id: {}", owner_id),
            });
        }
        drop(users);

        let project = Project {
            id: Uuid::new_v4().to_string(),
            name: name.clone(),
            owner_id: owner_id.clone(),
            collaborators: vec![],
            created_at: Utc::now(),
            updated_at: Utc::now(),
        };

        let mut projects = self.state.projects.write().await;
        projects.insert(project.id.clone(), project.clone());

        tracing::info!("Created project: {} for user: {}", name, owner_id);
        Ok(project)
    }

    async fn get_project(self, id: String) -> Result<Project, ServiceError> {
        let projects = self.state.projects.read().await;
        projects.get(&id)
            .cloned()
            .ok_or_else(|| ServiceError::NotFound {
                resource: format!("Project with id: {}", id),
            })
    }

    async fn update_project(self, id: String, name: Option<String>) -> Result<Project, ServiceError> {
        let mut projects = self.state.projects.write().await;
        let project = projects.get_mut(&id).ok_or_else(|| ServiceError::NotFound {
            resource: format!("Project with id: {}", id),
        })?;

        if let Some(new_name) = name {
            project.name = new_name;
        }
        project.updated_at = Utc::now();

        let updated_project = project.clone();
        tracing::info!("Updated project: {}", id);
        Ok(updated_project)
    }

    async fn delete_project(self, id: String) -> Result<(), ServiceError> {
        let mut projects = self.state.projects.write().await;
        projects.remove(&id).ok_or_else(|| ServiceError::NotFound {
            resource: format!("Project with id: {}", id),
        })?;

        tracing::info!("Deleted project: {}", id);
        Ok(())
    }

    async fn list_projects(self, owner_id: String) -> Result<Vec<Project>, ServiceError> {
        let projects = self.state.projects.read().await;
        let user_projects: Vec<Project> = projects
            .values()
            .filter(|p| p.owner_id == owner_id || p.collaborators.contains(&owner_id))
            .cloned()
            .collect();

        Ok(user_projects)
    }

    async fn add_collaborator(self, project_id: String, user_id: String) -> Result<(), ServiceError> {
        // Verify user exists
        let users = self.state.users.read().await;
        if !users.contains_key(&user_id) {
            return Err(ServiceError::NotFound {
                resource: format!("User with id: {}", user_id),
            });
        }
        drop(users);

        let mut projects = self.state.projects.write().await;
        let project = projects.get_mut(&project_id).ok_or_else(|| ServiceError::NotFound {
            resource: format!("Project with id: {}", project_id),
        })?;

        if !project.collaborators.contains(&user_id) {
            project.collaborators.push(user_id.clone());
            project.updated_at = Utc::now();
        }

        tracing::info!("Added collaborator {} to project {}", user_id, project_id);
        Ok(())
    }

    async fn remove_collaborator(self, project_id: String, user_id: String) -> Result<(), ServiceError> {
        let mut projects = self.state.projects.write().await;
        let project = projects.get_mut(&project_id).ok_or_else(|| ServiceError::NotFound {
            resource: format!("Project with id: {}", project_id),
        })?;

        project.collaborators.retain(|id| id != &user_id);
        project.updated_at = Utc::now();

        tracing::info!("Removed collaborator {} from project {}", user_id, project_id);
        Ok(())
    }
}

// Audio service implementation
#[derive(Clone)]
pub struct AudioServiceImpl {
    state: AppState,
}

impl AudioServiceImpl {
    pub fn new(state: AppState) -> Self {
        Self { state }
    }
}

#[cfg_attr(feature = "network", tarpc::server)]
#[cfg_attr(feature = "desktop", taurpc::resolvers)]
impl AudioService for AudioServiceImpl {
    async fn upload_track(
        self,
        project_id: String,
        name: String,
        file_data: Vec<u8>,
    ) -> Result<AudioTrack, ServiceError> {
        // Verify project exists
        let projects = self.state.projects.read().await;
        if !projects.contains_key(&project_id) {
            return Err(ServiceError::NotFound {
                resource: format!("Project with id: {}", project_id),
            });
        }
        drop(projects);

        let track = AudioTrack {
            id: Uuid::new_v4().to_string(),
            project_id: project_id.clone(),
            name: name.clone(),
            file_path: format!("/tmp/track_{}.wav", Uuid::new_v4()),
            duration_ms: 30000, // Mock 30 second duration
            sample_rate: 44100,
            channels: 2,
        };

        // In a real implementation, you'd save the file_data to disk
        tracing::info!("Uploaded {} bytes for track: {}", file_data.len(), name);

        let mut tracks = self.state.tracks.write().await;
        tracks.insert(track.id.clone(), track.clone());

        tracing::info!("Created track: {} in project: {}", name, project_id);
        Ok(track)
    }

    async fn get_track(self, id: String) -> Result<AudioTrack, ServiceError> {
        let tracks = self.state.tracks.read().await;
        tracks.get(&id)
            .cloned()
            .ok_or_else(|| ServiceError::NotFound {
                resource: format!("Track with id: {}", id),
            })
    }

    async fn delete_track(self, id: String) -> Result<(), ServiceError> {
        let mut tracks = self.state.tracks.write().await;
        tracks.remove(&id).ok_or_else(|| ServiceError::NotFound {
            resource: format!("Track with id: {}", id),
        })?;

        tracing::info!("Deleted track: {}", id);
        Ok(())
    }

    async fn list_tracks(self, project_id: String) -> Result<Vec<AudioTrack>, ServiceError> {
        let tracks = self.state.tracks.read().await;
        let project_tracks: Vec<AudioTrack> = tracks
            .values()
            .filter(|t| t.project_id == project_id)
            .cloned()
            .collect();

        Ok(project_tracks)
    }

    async fn process_track(
        self,
        track_id: String,
        effects: Vec<AudioEffect>,
    ) -> Result<ProcessingJob, ServiceError> {
        // Verify track exists
        let tracks = self.state.tracks.read().await;
        if !tracks.contains_key(&track_id) {
            return Err(ServiceError::NotFound {
                resource: format!("Track with id: {}", track_id),
            });
        }
        drop(tracks);

        let job = ProcessingJob {
            id: Uuid::new_v4().to_string(),
            track_id: track_id.clone(),
            effects: effects.clone(),
            status: JobStatus::Pending,
        };

        let mut jobs = self.state.jobs.write().await;
        jobs.insert(job.id.clone(), job.clone());

        // Simulate processing by spawning a background task
        let job_id = job.id.clone();
        let state = self.state.clone();
        tokio::spawn(async move {
            // Simulate processing time
            for progress in [10, 25, 50, 75, 90, 100] {
                tokio::time::sleep(std::time::Duration::from_millis(500)).await;

                let mut jobs = state.jobs.write().await;
                if let Some(job) = jobs.get_mut(&job_id) {
                    job.status = if progress == 100 {
                        JobStatus::Completed
                    } else {
                        JobStatus::Processing { progress }
                    };
                }

                tracing::info!("Job {} progress: {}%", job_id, progress);
            }
        });

        tracing::info!("Started processing job: {} for track: {}", job.id, track_id);
        Ok(job)
    }

    async fn get_processing_job(self, job_id: String) -> Result<ProcessingJob, ServiceError> {
        let jobs = self.state.jobs.read().await;
        jobs.get(&job_id)
            .cloned()
            .ok_or_else(|| ServiceError::NotFound {
                resource: format!("Job with id: {}", job_id),
            })
    }

    async fn subscribe_to_job_progress(self, job_id: String) -> Result<(), ServiceError> {
        // In a real implementation, this would set up a subscription
        // For now, just validate the job exists
        let jobs = self.state.jobs.read().await;
        if !jobs.contains_key(&job_id) {
            return Err(ServiceError::NotFound {
                resource: format!("Job with id: {}", job_id),
            });
        }

        tracing::info!("Subscribed to progress for job: {}", job_id);
        Ok(())
    }
}

// Event handler for desktop UI
#[cfg(feature = "desktop")]
#[derive(Clone)]
pub struct AudioEventsImpl;

#[cfg(feature = "desktop")]
#[taurpc::resolvers]
impl AudioEvents for AudioEventsImpl {}

// Helper function to initialize tracing
pub fn init_tracing(service_name: &'static str) -> anyhow::Result<()> {
    let env_filter = tracing_subscriber::EnvFilter::try_from_default_env()
        .unwrap_or_else(|_| tracing_subscriber::EnvFilter::new("info"));

    tracing_subscriber::fmt()
        .with_env_filter(env_filter)
        .with_target(true)
        .init();

    tracing::info!("Initialized tracing for service: {}", service_name);
    Ok(())
}

// Service factory for different deployment modes
pub enum ServiceMode {
    Monolith,
    Microservices,
    Desktop,
}

pub struct ServiceFactory;

impl ServiceFactory {
    pub fn create_services(mode: ServiceMode, state: AppState) -> ServiceBundle {
        match mode {
            ServiceMode::Monolith => ServiceBundle {
                user_service: Box::new(UserServiceImpl::new(state.clone())),
                project_service: Box::new(ProjectServiceImpl::new(state.clone())),
                audio_service: Box::new(AudioServiceImpl::new(state)),
            },
            ServiceMode::Microservices => {
                // In a real implementation, these would be network clients
                todo!("Create network service clients")
            }
            ServiceMode::Desktop => {
                // In a real implementation, these would be taurpc clients
                todo!("Create desktop IPC clients")
            }
        }
    }
}

pub struct ServiceBundle {
    pub user_service: Box<dyn UserService>,
    pub project_service: Box<dyn ProjectService>,
    pub audio_service: Box<dyn AudioService>,
}
