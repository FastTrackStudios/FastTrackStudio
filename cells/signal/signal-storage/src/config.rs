//! Configuration for cloud storage connections.

use std::time::Duration;

/// Cloud database provider presets.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CloudProvider {
    /// Supabase PostgreSQL (pooler mode).
    Supabase {
        project_ref: String,
        password: String,
    },
    /// Neon serverless PostgreSQL.
    Neon {
        project_id: String,
        role: String,
        password: String,
        database: String,
    },
    /// Railway PostgreSQL.
    Railway {
        host: String,
        port: u16,
        user: String,
        password: String,
        database: String,
    },
    /// Custom PostgreSQL connection string.
    Custom {
        connection_string: String,
    },
}

impl CloudProvider {
    /// Build the PostgreSQL connection string for this provider.
    pub fn connection_string(&self) -> String {
        match self {
            Self::Supabase {
                project_ref,
                password,
            } => {
                format!(
                    "postgres://postgres.{}:{}@aws-0-us-east-1.pooler.supabase.com:6543/postgres?sslmode=require",
                    project_ref, password
                )
            }
            Self::Neon {
                project_id,
                role,
                password,
                database,
            } => {
                format!(
                    "postgres://{}:{}@{}.neon.tech/{}?sslmode=require",
                    role, password, project_id, database
                )
            }
            Self::Railway {
                host,
                port,
                user,
                password,
                database,
            } => {
                format!(
                    "postgres://{}:{}@{}:{}/{}?sslmode=require",
                    user, password, host, port, database
                )
            }
            Self::Custom { connection_string } => connection_string.clone(),
        }
    }
}

/// Configuration for the cloud storage connection pool.
#[derive(Debug, Clone)]
pub struct CloudConfig {
    /// Connection string or provider.
    pub connection_string: String,
    /// Maximum number of connections in the pool.
    pub max_connections: u32,
    /// Minimum number of idle connections to keep.
    pub min_connections: u32,
    /// Maximum time to wait for a connection from the pool.
    pub acquire_timeout: Duration,
    /// Maximum connection lifetime before recycling.
    pub max_lifetime: Duration,
    /// Idle timeout before closing a connection.
    pub idle_timeout: Duration,
    /// Number of retry attempts for transient failures.
    pub max_retries: u32,
    /// Base delay between retries (doubles each attempt).
    pub retry_base_delay: Duration,
    /// Statement-level timeout.
    pub statement_timeout: Duration,
}

impl CloudConfig {
    /// Create config from a cloud provider preset.
    pub fn from_provider(provider: CloudProvider) -> Self {
        Self {
            connection_string: provider.connection_string(),
            ..Self::default()
        }
    }

    /// Create config from an explicit connection string.
    pub fn from_connection_string(url: impl Into<String>) -> Self {
        Self {
            connection_string: url.into(),
            ..Self::default()
        }
    }

    /// Create config from the `DATABASE_URL` environment variable.
    pub fn from_env() -> Result<Self, crate::error::StorageError> {
        let url = std::env::var("DATABASE_URL").map_err(|_| {
            crate::error::StorageError::Config(
                "DATABASE_URL environment variable not set".to_string(),
            )
        })?;
        Ok(Self::from_connection_string(url))
    }
}

impl Default for CloudConfig {
    fn default() -> Self {
        Self {
            connection_string: String::new(),
            max_connections: 10,
            min_connections: 1,
            acquire_timeout: Duration::from_secs(5),
            max_lifetime: Duration::from_secs(30 * 60),
            idle_timeout: Duration::from_secs(10 * 60),
            max_retries: 3,
            retry_base_delay: Duration::from_millis(100),
            statement_timeout: Duration::from_secs(30),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn supabase_connection_string() {
        let provider = CloudProvider::Supabase {
            project_ref: "myproject".to_string(),
            password: "secret".to_string(),
        };
        let url = provider.connection_string();
        assert!(url.contains("myproject"));
        assert!(url.contains("secret"));
        assert!(url.contains("pooler.supabase.com"));
        assert!(url.contains("sslmode=require"));
    }

    #[test]
    fn neon_connection_string() {
        let provider = CloudProvider::Neon {
            project_id: "cool-dawn-123456".to_string(),
            role: "neondb_owner".to_string(),
            password: "pass123".to_string(),
            database: "neondb".to_string(),
        };
        let url = provider.connection_string();
        assert!(url.contains("neon.tech"));
        assert!(url.contains("neondb_owner"));
        assert!(url.contains("sslmode=require"));
    }

    #[test]
    fn railway_connection_string() {
        let provider = CloudProvider::Railway {
            host: "roundhouse.proxy.rlwy.net".to_string(),
            port: 5432,
            user: "postgres".to_string(),
            password: "abc123".to_string(),
            database: "railway".to_string(),
        };
        let url = provider.connection_string();
        assert!(url.contains("roundhouse.proxy.rlwy.net"));
        assert!(url.contains("5432"));
    }

    #[test]
    fn custom_connection_string_passthrough() {
        let url = "postgres://user:pass@localhost:5432/db";
        let provider = CloudProvider::Custom {
            connection_string: url.to_string(),
        };
        assert_eq!(provider.connection_string(), url);
    }

    #[test]
    fn config_from_provider() {
        let config = CloudConfig::from_provider(CloudProvider::Custom {
            connection_string: "postgres://localhost/test".to_string(),
        });
        assert_eq!(config.connection_string, "postgres://localhost/test");
        assert_eq!(config.max_connections, 10);
    }

    #[test]
    fn config_defaults_are_sensible() {
        let config = CloudConfig::default();
        assert_eq!(config.max_connections, 10);
        assert_eq!(config.min_connections, 1);
        assert_eq!(config.max_retries, 3);
        assert!(config.acquire_timeout.as_secs() > 0);
        assert!(config.statement_timeout.as_secs() > 0);
    }
}
