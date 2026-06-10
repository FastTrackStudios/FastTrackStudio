//! Public entry point for architect-auth.

pub use auth::*;
pub use auth_proto as proto;

#[cfg(feature = "db")]
pub mod db {
    pub use auth::backend_db::AuthSeaOrmStorage;
    pub use auth_db::*;
}
