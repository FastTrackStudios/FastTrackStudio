use architect_auth::{
    ArchitectAuth, AuthServiceDispatcher,
    db::{AuthSeaOrmStorage, Migrator},
    transport::vox::{AuthClientMiddleware, AuthServerMiddleware, AuthVoxService},
};
use sea_orm::Database;
use sea_orm_migration::MigratorTrait;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let db = Database::connect("sqlite::memory:").await?;
    Migrator::up(&db, None).await?;
    let auth = ArchitectAuth::builder()
        .secret("a-secret-at-least-32-bytes-long!!")
        .storage(AuthSeaOrmStorage::new(db))
        .build()?;

    let _dispatcher =
        AuthServiceDispatcher::new(AuthVoxService::new(auth)).with_middleware(AuthServerMiddleware);
    let _client_middleware = AuthClientMiddleware::bearer("session-token");

    Ok(())
}
