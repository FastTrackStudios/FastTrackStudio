//! Wires [`VersionStoreBackend`] into an actual jj-lib repo: everything
//! above the `Backend` trait — op-log concurrency, divergent changes,
//! transactions — is jj-lib's own machinery, unmodified. This module only
//! supplies the initializers `ReadonlyRepo::init` needs and a couple of
//! settings defaults.

use std::path::Path;
use std::sync::Arc;

use jj_lib::backend::Backend;
use jj_lib::config::{ConfigLayer, ConfigSource, StackedConfig};
use jj_lib::repo::{ReadonlyRepo, RepoInitError};
use jj_lib::settings::UserSettings;
use jj_lib::signing::Signer;

use crate::backend::VersionStoreBackend;
use crate::error::{Error, Result};

/// Default settings: jj-lib's own baked-in config (`config/misc.toml`) is
/// enough — `user.name`/`user.email` default to `""`, `signing.behavior`
/// defaults to `"keep"` with `signing.backend = "none"`, which resolves to
/// no signing backend configured (`Signer::new(None, vec![])` below) and no
/// commits ever get signed.
///
/// One override: ADR 0001's backend policy calls for
/// `snapshot.max-new-file-size = 0` (disabled) — this crate's own
/// `checkpoint` module never goes through jj's `local_working_copy`
/// snapshotting (it builds trees directly, so the limit has no effect
/// today), but a future working-copy-driven flow (the sync daemon,
/// desktop checkout) will load this same config, and multi-GB media must
/// never hit jj-cli's 1 MiB anti-footgun default.
pub fn default_settings() -> Result<UserSettings> {
    let mut config = StackedConfig::with_defaults();
    let overrides = ConfigLayer::parse(ConfigSource::User, "[snapshot]\nmax-new-file-size = 0\n")
        .map_err(|e| Error::Repo(format!("building snapshot policy overrides: {e}")))?;
    config.add_layer(overrides);
    UserSettings::from_config(config)
        .map_err(|e| Error::Repo(format!("building default UserSettings: {e}")))
}

/// Initialize a brand-new repo at `repo_path` (must not yet exist) backed
/// by [`VersionStoreBackend`]. `repo_path` becomes the jj repo's `.jj`-style
/// metadata directory; the backend's own chunk/object stores live under
/// `repo_path/store` (jj's own convention — see `ReadonlyRepo::init`).
pub async fn init_repo(repo_path: &Path) -> Result<Arc<ReadonlyRepo>> {
    let settings = default_settings()?;
    tokio::fs::create_dir_all(repo_path).await?;

    let backend_initializer =
        |_settings: &UserSettings,
         store_path: &Path|
         -> std::result::Result<Box<dyn Backend>, jj_lib::backend::BackendInitError> {
            let store_path = store_path.to_path_buf();
            pollster::block_on(VersionStoreBackend::open(&store_path))
                .map(|backend| Box::new(backend) as Box<dyn Backend>)
                .map_err(|e| jj_lib::backend::BackendInitError(e.into()))
        };

    ReadonlyRepo::init(
        &settings,
        repo_path,
        &backend_initializer,
        Signer::new(None, vec![]),
        ReadonlyRepo::default_op_store_initializer(),
        ReadonlyRepo::default_op_heads_store_initializer(),
        ReadonlyRepo::default_index_store_initializer(),
        ReadonlyRepo::default_submodule_store_initializer(),
    )
    .await
    .map_err(|e: RepoInitError| Error::Repo(e.to_string()))
}
