//! Public Signal crate.
//!
//! Re-exports protocol types and controller APIs for consumers.

pub use signal_controller::SignalController;
pub use signal_live::SignalLive;
pub use signal_proto::*;
pub use signal_storage::{
    default_block_collections, default_module_collections, default_seed_engines, default_seed_layers,
    default_seed_profiles, default_seed_rigs, default_seed_songs, runtime_seed_bundle, BlockRepo,
    BlockRepoLive, Database, DatabaseConnection, DbErr, EngineRepo, EngineRepoLive, LayerRepo,
    LayerRepoLive, ModuleRepo, ModuleRepoLive, ProfileRepo, ProfileRepoLive, RigRepo,
    RigRepoLive, SongRepo, SongRepoLive, StorageError, StorageResult,
};
use std::sync::Arc;

pub async fn bootstrap_in_memory_controller_async() -> Result<SignalController, StorageError> {
    let db = Database::connect("sqlite::memory:").await?;
    let seeds = runtime_seed_bundle();

    let block_repo = BlockRepoLive::new(db.clone());
    block_repo.init_schema().await?;
    block_repo
        .reseed_defaults(&seeds.block_collections)
        .await?;

    let module_repo = ModuleRepoLive::new(db.clone());
    module_repo.init_schema().await?;
    module_repo
        .reseed_defaults(&seeds.module_collections)
        .await?;

    let layer_repo = LayerRepoLive::new(db.clone());
    layer_repo.init_schema().await?;
    for layer in seeds.layers {
        layer_repo.save_layer(&layer).await?;
    }

    let engine_repo = EngineRepoLive::new(db.clone());
    engine_repo.init_schema().await?;
    for engine in seeds.engines {
        engine_repo.save_engine(&engine).await?;
    }

    let rig_repo = RigRepoLive::new(db.clone());
    rig_repo.init_schema().await?;
    for rig in seeds.rigs {
        rig_repo.save_rig(&rig).await?;
    }

    let profile_repo = ProfileRepoLive::new(db.clone());
    profile_repo.init_schema().await?;
    for profile in seeds.profiles {
        profile_repo.save_profile(&profile).await?;
    }

    let song_repo = SongRepoLive::new(db);
    song_repo.init_schema().await?;
    for song in seeds.songs {
        song_repo.save_song(&song).await?;
    }

    let service = Arc::new(SignalLive::new(
        Arc::new(block_repo),
        Arc::new(module_repo),
        Arc::new(layer_repo),
        Arc::new(engine_repo),
        Arc::new(rig_repo),
        Arc::new(profile_repo),
        Arc::new(song_repo),
    ));
    Ok(SignalController::new(service))
}

pub fn bootstrap_in_memory_controller() -> SignalController {
    let runtime = tokio::runtime::Runtime::new().expect("failed to build tokio runtime");
    let db = runtime.block_on(async {
        let seeds = runtime_seed_bundle();
        let db = Database::connect("sqlite::memory:")
            .await
            .expect("failed to connect in-memory sqlite");

        let block_repo = BlockRepoLive::new(db.clone());
        block_repo
            .init_schema()
            .await
            .expect("failed to initialize block schema");
        block_repo
            .reseed_defaults(&seeds.block_collections)
            .await
            .expect("failed to reseed block collections");

        let module_repo = ModuleRepoLive::new(db.clone());
        module_repo
            .init_schema()
            .await
            .expect("failed to initialize module schema");
        module_repo
            .reseed_defaults(&seeds.module_collections)
            .await
            .expect("failed to reseed module collections");

        let layer_repo = LayerRepoLive::new(db.clone());
        layer_repo
            .init_schema()
            .await
            .expect("failed to initialize layer schema");
        for layer in seeds.layers {
            layer_repo
                .save_layer(&layer)
                .await
                .expect("failed to seed layer");
        }

        let engine_repo = EngineRepoLive::new(db.clone());
        engine_repo
            .init_schema()
            .await
            .expect("failed to initialize engine schema");
        for engine in seeds.engines {
            engine_repo
                .save_engine(&engine)
                .await
                .expect("failed to seed engine");
        }

        let rig_repo = RigRepoLive::new(db.clone());
        rig_repo
            .init_schema()
            .await
            .expect("failed to initialize rig schema");
        for rig in seeds.rigs {
            rig_repo.save_rig(&rig).await.expect("failed to seed rig");
        }

        let profile_repo = ProfileRepoLive::new(db.clone());
        profile_repo
            .init_schema()
            .await
            .expect("failed to initialize profile schema");
        for profile in seeds.profiles {
            profile_repo
                .save_profile(&profile)
                .await
                .expect("failed to seed profile");
        }

        let song_repo = SongRepoLive::new(db.clone());
        song_repo
            .init_schema()
            .await
            .expect("failed to initialize song schema");
        for song in seeds.songs {
            song_repo
                .save_song(&song)
                .await
                .expect("failed to seed song");
        }

        db
    });
    let service = Arc::new(SignalLive::from_db(db));
    SignalController::new(service)
}
