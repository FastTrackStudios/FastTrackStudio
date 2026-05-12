//! Server-only persistence layer.
//!
//! Re-exports the SeaORM bits emitted by the `EntityToModels` macro on
//! [`example_proto::Model`] and bundles the migration definitions.
//! Anything that needs to read or write the `examples` table — the
//! axum server, a CLI tool, a migration binary — depends on this
//! crate. Wasm clients never see it.

pub use example_proto::Entity as ExampleEntity;
pub use example_proto::{
    ActiveModel as ExampleActiveModel, Column as ExampleColumn, Model as ExampleModel,
    Relation as ExampleRelation,
};
// crudcrate-generated repo + storage glue:
pub use example_proto::{
    Example, ExampleCreate, ExampleList, ExampleRepo, ExampleRepoStorage, ExampleUpdate,
};

pub mod migrations;

pub use migrations::Migrator;
