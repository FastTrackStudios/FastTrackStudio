//! Migrator over the auth tables. Drop-in for `MigratorTrait`.

use sea_orm_migration::prelude::*;

mod m1 {
    pub use crate::migration_m1::*;
}

pub struct Migrator;

#[async_trait::async_trait]
impl MigratorTrait for Migrator {
    fn migrations() -> Vec<Box<dyn MigrationTrait>> {
        vec![Box::new(m1::Migration)]
    }
}
