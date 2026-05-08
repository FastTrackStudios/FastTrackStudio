use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        // For fresh databases, the column is created by
        // `create_table_from_entity` in the initial migration when the
        // entity already declares the field, so this ADD COLUMN is a
        // no-op (returns duplicate-column, treated as success). For
        // pre-existing databases that were migrated before the field
        // was added, this migration adds the missing column.
        let result = manager
            .alter_table(
                Table::alter()
                    .table(People::Table)
                    .add_column(
                        ColumnDef::new(People::Properties)
                            .json()
                            .not_null()
                            .default("{}"),
                    )
                    .to_owned(),
            )
            .await;
        match result {
            Ok(()) => Ok(()),
            Err(err) if is_duplicate_column(&err) => Ok(()),
            Err(err) => Err(err),
        }
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .alter_table(
                Table::alter()
                    .table(People::Table)
                    .drop_column(People::Properties)
                    .to_owned(),
            )
            .await?;
        Ok(())
    }
}

fn is_duplicate_column(err: &DbErr) -> bool {
    let message = err.to_string().to_ascii_lowercase();
    message.contains("duplicate column")
}

#[derive(DeriveIden)]
enum People {
    Table,
    Properties,
}
