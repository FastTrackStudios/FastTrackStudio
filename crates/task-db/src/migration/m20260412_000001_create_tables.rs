use sea_orm::Schema;
use sea_orm_migration::prelude::*;
use task_core::{calendar_event, client, expense, project, revenue, task};

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        let schema = Schema::new(manager.get_database_backend());

        // ── Projects ────────────────────────────────────────────────
        manager
            .create_table(
                schema
                    .create_table_from_entity(project::Entity)
                    .if_not_exists()
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_projects_title")
                    .table(Projects::Table)
                    .col(project::Column::Title)
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_projects_status")
                    .table(Projects::Table)
                    .col(project::Column::Status)
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_projects_organization")
                    .table(Projects::Table)
                    .col(project::Column::Organization)
                    .to_owned(),
            )
            .await?;

        // ── Tasks ───────────────────────────────────────────────────
        manager
            .create_table(
                schema
                    .create_table_from_entity(task::Entity)
                    .if_not_exists()
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_tasks_title")
                    .table(Tasks::Table)
                    .col(task::Column::Title)
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_tasks_status")
                    .table(Tasks::Table)
                    .col(task::Column::Status)
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_tasks_assignee")
                    .table(Tasks::Table)
                    .col(task::Column::Assignee)
                    .to_owned(),
            )
            .await?;

        // ── Clients ─────────────────────────────────────────────────
        manager
            .create_table(
                schema
                    .create_table_from_entity(client::Entity)
                    .if_not_exists()
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_clients_name")
                    .table(Clients::Table)
                    .col(client::Column::Name)
                    .to_owned(),
            )
            .await?;

        // ── Expenses ────────────────────────────────────────────────
        manager
            .create_table(
                schema
                    .create_table_from_entity(expense::Entity)
                    .if_not_exists()
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_expenses_date")
                    .table(Expenses::Table)
                    .col(expense::Column::Date)
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_expenses_status")
                    .table(Expenses::Table)
                    .col(expense::Column::Status)
                    .to_owned(),
            )
            .await?;

        // ── Revenues ────────────────────────────────────────────────
        manager
            .create_table(
                schema
                    .create_table_from_entity(revenue::Entity)
                    .if_not_exists()
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_revenues_date")
                    .table(Revenues::Table)
                    .col(revenue::Column::Date)
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_revenues_invoice_id")
                    .table(Revenues::Table)
                    .col(revenue::Column::InvoiceId)
                    .to_owned(),
            )
            .await?;

        // ── Calendar events ────────────────────────────────────────
        manager
            .create_table(
                schema
                    .create_table_from_entity(calendar_event::Entity)
                    .if_not_exists()
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_calendar_events_start")
                    .table(CalendarEvents::Table)
                    .col(calendar_event::Column::Start)
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_calendar_events_external")
                    .table(CalendarEvents::Table)
                    .col(calendar_event::Column::ExternalSource)
                    .col(calendar_event::Column::ExternalId)
                    .to_owned(),
            )
            .await?;

        // ── Comments ────────────────────────────────────────────────
        manager
            .create_table(
                Table::create()
                    .table(Comments::Table)
                    .if_not_exists()
                    .col(ColumnDef::new(Comments::Id).uuid().not_null().primary_key())
                    .col(ColumnDef::new(Comments::EntityId).uuid().not_null())
                    .col(
                        ColumnDef::new(Comments::EntityType)
                            .string_len(50)
                            .not_null(),
                    )
                    .col(ColumnDef::new(Comments::Author).string_len(100).not_null())
                    .col(ColumnDef::new(Comments::Body).text().not_null())
                    .col(ColumnDef::new(Comments::TimeStart).double())
                    .col(ColumnDef::new(Comments::TimeEnd).double())
                    .col(ColumnDef::new(Comments::ReplyTo).uuid())
                    .col(
                        ColumnDef::new(Comments::Resolved)
                            .boolean()
                            .not_null()
                            .default(false),
                    )
                    .col(ColumnDef::new(Comments::ResolvedBy).string_len(100))
                    .col(
                        ColumnDef::new(Comments::Mentions)
                            .json()
                            .not_null()
                            .default("[]"),
                    )
                    .col(ColumnDef::new(Comments::ExternalId).string_len(255))
                    .col(ColumnDef::new(Comments::DeletedAt).timestamp_with_time_zone())
                    .col(
                        ColumnDef::new(Comments::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(Comments::UpdatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_comments_entity")
                    .table(Comments::Table)
                    .col(Comments::EntityId)
                    .col(Comments::EntityType)
                    .to_owned(),
            )
            .await?;

        // ── Task Relations ──────────────────────────────────────────
        manager
            .create_table(
                Table::create()
                    .table(TaskRelations::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(TaskRelations::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(TaskRelations::IssueId).uuid().not_null())
                    .col(
                        ColumnDef::new(TaskRelations::RelatedIssueId)
                            .uuid()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(TaskRelations::RelationType)
                            .string_len(50)
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(TaskRelations::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .to_owned(),
            )
            .await?;

        // ── Reactions ───────────────────────────────────────────────
        manager
            .create_table(
                Table::create()
                    .table(Reactions::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(Reactions::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(Reactions::EntityId).uuid().not_null())
                    .col(
                        ColumnDef::new(Reactions::EntityType)
                            .string_len(50)
                            .not_null(),
                    )
                    .col(ColumnDef::new(Reactions::Emoji).string_len(10).not_null())
                    .col(ColumnDef::new(Reactions::User).string_len(100).not_null())
                    .col(
                        ColumnDef::new(Reactions::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .to_owned(),
            )
            .await?;

        // ── Activities ──────────────────────────────────────────────
        manager
            .create_table(
                Table::create()
                    .table(Activities::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(Activities::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(Activities::EntityId).uuid().not_null())
                    .col(
                        ColumnDef::new(Activities::EntityType)
                            .string_len(50)
                            .not_null(),
                    )
                    .col(ColumnDef::new(Activities::Verb).string_len(50).not_null())
                    .col(ColumnDef::new(Activities::Field).string_len(100))
                    .col(ColumnDef::new(Activities::OldValue).text())
                    .col(ColumnDef::new(Activities::NewValue).text())
                    .col(ColumnDef::new(Activities::Actor).string_len(100))
                    .col(
                        ColumnDef::new(Activities::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_activities_entity")
                    .table(Activities::Table)
                    .col(Activities::EntityId)
                    .col(Activities::EntityType)
                    .to_owned(),
            )
            .await?;

        // ── Notifications ───────────────────────────────────────────
        manager
            .create_table(
                Table::create()
                    .table(Notifications::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(Notifications::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(
                        ColumnDef::new(Notifications::Recipient)
                            .string_len(100)
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(Notifications::Kind)
                            .string_len(50)
                            .not_null(),
                    )
                    .col(ColumnDef::new(Notifications::Message).text().not_null())
                    .col(ColumnDef::new(Notifications::Actor).string_len(100))
                    .col(ColumnDef::new(Notifications::EntityId).uuid())
                    .col(ColumnDef::new(Notifications::EntityType).string_len(50))
                    .col(ColumnDef::new(Notifications::Project).string_len(255))
                    .col(ColumnDef::new(Notifications::ReadAt).timestamp_with_time_zone())
                    .col(ColumnDef::new(Notifications::SnoozedTill).timestamp_with_time_zone())
                    .col(
                        ColumnDef::new(Notifications::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_notifications_recipient")
                    .table(Notifications::Table)
                    .col(Notifications::Recipient)
                    .col(Notifications::ReadAt)
                    .to_owned(),
            )
            .await?;

        // ── Saved Views ─────────────────────────────────────────────
        manager
            .create_table(
                Table::create()
                    .table(SavedViews::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(SavedViews::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(SavedViews::Title).string_len(255).not_null())
                    .col(ColumnDef::new(SavedViews::Description).text())
                    .col(ColumnDef::new(SavedViews::Project).string_len(255))
                    .col(
                        ColumnDef::new(SavedViews::Filters)
                            .json()
                            .not_null()
                            .default("{}"),
                    )
                    .col(
                        ColumnDef::new(SavedViews::Display)
                            .json()
                            .not_null()
                            .default("{}"),
                    )
                    .col(ColumnDef::new(SavedViews::CreatedBy).string_len(100))
                    .col(
                        ColumnDef::new(SavedViews::IsShared)
                            .boolean()
                            .not_null()
                            .default(false),
                    )
                    .col(ColumnDef::new(SavedViews::SortOrder).double())
                    .col(
                        ColumnDef::new(SavedViews::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(SavedViews::UpdatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .to_owned(),
            )
            .await?;

        // ── Cycles ──────────────────────────────────────────────────
        manager
            .create_table(
                Table::create()
                    .table(Cycles::Table)
                    .if_not_exists()
                    .col(ColumnDef::new(Cycles::Id).uuid().not_null().primary_key())
                    .col(ColumnDef::new(Cycles::Title).string_len(255).not_null())
                    .col(ColumnDef::new(Cycles::Description).text())
                    .col(ColumnDef::new(Cycles::Project).string_len(255).not_null())
                    .col(
                        ColumnDef::new(Cycles::Status)
                            .string_len(50)
                            .not_null()
                            .default("Planned"),
                    )
                    .col(ColumnDef::new(Cycles::OwnedBy).string_len(100))
                    .col(ColumnDef::new(Cycles::StartDate).date())
                    .col(ColumnDef::new(Cycles::EndDate).date())
                    .col(
                        ColumnDef::new(Cycles::Tasks)
                            .json()
                            .not_null()
                            .default("[]"),
                    )
                    .col(ColumnDef::new(Cycles::SortOrder).double())
                    .col(
                        ColumnDef::new(Cycles::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(Cycles::UpdatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .to_owned(),
            )
            .await?;

        Ok(())
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(Cycles::Table).to_owned())
            .await?;
        manager
            .drop_table(Table::drop().table(SavedViews::Table).to_owned())
            .await?;
        manager
            .drop_table(Table::drop().table(Notifications::Table).to_owned())
            .await?;
        manager
            .drop_table(Table::drop().table(Activities::Table).to_owned())
            .await?;
        manager
            .drop_table(Table::drop().table(Reactions::Table).to_owned())
            .await?;
        manager
            .drop_table(Table::drop().table(TaskRelations::Table).to_owned())
            .await?;
        manager
            .drop_table(Table::drop().table(Comments::Table).to_owned())
            .await?;
        manager
            .drop_table(Table::drop().table(CalendarEvents::Table).to_owned())
            .await?;
        manager
            .drop_table(Table::drop().table(Revenues::Table).to_owned())
            .await?;
        manager
            .drop_table(Table::drop().table(Expenses::Table).to_owned())
            .await?;
        manager
            .drop_table(Table::drop().table(Clients::Table).to_owned())
            .await?;
        manager
            .drop_table(Table::drop().table(Tasks::Table).to_owned())
            .await?;
        manager
            .drop_table(Table::drop().table(Projects::Table).to_owned())
            .await?;
        Ok(())
    }
}

// ── Table identifiers ───────────────────────────────────────────────────────

#[derive(DeriveIden)]
enum Projects {
    Table,
}

#[derive(DeriveIden)]
enum Tasks {
    Table,
}

#[derive(DeriveIden)]
enum Clients {
    Table,
}

#[derive(DeriveIden)]
enum Expenses {
    Table,
}

#[derive(DeriveIden)]
enum Revenues {
    Table,
}

#[derive(DeriveIden)]
enum CalendarEvents {
    Table,
}

#[derive(DeriveIden)]
enum Comments {
    Table,
    Id,
    EntityId,
    EntityType,
    Author,
    Body,
    TimeStart,
    TimeEnd,
    ReplyTo,
    Resolved,
    ResolvedBy,
    Mentions,
    ExternalId,
    DeletedAt,
    CreatedAt,
    UpdatedAt,
}

#[derive(DeriveIden)]
enum TaskRelations {
    Table,
    Id,
    IssueId,
    RelatedIssueId,
    RelationType,
    CreatedAt,
}

#[derive(DeriveIden)]
enum Reactions {
    Table,
    Id,
    EntityId,
    EntityType,
    Emoji,
    User,
    CreatedAt,
}

#[derive(DeriveIden)]
enum Activities {
    Table,
    Id,
    EntityId,
    EntityType,
    Verb,
    Field,
    OldValue,
    NewValue,
    Actor,
    CreatedAt,
}

#[derive(DeriveIden)]
enum Notifications {
    Table,
    Id,
    Recipient,
    Kind,
    Message,
    Actor,
    EntityId,
    EntityType,
    Project,
    ReadAt,
    SnoozedTill,
    CreatedAt,
}

#[derive(DeriveIden)]
enum SavedViews {
    Table,
    Id,
    Title,
    Description,
    Project,
    Filters,
    Display,
    CreatedBy,
    IsShared,
    SortOrder,
    CreatedAt,
    UpdatedAt,
}

#[derive(DeriveIden)]
enum Cycles {
    Table,
    Id,
    Title,
    Description,
    Project,
    Status,
    OwnedBy,
    StartDate,
    EndDate,
    Tasks,
    SortOrder,
    CreatedAt,
    UpdatedAt,
}
