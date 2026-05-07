#![allow(clippy::needless_lifetimes, clippy::manual_strip)]
#![allow(unused_imports)]

use std::path::{Path, PathBuf};

use clap::Subcommand;
use uuid::Uuid;

use crate::shared::{RemoteVoxConfig, remote_find_task};
use task_core::attachment::{Attachment, default_remote_path, label_from_path};
use task_core::service::{AttachmentService, AttachmentUploadRequest};

/// Owner reference: either a task reference resolved through the existing
/// `remote_find_task` helpers, or an explicit `<type, id>` pair for non-task
/// owners (project / comment / calendar_event / person).
#[derive(Debug, Clone)]
pub(crate) struct OwnerRef {
    pub owner_type: String,
    pub owner_id: Uuid,
}

#[derive(Subcommand)]
pub(crate) enum AttachmentCommands {
    /// Upload a file and attach it to an entity.
    Attach {
        /// Task reference (id/title/identifier) — resolved unless --type/--id used.
        reference: Option<String>,
        /// Local file to upload.
        file: PathBuf,
        /// Override the user-friendly label (default: file basename).
        #[arg(long)]
        label: Option<String>,
        /// Storage source. `nextcloud` (default) actually pushes bytes;
        /// `local`/`s3` only register metadata.
        #[arg(long, default_value = "nextcloud")]
        source: String,
        /// Provider-relative remote path. Defaults to
        /// `attachments/<owner_type>/<owner_id>/<basename>`.
        #[arg(long = "remote-path")]
        remote_path: Option<String>,
        /// MIME type override.
        #[arg(long)]
        mime: Option<String>,
        /// Owner type for non-task owners (project/comment/calendar_event/person).
        #[arg(long = "type")]
        owner_type: Option<String>,
        /// Owner id for non-task owners.
        #[arg(long = "id")]
        owner_id: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// List attachments hung off a given owner.
    List {
        /// Task reference (resolved unless --type/--id used).
        reference: Option<String>,
        #[arg(long = "type")]
        owner_type: Option<String>,
        #[arg(long = "id")]
        owner_id: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Download an attachment by id.
    Download {
        id: String,
        /// Output path. Defaults to the attachment's basename in the cwd.
        #[arg(long)]
        out: Option<PathBuf>,
    },
    /// Delete an attachment by id (drops the row + remote object if applicable).
    Delete { id: String },
}

async fn resolve_owner(
    remote: &RemoteVoxConfig,
    reference: Option<String>,
    owner_type: Option<String>,
    owner_id: Option<String>,
) -> eyre::Result<OwnerRef> {
    if let (Some(t), Some(id)) = (owner_type.as_ref(), owner_id.as_ref()) {
        let parsed =
            Uuid::parse_str(id).map_err(|err| eyre::eyre!("--id must be a UUID: {err}"))?;
        return Ok(OwnerRef {
            owner_type: t.clone(),
            owner_id: parsed,
        });
    }
    let Some(reference) = reference else {
        eyre::bail!("Provide a task <reference> or both --type and --id for non-task owners.");
    };
    let task = remote_find_task(remote, &reference).await?;
    Ok(OwnerRef {
        owner_type: "task".to_string(),
        owner_id: task.id,
    })
}

pub(crate) async fn run_remote_attachment_command(
    remote: &RemoteVoxConfig,
    actor: Option<&str>,
    command: AttachmentCommands,
) -> eyre::Result<()> {
    let client = remote.attachment().await?;
    match command {
        AttachmentCommands::Attach {
            reference,
            file,
            label,
            source,
            remote_path,
            mime,
            owner_type,
            owner_id,
            json,
        } => {
            let owner = resolve_owner(remote, reference, owner_type, owner_id).await?;
            let bytes = std::fs::read(&file)
                .map_err(|err| eyre::eyre!("read {}: {err}", file.display()))?;
            let basename = file
                .file_name()
                .and_then(|s| s.to_str())
                .map(|s| s.to_string())
                .unwrap_or_else(|| "attachment.bin".to_string());
            let path = remote_path.unwrap_or_else(|| {
                default_remote_path(&owner.owner_type, owner.owner_id, &basename)
            });
            let request = AttachmentUploadRequest {
                owner_type: owner.owner_type,
                owner_id: owner.owner_id,
                path,
                label,
                mime,
                bytes,
                uploader: actor.map(|s| s.to_string()),
                source,
            };
            let attachment = client
                .upload(request)
                .await
                .map_err(|err| eyre::eyre!("upload failed: {err}"))?;
            if json {
                println!("{}", serde_json::to_string_pretty(&attachment)?);
            } else {
                println!(
                    "Attached {} ({}) -> {}",
                    attachment.label.as_deref().unwrap_or("(no label)"),
                    attachment.id,
                    attachment.path
                );
            }
        }
        AttachmentCommands::List {
            reference,
            owner_type,
            owner_id,
            json,
        } => {
            let owner = resolve_owner(remote, reference, owner_type, owner_id).await?;
            let rows = client
                .list_for_entity(owner.owner_type.clone(), owner.owner_id)
                .await
                .map_err(|err| eyre::eyre!("list failed: {err}"))?;
            if json {
                println!("{}", serde_json::to_string_pretty(&rows)?);
            } else {
                print_attachments_table(&rows);
            }
        }
        AttachmentCommands::Download { id, out } => {
            let parsed = Uuid::parse_str(&id).map_err(|err| eyre::eyre!("invalid id: {err}"))?;
            let resp = client
                .download(parsed)
                .await
                .map_err(|err| eyre::eyre!("download failed: {err}"))?;
            let target = out.unwrap_or_else(|| PathBuf::from(label_from_path(&resp.metadata.path)));
            std::fs::write(&target, &resp.bytes)
                .map_err(|err| eyre::eyre!("write {}: {err}", target.display()))?;
            println!(
                "Downloaded {} bytes -> {}",
                resp.bytes.len(),
                target.display()
            );
        }
        AttachmentCommands::Delete { id } => {
            let parsed = Uuid::parse_str(&id).map_err(|err| eyre::eyre!("invalid id: {err}"))?;
            client
                .delete(parsed)
                .await
                .map_err(|err| eyre::eyre!("delete failed: {err}"))?;
            println!("Deleted attachment {parsed}.");
        }
    }
    Ok(())
}

fn print_attachments_table(rows: &[Attachment]) {
    if rows.is_empty() {
        println!("(no attachments)");
        return;
    }
    println!(
        "{:<36}  {:<12}  {:<10}  {:<8}  path",
        "id", "source", "size", "mime"
    );
    for a in rows {
        let size = a
            .size_bytes
            .map(|n| format!("{n}"))
            .unwrap_or_else(|| "?".to_string());
        let mime = a.mime.as_deref().unwrap_or("?");
        println!(
            "{:<36}  {:<12}  {:<10}  {:<8}  {}",
            a.id, a.source, size, mime, a.path
        );
    }
}
