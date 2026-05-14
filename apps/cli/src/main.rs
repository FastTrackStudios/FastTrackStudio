//! `task` CLI — vertical-slice scaffold.
//!
//! Talks to a `task-server` over vox WebSocket. `task list` calls the
//! auto-generated `TaskRepoClient::list`; `task doctor` just prints
//! the resolved endpoint URL.
//!
//! Endpoint resolution (first match wins):
//! 1. `--server <url>` flag.
//! 2. `TASK_VOX_URL` env var (loaded from `.env` if present in CWD
//!    or any parent dir — see `dotenvy::dotenv()`). Named to avoid
//!    collision with the broader `TASK_SERVER` you may already
//!    have set for the prod box.
//! 3. `ws://127.0.0.1:9090/vox` default.

mod shared;

use std::collections::HashMap;

use clap::{Parser, Subcommand};
use project_crdt::{ProjectRepoLoro, TaskRepoLoro};
use project_proto::architect::Page;
use project_proto::{ProjectCreate, ProjectRepo, TaskCreate, TaskRepo, TaskUpdate};
use shared::{LiveSession, RemoteVoxConfig};
use uuid::Uuid;

#[derive(Parser)]
#[command(name = "task", about = "Task management CLI", version)]
struct Cli {
    /// Vox WebSocket URL (e.g. ws://127.0.0.1:9090/vox). Falls back
    /// to `TASK_VOX_URL` (loaded from .env) then to the localhost
    /// default.
    #[arg(
        long,
        env = "TASK_VOX_URL",
        default_value = "ws://127.0.0.1:9090/vox",
        global = true
    )]
    server: String,

    /// Architect Auth session token for remote vox.
    #[arg(long, env = "TASK_SESSION_TOKEN", global = true)]
    session_token: Option<String>,

    /// Organization id to route remote vox requests.
    #[arg(long, env = "TASK_ORGANIZATION_ID", global = true)]
    organization_id: Option<String>,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// List tasks grouped by project.
    List,
    /// Mark a task done (or undone). Triggers a live push to every
    /// subscribed peer through `WorkspaceSync`.
    SetDone {
        /// Task UUID (any unambiguous prefix accepted).
        task_id: String,
        /// Mark the task open again instead of done.
        #[arg(long)]
        undo: bool,
    },
    /// Create a new task inside a project (matched by name prefix).
    NewTask {
        /// Project name (or unique prefix).
        project: String,
        /// Task title.
        title: String,
    },
    /// Create a new project.
    NewProject {
        /// Project name.
        name: String,
    },
    /// Probe the configured vox endpoint.
    Doctor,
    /// Export the org vault to an Obsidian-shaped directory.
    /// Phase 9 — single vault for now (multi-vault servers
    /// follow); writes one `.md` per Page with frontmatter +
    /// rendered blocks, and folder prefixes per the vault's
    /// folder hierarchy.
    Export {
        /// Output directory. Created if missing; existing files
        /// are overwritten. Tested via the workspace's
        /// `markdown_roundtrip` integration test.
        #[arg(long)]
        out: std::path::PathBuf,
    },
    /// Import an Obsidian vault directory into the server's org
    /// vault. Walks `.md` files, parses each as a Page, creates
    /// folders for the path hierarchy. Idempotent at the byte
    /// level: re-running produces the same export.
    Import {
        /// Source directory.
        #[arg(long)]
        path: std::path::PathBuf,
    },
}

#[tokio::main]
async fn main() -> eyre::Result<()> {
    // Best-effort .env load before clap reads env. Missing file is
    // not an error — we just fall through to the hard-coded default.
    let _ = dotenvy::dotenv();
    let cli = Cli::parse();
    match cli.command {
        Commands::List => {
            let remote =
                RemoteVoxConfig::from_args(cli.server, cli.session_token, cli.organization_id)?;
            let session = LiveSession::open(&remote).await?;
            let project_repo = ProjectRepoLoro::new(&session.doc);
            let task_repo = TaskRepoLoro::new(&session.doc);
            let big_page = Page {
                index: 0,
                size: 1000,
            };
            let project_page = project_repo
                .list(big_page.clone(), None, None)
                .await
                .map_err(|e| eyre::eyre!("project list: {e}"))?;
            let task_page = task_repo
                .list(big_page, None, None)
                .await
                .map_err(|e| eyre::eyre!("task list: {e}"))?;

            let mut grouped: HashMap<Uuid, Vec<_>> = HashMap::new();
            for task in task_page.items {
                grouped.entry(task.project_id).or_default().push(task);
            }

            if project_page.items.is_empty() {
                println!("(no projects)");
            } else {
                let mut projects = project_page.items;
                projects.sort_by(|a, b| a.name.cmp(&b.name));
                for project in projects {
                    let tasks = grouped.remove(&project.id).unwrap_or_default();
                    println!("\n## {}  ({} tasks)", project.name, tasks.len());
                    for task in tasks {
                        let short_id = &task.id.to_string()[..8];
                        let mark = if task.done { "[x]" } else { "[ ]" };
                        println!("  [{short_id}] {mark} {}", task.title);
                    }
                }
                // Orphans — tasks pointing at projects we don't have
                // in the snapshot (would be a data bug, but print
                // rather than silently drop).
                for (project_id, tasks) in grouped {
                    println!(
                        "\n## (unknown project {project_id})  ({} tasks)",
                        tasks.len()
                    );
                    for task in tasks {
                        let short_id = &task.id.to_string()[..8];
                        let mark = if task.done { "[x]" } else { "[ ]" };
                        println!("  [{short_id}] {mark} {}", task.title);
                    }
                }
            }
        }
        Commands::SetDone { task_id, undo } => {
            let done = !undo;
            let remote =
                RemoteVoxConfig::from_args(cli.server, cli.session_token, cli.organization_id)?;
            let session = LiveSession::open(&remote).await?;
            let task_repo = TaskRepoLoro::new(&session.doc);
            let big_page = Page {
                index: 0,
                size: 1000,
            };
            // Resolve a UUID prefix → full id by scanning the local
            // snapshot. No round-trip needed; the doc is already
            // loaded.
            let id = if let Ok(id) = Uuid::parse_str(&task_id) {
                id
            } else {
                let page = task_repo
                    .list(big_page, None, None)
                    .await
                    .map_err(|e| eyre::eyre!("task list (for id resolve): {e}"))?;
                let prefix = task_id.to_lowercase();
                let matches: Vec<_> = page
                    .items
                    .iter()
                    .filter(|t| t.id.to_string().starts_with(&prefix))
                    .collect();
                match matches.as_slice() {
                    [] => return Err(eyre::eyre!("no task matched prefix {task_id:?}")),
                    [t] => t.id,
                    multi => {
                        return Err(eyre::eyre!(
                            "{} tasks matched prefix {task_id:?}; use a longer prefix",
                            multi.len()
                        ));
                    }
                }
            };
            let updated = task_repo
                .update(
                    id,
                    TaskUpdate {
                        done: Some(done),
                        ..Default::default()
                    },
                )
                .await
                .map_err(|e| eyre::eyre!("task update: {e}"))?;
            let mark = if updated.done { "done" } else { "open" };
            println!("{} {} → {mark}", updated.id, updated.title);
            session.flush().await?;
        }
        Commands::NewTask { project, title } => {
            let remote =
                RemoteVoxConfig::from_args(cli.server, cli.session_token, cli.organization_id)?;
            let session = LiveSession::open(&remote).await?;
            let project_repo = ProjectRepoLoro::new(&session.doc);
            let task_repo = TaskRepoLoro::new(&session.doc);
            let big_page = Page {
                index: 0,
                size: 1000,
            };
            let projects = project_repo
                .list(big_page, None, None)
                .await
                .map_err(|e| eyre::eyre!("project list: {e}"))?;
            // Exact match first; then case-insensitive prefix.
            let project_id = if let Some(p) = projects.items.iter().find(|p| p.name == project) {
                p.id
            } else {
                let needle = project.to_lowercase();
                let matches: Vec<_> = projects
                    .items
                    .iter()
                    .filter(|p| p.name.to_lowercase().starts_with(&needle))
                    .collect();
                match matches.as_slice() {
                    [] => return Err(eyre::eyre!("no project matched {project:?}")),
                    [p] => p.id,
                    multi => {
                        return Err(eyre::eyre!(
                            "{} projects matched {project:?}; be more specific",
                            multi.len()
                        ));
                    }
                }
            };
            let created = task_repo
                .create(TaskCreate {
                    project_id,
                    title: title.clone(),
                    done: false,
                })
                .await
                .map_err(|e| eyre::eyre!("task create: {e}"))?;
            println!("{} {}", created.id, created.title);
            session.flush().await?;
        }
        Commands::NewProject { name } => {
            let remote =
                RemoteVoxConfig::from_args(cli.server, cli.session_token, cli.organization_id)?;
            let session = LiveSession::open(&remote).await?;
            let project_repo = ProjectRepoLoro::new(&session.doc);
            let created = project_repo
                .create(ProjectCreate { name: name.clone() })
                .await
                .map_err(|e| eyre::eyre!("project create: {e}"))?;
            println!("{} {}", created.id, created.name);
            session.flush().await?;
        }
        Commands::Doctor => {
            let remote =
                RemoteVoxConfig::from_args(cli.server, cli.session_token, cli.organization_id)?;
            println!("Vox endpoint: {}", remote.display_url);
        }
        Commands::Export { out } => {
            let remote =
                RemoteVoxConfig::from_args(cli.server, cli.session_token, cli.organization_id)?;
            let session = LiveSession::open_doc(&remote, project_proto::DocId::org_vault()).await?;
            run_export(&session.doc, &out).await?;
            println!("Exported to {}", out.display());
        }
        Commands::Import { path } => {
            let remote =
                RemoteVoxConfig::from_args(cli.server, cli.session_token, cli.organization_id)?;
            let session = LiveSession::open_doc(&remote, project_proto::DocId::org_vault()).await?;
            run_import(&session.doc, &path).await?;
            session.flush().await?;
            println!("Imported {}", path.display());
        }
    }
    Ok(())
}

/// Walks the org-vault doc, renders to markdown, writes to `out`.
async fn run_export(
    doc: &std::sync::Arc<crdt::CrdtDoc>,
    out: &std::path::Path,
) -> eyre::Result<()> {
    use knowledge_crdt::{BlockRepoLoro, FolderRepoLoro, PageRepoLoro, VaultRepoLoro};
    use knowledge_proto::{BlockRepo, FolderRepo, PageRepo, VaultRepo};
    let big = Page {
        index: 0,
        size: 10_000,
    };
    let vault_repo = VaultRepoLoro::new(doc);
    let folder_repo = FolderRepoLoro::new(doc);
    let page_repo = PageRepoLoro::new(doc);
    let block_repo = BlockRepoLoro::new(doc);

    let vaults = vault_repo
        .list(big.clone(), None, None)
        .await
        .map_err(|e| eyre::eyre!("vault list: {e}"))?;
    let vault = vaults
        .items
        .into_iter()
        .next()
        .ok_or_else(|| eyre::eyre!("server has no vault to export"))?;
    let folders = folder_repo
        .list(big.clone(), None, None)
        .await
        .map_err(|e| eyre::eyre!("folder list: {e}"))?;
    let pages = page_repo
        .list(big.clone(), None, None)
        .await
        .map_err(|e| eyre::eyre!("page list: {e}"))?;
    let blocks = block_repo
        .list(big, None, None)
        .await
        .map_err(|e| eyre::eyre!("block list: {e}"))?;

    let plan =
        knowledge_proto::export::export_vault(&vault, &folders.items, &pages.items, &blocks.items);
    std::fs::create_dir_all(out)?;
    for file in plan.files {
        let dest = out.join(&file.relative_path);
        if let Some(parent) = dest.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(&dest, file.contents)?;
    }
    Ok(())
}

/// Walks `path`, parses every `.md`, and inserts the result into
/// the org-vault doc via the Knowledge repos.
async fn run_import(
    doc: &std::sync::Arc<crdt::CrdtDoc>,
    path: &std::path::Path,
) -> eyre::Result<()> {
    use knowledge_crdt::{BlockRepoLoro, FolderRepoLoro, PageRepoLoro, VaultRepoLoro};
    use knowledge_proto::export::{ImportFile, import_vault};
    use knowledge_proto::{
        BlockCreate, BlockRepo, FolderCreate, FolderRepo, PageCreate, PageRepo, VaultRepo,
    };

    let mut files: Vec<ImportFile> = Vec::new();
    for entry in walkdir::WalkDir::new(path) {
        let entry = entry?;
        if !entry.file_type().is_file() {
            continue;
        }
        let rel = entry
            .path()
            .strip_prefix(path)?
            .to_string_lossy()
            .replace('\\', "/");
        let contents = std::fs::read_to_string(entry.path())?;
        files.push(ImportFile {
            relative_path: rel,
            contents,
        });
    }
    let imported = import_vault("Imported", &files)?;
    // Insert into the org-vault doc. We don't reset the existing
    // vault — that lets users layer imports on top of running
    // state.
    let vault_repo = VaultRepoLoro::new(doc);
    let folder_repo = FolderRepoLoro::new(doc);
    let page_repo = PageRepoLoro::new(doc);
    let block_repo = BlockRepoLoro::new(doc);

    let vaults = vault_repo
        .list(Page { index: 0, size: 10 }, None, None)
        .await
        .map_err(|e| eyre::eyre!("vault list: {e}"))?;
    let target_vault_id = match vaults.items.into_iter().next() {
        Some(v) => v.id,
        None => {
            // No vault on server yet — create one.
            vault_repo
                .create(knowledge_proto::VaultCreate {
                    name: imported.vault.name.clone(),
                    root_path: None,
                    use_markdown_links: false,
                    new_link_format: "shortest".into(),
                    attachment_folder_path: "".into(),
                    default_view_mode: "source".into(),
                    config_json: imported.vault.config_json.clone(),
                })
                .await
                .map_err(|e| eyre::eyre!("vault create: {e}"))?
                .id
        }
    };

    // Folders parent-first.
    let mut folders = imported.folders;
    folders.sort_by_key(|f| f.path.matches('/').count());
    let mut folder_id_remap: std::collections::HashMap<uuid::Uuid, uuid::Uuid> =
        std::collections::HashMap::new();
    for f in folders {
        let parent_id = f
            .parent_id
            .and_then(|pid| folder_id_remap.get(&pid).copied());
        let new = folder_repo
            .create(FolderCreate {
                vault_id: target_vault_id,
                path: f.path.clone(),
                parent_id,
            })
            .await
            .map_err(|e| eyre::eyre!("folder: {e}"))?;
        folder_id_remap.insert(f.id, new.id);
    }
    let mut page_id_remap: std::collections::HashMap<uuid::Uuid, uuid::Uuid> =
        std::collections::HashMap::new();
    for p in imported.pages {
        let folder_id = p
            .folder_id
            .and_then(|fid| folder_id_remap.get(&fid).copied());
        let new = page_repo
            .create(PageCreate {
                vault_id: target_vault_id,
                folder_id,
                path: p.path.clone(),
                basename: p.basename.clone(),
                ext: p.ext.clone(),
                aliases: p.aliases.clone(),
                frontmatter_json: p.frontmatter_json.clone(),
                stat_ctime: p.stat_ctime,
                stat_mtime: p.stat_mtime,
                stat_size: p.stat_size,
                is_journal: p.is_journal,
                journal_day: p.journal_day.clone(),
                shadow_for_kind: p.shadow_for_kind.clone(),
                shadow_for_id: p.shadow_for_id,
            })
            .await
            .map_err(|e| eyre::eyre!("page: {e}"))?;
        page_id_remap.insert(p.id, new.id);
    }
    for b in imported.blocks {
        let Some(&page_id) = page_id_remap.get(&b.page_id) else {
            continue;
        };
        block_repo
            .create(BlockCreate {
                vault_id: target_vault_id,
                page_id,
                parent_block_id: None,
                sort_key: b.sort_key.clone(),
                kind: b.kind.clone(),
                content: b.content.clone(),
                heading_level: b.heading_level,
                list_ordered: b.list_ordered,
                list_task: b.list_task.clone(),
                code_lang: b.code_lang.clone(),
                callout_kind: b.callout_kind.clone(),
                callout_foldable: b.callout_foldable,
                properties_json: b.properties_json.clone(),
                obsidian_block_id: b.obsidian_block_id.clone(),
                collapsed: b.collapsed,
                refs_json: b.refs_json.clone(),
                canvas_node_json: b.canvas_node_json.clone(),
            })
            .await
            .map_err(|e| eyre::eyre!("block: {e}"))?;
    }
    Ok(())
}
