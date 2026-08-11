//! `task files …` — the Files RPC surface v1 (issue #259, ADR 0001):
//! turn a folder into a File Root, browse it, read a file's version
//! chain, checkpoint on demand. Talks to the org's `FilesService` over
//! vox — remote server or embedded in-process backend alike, exactly
//! like `task timer …` (see `establish_for_url`).

use clap::Subcommand;
use files_proto::{FilesServiceClient, RootFlavor};

use crate::establish_for_url;
use crate::resolve_org_vox_url;

#[derive(Subcommand)]
pub(crate) enum FilesCmd {
    /// File Root CRUD (create / list / get).
    #[command(subcommand)]
    Root(FilesRootCmd),
    /// Root-scoped directory listing — the marker file and version
    /// store are hidden. Empty `subpath` lists the root itself.
    Browse {
        root_id: uuid::Uuid,
        #[arg(default_value = "")]
        subpath: String,
        #[arg(long)]
        json: bool,
    },
    /// Rootless directory listing ("Drive" browsing — loose files
    /// outside any root, per the glossary). Shows everything,
    /// including a root's own internals if `path` happens to be one.
    DriveBrowse {
        path: String,
        #[arg(long)]
        json: bool,
    },
    /// A file's version chain (newest first), following recorded
    /// renames.
    Chain {
        root_id: uuid::Uuid,
        path: String,
        #[arg(long)]
        json: bool,
    },
    /// Certify a Session checkpoint right now: full-scan the root's
    /// live tree, diff against the current head, write one commit.
    /// Ends the root's open session.
    Checkpoint {
        root_id: uuid::Uuid,
        /// Defaults to "checkpoint now".
        #[arg(long)]
        message: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// The root's auto-snapshots (newest first) — the ephemeral
    /// mid-session captures. Never version-chain entries.
    Snapshots {
        root_id: uuid::Uuid,
        #[arg(long)]
        json: bool,
    },
    /// Tell the cadence engine these root-relative paths were just
    /// written — what a watcher does, for a machine the server can't
    /// watch itself.
    Hint {
        root_id: uuid::Uuid,
        /// Root-relative paths.
        #[arg(required = true)]
        paths: Vec<String>,
    },
    /// The root's Ignore set (patterns neither versioned nor synced).
    #[command(subcommand)]
    Ignore(FilesIgnoreCmd),
}

#[derive(Subcommand)]
pub(crate) enum FilesIgnoreCmd {
    /// Show the root's Ignore set.
    Show {
        root_id: uuid::Uuid,
        #[arg(long)]
        json: bool,
    },
    /// Replace the root's Ignore set with these patterns.
    Set {
        root_id: uuid::Uuid,
        #[arg(required = true)]
        patterns: Vec<String>,
        #[arg(long)]
        json: bool,
    },
}

#[derive(Subcommand)]
pub(crate) enum FilesRootCmd {
    /// Turn an existing folder into a File Root.
    Create {
        path: String,
        /// Defaults to the folder's own name.
        #[arg(long)]
        name: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Every File Root known to this org.
    List {
        #[arg(long)]
        json: bool,
    },
    /// One root by id.
    Get {
        id: uuid::Uuid,
        #[arg(long)]
        json: bool,
    },
}

pub(crate) async fn run_files(cmd: FilesCmd, org_override: Option<&str>) -> eyre::Result<()> {
    let slug = crate::resolve_slug(org_override)?;
    let vox_url = resolve_org_vox_url(None, &slug);
    let client: FilesServiceClient = establish_for_url(&vox_url).await?;

    match cmd {
        FilesCmd::Root(FilesRootCmd::Create { path, name, json }) => {
            let name = name.unwrap_or_else(|| {
                std::path::Path::new(&path)
                    .file_name()
                    .map(|n| n.to_string_lossy().into_owned())
                    .unwrap_or_else(|| path.clone())
            });
            let root = client
                .create_root(path, name, RootFlavor::Media)
                .await
                .map_err(|e| eyre::eyre!("create_root: {e}"))?;
            if json {
                println!("{}", serde_json::to_string_pretty(&root)?);
            } else {
                println!("{} ({})", root.id, root.path);
            }
        }
        FilesCmd::Root(FilesRootCmd::List { json }) => {
            let roots = client
                .list_roots()
                .await
                .map_err(|e| eyre::eyre!("list_roots: {e}"))?;
            if json {
                println!("{}", serde_json::to_string_pretty(&roots)?);
            } else {
                for r in roots {
                    println!("{}  {}  {}", r.id, r.name, r.path);
                }
            }
        }
        FilesCmd::Root(FilesRootCmd::Get { id, json }) => {
            let root = client
                .get_root(id)
                .await
                .map_err(|e| eyre::eyre!("get_root: {e}"))?;
            if json {
                println!("{}", serde_json::to_string_pretty(&root)?);
            } else {
                println!("{} ({})", root.name, root.path);
            }
        }
        FilesCmd::Browse {
            root_id,
            subpath,
            json,
        } => {
            let entries = client
                .browse(root_id, subpath)
                .await
                .map_err(|e| eyre::eyre!("browse: {e}"))?;
            print_entries(&entries, json)?;
        }
        FilesCmd::DriveBrowse { path, json } => {
            let entries = client
                .drive_browse(path)
                .await
                .map_err(|e| eyre::eyre!("drive_browse: {e}"))?;
            print_entries(&entries, json)?;
        }
        FilesCmd::Chain {
            root_id,
            path,
            json,
        } => {
            let chain = client
                .chain(root_id, path)
                .await
                .map_err(|e| eyre::eyre!("chain: {e}"))?;
            if json {
                println!("{}", serde_json::to_string_pretty(&chain)?);
            } else {
                for entry in chain {
                    let renamed = entry
                        .renamed_from
                        .map(|p| format!(" (renamed from {p})"))
                        .unwrap_or_default();
                    println!(
                        "{}  {}{}",
                        &entry.commit_id[..12.min(entry.commit_id.len())],
                        entry.path,
                        renamed
                    );
                }
            }
        }
        FilesCmd::Checkpoint {
            root_id,
            message,
            json,
        } => {
            let info = client
                .checkpoint_now(root_id, message)
                .await
                .map_err(|e| eyre::eyre!("checkpoint_now: {e}"))?;
            if json {
                println!("{}", serde_json::to_string_pretty(&info)?);
            } else {
                println!(
                    "{}  {} ({} paths changed{}{})",
                    &info.commit_id[..12.min(info.commit_id.len())],
                    info.description,
                    info.changed_paths.len(),
                    if info.requeued_paths.is_empty() {
                        String::new()
                    } else {
                        format!(", {} requeued", info.requeued_paths.len())
                    },
                    if info.save_points.is_empty() {
                        String::new()
                    } else {
                        format!(", {} save points", info.save_points.len())
                    },
                );
            }
        }
        FilesCmd::Snapshots { root_id, json } => {
            let snapshots = client
                .snapshots(root_id)
                .await
                .map_err(|e| eyre::eyre!("snapshots: {e}"))?;
            if json {
                println!("{}", serde_json::to_string_pretty(&snapshots)?);
            } else {
                for s in snapshots {
                    let saves: Vec<&str> = s.save_points.iter().map(|p| p.path.as_str()).collect();
                    println!(
                        "{}  {}  {} paths{}",
                        &s.snapshot_id[..12.min(s.snapshot_id.len())],
                        s.at.to_rfc3339(),
                        s.changed_paths.len(),
                        if saves.is_empty() {
                            String::new()
                        } else {
                            format!("  save points: {}", saves.join(", "))
                        },
                    );
                }
            }
        }
        FilesCmd::Hint { root_id, paths } => {
            let accepted = client
                .hint_activity(root_id, paths)
                .await
                .map_err(|e| eyre::eyre!("hint_activity: {e}"))?;
            println!("{accepted} hints accepted (the rest are in the Ignore set)");
        }
        FilesCmd::Ignore(FilesIgnoreCmd::Show { root_id, json }) => {
            let patterns = client
                .ignore_set(root_id)
                .await
                .map_err(|e| eyre::eyre!("ignore_set: {e}"))?;
            print_patterns(&patterns, json)?;
        }
        FilesCmd::Ignore(FilesIgnoreCmd::Set {
            root_id,
            patterns,
            json,
        }) => {
            let stored = client
                .set_ignore_set(root_id, patterns)
                .await
                .map_err(|e| eyre::eyre!("set_ignore_set: {e}"))?;
            print_patterns(&stored, json)?;
        }
    }
    Ok(())
}

fn print_patterns(patterns: &[String], json: bool) -> eyre::Result<()> {
    if json {
        println!("{}", serde_json::to_string_pretty(patterns)?);
        return Ok(());
    }
    for p in patterns {
        println!("{p}");
    }
    Ok(())
}

fn print_entries(entries: &[files_proto::BrowseEntry], json: bool) -> eyre::Result<()> {
    if json {
        println!("{}", serde_json::to_string_pretty(entries)?);
        return Ok(());
    }
    for e in entries {
        let kind = if e.is_dir { "dir " } else { "file" };
        let size = e.size.map(|s| s.to_string()).unwrap_or_default();
        println!("{kind}  {size:>10}  {}", e.name);
    }
    Ok(())
}
