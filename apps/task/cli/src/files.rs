//! `task files …` — the Files RPC surface (issue #259, ADR 0001):
//! turn a folder into a File Root, browse it, read a file's version
//! chain, checkpoint on demand. Talks to the org's `FilesService` over
//! vox — remote server or embedded in-process backend alike, exactly
//! like `task timer …` (see `establish_for_url`).
//!
//! Issue #261 adds the curated verbs — `task files version …` (Named
//! Versions), `task files project-version …` (Project Versions), and
//! `task files gc` (the Vault-protected sweep). Those entities are
//! vault pages, so they are equally editable in a text editor; the CLI
//! is the path that also validates the reference against the store.

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
    Checkpoint {
        root_id: uuid::Uuid,
        /// Defaults to "checkpoint now".
        #[arg(long)]
        message: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Named Versions — curated labels on top of the automatic chain
    /// ("v3 for client"). Vault entities, not store constructs.
    #[command(subcommand)]
    Version(FilesVersionCmd),
    /// Project Versions — whole-project iterations of one root,
    /// auto-numbered, with the folder name never changing.
    #[command(subcommand)]
    ProjectVersion(FilesProjectVersionCmd),
    /// Sweep a root's version store. Everything the Vault references —
    /// Named Versions, Project Version starts — is immortal.
    Gc {
        root_id: uuid::Uuid,
        /// Refuse to sweep anything written in the last N seconds
        /// (the concurrent-writer guard). Defaults to 60.
        #[arg(long)]
        keep_newer_secs: Option<u64>,
        #[arg(long)]
        json: bool,
    },
}

#[derive(Subcommand)]
pub(crate) enum FilesVersionCmd {
    /// Name a checkpoint as a deliverable.
    Name {
        root_id: uuid::Uuid,
        /// Hex commit id — the full id, or any unambiguous prefix
        /// (`task files chain` prints the first twelve characters).
        commit_id: String,
        name: String,
        #[arg(long)]
        json: bool,
    },
    /// Every Named Version, newest first.
    List {
        /// Limit to one root.
        #[arg(long)]
        root_id: Option<uuid::Uuid>,
        #[arg(long)]
        json: bool,
    },
    /// What a Named Version points at right now — the resolution a
    /// share link targeting it performs.
    Resolve {
        id: uuid::Uuid,
        #[arg(long)]
        json: bool,
    },
    /// Drop a Named Version's curation. The automatic chain is
    /// untouched; its content stops being immortal at the next `gc`.
    Remove { id: uuid::Uuid },
}

#[derive(Subcommand)]
pub(crate) enum FilesProjectVersionCmd {
    /// Start the next Project Version of a root, from its current
    /// checkpoint head.
    Start {
        root_id: uuid::Uuid,
        #[arg(long)]
        label: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Every Project Version of a root, oldest first.
    List {
        root_id: uuid::Uuid,
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
                    let named = if entry.names.is_empty() {
                        String::new()
                    } else {
                        format!("  [{}]", entry.names.join(", "))
                    };
                    println!(
                        "{}  {}{}{}",
                        short(&entry.commit_id),
                        entry.path,
                        renamed,
                        named
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
                    "{}  {} ({} paths changed)",
                    short(&info.commit_id),
                    info.description,
                    info.changed_paths.len()
                );
            }
        }
        FilesCmd::Version(FilesVersionCmd::Name {
            root_id,
            commit_id,
            name,
            json,
        }) => {
            let named = client
                .name_version(root_id, commit_id, name)
                .await
                .map_err(|e| eyre::eyre!("name_version: {e}"))?;
            if json {
                println!("{}", serde_json::to_string_pretty(&named)?);
            } else {
                println!(
                    "{}  {}  {}  ({})",
                    named.id,
                    short(&named.commit_id),
                    named.name,
                    named.path
                );
            }
        }
        FilesCmd::Version(FilesVersionCmd::List { root_id, json }) => {
            let versions = client
                .list_named_versions(root_id)
                .await
                .map_err(|e| eyre::eyre!("list_named_versions: {e}"))?;
            if json {
                println!("{}", serde_json::to_string_pretty(&versions)?);
            } else {
                for v in versions {
                    println!("{}  {}  {}", v.id, short(&v.commit_id), v.name);
                }
            }
        }
        FilesCmd::Version(FilesVersionCmd::Resolve { id, json }) => {
            let target = client
                .resolve_named_version(id)
                .await
                .map_err(|e| eyre::eyre!("resolve_named_version: {e}"))?;
            if json {
                println!("{}", serde_json::to_string_pretty(&target)?);
            } else {
                println!(
                    "root {}  change {}  commit {}",
                    target.root_id,
                    short(&target.change_id),
                    target.commit_id
                );
            }
        }
        FilesCmd::Version(FilesVersionCmd::Remove { id }) => {
            client
                .unname_version(id)
                .await
                .map_err(|e| eyre::eyre!("unname_version: {e}"))?;
            println!("removed {id}");
        }
        FilesCmd::ProjectVersion(FilesProjectVersionCmd::Start {
            root_id,
            label,
            json,
        }) => {
            let pv = client
                .start_project_version(root_id, label)
                .await
                .map_err(|e| eyre::eyre!("start_project_version: {e}"))?;
            if json {
                println!("{}", serde_json::to_string_pretty(&pv)?);
            } else {
                println!("v{}{}  ({})", pv.number, label_suffix(&pv.label), pv.path);
            }
        }
        FilesCmd::ProjectVersion(FilesProjectVersionCmd::List { root_id, json }) => {
            let versions = client
                .list_project_versions(root_id)
                .await
                .map_err(|e| eyre::eyre!("list_project_versions: {e}"))?;
            if json {
                println!("{}", serde_json::to_string_pretty(&versions)?);
            } else {
                for v in versions {
                    println!(
                        "v{}{}  {}  {}",
                        v.number,
                        label_suffix(&v.label),
                        short(&v.commit_id),
                        v.id
                    );
                }
            }
        }
        FilesCmd::Gc {
            root_id,
            keep_newer_secs,
            json,
        } => {
            let report = client
                .gc_root(root_id, keep_newer_secs)
                .await
                .map_err(|e| eyre::eyre!("gc_root: {e}"))?;
            if json {
                println!("{}", serde_json::to_string_pretty(&report)?);
            } else {
                println!(
                    "{} objects, {} manifests swept; {} vault-protected commits",
                    report.objects_swept, report.manifests_swept, report.protected_commits
                );
            }
        }
    }
    Ok(())
}

/// Hex ids are long and only their prefix is ever typed back.
fn short(hex: &str) -> &str {
    &hex[..12.min(hex.len())]
}

fn label_suffix(label: &Option<String>) -> String {
    label
        .as_deref()
        .map(|l| format!(" — {l}"))
        .unwrap_or_default()
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
