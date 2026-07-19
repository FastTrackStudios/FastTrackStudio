//! `task collection …` + `task song …` — headless library / setlist
//! seeding (W0.5).
//!
//! A song **Library**, a **Setlist**, a **Show**, and a **Playlist** are
//! the same primitive: an ordered list of [`NodeRef`] items served by
//! `CollectionService`. These verbs create such collections, populate and
//! reorder them, and inspect them — all without a GUI, so the W7 (Alan
//! Parsons import) and W8 (Days to Praise seed) pipelines can drive them.
//!
//! `task song add` is the composite verb: it builds a durable **Song
//! folder** with the `song` crate (a `song.md` index + a default
//! arrangement), attaches chart / pdf / audio files, and then registers
//! the song in a target collection as a `song:<slug>` node.
//!
//! Lives in its own module (like `plan` / `workstream`) so concurrent
//! agents editing `main.rs` only collide on the two-line dispatch arm.
//! Client construction, org resolution, and the vox transport all reuse
//! the shared helpers in `main.rs` (`crate::establish_client`,
//! `crate::resolve_active_org`, `crate::org_ctx`).

use std::path::{Path, PathBuf};

use clap::Subcommand;
use collection_proto::{
    Collection, CollectionKind, CollectionServiceClient, NodeKind, NodeRef, Placement,
};
use eyre::Context;
use song::{Arrangement, AttachmentRef, ChartRef, Key, PartsManifest, Song};
use uuid::Uuid;

use crate::errors;

// ── CLI surface ───────────────────────────────────────────────────────

#[derive(Subcommand)]
pub enum CollectionCmd {
    /// Create an empty collection.
    Create {
        /// Human title (e.g. `"Sunday Library"`).
        title: String,
        /// What kind of collection: `library` | `setlist` | `show` |
        /// `playlist` | `other:NAME`.
        #[arg(long, default_value = "library")]
        kind: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Add a node to a collection (append, or insert after `--after`).
    Add {
        /// Target collection — id or title.
        collection: String,
        /// Node reference token (`kind:id`, e.g. `song:great-are-you-lord`).
        #[arg(long)]
        node: String,
        /// Insert immediately after this node token. Omit to append.
        #[arg(long)]
        after: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Move an existing item to sit after `--after` (or to the tail).
    Reorder {
        /// Target collection — id or title.
        collection: String,
        /// The node token to move.
        #[arg(long)]
        node: String,
        /// Move the item to sit after this node token. Omit for the tail.
        #[arg(long)]
        after: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// List collections in the active org, optionally filtered by kind.
    List {
        /// Restrict to one kind (`library` | `setlist` | … | `other:NAME`).
        #[arg(long)]
        kind: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Show one collection and its ordered items.
    Show {
        /// Collection — id or title.
        collection: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
}

#[derive(Subcommand)]
pub enum SongCmd {
    /// Build a Song folder and add it to a collection as a `song:` node.
    Add {
        /// Target collection — id or title.
        collection: String,
        /// Song title (e.g. `"Great Are You Lord"`).
        #[arg(long)]
        title: String,
        /// Musical key of the default arrangement (`"Bb Minor"`,
        /// `"F# Dorian"`, `"C"`). Defaults to `C Major`.
        #[arg(long)]
        key: Option<String>,
        /// Chart file (e.g. a `.kf` keyflow chart) — copied into the
        /// default arrangement folder.
        #[arg(long, value_name = "FILE")]
        chart: Option<PathBuf>,
        /// PDF attachment(s). Repeatable.
        #[arg(long = "pdf", value_name = "FILE")]
        pdf: Vec<PathBuf>,
        /// Audio attachment(s). Repeatable.
        #[arg(long = "audio", value_name = "FILE")]
        audio: Vec<PathBuf>,
        /// Name of the default arrangement (default `Default`).
        #[arg(long)]
        arrangement: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
}

// ── parsing helpers ───────────────────────────────────────────────────

/// Parse a `--kind` value into a [`CollectionKind`]. `other:NAME` maps to
/// [`CollectionKind::Other`]; the four named kinds match case-insensitively;
/// anything else falls through to `Other(<verbatim>)`.
fn parse_kind(s: &str) -> CollectionKind {
    if let Some(rest) = s
        .strip_prefix("other:")
        .or_else(|| s.strip_prefix("Other:"))
        .or_else(|| s.strip_prefix("OTHER:"))
    {
        return CollectionKind::Other(rest.to_string());
    }
    match s.to_ascii_lowercase().as_str() {
        "library" => CollectionKind::Library,
        "setlist" => CollectionKind::Setlist,
        "show" => CollectionKind::Show,
        "playlist" => CollectionKind::Playlist,
        other => CollectionKind::Other(other.to_string()),
    }
}

/// Parse a `kind:id[#anchor]` node token into a [`NodeRef`].
fn parse_node(token: &str) -> eyre::Result<NodeRef> {
    NodeRef::parse(token).ok_or_else(|| {
        errors::usage("parse node")
            .cause(format!("`{token}` is not a `kind:id` token"))
            .hint("e.g. `song:great-are-you-lord`, `note:Journal/2026.md`")
            .report()
    })
}

/// Slugify a title into a folder-safe token (mirrors the `song` crate's
/// internal slug rule for the on-disk arrangement dirs).
fn slug(name: &str) -> String {
    let s: String = name
        .trim()
        .to_ascii_lowercase()
        .chars()
        .map(|c| if c.is_ascii_alphanumeric() { c } else { '-' })
        .collect();
    let cleaned = s
        .split('-')
        .filter(|p| !p.is_empty())
        .collect::<Vec<_>>()
        .join("-");
    if cleaned.is_empty() {
        "song".to_string()
    } else {
        cleaned
    }
}

// ── shared client / resolution ────────────────────────────────────────

async fn client_for(
    org: Option<String>,
    server: Option<String>,
) -> eyre::Result<(CollectionServiceClient, String)> {
    let slug = crate::resolve_active_org(org)?;
    let client = crate::establish_client::<CollectionServiceClient>(server, &slug).await?;
    Ok((client, slug))
}

/// Resolve a `<collection>` argument (an id or a title) to a [`Collection`].
async fn resolve_collection(
    client: &CollectionServiceClient,
    org_slug: &str,
    target: &str,
) -> eyre::Result<Collection> {
    // Fast path: treat the argument as an id.
    if let Some(c) = client
        .get(target.to_owned())
        .await
        .map_err(|e| eyre::eyre!("get: {e:?}"))?
    {
        return Ok(c);
    }
    // Fall back to a title match within the org.
    let all = client
        .list(org_slug.to_owned(), None)
        .await
        .map_err(|e| eyre::eyre!("list: {e:?}"))?;
    all.into_iter()
        .find(|c| c.id == target || c.title == target)
        .ok_or_else(|| {
            errors::not_found("resolve collection", target)
                .cause("no id or title match in this org")
                .report()
        })
}

fn print_collection(c: &Collection) {
    println!("{} [{}]", c.title, c.kind.as_str());
    println!("  id:    {}", c.id);
    println!("  org:   {}", c.org);
    println!("  items: {}", c.items.len());
    for (i, item) in c.items.iter().enumerate() {
        println!("    {:>3}. {}", i + 1, item.node.to_token());
    }
}

fn emit_json<T: serde::Serialize>(v: &T) -> eyre::Result<()> {
    println!(
        "{}",
        serde_json::to_string_pretty(v).map_err(|e| eyre::eyre!("json: {e}"))?
    );
    Ok(())
}

// ── run: collection ───────────────────────────────────────────────────

pub async fn run_collection(cmd: CollectionCmd) -> eyre::Result<()> {
    match cmd {
        CollectionCmd::Create {
            title,
            kind,
            org,
            server,
            json,
        } => {
            let (client, slug) = client_for(org, server).await?;
            let created = client
                .create(slug, title, parse_kind(&kind))
                .await
                .map_err(|e| eyre::eyre!("create: {e:?}"))?;
            if json {
                return emit_json(&created);
            }
            println!("created collection {}", created.id);
            print_collection(&created);
        }
        CollectionCmd::Add {
            collection,
            node,
            after,
            org,
            server,
            json,
        } => {
            let (client, slug) = client_for(org, server).await?;
            let coll = resolve_collection(&client, &slug, &collection).await?;
            let node = parse_node(&node)?;
            let after = after.as_deref().map(parse_node).transpose()?;
            let updated = client
                .add_item(Placement {
                    collection_id: coll.id,
                    node,
                    after,
                })
                .await
                .map_err(|e| eyre::eyre!("add_item: {e:?}"))?;
            if json {
                return emit_json(&updated);
            }
            print_collection(&updated);
        }
        CollectionCmd::Reorder {
            collection,
            node,
            after,
            org,
            server,
            json,
        } => {
            let (client, slug) = client_for(org, server).await?;
            let coll = resolve_collection(&client, &slug, &collection).await?;
            let node = parse_node(&node)?;
            let after = after.as_deref().map(parse_node).transpose()?;
            let updated = client
                .reorder(Placement {
                    collection_id: coll.id,
                    node,
                    after,
                })
                .await
                .map_err(|e| eyre::eyre!("reorder: {e:?}"))?;
            if json {
                return emit_json(&updated);
            }
            print_collection(&updated);
        }
        CollectionCmd::List {
            kind,
            org,
            server,
            json,
        } => {
            let (client, slug) = client_for(org, server).await?;
            let filter = kind.as_deref().map(parse_kind);
            let rows = client
                .list(slug, filter)
                .await
                .map_err(|e| eyre::eyre!("list: {e:?}"))?;
            if json {
                return emit_json(&rows);
            }
            println!("{} collections", rows.len());
            for c in &rows {
                println!(
                    "  {} [{}]  {} items  ({})",
                    c.title,
                    c.kind.as_str(),
                    c.items.len(),
                    c.id
                );
            }
        }
        CollectionCmd::Show {
            collection,
            org,
            server,
            json,
        } => {
            let (client, slug) = client_for(org, server).await?;
            let coll = resolve_collection(&client, &slug, &collection).await?;
            if json {
                return emit_json(&coll);
            }
            print_collection(&coll);
        }
    }
    Ok(())
}

// ── run: song ─────────────────────────────────────────────────────────

pub async fn run_song(cmd: SongCmd) -> eyre::Result<()> {
    match cmd {
        SongCmd::Add {
            collection,
            title,
            key,
            chart,
            pdf,
            audio,
            arrangement,
            org,
            server,
            json,
        } => {
            // Resolve the org once: its slug names the collection service's
            // scope, and its on-disk root is where the Song folder is
            // written (`<org>/resources/songs/<slug>`).
            let active = crate::org_ctx::resolve_active(org.as_deref())?;
            let org_slug = active.root.slug().to_string();
            let song_slug = slug(&title);
            let song_root = active
                .root
                .resources_dir()
                .join("songs")
                .join(&song_slug);

            let key: Key = match key.as_deref() {
                Some(k) => k
                    .parse()
                    .map_err(|e| errors::usage("parse key").cause(format!("{e}")).report())?,
                None => Key::default(),
            };
            let arr_name = arrangement.unwrap_or_else(|| "Default".to_string());
            let arr_slug = slug(&arr_name);
            let arr_id = Uuid::new_v4();

            // Chart lands inside the default arrangement folder; pdfs +
            // audio land under `attachments/`. Refs are relative paths.
            let chart_ref = chart.as_ref().map(|src| {
                let fname = file_name(src);
                ChartRef::from_path(format!("arrangements/{arr_slug}/{fname}"))
            });

            let mut attachment_refs = Vec::new();
            for src in &pdf {
                attachment_refs.push(attachment_ref(src, "pdf"));
            }
            for src in &audio {
                attachment_refs.push(attachment_ref(src, "audio"));
            }

            let arrangement = Arrangement {
                id: arr_id,
                name: arr_name,
                key,
                chart_ref: chart_ref.clone(),
                parts: PartsManifest::default(),
                attachment_refs: attachment_refs.clone(),
            };
            let song = Song {
                id: Uuid::new_v4(),
                title: title.clone(),
                tags: Vec::new(),
                default_arrangement: arr_id,
                arrangements: vec![arrangement],
            };

            // Write the folder skeleton (song.md + arrangement.md + dirs)…
            song::to_folder(&song, &song_root)
                .map_err(|e| eyre::eyre!("write song folder {}: {e}", song_root.display()))?;

            // …then copy the referenced bytes into place.
            if let (Some(src), Some(cref)) = (chart.as_ref(), chart_ref.as_ref()) {
                if let Some(rel) = &cref.path {
                    copy_into(src, &song_root.join(rel))?;
                }
            }
            for (src, aref) in pdf.iter().chain(audio.iter()).zip(&attachment_refs) {
                if let Some(rel) = &aref.path {
                    copy_into(src, &song_root.join(rel))?;
                }
            }

            // Register the song in the target collection.
            let client =
                crate::establish_client::<CollectionServiceClient>(server, &org_slug).await?;
            let coll = resolve_collection(&client, &org_slug, &collection).await?;
            let node = NodeRef::new(NodeKind::Song, song_slug.clone());
            let updated = client
                .add_item(Placement {
                    collection_id: coll.id,
                    node,
                    after: None,
                })
                .await
                .map_err(|e| eyre::eyre!("add_item: {e:?}"))?;

            if json {
                return emit_json(&updated);
            }
            println!(
                "wrote song `{title}` → {}",
                song_root.display()
            );
            println!("added song:{song_slug} to `{}`", updated.title);
            print_collection(&updated);
        }
    }
    Ok(())
}

// ── song file helpers ─────────────────────────────────────────────────

fn file_name(p: &Path) -> String {
    p.file_name()
        .map(|s| s.to_string_lossy().into_owned())
        .unwrap_or_else(|| "attachment".to_string())
}

fn attachment_ref(src: &Path, kind: &str) -> AttachmentRef {
    let fname = file_name(src);
    AttachmentRef {
        id: Uuid::new_v4().simple().to_string(),
        path: Some(format!("attachments/{fname}")),
        sha256: None,
        kind: Some(kind.to_string()),
    }
}

/// Copy `src` to `dest`, creating parent dirs. Errors if `src` is missing.
fn copy_into(src: &Path, dest: &Path) -> eyre::Result<()> {
    if let Some(parent) = dest.parent() {
        std::fs::create_dir_all(parent)
            .wrap_err_with(|| format!("mkdir {}", parent.display()))?;
    }
    std::fs::copy(src, dest)
        .wrap_err_with(|| format!("copy {} → {}", src.display(), dest.display()))?;
    Ok(())
}
