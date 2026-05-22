//! `task` CLI — vertical-slice scaffold.
//!
//! After the Loro entity layer was ripped, the surface is:
//! - `task doctor`        — print resolved vox endpoint URL.
//! - `task vault <cmd>`   — filesystem-native vault queries +
//!                          mutations (open / pages / tags /
//!                          tasks / backlinks / outline / grep /
//!                          property-{read,set,remove} / create /
//!                          append / delete / move / base-query).
//!
//! Task / project commands (`list`, `set-done`, `new-task`,
//! `new-project`) went away with `project-crdt`. Rebuild them
//! against `vault::Vault` once the on-disk task convention is
//! pinned down (frontmatter shape, folder layout).
//!
//! Endpoint resolution for `task doctor` (first match wins):
//! 1. `--server <url>` flag.
//! 2. `TASK_VOX_URL` env var (loaded from `.env` if present).
//! 3. `ws://127.0.0.1:9090/vox` default.

mod shared;

use clap::{Parser, Subcommand};
use shared::RemoteVoxConfig;
use std::collections::HashMap;
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
    /// Probe the configured vox endpoint.
    Doctor,
    /// FS-native vault queries — no server, no CRDT.
    /// Operates directly on `.md` and `.base` files. Mirrors the
    /// Obsidian-CLI / TaskNotes-CLI surface for shell scripting.
    Vault {
        #[command(subcommand)]
        cmd: VaultCmd,
    },
}

// Task / project commands (`list`, `set-done`, `new-task`,
// `new-project`) were ripped when the CRDT entity layer went
// away. Rebuild them in a follow-up slice against vault::Vault
// — tasks become markdown pages with `type: task` frontmatter,
// projects become folders.

#[derive(Subcommand)]
enum VaultCmd {
    /// Open a vault and print a one-line summary.
    Open { path: std::path::PathBuf },
    /// List pages in a vault, optionally filtered by folder, tag, or
    /// `key=value` frontmatter match. Prints one vault-relative path
    /// per line so output is pipe-friendly.
    Pages {
        path: std::path::PathBuf,
        /// Restrict to pages whose folder equals (or starts with) this.
        #[arg(long)]
        folder: Option<String>,
        /// Restrict to pages tagged `<tag>` (frontmatter `tags` or
        /// inline `#tag`).
        #[arg(long)]
        tag: Option<String>,
        /// `key=value` frontmatter match. Value is parsed as JSON,
        /// falling back to a string. Repeatable; all must match.
        #[arg(long = "fm", value_name = "KEY=VAL")]
        fm: Vec<String>,
        /// Emit one JSON object per line instead of the path.
        #[arg(long)]
        json: bool,
    },
    /// List every tag in the vault with a count.
    Tags { path: std::path::PathBuf },
    /// List `.base` files and (if parsed cleanly) their view names.
    Bases { path: std::path::PathBuf },
    /// Print a single page's raw markdown.
    Cat {
        path: std::path::PathBuf,
        /// Vault-relative path of the page, e.g. `Music/Charts.md`.
        rel_path: String,
    },
    /// Substring search across page bodies (case-insensitive).
    /// Prints `path:line:content` like grep.
    Grep {
        path: std::path::PathBuf,
        pattern: String,
    },
    /// Pages that link TO the given page.
    Backlinks {
        path: std::path::PathBuf,
        rel_path: String,
    },
    /// Outgoing wikilinks from a page (resolved + raw target).
    Links {
        path: std::path::PathBuf,
        rel_path: String,
    },
    /// Pages with no incoming links.
    Orphans { path: std::path::PathBuf },
    /// Pages with no outgoing links.
    Deadends { path: std::path::PathBuf },
    /// Wikilink targets that don't resolve to any page.
    Unresolved { path: std::path::PathBuf },
    /// Heading outline of a single page.
    Outline {
        path: std::path::PathBuf,
        rel_path: String,
    },
    /// List distinct frontmatter property keys across the vault.
    Properties { path: std::path::PathBuf },
    /// Read one frontmatter property from a page.
    PropertyRead {
        path: std::path::PathBuf,
        rel_path: String,
        key: String,
    },
    /// Set a frontmatter property on a page (creates key if absent).
    PropertySet {
        path: std::path::PathBuf,
        rel_path: String,
        key: String,
        /// Value parsed as JSON; falls back to a string literal.
        value: String,
    },
    /// Remove a frontmatter property from a page (no-op if absent).
    PropertyRemove {
        path: std::path::PathBuf,
        rel_path: String,
        key: String,
    },
    /// All aliases declared via frontmatter, sorted.
    Aliases { path: std::path::PathBuf },
    /// All `- [ ]` task items across the vault. `path:line marker text`.
    Tasks { path: std::path::PathBuf },
    /// Word + character count for the vault (or one page when `--page` set).
    Wordcount {
        path: std::path::PathBuf,
        #[arg(long)]
        page: Option<String>,
    },
    /// Run all views in a `.base` file over the vault's pages.
    /// `--view <name>` runs only the matching view (case-insensitive).
    BaseQuery {
        path: std::path::PathBuf,
        /// Vault-relative path to the `.base` file.
        base: String,
        #[arg(long)]
        view: Option<String>,
    },
    /// Create a new `.md` page. Fails if the file already exists.
    Create {
        path: std::path::PathBuf,
        rel_path: String,
        /// Optional initial body. Reads stdin when omitted.
        #[arg(long)]
        body: Option<String>,
    },
    /// Append text to an existing page.
    Append {
        path: std::path::PathBuf,
        rel_path: String,
        text: String,
        /// No leading newline.
        #[arg(long)]
        inline: bool,
    },
    /// Prepend text immediately after the frontmatter (or at top
    /// when none).
    Prepend {
        path: std::path::PathBuf,
        rel_path: String,
        text: String,
        #[arg(long)]
        inline: bool,
    },
    /// Delete a page.
    Delete {
        path: std::path::PathBuf,
        rel_path: String,
    },
    /// Move / rename a page.
    Move {
        path: std::path::PathBuf,
        from: String,
        to: String,
    },
}

#[tokio::main]
async fn main() -> eyre::Result<()> {
    // Best-effort .env load before clap reads env. Missing file is
    // not an error — we just fall through to the hard-coded default.
    let _ = dotenvy::dotenv();
    let cli = Cli::parse();
    match cli.command {
        Commands::Doctor => {
            let remote =
                RemoteVoxConfig::from_args(cli.server, cli.session_token, cli.organization_id)?;
            println!("Vox endpoint: {}", remote.display_url);
        }
        Commands::Vault { cmd } => {
            return run_vault(cmd);
        }
    }
    Ok(())
}

// ── FS-native Obsidian vault subcommands ─────────────────────────────

fn run_vault(cmd: VaultCmd) -> eyre::Result<()> {
    use vault_obsidian::Vault;
    match cmd {
        VaultCmd::Open { path } => {
            let t0 = std::time::Instant::now();
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            println!(
                "vault: {}\n  pages:       {}\n  bases:       {}\n  attachments: {}\n  loaded in:   {:?}",
                v.root.display(),
                v.pages.len(),
                v.bases.len(),
                v.attachments.len(),
                t0.elapsed(),
            );
        }
        VaultCmd::Pages {
            path,
            folder,
            tag,
            fm,
            json,
        } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let fm_pairs = parse_fm_pairs(&fm)?;
            for page in &v.pages {
                if let Some(f) = &folder {
                    if !page.folder.starts_with(f) {
                        continue;
                    }
                }
                if let Some(t) = &tag {
                    if !page_matches_tag(page, t) {
                        continue;
                    }
                }
                if !fm_pairs.iter().all(|(k, v)| page_matches_fm(page, k, v)) {
                    continue;
                }
                if json {
                    let obj = serde_json::json!({
                        "path": page.rel_path,
                        "basename": page.basename,
                        "folder": page.folder,
                        "frontmatter": page
                            .parsed
                            .frontmatter
                            .iter()
                            .map(|e| (e.key.clone(), e.value.clone()))
                            .collect::<serde_json::Map<_, _>>(),
                    });
                    println!("{obj}");
                } else {
                    println!("{}", page.rel_path);
                }
            }
        }
        VaultCmd::Tags { path } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let mut counts: HashMap<String, usize> = HashMap::new();
            for page in &v.pages {
                for t in collect_page_tags(page) {
                    *counts.entry(t).or_insert(0) += 1;
                }
            }
            let mut rows: Vec<_> = counts.into_iter().collect();
            rows.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.cmp(&b.0)));
            for (tag, n) in rows {
                println!("{n:>5}  #{tag}");
            }
        }
        VaultCmd::Bases { path } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            for base in &v.bases {
                match &base.parsed {
                    Ok(p) => {
                        let views: Vec<&str> = p.views.iter().map(|v| v.name.as_str()).collect();
                        println!("{}  [{}]", base.rel_path, views.join(", "));
                    }
                    Err(e) => println!("{}  (parse error: {e})", base.rel_path),
                }
            }
        }
        VaultCmd::Cat { path, rel_path } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let page = v
                .page(&rel_path)
                .ok_or_else(|| eyre::eyre!("page not found: {rel_path}"))?;
            print!("{}", page.raw);
        }
        VaultCmd::Grep { path, pattern } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let needle = pattern.to_lowercase();
            for page in &v.pages {
                for (i, line) in page.raw.lines().enumerate() {
                    if line.to_lowercase().contains(&needle) {
                        println!("{}:{}:{}", page.rel_path, i + 1, line);
                    }
                }
            }
        }
        VaultCmd::Backlinks { path, rel_path } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let idx = vault_obsidian::LinkIndex::build(&v);
            for p in idx.backlinks(&rel_path) {
                println!("{p}");
            }
        }
        VaultCmd::Links { path, rel_path } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let idx = vault_obsidian::LinkIndex::build(&v);
            for link in idx.outgoing(&rel_path) {
                match link.resolved {
                    Some(target) => println!("{}\t→ {target}", link.linkpath),
                    None => println!("{}\t(unresolved)", link.linkpath),
                }
            }
        }
        VaultCmd::Orphans { path } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let idx = vault_obsidian::LinkIndex::build(&v);
            for p in idx.orphans() {
                println!("{p}");
            }
        }
        VaultCmd::Deadends { path } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let idx = vault_obsidian::LinkIndex::build(&v);
            for p in idx.deadends() {
                println!("{p}");
            }
        }
        VaultCmd::Unresolved { path } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let idx = vault_obsidian::LinkIndex::build(&v);
            for u in idx.unresolved() {
                println!("{}\t{}", u.source, u.linkpath);
            }
        }
        VaultCmd::Outline { path, rel_path } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let page = v
                .page(&rel_path)
                .ok_or_else(|| eyre::eyre!("page not found: {rel_path}"))?;
            for h in vault_obsidian::outline(page).headings {
                let bar = "#".repeat(h.level as usize);
                println!("{:>5}  {bar} {}", h.line, h.text);
            }
        }
        VaultCmd::Properties { path } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            for k in vault_obsidian::list_property_keys(&v) {
                println!("{k}");
            }
        }
        VaultCmd::PropertyRead {
            path,
            rel_path,
            key,
        } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let page = v
                .page(&rel_path)
                .ok_or_else(|| eyre::eyre!("page not found: {rel_path}"))?;
            match vault_obsidian::read_property(page, &key) {
                Some(v) => println!("{v}"),
                None => {}
            }
        }
        VaultCmd::PropertySet {
            path,
            rel_path,
            key,
            value,
        } => {
            let mut v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let parsed: serde_json::Value = serde_json::from_str(&value)
                .unwrap_or_else(|_| serde_json::Value::String(value.clone()));
            let guard = vault_obsidian::SelfWriteGuard::new();
            vault_obsidian::set_property(&mut v, &rel_path, &key, parsed, &guard)
                .map_err(|e| eyre::eyre!("set: {e}"))?;
        }
        VaultCmd::PropertyRemove {
            path,
            rel_path,
            key,
        } => {
            let mut v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let guard = vault_obsidian::SelfWriteGuard::new();
            vault_obsidian::remove_property(&mut v, &rel_path, &key, &guard)
                .map_err(|e| eyre::eyre!("remove: {e}"))?;
        }
        VaultCmd::Aliases { path } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            for a in vault_obsidian::list_aliases(&v) {
                println!("{}\t{}", a.alias, a.page);
            }
        }
        VaultCmd::Tasks { path } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            for t in vault_obsidian::list_tasks(&v) {
                println!("{}:{}\t[{}] {}", t.page, t.line, t.marker, t.text);
            }
        }
        VaultCmd::Wordcount { path, page } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let wc = match page {
                Some(rel) => {
                    let p = v
                        .page(&rel)
                        .ok_or_else(|| eyre::eyre!("page not found: {rel}"))?;
                    vault_obsidian::page_wordcount(p)
                }
                None => vault_obsidian::vault_wordcount(&v),
            };
            println!(
                "pages: {}\nwords: {}\ncharacters: {}",
                wc.pages, wc.words, wc.characters
            );
        }
        VaultCmd::BaseQuery { path, base, view } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            match view {
                Some(view_name) => {
                    let ev = vault_obsidian::query_view(&v, &base, &view_name)
                        .map_err(|e| eyre::eyre!("query: {e}"))?;
                    print_executed_view(&view_name, &ev);
                }
                None => {
                    let results = vault_obsidian::query_all_views(&v, &base)
                        .map_err(|e| eyre::eyre!("query: {e}"))?;
                    for (name, ev) in results {
                        print_executed_view(&name, &ev);
                    }
                }
            }
        }
        VaultCmd::Create {
            path,
            rel_path,
            body,
        } => {
            let mut v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let body = match body {
                Some(b) => b,
                None => {
                    use std::io::Read;
                    let mut buf = String::new();
                    std::io::stdin().read_to_string(&mut buf)?;
                    buf
                }
            };
            let guard = vault_obsidian::SelfWriteGuard::new();
            vault_obsidian::create_page(&mut v, &rel_path, &[], &body, &guard)
                .map_err(|e| eyre::eyre!("create: {e}"))?;
        }
        VaultCmd::Append {
            path,
            rel_path,
            text,
            inline,
        } => {
            let mut v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let guard = vault_obsidian::SelfWriteGuard::new();
            vault_obsidian::append_to_page(&mut v, &rel_path, &text, inline, &guard)
                .map_err(|e| eyre::eyre!("append: {e}"))?;
        }
        VaultCmd::Prepend {
            path,
            rel_path,
            text,
            inline,
        } => {
            let mut v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let guard = vault_obsidian::SelfWriteGuard::new();
            vault_obsidian::prepend_to_page(&mut v, &rel_path, &text, inline, &guard)
                .map_err(|e| eyre::eyre!("prepend: {e}"))?;
        }
        VaultCmd::Delete { path, rel_path } => {
            let mut v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let guard = vault_obsidian::SelfWriteGuard::new();
            vault_obsidian::delete_page(&mut v, &rel_path, &guard)
                .map_err(|e| eyre::eyre!("delete: {e}"))?;
        }
        VaultCmd::Move { path, from, to } => {
            let mut v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let guard = vault_obsidian::SelfWriteGuard::new();
            vault_obsidian::move_page(&mut v, &from, &to, &guard)
                .map_err(|e| eyre::eyre!("move: {e}"))?;
        }
    }
    Ok(())
}

fn print_executed_view(name: &str, ev: &vault_live::bases::ExecutedView) {
    let total: usize = ev.groups.iter().map(|(_, r)| r.len()).sum();
    println!("## {name}  ({total} rows)");
    for (bucket, rows) in &ev.groups {
        if !bucket.is_empty() {
            println!("  [{bucket}]");
        }
        for row in rows {
            println!("    {}", row.basename);
        }
    }
}

fn parse_fm_pairs(raw: &[String]) -> eyre::Result<Vec<(String, serde_json::Value)>> {
    raw.iter()
        .map(|s| {
            let (k, v) = s
                .split_once('=')
                .ok_or_else(|| eyre::eyre!("--fm expects KEY=VALUE, got `{s}`"))?;
            let parsed: serde_json::Value = serde_json::from_str(v)
                .unwrap_or_else(|_| serde_json::Value::String(v.to_string()));
            Ok((k.to_string(), parsed))
        })
        .collect()
}

fn page_matches_fm(page: &vault_obsidian::VaultPage, key: &str, value: &serde_json::Value) -> bool {
    page.parsed
        .frontmatter
        .iter()
        .any(|e| e.key == key && &e.value == value)
}

fn page_matches_tag(page: &vault_obsidian::VaultPage, tag: &str) -> bool {
    // Match Obsidian: a query for `#parent` also includes any
    // `#parent/child` nested tags.
    let prefix = format!("{tag}/");
    collect_page_tags(page)
        .into_iter()
        .any(|t| t == tag || t.starts_with(&prefix))
}

fn collect_page_tags(page: &vault_obsidian::VaultPage) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    for e in &page.parsed.frontmatter {
        if e.key == "tags" || e.key == "tag" {
            match &e.value {
                serde_json::Value::String(s) => {
                    for t in s.split([',', ' ']) {
                        let t = t.trim().trim_start_matches('#');
                        if !t.is_empty() {
                            out.push(t.to_string());
                        }
                    }
                }
                serde_json::Value::Array(arr) => {
                    for v in arr {
                        if let Some(s) = v.as_str() {
                            out.push(s.trim_start_matches('#').to_string());
                        }
                    }
                }
                _ => {}
            }
        }
    }
    for b in &page.parsed.blocks {
        for r in &b.refs {
            if let vault_live::refs::Ref::Tag(t) = r {
                out.push(t.path.join("/"));
            }
        }
    }
    out.sort();
    out.dedup();
    out
}
