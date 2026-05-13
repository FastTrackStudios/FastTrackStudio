//! `architect feature new <name>` — scaffold a feature crate family.
//!
//! Writes the canonical layout:
//!
//!     features/<name>/
//!       <name>-proto/        contract (architect::Entity + a placeholder type)
//!       <name>-memory/       in-tree in-memory backend
//!       <name>/              facade (vox + server-* + backend-* features)
//!       spec/<name>.md       tracey rule stub
//!       tests/native/        cargo test against the memory backend
//!
//! Then updates:
//!
//!   * Cargo.toml — members, default-members, workspace.dependencies
//!   * .config/tracey/config.styx — append a new spec block

use std::fs;
use std::path::{Path, PathBuf};

use heck::{ToPascalCase, ToSnakeCase};
use owo_colors::OwoColorize;
use toml_edit::{Array, DocumentMut, Item, Table, value};

pub fn feature_new(repo_root: &Path, name: &str, force: bool) -> eyre::Result<()> {
    validate_name(name)?;
    let names = Names::from_kebab(name);

    eprintln!(
        "{} scaffolding feature {} at features/{name}/",
        "❯❯".cyan().bold(),
        names.kebab.bold()
    );

    let feature_dir = repo_root.join("features").join(&names.kebab);
    if feature_dir.exists() && !force {
        let occupied = feature_dir
            .read_dir()
            .map(|d| d.count() > 0)
            .unwrap_or(false);
        if occupied {
            eyre::bail!(
                "features/{} exists and is non-empty; re-run with --force to overwrite",
                names.kebab
            );
        }
    }

    write_proto(&feature_dir, &names)?;
    write_memory(&feature_dir, &names)?;
    write_facade(&feature_dir, &names)?;
    write_spec(&feature_dir, &names)?;
    write_tests_native(&feature_dir, &names)?;

    update_workspace_cargo(repo_root, &names)?;
    update_tracey_config(repo_root, &names)?;

    eprintln!(
        "{} feature {} scaffolded.

Next steps:
  1. Rename the placeholder `{}` entity in features/{}/{}-proto/src/lib.rs
     to fit your domain (e.g. `Channel`, `Track`, `Clip`).
  2. Flesh out the memory backend at features/{}/{}-memory/src/lib.rs.
  3. Write rules at features/{}/spec/{}.md and verify with
     `cargo xtask tracey-validate`.
  4. Add a binary that depends on `{}` with the features you need.
",
        "✓".green().bold(),
        names.kebab.bold(),
        names.pascal,
        names.kebab,
        names.kebab,
        names.kebab,
        names.kebab,
        names.kebab,
        names.kebab,
        names.kebab,
    );

    Ok(())
}

// ── Name conversions ──────────────────────────────────────────────────

struct Names {
    kebab: String,  // mixing
    snake: String,  // mixing
    pascal: String, // Mixing
}

impl Names {
    fn from_kebab(s: &str) -> Self {
        Self {
            kebab: s.to_string(),
            snake: s.to_snake_case(),
            pascal: s.to_pascal_case(),
        }
    }
}

fn validate_name(name: &str) -> eyre::Result<()> {
    if name.is_empty() {
        eyre::bail!("feature name is empty");
    }
    if !name
        .chars()
        .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '-')
    {
        eyre::bail!(
            "feature name `{}` must be kebab-case ([a-z0-9-]+); cargo package names accept only these",
            name
        );
    }
    if !name.chars().next().unwrap().is_ascii_lowercase() {
        eyre::bail!("feature name `{}` must start with a letter", name);
    }
    Ok(())
}

// ── Crate writers ─────────────────────────────────────────────────────

fn write_proto(feature_dir: &Path, n: &Names) -> eyre::Result<()> {
    let dir = feature_dir.join(format!("{}-proto", n.kebab));
    fs::create_dir_all(dir.join("src"))?;

    write(
        dir.join("Cargo.toml"),
        format!(
            r#"[package]
name = "{kebab}-proto"
version.workspace = true
edition.workspace = true
authors.workspace = true
license.workspace = true

# Wire contract for the `{kebab}` feature. Pull this crate (or the
# `{kebab}` facade) from any consumer — wasm-clean by default.

[dependencies]
architect.workspace = true
chrono.workspace = true
uuid = {{ workspace = true, features = ["js"] }}
facet.workspace = true
thiserror.workspace = true

vox = {{ workspace = true, optional = true }}
sea-orm = {{ workspace = true, optional = true }}

[features]
default = ["vox"]
vox = ["dep:vox", "architect/vox"]
server = ["architect/server-seaorm", "dep:sea-orm"]
full = ["vox", "server"]
"#,
            kebab = n.kebab,
        ),
    )?;

    write(
        dir.join("src").join("lib.rs"),
        format!(
            r#"//! `{kebab}-proto` — wire contract for the `{kebab}` feature.
//!
//! Rename the `{pascal}` placeholder below to whatever the canonical
//! entity for this feature actually is (`Channel`, `Track`, `Clip`, …).

pub use architect;

use architect::Entity;
use chrono::{{DateTime, Utc}};
use uuid::Uuid;

#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "{snake}_items", repo)]
pub struct {pascal} {{
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    pub name: String,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}}

// Hand-written service trait. Architect doesn't touch this; replace
// the placeholder methods with your real domain operations.

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum {pascal}ServiceError {{
    #[error("not found")]
    NotFound,
    #[error("invalid input: {{0}}")]
    InvalidInput(String),
    #[error("internal error: {{0}}")]
    Internal(String),
}}

#[cfg_attr(feature = "vox", vox::service)]
pub trait {pascal}Service {{
    /// TODO: replace with a real domain method.
    async fn ping(&self) -> Result<String, {pascal}ServiceError>;
}}
"#,
            kebab = n.kebab,
            snake = n.snake,
            pascal = n.pascal,
        ),
    )?;
    Ok(())
}

fn write_memory(feature_dir: &Path, n: &Names) -> eyre::Result<()> {
    let dir = feature_dir.join(format!("{}-memory", n.kebab));
    fs::create_dir_all(dir.join("src"))?;

    write(
        dir.join("Cargo.toml"),
        format!(
            r#"[package]
name = "{kebab}-memory"
version.workspace = true
edition.workspace = true
authors.workspace = true
license.workspace = true

# In-memory implementation of `{kebab}-proto`'s repository trait. Useful
# for tests and ephemeral single-process deployments. Depends only on
# the contract crate — no sea-orm, no architect/server.

[dependencies]
{kebab}-proto.workspace = true
architect.workspace = true
uuid.workspace = true
chrono.workspace = true
tokio = {{ workspace = true, features = ["sync"] }}
"#,
            kebab = n.kebab,
        ),
    )?;

    write(
        dir.join("src").join("lib.rs"),
        format!(
            r#"//! In-memory `{pascal}Repo` implementation.

use std::sync::Arc;

use architect::{{Filter, Page, RepoError, Sort, SortOrder}};
use chrono::Utc;
use {snake}_proto::{{
    {pascal}, {pascal}Create, {pascal}List, {pascal}Repo, {pascal}Update,
}};
use tokio::sync::RwLock;
use uuid::Uuid;

#[derive(Clone, Default)]
pub struct {pascal}RepoMemory {{
    inner: Arc<RwLock<Vec<{pascal}>>>,
}}

impl {pascal}RepoMemory {{
    pub fn new() -> Self {{
        Self::default()
    }}
}}

impl {pascal}Repo for {pascal}RepoMemory {{
    async fn get(&self, id: Uuid) -> Result<{pascal}, RepoError> {{
        self.inner
            .read()
            .await
            .iter()
            .find(|e| e.id == id)
            .cloned()
            .ok_or(RepoError::NotFound)
    }}

    async fn list(
        &self,
        page: Page,
        sort: Option<Sort>,
        _filter: Option<Filter>,
    ) -> Result<{pascal}List, RepoError> {{
        let mut items: Vec<{pascal}> = self.inner.read().await.iter().cloned().collect();

        if let Some(s) = sort {{
            match s.field.as_str() {{
                "name" => {{
                    items.sort_by(|a, b| a.name.cmp(&b.name));
                    if matches!(s.order, SortOrder::Desc) {{
                        items.reverse();
                    }}
                }}
                other => {{
                    return Err(RepoError::InvalidInput(format!(
                        "unsortable field: {{other}}"
                    )));
                }}
            }}
        }}

        let total = items.len() as u32;
        let size = page.size.max(1) as usize;
        let start = (page.index as usize).saturating_mul(size);
        let items = items.into_iter().skip(start).take(size).collect();
        Ok({pascal}List {{ items, total, page }})
    }}

    async fn create(&self, input: {pascal}Create) -> Result<{pascal}, RepoError> {{
        let now = Utc::now();
        let row = {pascal} {{
            id: Uuid::new_v4(),
            name: input.name,
            created_at: now,
            updated_at: now,
        }};
        self.inner.write().await.push(row.clone());
        Ok(row)
    }}

    async fn update(&self, id: Uuid, input: {pascal}Update) -> Result<{pascal}, RepoError> {{
        let mut guard = self.inner.write().await;
        let row = guard
            .iter_mut()
            .find(|e| e.id == id)
            .ok_or(RepoError::NotFound)?;
        if let Some(v) = input.name {{
            row.name = v;
        }}
        row.updated_at = Utc::now();
        Ok(row.clone())
    }}

    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {{
        let mut guard = self.inner.write().await;
        let before = guard.len();
        guard.retain(|e| e.id != id);
        if guard.len() == before {{
            return Err(RepoError::NotFound);
        }}
        Ok(())
    }}
}}
"#,
            pascal = n.pascal,
            snake = n.snake,
        ),
    )?;
    Ok(())
}

fn write_facade(feature_dir: &Path, n: &Names) -> eyre::Result<()> {
    let dir = feature_dir.join(&n.kebab);
    fs::create_dir_all(dir.join("src"))?;

    write(
        dir.join("Cargo.toml"),
        format!(
            r#"[package]
name = "{kebab}"
version.workspace = true
edition.workspace = true

# Facade — single import root for the `{kebab}` feature.

[dependencies]
{kebab}-proto.workspace = true
{kebab}-memory = {{ workspace = true, optional = true }}
architect = {{ workspace = true, optional = true }}

[features]
default = []
vox = ["{kebab}-proto/vox"]
server-seaorm = ["{kebab}-proto/server"]
server-axum = ["dep:architect", "architect/server-axum"]
backend-memory = ["dep:{kebab}-memory"]
full = [
    "vox",
    "server-seaorm",
    "server-axum",
    "backend-memory",
]
"#,
            kebab = n.kebab,
        ),
    )?;

    write(
        dir.join("src").join("lib.rs"),
        format!(
            r#"//! Facade for the `{kebab}` feature.

pub use {snake}_proto::*;

#[cfg(feature = "backend-memory")]
pub mod backend_memory {{
    pub use {snake}_memory::{pascal}RepoMemory;
}}

#[cfg(feature = "server-axum")]
pub use architect::axum_ws;
"#,
            kebab = n.kebab,
            snake = n.snake,
            pascal = n.pascal,
        ),
    )?;
    Ok(())
}

fn write_spec(feature_dir: &Path, n: &Names) -> eyre::Result<()> {
    let dir = feature_dir.join("spec");
    fs::create_dir_all(&dir)?;

    write(
        dir.join(format!("{}.md", n.kebab)),
        format!(
            r#"+++
title = "{pascal} contract"
description = "Tracey-tracked rules the {pascal}Repo implementation must hold."
weight = 100
+++

Rules are linked to source via `r[impl <id>]` and `r[verify <id>]`
annotations. Run `cargo xtask tracey-validate` to confirm coverage.

r[{snake}.placeholder]
TODO: write a real rule for this feature. Tracey enforces
lowercase-dot-separated IDs and that each rule has at least one impl
+ verify reference.
"#,
            pascal = n.pascal,
            snake = n.snake,
        ),
    )?;
    Ok(())
}

fn write_tests_native(feature_dir: &Path, n: &Names) -> eyre::Result<()> {
    let dir = feature_dir.join("tests").join("native");
    fs::create_dir_all(dir.join("src"))?;

    write(
        dir.join("Cargo.toml"),
        format!(
            r#"[package]
name = "{kebab}-tests-native"
version.workspace = true
edition.workspace = true
authors.workspace = true
license.workspace = true
publish = false

[dependencies]
{kebab}-proto.workspace = true
{kebab}-memory.workspace = true
architect.workspace = true
uuid.workspace = true
tokio = {{ workspace = true, features = ["macros", "rt"] }}
"#,
            kebab = n.kebab,
        ),
    )?;

    write(
        dir.join("src").join("lib.rs"),
        format!(
            r#"//! Native integration tests for the `{kebab}` feature.

#![cfg(test)]

use {snake}_memory::{pascal}RepoMemory;
use {snake}_proto::{{{pascal}Create, {pascal}Repo}};

#[tokio::test]
async fn create_then_get_round_trip() {{
    let r = {pascal}RepoMemory::new();
    let created = r
        .create({pascal}Create {{ name: "alpha".into() }})
        .await
        .unwrap();
    let got = r.get(created.id).await.unwrap();
    assert_eq!(got.id, created.id);
    assert_eq!(got.name, "alpha");
}}
"#,
            kebab = n.kebab,
            snake = n.snake,
            pascal = n.pascal,
        ),
    )?;
    Ok(())
}

// ── Workspace + tracey integration ────────────────────────────────────

fn update_workspace_cargo(repo_root: &Path, n: &Names) -> eyre::Result<()> {
    let path = repo_root.join("Cargo.toml");
    let contents = fs::read_to_string(&path)?;
    let mut doc: DocumentMut = contents.parse()?;

    // Members the cargo workspace builds.
    let members_to_add = [
        format!("features/{}/{}", n.kebab, n.kebab),
        format!("features/{}/{}-proto", n.kebab, n.kebab),
        format!("features/{}/{}-memory", n.kebab, n.kebab),
        format!("features/{}/tests/native", n.kebab),
    ];

    for key in ["members", "default-members"] {
        if let Some(arr) = doc
            .get_mut("workspace")
            .and_then(|w| w.get_mut(key))
            .and_then(|v| v.as_array_mut())
        {
            for m in &members_to_add {
                if !array_contains_str(arr, m) {
                    arr.push(m.as_str());
                }
            }
        }
    }

    // workspace.dependencies entries.
    let deps_to_add = [
        (
            n.kebab.clone(),
            format!("features/{}/{}", n.kebab, n.kebab),
        ),
        (
            format!("{}-proto", n.kebab),
            format!("features/{}/{}-proto", n.kebab, n.kebab),
        ),
        (
            format!("{}-memory", n.kebab),
            format!("features/{}/{}-memory", n.kebab, n.kebab),
        ),
    ];

    if let Some(deps_table) = doc
        .get_mut("workspace")
        .and_then(|w| w.get_mut("dependencies"))
        .and_then(|v| v.as_table_mut())
    {
        for (name, path_rel) in &deps_to_add {
            if !deps_table.contains_key(name) {
                let mut t = Table::new();
                t.set_implicit(false);
                t["path"] = value(path_rel);
                deps_table.insert(name, Item::Value(toml_edit::Value::InlineTable(
                    t.into_inline_table(),
                )));
            }
        }
    }

    fs::write(&path, doc.to_string())?;
    eprintln!("  {} updated Cargo.toml", "✓".green());
    Ok(())
}

fn array_contains_str(arr: &Array, needle: &str) -> bool {
    arr.iter()
        .filter_map(|v| v.as_str())
        .any(|s| s == needle)
}

fn update_tracey_config(repo_root: &Path, n: &Names) -> eyre::Result<()> {
    let path = repo_root.join(".config").join("tracey").join("config.styx");
    if !path.exists() {
        // No tracey config — skip silently.
        return Ok(());
    }
    let mut contents = fs::read_to_string(&path)?;
    let probe = format!("name {}", n.kebab);
    if contents.contains(&probe) {
        return Ok(());
    }

    // Find the closing `)` of the outer specs block. The styx file
    // shape is:
    //   @schema ...
    //
    //   specs (
    //       { ... }
    //       { ... }  <- we want to insert another block here
    //   )
    //
    // Append before the LAST closing `)` on its own line, indented or
    // not. Cheap heuristic: replace the final `\n)\n` with our block + `\n)\n`.
    // Default new features to a single `live` impl, scoped to the
    // memory backend that the scaffold just created. Adding more
    // backends later means adding more impl blocks (e.g. `mock`,
    // `reaper`, `protools`) under the same spec.
    let block = format!(
        r#"
    {{
        name {kebab}
        include (features/{kebab}/spec/**/*.md)
        impls (
            {{
                name live
                include (
                    features/{kebab}/{kebab}-memory/**/*.rs
                )
                exclude (
                    features/{kebab}/**/target/**
                )
                test_include (
                    features/{kebab}/tests/**/*.rs
                )
            }}
        )
    }}
"#,
        kebab = n.kebab,
    );

    // Insert just before the last `)` (the outer specs close).
    if let Some(idx) = contents.rfind(')') {
        contents.insert_str(idx, &block);
        fs::write(&path, contents)?;
        eprintln!("  {} updated .config/tracey/config.styx", "✓".green());
    }
    Ok(())
}

// ── File I/O helper ───────────────────────────────────────────────────

fn write(path: PathBuf, contents: impl AsRef<[u8]>) -> eyre::Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::write(&path, contents)?;
    Ok(())
}
