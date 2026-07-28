//! `task api` — render the self-describing API reference.
//!
//! The CLI links `task-server`, so this renders
//! [`task_server::api_ref::reference`] — the SAME registry fold the
//! server serves at `GET /org/{slug}/api` and the permit gate installs.
//! No server or network involved: the output describes the build in
//! front of you (which is exactly what the committed markdown reference
//! should track).
//!
//! - `task api` — one line per service (name, alias, stream-ness,
//!   method count, schema stamp).
//! - `task api <service>` — one service's methods in full (args,
//!   permit action + resource, stream, audited). Matches the permit
//!   alias (`task`, `vault-sync`), the descriptor name
//!   (`TaskServiceRpc`), or a unique substring of either.
//! - `task api --markdown` — the whole reference as markdown; commit it
//!   as `apps/task/docs/api-reference.md`.
//! - `task api --json` — the exact JSON body `GET /org/{slug}/api`
//!   serves (minus the `org` field the handler stamps in).

use clap::Args;
use task_server::api_ref::{ApiService, reference};

#[derive(Args)]
pub(crate) struct ApiArgs {
    /// Show one service's methods (permit alias, descriptor name, or a
    /// unique substring of either).
    service: Option<String>,

    /// Emit the whole reference as markdown
    /// (`task api --markdown > apps/task/docs/api-reference.md`).
    #[arg(long, conflicts_with = "json")]
    markdown: bool,

    /// Emit the reference as JSON — the same shape `GET
    /// /org/{slug}/api` serves.
    #[arg(long)]
    json: bool,
}

pub(crate) fn run_api(args: ApiArgs) -> eyre::Result<()> {
    if args.json {
        println!(
            "{}",
            serde_json::to_string_pretty(&task_server::api_ref::reference_json())?
        );
        return Ok(());
    }
    let services = reference();
    if args.markdown {
        print!("{}", render_markdown(&services));
        return Ok(());
    }
    match args.service {
        Some(query) => render_one(&services, &query),
        None => {
            render_index(&services);
            Ok(())
        }
    }
}

/// Find a service by alias, descriptor name, or unique substring.
fn find<'a>(services: &'a [ApiService], query: &str) -> Result<&'a ApiService, String> {
    let q = query.to_ascii_lowercase();
    // Exact alias or descriptor name first.
    if let Some(s) = services
        .iter()
        .find(|s| s.alias.is_some_and(|a| a == q) || s.name.to_ascii_lowercase() == q)
    {
        return Ok(s);
    }
    let matches: Vec<&ApiService> = services
        .iter()
        .filter(|s| {
            s.name.to_ascii_lowercase().contains(&q)
                || s.alias.is_some_and(|a| a.contains(&q))
        })
        .collect();
    match matches.as_slice() {
        [one] => Ok(one),
        [] => Err(format!("no mounted service matches `{query}`")),
        many => Err(format!(
            "`{query}` is ambiguous — matches: {}",
            many.iter()
                .map(|s| s.alias.unwrap_or(s.name))
                .collect::<Vec<_>>()
                .join(", ")
        )),
    }
}

fn render_index(services: &[ApiService]) {
    let streams = services.iter().filter(|s| s.stream).count();
    println!(
        "{} services mounted ({} streams, {} rpc). `task api <service>` for methods.",
        services.len(),
        streams,
        services.len() - streams
    );
    println!();
    let width = services
        .iter()
        .map(|s| s.alias.unwrap_or(s.name).len())
        .max()
        .unwrap_or(0);
    for s in services {
        println!(
            "  {:width$}  {:2} method(s)  {}  stamp {}   ({})",
            s.alias.unwrap_or(s.name),
            s.methods.len(),
            if s.stream { "stream" } else { "rpc   " },
            s.stamp,
            s.name,
        );
    }
}

fn render_one(services: &[ApiService], query: &str) -> eyre::Result<()> {
    let s = find(services, query).map_err(|e| eyre::eyre!(e))?;
    println!(
        "{} ({}) — {}, stamp {}",
        s.alias.unwrap_or(s.name),
        s.name,
        if s.stream {
            "#[subscribe] stream service"
        } else {
            "rpc service"
        },
        s.stamp,
    );
    if let Some(doc) = s.doc {
        println!("  {}", doc.trim());
    }
    println!();
    for m in &s.methods {
        let mut tags = Vec::new();
        if m.stream {
            tags.push("stream");
        }
        if m.audited {
            tags.push("audited");
        }
        let tags = if tags.is_empty() {
            String::new()
        } else {
            format!("  [{}]", tags.join(", "))
        };
        println!(
            "  {}({})  — {} on {}{}",
            m.name,
            m.args.join(", "),
            m.action.unwrap_or("?"),
            m.resource.unwrap_or("?"),
            tags,
        );
        if let Some(doc) = m.doc {
            if let Some(line) = doc.trim().lines().next() {
                println!("      {line}");
            }
        }
    }
    Ok(())
}

fn render_markdown(services: &[ApiService]) -> String {
    use std::fmt::Write as _;
    let streams = services.iter().filter(|s| s.stream).count();
    let mut out = String::new();
    let _ = writeln!(out, "# Task API reference");
    let _ = writeln!(out);
    let _ = writeln!(
        out,
        "<!-- GENERATED — do not edit by hand. Regenerate with:\n\
         \x20    cargo run -p task-cli -- api --markdown > apps/task/docs/api-reference.md\n\
         \x20    Source: `task_server::permits::mounts()` (apps/task/server/src/permits.rs),\n\
         \x20    the single registry the router, permit gate, and schema stamps derive from.\n\
         \x20    Served live at `GET /org/{{slug}}/api`. -->"
    );
    let _ = writeln!(out);
    let _ = writeln!(
        out,
        "{} services mounted: {} plain RPC, {} `#[subscribe]` streams. Every method \
         lists its permit — the `<action>` on `<resource>` the permissions gate \
         checks (see `apps/task/server/src/permits.rs`). `audited` methods emit an \
         audit line even when allowed. A `stream` method takes a `Tx` sink and \
         pushes to the caller instead of returning once.",
        services.len(),
        services.len() - streams,
        streams,
    );
    let _ = writeln!(out);
    for s in services {
        let _ = writeln!(
            out,
            "## `{}` ({}){}",
            s.alias.unwrap_or(s.name),
            s.name,
            if s.stream { " — stream" } else { "" }
        );
        let _ = writeln!(out);
        let _ = writeln!(out, "Schema stamp: `{}`", s.stamp);
        let _ = writeln!(out);
        let _ = writeln!(out, "| method | args | permit | notes |");
        let _ = writeln!(out, "|---|---|---|---|");
        for m in &s.methods {
            let mut notes = Vec::new();
            if m.stream {
                notes.push("stream");
            }
            if m.audited {
                notes.push("audited");
            }
            let _ = writeln!(
                out,
                "| `{}` | {} | `{}` on `{}` | {} |",
                m.name,
                if m.args.is_empty() {
                    "—".to_owned()
                } else {
                    m.args
                        .iter()
                        .map(|a| format!("`{a}`"))
                        .collect::<Vec<_>>()
                        .join(", ")
                },
                m.action.unwrap_or("?"),
                m.resource.unwrap_or("?"),
                if notes.is_empty() {
                    "—".to_owned()
                } else {
                    notes.join(", ")
                },
            );
        }
        let _ = writeln!(out);
    }
    out
}
