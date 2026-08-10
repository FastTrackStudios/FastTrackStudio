//! `task runner` — register this machine as a runner, list the
//! registry, heartbeat, and deregister.
//!
//! A runner is the thing that actually executes agent work. It
//! declares what it can do; the server routes tickets by matching
//! against that declaration. Capabilities are a closed set —
//! `records`, `shell`, `build`, `repo:<owner>/<name>` — and `build`
//! is deliberately separate from `shell` so a machine can read
//! source without being allowed to compile.

use agent_proto::backend::{AgentBackend, BackendKind};
use agent_proto::runner::{RunnerProfile, RunnerScope, parse_capabilities};
use agent_proto::service::backends::BackendsClient;
use chrono::Utc;
use clap::Subcommand;

use crate::establish_for_url;
use crate::resolve_active_org;
use crate::resolve_org_vox_url;

#[derive(Subcommand, Debug)]
pub enum RunnerCmd {
    /// Register this machine (or update its registration).
    ///
    /// Re-running with different flags updates in place — the id is
    /// the identity, so a runner that restarts does not duplicate.
    Register {
        /// Stable runner id. Defaults to this machine's hostname,
        /// which is almost always what you want.
        #[arg(long)]
        id: Option<String>,
        /// Human-facing label. Defaults to the id.
        #[arg(long)]
        label: Option<String>,
        /// Repeatable capability: `records`, `shell`, `build`, or
        /// `repo:<owner>/<name>`. Anything else is refused.
        #[arg(long = "cap", value_name = "CAPABILITY")]
        caps: Vec<String>,
        /// Repeatable org slug this runner serves. Omit for any.
        #[arg(long = "scope-org", value_name = "SLUG")]
        scope_orgs: Vec<String>,
        /// Repeatable project id this runner serves. Omit for any.
        #[arg(long = "scope-project", value_name = "UUID")]
        scope_projects: Vec<uuid::Uuid>,
        /// How many tickets to hold at once. `0` registers the
        /// runner but takes nothing — the way to drain a machine
        /// without deregistering it.
        #[arg(long, default_value_t = 2)]
        max_concurrent: u32,
        /// Send a heartbeat straight after registering, so the
        /// runner is immediately routable.
        #[arg(long, default_value_t = true)]
        beat: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// List every registered runner and whether it is live.
    List {
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Tell the server this runner is alive.
    Beat {
        /// Runner id. Defaults to this machine's hostname.
        #[arg(long)]
        id: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Deregister a runner.
    Remove {
        id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
}

/// This machine's name, used as the default runner id.
fn hostname() -> String {
    std::env::var("HOSTNAME")
        .or_else(|_| std::env::var("HOST"))
        .ok()
        .filter(|h| !h.is_empty())
        .or_else(|| {
            std::fs::read_to_string("/etc/hostname")
                .ok()
                .map(|h| h.trim().to_string())
                .filter(|h| !h.is_empty())
        })
        .unwrap_or_else(|| "runner".into())
}

async fn client(org: Option<String>, server: Option<String>) -> eyre::Result<BackendsClient> {
    let slug = resolve_active_org(org)?;
    let url = resolve_org_vox_url(server, &slug);
    establish_for_url(&url).await
}

pub(crate) async fn run_runner(cmd: RunnerCmd) -> eyre::Result<()> {
    match cmd {
        RunnerCmd::Register {
            id,
            label,
            caps,
            scope_orgs,
            scope_projects,
            max_concurrent,
            beat,
            org,
            server,
        } => {
            let id = id.unwrap_or_else(hostname);
            // Parse before dialling: a bad capability should fail
            // here, naming the token, not as a wire error.
            let capabilities = parse_capabilities(&caps)?;

            let backend = AgentBackend {
                id: id.clone(),
                label: label.unwrap_or_else(|| id.clone()),
                kind: BackendKind::CliBridge,
                config_json: String::new(),
                registered_at: Utc::now(),
                last_seen: None,
                runner: RunnerProfile {
                    id: id.clone(),
                    capabilities,
                    scope: RunnerScope {
                        orgs: scope_orgs,
                        projects: scope_projects,
                    },
                    max_concurrent,
                },
            };

            let c = client(org, server).await?;
            let saved = c.upsert_backend(backend).await?;
            if beat {
                c.heartbeat_backend(saved.id.clone()).await?;
            }

            println!("registered {}", saved.id);
            println!(
                "  capabilities: {}",
                if saved.runner.capabilities.is_empty() {
                    "(none)".to_string()
                } else {
                    saved
                        .runner
                        .capabilities
                        .iter()
                        .map(agent_proto::runner::Capability::as_string)
                        .collect::<Vec<_>>()
                        .join(", ")
                }
            );
            println!("  max concurrent: {}", saved.runner.max_concurrent);
            let scope = &saved.runner.scope;
            println!(
                "  scope: {}",
                if scope.orgs.is_empty() && scope.projects.is_empty() {
                    "unrestricted".to_string()
                } else {
                    format!(
                        "orgs=[{}] projects=[{}]",
                        scope.orgs.join(", "),
                        scope
                            .projects
                            .iter()
                            .map(ToString::to_string)
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
            );
            if beat {
                println!("  heartbeat: sent (routable now)");
            }
        }

        RunnerCmd::List { org, server } => {
            let c = client(org, server).await?;
            let runners = c.list_backends().await?;
            if runners.is_empty() {
                println!("(no runners registered)");
                return Ok(());
            }
            for r in runners {
                let health = c.backend_health(r.id.clone()).await?;
                let state = if health.reachable { "live " } else { "stale" };
                let caps = r
                    .runner
                    .capabilities
                    .iter()
                    .map(agent_proto::runner::Capability::as_string)
                    .collect::<Vec<_>>()
                    .join(",");
                println!(
                    "{state}  {:<20} x{:<3} {caps}",
                    r.id, r.runner.max_concurrent
                );
            }
        }

        RunnerCmd::Beat { id, org, server } => {
            let id = id.unwrap_or_else(hostname);
            let c = client(org, server).await?;
            c.heartbeat_backend(id.clone()).await?;
            println!("beat {id}");
        }

        RunnerCmd::Remove { id, org, server } => {
            let c = client(org, server).await?;
            c.remove_backend(id.clone()).await?;
            println!("removed {id}");
        }
    }
    Ok(())
}
