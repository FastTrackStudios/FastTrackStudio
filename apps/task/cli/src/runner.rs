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
    /// What this runner may take right now — the queue as the
    /// runner sees it, filtered by capability, scope and free slots.
    ///
    /// `--why` also prints, for each ticket it cannot take, the
    /// reason — which is how you answer "why is my runner idle?".
    Takeable {
        /// Runner id. Defaults to this machine's hostname.
        #[arg(long)]
        id: Option<String>,
        /// How many tickets this runner already holds.
        #[arg(long, default_value_t = 0)]
        in_flight: u32,
        /// Also explain every refusal.
        #[arg(long)]
        why: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Agent-ready tickets nothing in the fleet can take.
    ///
    /// A ticket no live runner satisfies must be reported, not left
    /// sitting in the queue looking available. Malformed tickets —
    /// a capability nobody could ever offer because it is a typo —
    /// are listed separately, because the fix is editing the ticket
    /// rather than adding a machine.
    Unroutable {
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

        RunnerCmd::Takeable {
            id,
            in_flight,
            why,
            org,
            server,
        } => {
            let id = id.unwrap_or_else(hostname);
            let (slug, url) = ctx(org, server)?;
            let backends: BackendsClient = establish_for_url(&url).await?;
            let me = backends
                .list_backends()
                .await?
                .into_iter()
                .find(|b| b.id == id)
                .ok_or_else(|| eyre::eyre!("runner `{id}` is not registered"))?;

            let tickets = agent_ready_tickets(&url).await?;
            let refs = ticket_refs(&tickets, &slug);

            let takeable = agent_proto::routing::takeable(&me.runner, &refs, in_flight);
            if takeable.is_empty() {
                println!("(nothing takeable)");
            }
            for tid in &takeable {
                if let Some(t) = tickets.iter().find(|t| t.id == *tid) {
                    println!("{}  {}", crate::shared::short_uuid(&t.id), t.title);
                }
            }

            if why {
                for r in agent_proto::routing::refusals(&me.runner, &refs, in_flight) {
                    if let Some(t) = tickets.iter().find(|t| t.id == r.ticket) {
                        println!(
                            "skip {}  {}  — {}",
                            crate::shared::short_uuid(&t.id),
                            t.title,
                            r.reason
                        );
                    }
                }
            }
        }

        RunnerCmd::Unroutable { org, server } => {
            let (slug, url) = ctx(org, server)?;
            let backends: BackendsClient = establish_for_url(&url).await?;

            // Live runners only: a registration nobody is behind
            // must not make a ticket look routable.
            let mut live = Vec::new();
            for b in backends.list_backends().await? {
                if backends.backend_health(b.id.clone()).await?.reachable {
                    live.push(b.runner);
                }
            }

            let tickets = agent_ready_tickets(&url).await?;
            let refs = ticket_refs(&tickets, &slug);

            let stuck = agent_proto::routing::unroutable(&refs, &live);
            let bad = agent_proto::routing::malformed(&refs);

            if stuck.is_empty() && bad.is_empty() {
                println!("(everything agent-ready can be taken by some runner)");
            }
            for (tid, reason) in stuck {
                if let Some(t) = tickets.iter().find(|t| t.id == tid) {
                    println!(
                        "unroutable  {}  {}  — {reason}",
                        crate::shared::short_uuid(&t.id),
                        t.title
                    );
                }
            }
            for (tid, reason) in bad {
                if let Some(t) = tickets.iter().find(|t| t.id == tid) {
                    println!(
                        "malformed   {}  {}  — {reason}",
                        crate::shared::short_uuid(&t.id),
                        t.title
                    );
                }
            }
        }
    }
    Ok(())
}

fn ctx(org: Option<String>, server: Option<String>) -> eyre::Result<(String, String)> {
    let slug = resolve_active_org(org)?;
    let url = resolve_org_vox_url(server, &slug);
    Ok((slug, url))
}

/// Open, unblocked, unclaimed tickets tagged `ready-for-agent`.
///
/// The same frontier `issue ready` computes, narrowed to the agent
/// lane: a human-only ticket is not a routing failure.
async fn agent_ready_tickets(url: &str) -> eyre::Result<Vec<task::TaskInfo>> {
    let client = crate::task_cmd::connect_task_client(url).await?;
    let rows = client
        .list()
        .await
        .map_err(|e| eyre::eyre!("list: {e:?}"))?;

    let by_id: std::collections::HashMap<uuid::Uuid, &task::TaskInfo> =
        rows.iter().map(|t| (t.id, t)).collect();

    let done = |t: &task::TaskInfo| {
        matches!(
            task::Status::from_str(&t.status),
            Some(task::Status::Done | task::Status::Cancelled)
        )
    };

    Ok(rows
        .iter()
        .filter(|t| !done(t))
        .filter(|t| task::has_triage_label(t, task::TriageLabel::ReadyForAgent))
        .filter(|t| {
            // Unclaimed.
            t.workflow
                .as_ref()
                .is_none_or(|w| w.assignees.0.is_empty())
        })
        .filter(|t| {
            // Every blocker closed.
            let blockers = t.workflow.as_ref().map_or(&[][..], |w| &w.blockers.0[..]);
            blockers
                .iter()
                .all(|b| by_id.get(b).is_some_and(|b| done(b)))
        })
        .cloned()
        .collect())
}

fn ticket_refs<'a>(
    tickets: &'a [task::TaskInfo],
    org: &'a str,
) -> Vec<agent_proto::routing::TicketRef<'a>> {
    tickets
        .iter()
        .map(|t| agent_proto::routing::TicketRef {
            id: t.id,
            capabilities: t
                .workflow
                .as_ref()
                .map_or(&[][..], |w| &w.capabilities.0[..]),
            org,
            project: t.project_id,
        })
        .collect()
}
