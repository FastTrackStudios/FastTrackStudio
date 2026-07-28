//! `task org …` — federated on-disk org roots.
//!
//! Moved verbatim out of `main.rs`; behaviour unchanged.

use clap::Subcommand;

use crate::establish_server_client;

#[derive(Subcommand)]
pub(crate) enum OrgCmd {
    /// Ask the server to scaffold a new org. Connects to
    /// `<server>/server/vox` (`task-server` exposes the
    /// `OrgManagementService` RPC there) and the server
    /// writes the `<data_root>/orgs/<slug>/` dir + opens its
    /// per-org SQLite DBs + hot-adds it to the live
    /// dispatcher. No filesystem mutation runs on the client.
    ///
    /// Authorization: when the server has no orgs hosted yet
    /// it's in bootstrap mode and accepts this call
    /// unauthenticated. Otherwise the active session token
    /// must be a valid session against the server's home org.
    Create {
        /// `[a-z0-9-]`, 1-64 chars, no leading/trailing `-`.
        slug: String,
        /// Human-facing display name. Free-form UTF-8.
        #[arg(long)]
        name: String,
        /// Mark this org as the identity anchor (home).
        /// Only one home per server is allowed.
        #[arg(long)]
        home: bool,
        /// Server URL. Defaults to the active session's home
        /// server URL when set, else `http://127.0.0.1:18080`.
        #[arg(long)]
        server: Option<String>,
    },
    /// Local fallback: scaffold an org by writing directly to
    /// `<data-root>/orgs/<slug>/`. Bypasses the server — only
    /// useful when administering the server's filesystem
    /// out-of-band (recovery, migration). Prefer
    /// `task org create` for normal seeding.
    Init {
        /// `[a-z0-9-]`, 1-64 chars, no leading/trailing `-`.
        slug: String,
        /// Human-facing display name. Free-form UTF-8.
        #[arg(long)]
        name: String,
        /// Mark this org as the identity anchor (home).
        #[arg(long)]
        home: bool,
    },
    /// Ask the server to list its hosted orgs (the wire
    /// equivalent of `/.well-known/task-server.json`).
    /// Defaults to the active session's home server URL.
    List {
        #[arg(long)]
        server: Option<String>,
    },
}

pub(crate) async fn run_org(cmd: OrgCmd) -> eyre::Result<()> {
    match cmd {
        OrgCmd::Create {
            slug,
            name,
            home,
            server,
        } => {
            let token = crate::session_store::load()?
                .and_then(|s| s.servers.get(&s.active).map(|e| e.token.clone()))
                .unwrap_or_default();
            let (client, url): (org_proto::OrgManagementServiceClient, _) =
                establish_server_client(server.as_deref()).await?;
            let manifest = client
                .create_org(org_proto::CreateOrgRequest {
                    session_token: token,
                    slug: slug.clone(),
                    display_name: name,
                    is_home: home,
                })
                .await
                .map_err(|e| eyre::eyre!("create_org: {e:?}"))?;
            println!("Server created org `{slug}`");
            println!("  id:         {}", manifest.id);
            println!("  name:       {}", manifest.display_name);
            println!("  is_home:    {}", manifest.is_home);
            println!("  server vox: {url}");
        }
        OrgCmd::Init { slug, name, home } => {
            let root = org_proto::DataRoot::from_env()
                .map_err(|e| eyre::eyre!("resolve data root: {e}"))?;
            root.ensure()
                .map_err(|e| eyre::eyre!("ensure data root: {e}"))?;
            let org = root
                .init_org(&slug, &name, home)
                .map_err(|e| eyre::eyre!("init org: {e}"))?;
            let manifest = org
                .manifest()
                .map_err(|e| eyre::eyre!("load fresh manifest: {e}"))?;
            println!(
                "Initialized org `{}` at {} (LOCAL — bypassing server)",
                slug,
                org.path().display()
            );
            println!("  id:         {}", manifest.id);
            println!("  name:       {}", manifest.display_name);
            println!("  is_home:    {}", manifest.is_home);
            println!("  vault:      {}", org.vault_dir().display());
            println!("  auth.db:    {}", org.auth_db().display());
            println!("  timer.db:   {}", org.timer_db().display());
            println!("  finance.db: {}", org.finance_db().display());
            println!("\nNote: prefer `task org create` so the server is the source of truth.");
        }
        OrgCmd::List { server } => {
            let (client, url): (org_proto::OrgManagementServiceClient, _) =
                establish_server_client(server.as_deref()).await?;
            let orgs = client
                .list_orgs()
                .await
                .map_err(|e| eyre::eyre!("list_orgs: {e:?}"))?;
            if orgs.is_empty() {
                println!("(server has no orgs hosted at {url})");
                return Ok(());
            }
            for m in orgs {
                let badge = if m.is_home { " [home]" } else { "" };
                println!("{}{}  {}  ({})", m.slug, badge, m.display_name, m.id);
                if !m.federation_url.is_empty() {
                    println!("    federation: {}", m.federation_url);
                }
            }
        }
    }
    Ok(())
}
