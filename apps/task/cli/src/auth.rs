//! `task auth …` — architect-auth sign-in / session / org selection.
//!
//! Moved verbatim out of `main.rs`; behaviour unchanged.

use clap::Subcommand;

use crate::establish_for_url;
use crate::global_org;
use crate::resolve_org_vox_url;
use crate::resolve_server_base;
use crate::run;

#[derive(Subcommand)]
pub(crate) enum AuthCmd {
    /// Sign in over the server's per-org `AuthService`
    /// (`<server>/org/<slug>/vox`) — works against a remote
    /// server with NO local org dir. Persists the session
    /// (token + user + org + server URL) to
    /// `$XDG_DATA_HOME/task/session.json`, so subsequent
    /// commands need nothing else: the stored server URL is
    /// used whenever `--server` / `TASK_VOX_URL` is absent.
    Login {
        #[arg(long)]
        email: String,
        #[arg(long)]
        password: String,
    },
    /// Create a new email/password user over the org's
    /// `AuthService` and persist the resulting session — like
    /// `login`, purely remote. The first user signed up in a
    /// fresh org is its de-facto owner — architect-auth has no
    /// separate ownership concept yet. Use `--org <slug>` /
    /// `--server <url>` to target a specific org.
    Signup {
        #[arg(long)]
        email: String,
        #[arg(long)]
        password: String,
        /// Optional username — needed if you want
        /// `SignInUsername` to work later. Free-form, but the
        /// architect-auth username uniqueness constraint
        /// applies per `auth.sqlite`.
        #[arg(long)]
        username: Option<String>,
        /// Optional display name. Falls back to the email
        /// localpart in the UI when empty.
        #[arg(long)]
        name: Option<String>,
    },
    /// Print the active session (email, user id, org id).
    Whoami,
    /// Switch the active session entry (server profile) without
    /// re-authenticating. `task auth whoami` lists the stored
    /// entries; reference one by key (`slug@host`), bare slug, or
    /// any unique prefix. Subsequent commands talk to that
    /// entry's server unless `--server` / `TASK_VOX_URL` says
    /// otherwise.
    Use {
        /// Session reference — key, slug, or unique prefix.
        session: String,
    },
    /// Invalidate the active session server-side AND remove
    /// the local session file.
    Logout,
    /// Org membership + selection.
    #[command(subcommand)]
    Org(AuthOrgCmd),
    /// List every user in the active org's `auth.sqlite`.
    /// Useful when you need a user_id to pass to
    /// `timer reassign-user --to`.
    Users,
}

#[derive(Subcommand)]
pub(crate) enum AuthOrgCmd {
    /// List orgs the signed-in user is a member of.
    List,
    /// Set the active org for subsequent commands. Updates
    /// both the local session file and the server-side
    /// `auth_session.active_organization_id`.
    Use {
        /// Org reference — UUID, slug, or name (exact / unique
        /// prefix), matched against your memberships.
        org_id: String,
    },
}

/// Open `ArchitectAuth` against a specific org's
/// `auth.sqlite` — same DB the server uses for that org.
/// CLI ↔ server interop hinges on matching the
/// `<data_root>/orgs/<slug>/auth.sqlite` resolver plus
/// `DEFAULT_AUTH_SECRET`.
async fn open_local_auth(
    auth_db_path: &std::path::Path,
) -> eyre::Result<architect_auth::ArchitectAuth<architect_auth::db::AuthSeaOrmStorage>> {
    use architect_auth::db::{AuthSeaOrmStorage, Migrator as AuthMigrator};
    use sea_orm::Database;
    use sea_orm_migration::MigratorTrait;
    let db_url = format!("sqlite://{}?mode=rwc", auth_db_path.display());
    let db = Database::connect(&db_url)
        .await
        .map_err(|e| eyre::eyre!("connect auth db `{db_url}`: {e}"))?;
    AuthMigrator::up(&db, None)
        .await
        .map_err(|e| eyre::eyre!("auth migrations: {e}"))?;
    let storage = AuthSeaOrmStorage::new(db);
    architect_auth::ArchitectAuth::builder()
        .secret(crate::session_store::DEFAULT_AUTH_SECRET)
        .storage(storage)
        .build()
        .map_err(|e| eyre::eyre!("build ArchitectAuth: {e}"))
}

/// One hosted org row from a server's discovery document.
#[derive(Debug, serde::Deserialize)]
struct HostedOrg {
    slug: String,
    #[serde(default)]
    is_home: bool,
}

/// `ws(s)://` vox base → `http(s)://` origin+path for the same
/// server (the well-known + health endpoints live on plain HTTP).
pub(crate) fn ws_base_to_http(base: &str) -> String {
    base.replacen("wss://", "https://", 1)
        .replacen("ws://", "http://", 1)
}

/// Fetch the server's `/.well-known/task-server.json` org list —
/// the remote replacement for scanning `<data_root>/orgs/`.
async fn fetch_hosted_orgs(base: &str) -> eyre::Result<Vec<HostedOrg>> {
    let url = format!(
        "{}/.well-known/task-server.json",
        ws_base_to_http(&crate::session_store::normalize_server_base(base))
    );
    let doc: serde_json::Value = reqwest::get(&url)
        .await
        .map_err(|e| eyre::eyre!("fetch {url}: {e}"))?
        .json()
        .await
        .map_err(|e| eyre::eyre!("parse {url}: {e}"))?;
    let orgs = doc
        .get("orgs")
        .cloned()
        .ok_or_else(|| eyre::eyre!("{url}: no `orgs` field"))?;
    serde_json::from_value(orgs).map_err(|e| eyre::eyre!("{url}: bad `orgs` shape: {e}"))
}

/// Resolve which (org slug, server base) the remote auth verbs
/// operate on. Purely remote — requires NO local org dir:
///
/// - server: [`resolve_server_base`] precedence (flag > env >
///   session > localhost).
/// - slug: `--org` > the session entry for that server > the
///   server's well-known org list (its home org, or the single
///   hosted org).
///
/// When discovery works, an unknown slug fails here with the
/// hosted list — clearer than a raw vox connect error. When the
/// well-known endpoint is unreachable the vox connect downstream
/// reports the connection failure with its own taxonomy.
async fn resolve_auth_target(org_override: Option<&str>) -> eyre::Result<(String, String)> {
    let base = resolve_server_base(None);
    let hosted = fetch_hosted_orgs(&base).await.ok();
    let chosen = if let Some(s) = org_override.map(str::to_owned).or_else(global_org) {
        Some(s)
    } else if let Some((_, entry)) = crate::session_store::load()?
        .as_ref()
        .and_then(|s| s.entry_for_server(&base))
    {
        Some(entry.slug.clone())
    } else {
        // No flag, no session: let the server disambiguate — its
        // home org, or the only org it hosts.
        hosted.as_ref().and_then(|orgs| {
            orgs.iter()
                .find(|o| o.is_home)
                .or_else(|| (orgs.len() == 1).then(|| &orgs[0]))
                .map(|o| o.slug.clone())
        })
    };
    let Some(slug) = chosen else {
        return Err(crate::errors::usage("resolve org for auth")
            .cause(format!(
                "no `--org` given and nothing to infer it from ({base})"
            ))
            .hint("pass --org <slug> (see `task org list --server …` for what the server hosts)")
            .report());
    };
    if let Some(orgs) = &hosted {
        if !orgs.iter().any(|o| o.slug == slug) {
            let names: Vec<&str> = orgs.iter().map(|o| o.slug.as_str()).collect();
            return Err(crate::errors::not_found("resolve org on server", &slug)
                .cause(format!("{base} hosts: {}", names.join(", ")))
                .hint("pass --org <slug> from that list, or `task org create` it first")
                .report());
        }
    }
    Ok((slug, base))
}

pub(crate) async fn run_auth(cmd: AuthCmd, org_override: Option<&str>) -> eyre::Result<()> {
    use architect_auth::commands::CurrentSession;
    use architect_auth::proto::{AuthServiceClient, SignInEmailPassword, SignUpEmailPassword};
    match cmd {
        AuthCmd::Signup {
            email,
            password,
            username,
            name,
        } => {
            // Remote-first: sign up over the org's AuthService —
            // the same per-org vox endpoint every other service
            // rides. No local org dir required.
            let (slug, base) = resolve_auth_target(org_override).await?;
            let url = resolve_org_vox_url(Some(base.clone()), &slug);
            let client: AuthServiceClient = establish_for_url(&url).await?;
            let bundle = client
                .sign_up_email_password(SignUpEmailPassword {
                    email: email.clone(),
                    password,
                    name: name.clone(),
                    username: username.clone(),
                    image: None,
                    metadata_json: None,
                    ip_address: None,
                    user_agent: Some("task-cli".into()),
                })
                .await
                .map_err(|e| eyre::eyre!("sign up: {e}"))?;
            let resolved_email = bundle.user.email.clone().unwrap_or_else(|| email.clone());
            // Persist the session keyed by (server, org) — same
            // shape as `Login` so subsequent commands work
            // without a follow-up `task auth login`.
            let mut sess = crate::session_store::load()?.unwrap_or_else(crate::session_store::CliSession::empty);
            let key = sess.record_login(
                &slug,
                &base,
                bundle.user.id,
                resolved_email.clone(),
                bundle.token.clone(),
            );
            crate::session_store::save(&sess)?;
            println!(
                "Created user {} ({}) in org `{slug}`",
                resolved_email, bundle.user.id,
            );
            if let Some(u) = username {
                println!("  username: {u}");
            }
            if let Some(n) = name {
                println!("  name:     {n}");
            }
            println!("  server:   {base}");
            println!("  session:  {key}");
        }
        AuthCmd::Login { email, password } => {
            // Remote-first sign-in over the org's AuthService.
            // The org is resolved via the server's well-known
            // document — no `task org init` needed on this box.
            let (slug, base) = resolve_auth_target(org_override).await?;
            let url = resolve_org_vox_url(Some(base.clone()), &slug);
            let client: AuthServiceClient = establish_for_url(&url).await?;
            let bundle = client
                .sign_in_email_password(SignInEmailPassword {
                    email: email.clone(),
                    password,
                    ip_address: None,
                    user_agent: Some("task-cli".into()),
                })
                .await
                .map_err(|e| eyre::eyre!("sign in: {e}"))?;
            let resolved_email = bundle.user.email.clone().unwrap_or_else(|| email.clone());
            // Multi-server session: insert/update the entry keyed
            // by (server, org) and make it active. The stored
            // server URL is what later invocations resolve when
            // neither `--server` nor `TASK_VOX_URL` is set.
            let mut sess = crate::session_store::load()?.unwrap_or_else(crate::session_store::CliSession::empty);
            let key = sess.record_login(
                &slug,
                &base,
                bundle.user.id,
                resolved_email.clone(),
                bundle.token.clone(),
            );
            crate::session_store::save(&sess)?;
            println!(
                "Signed in as {} ({}) on org `{slug}`",
                resolved_email, bundle.user.id,
            );
            println!("  server:   {base}");
            println!("  session:  {key}");
            if let Some(member_org) = bundle.session.active_organization_id {
                println!("Architect-auth active membership: {member_org}");
            }
        }
        AuthCmd::Whoami => match crate::session_store::load()? {
            Some(s) => {
                println!(
                    "home:   {}",
                    if s.home.is_empty() {
                        "(none)"
                    } else {
                        s.home.as_str()
                    }
                );
                println!("active: {}", s.active);
                for (key, entry) in &s.servers {
                    let marker = if *key == s.active { "*" } else { " " };
                    println!(
                        "{marker} {key:<28}  org={}  {}  {}  server={}",
                        entry.slug, entry.email, entry.user_id, entry.url
                    );
                }
                // Where the NEXT command will go, after the full
                // precedence fold (flag > env > session > default).
                println!("server: {} (this invocation)", resolve_server_base(None));
                println!("session: {}", crate::session_store::session_path()?.display());
            }
            None => {
                println!("Not signed in. Run `task auth login --email … --password …`.");
            }
        },
        AuthCmd::Use { session } => {
            let Some(mut sess) = crate::session_store::load()? else {
                return Err(crate::errors::usage("auth use")
                    .cause("no stored session")
                    .hint("run `task auth login` first")
                    .report());
            };
            let key = match_session_entry(&sess, &session)?;
            sess.active = key.clone();
            crate::session_store::save(&sess)?;
            let entry = &sess.servers[&key];
            println!(
                "Active session: {key} — org `{}` on {} ({})",
                entry.slug, entry.url, entry.email
            );
        }
        AuthCmd::Logout => {
            let Some(mut sess) = crate::session_store::load()? else {
                println!("Not signed in — nothing to do.");
                return Ok(());
            };
            // Which entry? `--org` picks by slug (preferring the
            // entry on the currently-resolved server); default is
            // the active entry. Other servers stay linked.
            let key = match org_override.map(str::to_owned).or_else(global_org) {
                Some(slug) => {
                    let base = resolve_server_base(None);
                    sess.servers
                        .iter()
                        .find(|(_, e)| e.slug == slug && crate::session_store::same_server(&e.url, &base))
                        .or_else(|| sess.servers.iter().find(|(_, e)| e.slug == slug))
                        .map(|(k, _)| k.clone())
                        .ok_or_else(|| {
                            crate::errors::not_found("logout", &slug)
                                .cause("no stored session for that org")
                                .hint("`task auth whoami` lists the signed-in sessions")
                                .report()
                        })?
                }
                None => sess.active.clone(),
            };
            if let Some(entry) = sess.servers.remove(&key) {
                // Server-side revoke, best effort — over the
                // entry's OWN server (remote logout). Legacy
                // `"local"` entries go straight at the org's
                // on-disk auth.sqlite.
                let revoked: eyre::Result<()> = if entry.url == crate::session_store::LOCAL_URL {
                    revoke_local_session(&entry).await
                } else {
                    let url = resolve_org_vox_url(Some(entry.url.clone()), &entry.slug);
                    match Box::pin(establish_for_url::<AuthServiceClient>(&url)).await {
                        Ok(client) => client
                            .sign_out(entry.token.clone())
                            .await
                            .map_err(|e| eyre::eyre!("{e}")),
                        Err(e) => Err(e),
                    }
                };
                if let Err(e) = revoked {
                    eprintln!("warning: server-side sign out failed: {e:#}");
                }
                println!("Signed out of `{}` ({}).", entry.slug, entry.url);
            } else {
                println!("No stored session under `{key}`.");
            }
            // If no servers left, clear the file entirely; else
            // write the shrunken session back.
            if sess.servers.is_empty() {
                crate::session_store::clear()?;
            } else {
                // Active falls back to home if home is still
                // present, otherwise pick the first remaining
                // server.
                if !sess.servers.contains_key(&sess.active) {
                    sess.active = if sess.servers.contains_key(&sess.home) {
                        sess.home.clone()
                    } else {
                        sess.servers.keys().next().cloned().unwrap_or_default()
                    };
                }
                crate::session_store::save(&sess)?;
            }
        }
        AuthCmd::Org(AuthOrgCmd::List) => {
            let ctx = local_org_ctx(org_override, "auth org list")?;
            let auth_db_path = ctx.root.auth_db();
            let Some(sess) = crate::session_store::load()? else {
                return Err(eyre::eyre!("not signed in — run `task auth login` first"));
            };
            let Some(active_entry) = sess.active_server() else {
                return Err(eyre::eyre!(
                    "no active server entry in session — run `task auth login --org {} …` first",
                    ctx.root.slug()
                ));
            };
            let auth = open_local_auth(&auth_db_path).await?;
            // Verify session still valid + refresh user_id.
            let bundle = auth
                .current_session(CurrentSession {
                    token: active_entry.token.clone(),
                })
                .await
                .map_err(|e| eyre::eyre!("session: {e}"))?;
            let memberships = list_user_memberships(bundle.user.id, &auth_db_path).await?;
            if memberships.is_empty() {
                println!("(no org memberships)");
            }
            for (member, org) in memberships {
                println!(
                    "  {}  {}  ({})",
                    member.organization_id, org.name, member.role
                );
            }
        }
        AuthCmd::Org(AuthOrgCmd::Use { org_id }) => {
            let ctx = local_org_ctx(org_override, "auth org use")?;
            let auth_db_path = ctx.root.auth_db();
            let Some(sess) = crate::session_store::load()? else {
                return Err(eyre::eyre!("not signed in — run `task auth login` first"));
            };
            let Some(active_entry) = sess.active_server() else {
                return Err(eyre::eyre!("no active server in session"));
            };
            // Resolve the reference against the user's memberships:
            // uuid / id prefix, slug, or name (exact / unique prefix)
            // — same matcher as every other entity flag. Doubles as
            // the membership check (non-members never match).
            let memberships = list_user_memberships(active_entry.user_id, &auth_db_path).await?;
            let cands: Vec<crate::json_out::Candidate> = memberships
                .iter()
                .map(|(m, o)| (m.organization_id, o.name.clone(), o.slug.clone()))
                .collect();
            let resolved = match crate::json_out::match_entity(&cands, &org_id, "organization") {
                Ok(i) => cands[i].0,
                Err(fail) => {
                    return Err(fail.into_report("organization", &org_id));
                }
            };
            update_session_active_org(&active_entry.token, Some(resolved), &auth_db_path).await?;
            println!("Architect-auth active membership set to {resolved}");
        }
        AuthCmd::Users => {
            use architect_auth::db::AuthUserEntity;
            use sea_orm::{Database, EntityTrait};
            let ctx = local_org_ctx(org_override, "auth users")?;
            let auth_db_path = ctx.root.auth_db();
            if !auth_db_path.exists() {
                return Err(eyre::eyre!("no auth.sqlite at {}", auth_db_path.display()));
            }
            let url = format!("sqlite://{}?mode=ro", auth_db_path.display());
            let db = Database::connect(&url)
                .await
                .map_err(|e| eyre::eyre!("open {url}: {e}"))?;
            let users = AuthUserEntity::find()
                .all(&db)
                .await
                .map_err(|e| eyre::eyre!("query auth_users: {e}"))?;
            if users.is_empty() {
                println!("(no users)");
            }
            println!("{:<38}  {:<24}  email", "user_id", "name");
            for u in users {
                println!(
                    "{:<38}  {:<24}  {}",
                    u.id,
                    u.name.unwrap_or_default(),
                    u.email.unwrap_or_default()
                );
            }
        }
    }
    Ok(())
}

/// Resolve a `task auth use` reference against the stored session
/// entries: exact key, exact slug (unique), then unique prefix of
/// either. Ambiguity and misses list what IS stored.
fn match_session_entry(sess: &crate::session_store::CliSession, reference: &str) -> eyre::Result<String> {
    if sess.servers.contains_key(reference) {
        return Ok(reference.to_owned());
    }
    let slug_hits: Vec<&String> = sess
        .servers
        .iter()
        .filter(|(_, e)| e.slug == reference)
        .map(|(k, _)| k)
        .collect();
    if let [one] = slug_hits.as_slice() {
        return Ok((*one).clone());
    }
    let prefix_hits: Vec<&String> = if slug_hits.is_empty() {
        sess.servers
            .iter()
            .filter(|(k, e)| k.starts_with(reference) || e.slug.starts_with(reference))
            .map(|(k, _)| k)
            .collect()
    } else {
        slug_hits
    };
    let stored = || {
        sess.servers
            .keys()
            .map(String::as_str)
            .collect::<Vec<_>>()
            .join(", ")
    };
    match prefix_hits.as_slice() {
        [one] => Ok((*one).clone()),
        [] => Err(crate::errors::not_found("auth use", reference)
            .cause(format!("stored sessions: {}", stored()))
            .hint("`task auth whoami` lists the stored entries")
            .report()),
        many => Err(crate::errors::conflict("auth use", reference)
            .cause(format!(
                "matches {} entries: {}",
                many.len(),
                many.iter()
                    .map(|k| k.as_str())
                    .collect::<Vec<_>>()
                    .join(", ")
            ))
            .hint("disambiguate with the full key (`slug@host`)")
            .report()),
    }
}

/// Resolve a LOCAL on-disk org for the auth verbs that read the
/// org's `auth.sqlite` directly (`auth users`, `auth org …`).
/// Distinguishes "this command is local-only" from "you're not
/// signed in": a remote session can never serve these — they need
/// an org dir under the data root.
fn local_org_ctx(org_override: Option<&str>, what: &str) -> eyre::Result<crate::org_ctx::ActiveOrg> {
    crate::org_ctx::resolve_active(org_override).map_err(|e| {
        crate::errors::usage(format!(
            "{what} is a local-only command (it reads the org's on-disk auth.sqlite)"
        ))
        .cause(format!("{e:#}"))
        .hint(
            "run `task org init <slug>` to create a local org dir; a remote session \
             (`task auth login --server …`) cannot serve this command",
        )
        .report()
    })
}

/// Best-effort server-side revocation for a legacy `"local"`
/// session entry: open the org's `auth.sqlite` directly, like the
/// old local-first `auth logout` did.
async fn revoke_local_session(entry: &crate::session_store::ServerEntry) -> eyre::Result<()> {
    use architect_auth::commands::SignOut;
    let root =
        org_proto::DataRoot::from_env().map_err(|e| eyre::eyre!("resolve data root: {e}"))?;
    let (org, _) = root
        .load_org(&entry.slug)
        .map_err(|e| eyre::eyre!("no local org dir for `{}`: {e}", entry.slug))?;
    let auth = open_local_auth(&org.auth_db()).await?;
    auth.sign_out(SignOut {
        token: entry.token.clone(),
    })
    .await
    .map_err(|e| eyre::eyre!("{e}"))?;
    Ok(())
}

async fn open_auth_db(auth_db_path: &std::path::Path) -> eyre::Result<sea_orm::DatabaseConnection> {
    use sea_orm::Database;
    use sea_orm_migration::MigratorTrait;
    let db = Database::connect(format!("sqlite://{}?mode=rwc", auth_db_path.display()))
        .await
        .map_err(|e| eyre::eyre!("connect auth db: {e}"))?;
    architect_auth::db::Migrator::up(&db, None)
        .await
        .map_err(|e| eyre::eyre!("auth migrations: {e}"))?;
    Ok(db)
}

async fn list_user_memberships(
    user_id: uuid::Uuid,
    auth_db_path: &std::path::Path,
) -> eyre::Result<
    Vec<(
        architect_auth::db::AuthMemberModel,
        architect_auth::db::AuthOrganizationModel,
    )>,
> {
    use architect_auth::db::{AuthMemberColumn, AuthMemberEntity, AuthOrganizationEntity};
    use sea_orm::{ColumnTrait, EntityTrait, QueryFilter};
    let db = open_auth_db(auth_db_path).await?;
    let members = AuthMemberEntity::find()
        .filter(AuthMemberColumn::UserId.eq(user_id))
        .all(&db)
        .await
        .map_err(|e| eyre::eyre!("list members: {e}"))?;
    let mut out = Vec::with_capacity(members.len());
    for m in members {
        let Some(org) = AuthOrganizationEntity::find_by_id(m.organization_id)
            .one(&db)
            .await
            .map_err(|e| eyre::eyre!("find org {}: {e}", m.organization_id))?
        else {
            continue;
        };
        out.push((m, org));
    }
    Ok(out)
}

async fn update_session_active_org(
    token: &str,
    org_id: Option<uuid::Uuid>,
    auth_db_path: &std::path::Path,
) -> eyre::Result<()> {
    use architect_auth::db::{AuthSessionActiveModel, AuthSessionColumn, AuthSessionEntity};
    use sea_orm::{ActiveModelTrait, ColumnTrait, EntityTrait, IntoActiveModel, QueryFilter, Set};
    let token_hash = hash_session_token(crate::session_store::DEFAULT_AUTH_SECRET, token);
    let db = open_auth_db(auth_db_path).await?;
    let row = AuthSessionEntity::find()
        .filter(AuthSessionColumn::TokenHash.eq(token_hash))
        .one(&db)
        .await
        .map_err(|e| eyre::eyre!("find session: {e}"))?
        .ok_or_else(|| eyre::eyre!("session not found — session file may be stale"))?;
    let mut am: AuthSessionActiveModel = row.into_active_model();
    am.active_organization_id = Set(org_id);
    am.update(&db)
        .await
        .map_err(|e| eyre::eyre!("update session: {e}"))?;
    Ok(())
}

/// Reproduce `architect-auth::crypto::hash_token`. The auth
/// crate keeps the helper crate-private; we re-implement the
/// exact same recipe so the CLI can look up its own session
/// row by token hash without depending on auth internals.
///
/// **Recipe (must match `architect-auth/crypto.rs`):**
/// `base64url-no-pad(SHA256(secret || ":" || token))`.
fn hash_session_token(secret: &str, token: &str) -> String {
    use base64::Engine;
    use base64::engine::general_purpose::URL_SAFE_NO_PAD;
    use sha2::{Digest, Sha256};
    let mut h = Sha256::new();
    h.update(secret.as_bytes());
    h.update(b":");
    h.update(token.as_bytes());
    URL_SAFE_NO_PAD.encode(h.finalize())
}
