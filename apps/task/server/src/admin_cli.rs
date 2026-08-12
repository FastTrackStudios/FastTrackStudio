//! Operator subcommands, run ON the server against its own data root.
//!
//! ## Why these exist separately from the vox surface
//!
//! The vox `AuthService::migrate_user_email` authorizes with a SESSION —
//! the caller proves who they are, and is recorded as `changed_by`. That
//! is right for a person acting in the app, and wrong for an operator:
//! the usual reason to migrate an address is that its owner can no longer
//! sign in with it, so requiring their session to fix their login is
//! circular. Routing an operator through it also means impersonating the
//! user, which is worse than not authenticating at all — it puts their
//! password somewhere it needn't be and records the change as if they
//! made it.
//!
//! So authorization here is FILESYSTEM OWNERSHIP of the data root, the
//! same argument `OrgManagementImpl::new_local_trusted` and
//! `SnapshotImpl::new_local_trusted` already make: something that can
//! open `auth.sqlite` can do anything to it anyway, and a session check
//! on top would be theatre. In the cluster that means `kubectl exec` into
//! the pod, which is already the highest privilege available.
//!
//! Changes are recorded as `changed_by: None` (nobody signed in did it)
//! with an explicit reason, so the trail distinguishes an operator
//! migration from a user's own.

use eyre::{Context as _, bail};

/// Handle an `admin …` invocation. Returns `Ok(false)` when argv is not
/// an admin subcommand, so `main` falls through to booting the server.
pub async fn dispatch() -> eyre::Result<bool> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    if args.first().map(String::as_str) != Some("admin") {
        return Ok(false);
    }
    match args.get(1).map(String::as_str) {
        Some("migrate-email") => migrate_email(&args[2..]).await.map(|()| true),
        Some("email-history") => email_history(&args[2..]).await.map(|()| true),
        Some("set-password") => set_password(&args[2..]).await.map(|()| true),
        Some("list-users") => list_users(&args[2..]).await.map(|()| true),
        Some("delete-user") => delete_user(&args[2..]).await.map(|()| true),
        Some("set-role") => set_role(&args[2..]).await.map(|()| true),
        Some("create-user") => create_user(&args[2..]).await.map(|()| true),
        Some("webdav") => webdav(&args[2..]).map(|()| true),
        other => {
            eprintln!(
                "usage:\n  \
                 task-server admin migrate-email --org <slug> --from <email> --to <email> \\\n    \
                 [--reason <text>] [--dry-run]\n  \
                 task-server admin email-history --org <slug> --email <address>\n  \
                 task-server admin set-password --org <slug> --email <address>\n    \
                 (reads the new password from STDIN)\n  \
                 task-server admin list-users --org <slug>\n  \
                 task-server admin delete-user --org <slug> --email <address> --yes\n  \
                 task-server admin set-role --org <slug> --email <address> [--role admin|--clear]\n  \
                 task-server admin create-user --org <slug> --email <address> \\\n    \
                 [--name <display>] [--username <handle>] (reads the password from STDIN)\n  \
                 task-server admin webdav --org <slug> [--hide <root-id>|--show <root-id>]\n    \
                 (no flag lists the org's File Roots and their WebDAV visibility)\n"
            );
            bail!("unknown admin subcommand: {}", other.unwrap_or("(none)"));
        }
    }
}

/// Minimal `--flag value` parsing. Deliberately not clap: this is two
/// operator verbs on a server binary, and adding an arg parser to it for
/// them would be the larger change.
fn flag(args: &[String], name: &str) -> Option<String> {
    args.iter()
        .position(|a| a == name)
        .and_then(|i| args.get(i + 1))
        .cloned()
}

fn has(args: &[String], name: &str) -> bool {
    args.iter().any(|a| a == name)
}

/// Open one org's auth store directly from the data root.
async fn open_org_auth(slug: &str) -> eyre::Result<crate::AuthState> {
    let data_root = org_proto::DataRoot::from_env()
        .map_err(|e| eyre::eyre!("data root: {e}"))?;
    let org_root = data_root.org(slug);
    let db = org_root.auth_db();
    if !db.exists() {
        bail!("org `{slug}` has no auth store at {}", db.display());
    }
    let url = format!("sqlite://{}?mode=rwc", db.display());
    // The running server holds this same file open. Both connections use
    // WAL (`AuthState::open` enables it), which is exactly the
    // multi-process case WAL exists for, so a short write here is safe
    // alongside a live server.
    crate::AuthState::open(&url, &crate::auth_secret())
        .await
        .wrap_err_with(|| format!("open auth store for `{slug}`"))
}

async fn migrate_email(args: &[String]) -> eyre::Result<()> {
    let (Some(slug), Some(from), Some(to)) =
        (flag(args, "--org"), flag(args, "--from"), flag(args, "--to"))
    else {
        bail!("--org, --from and --to are all required");
    };
    let reason = flag(args, "--reason")
        .unwrap_or_else(|| "operator migration (task-server admin)".to_owned());
    let dry = has(args, "--dry-run");

    let auth = open_org_auth(&slug).await?;
    let user = auth
        .auth
        .find_user_by_email(&from)
        .await
        .map_err(|e| eyre::eyre!("look up `{from}` in `{slug}`: {e:?}"))?
        .ok_or_else(|| eyre::eyre!("no account with email `{from}` in org `{slug}`"))?;

    println!("{slug}: {} is {}", user.id, from);
    if dry {
        println!("  dry run — would migrate to {to}");
        return Ok(());
    }

    let moved = auth
        .auth
        .migrate_user_email(architect_auth::MigrateUserEmail {
            user_id: user.id,
            new_email: to.clone(),
            // Nobody signed in did this; the trail should say so rather
            // than name a user who wasn't involved.
            changed_by: None,
            reason: Some(reason),
        })
        .await
        .map_err(|e| eyre::eyre!("migrate `{from}` -> `{to}` in `{slug}`: {e:?}"))?;

    println!(
        "  now {} (user id {} — unchanged)",
        moved.email.as_deref().unwrap_or("(none)"),
        moved.id
    );
    // The id holding is the property that matters: everything keyed on it
    // (tasks, timers, sessions, authorship) stays attached.
    if moved.id != user.id {
        bail!("user id changed — this should be impossible; investigate before continuing");
    }
    Ok(())
}

/// Grant or clear architect-auth's `admin` role.
///
/// This is the bootstrap for admin itself: `require_admin` needs an
/// existing admin, so the FIRST one cannot be made through the admin
/// flows. Possession of the auth store is the only authority that
/// predates any account.
///
/// Scope note: this sets `auth_users.role`, which gates architect-auth's
/// `admin_*` flows. It is NOT the permission gate's role — that comes
/// from architect-permissions, which currently gives every validated
/// user the same default and never consults this column. Granting admin
/// therefore does not (yet) widen what the gate allows.
async fn set_role(args: &[String]) -> eyre::Result<()> {
    let (Some(slug), Some(email)) = (flag(args, "--org"), flag(args, "--email")) else {
        bail!("--org and --email are required");
    };
    let role = if has(args, "--clear") {
        None
    } else {
        Some(flag(args, "--role").unwrap_or_else(|| "admin".to_owned()))
    };
    let auth = open_org_auth(&slug).await?;
    let user = auth
        .auth
        .find_user_by_email(&email)
        .await
        .map_err(|e| eyre::eyre!("look up `{email}`: {e:?}"))?
        .ok_or_else(|| eyre::eyre!("no account with email `{email}` in org `{slug}`"))?;
    let updated = auth
        .auth
        .set_user_role_local_trusted(user.id, role.clone())
        .await
        .map_err(|e| eyre::eyre!("set role for `{email}` in `{slug}`: {e:?}"))?;
    println!(
        "{slug}: {} ({email}) role = {}",
        updated.id,
        updated.role.as_deref().unwrap_or("(none)")
    );
    Ok(())
}

/// Every account in one org's store.
async fn list_users(args: &[String]) -> eyre::Result<()> {
    let Some(slug) = flag(args, "--org") else {
        bail!("--org is required");
    };
    let auth = open_org_auth(&slug).await?;
    let users = auth
        .auth
        .list_users_local_trusted()
        .await
        .map_err(|e| eyre::eyre!("list users in `{slug}`: {e:?}"))?;
    if users.is_empty() {
        println!("{slug}: no accounts");
        return Ok(());
    }
    println!("{slug}:");
    for u in users {
        println!(
            "  {}  {}",
            u.id,
            u.email.as_deref().unwrap_or("(no email)")
        );
    }
    Ok(())
}

/// Remove an account outright.
///
/// Requires `--yes`. This is not recoverable from here — the row is gone
/// and anything keyed on the user id is orphaned, so an operator should
/// have to say so deliberately rather than discover it from a typo in an
/// `--email` flag.
async fn delete_user(args: &[String]) -> eyre::Result<()> {
    let (Some(slug), Some(email)) = (flag(args, "--org"), flag(args, "--email")) else {
        bail!("--org and --email are required");
    };
    let auth = open_org_auth(&slug).await?;
    let user = auth
        .auth
        .find_user_by_email(&email)
        .await
        .map_err(|e| eyre::eyre!("look up `{email}`: {e:?}"))?
        .ok_or_else(|| eyre::eyre!("no account with email `{email}` in org `{slug}`"))?;

    if !has(args, "--yes") {
        println!("{slug}: would delete {} ({email})", user.id);
        println!("  re-run with --yes to actually delete — this cannot be undone");
        return Ok(());
    }
    auth.auth
        .delete_user_local_trusted(user.id)
        .await
        .map_err(|e| eyre::eyre!("delete `{email}` in `{slug}`: {e:?}"))?;
    println!("{slug}: deleted {} ({email})", user.id);
    Ok(())
}

/// Create an account in one org's auth store.
///
/// **This is the bootstrap for org membership**, and it exists because
/// there was no way in. `AuthService::sign_up_email_password` is
/// deliberately not public — open self-registration plus the org lane's
/// default `member` role made enforcement bypassable in one call — so
/// only an existing member can provision an account. An org with zero
/// accounts therefore has nobody who could create the first one, and is
/// unreachable by every client: CLI, GUI and agent alike. Five of the
/// six orgs on production are in exactly that state.
///
/// Possession of the auth store is the only authority that predates any
/// account, which is why this lives on the server binary next to
/// [`set_role`] rather than behind an RPC.
///
/// The password is read from STDIN for the same reason as
/// [`set_password`]: arguments are visible to every user on the box via
/// `ps` and land in shell history.
///
/// Creating the same email in several orgs makes several *distinct*
/// accounts with distinct user ids — auth stores are per-org and there
/// is no cross-org identity yet (see `plans/federated-task-platform.md`
/// phase 3). They share a login, not a principal.
async fn create_user(args: &[String]) -> eyre::Result<()> {
    use std::io::Read as _;

    let (Some(slug), Some(email)) = (flag(args, "--org"), flag(args, "--email")) else {
        bail!("--org and --email are required");
    };
    let mut password = String::new();
    std::io::stdin()
        .read_to_string(&mut password)
        .wrap_err("read the password from stdin")?;
    let password = password.trim_end_matches(['\n', '\r']).to_owned();
    if password.is_empty() {
        bail!(
            "no password on stdin — pipe it in, e.g. `kubectl exec -i … -- task-server admin create-user …`"
        );
    }

    let auth = open_org_auth(&slug).await?;
    // Idempotence: re-running after a partial sweep across several orgs
    // should report the existing account, not fail halfway with a
    // uniqueness error that leaves the operator guessing which orgs got
    // done.
    if let Some(existing) = auth
        .auth
        .find_user_by_email(&email)
        .await
        .map_err(|e| eyre::eyre!("look up `{email}` in `{slug}`: {e:?}"))?
    {
        println!(
            "{slug}: {email} already exists ({}) — nothing to do",
            existing.id
        );
        println!("  use `set-password` to change its credential");
        return Ok(());
    }

    let bundle = auth
        .auth
        .create_email_password_user(architect_auth::CreateEmailPasswordUser {
            email: email.clone(),
            password,
            name: flag(args, "--name"),
            username: flag(args, "--username"),
            image: None,
            metadata_json: None,
            ip_address: None,
            user_agent: Some("task-server admin create-user".into()),
        })
        .await
        .map_err(|e| eyre::eyre!("create `{email}` in `{slug}`: {e:?}"))?;

    println!("{slug}: created {} ({email})", bundle.user.id);
    println!("  sign in with: task auth login --org {slug} --email {email}");
    Ok(())
}

/// Set an account's password without knowing the old one.
///
/// The operator counterpart to `AuthService::change_password`, which is
/// self-service and requires the current password. This one exists for
/// the case that flow cannot serve: the owner cannot sign in, so there
/// is no session and no known credential.
///
/// The new password is read from STDIN, never from an argument.
/// Arguments are visible to every user on the box via `ps` for the life
/// of the process, and land in shell history; stdin does neither. That
/// also means whoever runs this supplies the secret directly — it is not
/// something the command can be handed by a third party.
async fn set_password(args: &[String]) -> eyre::Result<()> {
    use std::io::Read as _;

    let (Some(slug), Some(email)) = (flag(args, "--org"), flag(args, "--email")) else {
        bail!("--org and --email are required");
    };
    let mut new_password = String::new();
    std::io::stdin()
        .read_to_string(&mut new_password)
        .wrap_err("read the new password from stdin")?;
    let new_password = new_password.trim_end_matches(['\n', '\r']).to_owned();
    if new_password.is_empty() {
        bail!("no password on stdin — pipe it in, e.g. `kubectl exec -i … -- task-server admin set-password …`");
    }

    let auth = open_org_auth(&slug).await?;
    let user = auth
        .auth
        .find_user_by_email(&email)
        .await
        .map_err(|e| eyre::eyre!("look up `{email}`: {e:?}"))?
        .ok_or_else(|| eyre::eyre!("no account with email `{email}` in org `{slug}`"))?;

    auth.auth
        .set_user_password_local_trusted(user.id, &new_password)
        .await
        .map_err(|e| eyre::eyre!("set password for `{email}` in `{slug}`: {e:?}"))?;

    println!("{slug}: password set for {} ({email})", user.id);
    println!("  existing sessions are NOT revoked — sign out elsewhere if that matters");
    Ok(())
}

async fn email_history(args: &[String]) -> eyre::Result<()> {
    let (Some(slug), Some(email)) = (flag(args, "--org"), flag(args, "--email")) else {
        bail!("--org and --email are required");
    };
    let auth = open_org_auth(&slug).await?;
    let user = auth
        .auth
        .find_user_by_email(&email)
        .await
        .map_err(|e| eyre::eyre!("look up `{email}`: {e:?}"))?
        .ok_or_else(|| eyre::eyre!("no account with email `{email}` in org `{slug}`"))?;
    let history = auth
        .auth
        .list_email_history(user.id)
        .await
        .map_err(|e| eyre::eyre!("history for `{email}`: {e:?}"))?;
    if history.is_empty() {
        println!("{slug}: {} has never changed email", user.id);
        return Ok(());
    }
    println!("{slug}: {}", user.id);
    for row in history {
        println!(
            "  {}  {} -> {}  by {}",
            row.created_at.to_rfc3339(),
            row.previous_email.as_deref().unwrap_or("(none)"),
            row.new_email,
            row.changed_by
                .map_or_else(|| "operator".to_owned(), |id| id.to_string()),
        );
        if let Some(reason) = row.reason {
            println!("      {reason}");
        }
    }
    Ok(())
}

/// `admin webdav --org <slug> [--hide <root-id>|--show <root-id>]` —
/// the operator surface for "a per-root policy can hide a root from
/// WebDAV" (issue #274).
///
/// The policy lives in a JSON file beside the org's Files registry, and
/// that file is deliberately the source of truth (see
/// `files_webdav::WebdavPolicy`): hiding a root is an operator decision
/// on a compat surface, not something the RPC contract should carry. But
/// "edit this JSON by hand" is not an operator surface, so this verb is
/// the one that writes it correctly — and, with no flag, answers the
/// question an operator actually has, which is "what is exposed right
/// now?". The running server re-reads the file on its next request; no
/// restart, no signal.
///
/// Authorization is filesystem ownership of the data root, exactly like
/// every other verb in this module.
fn webdav(args: &[String]) -> eyre::Result<()> {
    let Some(slug) = flag(args, "--org") else {
        bail!("--org <slug> is required");
    };
    let data_root = org_proto::DataRoot::from_env().map_err(|e| eyre::eyre!("data root: {e}"))?;
    let files_dir = data_root.org(&slug).path().join("files");
    if !files_dir.is_dir() {
        bail!("org `{slug}` has no Files area at {}", files_dir.display());
    }
    let backend = files::FilesBackend::new(&files_dir, data_root.org(&slug).path().join("vault"))
        .map_err(|e| eyre::eyre!("open files backend for `{slug}`: {e}"))?;
    let policy = files_webdav::WebdavPolicy::open(&files_dir);

    for (name, hide) in [("--hide", true), ("--show", false)] {
        if let Some(raw) = flag(args, name) {
            let id = raw
                .parse::<uuid::Uuid>()
                .wrap_err_with(|| format!("{name} takes a root id, got `{raw}`"))?;
            // Refuse an id this org does not have — a typo'd uuid would
            // otherwise be accepted silently and hide nothing.
            let roots = pollster::block_on(files::FilesService::list_roots(&backend))
                .map_err(|e| eyre::eyre!("list roots: {e}"))?;
            if !roots.iter().any(|r| r.id == id) {
                bail!("org `{slug}` has no File Root {id}");
            }
            policy
                .set_hidden(id, hide)
                .wrap_err_with(|| format!("write webdav policy for `{slug}`"))?;
            println!(
                "{slug}: root {id} is now {} on WebDAV",
                if hide { "hidden" } else { "visible" }
            );
            return Ok(());
        }
    }

    let roots = pollster::block_on(files::FilesService::list_roots(&backend))
        .map_err(|e| eyre::eyre!("list roots: {e}"))?;
    if roots.is_empty() {
        println!("{slug}: no File Roots");
        return Ok(());
    }
    let hidden = policy
        .hidden_set()
        .wrap_err_with(|| format!("read webdav policy for `{slug}`"))?;
    println!("{slug}: WebDAV policy at {}", policy.path().display());
    for root in roots {
        println!(
            "  {}  {:<8}  {}",
            root.id,
            if hidden.contains(&root.id) {
                "hidden"
            } else {
                "visible"
            },
            root.name,
        );
    }
    Ok(())
}
