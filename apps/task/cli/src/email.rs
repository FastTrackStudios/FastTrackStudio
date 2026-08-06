//! `task email …` — manage the org's mail accounts.
//!
//! Accounts live as one directory per account under the org's mail
//! root (`<org>/vault/Mail/<id>/`, or `TASK_SERVER_MAIL_ROOT`), each
//! optionally carrying an `account.json`
//! ([`email_config::AccountConfig`]). The server scans that tree at
//! startup and routes each account to the Maildir or IMAP backend
//! according to its `backend` field. A directory with no config is a
//! plain local Maildir, which is why the zero-config fixture mailbox
//! keeps working.
//!
//! These commands write that tree. **The server reads it at startup**,
//! so adding or removing an account needs a restart to take effect —
//! `add` says so rather than leaving you wondering why nothing changed.
//!
//! ## Passwords
//!
//! The password is an [`email_secret::Secret`], never plaintext in the
//! JSON unless you ask for it:
//!
//! - `--password-command 'rbw get gmail'` — resolve by shelling out
//!   (argv-style, no shell interpolation). The recommended shape here:
//!   the secret stays in your password manager.
//! - `--password-env GMAIL_APP_PASSWORD` — read from the server's
//!   environment.
//! - `--password-keyring` — OS keyring, service `task-email`.
//! - `--password-raw` — inline in the file. Refused unless
//!   `--i-mean-it` is also passed, because it writes a live credential
//!   to disk in the vault.
//!
//! ## Gmail
//!
//! `task email add-gmail you@gmail.com --password-command 'rbw get
//! gmail-app-password'` fills in `imap.gmail.com:993` (implicit TLS)
//! and `smtp.gmail.com:587` (STARTTLS).
//!
//! The one thing Gmail still requires is an **app password** (Google
//! Account → Security → 2-Step Verification → App passwords); a normal
//! account password will not authenticate. Reaching that page prompts
//! for a password re-verification, so it is a hands-on step.
//!
//! There is **no IMAP toggle to turn on**. Google made IMAP always-on
//! and removed the control — the Forwarding and POP/IMAP settings page
//! now carries only the auto-expunge / deletion / folder-size options.
//! Checked against a live account 2026-08-05; earlier guidance here
//! (and everywhere else on the internet) says to enable it, and that
//! step no longer exists.

use clap::Subcommand;
use email_config::{AccountConfig, BackendKind, FolderAliases, SmtpConfig, TlsMode};
use email_secret::Secret;

use crate::resolve_active_org;

#[derive(Subcommand)]
pub(crate) enum EmailCmd {
    /// List the org's configured mail accounts.
    List {
        /// Emit the accounts as a JSON array.
        #[arg(long)]
        json: bool,
    },
    /// Add a remote IMAP account.
    Add {
        /// Account id — the directory name under the mail root, and
        /// what the UI and `--account` flags refer to. Defaults to the
        /// address.
        #[arg(long)]
        id: Option<String>,
        /// The email address this account sends and receives as.
        address: String,
        /// IMAP host, e.g. `imap.gmail.com`.
        #[arg(long)]
        imap_host: String,
        #[arg(long, default_value_t = 993)]
        imap_port: u16,
        /// `implicit` (993) | `starttls` (143) | `none` (tests only).
        #[arg(long, default_value = "implicit")]
        imap_tls: String,
        /// IMAP username. Defaults to the address.
        #[arg(long)]
        username: Option<String>,
        /// SMTP host for sending. Omit to make the account read-only.
        #[arg(long)]
        smtp_host: Option<String>,
        #[arg(long, default_value_t = 587)]
        smtp_port: u16,
        /// `starttls` (587) | `implicit` (465) | `none`.
        #[arg(long, default_value = "starttls")]
        smtp_tls: String,
        /// Display name on outgoing mail.
        #[arg(long)]
        display_name: Option<String>,
        #[command(flatten)]
        password: PasswordArgs,
    },
    /// Add a Gmail account with Google's endpoints prefilled.
    ///
    /// Requires IMAP enabled in Gmail and an app password — a normal
    /// account password will not authenticate.
    AddGmail {
        /// Your Gmail address.
        address: String,
        /// Account id. Defaults to the address.
        #[arg(long)]
        id: Option<String>,
        #[arg(long)]
        display_name: Option<String>,
        #[command(flatten)]
        password: PasswordArgs,
    },
    /// Remove an account's configuration.
    Remove {
        /// Account id.
        id: String,
        /// Also delete the account's local maildir. Irreversible —
        /// without it only `account.json` is removed, so a local
        /// mailbox survives and can be re-registered.
        #[arg(long)]
        purge_maildir: bool,
    },
    /// Connect to an account and list its folders — the check that
    /// credentials and endpoints actually work.
    Test {
        /// Account id.
        id: String,
    },
}

/// How to resolve the account password. Exactly one source is
/// required.
///
/// `--i-mean-it` is deliberately OUTSIDE the group: it is a
/// confirmation for `--password-raw`, not a way to supply a password,
/// and having it satisfy the group would let `add-gmail --i-mean-it`
/// through with no credential at all.
#[derive(clap::Args)]
#[command(group = clap::ArgGroup::new("password_source").required(true).multiple(false))]
pub(crate) struct PasswordArgs {
    /// Shell out to fetch the password, argv-style:
    /// `--password-command 'rbw get gmail-app-password'`.
    #[arg(long, group = "password_source")]
    password_command: Option<String>,
    /// Read the password from this environment variable *on the
    /// server*.
    #[arg(long, group = "password_source")]
    password_env: Option<String>,
    /// Read from the OS keyring (service `task-email`, account
    /// `<id>:password`).
    #[arg(long, group = "password_source")]
    password_keyring: bool,
    /// Store the password verbatim in `account.json`. Requires
    /// `--i-mean-it`.
    #[arg(long, group = "password_source")]
    password_raw: Option<String>,
    /// Confirm `--password-raw`, which writes a live credential to
    /// disk.
    #[arg(long, requires = "password_raw")]
    i_mean_it: bool,
}

impl PasswordArgs {
    fn resolve(&self, account_id: &str) -> eyre::Result<Secret> {
        if let Some(cmd) = &self.password_command {
            // argv-style: split on whitespace, no shell, no quoting
            // surprises. `email_secret` runs it directly.
            let argv: Vec<String> = cmd.split_whitespace().map(str::to_owned).collect();
            if argv.is_empty() {
                eyre::bail!("--password-command is empty");
            }
            return Ok(Secret::Command {
                argv,
                timeout_ms: Some(10_000),
            });
        }
        if let Some(name) = &self.password_env {
            return Ok(Secret::EnvVar { name: name.clone() });
        }
        if self.password_keyring {
            return Ok(Secret::Keyring {
                service: "task-email".to_owned(),
                account: format!("{account_id}:password"),
            });
        }
        if let Some(raw) = &self.password_raw {
            if !self.i_mean_it {
                eyre::bail!(
                    "--password-raw writes the password in cleartext into the org's vault \
                     (account.json), where it will be backed up and possibly synced. \
                     Prefer --password-command 'rbw get <entry>' or --password-env. \
                     Pass --i-mean-it to do it anyway."
                );
            }
            return Ok(Secret::raw(raw.clone()));
        }
        // clap's `required = true` group makes this unreachable.
        eyre::bail!("no password source given")
    }
}

fn parse_tls(s: &str) -> eyre::Result<TlsMode> {
    match s.to_ascii_lowercase().as_str() {
        "implicit" | "tls" | "ssl" => Ok(TlsMode::Implicit),
        "starttls" => Ok(TlsMode::Starttls),
        "none" | "plain" => Ok(TlsMode::None),
        other => eyre::bail!("unknown tls mode {other:?} (implicit | starttls | none)"),
    }
}

/// The org's mail root — same resolution the server does, so the CLI
/// writes where the server reads.
fn mail_root(slug: &str) -> eyre::Result<std::path::PathBuf> {
    if let Ok(explicit) = std::env::var("TASK_SERVER_MAIL_ROOT") {
        return Ok(std::path::PathBuf::from(explicit));
    }
    let root = org_proto::DataRoot::from_env()
        .map_err(|e| eyre::eyre!("no data root ({e}) — set TASK_DATA_ROOT or run `task org init`"))?;
    Ok(root.org(slug).path().join("vault").join("Mail"))
}

pub(crate) async fn run_email(cmd: EmailCmd, org_override: Option<&str>) -> eyre::Result<()> {
    let slug = resolve_active_org(org_override.map(str::to_owned))?;
    let root = mail_root(&slug)?;

    match cmd {
        EmailCmd::List { json } => list(&root, json),
        EmailCmd::Add {
            id,
            address,
            imap_host,
            imap_port,
            imap_tls,
            username,
            smtp_host,
            smtp_port,
            smtp_tls,
            display_name,
            password,
        } => {
            let id = id.unwrap_or_else(|| address.clone());
            let secret = password.resolve(&id)?;
            let submit = match smtp_host {
                Some(host) => Some(SmtpConfig {
                    host,
                    port: smtp_port,
                    tls: parse_tls(&smtp_tls)?,
                    username: username.clone().unwrap_or_else(|| address.clone()),
                    password: secret.clone(),
                }),
                None => None,
            };
            let cfg = AccountConfig {
                id: email_proto::AccountId(id.clone()),
                name: id.clone(),
                address: address.clone(),
                display_name,
                backend: BackendKind::Imap {
                    host: imap_host,
                    port: imap_port,
                    tls: parse_tls(&imap_tls)?,
                    username: username.unwrap_or(address),
                    password: secret,
                    submit,
                },
                signature: None,
                folder_aliases: FolderAliases::new(),
            };
            write_account(&root, &id, &cfg)
        }
        EmailCmd::AddGmail {
            address,
            id,
            display_name,
            password,
        } => {
            let id = id.unwrap_or_else(|| address.clone());
            let secret = password.resolve(&id)?;
            let cfg = AccountConfig {
                id: email_proto::AccountId(id.clone()),
                name: id.clone(),
                address: address.clone(),
                display_name,
                backend: BackendKind::Imap {
                    host: "imap.gmail.com".into(),
                    port: 993,
                    tls: TlsMode::Implicit,
                    username: address.clone(),
                    password: secret.clone(),
                    submit: Some(SmtpConfig {
                        host: "smtp.gmail.com".into(),
                        port: 587,
                        tls: TlsMode::Starttls,
                        username: address,
                        password: secret,
                    }),
                },
                signature: None,
                folder_aliases: FolderAliases::new(),
            };
            write_account(&root, &id, &cfg)?;
            println!();
            println!("Gmail still needs an APP PASSWORD on Google's side:");
            println!("  Google Account → Security → 2-Step Verification → App passwords");
            println!("  (https://myaccount.google.com/apppasswords)");
            println!("  A normal account password will NOT authenticate.");
            println!();
            println!("You do NOT need to enable IMAP — Google made it always-on and removed");
            println!("the setting; there is no toggle on Forwarding and POP/IMAP any more.");
            println!();
            println!("Then check it before restarting the server:");
            println!("  task email test {id}");
            Ok(())
        }
        EmailCmd::Remove { id, purge_maildir } => remove(&root, &id, purge_maildir),
        EmailCmd::Test { id } => test(&root, &id).await,
    }
}

fn write_account(
    root: &std::path::Path,
    id: &str,
    cfg: &AccountConfig,
) -> eyre::Result<()> {
    let dir = root.join(id);
    std::fs::create_dir_all(&dir)
        .map_err(|e| eyre::eyre!("create {}: {e}", dir.display()))?;
    let path = dir.join("account.json");
    let existed = path.exists();
    cfg.save_json(&path)
        .map_err(|e| eyre::eyre!("write {}: {e}", path.display()))?;
    println!(
        "{} account {id} → {}",
        if existed { "updated" } else { "added" },
        path.display()
    );
    println!("restart task-server for it to take effect (accounts are read at startup).");
    Ok(())
}

fn list(root: &std::path::Path, json: bool) -> eyre::Result<()> {
    let mut rows = Vec::new();
    if let Ok(entries) = std::fs::read_dir(root) {
        for entry in entries.flatten() {
            let path = entry.path();
            if !path.is_dir() {
                continue;
            }
            let Some(id) = path.file_name().and_then(|s| s.to_str()) else {
                continue;
            };
            let cfg = AccountConfig::load_json(&path.join("account.json")).ok().flatten();
            let (kind, address, host) = match cfg.as_ref().map(|c| (&c.backend, &c.address)) {
                Some((BackendKind::Imap { host, port, .. }, addr)) => {
                    ("imap", addr.clone(), format!("{host}:{port}"))
                }
                Some((BackendKind::Jmap { session_url, .. }, addr)) => {
                    ("jmap", addr.clone(), session_url.clone())
                }
                Some((BackendKind::Nextcloud { base_url, .. }, addr)) => {
                    ("nextcloud", addr.clone(), base_url.clone())
                }
                Some((BackendKind::Maildir { .. }, addr)) => {
                    ("maildir", addr.clone(), path.display().to_string())
                }
                // No account.json at all: a bare local maildir.
                None => ("maildir", id.to_owned(), path.display().to_string()),
            };
            rows.push((id.to_owned(), kind, address, host));
        }
    }
    rows.sort();

    if json {
        let out: Vec<serde_json::Value> = rows
            .iter()
            .map(|(id, kind, address, host)| {
                serde_json::json!({ "id": id, "kind": kind, "address": address, "host": host })
            })
            .collect();
        println!("{}", serde_json::to_string_pretty(&out)?);
        return Ok(());
    }
    if rows.is_empty() {
        println!("no mail accounts configured ({})", root.display());
        println!("add one with `task email add-gmail <address> --password-command '<cmd>'`");
        return Ok(());
    }
    for (id, kind, address, host) in rows {
        println!("{id:24} {kind:10} {address:32} {host}");
    }
    Ok(())
}

fn remove(root: &std::path::Path, id: &str, purge_maildir: bool) -> eyre::Result<()> {
    let dir = root.join(id);
    if !dir.exists() {
        eyre::bail!("no account {id:?} under {}", root.display());
    }
    if purge_maildir {
        std::fs::remove_dir_all(&dir)
            .map_err(|e| eyre::eyre!("remove {}: {e}", dir.display()))?;
        println!("removed account {id} and its local maildir");
    } else {
        let cfg = dir.join("account.json");
        if cfg.exists() {
            std::fs::remove_file(&cfg)
                .map_err(|e| eyre::eyre!("remove {}: {e}", cfg.display()))?;
        }
        println!("removed account {id}'s config (local maildir kept)");
    }
    println!("restart task-server for it to take effect.");
    Ok(())
}

/// Connect and list folders. This is the command that tells you
/// whether the password source resolves AND the endpoint accepts it —
/// run it before restarting the server, so a bad credential surfaces
/// here rather than as an empty `/email` page.
async fn test(root: &std::path::Path, id: &str) -> eyre::Result<()> {
    let path = root.join(id).join("account.json");
    let cfg = AccountConfig::load_json(&path)
        .map_err(|e| eyre::eyre!("read {}: {e}", path.display()))?
        .ok_or_else(|| eyre::eyre!("no account.json for {id:?} — is it a plain maildir?"))?;

    if !matches!(cfg.backend, BackendKind::Imap { .. }) {
        eyre::bail!("account {id:?} is not an IMAP account; nothing to connect to");
    }

    // Resolve the secret first and say so separately: "wrong password"
    // and "password source broken" are different problems and the
    // difference is most of the debugging.
    if let BackendKind::Imap { password, .. } = &cfg.backend {
        match password.resolve().await {
            Ok(v) if v.as_str().is_empty() => {
                eyre::bail!("password source resolved to an empty string")
            }
            Ok(_) => println!("password source: ok"),
            Err(e) => eyre::bail!("password source failed: {e}"),
        }
    }

    let backend = email_imap::Backend::from_configs(vec![cfg])
        .map_err(|e| eyre::eyre!("build imap backend: {e}"))?;
    let account = id.to_owned();
    // `EmailSync`'s methods block on a runtime internally, so they must
    // not run on a runtime worker thread.
    let folders = tokio::task::spawn_blocking(move || {
        use email_proto::EmailSync;
        backend.list_folders(&account)
    })
    .await??;

    println!("connected — {} folders:", folders.len());
    for f in folders {
        let unread = f.unread_count.map_or(String::new(), |n| format!("  ({n} unread)"));
        println!("  {}{unread}", f.name);
    }
    Ok(())
}
