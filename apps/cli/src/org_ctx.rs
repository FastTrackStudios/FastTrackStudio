//! Active-org resolver for CLI commands.
//!
//! Every subcommand that reads or writes org-scoped state
//! (timer, finance, auth login, future: project/wiki) calls
//! [`resolve_active`] to pick the [`org_proto::OrgRoot`] it
//! should operate against.
//!
//! Resolution order:
//!
//! 1. Explicit `--org <slug>` global flag (highest priority).
//! 2. Active org in `session.json` (set by `task auth login`
//!    or a future `task auth use`).
//! 3. Single-org disambiguation: if `<data_root>/orgs/` has
//!    exactly one org dir, use it.
//! 4. Auto-bootstrap: if no orgs exist, scaffold `default`
//!    on first use so commands don't fail on a fresh
//!    install.
//! 5. Multiple orgs but no active selection — error,
//!    instructing the user to pick one.

use eyre::Context;

use crate::session_store;

/// Result of [`resolve_active`]: the org root + a debug hint
/// for `--verbose` printing.
pub struct ActiveOrg {
    pub root: org_proto::OrgRoot,
    // Held for future `task org whoami` printing and audit
    // logging. Not read on every command — keep alive so the
    // struct is the canonical "what did the resolver pick"
    // bundle and consumers can opt in.
    #[allow(dead_code)]
    pub manifest: org_proto::OrgManifest,
    #[allow(dead_code)]
    pub source: OrgSource,
}

#[derive(Debug, Clone, Copy)]
#[allow(dead_code)]
pub enum OrgSource {
    ExplicitFlag,
    Session,
    SingleOrg,
    AutoBootstrap,
}

/// `<data_root>/orgs/<slug>/` resolver for the active org.
///
/// `cli_override` is the value of the top-level `--org`
/// flag, when present.
pub fn resolve_active(cli_override: Option<&str>) -> eyre::Result<ActiveOrg> {
    let root = org_proto::DataRoot::from_env().wrap_err("resolve data root")?;
    root.ensure().wrap_err("ensure data root")?;

    // 1. --org <slug>.
    if let Some(slug) = cli_override {
        let (org, manifest) = root.load_org(slug).wrap_err_with(|| {
            format!(
                "--org {slug}: org dir not found at {}; run `task org init {slug}` first",
                root.orgs_dir().join(slug).display()
            )
        })?;
        return Ok(ActiveOrg {
            root: org,
            manifest,
            source: OrgSource::ExplicitFlag,
        });
    }

    // 2. session.json active slug.
    if let Some(session) = session_store::load()? {
        let slug = session.active.as_str();
        if !slug.is_empty() {
            let (org, manifest) = root.load_org(slug).wrap_err_with(|| {
                format!(
                    "active org `{slug}` in session.json has no matching dir at {}; \
                     either run `task org init {slug}` or update the session",
                    root.orgs_dir().join(slug).display()
                )
            })?;
            return Ok(ActiveOrg {
                root: org,
                manifest,
                source: OrgSource::Session,
            });
        }
    }

    // 3 + 4. Scan + disambiguate / bootstrap.
    let orgs = root.scan_orgs().wrap_err("scan orgs")?;
    match orgs.len() {
        1 => {
            let (org, manifest) = orgs.into_iter().next().expect("len 1");
            Ok(ActiveOrg {
                root: org,
                manifest,
                source: OrgSource::SingleOrg,
            })
        }
        0 => {
            let (org, manifest) = bootstrap_default(&root)?;
            Ok(ActiveOrg {
                root: org,
                manifest,
                source: OrgSource::AutoBootstrap,
            })
        }
        _ => {
            let names: Vec<&str> = orgs.iter().map(|(o, _)| o.slug()).collect();
            Err(eyre::eyre!(
                "multiple orgs under {} ({}); pick one with `--org <slug>` or set an active org in session.json",
                root.orgs_dir().display(),
                names.join(", "),
            ))
        }
    }
}

/// Scaffold the auto-bootstrap `default` org. Idempotent —
/// if the dir exists (e.g. a race created it), [`load_org`]
/// is preferred; this only fires when `scan_orgs` returned
/// zero loadable orgs.
fn bootstrap_default(
    root: &org_proto::DataRoot,
) -> eyre::Result<(org_proto::OrgRoot, org_proto::OrgManifest)> {
    let org = root
        .init_org("default", "Default", false)
        .wrap_err("auto-bootstrap default org")?;
    let manifest = org.manifest().wrap_err("load fresh default manifest")?;
    Ok((org, manifest))
}
