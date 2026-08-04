//! Release resolution against the codeberg (Gitea/Forgejo) API.
//!
//! `GET /api/v1/repos/FastTrackStudios/FastTrackStudio/releases/latest`
//! (or `/releases/tags/<tag>`), then pick a release asset by name and its
//! `browser_download_url`. An optional `$CODEBERG_TOKEN` is sent as
//! `Authorization: token <t>` (private repos / rate limits); public access
//! works without it.
//!
//! Asset naming isn't uniform across platforms — Linux tarballs and the
//! macOS plugin zip both follow `<prefix>-<platform_suffix>.<ext>`, but the
//! macOS app `.dmg` (built by `deploy-macos.sh`) embeds a build-number
//! infix and no arch token. `resolve_matching` is the shared fetch +
//! fallback logic; each platform/asset-shape gets its own thin wrapper
//! around it.

use eyre::{Context, eyre};

const API_BASE: &str = "https://codeberg.org/api/v1/repos/FastTrackStudios/FastTrackStudio";

pub struct Asset {
    pub name: String,
    pub url: String,
}

pub struct Release {
    pub tag: String,
    pub tarball: Asset,
    pub sums: Option<Asset>,
}

/// Resolve the latest FastTrackStudio app release, or a specific tag —
/// Linux tarball (`fasttrackstudio-*-x86_64-linux.tar.gz`).
pub async fn resolve(client: &reqwest::Client, tag: Option<&str>) -> eyre::Result<Release> {
    resolve_with_prefix(client, tag, "fasttrackstudio-").await
}

/// Same resolution, but pick the platform tarball by an arbitrary asset
/// name prefix (e.g. "fts-plugins-" for the plugin bundle), with the
/// extension appropriate to the host platform (`.tar.gz` on Linux, `.zip`
/// for the macOS plugin bundle — Apple notarization requires zip/pkg/dmg).
pub async fn resolve_with_prefix(
    client: &reqwest::Client,
    tag: Option<&str>,
    asset_prefix: &str,
) -> eyre::Result<Release> {
    let ext = if cfg!(target_os = "macos") { "zip" } else { "tar.gz" };
    resolve_with_prefix_ext(client, tag, asset_prefix, ext).await
}

/// Same as `resolve_with_prefix`, with an explicit asset extension.
pub async fn resolve_with_prefix_ext(
    client: &reqwest::Client,
    tag: Option<&str>,
    asset_prefix: &str,
    ext: &str,
) -> eyre::Result<Release> {
    let suffix = format!("-{}.{ext}", crate::platform_suffix()?);
    let prefix = asset_prefix.to_string();
    let suffix_owned = suffix.clone();
    resolve_matching(
        client,
        tag,
        move |name| name.starts_with(&prefix) && name.ends_with(&suffix_owned),
        &format!("{asset_prefix}*{suffix}"),
    )
    .await
}

/// Resolve the macOS app `.dmg` asset (`FastTrackStudio-<ver>-<build>-macos.dmg`
/// — the build number is a unix timestamp, not something we can predict, so
/// this matches on prefix/suffix only). No `SHA256SUMS` covers it; the
/// notarization signature is the integrity/authenticity check instead.
pub async fn resolve_macos_dmg(client: &reqwest::Client, tag: Option<&str>) -> eyre::Result<Release> {
    resolve_matching(
        client,
        tag,
        |name| name.starts_with("FastTrackStudio-") && name.ends_with("-macos.dmg"),
        "FastTrackStudio-*-macos.dmg",
    )
    .await
}

/// Fetch a release (latest, or a specific tag) and pick the one asset
/// satisfying `matches`; `asset_desc` is only used in the error message.
async fn resolve_matching(
    client: &reqwest::Client,
    tag: Option<&str>,
    matches: impl Fn(&str) -> bool,
    asset_desc: &str,
) -> eyre::Result<Release> {
    let url = match tag {
        Some(tag) => format!("{API_BASE}/releases/tags/{tag}"),
        None => format!("{API_BASE}/releases/latest"),
    };

    let mut req = client.get(&url).header("Accept", "application/json");
    if let Ok(token) = std::env::var("CODEBERG_TOKEN")
        && !token.is_empty() {
            req = req.header("Authorization", format!("token {token}"));
        }

    let resp = req.send().await.wrap_err_with(|| format!("requesting {url}"))?;
    let status = resp.status();
    let body = resp.bytes().await.wrap_err("reading release response")?;
    if !status.is_success() {
        // Gitea's /releases/latest EXCLUDES prereleases — when only
        // alphas exist it 404s. Fall back to the newest release of any
        // kind.
        if status.as_u16() == 404 && tag.is_none() {
            return Box::pin(resolve_newest_any(client, matches, asset_desc)).await;
        }
        let hint = match (status.as_u16(), tag) {
            (404, Some(tag)) => format!(" (no release tagged {tag}?)"),
            (404, None) => " (no releases published yet?)".to_string(),
            _ => String::new(),
        };
        return Err(eyre!("{url} -> HTTP {status}{hint}"));
    }

    let release: serde_json::Value =
        serde_json::from_slice(&body).wrap_err("parsing release JSON")?;
    let tag = release["tag_name"]
        .as_str()
        .ok_or_else(|| eyre!("release JSON has no tag_name"))?
        .to_string();

    let assets: Vec<Asset> = release["assets"]
        .as_array()
        .map(|a| a.as_slice())
        .unwrap_or_default()
        .iter()
        .filter_map(|a| {
            Some(Asset {
                name: a["name"].as_str()?.to_string(),
                url: a["browser_download_url"].as_str()?.to_string(),
            })
        })
        .collect();

    let tarball = assets
        .iter()
        .find(|a| matches(&a.name))
        .map(|a| Asset { name: a.name.clone(), url: a.url.clone() })
        .ok_or_else(|| {
            let names: Vec<&str> = assets.iter().map(|a| a.name.as_str()).collect();
            eyre!(
                "release {tag} has no {asset_desc} asset (assets: {})",
                if names.is_empty() { "none".to_string() } else { names.join(", ") }
            )
        })?;

    let sums = assets.into_iter().find(|a| a.name == "SHA256SUMS");

    Ok(Release { tag, tarball, sums })
}

/// Newest release of any kind (prereleases included): first entry of
/// the paginated list.
async fn resolve_newest_any(
    client: &reqwest::Client,
    matches: impl Fn(&str) -> bool,
    asset_desc: &str,
) -> eyre::Result<Release> {
    let url = format!("{API_BASE}/releases?limit=1");
    let mut req = client.get(&url).header("Accept", "application/json");
    if let Ok(token) = std::env::var("CODEBERG_TOKEN")
        && !token.is_empty() {
            req = req.header("Authorization", format!("token {token}"));
        }
    let resp = req.send().await.wrap_err_with(|| format!("requesting {url}"))?;
    let status = resp.status();
    let body = resp.bytes().await.wrap_err("reading releases response")?;
    if !status.is_success() {
        return Err(eyre!("{url} -> HTTP {status}"));
    }
    let list: serde_json::Value =
        serde_json::from_slice(&body).wrap_err("parsing releases JSON")?;
    let first = list
        .as_array()
        .and_then(|a| a.first())
        .ok_or_else(|| eyre!("no releases published yet"))?;
    let tag = first["tag_name"]
        .as_str()
        .ok_or_else(|| eyre!("release JSON has no tag_name"))?;
    Box::pin(resolve_matching(client, Some(tag), matches, asset_desc)).await
}
