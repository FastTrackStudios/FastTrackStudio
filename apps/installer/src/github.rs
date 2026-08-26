//! Release resolution against the GitHub API.
//!
//! `GET /repos/FastTrackStudios/FastTrackStudio/releases/tags/<tag>`, or —
//! with no tag — a scan of `/releases`, then pick a release asset by name
//! and its `browser_download_url`. An optional `$GITHUB_TOKEN` (or
//! `$GH_TOKEN`, what the `gh` CLI exports) is sent as
//! `Authorization: Bearer <t>` for private repos and the higher rate limit;
//! public access works without it.
//!
//! **Releases are resolved by asset, not by recency.** One repo publishes
//! two products — FastTrackStudio under `v*` and Task under `task-v*` — so
//! `/releases/latest` regularly names a release with nothing in it for the
//! caller (and, being GitHub, skips prereleases besides, which is all the
//! FTS line has published so far). Resolution walks the release list newest
//! first and takes the first release that actually carries a matching
//! asset, which is correct regardless of how the tags are namespaced.
//!
//! Asset naming isn't uniform across platforms — Linux tarballs and the
//! macOS plugin zip both follow `<prefix>-<platform_suffix>.<ext>`, but the
//! macOS app `.dmg` (built by `deploy-macos.sh`) embeds a build-number
//! infix and no arch token. `resolve_matching` is the shared fetch +
//! scan logic; each platform/asset-shape gets its own thin wrapper
//! around it.

use eyre::{Context, eyre};

const API_BASE: &str = "https://api.github.com/repos/FastTrackStudios/FastTrackStudio";

/// How many releases back to look for one carrying the wanted asset.
const RELEASE_SCAN_LIMIT: usize = 30;

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
/// name prefix (e.g. "fasttrackstudio-" for the Linux app tarball) —
/// `<prefix>-<platform_suffix>.tar.gz`. Linux-only in practice: the macOS
/// app and plugin bundle have their own resolvers below (different
/// container formats and, for the app, a non-conforming build-number
/// infix in the filename).
pub async fn resolve_with_prefix(
    client: &reqwest::Client,
    tag: Option<&str>,
    asset_prefix: &str,
) -> eyre::Result<Release> {
    resolve_with_prefix_ext(client, tag, asset_prefix, "tar.gz").await
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
/// — a single universal (lipo'd) build covering both Mac architectures,
/// same as the plugin zip below, so no arch token or `platform_suffix()`
/// involved. The build number is a unix timestamp, not something we can
/// predict, so this matches on prefix/suffix only. No `SHA256SUMS` covers
/// it; the notarization signature is the integrity/authenticity check
/// instead.
pub async fn resolve_macos_dmg(
    client: &reqwest::Client,
    tag: Option<&str>,
) -> eyre::Result<Release> {
    resolve_matching(
        client,
        tag,
        |name| name.starts_with("FastTrackStudio-") && name.ends_with("-macos.dmg"),
        "FastTrackStudio-*-macos.dmg",
    )
    .await
}

/// Resolve the macOS plugin bundle `.zip` asset
/// (`fts-plugins-v<ver>-macos.zip`) — a single universal (lipo'd) build
/// covering both Mac architectures (nice-plug-xtask's `bundle-universal`),
/// unlike the app dmg. No arch token, so no `platform_suffix()` involved.
pub async fn resolve_macos_plugins_zip(
    client: &reqwest::Client,
    tag: Option<&str>,
) -> eyre::Result<Release> {
    resolve_matching(
        client,
        tag,
        |name| name.starts_with("fts-plugins-") && name.ends_with("-macos.zip"),
        "fts-plugins-*-macos.zip",
    )
    .await
}

/// Resolve a release carrying an asset satisfying `matches`: the one tagged
/// `tag`, or — with no tag — the newest release that has such an asset.
/// `asset_desc` is only used in the error message.
async fn resolve_matching(
    client: &reqwest::Client,
    tag: Option<&str>,
    matches: impl Fn(&str) -> bool,
    asset_desc: &str,
) -> eyre::Result<Release> {
    match tag {
        // An explicit tag is a demand for THAT release: if it doesn't carry
        // the asset, say so rather than quietly installing a different
        // version than the one asked for.
        Some(tag) => {
            let url = format!("{API_BASE}/releases/tags/{tag}");
            let release = get_json(client, &url).await.map_err(|e| {
                eyre!("{e}\n(no release tagged {tag}? `gh release list` shows what exists)")
            })?;
            pick_asset(&release, &matches, asset_desc)
        }
        None => {
            let url = format!("{API_BASE}/releases?per_page={RELEASE_SCAN_LIMIT}");
            let list = get_json(client, &url).await?;
            let releases = list
                .as_array()
                .ok_or_else(|| eyre!("{url} did not return a release list"))?;
            if releases.is_empty() {
                return Err(eyre!("no releases published yet"));
            }
            // GitHub returns these newest-first, so the first match is the
            // newest release carrying the asset.
            for release in releases {
                if let Ok(found) = pick_asset(release, &matches, asset_desc) {
                    return Ok(found);
                }
            }
            let tags: Vec<&str> = releases
                .iter()
                .filter_map(|r| r["tag_name"].as_str())
                .collect();
            Err(eyre!(
                "none of the {} most recent releases carries a {asset_desc} asset \
                 (looked at: {})",
                releases.len(),
                tags.join(", ")
            ))
        }
    }
}

/// Pull the wanted asset (and any SHA256SUMS beside it) out of one release's
/// JSON.
fn pick_asset(
    release: &serde_json::Value,
    matches: &impl Fn(&str) -> bool,
    asset_desc: &str,
) -> eyre::Result<Release> {
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
        .map(|a| Asset {
            name: a.name.clone(),
            url: a.url.clone(),
        })
        .ok_or_else(|| {
            let names: Vec<&str> = assets.iter().map(|a| a.name.as_str()).collect();
            eyre!(
                "release {tag} has no {asset_desc} asset (assets: {})",
                if names.is_empty() {
                    "none".to_string()
                } else {
                    names.join(", ")
                }
            )
        })?;

    let sums = assets.into_iter().find(|a| a.name == "SHA256SUMS");

    Ok(Release { tag, tarball, sums })
}

/// GET a GitHub API endpoint as JSON, with the token if we have one.
async fn get_json(client: &reqwest::Client, url: &str) -> eyre::Result<serde_json::Value> {
    let mut req = client
        .get(url)
        .header("Accept", "application/vnd.github+json");
    // GITHUB_TOKEN is the CI/API name; GH_TOKEN is what the `gh` CLI exports,
    // so a developer with gh set up needs no extra configuration.
    let token = std::env::var("GITHUB_TOKEN")
        .or_else(|_| std::env::var("GH_TOKEN"))
        .unwrap_or_default();
    if !token.is_empty() {
        req = req.header("Authorization", format!("Bearer {token}"));
    }

    let resp = req
        .send()
        .await
        .wrap_err_with(|| format!("requesting {url}"))?;
    let status = resp.status();
    let body = resp.bytes().await.wrap_err("reading release response")?;
    if !status.is_success() {
        // 403 with no token is nearly always the 60-req/hour anonymous rate
        // limit rather than a real permission problem; say which it is.
        let hint = if status.as_u16() == 403 && std::env::var_os("GITHUB_TOKEN").is_none() {
            " (GitHub rate limit? set $GITHUB_TOKEN)"
        } else {
            ""
        };
        return Err(eyre!("{url} -> HTTP {status}{hint}"));
    }
    serde_json::from_slice(&body).wrap_err("parsing release JSON")
}
