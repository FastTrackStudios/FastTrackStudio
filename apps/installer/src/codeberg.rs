//! Release resolution against the codeberg (Gitea/Forgejo) API.
//!
//! `GET /api/v1/repos/FastTrackStudios/FastTrackStudio/releases/latest`
//! (or `/releases/tags/<tag>`), then pick the platform tarball asset by
//! name and its `browser_download_url`. An optional `$CODEBERG_TOKEN` is
//! sent as `Authorization: token <t>` (private repos / rate limits);
//! public access works without it.

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

/// Resolve the latest release, or the release for a specific tag.
pub async fn resolve(client: &reqwest::Client, tag: Option<&str>) -> eyre::Result<Release> {
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
            return Box::pin(resolve_newest_any(client)).await;
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

    let suffix = format!("-{}.tar.gz", crate::platform_suffix()?);
    let tarball = assets
        .iter()
        .find(|a| a.name.starts_with("fasttrackstudio-") && a.name.ends_with(&suffix))
        .map(|a| Asset { name: a.name.clone(), url: a.url.clone() })
        .ok_or_else(|| {
            let names: Vec<&str> = assets.iter().map(|a| a.name.as_str()).collect();
            eyre!(
                "release {tag} has no fasttrackstudio-*{suffix} asset (assets: {})",
                if names.is_empty() { "none".to_string() } else { names.join(", ") }
            )
        })?;

    let sums = assets
        .into_iter()
        .find(|a| a.name == "SHA256SUMS");

    Ok(Release { tag, tarball, sums })
}

/// Newest release of any kind (prereleases included): first entry of
/// the paginated list.
async fn resolve_newest_any(client: &reqwest::Client) -> eyre::Result<Release> {
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
    Box::pin(resolve(client, Some(tag))).await
}
