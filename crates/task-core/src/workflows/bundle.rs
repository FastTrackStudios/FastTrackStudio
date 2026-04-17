use std::path::Path;
use crate::service::VaultError;
use super::download::*;

/// Result of generating a bundle.
#[derive(Debug, Clone, Default)]
pub struct BundleGenerationResult {
    pub bundles_created: usize,
    pub files_copied: usize,
    pub shares_created: usize,
    pub errors: Vec<String>,
}

/// Generate download bundles on the local filesystem.
///
/// For each bundle in the portal:
/// 1. Create a folder: `{output_dir}/{bundle.id}/`
/// 2. Copy (or symlink) files from the project into the bundle folder
/// 3. Organize by category subfolder
///
/// Returns the generated folder paths.
pub fn generate_bundles_local(
    portal: &DownloadPortal,
    project_root: &Path,
    output_dir: &Path,
) -> Result<BundleGenerationResult, VaultError> {
    let mut result = BundleGenerationResult::default();

    for bundle in &portal.bundles {
        let bundle_dir = output_dir.join(&bundle.id);
        std::fs::create_dir_all(&bundle_dir)
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        // Copy explicit files
        for file in &bundle.files {
            let src = project_root.join(&file.source);
            if !src.exists() {
                result.errors.push(format!("File not found: {}", file.source));
                continue;
            }

            // Organize by category
            let dest_dir = if let Some(ref cat) = file.category {
                bundle_dir.join(cat)
            } else {
                bundle_dir.clone()
            };
            std::fs::create_dir_all(&dest_dir)
                .map_err(|e| VaultError::IoError(e.to_string()))?;

            let dest_name = file.dest.as_deref()
                .unwrap_or_else(|| src.file_name().unwrap().to_str().unwrap());
            let dest = dest_dir.join(dest_name);

            std::fs::copy(&src, &dest)
                .map_err(|e| VaultError::IoError(format!("Copy failed: {e}")))?;
            result.files_copied += 1;
        }

        // Apply include rules (glob patterns)
        for rule in &bundle.include_rules {
            let pattern = project_root.join(&rule.pattern);
            let _pattern_str = pattern.to_string_lossy();

            if let Ok(entries) = glob_files(&project_root, &rule.pattern) {
                let dest_dir = if let Some(ref cat) = rule.category {
                    bundle_dir.join(cat)
                } else {
                    bundle_dir.clone()
                };
                std::fs::create_dir_all(&dest_dir).ok();

                for entry in entries {
                    let dest = dest_dir.join(entry.file_name().unwrap());
                    if std::fs::copy(&entry, &dest).is_ok() {
                        result.files_copied += 1;
                    }
                }
            }
        }

        result.bundles_created += 1;
    }

    Ok(result)
}

/// Simple glob matching for include rules.
fn glob_files(root: &Path, pattern: &str) -> Result<Vec<std::path::PathBuf>, VaultError> {
    // Simple implementation: split pattern into directory + filename parts
    // Support * wildcard in the filename part
    let mut results = Vec::new();

    let pattern_path = root.join(pattern);
    let dir = pattern_path.parent().unwrap_or(root);
    let file_pattern = pattern_path.file_name()
        .and_then(|f| f.to_str())
        .unwrap_or("*");

    if !dir.exists() {
        return Ok(results);
    }

    if let Ok(entries) = std::fs::read_dir(dir) {
        for entry in entries.flatten() {
            let name = entry.file_name();
            let name_str = name.to_string_lossy();

            if file_pattern == "*" || matches_glob(&name_str, file_pattern) {
                results.push(entry.path());
            }
        }
    }

    Ok(results)
}

/// Simple glob matcher (supports * and ? wildcards).
fn matches_glob(name: &str, pattern: &str) -> bool {
    if pattern == "*" { return true; }

    // Handle *.ext pattern
    if let Some(ext) = pattern.strip_prefix("*.") {
        return name.ends_with(&format!(".{ext}"));
    }

    // Handle prefix* pattern
    if let Some(prefix) = pattern.strip_suffix("*") {
        return name.starts_with(prefix);
    }

    name == pattern
}

/// Create Nextcloud share links for bundles.
///
/// For each bundle, creates a public share via the Nextcloud OCS API
/// and stores the share URL in the bundle.
pub async fn create_nextcloud_shares(
    portal: &mut DownloadPortal,
    nc_url: &str,
    nc_user: &str,
    nc_pass: &str,
    bundles_path: &str,  // WebDAV path to the downloads/ folder
) -> Result<usize, VaultError> {
    let http = reqwest::Client::new();
    let mut shares_created = 0;

    for bundle in &mut portal.bundles {
        let share_path = format!("{}/{}", bundles_path.trim_end_matches('/'), bundle.id);

        // Create public share via OCS API
        let url = format!("{}/ocs/v2.php/apps/files_sharing/api/v1/shares", nc_url);
        let body = format!(
            "path={}&shareType=3&permissions=1",  // shareType=3 = public link, permissions=1 = read
            urlencode_form(&share_path)
        );

        // Add portal-level password if set
        let body = if let Some(ref pwd) = portal.password {
            format!("{}&password={}", body, urlencode_form(pwd))
        } else {
            body
        };

        // Add portal-level expiry if set
        let body = if let Some(ref expires) = portal.expires {
            format!("{}&expireDate={}", body, expires)
        } else {
            body
        };

        let resp = http.post(&url)
            .basic_auth(nc_user, Some(nc_pass))
            .header("OCS-APIRequest", "true")
            .header("Content-Type", "application/x-www-form-urlencoded")
            .body(body)
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("Share creation failed: {e}")))?;

        let text = resp.text().await
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        // Extract share URL and token from response
        if let Some(url) = extract_xml_value_simple(&text, "url") {
            bundle.direct_url = Some(url);
            shares_created += 1;
        }
        if let Some(token) = extract_xml_value_simple(&text, "token") {
            bundle.share_token = Some(token);
        }
    }

    Ok(shares_created)
}

fn urlencode_form(s: &str) -> String {
    s.replace(' ', "+")
     .replace('/', "%2F")
     .replace('&', "%26")
     .replace('=', "%3D")
}

fn extract_xml_value_simple(xml: &str, tag: &str) -> Option<String> {
    let open = format!("<{}>", tag);
    let close = format!("</{}>", tag);
    let start = xml.find(&open)? + open.len();
    let end = xml[start..].find(&close)? + start;
    Some(xml[start..end].to_string())
}
