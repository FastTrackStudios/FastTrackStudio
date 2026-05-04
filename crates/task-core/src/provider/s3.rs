//! S3-compatible object storage provider.
//!
//! Reads/writes the same `project.md` + `tasks/*.md` format, stored as
//! S3 objects with key prefixes mapping to project folders.
//!
//! ```text
//! s3://my-bucket/projects/
//! ├── Montreal Album/project.md
//! ├── Montreal Album/tasks/Mix track 1.md
//! └── Website Redesign/project.md
//! ```
//!
//! Uses the S3 REST API with HTTP basic auth, compatible with MinIO, R2,
//! and other S3-compatible stores that support basic authentication.
//! For AWS S3, set up an authenticating proxy or use a gateway that
//! translates basic auth to SigV4.

use async_trait::async_trait;

use super::traits::*;
use crate::project::Project;
use crate::service::VaultError;
use crate::task::Task;
use crate::task::WikiLink;
use crate::vault::Vault;

/// Configuration for an S3-compatible storage backend.
#[derive(Debug, Clone)]
pub struct S3Config {
    /// Bucket name.
    pub bucket: String,
    /// Key prefix for projects (e.g. "projects/").
    pub prefix: String,
    /// S3 endpoint URL (for MinIO, R2, etc.). None = AWS default.
    pub endpoint: Option<String>,
    /// AWS region.
    pub region: String,
    /// Access key ID.
    pub access_key: String,
    /// Secret access key.
    pub secret_key: String,
}

/// S3-compatible project provider.
///
/// Stores projects as:
/// - `{prefix}{project_title}/project.md` — project metadata
/// - `{prefix}{project_title}/tasks/{task_title}.md` — individual tasks
pub struct S3Provider {
    info: ProviderInfo,
    config: S3Config,
    http: reqwest::Client,
}

impl S3Provider {
    pub fn new(name: impl Into<String>, label: impl Into<String>, config: S3Config) -> Self {
        Self {
            info: ProviderInfo {
                name: name.into(),
                label: label.into(),
                kind: "s3".into(),
                writable: true,
            },
            config,
            http: reqwest::Client::new(),
        }
    }

    /// Base URL for the bucket (e.g. "http://localhost:9000/my-bucket").
    fn bucket_url(&self) -> String {
        let endpoint = self
            .config
            .endpoint
            .as_deref()
            .unwrap_or("https://s3.amazonaws.com");
        format!("{}/{}", endpoint.trim_end_matches('/'), self.config.bucket)
    }

    /// Full URL for an object key.
    fn object_url(&self, key: &str) -> String {
        // Percent-encode each path segment to handle spaces and special chars.
        let encoded: String = key
            .split('/')
            .map(|seg| urlenccode(seg))
            .collect::<Vec<_>>()
            .join("/");
        format!("{}/{}", self.bucket_url(), encoded)
    }

    fn project_key(&self, title: &str) -> String {
        format!("{}{}/project.md", self.config.prefix, title)
    }

    fn task_key(&self, project_title: &str, task_title: &str) -> String {
        format!(
            "{}{}/tasks/{}.md",
            self.config.prefix, project_title, task_title
        )
    }

    fn tasks_prefix(&self, project_title: &str) -> String {
        format!("{}{}/tasks/", self.config.prefix, project_title)
    }

    // ── S3 low-level operations ─────────────────────────────────────

    /// GET an object. Returns `None` for 404.
    async fn s3_get(&self, key: &str) -> Result<Option<String>, VaultError> {
        let url = self.object_url(key);

        let resp = self
            .http
            .get(&url)
            .basic_auth(&self.config.access_key, Some(&self.config.secret_key))
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("S3 GET failed: {e}")))?;

        if resp.status().as_u16() == 404 {
            return Ok(None);
        }
        if !resp.status().is_success() {
            return Err(VaultError::IoError(format!(
                "S3 GET {}: {}",
                key,
                resp.status()
            )));
        }

        Ok(Some(
            resp.text()
                .await
                .map_err(|e| VaultError::IoError(e.to_string()))?,
        ))
    }

    /// PUT an object.
    async fn s3_put(&self, key: &str, content: &str) -> Result<(), VaultError> {
        let url = self.object_url(key);

        let resp = self
            .http
            .put(&url)
            .basic_auth(&self.config.access_key, Some(&self.config.secret_key))
            .header("Content-Type", "text/markdown; charset=utf-8")
            .body(content.to_string())
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("S3 PUT failed: {e}")))?;

        if !resp.status().is_success() {
            return Err(VaultError::IoError(format!(
                "S3 PUT {}: {}",
                key,
                resp.status()
            )));
        }
        Ok(())
    }

    /// DELETE an object.
    async fn s3_delete(&self, key: &str) -> Result<(), VaultError> {
        let url = self.object_url(key);

        let resp = self
            .http
            .delete(&url)
            .basic_auth(&self.config.access_key, Some(&self.config.secret_key))
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("S3 DELETE failed: {e}")))?;

        // 204 No Content is the normal success for DELETE; 404 is also fine.
        if !resp.status().is_success() && resp.status().as_u16() != 404 {
            return Err(VaultError::IoError(format!(
                "S3 DELETE {}: {}",
                key,
                resp.status()
            )));
        }
        Ok(())
    }

    /// ListObjectsV2: list keys under a prefix. If `delimiter` is set,
    /// returns common prefixes (virtual "directories") instead of keys.
    async fn s3_list(
        &self,
        prefix: &str,
        delimiter: Option<&str>,
    ) -> Result<S3ListResult, VaultError> {
        let mut url = format!(
            "{}?list-type=2&prefix={}",
            self.bucket_url(),
            urlenccode(prefix),
        );
        if let Some(d) = delimiter {
            url.push_str(&format!("&delimiter={}", urlenccode(d)));
        }

        let mut all_keys = Vec::new();
        let mut all_prefixes = Vec::new();
        let mut continuation_token: Option<String> = None;

        loop {
            let mut page_url = url.clone();
            if let Some(ref token) = continuation_token {
                page_url.push_str(&format!("&continuation-token={}", urlenccode(token)));
            }

            let resp = self
                .http
                .get(&page_url)
                .basic_auth(&self.config.access_key, Some(&self.config.secret_key))
                .send()
                .await
                .map_err(|e| VaultError::IoError(format!("S3 ListObjectsV2 failed: {e}")))?;

            if !resp.status().is_success() {
                return Err(VaultError::IoError(format!(
                    "S3 ListObjectsV2 prefix={}: {}",
                    prefix,
                    resp.status()
                )));
            }

            let xml = resp
                .text()
                .await
                .map_err(|e| VaultError::IoError(e.to_string()))?;

            // Parse <Key>...</Key> entries
            all_keys.extend(extract_xml_values(&xml, "Key"));

            // Parse <Prefix>...</Prefix> inside <CommonPrefixes>
            all_prefixes.extend(extract_xml_values(&xml, "Prefix"));

            // Check for truncation / pagination
            let is_truncated = extract_xml_value(&xml, "IsTruncated")
                .map(|v| v == "true")
                .unwrap_or(false);

            if is_truncated {
                continuation_token = extract_xml_value(&xml, "NextContinuationToken");
                if continuation_token.is_none() {
                    break; // Safety: avoid infinite loop
                }
            } else {
                break;
            }
        }

        Ok(S3ListResult {
            keys: all_keys,
            common_prefixes: all_prefixes,
        })
    }

    /// List project folder names by listing with a delimiter.
    /// Returns just the project title strings.
    async fn list_project_names(&self) -> Result<Vec<String>, VaultError> {
        let result = self.s3_list(&self.config.prefix, Some("/")).await?;

        let mut names = Vec::new();
        for cp in &result.common_prefixes {
            // Common prefix looks like "projects/Montreal Album/"
            // Strip the base prefix and trailing slash to get the project name.
            let stripped = cp
                .strip_prefix(&self.config.prefix)
                .unwrap_or(cp)
                .trim_end_matches('/');
            if !stripped.is_empty() {
                names.push(stripped.to_string());
            }
        }
        Ok(names)
    }

    /// Load a single project bundle (project.md + tasks/*.md).
    async fn load_project_bundle(
        &self,
        project_name: &str,
    ) -> Result<Option<ProjectBundle>, VaultError> {
        // Load project.md
        let project_key = self.project_key(project_name);
        let project = match self.s3_get(&project_key).await? {
            Some(content) => Vault::parse_project_from_md(&content),
            None => None,
        };

        let project = project.unwrap_or_else(|| Project {
            title: project_name.to_string(),
            ..Default::default()
        });

        // List task files under tasks/ prefix
        let tasks_prefix = self.tasks_prefix(project_name);
        let list_result = self.s3_list(&tasks_prefix, None).await?;

        let mut tasks = Vec::new();
        for key in &list_result.keys {
            if !key.ends_with(".md") {
                continue;
            }
            if let Ok(Some(content)) = self.s3_get(key).await {
                if let Some(mut task) = Vault::parse_task_from_md(&content) {
                    let project_link = WikiLink(project.title.clone());
                    if !task.projects.contains(&project_link) {
                        task.projects.push(project_link);
                    }
                    tasks.push(task);
                }
            }
        }

        let location = format!("s3://{}/{}", self.config.bucket, project_key);
        Ok(Some(ProjectBundle {
            project,
            tasks,
            location,
            source: self.info.name.clone(),
        }))
    }
}

#[async_trait]
impl ProjectProvider for S3Provider {
    fn info(&self) -> &ProviderInfo {
        &self.info
    }

    async fn list_projects(&self) -> Result<Vec<Project>, VaultError> {
        let project_names = self.list_project_names().await?;

        let mut projects = Vec::new();
        for name in &project_names {
            let key = self.project_key(name);
            let project = match self.s3_get(&key).await? {
                Some(content) => Vault::parse_project_from_md(&content),
                None => None,
            };
            projects.push(project.unwrap_or_else(|| Project {
                title: name.clone(),
                ..Default::default()
            }));
        }
        Ok(projects)
    }

    async fn get_project(&self, title: &str) -> Result<Option<ProjectBundle>, VaultError> {
        self.load_project_bundle(title).await
    }

    async fn list_all(&self) -> Result<Vec<ProjectBundle>, VaultError> {
        let project_names = self.list_project_names().await?;

        let mut bundles = Vec::new();
        for name in &project_names {
            if let Some(bundle) = self.load_project_bundle(name).await? {
                bundles.push(bundle);
            }
        }
        Ok(bundles)
    }

    async fn create_project(&self, project: &Project) -> Result<String, VaultError> {
        let key = self.project_key(&project.title);
        let content = Vault::render_project_file(project, "")?;
        self.s3_put(&key, &content).await?;
        Ok(format!("s3://{}/{}", self.config.bucket, key))
    }

    async fn update_project(&self, project: &Project) -> Result<(), VaultError> {
        let key = self.project_key(&project.title);
        let content = Vault::render_project_file(project, "")?;
        self.s3_put(&key, &content).await
    }

    async fn save_task(&self, project_title: &str, task: &Task) -> Result<(), VaultError> {
        let key = self.task_key(project_title, &task.title);
        let content = Vault::render_task_file(task, "")?;
        self.s3_put(&key, &content).await
    }

    async fn delete_task(&self, project_title: &str, task_title: &str) -> Result<(), VaultError> {
        let key = self.task_key(project_title, task_title);
        self.s3_delete(&key).await
    }
}

// ── S3 XML / URL helpers ────────────────────────────────────────────

/// Result from a ListObjectsV2 call.
struct S3ListResult {
    /// Object keys returned in <Contents><Key>.
    keys: Vec<String>,
    /// Common prefixes returned when using a delimiter.
    common_prefixes: Vec<String>,
}

/// Minimal percent-encoding for S3 URL path segments and query values.
/// Encodes everything except unreserved characters (RFC 3986).
fn urlenccode(input: &str) -> String {
    let mut out = String::with_capacity(input.len() * 2);
    for b in input.bytes() {
        match b {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'~' => {
                out.push(b as char)
            }
            _ => {
                out.push('%');
                out.push_str(&format!("{:02X}", b));
            }
        }
    }
    out
}

/// Extract all values of `<Tag>...</Tag>` from XML (simple non-recursive parser).
fn extract_xml_values(xml: &str, tag: &str) -> Vec<String> {
    let open = format!("<{}>", tag);
    let close = format!("</{}>", tag);
    let mut values = Vec::new();
    let mut search_from = 0;

    while let Some(start) = xml[search_from..].find(&open) {
        let abs_start = search_from + start + open.len();
        if let Some(end) = xml[abs_start..].find(&close) {
            let value = &xml[abs_start..abs_start + end];
            // Decode basic XML entities
            let decoded = value
                .replace("&amp;", "&")
                .replace("&lt;", "<")
                .replace("&gt;", ">")
                .replace("&quot;", "\"")
                .replace("&apos;", "'");
            values.push(decoded);
            search_from = abs_start + end + close.len();
        } else {
            break;
        }
    }
    values
}

/// Extract first value of `<Tag>...</Tag>` from XML.
fn extract_xml_value(xml: &str, tag: &str) -> Option<String> {
    extract_xml_values(xml, tag).into_iter().next()
}
