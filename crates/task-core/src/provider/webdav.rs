//! WebDAV / Nextcloud provider.
//!
//! Reads/writes projects over WebDAV, which Nextcloud exposes for all
//! user files. This allows the Task app to access project files on a
//! Nextcloud server without NFS — just HTTPS.
//!
//! ```text
//! https://cloud.example.com/remote.php/dav/files/codywright/Projects/
//! ├── Montreal Album/project.md
//! ├── Montreal Album/tasks/Mix track 1.md
//! └── ...
//! ```
//!
use async_trait::async_trait;
use std::collections::BTreeMap;

use super::traits::*;
use crate::project::Project;
use crate::service::VaultError;
use crate::task::{Task, WikiLink};
use crate::vault::Vault;

/// Configuration for a WebDAV/Nextcloud connection.
#[derive(Debug, Clone)]
pub struct WebDavConfig {
    /// WebDAV base URL (e.g. "https://cloud.example.com/remote.php/dav/files/codywright/").
    pub url: String,
    /// Username for authentication.
    pub username: String,
    /// Password or app token.
    pub password: String,
    /// Path within the WebDAV share to the Projects directory (e.g. "Projects/").
    pub projects_path: String,
}

/// WebDAV-based project provider for Nextcloud and other WebDAV servers.
pub struct WebDavProvider {
    info: ProviderInfo,
    config: WebDavConfig,
    http: reqwest::Client,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WebDavResourceKind {
    File,
    Collection,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct WebDavPutOptions {
    pub content_type: Option<String>,
    pub if_match: Option<String>,
    pub if_none_match: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WebDavEntry {
    pub path: String,
    pub name: String,
    pub kind: WebDavResourceKind,
    pub content_type: Option<String>,
    pub content_length: Option<u64>,
    pub etag: Option<String>,
    pub last_modified: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WebDavLock {
    pub token: String,
    pub owner: Option<String>,
    pub timeout: Option<String>,
}

impl WebDavProvider {
    pub fn new(name: impl Into<String>, label: impl Into<String>, config: WebDavConfig) -> Self {
        Self {
            info: ProviderInfo {
                name: name.into(),
                label: label.into(),
                kind: "webdav".into(),
                writable: true,
            },
            config,
            http: reqwest::Client::new(),
        }
    }

    fn projects_url(&self) -> String {
        self.url_for_path("")
    }

    fn url_for_path(&self, path: &str) -> String {
        let base = self.config.url.trim_end_matches('/');
        let projects = self.config.projects_path.trim_matches('/');
        let path = path.trim_matches('/');
        let joined = [projects, path]
            .into_iter()
            .filter(|part| !part.is_empty())
            .collect::<Vec<_>>()
            .join("/");
        format!("{}/{}", base, encode_path(&joined),)
    }

    fn auth(&self, req: reqwest::RequestBuilder) -> reqwest::RequestBuilder {
        req.basic_auth(&self.config.username, Some(&self.config.password))
    }

    async fn propfind(&self, path: &str, depth: &str) -> Result<String, VaultError> {
        let body = r#"<?xml version="1.0" encoding="utf-8"?>
<d:propfind xmlns:d="DAV:">
  <d:prop>
    <d:resourcetype/>
    <d:displayname/>
    <d:getcontenttype/>
    <d:getcontentlength/>
    <d:getetag/>
    <d:getlastmodified/>
    <d:creationdate/>
    <d:supportedlock/>
    <d:lockdiscovery/>
  </d:prop>
</d:propfind>"#;
        self.propfind_with_body(path, depth, body).await
    }

    async fn propfind_with_body(
        &self,
        path: &str,
        depth: &str,
        body: &str,
    ) -> Result<String, VaultError> {
        let resp = self
            .auth(
                self.http
                    .request(webdav_method(b"PROPFIND")?, self.url_for_path(path)),
            )
            .header("Content-Type", "application/xml")
            .header("Depth", depth)
            .body(body.to_string())
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("WebDAV PROPFIND failed: {e}")))?;
        if !resp.status().is_success() {
            return Err(VaultError::IoError(format!(
                "WebDAV PROPFIND {}: {}",
                path,
                resp.status()
            )));
        }
        resp.text()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))
    }

    /// List resources at `path`. Depth `1` returns direct children; depth `0`
    /// returns only the resource itself; depth `infinity` is server-dependent.
    pub async fn list(&self, path: &str, depth: &str) -> Result<Vec<WebDavEntry>, VaultError> {
        let xml = self.propfind(path, depth).await?;
        Ok(parse_webdav_entries(&xml)
            .into_iter()
            .filter_map(|entry| self.relative_entry(entry, path))
            .collect())
    }

    /// Return metadata for one resource.
    pub async fn stat(&self, path: &str) -> Result<Option<WebDavEntry>, VaultError> {
        let mut entries = self.list(path, "0").await?;
        Ok(entries.pop())
    }

    /// Read a resource as bytes.
    pub async fn read(&self, path: &str) -> Result<Option<Vec<u8>>, VaultError> {
        let resp = self
            .auth(self.http.get(self.url_for_path(path)))
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("WebDAV GET failed: {e}")))?;
        if resp.status() == reqwest::StatusCode::NOT_FOUND {
            return Ok(None);
        }
        if !resp.status().is_success() {
            return Err(VaultError::IoError(format!(
                "WebDAV GET {}: {}",
                path,
                resp.status()
            )));
        }
        Ok(Some(
            resp.bytes()
                .await
                .map_err(|e| VaultError::IoError(e.to_string()))?
                .to_vec(),
        ))
    }

    /// Read a UTF-8 text resource.
    pub async fn read_text(&self, path: &str) -> Result<Option<String>, VaultError> {
        Ok(self
            .read(path)
            .await?
            .map(|bytes| String::from_utf8_lossy(&bytes).to_string()))
    }

    /// Write bytes to a resource, honoring optional ETag preconditions.
    pub async fn write(
        &self,
        path: &str,
        content: Vec<u8>,
        options: WebDavPutOptions,
    ) -> Result<(), VaultError> {
        let mut req = self
            .auth(self.http.put(self.url_for_path(path)))
            .body(content);
        if let Some(content_type) = options.content_type {
            req = req.header("Content-Type", content_type);
        }
        if let Some(etag) = options.if_match {
            req = req.header("If-Match", etag);
        }
        if let Some(etag) = options.if_none_match {
            req = req.header("If-None-Match", etag);
        }
        let resp = req
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("WebDAV PUT failed: {e}")))?;
        if !resp.status().is_success() && resp.status() != reqwest::StatusCode::CREATED {
            return Err(VaultError::IoError(format!(
                "WebDAV PUT {}: {}",
                path,
                resp.status()
            )));
        }
        Ok(())
    }

    /// Create a collection. Existing collections are treated as success.
    pub async fn create_dir(&self, path: &str) -> Result<(), VaultError> {
        self.mkcol(path).await
    }

    /// Delete a resource or collection.
    pub async fn remove(&self, path: &str) -> Result<(), VaultError> {
        self.delete(path).await
    }

    /// Copy a resource or collection.
    pub async fn copy(
        &self,
        from: &str,
        to: &str,
        overwrite: bool,
        depth: Option<&str>,
    ) -> Result<(), VaultError> {
        let mut req = self
            .auth(
                self.http
                    .request(webdav_method(b"COPY")?, self.url_for_path(from)),
            )
            .header("Destination", self.url_for_path(to))
            .header("Overwrite", if overwrite { "T" } else { "F" });
        if let Some(depth) = depth {
            req = req.header("Depth", depth);
        }
        let resp = req
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("WebDAV COPY failed: {e}")))?;
        if !resp.status().is_success() {
            return Err(VaultError::IoError(format!(
                "WebDAV COPY {} -> {}: {}",
                from,
                to,
                resp.status()
            )));
        }
        Ok(())
    }

    /// Move or rename a resource or collection.
    pub async fn move_resource(
        &self,
        from: &str,
        to: &str,
        overwrite: bool,
    ) -> Result<(), VaultError> {
        let resp = self
            .auth(
                self.http
                    .request(webdav_method(b"MOVE")?, self.url_for_path(from)),
            )
            .header("Destination", self.url_for_path(to))
            .header("Overwrite", if overwrite { "T" } else { "F" })
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("WebDAV MOVE failed: {e}")))?;
        if !resp.status().is_success() {
            return Err(VaultError::IoError(format!(
                "WebDAV MOVE {} -> {}: {}",
                from,
                to,
                resp.status()
            )));
        }
        Ok(())
    }

    /// Patch dead properties on a resource.
    pub async fn proppatch(
        &self,
        path: &str,
        set_props: &BTreeMap<String, String>,
        remove_props: &[String],
    ) -> Result<(), VaultError> {
        let mut body = String::from(
            r#"<?xml version="1.0" encoding="utf-8"?><d:propertyupdate xmlns:d="DAV:" xmlns:x="urn:task:props">"#,
        );
        if !set_props.is_empty() {
            body.push_str("<d:set><d:prop>");
            for (name, value) in set_props {
                body.push_str(&format!(
                    "<x:{}>{}</x:{}>",
                    xml_name(name),
                    escape_xml(value),
                    xml_name(name)
                ));
            }
            body.push_str("</d:prop></d:set>");
        }
        if !remove_props.is_empty() {
            body.push_str("<d:remove><d:prop>");
            for name in remove_props {
                body.push_str(&format!("<x:{} />", xml_name(name)));
            }
            body.push_str("</d:prop></d:remove>");
        }
        body.push_str("</d:propertyupdate>");

        let resp = self
            .auth(
                self.http
                    .request(webdav_method(b"PROPPATCH")?, self.url_for_path(path)),
            )
            .header("Content-Type", "application/xml")
            .body(body)
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("WebDAV PROPPATCH failed: {e}")))?;
        if !resp.status().is_success() {
            return Err(VaultError::IoError(format!(
                "WebDAV PROPPATCH {}: {}",
                path,
                resp.status()
            )));
        }
        Ok(())
    }

    /// Acquire an exclusive write lock. Nextcloud may reject locks depending
    /// on server configuration; callers should treat unsupported locks as a
    /// capability issue and fall back to ETags.
    pub async fn lock(
        &self,
        path: &str,
        owner: Option<&str>,
        timeout: Option<&str>,
        depth: Option<&str>,
    ) -> Result<WebDavLock, VaultError> {
        let body = format!(
            r#"<?xml version="1.0" encoding="utf-8"?><d:lockinfo xmlns:d="DAV:"><d:lockscope><d:exclusive/></d:lockscope><d:locktype><d:write/></d:locktype>{}</d:lockinfo>"#,
            owner
                .map(|owner| format!("<d:owner>{}</d:owner>", escape_xml(owner)))
                .unwrap_or_default()
        );
        let mut req = self
            .auth(
                self.http
                    .request(webdav_method(b"LOCK")?, self.url_for_path(path)),
            )
            .header("Content-Type", "application/xml")
            .body(body);
        if let Some(timeout) = timeout {
            req = req.header("Timeout", timeout);
        }
        if let Some(depth) = depth {
            req = req.header("Depth", depth);
        }
        let resp = req
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("WebDAV LOCK failed: {e}")))?;
        if !resp.status().is_success() {
            return Err(VaultError::IoError(format!(
                "WebDAV LOCK {}: {}",
                path,
                resp.status()
            )));
        }
        let token = resp
            .headers()
            .get("Lock-Token")
            .and_then(|h| h.to_str().ok())
            .map(|s| s.trim_matches('<').trim_matches('>').to_string())
            .unwrap_or_default();
        Ok(WebDavLock {
            token,
            owner: owner.map(str::to_string),
            timeout: timeout.map(str::to_string),
        })
    }

    /// Release a write lock.
    pub async fn unlock(&self, path: &str, token: &str) -> Result<(), VaultError> {
        let token = token.trim_matches('<').trim_matches('>');
        let resp = self
            .auth(
                self.http
                    .request(webdav_method(b"UNLOCK")?, self.url_for_path(path)),
            )
            .header("Lock-Token", format!("<{token}>"))
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("WebDAV UNLOCK failed: {e}")))?;
        if !resp.status().is_success() {
            return Err(VaultError::IoError(format!(
                "WebDAV UNLOCK {}: {}",
                path,
                resp.status()
            )));
        }
        Ok(())
    }

    async fn list_dirs(&self, path: &str) -> Result<Vec<String>, VaultError> {
        let xml = self.propfind(path, "1").await?;
        Ok(parse_webdav_entries(&xml)
            .into_iter()
            .filter(|href| href.kind == WebDavResourceKind::Collection)
            .filter_map(|href| self.direct_child_name(&href.path, path))
            .filter(|name| !name.starts_with('.') && !name.starts_with('_'))
            .collect())
    }

    async fn list_md_files(&self, path: &str) -> Result<Vec<String>, VaultError> {
        let xml = self.propfind(path, "1").await?;
        Ok(parse_webdav_entries(&xml)
            .into_iter()
            .filter(|href| href.kind == WebDavResourceKind::File)
            .filter_map(|href| self.direct_child_name(&href.path, path))
            .filter(|name| name.ends_with(".md"))
            .collect())
    }

    fn direct_child_name(&self, href_path: &str, parent_path: &str) -> Option<String> {
        let full_parent = [
            self.config.projects_path.trim_matches('/'),
            parent_path.trim_matches('/'),
        ]
        .into_iter()
        .filter(|part| !part.is_empty())
        .collect::<Vec<_>>()
        .join("/");
        direct_child_name(href_path, &full_parent)
    }

    fn relative_entry(&self, entry: WebDavRawEntry, parent_path: &str) -> Option<WebDavEntry> {
        let full_parent = [
            self.config.projects_path.trim_matches('/'),
            parent_path.trim_matches('/'),
        ]
        .into_iter()
        .filter(|part| !part.is_empty())
        .collect::<Vec<_>>()
        .join("/");
        let relative = relative_path(&entry.path, &self.config.projects_path)?;
        let name = direct_child_name(&entry.path, &full_parent).or_else(|| {
            relative
                .trim_matches('/')
                .rsplit('/')
                .next()
                .map(str::to_string)
        })?;
        Some(WebDavEntry {
            path: relative,
            name,
            kind: entry.kind,
            content_type: entry.content_type,
            content_length: entry.content_length,
            etag: entry.etag,
            last_modified: entry.last_modified,
        })
    }

    async fn get(&self, path: &str) -> Result<Option<String>, VaultError> {
        self.read_text(path).await
    }

    async fn put(&self, path: &str, content: &str) -> Result<(), VaultError> {
        self.write(
            path,
            content.as_bytes().to_vec(),
            WebDavPutOptions {
                content_type: Some("text/markdown; charset=utf-8".to_string()),
                ..Default::default()
            },
        )
        .await
    }

    async fn mkcol(&self, path: &str) -> Result<(), VaultError> {
        let resp = self
            .auth(
                self.http
                    .request(webdav_method(b"MKCOL")?, self.url_for_path(path)),
            )
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("WebDAV MKCOL failed: {e}")))?;
        if !resp.status().is_success()
            && resp.status() != reqwest::StatusCode::METHOD_NOT_ALLOWED
            && resp.status() != reqwest::StatusCode::CONFLICT
        {
            return Err(VaultError::IoError(format!(
                "WebDAV MKCOL {}: {}",
                path,
                resp.status()
            )));
        }
        Ok(())
    }

    async fn ensure_collection(&self, path: &str) -> Result<(), VaultError> {
        let mut current = String::new();
        for part in path
            .trim_matches('/')
            .split('/')
            .filter(|part| !part.is_empty())
        {
            if !current.is_empty() {
                current.push('/');
            }
            current.push_str(part);
            self.mkcol(&current).await?;
        }
        Ok(())
    }

    async fn delete(&self, path: &str) -> Result<(), VaultError> {
        let resp = self
            .auth(self.http.delete(self.url_for_path(path)))
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("WebDAV DELETE failed: {e}")))?;
        if !resp.status().is_success() && resp.status() != reqwest::StatusCode::NOT_FOUND {
            return Err(VaultError::IoError(format!(
                "WebDAV DELETE {}: {}",
                path,
                resp.status()
            )));
        }
        Ok(())
    }

    async fn load_project(&self, project_name: &str) -> Result<Option<ProjectBundle>, VaultError> {
        let project_md_path = format!("{project_name}/project.md");
        let project = match self.get(&project_md_path).await? {
            Some(content) => Vault::parse_project_from_md(&content),
            None => None,
        }
        .unwrap_or_else(|| Project {
            title: project_name.to_string(),
            ..Default::default()
        });

        let tasks_dir = format!("{project_name}/tasks");
        let task_files = self.list_md_files(&tasks_dir).await.unwrap_or_default();
        let mut tasks = Vec::new();
        for file_name in task_files {
            let task_path = format!("{tasks_dir}/{file_name}");
            if let Some(content) = self.get(&task_path).await? {
                if let Some(mut task) = Vault::parse_task_from_md(&content) {
                    let project_link = WikiLink(project.title.clone());
                    if !task.projects.contains(&project_link) {
                        task.projects.push(project_link);
                    }
                    tasks.push(task);
                }
            }
        }

        Ok(Some(ProjectBundle {
            project,
            tasks,
            location: format!("{}/{}", self.projects_url(), encode_path(project_name)),
            source: self.info.name.clone(),
        }))
    }
}

#[async_trait]
impl ProjectProvider for WebDavProvider {
    fn info(&self) -> &ProviderInfo {
        &self.info
    }

    async fn list_projects(&self) -> Result<Vec<Project>, VaultError> {
        let names = self.list_dirs("").await?;
        let mut projects = Vec::new();
        for name in names {
            let project = match self.get(&format!("{name}/project.md")).await? {
                Some(content) => Vault::parse_project_from_md(&content),
                None => None,
            }
            .unwrap_or_else(|| Project {
                title: name,
                ..Default::default()
            });
            projects.push(project);
        }
        Ok(projects)
    }

    async fn get_project(&self, title: &str) -> Result<Option<ProjectBundle>, VaultError> {
        self.load_project(title).await
    }

    async fn list_all(&self) -> Result<Vec<ProjectBundle>, VaultError> {
        let names = self.list_dirs("").await?;
        let mut bundles = Vec::new();
        for name in names {
            if let Some(bundle) = self.load_project(&name).await? {
                bundles.push(bundle);
            }
        }
        Ok(bundles)
    }

    async fn create_project(&self, project: &Project) -> Result<String, VaultError> {
        let project_dir = project.title.as_str();
        self.ensure_collection(project_dir).await?;
        self.ensure_collection(&format!("{project_dir}/tasks"))
            .await?;
        let content = Vault::render_project_file(project, "")?;
        self.put(&format!("{project_dir}/project.md"), &content)
            .await?;
        Ok(format!(
            "{}/{}",
            self.projects_url(),
            encode_path(project_dir)
        ))
    }

    async fn update_project(&self, project: &Project) -> Result<(), VaultError> {
        let path = format!("{}/project.md", project.title);
        let body = match self.get(&path).await? {
            Some(content) => Vault::extract_body(&content).unwrap_or("").to_string(),
            None => String::new(),
        };
        let body = project.body.as_deref().unwrap_or(&body);
        let content = Vault::render_project_file(project, body)?;
        self.put(&path, &content).await
    }

    async fn save_task(&self, project_title: &str, task: &Task) -> Result<(), VaultError> {
        let tasks_dir = format!("{project_title}/tasks");
        self.ensure_collection(&tasks_dir).await?;
        let path = format!("{tasks_dir}/{}.md", task.title);
        let body = match self.get(&path).await? {
            Some(content) => Vault::extract_body(&content).unwrap_or("").to_string(),
            None => task.body.clone(),
        };
        let content = Vault::render_task_file(task, &body)?;
        self.put(&path, &content).await
    }

    async fn delete_task(&self, project_title: &str, task_title: &str) -> Result<(), VaultError> {
        self.delete(&format!("{project_title}/tasks/{task_title}.md"))
            .await
    }
}

#[derive(Debug, Clone)]
struct WebDavRawEntry {
    path: String,
    kind: WebDavResourceKind,
    content_type: Option<String>,
    content_length: Option<u64>,
    etag: Option<String>,
    last_modified: Option<String>,
}

fn webdav_method(bytes: &'static [u8]) -> Result<reqwest::Method, VaultError> {
    reqwest::Method::from_bytes(bytes).map_err(|e| VaultError::IoError(e.to_string()))
}

fn parse_webdav_entries(xml: &str) -> Vec<WebDavRawEntry> {
    let mut out = Vec::new();
    let mut rest = xml;
    while let Some((raw_href, after_href)) = extract_next_tag(rest, "href") {
        let tail = &rest[after_href..];
        let next_response = tail
            .find(":response")
            .or_else(|| tail.find("<response"))
            .unwrap_or(tail.len());
        let response_tail = &tail[..next_response];
        let path = percent_decode(&raw_href);
        let is_collection = response_tail.contains("<d:collection")
            || response_tail.contains("<D:collection")
            || response_tail.contains("<collection")
            || path.ends_with('/');
        out.push(WebDavRawEntry {
            path,
            kind: if is_collection {
                WebDavResourceKind::Collection
            } else {
                WebDavResourceKind::File
            },
            content_type: extract_tag_value(response_tail, "getcontenttype"),
            content_length: extract_tag_value(response_tail, "getcontentlength")
                .and_then(|v| v.parse().ok()),
            etag: extract_tag_value(response_tail, "getetag"),
            last_modified: extract_tag_value(response_tail, "getlastmodified"),
        });
        rest = tail;
    }
    out
}

fn extract_tag_value(xml: &str, local_name: &str) -> Option<String> {
    extract_next_tag(xml, local_name).map(|(value, _)| value)
}

fn extract_next_tag(xml: &str, local_name: &str) -> Option<(String, usize)> {
    let candidates = [
        (format!("<d:{local_name}>"), format!("</d:{local_name}>")),
        (format!("<D:{local_name}>"), format!("</D:{local_name}>")),
        (format!("<{local_name}>"), format!("</{local_name}>")),
    ];
    let (open, close, open_pos) = candidates
        .iter()
        .filter_map(|(open, close)| xml.find(open).map(|pos| (open, close, pos)))
        .min_by_key(|(_, _, pos)| *pos)?;
    let value_start = open_pos + open.len();
    let value_end = xml[value_start..].find(close)? + value_start;
    Some((
        xml[value_start..value_end].trim().to_string(),
        value_end + close.len(),
    ))
}

fn direct_child_name(href_path: &str, parent_path: &str) -> Option<String> {
    let href = href_path.trim_end_matches('/');
    let parent = parent_path.trim_matches('/');
    let href_tail = if parent.is_empty() {
        href.rsplit('/').next()?.to_string()
    } else {
        let parent = format!("/{parent}/");
        let idx = href.find(&parent)?;
        href[idx + parent.len()..].to_string()
    };
    let name = href_tail.trim_matches('/');
    if name.is_empty() || name.contains('/') {
        None
    } else {
        Some(name.to_string())
    }
}

fn relative_path(href_path: &str, root_path: &str) -> Option<String> {
    let href = href_path.trim_matches('/');
    let root = root_path.trim_matches('/');
    if root.is_empty() {
        return Some(
            href.rsplit_once('/')
                .map(|(_, tail)| tail)
                .unwrap_or(href)
                .to_string(),
        );
    }
    if href == root {
        return Some(String::new());
    }
    let marker = format!("/{root}/");
    if let Some(idx) = href.find(&marker) {
        return Some(href[idx + marker.len()..].trim_matches('/').to_string());
    }
    href.strip_prefix(&format!("{root}/"))
        .map(|tail| tail.trim_matches('/').to_string())
}

fn escape_xml(value: &str) -> String {
    value
        .replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&apos;")
}

fn xml_name(name: &str) -> String {
    let mut out = String::new();
    for ch in name.chars() {
        if ch.is_ascii_alphanumeric() || ch == '-' || ch == '_' || ch == '.' {
            out.push(ch);
        } else {
            out.push('_');
        }
    }
    if out.is_empty() || out.as_bytes()[0].is_ascii_digit() {
        out.insert(0, '_');
    }
    out
}

fn encode_path(path: &str) -> String {
    path.split('/')
        .map(percent_encode_segment)
        .collect::<Vec<_>>()
        .join("/")
}

fn percent_encode_segment(segment: &str) -> String {
    let mut out = String::new();
    for byte in segment.bytes() {
        match byte {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'~' => {
                out.push(byte as char)
            }
            _ => out.push_str(&format!("%{byte:02X}")),
        }
    }
    out
}

fn percent_decode(input: &str) -> String {
    let mut out = Vec::new();
    let bytes = input.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%' && i + 2 < bytes.len() {
            if let Ok(hex) = std::str::from_utf8(&bytes[i + 1..i + 3]) {
                if let Ok(v) = u8::from_str_radix(hex, 16) {
                    out.push(v);
                    i += 3;
                    continue;
                }
            }
        }
        out.push(bytes[i]);
        i += 1;
    }
    String::from_utf8_lossy(&out).to_string()
}

#[cfg(test)]
mod tests {
    use std::collections::{BTreeMap, BTreeSet};
    use std::sync::Arc;

    use crate::provider::live_nextcloud_credentials;

    use super::*;
    use tokio::io::{AsyncReadExt, AsyncWriteExt};
    use tokio::net::{TcpListener, TcpStream};
    use tokio::sync::Mutex;

    #[test]
    fn parses_multistatus_direct_children() {
        let xml = r#"
        <d:multistatus xmlns:d="DAV:">
          <d:response>
            <d:href>/remote.php/dav/files/cody/Projects/</d:href>
            <d:propstat><d:prop><d:resourcetype><d:collection/></d:resourcetype></d:prop></d:propstat>
          </d:response>
          <d:response>
            <d:href>/remote.php/dav/files/cody/Projects/Montreal%20Album/</d:href>
            <d:propstat><d:prop><d:resourcetype><d:collection/></d:resourcetype></d:prop></d:propstat>
          </d:response>
          <d:response>
            <d:href>/remote.php/dav/files/cody/Projects/Montreal%20Album/project.md</d:href>
            <d:propstat><d:prop><d:resourcetype/></d:prop></d:propstat>
          </d:response>
        </d:multistatus>
        "#;

        let hrefs = parse_webdav_entries(xml);
        assert_eq!(hrefs.len(), 3);
        let dirs: Vec<_> = hrefs
            .iter()
            .filter(|href| href.kind == WebDavResourceKind::Collection)
            .filter_map(|href| direct_child_name(&href.path, "Projects"))
            .collect();
        assert_eq!(dirs, vec!["Montreal Album"]);
    }

    #[test]
    fn encodes_and_decodes_path_segments() {
        assert_eq!(
            encode_path("Projects/Montreal Album/tasks/A&B.md"),
            "Projects/Montreal%20Album/tasks/A%26B.md"
        );
        assert_eq!(percent_decode("Montreal%20Album"), "Montreal Album");
    }

    #[test]
    fn parses_webdav_file_metadata() {
        let xml = r#"
        <d:multistatus xmlns:d="DAV:">
          <d:response>
            <d:href>/remote.php/dav/files/cody/Projects/A/file.txt</d:href>
            <d:propstat><d:prop>
              <d:getcontenttype>text/plain</d:getcontenttype>
              <d:getcontentlength>42</d:getcontentlength>
              <d:getetag>&quot;abc&quot;</d:getetag>
              <d:getlastmodified>Wed, 29 Apr 2026 18:00:00 GMT</d:getlastmodified>
            </d:prop></d:propstat>
          </d:response>
        </d:multistatus>
        "#;
        let entries = parse_webdav_entries(xml);
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].kind, WebDavResourceKind::File);
        assert_eq!(entries[0].content_type.as_deref(), Some("text/plain"));
        assert_eq!(entries[0].content_length, Some(42));
        assert_eq!(entries[0].etag.as_deref(), Some("&quot;abc&quot;"));
    }

    #[test]
    fn sanitizes_dead_property_names() {
        assert_eq!(xml_name("task color"), "task_color");
        assert_eq!(xml_name("1bad"), "_1bad");
        assert_eq!(escape_xml("A&B <C>"), "A&amp;B &lt;C&gt;");
    }

    #[tokio::test]
    async fn local_webdav_provider_project_task_crud() {
        let server = LocalDavServer::start().await;
        let provider = WebDavProvider::new(
            "local-dav",
            "Local DAV",
            WebDavConfig {
                url: server.base_url(),
                username: "agent".into(),
                password: "secret".into(),
                projects_path: "Projects/".into(),
            },
        );
        let project = Project {
            title: "Local DAV Project".into(),
            ..Default::default()
        };
        let task = Task {
            title: "Write WebDAV integration test".into(),
            body: "Verify real HTTP WebDAV CRUD.".into(),
            projects: vec![WikiLink(project.title.clone())],
            ..Default::default()
        };

        let location = provider.create_project(&project).await.unwrap();
        assert!(location.ends_with("Projects/Local%20DAV%20Project"));

        let projects = provider.list_projects().await.unwrap();
        assert_eq!(projects.len(), 1);
        assert_eq!(projects[0].title, project.title);

        provider.save_task(&project.title, &task).await.unwrap();
        let loaded = provider
            .get_project(&project.title)
            .await
            .unwrap()
            .expect("project should load");
        assert_eq!(loaded.project.title, project.title);
        assert_eq!(loaded.tasks.len(), 1);
        assert_eq!(loaded.tasks[0].title, task.title);
        assert_eq!(loaded.tasks[0].body, task.body);
        assert!(
            loaded.tasks[0]
                .projects
                .iter()
                .any(|link| link.0 == project.title)
        );

        let stat = provider
            .stat(&format!("{}/tasks/{}.md", project.title, task.title))
            .await
            .unwrap()
            .expect("task file should exist");
        assert_eq!(stat.kind, WebDavResourceKind::File);
        assert_eq!(stat.content_type.as_deref(), Some("text/markdown"));

        provider
            .delete_task(&project.title, &task.title)
            .await
            .unwrap();
        let loaded = provider.get_project(&project.title).await.unwrap().unwrap();
        assert!(loaded.tasks.is_empty());

        provider.remove(&project.title).await.unwrap();
        assert!(provider.list_projects().await.unwrap().is_empty());

        let methods = server.methods().await;
        for expected in ["MKCOL", "PUT", "PROPFIND", "GET", "DELETE"] {
            assert!(
                methods.contains(expected),
                "expected local DAV server to receive {expected}, got {methods:?}"
            );
        }
    }

    #[derive(Clone)]
    struct LocalDavServer {
        addr: std::net::SocketAddr,
        state: Arc<Mutex<LocalDavState>>,
    }

    #[derive(Default)]
    struct LocalDavState {
        collections: BTreeSet<String>,
        files: BTreeMap<String, Vec<u8>>,
        methods: BTreeSet<String>,
    }

    struct LocalDavRequest {
        method: String,
        path: String,
        headers: BTreeMap<String, String>,
        body: Vec<u8>,
    }

    struct LocalDavResponse {
        status: u16,
        reason: &'static str,
        content_type: Option<&'static str>,
        body: Vec<u8>,
    }

    impl LocalDavServer {
        async fn start() -> Self {
            let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
            let addr = listener.local_addr().unwrap();
            let state = Arc::new(Mutex::new(LocalDavState::default()));
            state.lock().await.collections.insert("Projects".into());
            let server = Self {
                addr,
                state: state.clone(),
            };
            tokio::spawn(async move {
                loop {
                    let Ok((stream, _)) = listener.accept().await else {
                        break;
                    };
                    let state = state.clone();
                    tokio::spawn(async move {
                        let _ = handle_local_dav_connection(stream, state).await;
                    });
                }
            });
            server
        }

        fn base_url(&self) -> String {
            format!("http://{}/dav/", self.addr)
        }

        async fn methods(&self) -> BTreeSet<String> {
            self.state.lock().await.methods.clone()
        }
    }

    async fn handle_local_dav_connection(
        mut stream: TcpStream,
        state: Arc<Mutex<LocalDavState>>,
    ) -> std::io::Result<()> {
        let request = read_local_dav_request(&mut stream).await?;
        let response = handle_local_dav_request(request, state).await;
        write_local_dav_response(&mut stream, response).await
    }

    async fn read_local_dav_request(stream: &mut TcpStream) -> std::io::Result<LocalDavRequest> {
        let mut buffer = Vec::new();
        let header_end = loop {
            let mut chunk = [0_u8; 1024];
            let n = stream.read(&mut chunk).await?;
            if n == 0 {
                break None;
            }
            buffer.extend_from_slice(&chunk[..n]);
            if let Some(pos) = find_header_end(&buffer) {
                break Some(pos);
            }
        }
        .ok_or_else(|| std::io::Error::new(std::io::ErrorKind::UnexpectedEof, "no headers"))?;

        let header_bytes = &buffer[..header_end];
        let header_text = String::from_utf8_lossy(header_bytes);
        let mut lines = header_text.split("\r\n");
        let request_line = lines.next().unwrap_or_default();
        let mut request_parts = request_line.split_whitespace();
        let method = request_parts.next().unwrap_or_default().to_string();
        let path = request_parts.next().unwrap_or_default().to_string();
        let headers = lines
            .filter_map(|line| line.split_once(':'))
            .map(|(name, value)| (name.to_ascii_lowercase(), value.trim().to_string()))
            .collect::<BTreeMap<_, _>>();
        let content_length = headers
            .get("content-length")
            .and_then(|value| value.parse::<usize>().ok())
            .unwrap_or(0);
        let body_start = header_end + 4;
        let mut body = buffer[body_start..].to_vec();
        while body.len() < content_length {
            let mut chunk = vec![0_u8; content_length - body.len()];
            let n = stream.read(&mut chunk).await?;
            if n == 0 {
                break;
            }
            body.extend_from_slice(&chunk[..n]);
        }
        body.truncate(content_length);

        Ok(LocalDavRequest {
            method,
            path,
            headers,
            body,
        })
    }

    fn find_header_end(buffer: &[u8]) -> Option<usize> {
        buffer.windows(4).position(|window| window == b"\r\n\r\n")
    }

    async fn handle_local_dav_request(
        request: LocalDavRequest,
        state: Arc<Mutex<LocalDavState>>,
    ) -> LocalDavResponse {
        let mut state = state.lock().await;
        state.methods.insert(request.method.clone());
        let path = request_path_to_dav_path(&request.path);
        match request.method.as_str() {
            "PROPFIND" => {
                if !state.collections.contains(&path) && !state.files.contains_key(&path) {
                    return response(404, "Not Found", None, Vec::new());
                }
                let depth = request
                    .headers
                    .get("depth")
                    .map(String::as_str)
                    .unwrap_or("infinity");
                let xml = multistatus_xml(&state, &path, depth);
                response(
                    207,
                    "Multi-Status",
                    Some("application/xml"),
                    xml.into_bytes(),
                )
            }
            "MKCOL" => {
                if state.collections.contains(&path) {
                    return response(405, "Method Not Allowed", None, Vec::new());
                }
                if let Some(parent) = parent_path(&path) {
                    if !state.collections.contains(&parent) {
                        return response(409, "Conflict", None, Vec::new());
                    }
                }
                state.collections.insert(path);
                response(201, "Created", None, Vec::new())
            }
            "PUT" => {
                if let Some(parent) = parent_path(&path) {
                    if !state.collections.contains(&parent) {
                        return response(409, "Conflict", None, Vec::new());
                    }
                }
                state.files.insert(path, request.body);
                response(201, "Created", None, Vec::new())
            }
            "GET" => match state.files.get(&path) {
                Some(body) => response(200, "OK", Some("text/markdown"), body.clone()),
                None => response(404, "Not Found", None, Vec::new()),
            },
            "DELETE" => {
                remove_path(&mut state, &path);
                response(204, "No Content", None, Vec::new())
            }
            _ => response(405, "Method Not Allowed", None, Vec::new()),
        }
    }

    fn request_path_to_dav_path(path: &str) -> String {
        let path = path.split('?').next().unwrap_or(path);
        let path = percent_decode(path.trim_start_matches('/'));
        path.strip_prefix("dav/")
            .unwrap_or(&path)
            .trim_matches('/')
            .to_string()
    }

    fn parent_path(path: &str) -> Option<String> {
        path.rsplit_once('/').map(|(parent, _)| parent.to_string())
    }

    fn remove_path(state: &mut LocalDavState, path: &str) {
        state.files.remove(path);
        state.collections.remove(path);
        let prefix = format!("{path}/");
        state
            .files
            .retain(|candidate, _| !candidate.starts_with(&prefix));
        state
            .collections
            .retain(|candidate| !candidate.starts_with(&prefix));
    }

    fn multistatus_xml(state: &LocalDavState, path: &str, depth: &str) -> String {
        let mut paths = Vec::new();
        paths.push(path.to_string());
        if depth != "0" {
            let prefix = if path.is_empty() {
                String::new()
            } else {
                format!("{path}/")
            };
            for collection in &state.collections {
                if is_direct_child(collection, &prefix) {
                    paths.push(collection.clone());
                }
            }
            for file in state.files.keys() {
                if is_direct_child(file, &prefix) {
                    paths.push(file.clone());
                }
            }
        }
        paths.sort();
        paths.dedup();

        let mut xml =
            String::from(r#"<?xml version="1.0" encoding="utf-8"?><d:multistatus xmlns:d="DAV:">"#);
        for entry_path in paths {
            let is_collection = state.collections.contains(&entry_path);
            let href = format!(
                "/dav/{}{}",
                encode_path(&entry_path),
                if is_collection { "/" } else { "" }
            );
            let content_length = state.files.get(&entry_path).map(Vec::len).unwrap_or(0);
            let content_type = if is_collection {
                "httpd/unix-directory"
            } else {
                "text/markdown"
            };
            let resource_type = if is_collection {
                "<d:resourcetype><d:collection/></d:resourcetype>"
            } else {
                "<d:resourcetype/>"
            };
            xml.push_str(&format!(
                "<d:response><d:href>{}</d:href><d:propstat><d:prop>{}<d:getcontenttype>{}</d:getcontenttype><d:getcontentlength>{}</d:getcontentlength><d:getetag>\"{}\"</d:getetag><d:getlastmodified>Wed, 29 Apr 2026 18:00:00 GMT</d:getlastmodified></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat></d:response>",
                escape_xml(&href),
                resource_type,
                content_type,
                content_length,
                escape_xml(&entry_path)
            ));
        }
        xml.push_str("</d:multistatus>");
        xml
    }

    fn is_direct_child(candidate: &str, prefix: &str) -> bool {
        candidate
            .strip_prefix(prefix)
            .map(|tail| !tail.is_empty() && !tail.contains('/'))
            .unwrap_or(false)
    }

    fn response(
        status: u16,
        reason: &'static str,
        content_type: Option<&'static str>,
        body: Vec<u8>,
    ) -> LocalDavResponse {
        LocalDavResponse {
            status,
            reason,
            content_type,
            body,
        }
    }

    async fn write_local_dav_response(
        stream: &mut TcpStream,
        response: LocalDavResponse,
    ) -> std::io::Result<()> {
        let mut headers = format!(
            "HTTP/1.1 {} {}\r\nContent-Length: {}\r\nConnection: close\r\n",
            response.status,
            response.reason,
            response.body.len()
        );
        if let Some(content_type) = response.content_type {
            headers.push_str(&format!("Content-Type: {content_type}\r\n"));
        }
        headers.push_str("\r\n");
        stream.write_all(headers.as_bytes()).await?;
        stream.write_all(&response.body).await
    }

    #[tokio::test]
    #[ignore = "requires live Nextcloud credentials"]
    async fn live_nextcloud_file_management_smoke() {
        let credentials = live_nextcloud_credentials();
        let provider = WebDavProvider::new(
            "nextcloud",
            "Nextcloud",
            WebDavConfig {
                url: format!(
                    "{}/remote.php/dav/files/{}/",
                    credentials.url.trim_end_matches('/'),
                    credentials.username
                ),
                username: credentials.username,
                password: credentials.password,
                projects_path: credentials.projects_path,
            },
        );
        let suffix = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs();
        let dir = format!("_task_webdav_smoke_{suffix}");
        let file = format!("{dir}/hello.txt");
        let copy = format!("{dir}/hello-copy.txt");
        let moved = format!("{dir}/hello-moved.txt");

        provider.create_dir(&dir).await.unwrap();
        provider
            .write(
                &file,
                b"hello webdav".to_vec(),
                WebDavPutOptions {
                    content_type: Some("text/plain".to_string()),
                    ..Default::default()
                },
            )
            .await
            .unwrap();
        let stat = provider.stat(&file).await.unwrap().expect("stat file");
        assert_eq!(stat.kind, WebDavResourceKind::File);
        assert_eq!(
            provider.read_text(&file).await.unwrap().as_deref(),
            Some("hello webdav")
        );
        provider.copy(&file, &copy, true, None).await.unwrap();
        provider.move_resource(&copy, &moved, true).await.unwrap();
        let names: Vec<_> = provider
            .list(&dir, "1")
            .await
            .unwrap()
            .into_iter()
            .map(|entry| entry.name)
            .collect();
        assert!(names.iter().any(|name| name == "hello.txt"));
        assert!(names.iter().any(|name| name == "hello-moved.txt"));
        provider.remove(&dir).await.unwrap();
    }

    #[tokio::test]
    #[ignore = "requires live Nextcloud credentials"]
    async fn live_nextcloud_project_crud_smoke() {
        let credentials = live_nextcloud_credentials();
        let provider = WebDavProvider::new(
            "nextcloud",
            "Nextcloud",
            WebDavConfig {
                url: format!(
                    "{}/remote.php/dav/files/{}/",
                    credentials.url.trim_end_matches('/'),
                    credentials.username
                ),
                username: credentials.username,
                password: credentials.password,
                projects_path: credentials.projects_path,
            },
        );
        let suffix = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs();
        let project = Project {
            title: format!("_task_webdav_project_smoke_{suffix}"),
            ..Default::default()
        };
        let task = Task {
            title: "Smoke task".to_string(),
            body: "Created by live WebDAV smoke test".to_string(),
            ..Default::default()
        };

        provider.create_project(&project).await.unwrap();
        provider.save_task(&project.title, &task).await.unwrap();
        let loaded = provider
            .get_project(&project.title)
            .await
            .unwrap()
            .expect("created project");
        assert_eq!(loaded.project.title, project.title);
        assert!(loaded.tasks.iter().any(|task| task.title == "Smoke task"));
        provider
            .delete_task(&project.title, "Smoke task")
            .await
            .unwrap();
        provider.remove(&project.title).await.unwrap();
    }
}
