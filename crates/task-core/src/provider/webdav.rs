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
  </d:prop>
</d:propfind>"#;
        let resp = self
            .auth(
                self.http
                    .request(webdav_method(b"PROPFIND")?, self.url_for_path(path)),
            )
            .header("Content-Type", "application/xml")
            .header("Depth", depth)
            .body(body)
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

    async fn list_dirs(&self, path: &str) -> Result<Vec<String>, VaultError> {
        let xml = self.propfind(path, "1").await?;
        Ok(parse_webdav_hrefs(&xml)
            .into_iter()
            .filter(|href| href.is_collection)
            .filter_map(|href| self.direct_child_name(&href.path, path))
            .filter(|name| !name.starts_with('.') && !name.starts_with('_'))
            .collect())
    }

    async fn list_md_files(&self, path: &str) -> Result<Vec<String>, VaultError> {
        let xml = self.propfind(path, "1").await?;
        Ok(parse_webdav_hrefs(&xml)
            .into_iter()
            .filter(|href| !href.is_collection)
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

    async fn get(&self, path: &str) -> Result<Option<String>, VaultError> {
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
        resp.text()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))
            .map(Some)
    }

    async fn put(&self, path: &str, content: &str) -> Result<(), VaultError> {
        let resp = self
            .auth(self.http.put(self.url_for_path(path)))
            .header("Content-Type", "text/markdown; charset=utf-8")
            .body(content.to_string())
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
        let content = Vault::render_project_file(project, &body)?;
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
struct WebDavHref {
    path: String,
    is_collection: bool,
}

fn webdav_method(bytes: &'static [u8]) -> Result<reqwest::Method, VaultError> {
    reqwest::Method::from_bytes(bytes).map_err(|e| VaultError::IoError(e.to_string()))
}

fn parse_webdav_hrefs(xml: &str) -> Vec<WebDavHref> {
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
        out.push(WebDavHref {
            path,
            is_collection,
        });
        rest = tail;
    }
    out
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
    use super::*;

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

        let hrefs = parse_webdav_hrefs(xml);
        assert_eq!(hrefs.len(), 3);
        let dirs: Vec<_> = hrefs
            .iter()
            .filter(|href| href.is_collection)
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
}
