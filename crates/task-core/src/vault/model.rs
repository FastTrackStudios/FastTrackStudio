// r[impl task.file]
use std::fs;
use std::path::{Path, PathBuf};

use crate::asset::Asset;
use crate::calendar_event::CalendarEvent;
use crate::client::Client;
use crate::expense::Expense;
use crate::invoice::Invoice;
use crate::location::Location;
use crate::project::Project;
use crate::revenue::Revenue;
use crate::service::VaultError;
use crate::task::Task;

pub struct Vault {
    pub root: PathBuf,
}

impl Vault {
    pub fn new(root: impl AsRef<Path>) -> Self {
        Self {
            root: root.as_ref().to_path_buf(),
        }
    }

    // r[impl task.file]
    /// Walk the vault and load all parseable tasks.
    pub fn load_tasks(&self) -> Vec<Task> {
        self.walk_md_files()
            .filter_map(|content| Self::parse_task_from_md(&content))
            .collect()
    }

    /// Walk the vault and load all parseable projects.
    pub fn load_projects(&self) -> Vec<Project> {
        self.walk_md_files()
            .filter_map(|content| Self::parse_project_from_md(&content))
            .collect()
    }

    /// Load all first-class calendar events.
    pub fn load_calendar_events(&self) -> Vec<CalendarEvent> {
        let dir = self.root.join("calendar");
        if !dir.exists() {
            return vec![];
        }
        Self::walk_md_in(&dir)
            .filter_map(|content| Self::parse_calendar_event_from_md(&content))
            .collect()
    }

    /// Load all asset records.
    pub fn load_assets(&self) -> Vec<Asset> {
        let dir = self.root.join("assets");
        if !dir.exists() {
            return vec![];
        }
        Self::walk_md_in(&dir)
            .filter_map(|content| Self::parse_asset_from_md(&content))
            .collect()
    }

    /// Load all reusable location / venue records.
    pub fn load_locations(&self) -> Vec<Location> {
        let dir = self.root.join("locations");
        if !dir.exists() {
            return vec![];
        }
        Self::walk_md_in(&dir)
            .filter_map(|content| Self::parse_location_from_md(&content))
            .collect()
    }

    /// Walk a specific subdirectory (relative to root) and load projects.
    pub fn load_projects_in(&self, subdir: &str) -> Vec<Project> {
        let dir = self.root.join(subdir);
        if !dir.exists() {
            return vec![];
        }
        Self::walk_md_in(&dir)
            .filter_map(|content| Self::parse_project_from_md(&content))
            .collect()
    }

    /// Walk a specific subdirectory (relative to root) and load tasks.
    pub fn load_tasks_in(&self, subdir: &str) -> Vec<Task> {
        let dir = self.root.join(subdir);
        if !dir.exists() {
            return vec![];
        }
        Self::walk_md_in(&dir)
            .filter_map(|content| Self::parse_task_from_md(&content))
            .collect()
    }

    // r[impl task.dates.created-modified]
    /// Write a task back to the vault. Uses task.body for content after frontmatter.
    /// If task.body is empty and the file already exists, preserves existing body.
    pub fn save_task(&self, task: &Task) -> Result<(), VaultError> {
        let path = self.task_path(&task.title);
        let body = if !task.body.is_empty() {
            task.body.clone()
        } else if path.exists() {
            let content =
                fs::read_to_string(&path).map_err(|e| VaultError::IoError(e.to_string()))?;
            Self::extract_body(&content).unwrap_or("").to_string()
        } else {
            String::new()
        };

        let content = Self::render_task_file(task, &body)?;
        Self::atomic_write(&path, &content)
    }

    /// Write a project back to the vault. Creates the file if missing, preserves body.
    pub fn save_project(&self, project: &Project) -> Result<(), VaultError> {
        let path = self.project_path(&project.title);
        let body = if let Some(body) = project.body.as_ref() {
            body.clone()
        } else if path.exists() {
            let content =
                fs::read_to_string(&path).map_err(|e| VaultError::IoError(e.to_string()))?;
            Self::extract_body(&content).unwrap_or("").to_string()
        } else {
            String::new()
        };

        let content = Self::render_project_file(project, &body)?;
        Self::atomic_write(&path, &content)
    }

    /// Write a project to a specific subdirectory (e.g. "Projects").
    pub fn save_project_in(&self, subdir: &str, project: &Project) -> Result<(), VaultError> {
        let dir = self.root.join(subdir);
        let path = dir.join(format!("{}/project.md", project.title));
        let body = if let Some(body) = project.body.as_ref() {
            body.clone()
        } else if path.exists() {
            let content =
                fs::read_to_string(&path).map_err(|e| VaultError::IoError(e.to_string()))?;
            Self::extract_body(&content).unwrap_or("").to_string()
        } else {
            String::new()
        };

        let content = Self::render_project_file(project, &body)?;
        Self::atomic_write(&path, &content)
    }

    // ── Helpers ──────────────────────────────────────────────────────

    // r[impl sync.atomic-write]
    pub fn atomic_write(path: &Path, content: &str) -> Result<(), VaultError> {
        let tmp_path = path.with_extension("md.tmp");
        fs::create_dir_all(path.parent().unwrap())
            .map_err(|e| VaultError::IoError(e.to_string()))?;
        fs::write(&tmp_path, content).map_err(|e| VaultError::IoError(e.to_string()))?;
        fs::rename(&tmp_path, path).map_err(|e| VaultError::IoError(e.to_string()))?;
        Ok(())
    }

    fn task_path(&self, title: &str) -> PathBuf {
        self.root.join(format!("{}.md", title))
    }

    fn project_path(&self, title: &str) -> PathBuf {
        self.root.join(format!("{}/project.md", title))
    }

    fn walk_md_files(&self) -> impl Iterator<Item = String> {
        Self::walk_md_in(&self.root)
    }

    fn walk_md_in(dir: &Path) -> impl Iterator<Item = String> {
        walkdir::WalkDir::new(dir)
            .follow_links(false)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| e.path().extension().and_then(|s| s.to_str()) == Some("md"))
            .filter_map(|e| fs::read_to_string(e.path()).ok())
    }

    pub fn parse_task_from_md(content: &str) -> Option<Task> {
        let (frontmatter, body) = Self::split_frontmatter(content)?;
        let mut task = serde_yaml::from_str::<Task>(frontmatter).ok()?;
        if task.id == uuid::Uuid::nil() {
            if let Some(id) = frontmatter_string(frontmatter, "id") {
                match uuid::Uuid::parse_str(&id) {
                    Ok(uuid) => task.id = uuid,
                    Err(_) => {
                        if task.external_id.is_none() {
                            task.external_id = Some(id);
                        }
                        task.id = uuid::Uuid::new_v4();
                    }
                }
            }
        }
        task.body = body.to_string();
        Some(task)
    }

    pub fn parse_project_from_md(content: &str) -> Option<Project> {
        let (frontmatter, body) = Self::split_frontmatter(content)?;
        let mut project = serde_yaml::from_str::<Project>(frontmatter).ok()?;
        if project.id == uuid::Uuid::nil() {
            project.id = project_id_for_title(&project.title);
        }
        project.body = Some(body.to_string());
        Some(project)
    }

    pub fn parse_client_from_md(content: &str) -> Option<Client> {
        let (frontmatter, _body) = Self::split_frontmatter(content)?;
        facet_yaml::from_str::<Client>(frontmatter).ok()
    }

    pub fn parse_calendar_event_from_md(content: &str) -> Option<CalendarEvent> {
        let (frontmatter, body) = Self::split_frontmatter(content)?;
        let mut event = facet_yaml::from_str::<CalendarEvent>(frontmatter).ok()?;
        event.body = body.to_string();
        Some(event)
    }

    pub fn parse_location_from_md(content: &str) -> Option<Location> {
        let (frontmatter, body) = Self::split_frontmatter(content)?;
        let mut location = facet_yaml::from_str::<Location>(frontmatter).ok()?;
        location.body = body.to_string();
        Some(location)
    }

    pub fn render_calendar_event_file(
        event: &CalendarEvent,
        body: &str,
    ) -> Result<String, VaultError> {
        let yaml =
            facet_yaml::to_string(event).map_err(|e| VaultError::ParseError(e.to_string()))?;
        let yaml = yaml.strip_prefix("---\n").unwrap_or(&yaml);
        let body = if body.is_empty() { &event.body } else { body };
        Ok(format!("---\n{}---\n{}", yaml, body))
    }

    pub fn save_calendar_event(&self, event: &CalendarEvent) -> Result<(), VaultError> {
        let dir = self.root.join("calendar");
        std::fs::create_dir_all(&dir).map_err(|e| VaultError::IoError(e.to_string()))?;
        let path = dir.join(format!("{}.md", safe_file_name(&event.title)));
        let body = if !event.body.is_empty() {
            event.body.clone()
        } else if path.exists() {
            let content =
                fs::read_to_string(&path).map_err(|e| VaultError::IoError(e.to_string()))?;
            Self::extract_body(&content).unwrap_or("").to_string()
        } else {
            String::new()
        };
        let content = Self::render_calendar_event_file(event, &body)?;
        Self::atomic_write(&path, &content)
    }

    pub fn render_location_file(location: &Location, body: &str) -> Result<String, VaultError> {
        let yaml =
            facet_yaml::to_string(location).map_err(|e| VaultError::ParseError(e.to_string()))?;
        let yaml = yaml.strip_prefix("---\n").unwrap_or(&yaml);
        let body = if body.is_empty() {
            &location.body
        } else {
            body
        };
        Ok(format!("---\n{}---\n{}", yaml, body))
    }

    pub fn save_location(&self, location: &Location) -> Result<(), VaultError> {
        let dir = self.root.join("locations");
        std::fs::create_dir_all(&dir).map_err(|e| VaultError::IoError(e.to_string()))?;
        let path = dir.join(format!("{}.md", safe_file_name(&location.name)));
        let body = if !location.body.is_empty() {
            location.body.clone()
        } else if path.exists() {
            let content =
                fs::read_to_string(&path).map_err(|e| VaultError::IoError(e.to_string()))?;
            Self::extract_body(&content).unwrap_or("").to_string()
        } else {
            crate::location::render_location_body(location)
        };
        let content = Self::render_location_file(location, &body)?;
        Self::atomic_write(&path, &content)
    }

    pub fn delete_location(&self, reference: &str) -> Result<(), VaultError> {
        for location in self.load_locations() {
            if location.id.as_deref() == Some(reference)
                || location.name.eq_ignore_ascii_case(reference)
            {
                let path = self
                    .root
                    .join("locations")
                    .join(format!("{}.md", safe_file_name(&location.name)));
                if path.exists() {
                    std::fs::remove_file(&path).map_err(|e| VaultError::IoError(e.to_string()))?;
                }
                return Ok(());
            }
        }
        Err(VaultError::NotFound(reference.to_string()))
    }

    pub fn delete_calendar_event(&self, event_id: &str) -> Result<(), VaultError> {
        for event in self.load_calendar_events() {
            if event.id.as_deref() == Some(event_id) || event.title == event_id {
                let path = self
                    .root
                    .join("calendar")
                    .join(format!("{}.md", safe_file_name(&event.title)));
                if path.exists() {
                    std::fs::remove_file(&path).map_err(|e| VaultError::IoError(e.to_string()))?;
                }
                return Ok(());
            }
        }
        Ok(())
    }

    pub fn render_asset_file(asset: &Asset, body: &str) -> Result<String, VaultError> {
        let yaml =
            facet_yaml::to_string(asset).map_err(|e| VaultError::ParseError(e.to_string()))?;
        let yaml = yaml.strip_prefix("---\n").unwrap_or(&yaml);
        let body = if body.is_empty() { &asset.body } else { body };
        Ok(format!("---\n{}---\n{}", yaml, body))
    }

    pub fn parse_asset_from_md(content: &str) -> Option<Asset> {
        let (frontmatter, body) = Self::split_frontmatter(content)?;
        let mut asset = facet_yaml::from_str::<Asset>(frontmatter).ok()?;
        asset.body = body.to_string();
        Some(asset)
    }

    pub fn save_asset(&self, asset: &Asset) -> Result<(), VaultError> {
        let dir = self.root.join("assets");
        std::fs::create_dir_all(&dir).map_err(|e| VaultError::IoError(e.to_string()))?;
        let path = dir.join(format!("{}.md", safe_file_name(&asset.name)));
        let body = if !asset.body.is_empty() {
            asset.body.clone()
        } else if path.exists() {
            let content =
                fs::read_to_string(&path).map_err(|e| VaultError::IoError(e.to_string()))?;
            Self::extract_body(&content).unwrap_or("").to_string()
        } else {
            String::new()
        };
        let content = Self::render_asset_file(asset, &body)?;
        Self::atomic_write(&path, &content)
    }

    pub fn delete_asset(&self, asset_id: &str) -> Result<(), VaultError> {
        for asset in self.load_assets() {
            if asset.id.eq_ignore_ascii_case(asset_id) || asset.name.eq_ignore_ascii_case(asset_id)
            {
                let path = self
                    .root
                    .join("assets")
                    .join(format!("{}.md", safe_file_name(&asset.name)));
                if path.exists() {
                    std::fs::remove_file(&path).map_err(|e| VaultError::IoError(e.to_string()))?;
                }
                return Ok(());
            }
        }
        Err(VaultError::NotFound(asset_id.to_string()))
    }

    pub fn render_client_file(client: &Client, body: &str) -> Result<String, VaultError> {
        let yaml =
            facet_yaml::to_string(client).map_err(|e| VaultError::ParseError(e.to_string()))?;
        let yaml = yaml.strip_prefix("---\n").unwrap_or(&yaml);
        Ok(format!("---\n{}---\n{}", yaml, body))
    }

    /// Load all clients from the `clients/` subdirectory.
    pub fn load_clients(&self) -> Vec<Client> {
        let dir = self.root.join("clients");
        if !dir.exists() {
            return vec![];
        }
        Self::walk_md_in(&dir)
            .filter_map(|content| Self::parse_client_from_md(&content))
            .collect()
    }

    /// Save a client to `clients/<name>.md`.
    pub fn save_client(&self, client: &Client) -> Result<(), VaultError> {
        let dir = self.root.join("clients");
        std::fs::create_dir_all(&dir).map_err(|e| VaultError::IoError(e.to_string()))?;
        let path = dir.join(format!("{}.md", client.name));
        let body = if path.exists() {
            let content =
                fs::read_to_string(&path).map_err(|e| VaultError::IoError(e.to_string()))?;
            Self::extract_body(&content).unwrap_or("").to_string()
        } else {
            String::new()
        };
        let content = Self::render_client_file(client, &body)?;
        Self::atomic_write(&path, &content)
    }

    // ── Invoice I/O ──────────────────────────────────────────────────────────

    pub fn parse_invoice_from_md(content: &str) -> Option<Invoice> {
        let (frontmatter, _body) = Self::split_frontmatter(content)?;
        facet_yaml::from_str::<Invoice>(frontmatter).ok()
    }

    pub fn render_invoice_file(invoice: &Invoice, body: &str) -> Result<String, VaultError> {
        let yaml =
            facet_yaml::to_string(invoice).map_err(|e| VaultError::ParseError(e.to_string()))?;
        let yaml = yaml.strip_prefix("---\n").unwrap_or(&yaml);
        Ok(format!("---\n{}---\n{}", yaml, body))
    }

    /// Load all invoices from the `invoices/` subdirectory.
    pub fn load_invoices(&self) -> Vec<Invoice> {
        let dir = self.root.join("invoices");
        if !dir.exists() {
            return vec![];
        }
        Self::walk_md_in(&dir)
            .filter_map(|content| Self::parse_invoice_from_md(&content))
            .collect()
    }

    /// Save an invoice to `invoices/<id>.md`. Overwrites any existing file
    /// at the same id (edits are the common case).
    pub fn save_invoice(&self, invoice: &Invoice) -> Result<(), VaultError> {
        let dir = self.root.join("invoices");
        std::fs::create_dir_all(&dir).map_err(|e| VaultError::IoError(e.to_string()))?;
        let path = dir.join(format!("{}.md", invoice.id));
        // Re-render the human-friendly body from the frontmatter every
        // time — keeps the markdown representation in sync with the
        // authoritative YAML. Users who want custom body text can still
        // edit it; it just won't survive a service-side update.
        let body = crate::invoice::render_invoice_body(invoice);
        let content = Self::render_invoice_file(invoice, &body)?;
        Self::atomic_write(&path, &content)
    }

    /// Delete an invoice file by id.
    pub fn delete_invoice(&self, invoice_id: &str) -> Result<(), VaultError> {
        let path = self.root.join("invoices").join(format!("{invoice_id}.md"));
        if path.exists() {
            std::fs::remove_file(&path).map_err(|e| VaultError::IoError(e.to_string()))?;
        }
        Ok(())
    }

    // ── Expense I/O ───────────────────────────────────────────────────────────

    pub fn parse_expense_from_md(content: &str) -> Option<Expense> {
        let (frontmatter, _body) = Self::split_frontmatter(content)?;
        facet_yaml::from_str::<Expense>(frontmatter).ok()
    }

    pub fn render_expense_file(expense: &Expense, body: &str) -> Result<String, VaultError> {
        let yaml =
            facet_yaml::to_string(expense).map_err(|e| VaultError::ParseError(e.to_string()))?;
        let yaml = yaml.strip_prefix("---\n").unwrap_or(&yaml);
        Ok(format!("---\n{}---\n{}", yaml, body))
    }

    /// Load all expenses from the `expenses/` subdirectory.
    pub fn load_expenses(&self) -> Vec<Expense> {
        let dir = self.root.join("expenses");
        if !dir.exists() {
            return vec![];
        }
        Self::walk_md_in(&dir)
            .filter_map(|content| Self::parse_expense_from_md(&content))
            .collect()
    }

    /// Save an expense to `expenses/<id>.md`.
    pub fn save_expense(&self, expense: &Expense) -> Result<(), VaultError> {
        let dir = self.root.join("expenses");
        std::fs::create_dir_all(&dir).map_err(|e| VaultError::IoError(e.to_string()))?;
        let path = dir.join(format!("{}.md", expense.id));
        let body = crate::expense::render_expense_body(expense);
        let content = Self::render_expense_file(expense, &body)?;
        Self::atomic_write(&path, &content)
    }

    /// Delete an expense file by id.
    pub fn delete_expense(&self, expense_id: &str) -> Result<(), VaultError> {
        let path = self.root.join("expenses").join(format!("{expense_id}.md"));
        if path.exists() {
            std::fs::remove_file(&path).map_err(|e| VaultError::IoError(e.to_string()))?;
        }
        Ok(())
    }

    // ── Revenue I/O ──────────────────────────────────────────────────────────

    pub fn parse_revenue_from_md(content: &str) -> Option<Revenue> {
        let (frontmatter, _body) = Self::split_frontmatter(content)?;
        facet_yaml::from_str::<Revenue>(frontmatter).ok()
    }

    pub fn render_revenue_file(revenue: &Revenue, body: &str) -> Result<String, VaultError> {
        let yaml =
            facet_yaml::to_string(revenue).map_err(|e| VaultError::ParseError(e.to_string()))?;
        let yaml = yaml.strip_prefix("---\n").unwrap_or(&yaml);
        Ok(format!("---\n{}---\n{}", yaml, body))
    }

    /// Load all revenue records from the `revenue/` subdirectory.
    pub fn load_revenues(&self) -> Vec<Revenue> {
        let dir = self.root.join("revenue");
        if !dir.exists() {
            return vec![];
        }
        Self::walk_md_in(&dir)
            .filter_map(|content| Self::parse_revenue_from_md(&content))
            .collect()
    }

    /// Save revenue to `revenue/<id>.md`.
    pub fn save_revenue(&self, revenue: &Revenue) -> Result<(), VaultError> {
        let dir = self.root.join("revenue");
        std::fs::create_dir_all(&dir).map_err(|e| VaultError::IoError(e.to_string()))?;
        let path = dir.join(format!("{}.md", revenue.id));
        let body = crate::revenue::render_revenue_body(revenue);
        let content = Self::render_revenue_file(revenue, &body)?;
        Self::atomic_write(&path, &content)
    }

    /// Delete a revenue file by id.
    pub fn delete_revenue(&self, revenue_id: &str) -> Result<(), VaultError> {
        let path = self.root.join("revenue").join(format!("{revenue_id}.md"));
        if path.exists() {
            std::fs::remove_file(&path).map_err(|e| VaultError::IoError(e.to_string()))?;
        }
        Ok(())
    }

    pub fn render_task_file(task: &Task, body: &str) -> Result<String, VaultError> {
        let yaml =
            serde_yaml::to_string(task).map_err(|e| VaultError::ParseError(e.to_string()))?;
        let yaml = yaml.strip_prefix("---\n").unwrap_or(&yaml);
        // Use provided body, or fall back to task.body
        let body = if body.is_empty() { &task.body } else { body };
        Ok(format!("---\n{}---\n{}", yaml, body))
    }

    pub fn render_project_file(project: &Project, body: &str) -> Result<String, VaultError> {
        let yaml =
            serde_yaml::to_string(project).map_err(|e| VaultError::ParseError(e.to_string()))?;
        let yaml = yaml.strip_prefix("---\n").unwrap_or(&yaml);
        Ok(format!("---\n{}---\n{}", yaml, body))
    }

    /// Split "---\nFRONTMATTER\n---\nBODY" into (frontmatter, body).
    pub fn split_frontmatter(content: &str) -> Option<(&str, &str)> {
        let content = content.trim_start();
        if !content.starts_with("---") {
            return None;
        }
        let rest = &content[3..];
        let end = rest.find("\n---")?;
        let frontmatter = &rest[..end].trim_start_matches('\n');
        let body = &rest[end + 4..];
        let body = body.trim_start_matches('\n');
        Some((frontmatter, body))
    }

    pub fn extract_body(content: &str) -> Option<&str> {
        Self::split_frontmatter(content).map(|(_, body)| body)
    }
}

fn project_id_for_title(title: &str) -> uuid::Uuid {
    uuid::Uuid::new_v5(
        &uuid::Uuid::NAMESPACE_URL,
        format!("task-core:project:{title}").as_bytes(),
    )
}

fn safe_file_name(title: &str) -> String {
    title.replace('/', "-")
}

fn frontmatter_string(frontmatter: &str, key: &str) -> Option<String> {
    let prefix = format!("{key}:");
    frontmatter.lines().find_map(|line| {
        let value = line.trim().strip_prefix(&prefix)?.trim();
        Some(value.trim_matches('"').trim_matches('\'').to_string())
    })
}
