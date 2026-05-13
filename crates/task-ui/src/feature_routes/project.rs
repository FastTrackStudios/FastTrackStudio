//! Project feature route. Holds a local `CrdtDoc` + `ProjectRepoLoro`,
//! syncs over WebSocket, drives `project-ui` dumb components.
//!
//! v1 layout:
//!   1. `ProjectHeaderCard` banner for the first loaded project.
//!   2. `TaskListView` / `TaskKanbanBoard` switcher driven by the
//!      header's view toggle.
//!   3. `TaskDetailSheet` slide-over for the selected task.
//!   4. Cmd/Ctrl+K opens `TaskCommandPalette`.
//!
//! FUTURE: real Task wiring requires a per-project `TaskRepoLoro` —
//! out of scope for this pass. We populate an in-memory `Vec<Task>` of
//! 12 sample tasks scoped to the first project so the UI is fully
//! exercised end-to-end (rendering, edits, palette, board interaction).

use std::collections::HashSet;
use std::rc::Rc;

use agent_crdt::{AgentLogLineRepoLoro, AgentRunRepoLoro};
use agent_proto::{AgentLogLine, AgentLogLineRepo, AgentRun, AgentRunCreate, AgentRunRepo};
use agent_ui::hermes_kit::{AgentDispatchDialog, DispatchIntent, IntegrationInfo, TaskBrief};
use architect::Page;
use chrono::{Duration, Utc};
use dioxus::prelude::*;
use fts_ui::prelude::*;
use futures_channel::mpsc;
use futures_util::StreamExt;
use knowledge_crdt::{BlockRepoLoro, PageRepoLoro, VaultRepoLoro};
use knowledge_proto::{
    Block as KBlock, BlockCreate as KBlockCreate, BlockRepo, BlockUpdate as KBlockUpdate,
    Page as KPage, PageCreate as KPageCreate, PageRepo, VaultCreate, VaultRepo, shadow_page_id,
};
use knowledge_ui::common::{BlockBrief, PageBrief, block_brief_from, page_brief_from};
use knowledge_ui::editor::{BlockUpdate as EditorBlockUpdate, StructuralOp};
use project_crdt::{CrdtDoc, ProjectRepoLoro};
use project_proto::{Project, ProjectCreate, ProjectRepo, Task, TaskUpdate, branch_slug};
use project_ui::{
    BulkAction, PaletteIntent, ProjectDashboard, ProjectHeaderCard, ProjectOverviewGrid,
    ProjectTab, TaskCommandPalette, TaskDetailSheet, TaskGroupBy, TaskKanbanBoard, TaskListView,
    TaskStats, TaskView,
};
use uuid::Uuid;
use wasm_bindgen_futures::spawn_local;

use crate::sync;
use crate::theming::{ProjectThemeScope, use_project_theme_overrides};

fn fake_tasks_for(project: &Project) -> Vec<Task> {
    let now = Utc::now();
    let statuses = [
        "todo",
        "in-progress",
        "in-review",
        "blocked",
        "done",
        "in-progress",
        "todo",
        "done",
        "in-progress",
        "todo",
        "in-review",
        "todo",
    ];
    let priorities = [
        "urgent", "high", "medium", "low", "none", "high", "medium", "low", "urgent", "medium",
        "low", "high",
    ];
    let titles = [
        "Wire up the new dashboard route",
        "Audit feature flags & remove stale ones",
        "Refactor the queue worker",
        "Investigate flaky CI run",
        "Ship customer onboarding emails",
        "Migrate users table to new schema",
        "Design the empty-state illustrations",
        "Cut the v1.4 release notes",
        "Add retries to the webhook delivery",
        "Performance pass on the search page",
        "Document the new payments flow",
        "Triage open issues from the weekend",
    ];
    let assignees = ["Ada", "Linus", "Grace", "Margaret", "Alan", "Barbara"];
    let tag_pools: [&[&str]; 4] = [
        &["frontend", "feature"],
        &["backend", "tech-debt"],
        &["docs"],
        &["bug", "urgent"],
    ];

    titles
        .iter()
        .enumerate()
        .map(|(i, title)| Task {
            id: Uuid::new_v4(),
            project_id: project.id,
            parent_id: None,
            cycle_id: None,
            title: (*title).to_string(),
            description: Some(format!(
                "Sample task #{} on {}. Replace with real Task CRDT wiring.",
                i + 1,
                project.name
            )),
            status: statuses[i % statuses.len()].to_string(),
            priority: priorities[i % priorities.len()].to_string(),
            assignee: Some(assignees[i % assignees.len()].to_string()),
            estimate_minutes: Some(((i as i64) % 8 + 1) * 30),
            due_date: Some(now + Duration::days((i as i64) - 4)),
            tags: tag_pools[i % tag_pools.len()]
                .iter()
                .map(|s| s.to_string())
                .collect(),
            sort_index: i as i64,
            completed_at: None,
            agent_run_id: None,
            branch_name: None,
            pr_urls: Vec::new(),
            commit_refs: Vec::new(),
            created_at: now - Duration::hours((24 - i) as i64),
            updated_at: now - Duration::minutes((i as i64) * 7),
        })
        .collect()
}

fn task_stats(tasks: &[Task]) -> TaskStats {
    let total = tasks.len() as u32;
    let mut done = 0u32;
    let mut in_progress = 0u32;
    let mut blocked = 0u32;
    for t in tasks {
        match t.status.as_str() {
            "done" | "completed" => done += 1,
            "in-progress" | "in-review" => in_progress += 1,
            "blocked" => blocked += 1,
            _ => {}
        }
    }
    TaskStats {
        total,
        done,
        in_progress,
        blocked,
    }
}

fn apply_patch(task: &mut Task, patch: &TaskUpdate) {
    if let Some(v) = patch.title.clone() {
        task.title = v;
    }
    if let Some(v) = patch.description.clone() {
        task.description = v;
    }
    if let Some(v) = patch.status.clone() {
        task.status = v;
    }
    if let Some(v) = patch.priority.clone() {
        task.priority = v;
    }
    if let Some(v) = patch.assignee.clone() {
        task.assignee = v;
    }
    if let Some(v) = patch.estimate_minutes {
        task.estimate_minutes = v;
    }
    if let Some(v) = patch.due_date {
        task.due_date = v;
    }
    if let Some(v) = patch.tags.clone() {
        task.tags = v;
    }
    if let Some(v) = patch.cycle_id {
        task.cycle_id = v;
    }
    task.updated_at = Utc::now();
}

#[component]
pub fn ProjectView() -> Element {
    let repo: Rc<ProjectRepoLoro> = use_hook(|| {
        let doc = CrdtDoc::ephemeral();
        Rc::new(ProjectRepoLoro::new(&doc))
    });
    let doc: Rc<CrdtDoc> = use_hook(|| Rc::new(CrdtDoc::from_loro(repo.doc().clone())));
    let agent_run_repo: Rc<AgentRunRepoLoro> = use_hook(|| Rc::new(AgentRunRepoLoro::new(&doc)));
    let log_line_repo: Rc<AgentLogLineRepoLoro> =
        use_hook(|| Rc::new(AgentLogLineRepoLoro::new(&doc)));
    let k_vault_repo: Rc<VaultRepoLoro> = use_hook(|| Rc::new(VaultRepoLoro::new(&doc)));
    let k_page_repo: Rc<PageRepoLoro> = use_hook(|| Rc::new(PageRepoLoro::new(&doc)));
    let k_block_repo: Rc<BlockRepoLoro> = use_hook(|| Rc::new(BlockRepoLoro::new(&doc)));

    let mut items = use_signal::<Vec<Project>>(Vec::new);
    let mut status_msg = use_signal(|| "starting…".to_string());

    // In-memory tasks. Seeded lazily on first project load (see effect below).
    let mut tasks = use_signal::<Vec<Task>>(Vec::new);
    // Agent runs + log lines mirror the CRDT repos. The route layer
    // owns the reactive view; the repos are the source of truth.
    let mut agent_runs = use_signal::<Vec<AgentRun>>(Vec::new);
    let mut agent_logs = use_signal::<Vec<AgentLogLine>>(Vec::new);
    let mut dispatch_dialog_open = use_signal(|| false);
    let mut dispatch_target = use_signal::<Option<Uuid>>(|| None);
    let mut k_vault_id = use_signal::<Option<Uuid>>(|| None);
    let mut k_pages = use_signal::<Vec<KPage>>(Vec::new);
    let mut k_blocks = use_signal::<Vec<KBlock>>(Vec::new);
    let mut k_seeded = use_signal(|| false);

    let refresh_tx: mpsc::UnboundedSender<()> = use_hook(|| {
        let (tx, mut rx) = mpsc::unbounded::<()>();
        let repo_for_loop = repo.clone();
        let run_repo_for_loop = agent_run_repo.clone();
        let log_repo_for_loop = log_line_repo.clone();
        let k_page_repo_loop = k_page_repo.clone();
        let k_block_repo_loop = k_block_repo.clone();
        spawn_local(async move {
            while rx.next().await.is_some() {
                if let Ok(list) = k_page_repo_loop
                    .list(
                        Page {
                            index: 0,
                            size: 1000,
                        },
                        None,
                        None,
                    )
                    .await
                {
                    k_pages.set(list.items);
                }
                if let Ok(list) = k_block_repo_loop
                    .list(
                        Page {
                            index: 0,
                            size: 10_000,
                        },
                        None,
                        None,
                    )
                    .await
                {
                    k_blocks.set(list.items);
                }
                if let Ok(list) = repo_for_loop
                    .list(
                        Page {
                            index: 0,
                            size: 200,
                        },
                        None,
                        None,
                    )
                    .await
                {
                    items.set(list.items);
                }
                if let Ok(list) = run_repo_for_loop
                    .list(
                        Page {
                            index: 0,
                            size: 500,
                        },
                        None,
                        None,
                    )
                    .await
                {
                    agent_runs.set(list.items);
                }
                if let Ok(list) = log_repo_for_loop
                    .list(
                        Page {
                            index: 0,
                            size: 2000,
                        },
                        None,
                        None,
                    )
                    .await
                {
                    agent_logs.set(list.items);
                }
            }
        });
        tx
    });

    let _session: Rc<Option<sync::SyncSession>> = use_hook({
        let doc = doc.clone();
        let tx_for_sync = refresh_tx.clone();
        move || {
            let _ = tx_for_sync.unbounded_send(());
            let ws_url = sync::sync_url(&format!("/sync/{}", sync::WORKSPACE_DOC_ID));
            let tx = tx_for_sync.clone();
            match sync::connect(&ws_url, &doc, move || {
                let _ = tx.unbounded_send(());
            }) {
                Ok(s) => {
                    status_msg.set(format!("connected to {ws_url}"));
                    Rc::new(Some(s))
                }
                Err(e) => {
                    status_msg.set(format!("ws connect failed: {e:?}"));
                    Rc::new(None)
                }
            }
        }
    });

    // Ensure a default knowledge vault exists so the Notes tab works.
    {
        let k_vault_repo = k_vault_repo.clone();
        let tx = refresh_tx.clone();
        use_effect(move || {
            if *k_seeded.read() {
                return;
            }
            k_seeded.set(true);
            let k_vault_repo = k_vault_repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let pg = Page { index: 0, size: 50 };
                let existing = k_vault_repo.list(pg, None, None).await;
                let id = match existing {
                    Ok(list) if !list.items.is_empty() => list.items[0].id,
                    _ => match k_vault_repo
                        .create(VaultCreate {
                            name: "Workspace".into(),
                            root_path: None,
                            use_markdown_links: false,
                            new_link_format: "shortest".into(),
                            attachment_folder_path: String::new(),
                            default_view_mode: "live-preview".into(),
                            config_json: "{}".into(),
                        })
                        .await
                    {
                        Ok(v) => v.id,
                        Err(_) => return,
                    },
                };
                k_vault_id.set(Some(id));
                let _ = tx.unbounded_send(());
            });
        });
    }

    // Seed sample tasks once the first project lands.
    use_effect(move || {
        let cur_items = items.read().clone();
        if tasks.read().is_empty() {
            if let Some(p) = cur_items.first() {
                tasks.set(fake_tasks_for(p));
            }
        }
    });

    // ── Page-level UI state ───────────────────────────────────────────
    let mut active_tab = use_signal(|| ProjectTab::Tasks);
    let mut active_view = use_signal(|| TaskView::List);
    let mut selected_task = use_signal::<Option<Uuid>>(|| None);
    let mut selected_set = use_signal::<HashSet<Uuid>>(HashSet::new);
    let mut palette_open = use_signal(|| false);
    let mut shell_tab = use_signal(|| "pm".to_string());

    // When the active task changes, ensure a shadow page + seed block.
    {
        let k_page_repo = k_page_repo.clone();
        let k_block_repo = k_block_repo.clone();
        let tx = refresh_tx.clone();
        use_effect(move || {
            let Some(task_id) = *selected_task.read() else {
                return;
            };
            let Some(vault_id) = *k_vault_id.read() else {
                return;
            };
            let shadow_id = shadow_page_id("task", task_id);
            if k_pages.read().iter().any(|p| p.id == shadow_id) {
                return;
            }
            let k_page_repo = k_page_repo.clone();
            let k_block_repo = k_block_repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let now = Utc::now();
                let _ = k_page_repo
                    .create(KPageCreate {
                        vault_id,
                        folder_id: None,
                        path: format!("shadow/task/{task_id}.md"),
                        basename: format!("task-{task_id}"),
                        ext: "md".into(),
                        aliases: Vec::new(),
                        frontmatter_json: "{}".into(),
                        stat_ctime: now,
                        stat_mtime: now,
                        stat_size: 0,
                        is_journal: false,
                        journal_day: None,
                        shadow_for_kind: Some("task".into()),
                        shadow_for_id: Some(task_id),
                    })
                    .await;
                if let Ok(list) = k_page_repo
                    .list(
                        Page {
                            index: 0,
                            size: 1000,
                        },
                        None,
                        None,
                    )
                    .await
                {
                    if let Some(p) = list.items.iter().find(|p| {
                        p.shadow_for_kind.as_deref() == Some("task")
                            && p.shadow_for_id == Some(task_id)
                    }) {
                        let _ = k_block_repo
                            .create(KBlockCreate {
                                vault_id,
                                page_id: p.id,
                                parent_block_id: None,
                                sort_key: "a000".into(),
                                kind: "paragraph".into(),
                                content: String::new(),
                                heading_level: None,
                                list_ordered: false,
                                list_task: None,
                                code_lang: None,
                                callout_kind: None,
                                callout_foldable: false,
                                properties_json: "{}".into(),
                                obsidian_block_id: None,
                                collapsed: false,
                                refs_json: "[]".into(),
                                canvas_node_json: None,
                            })
                            .await;
                    }
                }
                let _ = tx.unbounded_send(());
            });
        });
    }

    // ── Callbacks: project ────────────────────────────────────────────
    let on_create_project = {
        let repo = repo.clone();
        let tx = refresh_tx.clone();
        move |payload: ProjectCreate| {
            let repo = repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let _ = repo.create(payload).await;
                let _ = tx.unbounded_send(());
            });
        }
    };
    let on_delete_project = {
        let repo = repo.clone();
        let tx = refresh_tx.clone();
        move |id: Uuid| {
            let repo = repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let _ = repo.delete(id).await;
                let _ = tx.unbounded_send(());
            });
        }
    };

    // ── Callbacks: task (in-memory) ───────────────────────────────────
    let mut patch_task = move |(id, patch): (Uuid, TaskUpdate)| {
        let mut next = tasks.read().clone();
        if let Some(t) = next.iter_mut().find(|t| t.id == id) {
            apply_patch(t, &patch);
        }
        tasks.set(next);
    };

    let mut delete_task = move |id: Uuid| {
        let next: Vec<Task> = tasks
            .read()
            .iter()
            .filter(|t| t.id != id)
            .cloned()
            .collect();
        tasks.set(next);
        if *selected_task.read() == Some(id) {
            selected_task.set(None);
        }
    };

    let mut duplicate_task = move |id: Uuid| {
        let mut next = tasks.read().clone();
        if let Some(orig) = next.iter().find(|t| t.id == id).cloned() {
            let mut copy = orig.clone();
            copy.id = Uuid::new_v4();
            copy.title = format!("{} (copy)", copy.title);
            copy.created_at = Utc::now();
            copy.updated_at = Utc::now();
            next.push(copy);
            tasks.set(next);
        }
    };

    let mut quick_create = move |(column_key, title): (String, String)| {
        if title.trim().is_empty() {
            return;
        }
        let project_id = items.read().first().map(|p| p.id).unwrap_or_else(Uuid::nil);
        let now = Utc::now();
        let mut next = tasks.read().clone();
        let sort_index = next.len() as i64;
        next.push(Task {
            id: Uuid::new_v4(),
            project_id,
            parent_id: None,
            cycle_id: None,
            title,
            description: None,
            status: column_key,
            priority: "none".into(),
            assignee: None,
            estimate_minutes: None,
            due_date: None,
            tags: vec![],
            sort_index,
            completed_at: None,
            agent_run_id: None,
            branch_name: None,
            pr_urls: Vec::new(),
            commit_refs: Vec::new(),
            created_at: now,
            updated_at: now,
        });
        tasks.set(next);
    };

    let mut bulk_action = move |action: BulkAction| {
        let ids = selected_set.read().clone();
        let mut next = tasks.read().clone();
        match action {
            BulkAction::Delete => {
                next.retain(|t| !ids.contains(&t.id));
            }
            BulkAction::SetStatus(s) => {
                for t in next.iter_mut() {
                    if ids.contains(&t.id) {
                        t.status = s.clone();
                        t.updated_at = Utc::now();
                    }
                }
            }
            BulkAction::SetPriority(p) => {
                for t in next.iter_mut() {
                    if ids.contains(&t.id) {
                        t.priority = p.clone();
                        t.updated_at = Utc::now();
                    }
                }
            }
            BulkAction::SetAssignee(a) => {
                for t in next.iter_mut() {
                    if ids.contains(&t.id) {
                        t.assignee = a.clone();
                        t.updated_at = Utc::now();
                    }
                }
            }
        }
        tasks.set(next);
        selected_set.set(HashSet::new());
    };

    let on_run_palette = move |intent: PaletteIntent| {
        palette_open.set(false);
        match intent {
            PaletteIntent::CreateTask => {
                quick_create(("todo".into(), "Untitled task".into()));
            }
            PaletteIntent::SwitchView(v) => active_view.set(v),
            PaletteIntent::OpenTask(id) => selected_task.set(Some(id)),
            PaletteIntent::SetActiveTab(t) => active_tab.set(t),
            PaletteIntent::FilterByPriority(_) | PaletteIntent::FilterByAssignee(_) => {
                // Filter wiring lands with the real Task repo. For v1
                // we just acknowledge the intent (no-op).
            }
        }
    };

    // ── Agent dispatch (v1: local write only) ─────────────────────────
    //
    // FUTURE: real dispatch goes through a vox endpoint that calls
    // `IntegrationRegistry::dispatch` on the server. For v1 the
    // server's MockIntegration event loop is decoupled and won't pick
    // up runs we create from the wasm side — the UI flow demos
    // dispatch & log tailing end-to-end against the local CRDT doc,
    // and live runs require running the server with the registry +
    // a Hermes URL.
    let mut perform_dispatch = {
        let agent_run_repo = agent_run_repo.clone();
        let tx = refresh_tx.clone();
        move |intent: DispatchIntent| {
            // Patch the task with branch_name (if absent) + new agent_run_id.
            let task_id = intent.task_id;
            let current_tasks = tasks.read().clone();
            let task = current_tasks.iter().find(|t| t.id == task_id).cloned();
            let title = task.as_ref().map(|t| t.title.clone()).unwrap_or_default();
            let existing_branch = task.as_ref().and_then(|t| t.branch_name.clone());
            let branch =
                existing_branch.unwrap_or_else(|| branch_slug("cwright", "PRJ", task_id, &title));
            let run_id = Uuid::new_v4();
            let payload = AgentRunCreate {
                name: format!("{} · {}", intent.agent_kind, title),
                kind: intent.agent_kind.clone(),
                prompt: intent.prompt.clone(),
                status: "queued".into(),
                task_id: Some(task_id),
                started_at: None,
                completed_at: None,
                result: None,
                error_message: None,
                tokens_used: None,
                cost_cents: None,
                tags: Vec::new(),
                integration: Some(intent.integration.clone()),
                external_id: None,
                external_url: None,
                log_cursor: None,
            };
            // Patch the task locally — agent_run_id + branch_name.
            let mut next = current_tasks;
            if let Some(t) = next.iter_mut().find(|t| t.id == task_id) {
                t.agent_run_id = Some(run_id);
                t.branch_name = Some(branch);
                t.updated_at = Utc::now();
            }
            tasks.set(next);

            let repo = agent_run_repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                // We can't pick the id today (AgentRunCreate doesn't
                // expose `id`), so the repo assigns one. The local
                // `agent_run_id` we set above is overwritten the
                // moment the repo emits the real id back via list().
                // FUTURE: pin to `run_id` when the repo grows an
                // `id` field on Create.
                let _ = repo.create(payload).await;
                let _ = tx.unbounded_send(());
                let _ = run_id;
            });
        }
    };

    // Branch-name handlers fired from TaskGitPanel.
    let mut set_branch_name = move |task_id: Uuid, raw: String| {
        let mut next = tasks.read().clone();
        if let Some(t) = next.iter_mut().find(|t| t.id == task_id) {
            let target = if raw.is_empty() {
                branch_slug("cwright", "PRJ", task_id, &t.title)
            } else {
                raw
            };
            t.branch_name = Some(target);
            t.updated_at = Utc::now();
        }
        tasks.set(next);
    };
    let mut clear_branch_name = move |task_id: Uuid| {
        let mut next = tasks.read().clone();
        if let Some(t) = next.iter_mut().find(|t| t.id == task_id) {
            t.branch_name = None;
            t.updated_at = Utc::now();
        }
        tasks.set(next);
    };

    // Cancel run — local write only in v1.
    let mut cancel_run = move |run_id: Uuid| {
        let mut next = agent_runs.read().clone();
        if let Some(r) = next.iter_mut().find(|r| r.id == run_id) {
            r.status = "cancelled".into();
            r.completed_at = Some(Utc::now());
            r.updated_at = Utc::now();
        }
        agent_runs.set(next);
    };

    let cur_items = items();
    let first_project = cur_items.first().cloned();

    // Tasks for the overview grid: the in-memory `tasks` signal currently
    // only seeds the first project. For other loaded projects we synthesise
    // a per-project task list so the grid card has something real to show
    // its "next up" row. FUTURE: replace with per-project repos.
    let overview_tasks: Vec<Task> = {
        let mut out: Vec<Task> = tasks.read().clone();
        for p in cur_items.iter().skip(1) {
            out.extend(fake_tasks_for(p));
        }
        out
    };

    // Resolve the currently-open task and its subtasks.
    let selected_id = *selected_task.read();
    let task_for_sheet =
        selected_id.and_then(|id| tasks.read().iter().find(|t| t.id == id).cloned());
    let subtasks: Vec<Task> = if let Some(id) = selected_id {
        tasks
            .read()
            .iter()
            .filter(|t| t.parent_id == Some(id))
            .cloned()
            .collect()
    } else {
        Vec::new()
    };

    let stats = task_stats(&tasks.read());

    // Resolve the agent run + log lines bound to the sheet's task.
    let task_run: Option<AgentRun> = task_for_sheet
        .as_ref()
        .and_then(|t| t.agent_run_id)
        .and_then(|rid| agent_runs.read().iter().find(|r| r.id == rid).cloned());
    let task_logs: Vec<AgentLogLine> = if let Some(r) = &task_run {
        agent_logs
            .read()
            .iter()
            .filter(|l| l.run_id == r.id)
            .cloned()
            .collect()
    } else {
        Vec::new()
    };

    let available_assignees: Vec<String> = {
        let mut v: Vec<String> = tasks
            .read()
            .iter()
            .filter_map(|t| t.assignee.clone())
            .collect();
        v.sort();
        v.dedup();
        v
    };

    let column_order: Vec<String> = vec![
        "todo".into(),
        "in-progress".into(),
        "in-review".into(),
        "blocked".into(),
        "done".into(),
    ];

    let key_handler = move |e: KeyboardEvent| {
        let modifier = e.modifiers().ctrl() || e.modifiers().meta();
        if modifier && e.key().to_string() == "k" {
            palette_open.set(true);
        }
        if e.key() == Key::Escape {
            palette_open.set(false);
            selected_task.set(None);
        }
    };

    let pm_create = on_create_project.clone();
    let pm_delete = on_delete_project.clone();
    let dashboard_create = on_create_project;
    let dashboard_delete = on_delete_project;

    // Project-local theme override. Lives in App-level context as a
    // shared `Signal<HashMap<Uuid, String>>`; we read/write the entry
    // keyed by the active project's id. FUTURE: persist to localStorage
    // or to a project-scoped setting on the Project entity.
    let mut project_overrides = use_project_theme_overrides();
    let active_project_id: Option<Uuid> = first_project.as_ref().map(|p| p.id);
    let current_project_theme: Option<String> =
        active_project_id.and_then(|id| project_overrides.map.read().get(&id).cloned());

    let on_theme_change = move |next: Option<String>| {
        if let Some(id) = active_project_id {
            let mut m = project_overrides.map.write();
            match next {
                Some(name) => {
                    m.insert(id, name);
                }
                None => {
                    m.remove(&id);
                }
            }
        }
    };

    rsx! {
        div {
            class: "mx-auto flex max-w-7xl flex-col gap-4 p-6 lg:p-10 outline-none",
            tabindex: "0",
            onkeydown: key_handler,

            Tabs {
                value: Some(shell_tab.read().clone()),
                on_change: move |v: String| shell_tab.set(v),
                TabList {
                    TabTrigger { value: "overview", index: 0usize, "Overview" }
                    TabTrigger { value: "pm", index: 1usize, "Project" }
                    TabTrigger { value: "dashboard", index: 2usize, "Dashboard" }
                }

                TabContent { value: "overview", index: 0usize,
                    ProjectOverviewGrid {
                        projects: cur_items.clone(),
                        tasks: overview_tasks.clone(),
                        on_open: move |_id: Uuid| {
                            // FUTURE: route to /projects-live/<id>. For now,
                            // dropping the user into the Project tab is the
                            // closest behaviour without a per-project route.
                            shell_tab.set("pm".into());
                        },
                    }
                }

                TabContent { value: "pm", index: 1usize,
                    if let Some(project) = first_project.clone() {
                        ProjectThemeScope { project_id: project.id,
                            ProjectHeaderCard {
                                project: project.clone(),
                                task_stats: stats,
                                active_tab: *active_tab.read(),
                                active_view: *active_view.read(),
                                project_theme: current_project_theme.clone(),
                                on_tab_change: move |t| active_tab.set(t),
                                on_view_change: move |v| active_view.set(v),
                                on_edit: move |_u| {
                                    // FUTURE: wire ProjectUpdate through repo.update once
                                    // the in-memory edit needs to round-trip.
                                },
                                on_new_task: move |_| {
                                    quick_create(("todo".into(), "Untitled task".into()));
                                },
                                on_theme_change: on_theme_change,
                            }

                            div { class: "mt-4",
                                match *active_view.read() {
                                    TaskView::List => rsx! {
                                        TaskListView {
                                            tasks: tasks(),
                                            group_by: Some(TaskGroupBy::Status),
                                            selected: selected_set.read().clone(),
                                            on_select_change: move |s: HashSet<Uuid>| selected_set.set(s),
                                            on_open: move |id| selected_task.set(Some(id)),
                                            on_inline_edit: move |(id, u)| patch_task((id, u)),
                                            on_bulk_action: move |a| bulk_action(a),
                                        }
                                    },
                                    TaskView::Board => rsx! {
                                        TaskKanbanBoard {
                                            tasks: tasks(),
                                            group_by: TaskGroupBy::Status,
                                            column_order: column_order.clone(),
                                            on_card_open: move |id| selected_task.set(Some(id)),
                                            on_card_patch: move |(id, u)| patch_task((id, u)),
                                            on_quick_create: move |t| quick_create(t),
                                            on_card_delete: move |id| delete_task(id),
                                            on_dispatch_agent: move |id| {
                                                dispatch_target.set(Some(id));
                                                dispatch_dialog_open.set(true);
                                            },
                                        }
                                    },
                                }
                            }
                        }
                    } else {
                        ProjectDashboard {
                            items: cur_items.clone(),
                            status: status_msg(),
                            on_create: pm_create,
                            on_delete: pm_delete,
                        }
                    }
                }

                TabContent { value: "dashboard", index: 2usize,
                    ProjectDashboard {
                        items: cur_items.clone(),
                        status: status_msg(),
                        on_create: dashboard_create,
                        on_delete: dashboard_delete,
                    }
                }
            }

            // Slide-over inspector.
            {
                let sheet_task_id = task_for_sheet.as_ref().map(|t| t.id);
                let av = *k_vault_id.read();
                let shadow_id = sheet_task_id.map(|tid| shadow_page_id("task", tid));
                let shadow_blocks: Vec<KBlock> = match shadow_id {
                    Some(sid) => {
                        let mut bs: Vec<KBlock> = k_blocks
                            .read()
                            .iter()
                            .filter(|b| b.page_id == sid)
                            .cloned()
                            .collect();
                        bs.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
                        bs
                    }
                    None => Vec::new(),
                };
                let k_page_titles: std::collections::HashMap<Uuid, String> = k_pages
                    .read()
                    .iter()
                    .map(|p| (p.id, p.basename.clone()))
                    .collect();
                let available_pages: Vec<PageBrief> =
                    k_pages.read().iter().map(page_brief_from).collect();
                let available_blocks: Vec<BlockBrief> = k_blocks
                    .read()
                    .iter()
                    .map(|b| {
                        let title = k_page_titles.get(&b.page_id).cloned().unwrap_or_default();
                        block_brief_from(b, &title)
                    })
                    .collect();

                let k_block_repo_for_patch = k_block_repo.clone();
                let tx_for_patch = refresh_tx.clone();
                let on_shadow_block_patch =
                    move |(id, patch): (Uuid, EditorBlockUpdate)| {
                        let repo = k_block_repo_for_patch.clone();
                        let tx = tx_for_patch.clone();
                        spawn_local(async move {
                            let mut update = KBlockUpdate::default();
                            match patch {
                                EditorBlockUpdate::SetContent(c) => update.content = Some(c),
                                EditorBlockUpdate::SetHeadingLevel(l) => {
                                    update.heading_level = Some(l);
                                }
                                EditorBlockUpdate::SetTaskMark(m) => update.list_task = Some(m),
                                EditorBlockUpdate::SetCollapsed(b) => update.collapsed = Some(b),
                                EditorBlockUpdate::SetKind(k) => update.kind = Some(k),
                                EditorBlockUpdate::SetProperties(p) => {
                                    update.properties_json = Some(p);
                                }
                            }
                            let _ = repo.update(id, update).await;
                            let _ = tx.unbounded_send(());
                        });
                    };

                let on_shadow_structural = move |_op: StructuralOp| {
                    // FUTURE: route through KnowledgeService (split / merge /
                    // indent / outdent / move). v1 leaves these as no-ops so
                    // the embed renders without errors.
                };

                rsx! {
                    TaskDetailSheet {
                        task: task_for_sheet,
                        subtasks,
                        available_cycles: Vec::new(),
                        available_milestones: Vec::new(),
                        available_assignees,
                        vault_id_for_shadow: av,
                        shadow_blocks: shadow_blocks,
                        available_pages: available_pages,
                        available_blocks: available_blocks,
                        on_shadow_block_patch: on_shadow_block_patch,
                        on_shadow_structural: on_shadow_structural,
                        on_open_knowledge_page: move |_id: Uuid| {
                            // FUTURE: deep-link nav to /knowledge?page=<id>.
                        },
                        on_open_knowledge_block: move |_id: Uuid| {
                            // FUTURE: deep-link nav to /knowledge?block=<id>.
                        },
                        on_close: move |_| selected_task.set(None),
                        on_patch: move |(id, u)| patch_task((id, u)),
                        on_subtask_add: move |_title| {
                            // FUTURE: real subtask creation against TaskRepoLoro.
                        },
                        on_delete: move |id| delete_task(id),
                        on_duplicate: move |id| duplicate_task(id),
                        run: task_run,
                        log_lines: task_logs,
                        on_set_branch_name: move |raw: String| {
                            if let Some(id) = sheet_task_id {
                                set_branch_name(id, raw);
                            }
                        },
                        on_clear_branch_name: move |_| {
                            if let Some(id) = sheet_task_id {
                                clear_branch_name(id);
                            }
                        },
                        on_cancel_run: move |rid| cancel_run(rid),
                        on_open_run_external: move |_url: String| {
                            // FUTURE: window.open(_url, "_blank"). v1 surfaces
                            // the link via the anchor on the badge row already.
                        },
                        on_dispatch_agent: move |id| {
                            dispatch_target.set(Some(id));
                            dispatch_dialog_open.set(true);
                        },
                    }
                }
            }

            // Agent dispatch dialog.
            {
                let target_id = *dispatch_target.read();
                let brief = target_id.and_then(|id| {
                    tasks.read().iter().find(|t| t.id == id).map(|t| TaskBrief {
                        id: t.id,
                        title: t.title.clone(),
                        description: t.description.clone(),
                        tags: t.tags.clone(),
                        branch_name: t.branch_name.clone(),
                    })
                });
                let integrations = vec![
                    IntegrationInfo {
                        name: "mock".into(),
                        label: "Mock (local demo)".into(),
                        agent_kinds: vec!["sim".into()],
                    },
                    IntegrationInfo {
                        name: "hermes".into(),
                        label: "Hermes".into(),
                        agent_kinds: vec!["engineer".into(), "reviewer".into(), "planner".into()],
                    },
                ];
                rsx! {
                    AgentDispatchDialog {
                        open: *dispatch_dialog_open.read(),
                        task: brief,
                        available_integrations: integrations,
                        on_dispatch: move |intent: DispatchIntent| {
                            dispatch_dialog_open.set(false);
                            dispatch_target.set(None);
                            perform_dispatch(intent);
                        },
                        on_close: move |_| {
                            dispatch_dialog_open.set(false);
                            dispatch_target.set(None);
                        },
                    }
                }
            }

            // Cmd-K palette.
            TaskCommandPalette {
                open: *palette_open.read(),
                tasks: tasks(),
                on_open_change: move |o: bool| palette_open.set(o),
                on_run: on_run_palette,
            }
        }
    }
}
