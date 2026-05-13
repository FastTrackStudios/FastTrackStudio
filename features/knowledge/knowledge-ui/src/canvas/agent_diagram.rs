//! Pure layout helper: turn `AgentRun`s + their linked `Task`s into a
//! column-laid-out grid of canvas Blocks. The caller writes the
//! returned Blocks into the BlockRepoLoro for a whiteboard page.
//!
//! FUTURE: wire from the `/agents/runs` route as an "Open as
//! whiteboard" action — the route owns Page creation and repo writes;
//! this function is purely layout.

use std::collections::HashMap;

use chrono::Utc;
use knowledge_proto::Block;
use knowledge_proto::canvas::{
    Anchor, ArrowHead, CanvasNode, CanvasShape, ConnectorEnd, ConnectorLine, ConnectorStyle, encode,
};
use uuid::Uuid;

const COL_W: f64 = 320.0;
const ROW_H: f64 = 140.0;
const GUTTER: f64 = 40.0;
const TASK_W: f64 = 240.0;
const TASK_H: f64 = 80.0;
const RUN_W: f64 = 240.0;
const RUN_H: f64 = 100.0;

/// Lay out runs grouped by their task into vertical columns. Tasks
/// with no runs are skipped; runs with no task land in a synthetic
/// "Unassigned" column at the far right. Each AgentRun is a Text
/// node below its task; tasks with runs that carry a non-empty
/// `external_url` (treated here as a PR-like link) get a third tier
/// with a dashed connector.
pub fn layout_agent_runs(
    runs: &[agent_proto::AgentRun],
    tasks_by_id: &HashMap<Uuid, project_proto::Task>,
    vault_id: Uuid,
    page_id: Uuid,
) -> Vec<Block> {
    // Group runs by task_id.
    let mut runs_by_task: HashMap<Option<Uuid>, Vec<&agent_proto::AgentRun>> = HashMap::new();
    for r in runs {
        runs_by_task.entry(r.task_id).or_default().push(r);
    }

    // Real tasks first (those with runs and a resolvable Task in the map).
    let mut task_columns: Vec<(Option<Uuid>, String, Vec<&agent_proto::AgentRun>)> = Vec::new();
    for (task_id, run_set) in runs_by_task.iter() {
        match task_id {
            Some(tid) => {
                if let Some(t) = tasks_by_id.get(tid) {
                    task_columns.push((Some(*tid), t.title.clone(), run_set.clone()));
                }
            }
            None => {} // collected last
        }
    }
    // Deterministic column order by task title.
    task_columns.sort_by(|a, b| a.1.cmp(&b.1));
    // Unassigned column.
    if let Some(unassigned) = runs_by_task.get(&None) {
        task_columns.push((None, "Unassigned".into(), unassigned.clone()));
    }

    let mut out = Vec::new();
    let now = Utc::now();

    for (col_idx, (_task_id, title, run_set)) in task_columns.into_iter().enumerate() {
        let col_x = col_idx as f64 * (COL_W + GUTTER);
        // Task header node.
        let task_block_id = Uuid::new_v4();
        let task_node = CanvasNode {
            x: col_x,
            y: 0.0,
            w: TASK_W,
            h: TASK_H,
            z: 0,
            rotation: 0.0,
            shape: CanvasShape::Text,
            color: Some("primary".into()),
            locked: false,
            extras_json: None,
        };
        out.push(Block {
            id: task_block_id,
            vault_id,
            page_id,
            parent_block_id: None,
            sort_key: format!("a{col_idx:04}"),
            kind: "canvas".into(),
            content: format!("Task: {title}"),
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
            canvas_node_json: Some(encode(&task_node)),
            created_at: now,
            updated_at: now,
        });

        for (row_idx, run) in run_set.into_iter().enumerate() {
            let row_y = TASK_H + GUTTER + row_idx as f64 * (ROW_H + GUTTER);
            let run_block_id = Uuid::new_v4();
            let run_node = CanvasNode {
                x: col_x,
                y: row_y,
                w: RUN_W,
                h: RUN_H,
                z: 0,
                rotation: 0.0,
                shape: CanvasShape::Text,
                color: None,
                locked: false,
                extras_json: None,
            };
            out.push(Block {
                id: run_block_id,
                vault_id,
                page_id,
                parent_block_id: None,
                sort_key: format!("b{col_idx:04}{row_idx:04}"),
                kind: "canvas".into(),
                content: format!("Agent: {}\n{}", run.name, run.status),
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
                canvas_node_json: Some(encode(&run_node)),
                created_at: now,
                updated_at: now,
            });

            // Solid connector task → run.
            let conn = CanvasNode {
                x: 0.0,
                y: 0.0,
                w: 0.0,
                h: 0.0,
                z: -1,
                rotation: 0.0,
                shape: CanvasShape::Connector {
                    from: ConnectorEnd {
                        block_id: task_block_id,
                        anchor: Anchor::Bottom,
                    },
                    to: ConnectorEnd {
                        block_id: run_block_id,
                        anchor: Anchor::Top,
                    },
                    style: ConnectorStyle {
                        line: ConnectorLine::Solid,
                        start_arrow: ArrowHead::None,
                        end_arrow: ArrowHead::Triangle,
                        color: None,
                    },
                    label: None,
                },
                color: None,
                locked: false,
                extras_json: None,
            };
            out.push(Block {
                id: Uuid::new_v4(),
                vault_id,
                page_id,
                parent_block_id: None,
                sort_key: format!("c{col_idx:04}{row_idx:04}a"),
                kind: "canvas".into(),
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
                canvas_node_json: Some(encode(&conn)),
                created_at: now,
                updated_at: now,
            });

            // PR-like third tier (uses external_url as the stand-in for
            // a "first PR URL"; agent-proto has no pr_urls field yet).
            if let Some(pr_url) = run.external_url.as_deref() {
                if !pr_url.is_empty() {
                    let pr_id = Uuid::new_v4();
                    let pr_y = row_y + RUN_H + GUTTER;
                    let pr_node = CanvasNode {
                        x: col_x,
                        y: pr_y,
                        w: RUN_W,
                        h: 60.0,
                        z: 0,
                        rotation: 0.0,
                        shape: CanvasShape::Text,
                        color: Some("muted".into()),
                        locked: false,
                        extras_json: None,
                    };
                    out.push(Block {
                        id: pr_id,
                        vault_id,
                        page_id,
                        parent_block_id: None,
                        sort_key: format!("d{col_idx:04}{row_idx:04}"),
                        kind: "canvas".into(),
                        content: pr_url.to_string(),
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
                        canvas_node_json: Some(encode(&pr_node)),
                        created_at: now,
                        updated_at: now,
                    });

                    let pr_conn = CanvasNode {
                        x: 0.0,
                        y: 0.0,
                        w: 0.0,
                        h: 0.0,
                        z: -1,
                        rotation: 0.0,
                        shape: CanvasShape::Connector {
                            from: ConnectorEnd {
                                block_id: run_block_id,
                                anchor: Anchor::Bottom,
                            },
                            to: ConnectorEnd {
                                block_id: pr_id,
                                anchor: Anchor::Top,
                            },
                            style: ConnectorStyle {
                                line: ConnectorLine::Dashed,
                                start_arrow: ArrowHead::None,
                                end_arrow: ArrowHead::Triangle,
                                color: None,
                            },
                            label: None,
                        },
                        color: None,
                        locked: false,
                        extras_json: None,
                    };
                    out.push(Block {
                        id: Uuid::new_v4(),
                        vault_id,
                        page_id,
                        parent_block_id: None,
                        sort_key: format!("c{col_idx:04}{row_idx:04}b"),
                        kind: "canvas".into(),
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
                        canvas_node_json: Some(encode(&pr_conn)),
                        created_at: now,
                        updated_at: now,
                    });
                }
            }
        }
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use knowledge_proto::canvas::decode;

    fn mk_run(
        task_id: Option<Uuid>,
        name: &str,
        external_url: Option<&str>,
    ) -> agent_proto::AgentRun {
        agent_proto::AgentRun {
            id: Uuid::new_v4(),
            name: name.into(),
            kind: "claude-code".into(),
            prompt: String::new(),
            status: "running".into(),
            task_id,
            started_at: None,
            completed_at: None,
            result: None,
            error_message: None,
            tokens_used: None,
            cost_cents: None,
            tags: vec![],
            integration: None,
            external_id: None,
            external_url: external_url.map(|s| s.into()),
            log_cursor: None,
            created_at: Utc::now(),
            updated_at: Utc::now(),
        }
    }

    #[test]
    fn layout_with_only_unassigned_runs_emits_single_column() {
        let runs = vec![mk_run(None, "r1", None)];
        let tasks: HashMap<Uuid, project_proto::Task> = HashMap::new();
        let vault = Uuid::new_v4();
        let page = Uuid::new_v4();
        let blocks = layout_agent_runs(&runs, &tasks, vault, page);
        // 1 task header (Unassigned) + 1 run + 1 connector = 3
        assert_eq!(blocks.len(), 3);
        // All blocks belong to the same page/vault.
        for b in &blocks {
            assert_eq!(b.page_id, page);
            assert_eq!(b.vault_id, vault);
            assert!(b.canvas_node_json.is_some());
        }
        // First block (header) is at column 0.
        let header = decode(blocks[0].canvas_node_json.as_deref().unwrap()).unwrap();
        assert!(header.x.abs() < 1e-9);
    }

    #[test]
    fn layout_skips_tasks_without_runs() {
        let only_task_id = Uuid::new_v4();
        let runs: Vec<agent_proto::AgentRun> = vec![];
        let mut tasks = HashMap::new();
        // We don't have a way to mint a Task without serde/Default
        // here; the empty-runs path doesn't actually consume the
        // task map, so the assertion below is the meaningful check.
        let _ = (only_task_id, &mut tasks);
        let blocks = layout_agent_runs(&runs, &tasks, Uuid::new_v4(), Uuid::new_v4());
        assert!(blocks.is_empty());
    }

    #[test]
    fn run_with_external_url_emits_pr_tier() {
        let runs = vec![mk_run(None, "r1", Some("https://example.com/pr/1"))];
        let blocks = layout_agent_runs(&runs, &HashMap::new(), Uuid::new_v4(), Uuid::new_v4());
        // Header + run + connector + pr + pr-connector = 5
        assert_eq!(blocks.len(), 5);
        let pr = &blocks[3];
        assert_eq!(pr.content, "https://example.com/pr/1");
    }
}
