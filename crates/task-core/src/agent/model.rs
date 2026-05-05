use facet::Facet;

use crate::task::Task;

/// A machine-readable execution plan for delegating a task in small chunks.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct AgentPlan {
    pub task_title: String,
    pub task_id: Option<String>,
    #[facet(default)]
    pub blocked_by: Vec<String>,
    #[facet(default)]
    pub nodes: Vec<AgentPlanNode>,
    #[facet(default)]
    pub edges: Vec<AgentPlanEdge>,
    #[facet(default)]
    pub runnable_node_ids: Vec<String>,
}

/// One actionable node in an execution plan.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct AgentPlanNode {
    pub id: String,
    pub title: String,
    pub kind: AgentPlanNodeKind,
    pub status: AgentPlanNodeStatus,
    pub prompt: String,
    #[facet(default)]
    pub depends_on: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
#[repr(u8)]
pub enum AgentPlanNodeKind {
    /// A dependency that must be resolved before this task can start.
    Dependency,
    /// Inspect the current state and gather context.
    #[default]
    Inspect,
    /// A checklist or subtasks item from the task body.
    Subtask,
    /// A planning/decomposition step for tasks without explicit subtasks.
    Decompose,
    /// The first implementation slice.
    Implement,
    /// Verify the result and review the diff.
    Verify,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
#[repr(u8)]
pub enum AgentPlanNodeStatus {
    /// Not yet runnable because something else must happen first.
    Blocked,
    /// Ready to run right away.
    #[default]
    Ready,
    /// Planned but not yet runnable.
    Pending,
    /// Completed.
    Done,
}

#[derive(Debug, Clone, PartialEq, Facet)]
pub struct AgentPlanEdge {
    pub from: String,
    pub to: String,
}

pub fn build_agent_plan(task: &Task) -> AgentPlan {
    let blocked_by = task
        .blocked_by
        .iter()
        .map(|dep| dep.uid.clone())
        .collect::<Vec<_>>();

    let mut nodes = Vec::new();
    let mut edges = Vec::new();
    let mut runnable_node_ids = Vec::new();

    let dependency_ids = build_dependency_nodes(task, &mut nodes, &mut edges);
    let inspect_id = "inspect".to_string();
    let inspect_status = if dependency_ids.is_empty() {
        AgentPlanNodeStatus::Ready
    } else {
        AgentPlanNodeStatus::Blocked
    };
    nodes.push(AgentPlanNode {
        id: inspect_id.clone(),
        title: "Inspect task context and constraints".into(),
        kind: AgentPlanNodeKind::Inspect,
        status: inspect_status,
        prompt: format!(
            "Review the task '{}' and gather implementation context before delegating work. Task body: {}",
            task.title,
            summarize_body(&task.body)
        ),
        depends_on: dependency_ids.clone(),
    });
    for dep in &dependency_ids {
        edges.push(AgentPlanEdge {
            from: dep.clone(),
            to: inspect_id.clone(),
        });
    }

    let mut previous = inspect_id.clone();
    let subtasks = task.subtasks();
    if subtasks.is_empty() {
        for node in build_fallback_nodes(task, &inspect_id) {
            edges.extend(node.depends_on.iter().cloned().map(|from| AgentPlanEdge {
                from,
                to: node.id.clone(),
            }));
            nodes.push(node);
        }
    } else {
        for (idx, subtask) in subtasks.iter().enumerate() {
            let id = format!("subtask-{}", idx + 1);
            let depends_on = vec![previous.clone()];
            nodes.push(AgentPlanNode {
                id: id.clone(),
                title: subtask.title.clone(),
                kind: AgentPlanNodeKind::Subtask,
                status: AgentPlanNodeStatus::Pending,
                prompt: format!(
                    "Complete checklist item {} for task '{}': {}",
                    idx + 1,
                    task.title,
                    subtask.title
                ),
                depends_on: depends_on.clone(),
            });
            edges.push(AgentPlanEdge {
                from: previous.clone(),
                to: id.clone(),
            });
            previous = id;
        }

        let verify_id = "verify".to_string();
        nodes.push(AgentPlanNode {
            id: verify_id.clone(),
            title: "Verify and review the result".into(),
            kind: AgentPlanNodeKind::Verify,
            status: AgentPlanNodeStatus::Pending,
            prompt: format!(
                "Run the focused checks for '{}' and verify the delivered work matches the task acceptance criteria.",
                task.title
            ),
            depends_on: vec![previous.clone()],
        });
        edges.push(AgentPlanEdge {
            from: previous,
            to: verify_id,
        });
    }

    if dependency_ids.is_empty() {
        runnable_node_ids.push(inspect_id.clone());
    }

    AgentPlan {
        task_title: task.title.clone(),
        task_id: Some(task.id_ref()),
        blocked_by,
        nodes,
        edges,
        runnable_node_ids,
    }
}

fn build_dependency_nodes(
    task: &Task,
    nodes: &mut Vec<AgentPlanNode>,
    edges: &mut Vec<AgentPlanEdge>,
) -> Vec<String> {
    let mut ids = Vec::new();
    for (idx, dep) in task.blocked_by.iter().enumerate() {
        let id = format!("dependency-{}", idx + 1);
        nodes.push(AgentPlanNode {
            id: id.clone(),
            title: format!("Resolve dependency {}", dep.uid),
            kind: AgentPlanNodeKind::Dependency,
            status: AgentPlanNodeStatus::Blocked,
            prompt: format!(
                "This task is blocked by {} (reltype: {:?}, gap: {:?}). Resolve or acknowledge that dependency before delegating the downstream work.",
                dep.uid, dep.reltype, dep.gap
            ),
            depends_on: Vec::new(),
        });
        edges.push(AgentPlanEdge {
            from: dep.uid.clone(),
            to: id.clone(),
        });
        ids.push(id);
    }
    ids
}

fn build_fallback_nodes(task: &Task, inspect_id: &str) -> Vec<AgentPlanNode> {
    let mut nodes = Vec::new();
    let decompose_id = "decompose".to_string();
    nodes.push(AgentPlanNode {
        id: decompose_id.clone(),
        title: "Break the task into agent-sized chunks".into(),
        kind: AgentPlanNodeKind::Decompose,
        status: AgentPlanNodeStatus::Pending,
        prompt: format!(
            "Split '{}' into 2-5 minute subtasks with clear file/path scope, dependencies, and verification steps.",
            task.title
        ),
        depends_on: vec![inspect_id.to_string()],
    });

    let implement_id = "implement".to_string();
    nodes.push(AgentPlanNode {
        id: implement_id.clone(),
        title: "Implement the smallest viable slice".into(),
        kind: AgentPlanNodeKind::Implement,
        status: AgentPlanNodeStatus::Pending,
        prompt: format!(
            "Implement the first bounded slice of '{}' using TDD and keep the change reviewable.",
            task.title
        ),
        depends_on: vec![decompose_id],
    });

    let verify_id = "verify".to_string();
    nodes.push(AgentPlanNode {
        id: verify_id,
        title: "Verify the result and review the diff".into(),
        kind: AgentPlanNodeKind::Verify,
        status: AgentPlanNodeStatus::Pending,
        prompt: format!(
            "Run focused tests, inspect the diff, and confirm the work for '{}' still matches the task goal.",
            task.title
        ),
        depends_on: vec![implement_id],
    });

    nodes
}

fn summarize_body(body: &str) -> String {
    let summary: Vec<&str> = body
        .lines()
        .filter(|line| !line.trim().is_empty())
        .take(6)
        .collect();
    if summary.is_empty() {
        "(no body)".into()
    } else {
        summary.join(" ")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::task::{DependencyRelType, Priority, Status, TaskDependency};

    fn make_task(body: &str, blocked_by: Vec<TaskDependency>) -> Task {
        Task {
            title: "Build agent delegation graph".into(),
            status: Status::Open,
            priority: Priority::Normal,
            body: body.into(),
            blocked_by: blocked_by.into(),
            ..Default::default()
        }
    }

    #[test]
    fn plan_uses_checklist_subtasks_when_present() {
        let task = make_task(
            "- [ ] Inspect current implementation\n- [ ] Add planner output\n- [ ] Verify JSON shape",
            vec![],
        );
        let plan = build_agent_plan(&task);

        let titles = plan
            .nodes
            .iter()
            .map(|node| node.title.as_str())
            .collect::<Vec<_>>();

        assert_eq!(
            titles,
            vec![
                "Inspect task context and constraints",
                "Inspect current implementation",
                "Add planner output",
                "Verify JSON shape",
                "Verify and review the result",
            ]
        );
        assert_eq!(plan.runnable_node_ids, vec!["inspect".to_string()]);
        assert!(plan.blocked_by.is_empty());
        assert_eq!(plan.edges.len(), 4);
    }

    #[test]
    fn plan_falls_back_to_decompose_and_implement_when_no_subtasks_exist() {
        let task = make_task("Implement the feature end to end.", vec![]);
        let plan = build_agent_plan(&task);

        let kinds = plan.nodes.iter().map(|node| &node.kind).collect::<Vec<_>>();
        assert_eq!(
            kinds,
            vec![
                &AgentPlanNodeKind::Inspect,
                &AgentPlanNodeKind::Decompose,
                &AgentPlanNodeKind::Implement,
                &AgentPlanNodeKind::Verify,
            ]
        );
        assert_eq!(plan.runnable_node_ids, vec!["inspect".to_string()]);
        assert_eq!(plan.edges.len(), 3);
    }

    #[test]
    fn blocked_dependencies_are_exposed_as_graph_nodes() {
        let task = make_task(
            "Work starts after an upstream dependency lands.",
            vec![TaskDependency {
                uid: "TASK-123".into(),
                reltype: DependencyRelType::FinishToStart,
                gap: None,
            }],
        );
        let plan = build_agent_plan(&task);

        assert_eq!(plan.blocked_by, vec!["TASK-123".to_string()]);
        assert_eq!(plan.nodes[0].kind, AgentPlanNodeKind::Dependency);
        assert_eq!(plan.nodes[1].status, AgentPlanNodeStatus::Blocked);
        assert!(plan.runnable_node_ids.is_empty());
    }
}
