use std::cmp::Reverse;

use task_core::{Query, Task};
use wasm_bindgen::prelude::*;

fn err_json(msg: &str) -> String {
    format!("{{\"error\":\"{}\"}}", msg.replace('"', "\\\""))
}

/// Sort a JSON array of tasks by urgency score descending.
// r[impl plugin.urgency-sort]
#[wasm_bindgen]
pub fn sort_by_urgency(tasks_json: &str) -> String {
    let mut tasks: Vec<Task> = match facet_json::from_str(tasks_json) {
        Ok(t) => t,
        Err(e) => return err_json(&e.to_string()),
    };
    tasks.sort_by_key(|t| Reverse(t.urgency_score()));
    match facet_json::to_string(&tasks) {
        Ok(s) => s,
        Err(e) => err_json(&e.to_string()),
    }
}

/// Compute urgency score for a single task JSON. Returns -1 on error.
// r[impl plugin.urgency-score]
#[wasm_bindgen]
pub fn urgency_score(task_json: &str) -> i32 {
    match facet_json::from_str::<Task>(task_json) {
        Ok(t) => t.urgency_score(),
        Err(_) => -1,
    }
}

/// Execute a Query (JSON) against a task array (JSON). Returns filtered+sorted task array JSON.
// r[impl plugin.query]
#[wasm_bindgen]
pub fn execute_query(tasks_json: &str, query_json: &str) -> String {
    let tasks: Vec<Task> = match facet_json::from_str(tasks_json) {
        Ok(t) => t,
        Err(e) => return err_json(&e.to_string()),
    };
    let query: Query = match facet_json::from_str(query_json) {
        Ok(q) => q,
        Err(e) => return err_json(&e.to_string()),
    };
    let results: Vec<Task> = query.execute(&tasks).into_iter().cloned().collect();
    match facet_json::to_string(&results) {
        Ok(s) => s,
        Err(e) => err_json(&e.to_string()),
    }
}

/// Parse a YAML frontmatter string into a Task JSON string.
// r[impl plugin.parse-task]
#[wasm_bindgen]
pub fn parse_task_yaml(yaml: &str) -> String {
    match facet_yaml::from_str::<Task>(yaml) {
        Ok(task) => match facet_json::to_string(&task) {
            Ok(s) => s,
            Err(e) => err_json(&e.to_string()),
        },
        Err(e) => err_json(&e.to_string()),
    }
}

/// Serialize a Task JSON string to a YAML frontmatter string.
// r[impl plugin.serialize-task]
#[wasm_bindgen]
pub fn serialize_task_yaml(task_json: &str) -> String {
    match facet_json::from_str::<Task>(task_json) {
        Ok(task) => match facet_yaml::to_string(&task) {
            Ok(s) => s,
            Err(e) => err_json(&e.to_string()),
        },
        Err(e) => err_json(&e.to_string()),
    }
}

/// Validate a Task JSON string. Returns empty string if valid, error message otherwise.
// r[impl plugin.validate-task]
#[wasm_bindgen]
pub fn validate_task(task_json: &str) -> String {
    match facet_json::from_str::<Task>(task_json) {
        Ok(_) => String::new(),
        Err(e) => e.to_string(),
    }
}
