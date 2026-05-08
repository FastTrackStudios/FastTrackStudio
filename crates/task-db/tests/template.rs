use task_core::service::{MaterializeRequest, TemplateService};
use task_core::service_impl::{TemplateServiceDeps, TemplateServiceImpl};
use task_db::seed;

#[tokio::test]
async fn materialize_album_production_creates_project_plus_tasks() {
    let db = task_db::init_memory().await.expect("init in-memory db");
    seed::seed_demo_data(&db).await.expect("seed");

    let svc = TemplateServiceImpl::new(TemplateServiceDeps { db: db.clone() });

    let templates = svc.list_templates().await.expect("list templates");
    assert!(
        !templates.is_empty(),
        "demo seed should register at least one template"
    );
    let album = templates
        .iter()
        .find(|t| t.integration_name == "audio-production" && t.name == "Album Production")
        .expect("Album Production template seeded");
    let template_task_count: usize =
        serde_json::from_str::<serde_json::Value>(&album.task_templates_json)
            .ok()
            .and_then(|v| v.as_array().map(|a| a.len()))
            .unwrap_or(0);
    assert!(template_task_count >= 5, "want >=5 task templates");

    let result = svc
        .materialize(MaterializeRequest {
            integration_name: "audio-production".to_string(),
            template_name: "Album Production".to_string(),
            project_name: "Test Album 2026".to_string(),
            area: None,
            organization: Some("FastTrackAudio".to_string()),
            lead: Some("cody".to_string()),
        })
        .await
        .expect("materialize");

    assert!(result.created_project, "should create the project");
    assert_eq!(
        result.created_task_count as usize, template_task_count,
        "should create one task per template task"
    );
    assert_eq!(result.unchanged_task_count, 0);

    // Re-running is idempotent: same project, all tasks unchanged.
    let again = svc
        .materialize(MaterializeRequest {
            integration_name: "audio-production".to_string(),
            template_name: "Album Production".to_string(),
            project_name: "Test Album 2026".to_string(),
            area: None,
            organization: Some("FastTrackAudio".to_string()),
            lead: Some("cody".to_string()),
        })
        .await
        .expect("materialize again");
    assert!(
        !again.created_project,
        "second run should reuse the project"
    );
    assert_eq!(again.created_task_count, 0);
    assert_eq!(again.unchanged_task_count as usize, template_task_count);
    assert_eq!(again.project_id, result.project_id);
    assert_eq!(again.task_ids, result.task_ids);
}
