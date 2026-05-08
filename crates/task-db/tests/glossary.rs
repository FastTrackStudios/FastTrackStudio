//! Integration tests for the cross-cutting Glossary entity, the
//! `[[wikilink]]` resolver, and the demo seed.

use task_core::service::{
    CreateGlossaryTermRequest, GlossaryService, GlossaryTermPatch, ResolveInTextRequest,
};
use task_core::service_impl::{GlossaryServiceDeps, GlossaryServiceImpl};
use task_db::seed::seed_demo_data;

#[tokio::test]
async fn seed_populates_glossary_across_categories() {
    let db = task_db::init_memory().await.expect("init db");
    let s = seed_demo_data(&db).await.expect("seed");
    assert!(
        s.glossary_terms_created >= 13,
        "glossary_terms_created={}",
        s.glossary_terms_created
    );
    let svc = GlossaryServiceImpl::new(GlossaryServiceDeps { db: db.clone() });
    let cooking = svc
        .list_terms(None, Some("cooking".into()))
        .await
        .expect("list cooking");
    assert!(
        cooking.len() >= 10,
        "cooking categories should seed >=10 terms; got {}",
        cooking.len()
    );
    let audio = svc
        .list_terms(None, Some("audio-production".into()))
        .await
        .expect("list audio");
    assert!(
        audio.len() >= 3,
        "audio-production should seed >=3 terms; got {}",
        audio.len()
    );
}

#[tokio::test]
async fn find_term_by_slug_or_alias() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = GlossaryServiceImpl::new(GlossaryServiceDeps { db: db.clone() });

    let by_slug = svc
        .find_term_by_slug_or_alias(None, None, "simmer".into())
        .await
        .expect("by slug");
    assert!(by_slug.is_some());
    assert_eq!(by_slug.unwrap().slug, "simmer");

    let by_alias = svc
        .find_term_by_slug_or_alias(None, None, "simmering".into())
        .await
        .expect("by alias");
    assert!(by_alias.is_some());
    assert_eq!(by_alias.unwrap().slug, "simmer");

    let missing = svc
        .find_term_by_slug_or_alias(None, None, "definitely-not-a-term".into())
        .await
        .expect("missing");
    assert!(missing.is_none());
}

#[tokio::test]
async fn resolve_in_text_hits_seeded_terms_via_alias() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = GlossaryServiceImpl::new(GlossaryServiceDeps { db: db.clone() });

    let view = svc
        .resolve_in_text(ResolveInTextRequest {
            text: "Bring to a [[simmer]] then [[deglazing]] the pan.".into(),
            organization: None,
            category: Some("cooking".into()),
        })
        .await
        .expect("resolve");
    assert_eq!(view.resolved_term_ids.len(), 2);
}

#[tokio::test]
async fn resolve_in_text_respects_category_scope() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = GlossaryServiceImpl::new(GlossaryServiceDeps { db: db.clone() });

    let view = svc
        .resolve_in_text(ResolveInTextRequest {
            text: "Then we go to [[mastering]].".into(),
            organization: None,
            category: Some("cooking".into()),
        })
        .await
        .expect("resolve cooking-only");
    assert!(
        view.resolved_term_ids.is_empty(),
        "audio-only [[mastering]] must not resolve under cooking scope"
    );

    let view = svc
        .resolve_in_text(ResolveInTextRequest {
            text: "Then we go to [[mastering]].".into(),
            organization: None,
            category: Some("audio-production".into()),
        })
        .await
        .expect("resolve audio");
    assert_eq!(view.resolved_term_ids.len(), 1);
}

#[tokio::test]
async fn resolve_in_text_skips_code_blocks() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = GlossaryServiceImpl::new(GlossaryServiceDeps { db: db.clone() });

    let text = "Real [[simmer]]\n```\nfake [[deglaze]] inside fence\n```\nreal [[reduce]]";
    let view = svc
        .resolve_in_text(ResolveInTextRequest {
            text: text.to_string(),
            organization: None,
            category: Some("cooking".into()),
        })
        .await
        .expect("resolve");
    assert_eq!(view.resolved_term_ids.len(), 2);
}

#[tokio::test]
async fn add_alias_is_idempotent() {
    let db = task_db::init_memory().await.expect("init db");
    let svc = GlossaryServiceImpl::new(GlossaryServiceDeps { db: db.clone() });
    let term = svc
        .create_term(CreateGlossaryTermRequest {
            name: "Test Term".into(),
            slug: None,
            body_markdown: "definition".into(),
            aliases: vec!["alpha".into()],
            category: "cooking".into(),
            related_term_ids: vec![],
            organization: None,
            created_by: None,
        })
        .await
        .expect("create");

    let updated = svc
        .add_alias(term.id, "Beta".into())
        .await
        .expect("add alias");
    assert_eq!(
        updated.aliases.0,
        vec!["alpha".to_string(), "beta".to_string()]
    );
    let again = svc
        .add_alias(term.id, "BETA".into())
        .await
        .expect("add alias again");
    assert_eq!(
        again.aliases.0,
        vec!["alpha".to_string(), "beta".to_string()],
        "duplicate alias should be a no-op"
    );

    // Update path also normalizes.
    let patched = svc
        .update_term(
            term.id,
            GlossaryTermPatch {
                aliases: Some(vec!["Gamma".into(), "  gamma  ".into(), "delta".into()]),
                ..Default::default()
            },
        )
        .await
        .expect("update aliases");
    assert_eq!(
        patched.aliases.0,
        vec!["gamma".to_string(), "delta".to_string()]
    );
}
