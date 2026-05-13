#![cfg(test)]

use architect::Page;
use loro::ExportMode;
use threads_crdt::{AttachmentRepoLoro, CommentRepoLoro, CrdtDoc, ReactionRepoLoro};
use threads_proto::{
    AttachmentCreate, AttachmentRepo, CommentCreate, CommentRepo, ReactionCreate, ReactionRepo,
};
use uuid::Uuid;

#[tokio::test]
async fn comment_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let repo = CommentRepoLoro::new(&doc);
    let entity_id = Uuid::new_v4();
    let c = repo
        .create(CommentCreate {
            entity_id,
            entity_type: "task".into(),
            author: "cody".into(),
            body: "needs more cowbell".into(),
            time_start_ms: Some(12_500),
            time_end_ms: Some(15_000),
            reply_to: None,
            resolved: false,
            resolved_by: None,
            mentions: vec!["alice".into()],
            tags: vec!["mix".into()],
        })
        .await
        .unwrap();
    let got = repo.get(c.id).await.unwrap();
    assert_eq!(got.author, "cody");
    assert_eq!(got.entity_id, entity_id);
    assert_eq!(got.body, "needs more cowbell");
    assert_eq!(got.time_start_ms, Some(12_500));
    assert_eq!(got.mentions, vec!["alice".to_string()]);
    assert!(!got.resolved);
}

#[tokio::test]
async fn reaction_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let repo = ReactionRepoLoro::new(&doc);
    let target = Uuid::new_v4();
    let r = repo
        .create(ReactionCreate {
            entity_id: target,
            entity_type: "comment".into(),
            emoji: "🔥".into(),
            user: "cody".into(),
        })
        .await
        .unwrap();
    let got = repo.get(r.id).await.unwrap();
    assert_eq!(got.emoji, "🔥");
    assert_eq!(got.user, "cody");
    assert_eq!(got.entity_id, target);
}

#[tokio::test]
async fn attachment_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let repo = AttachmentRepoLoro::new(&doc);
    let owner = Uuid::new_v4();
    let a = repo
        .create(AttachmentCreate {
            owner_id: owner,
            owner_type: "comment".into(),
            source: "nextcloud".into(),
            path: "/Media/clip.wav".into(),
            label: Some("rough mix".into()),
            mime: Some("audio/wav".into()),
            size_bytes: Some(1_048_576),
            checksum: Some("sha256:abcd".into()),
            uploader: Some("cody".into()),
            tags: vec!["audio".into()],
        })
        .await
        .unwrap();
    let got = repo.get(a.id).await.unwrap();
    assert_eq!(got.path, "/Media/clip.wav");
    assert_eq!(got.owner_id, owner);
    assert_eq!(got.size_bytes, Some(1_048_576));
    assert_eq!(got.mime.as_deref(), Some("audio/wav"));
}

#[tokio::test]
async fn all_three_coexist_in_one_doc() {
    let doc = CrdtDoc::ephemeral();
    let comments = CommentRepoLoro::new(&doc);
    let reactions = ReactionRepoLoro::new(&doc);
    let attachments = AttachmentRepoLoro::new(&doc);

    let target = Uuid::new_v4();

    comments
        .create(CommentCreate {
            entity_id: target,
            entity_type: "task".into(),
            author: "alice".into(),
            body: "first".into(),
            time_start_ms: None,
            time_end_ms: None,
            reply_to: None,
            resolved: false,
            resolved_by: None,
            mentions: vec![],
            tags: vec![],
        })
        .await
        .unwrap();

    reactions
        .create(ReactionCreate {
            entity_id: target,
            entity_type: "task".into(),
            emoji: "👍".into(),
            user: "bob".into(),
        })
        .await
        .unwrap();

    attachments
        .create(AttachmentCreate {
            owner_id: target,
            owner_type: "task".into(),
            source: "local".into(),
            path: "/tmp/a.txt".into(),
            label: None,
            mime: None,
            size_bytes: None,
            checksum: None,
            uploader: None,
            tags: vec![],
        })
        .await
        .unwrap();

    let page = || Page {
        index: 0,
        size: 100,
    };
    assert_eq!(comments.list(page(), None, None).await.unwrap().total, 1);
    assert_eq!(reactions.list(page(), None, None).await.unwrap().total, 1);
    assert_eq!(attachments.list(page(), None, None).await.unwrap().total, 1);
}

#[tokio::test]
async fn replicas_converge_across_all_entities() {
    let a = CrdtDoc::ephemeral();
    let b = CrdtDoc::ephemeral();
    let ca = CommentRepoLoro::new(&a);
    let cb = CommentRepoLoro::new(&b);
    let ra = ReactionRepoLoro::new(&a);
    let rb = ReactionRepoLoro::new(&b);

    let target = Uuid::new_v4();

    ca.create(CommentCreate {
        entity_id: target,
        entity_type: "task".into(),
        author: "alice".into(),
        body: "from-a".into(),
        time_start_ms: None,
        time_end_ms: None,
        reply_to: None,
        resolved: false,
        resolved_by: None,
        mentions: vec![],
        tags: vec![],
    })
    .await
    .unwrap();

    rb.create(ReactionCreate {
        entity_id: target,
        entity_type: "task".into(),
        emoji: "🎉".into(),
        user: "bob".into(),
    })
    .await
    .unwrap();

    let ab = ca.doc().export(ExportMode::all_updates()).unwrap();
    let bb = cb.doc().export(ExportMode::all_updates()).unwrap();
    cb.doc().import(&ab).unwrap();
    ca.doc().import(&bb).unwrap();

    let page = || Page {
        index: 0,
        size: 100,
    };
    assert_eq!(ca.list(page(), None, None).await.unwrap().total, 1);
    assert_eq!(cb.list(page(), None, None).await.unwrap().total, 1);
    assert_eq!(ra.list(page(), None, None).await.unwrap().total, 1);
    assert_eq!(rb.list(page(), None, None).await.unwrap().total, 1);
}
