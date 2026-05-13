//! Native integration tests for the `chat` feature.

#![cfg(test)]

use architect::Page;
use chat_crdt::{ChannelMemberRepoLoro, ChannelRepoLoro, CrdtDoc, MessageRepoLoro};
use chat_proto::{
    ChannelCreate, ChannelMemberCreate, ChannelMemberRepo, ChannelRepo, MessageCreate, MessageRepo,
};
use chrono::Utc;
use loro::ExportMode;

#[tokio::test]
async fn channel_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let channels = ChannelRepoLoro::new(&doc);
    let c = channels
        .create(ChannelCreate {
            name: "general".into(),
            kind: "group".into(),
            topic: Some("watercooler".into()),
            project_id: None,
            archived: false,
            tags: vec!["public".into()],
        })
        .await
        .unwrap();
    let got = channels.get(c.id).await.unwrap();
    assert_eq!(got.name, "general");
    assert_eq!(got.kind, "group");
    assert_eq!(got.topic.as_deref(), Some("watercooler"));
    assert_eq!(got.tags, vec!["public".to_string()]);
    assert!(!got.archived);
}

#[tokio::test]
async fn message_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let channels = ChannelRepoLoro::new(&doc);
    let messages = MessageRepoLoro::new(&doc);

    let ch = channels
        .create(ChannelCreate {
            name: "dev".into(),
            kind: "group".into(),
            topic: None,
            project_id: None,
            archived: false,
            tags: vec![],
        })
        .await
        .unwrap();

    let m = messages
        .create(MessageCreate {
            channel_id: Some(ch.id),
            author: "cody".into(),
            body: "hello world".into(),
            reply_to: None,
            edited_at: None,
            deleted: false,
            mentions: vec!["alice".into()],
            attachment_ids: vec!["att-1".into()],
            role: None,
            model: None,
            reasoning: None,
            tool_calls_json: None,
            finish_reason: None,
            tokens_input: None,
            tokens_output: None,
            cost_cents: None,
            streaming: false,
            agent_conversation_id: None,
        })
        .await
        .unwrap();

    let got = messages.get(m.id).await.unwrap();
    assert_eq!(got.body, "hello world");
    assert_eq!(got.author, "cody");
    assert_eq!(got.channel_id, Some(ch.id));
    assert_eq!(got.mentions, vec!["alice".to_string()]);
    assert_eq!(got.attachment_ids, vec!["att-1".to_string()]);
    assert!(!got.deleted);
}

#[tokio::test]
async fn member_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let channels = ChannelRepoLoro::new(&doc);
    let members = ChannelMemberRepoLoro::new(&doc);

    let ch = channels
        .create(ChannelCreate {
            name: "ops".into(),
            kind: "group".into(),
            topic: None,
            project_id: None,
            archived: false,
            tags: vec![],
        })
        .await
        .unwrap();

    let mem = members
        .create(ChannelMemberCreate {
            channel_id: ch.id,
            user: "cody".into(),
            role: "admin".into(),
            joined_at: Utc::now(),
            last_read_message_id: None,
            muted: false,
        })
        .await
        .unwrap();

    let got = members.get(mem.id).await.unwrap();
    assert_eq!(got.user, "cody");
    assert_eq!(got.role, "admin");
    assert_eq!(got.channel_id, ch.id);
    assert!(!got.muted);
}

#[tokio::test]
async fn all_three_coexist_in_one_doc() {
    let doc = CrdtDoc::ephemeral();
    let channels = ChannelRepoLoro::new(&doc);
    let messages = MessageRepoLoro::new(&doc);
    let members = ChannelMemberRepoLoro::new(&doc);

    let ch = channels
        .create(ChannelCreate {
            name: "coexist".into(),
            kind: "group".into(),
            topic: None,
            project_id: None,
            archived: false,
            tags: vec![],
        })
        .await
        .unwrap();

    messages
        .create(MessageCreate {
            channel_id: Some(ch.id),
            author: "a".into(),
            body: "m1".into(),
            reply_to: None,
            edited_at: None,
            deleted: false,
            mentions: vec![],
            attachment_ids: vec![],
            role: None,
            model: None,
            reasoning: None,
            tool_calls_json: None,
            finish_reason: None,
            tokens_input: None,
            tokens_output: None,
            cost_cents: None,
            streaming: false,
            agent_conversation_id: None,
        })
        .await
        .unwrap();

    members
        .create(ChannelMemberCreate {
            channel_id: ch.id,
            user: "a".into(),
            role: "member".into(),
            joined_at: Utc::now(),
            last_read_message_id: None,
            muted: false,
        })
        .await
        .unwrap();

    let page = || Page {
        index: 0,
        size: 100,
    };
    assert_eq!(channels.list(page(), None, None).await.unwrap().total, 1);
    assert_eq!(messages.list(page(), None, None).await.unwrap().total, 1);
    assert_eq!(members.list(page(), None, None).await.unwrap().total, 1);
}

#[tokio::test]
async fn replicas_converge_across_all_entities() {
    let a = CrdtDoc::ephemeral();
    let b = CrdtDoc::ephemeral();
    let ca = ChannelRepoLoro::new(&a);
    let cb = ChannelRepoLoro::new(&b);
    let ma = MessageRepoLoro::new(&a);
    let mb = MessageRepoLoro::new(&b);
    let mema = ChannelMemberRepoLoro::new(&a);
    let memb = ChannelMemberRepoLoro::new(&b);

    let cha = ca
        .create(ChannelCreate {
            name: "a-side".into(),
            kind: "group".into(),
            topic: None,
            project_id: None,
            archived: false,
            tags: vec![],
        })
        .await
        .unwrap();
    let chb = cb
        .create(ChannelCreate {
            name: "b-side".into(),
            kind: "group".into(),
            topic: None,
            project_id: None,
            archived: false,
            tags: vec![],
        })
        .await
        .unwrap();

    ma.create(MessageCreate {
        channel_id: Some(cha.id),
        author: "alice".into(),
        body: "from-a".into(),
        reply_to: None,
        edited_at: None,
        deleted: false,
        mentions: vec![],
        attachment_ids: vec![],
        role: None,
        model: None,
        reasoning: None,
        tool_calls_json: None,
        finish_reason: None,
        tokens_input: None,
        tokens_output: None,
        cost_cents: None,
        streaming: false,
        agent_conversation_id: None,
    })
    .await
    .unwrap();
    mb.create(MessageCreate {
        channel_id: Some(chb.id),
        author: "bob".into(),
        body: "from-b".into(),
        reply_to: None,
        edited_at: None,
        deleted: false,
        mentions: vec![],
        attachment_ids: vec![],
        role: None,
        model: None,
        reasoning: None,
        tool_calls_json: None,
        finish_reason: None,
        tokens_input: None,
        tokens_output: None,
        cost_cents: None,
        streaming: false,
        agent_conversation_id: None,
    })
    .await
    .unwrap();

    mema.create(ChannelMemberCreate {
        channel_id: cha.id,
        user: "alice".into(),
        role: "admin".into(),
        joined_at: Utc::now(),
        last_read_message_id: None,
        muted: false,
    })
    .await
    .unwrap();
    memb.create(ChannelMemberCreate {
        channel_id: chb.id,
        user: "bob".into(),
        role: "admin".into(),
        joined_at: Utc::now(),
        last_read_message_id: None,
        muted: false,
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
    assert_eq!(ca.list(page(), None, None).await.unwrap().total, 2);
    assert_eq!(cb.list(page(), None, None).await.unwrap().total, 2);
    assert_eq!(ma.list(page(), None, None).await.unwrap().total, 2);
    assert_eq!(mb.list(page(), None, None).await.unwrap().total, 2);
    assert_eq!(mema.list(page(), None, None).await.unwrap().total, 2);
    assert_eq!(memb.list(page(), None, None).await.unwrap().total, 2);
}

// ── Phase A: AI-extended Message fields round trip ───────────────────

#[tokio::test]
async fn ai_message_fields_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let messages = MessageRepoLoro::new(&doc);

    let conv_id = uuid::Uuid::new_v4();

    // All AI fields set.
    let full = messages
        .create(MessageCreate {
            channel_id: None,
            author: "assistant".into(),
            body: "Here's the plan.".into(),
            reply_to: None,
            edited_at: None,
            deleted: false,
            mentions: vec![],
            attachment_ids: vec![],
            role: Some("assistant".into()),
            model: Some("claude-opus-4-7".into()),
            reasoning: Some("Thinking step by step...".into()),
            tool_calls_json: Some("[{\"name\":\"shell\"}]".into()),
            finish_reason: Some("stop".into()),
            tokens_input: Some(420),
            tokens_output: Some(180),
            cost_cents: Some(3),
            streaming: false,
            agent_conversation_id: Some(conv_id),
        })
        .await
        .unwrap();

    let got = messages.get(full.id).await.unwrap();
    assert_eq!(got.channel_id, None);
    assert_eq!(got.agent_conversation_id, Some(conv_id));
    assert_eq!(got.role.as_deref(), Some("assistant"));
    assert_eq!(got.model.as_deref(), Some("claude-opus-4-7"));
    assert!(got.reasoning.is_some());
    assert_eq!(got.tool_calls_json.as_deref(), Some("[{\"name\":\"shell\"}]"));
    assert_eq!(got.finish_reason.as_deref(), Some("stop"));
    assert_eq!(got.tokens_input, Some(420));
    assert_eq!(got.tokens_output, Some(180));
    assert_eq!(got.cost_cents, Some(3));
    assert!(!got.streaming);

    // All AI fields None (human chat shape, channel_id Some).
    let ch_id = uuid::Uuid::new_v4();
    let bare = messages
        .create(MessageCreate {
            channel_id: Some(ch_id),
            author: "cody".into(),
            body: "hi".into(),
            reply_to: None,
            edited_at: None,
            deleted: false,
            mentions: vec![],
            attachment_ids: vec![],
            role: None,
            model: None,
            reasoning: None,
            tool_calls_json: None,
            finish_reason: None,
            tokens_input: None,
            tokens_output: None,
            cost_cents: None,
            streaming: false,
            agent_conversation_id: None,
        })
        .await
        .unwrap();
    let got2 = messages.get(bare.id).await.unwrap();
    assert_eq!(got2.channel_id, Some(ch_id));
    assert_eq!(got2.agent_conversation_id, None);
    assert!(got2.role.is_none());
    assert!(got2.tokens_input.is_none());
}
