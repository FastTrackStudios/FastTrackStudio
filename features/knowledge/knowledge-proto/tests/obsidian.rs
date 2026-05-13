//! Pure unit tests for the Obsidian-compat helpers. No CRDT, no
//! Repo — these functions are string-in / string-out and easy to
//! pin behaviorally.

use chrono::Utc;
use knowledge_proto::obsidian::{
    extract_refs, parse_block_id, parse_frontmatter, parse_page, resolve_linkpath,
    serialize_frontmatter, serialize_page, translate_logseq_block_refs,
};
use knowledge_proto::refs::Ref;
use knowledge_proto::shadow::shadow_page_id;
use knowledge_proto::{Block, Page};
use uuid::Uuid;

// ── frontmatter ─────────────────────────────────────────────────────

#[test]
fn frontmatter_empty_source() {
    let (fm, off) = parse_frontmatter("hello");
    assert!(fm.is_empty());
    assert_eq!(off, 0);
}

#[test]
fn frontmatter_simple() {
    let src = "---\ntitle: Hello\ntags:\n  - foo\n  - bar\n---\nbody\n";
    let (fm, off) = parse_frontmatter(src);
    assert_eq!(fm.len(), 2);
    assert_eq!(fm[0].key, "title");
    assert_eq!(fm[1].key, "tags");
    assert!(matches!(&fm[1].value, serde_json::Value::Array(arr) if arr.len() == 2));
    assert_eq!(&src[off..], "body\n");
}

#[test]
fn frontmatter_aliases_list() {
    let src = "---\naliases:\n  - one\n  - two\n---\n";
    let (fm, _) = parse_frontmatter(src);
    assert_eq!(fm[0].key, "aliases");
    if let serde_json::Value::Array(a) = &fm[0].value {
        assert_eq!(a.len(), 2);
    } else {
        panic!("aliases should be array");
    }
}

#[test]
fn frontmatter_unknown_keys_preserved() {
    let src = "---\nmy_custom_key: 42\nanother: hi\n---\n";
    let (fm, _) = parse_frontmatter(src);
    let keys: Vec<_> = fm.iter().map(|e| e.key.as_str()).collect();
    assert_eq!(keys, vec!["my_custom_key", "another"]);
}

#[test]
fn frontmatter_round_trip_preserves_key_order() {
    let src = "---\nb: 2\na: 1\nc: 3\n---\nbody\n";
    let (fm, _) = parse_frontmatter(src);
    let serialized = serialize_frontmatter(&fm);
    // Key order preserved
    let b_pos = serialized.find("b:").unwrap();
    let a_pos = serialized.find("a:").unwrap();
    let c_pos = serialized.find("c:").unwrap();
    assert!(b_pos < a_pos && a_pos < c_pos);
}

// ── parse_block_id ─────────────────────────────────────────────────

#[test]
fn block_id_trailing() {
    let (c, id) = parse_block_id("hello world ^abc-123");
    assert_eq!(c, "hello world");
    assert_eq!(id.as_deref(), Some("abc-123"));
}

#[test]
fn block_id_own_line() {
    let (c, id) = parse_block_id("^just-id");
    // Block-id regex requires a preceding `\s` OR start-of-line; the
    // current source matches on start-of-line so `^just-id` parses
    // as block id only, leaving content empty.
    assert_eq!(c, "");
    assert_eq!(id.as_deref(), Some("just-id"));
}

#[test]
fn block_id_no_id() {
    let (c, id) = parse_block_id("plain content");
    assert_eq!(c, "plain content");
    assert!(id.is_none());
}

#[test]
fn block_id_malformed() {
    let (c, id) = parse_block_id("text ^");
    assert_eq!(c, "text ^");
    assert!(id.is_none());
}

// ── extract_refs ───────────────────────────────────────────────────

#[test]
fn refs_single_link() {
    let r = extract_refs("see [[Other Page]] for details");
    assert_eq!(r.len(), 1);
    assert!(matches!(&r[0], Ref::Link(l) if l.target_linkpath == "Other Page"));
}

#[test]
fn refs_embed_and_link_and_tag() {
    let r = extract_refs("![[image.png]] and [[Page]] with #project/alpha tag");
    assert_eq!(r.len(), 3);
    assert!(matches!(&r[0], Ref::Embed(_)));
    assert!(matches!(&r[1], Ref::Link(_)));
    assert!(matches!(&r[2], Ref::Tag(t) if t.path == vec!["project", "alpha"]));
}

#[test]
fn refs_link_with_subpath_and_alias() {
    let r = extract_refs("[[Page#Heading|Display]]");
    assert_eq!(r.len(), 1);
    if let Ref::Link(l) = &r[0] {
        assert_eq!(l.target_linkpath, "Page");
        assert_eq!(l.heading.as_deref(), Some("Heading"));
        assert_eq!(l.alias.as_deref(), Some("Display"));
    } else {
        panic!("expected link");
    }
}

#[test]
fn refs_block_id_link() {
    let r = extract_refs("[[Page#^abc-123]]");
    if let Ref::Link(l) = &r[0] {
        assert_eq!(l.block_id.as_deref(), Some("abc-123"));
    } else {
        panic!();
    }
}

#[test]
fn refs_entity_link() {
    let r = extract_refs("see [[entity://task/00000000-0000-0000-0000-000000000001|My Task]]");
    assert_eq!(r.len(), 1);
    if let Ref::Entity(e) = &r[0] {
        assert_eq!(e.kind, "task");
        assert_eq!(e.display.as_deref(), Some("My Task"));
    } else {
        panic!("expected entity ref, got {:?}", r[0]);
    }
}

// ── resolve_linkpath ───────────────────────────────────────────────

fn mk_page(path: &str) -> Page {
    let now = Utc::now();
    let basename = path
        .rsplit('/')
        .next()
        .unwrap()
        .trim_end_matches(".md")
        .to_string();
    Page {
        id: Uuid::new_v4(),
        vault_id: Uuid::nil(),
        folder_id: None,
        path: path.into(),
        basename,
        ext: "md".into(),
        aliases: Vec::new(),
        frontmatter_json: "[]".into(),
        stat_ctime: now,
        stat_mtime: now,
        stat_size: 0,
        is_journal: false,
        journal_day: None,
        shadow_for_kind: None,
        shadow_for_id: None,
        created_at: now,
        updated_at: now,
    }
}

#[test]
fn resolve_unique_basename() {
    let pages = vec![mk_page("notes/Hello.md"), mk_page("other/World.md")];
    let r = resolve_linkpath("Hello", "notes/Source.md", &pages);
    assert_eq!(r, Some(pages[0].id));
}

#[test]
fn resolve_ambiguous_shortest_wins() {
    let pages = vec![
        mk_page("deep/folder/path/Hello.md"),
        mk_page("Hello.md"),
        mk_page("a/Hello.md"),
    ];
    let r = resolve_linkpath("Hello", "src.md", &pages);
    assert_eq!(r, Some(pages[1].id), "expected shortest-path winner");
}

#[test]
fn resolve_with_explicit_path() {
    let pages = vec![mk_page("notes/Hello.md")];
    let r = resolve_linkpath("notes/Hello", "src.md", &pages);
    assert_eq!(r, Some(pages[0].id));
}

#[test]
fn resolve_none_when_missing() {
    let pages = vec![mk_page("Hello.md")];
    let r = resolve_linkpath("Missing", "x.md", &pages);
    assert!(r.is_none());
}

// ── translate_logseq_block_refs ────────────────────────────────────

fn mk_block(id: Uuid, page_id: Uuid, obs: Option<&str>) -> Block {
    let now = Utc::now();
    Block {
        id,
        vault_id: Uuid::nil(),
        page_id,
        parent_block_id: None,
        sort_key: "a".into(),
        kind: "paragraph".into(),
        content: String::new(),
        heading_level: None,
        list_ordered: false,
        list_task: None,
        code_lang: None,
        callout_kind: None,
        callout_foldable: false,
        properties_json: "[]".into(),
        obsidian_block_id: obs.map(str::to_string),
        collapsed: false,
        refs_json: "[]".into(),
        canvas_node_json: None,
        created_at: now,
        updated_at: now,
    }
}

#[test]
fn translate_known_block_ref() {
    let tid = Uuid::parse_str("11111111-2222-3333-4444-555555555555").unwrap();
    let blocks = vec![mk_block(tid, Uuid::nil(), Some("bx7"))];
    let out = translate_logseq_block_refs(&format!("see (({tid}))"), &blocks);
    assert!(out.contains("[[#^bx7]]"), "got: {}", out);
}

#[test]
fn translate_unknown_block_ref_passthrough() {
    let tid = Uuid::parse_str("11111111-2222-3333-4444-555555555555").unwrap();
    let src = format!("see (({tid}))");
    let out = translate_logseq_block_refs(&src, &[]);
    assert_eq!(out, src);
}

// ── parse_page corpus ──────────────────────────────────────────────

#[test]
fn parse_plain_paragraph() {
    let p = parse_page("Hello world.\n");
    assert_eq!(p.frontmatter.len(), 0);
    assert_eq!(p.blocks.len(), 1);
    assert_eq!(p.blocks[0].kind, "paragraph");
    assert_eq!(p.blocks[0].content, "Hello world.");
}

#[test]
fn parse_frontmatter_plus_body() {
    let src = "---\naliases: [a, b]\ntags: [x]\ncustom: val\n---\nHello.\n";
    let p = parse_page(src);
    assert_eq!(p.frontmatter.len(), 3);
    assert_eq!(p.blocks.len(), 1);
    assert_eq!(p.blocks[0].content, "Hello.");
}

#[test]
fn parse_headings() {
    let p = parse_page("# H1\n\n## H2\n\n### H3\n");
    assert_eq!(p.blocks.len(), 3);
    assert_eq!(p.blocks[0].heading_level, Some(1));
    assert_eq!(p.blocks[1].heading_level, Some(2));
    assert_eq!(p.blocks[2].heading_level, Some(3));
}

#[test]
fn parse_nested_unordered_list() {
    let p = parse_page("- one\n  - one-a\n  - one-b\n- two\n");
    let kinds: Vec<&str> = p.blocks.iter().map(|b| b.kind.as_str()).collect();
    assert_eq!(kinds, vec!["list_item"; 4]);
    let depths: Vec<u32> = p.blocks.iter().map(|b| b.indent_depth).collect();
    assert_eq!(depths, vec![0, 1, 1, 0]);
}

#[test]
fn parse_numbered_list_with_tasks() {
    let p = parse_page("1. [ ] todo\n2. [x] done\n");
    assert_eq!(p.blocks.len(), 2);
    assert!(p.blocks.iter().all(|b| b.list_ordered));
    assert_eq!(p.blocks[0].list_task.as_deref(), Some(" "));
    assert_eq!(p.blocks[1].list_task.as_deref(), Some("x"));
}

#[test]
fn parse_code_fence_with_lang() {
    let p = parse_page("```rust\nfn x() {}\n```\n");
    assert_eq!(p.blocks.len(), 1);
    assert_eq!(p.blocks[0].kind, "code");
    assert_eq!(p.blocks[0].code_lang.as_deref(), Some("rust"));
    assert!(p.blocks[0].content.contains("fn x()"));
}

#[test]
fn parse_callout_foldable_and_plain() {
    let p = parse_page("> [!note] Hello\n> body\n\n> [!warning]- Foldy\n> stuff\n");
    assert_eq!(p.blocks.len(), 2);
    assert_eq!(p.blocks[0].callout_kind.as_deref(), Some("note"));
    assert!(!p.blocks[0].callout_foldable);
    assert_eq!(p.blocks[1].callout_kind.as_deref(), Some("warning"));
    assert!(p.blocks[1].callout_foldable);
}

#[test]
fn parse_mixed_inline_refs() {
    let src = "see [[Page]] and ![[img.png]] tagged #project/alpha and \
        [[entity://task/00000000-0000-0000-0000-000000000abc]]\n";
    let p = parse_page(src);
    assert_eq!(p.blocks.len(), 1);
    assert_eq!(p.blocks[0].refs.len(), 4);
}

// ── round-trip stability ──────────────────────────────────────────

fn block_from_parsed(idx: usize, parsed: &knowledge_proto::obsidian::ParsedBlock) -> Block {
    let now = Utc::now();
    let props_json = serde_json::to_string(&parsed.properties).unwrap_or_else(|_| "[]".into());
    let refs_json = serde_json::to_string(&parsed.refs).unwrap_or_else(|_| "[]".into());
    Block {
        id: Uuid::new_v4(),
        vault_id: Uuid::nil(),
        page_id: Uuid::nil(),
        parent_block_id: None,
        sort_key: format!("{:08}", idx),
        kind: parsed.kind.clone(),
        content: parsed.content.clone(),
        heading_level: parsed.heading_level,
        list_ordered: parsed.list_ordered,
        list_task: parsed.list_task.clone(),
        code_lang: parsed.code_lang.clone(),
        callout_kind: parsed.callout_kind.clone(),
        callout_foldable: parsed.callout_foldable,
        properties_json: props_json,
        obsidian_block_id: parsed.obsidian_block_id.clone(),
        collapsed: false,
        refs_json,
        canvas_node_json: None,
        created_at: now,
        updated_at: now,
    }
}

#[test]
fn round_trip_block_tree_stable() {
    // parse → serialize → parse produces an equivalent block tree.
    // Note: we don't claim byte-identity (Phase A serializer normalizes
    // whitespace); we claim the block sequence + key fields match.
    let src = "# Hello\n\nA paragraph.\n\n- one\n- two\n";
    let p1 = parse_page(src);
    let fm_json = serde_json::to_string(&p1.frontmatter).unwrap();
    let now = Utc::now();
    let page = Page {
        id: Uuid::new_v4(),
        vault_id: Uuid::nil(),
        folder_id: None,
        path: "x.md".into(),
        basename: "x".into(),
        ext: "md".into(),
        aliases: Vec::new(),
        frontmatter_json: fm_json,
        stat_ctime: now,
        stat_mtime: now,
        stat_size: 0,
        is_journal: false,
        journal_day: None,
        shadow_for_kind: None,
        shadow_for_id: None,
        created_at: now,
        updated_at: now,
    };
    let blocks: Vec<Block> = p1
        .blocks
        .iter()
        .enumerate()
        .map(|(i, b)| block_from_parsed(i, b))
        .collect();
    let out = serialize_page(&page, &blocks);
    let p2 = parse_page(&out);
    assert_eq!(p1.blocks.len(), p2.blocks.len(), "block count differs");
    for (a, b) in p1.blocks.iter().zip(p2.blocks.iter()) {
        assert_eq!(a.kind, b.kind);
        assert_eq!(a.heading_level, b.heading_level);
        assert_eq!(a.list_ordered, b.list_ordered);
    }
}

// ── shadow_page_id ────────────────────────────────────────────────

#[test]
fn shadow_id_is_deterministic() {
    let id = Uuid::parse_str("11111111-2222-3333-4444-555555555555").unwrap();
    let a = shadow_page_id("task", id);
    let b = shadow_page_id("task", id);
    assert_eq!(a, b);
}

#[test]
fn shadow_id_differs_per_kind() {
    let id = Uuid::parse_str("11111111-2222-3333-4444-555555555555").unwrap();
    let a = shadow_page_id("task", id);
    let b = shadow_page_id("project", id);
    assert_ne!(a, b);
}
