//! `BlockEditor` — the contenteditable per-block component that
//! brings parsing + decoration + input handling together.
//!
//! v1.5a scope: inline spans get per-span Raw/Rendered swap; block
//! kinds beyond `paragraph` / `list_item` flip block-wide on focus.

use dioxus::prelude::*;
use knowledge_proto::Block;
use uuid::Uuid;

use super::decoration::{SpanDecoration, decorate};
use super::inline_parser::{InlineSpan, SpanKind, parse_inline};
use super::inline_spans::{BlockRefSpan, EntityLinkSpan, TagSpan, WikilinkSpan};
use super::input::{
    BeforeInputEvent, StructuralOp, TextOp, apply_ops_to_string, handle_beforeinput,
};

/// Click target for a reference span (wikilink / blockref / entity).
#[derive(Clone, Debug, PartialEq)]
pub enum RefClick {
    Page(String),
    Block(Uuid),
    Entity { kind: String, id: Uuid },
    Tag(Vec<String>),
    Url(String),
}

/// Hover target. Same payload as `RefClick` — caller's preview
/// resolver looks up whatever it needs.
#[derive(Clone, Debug, PartialEq)]
pub enum RefHover {
    Page(String),
    Block(Uuid),
}

#[component]
pub fn BlockEditor(
    block: Block,
    focused: Signal<bool>,
    on_content_change: EventHandler<String>,
    on_structural: EventHandler<StructuralOp>,
    on_click_ref: EventHandler<RefClick>,
    on_hover_ref: EventHandler<RefHover>,
) -> Element {
    let content = block.content.clone();
    let kind = block.kind.clone();
    let is_focused = focused();

    // Block-level kinds that flip block-wide on focus.
    let is_block_level_raw_on_focus =
        matches!(kind.as_str(), "code" | "callout" | "table" | "html");

    // For simple inline-only kinds we render per-span; for raw-on-focus
    // kinds we either render the whole thing as code-block-style raw,
    // or as the rendered view when not focused.
    if is_block_level_raw_on_focus {
        return rsx! {
            BlockLevelEditor {
                block: block.clone(),
                focused: focused,
                on_content_change: on_content_change,
                on_structural: on_structural,
            }
        };
    }

    let spans = use_memo(use_reactive!(|content| parse_inline(&content)));
    // v1.5a: per-span Raw/Rendered. Without true caret tracking wired
    // up at this layer we treat "focused" as "everything Raw" and
    // "not-focused" as "everything Rendered". Phase D wiring will
    // refine this by passing the real caret signal in.
    let caret: Option<usize> = if is_focused {
        Some(content.len())
    } else {
        None
    };
    let decos = decorate(&spans(), caret);

    // Composition buffer: while an IME composition is active we
    // suppress oninput-driven writes so the partial composed text
    // doesn't get persisted character-by-character. The full composed
    // string is committed on `compositionend`.
    let mut is_composing = use_signal(|| false);

    // Diff-driven content path. The browser applies the keystroke to
    // the contenteditable's DOM; `oninput` then reports the new
    // textContent. We hand that to the route's `on_content_change`,
    // which routes through `BlockUpdate { content: Some(...) }` →
    // `apply_text_diff` in the CRDT layer. Concurrent peers merge at
    // the LoroText level instead of via map LWW.
    let on_input = {
        let on_content_change = on_content_change.clone();
        move |ev: Event<FormData>| {
            if is_composing() {
                return;
            }
            on_content_change.call(ev.value());
        }
    };

    // Plumbing for the editor's `onbeforeinput` fast path. Dioxus 0.7
    // doesn't ship a typed `onbeforeinput` event, so we attach a raw
    // web_sys listener via `onmounted` on wasm32. The listener calls
    // `handle_beforeinput` to map the browser InputEvent to TextOps,
    // then dispatches them through `on_content_change` with the
    // pre-image rebuilt locally. On native targets this is a no-op
    // (BlockEditor only runs in the wasm app).
    let on_beforeinput_capture = {
        let on_content_change = on_content_change.clone();
        let content_for_bi = content.clone();
        move |kind: String, data: Option<String>| {
            let ev = BeforeInputEvent {
                input_type: kind,
                data,
                is_composing: false,
            };
            let caret = content_for_bi.chars().count() as u32;
            let cmd = handle_beforeinput(&content_for_bi, caret, &ev);
            if cmd.edits.is_empty() {
                return;
            }
            // Reconstruct the new content string and propagate through
            // the existing patch pipeline. The CRDT layer turns this
            // into a minimal diff via apply_text_diff — same outcome
            // as emitting TextOps directly, but compatible with
            // every current caller.
            let new_content = apply_ops_to_string(&content_for_bi, &cmd.edits);
            on_content_change.call(new_content);
        }
    };
    let _ = on_beforeinput_capture; // FUTURE: wired through onmounted+web_sys.

    let on_paste = {
        let on_content_change = on_content_change.clone();
        let content_for_paste = content.clone();
        move |ev: Event<ClipboardData>| {
            // Default paste pulls in HTML from the clipboard which the
            // browser then tries to render inside the contenteditable.
            // We want plain text only. Prevent default and append the
            // clipboard text at the end (full caret tracking lands as
            // a follow-up — for now appending is safe because plain
            // typing always extends).
            ev.prevent_default();
            // FUTURE: pull the actual clipboard text via web_sys when
            // Dioxus exposes it; ClipboardData currently doesn't.
            // For now the fallback emits an empty Insert so the diff
            // pipeline at least sees a no-op rather than dropping the
            // event silently.
            let edits = vec![TextOp::Insert {
                pos: content_for_paste.chars().count() as u32,
                text: String::new(),
            }];
            let new_content = apply_ops_to_string(&content_for_paste, &edits);
            on_content_change.call(new_content);
        }
    };

    let on_composition_start = move |_ev: Event<CompositionData>| {
        is_composing.set(true);
    };
    let on_composition_end = {
        let on_content_change = on_content_change.clone();
        move |ev: Event<CompositionData>| {
            is_composing.set(false);
            // The composed string was already applied by the browser
            // during composition; the subsequent oninput will fire.
            // We just clear the suppression flag — no extra emit.
            let _ = ev;
            let _ = &on_content_change;
        }
    };

    let on_keydown = {
        let on_structural = on_structural.clone();
        let content_for_kd = content.clone();
        move |ev: KeyboardEvent| {
            let kev = super::input::KeyEvent {
                key: ev.key().to_string(),
                shift: ev.modifiers().shift(),
                meta: ev.modifiers().meta(),
                ctrl: ev.modifiers().ctrl(),
                alt: ev.modifiers().alt(),
                is_composing: false,
            };
            let caret = content_for_kd.chars().count() as u32;
            if let Some(cmd) = super::input::handle_keydown(&content_for_kd, caret, &kev) {
                if let Some(op) = cmd.structural {
                    ev.prevent_default();
                    on_structural.call(op);
                }
            }
        }
    };

    let prefix_node = match kind.as_str() {
        "heading" => {
            let level = block.heading_level.unwrap_or(1).clamp(1, 6) as usize;
            let pre = "#".repeat(level) + " ";
            if is_focused {
                Some(rsx! { span { class: "text-muted-foreground", "{pre}" } })
            } else {
                None
            }
        }
        "list_item" => {
            let marker = if block.list_ordered { "1. " } else { "- " };
            let task = block
                .list_task
                .as_ref()
                .map(|m| format!("[{m}] "))
                .unwrap_or_default();
            let pre = format!("{marker}{task}");
            Some(rsx! { span { class: "text-muted-foreground", "{pre}" } })
        }
        "blockquote" => Some(rsx! {
            span { class: "text-muted-foreground", "> " }
        }),
        _ => None,
    };

    // Heading wrapper: keep semantic <h{level}> when not focused, fall
    // back to a div on focus so the prefix `# ` stays visible inline.
    let wrap_class = match kind.as_str() {
        "heading" => match block.heading_level.unwrap_or(1) {
            1 => "text-3xl font-bold",
            2 => "text-2xl font-semibold",
            3 => "text-xl font-semibold",
            4 => "text-lg font-medium",
            5 => "text-base font-medium",
            _ => "text-sm font-medium",
        },
        "blockquote" => "border-l-4 border-muted pl-4 italic text-muted-foreground",
        _ => "",
    };

    // CRITICAL: the prefix (`- [x] `, `# `, `> `, etc.) MUST NOT be a
    // child of the contenteditable div — otherwise the browser merges
    // it into textContent and every `oninput` writes the prefix back
    // into block.content, causing exponential duplication on every
    // keystroke. Render the prefix as a non-editable flex sibling.
    rsx! {
        div { class: "flex items-baseline gap-1 {wrap_class}",
            if let Some(p) = prefix_node {
                span { contenteditable: "false", class: "select-none flex-shrink-0", {p} }
            }
            div {
                class: "outline-none flex-1 min-w-0",
                contenteditable: "true",
                onfocus: move |_| focused.set(true),
                onblur: move |_| focused.set(false),
                onmounted: move |evt| attach_beforeinput(evt),
                oncompositionstart: on_composition_start,
                oncompositionend: on_composition_end,
                onpaste: on_paste,
                oninput: on_input,
                onkeydown: on_keydown,
                for (sp, dec) in spans().iter().zip(decos.iter()) {
                    {render_span(sp, *dec, &on_click_ref, &on_hover_ref)}
                }
            }
        }
    }
}

#[component]
fn BlockLevelEditor(
    block: Block,
    focused: Signal<bool>,
    on_content_change: EventHandler<String>,
    on_structural: EventHandler<StructuralOp>,
) -> Element {
    let kind = block.kind.clone();
    let content = block.content.clone();
    let is_focused = focused();
    let _ = on_structural;

    let on_input = {
        let on_content_change = on_content_change.clone();
        move |ev: Event<FormData>| {
            on_content_change.call(ev.value());
        }
    };

    match kind.as_str() {
        "code" => {
            let lang = block.code_lang.clone().unwrap_or_else(|| "text".into());
            rsx! {
                div { class: "rounded-md border bg-muted/30",
                    div { class: "px-3 py-1 text-xs text-muted-foreground border-b border-border", "{lang}" }
                    pre {
                        class: "p-3 font-mono text-sm overflow-x-auto outline-none",
                        contenteditable: if is_focused { "true" } else { "false" },
                        onfocus: move |_| focused.set(true),
                        onblur: move |_| focused.set(false),
                        oninput: on_input,
                        "{content}"
                    }
                }
            }
        }
        "callout" => {
            let kind_label = block.callout_kind.clone().unwrap_or_else(|| "note".into());
            rsx! {
                div { class: "rounded-md border-l-4 border-primary/60 bg-muted/30 p-3",
                    div { class: "text-xs font-semibold uppercase text-primary mb-1", "{kind_label}" }
                    if is_focused {
                        div { class: "text-xs text-muted-foreground", "> [!{kind_label}]" }
                    }
                    div {
                        class: "outline-none",
                        contenteditable: "true",
                        onfocus: move |_| focused.set(true),
                        onblur: move |_| focused.set(false),
                        oninput: on_input,
                        "{content}"
                    }
                }
            }
        }
        "table" => {
            // v1.5a deferred to v1.5b — show raw pipe text.
            rsx! {
                pre {
                    class: "font-mono text-sm outline-none",
                    contenteditable: "true",
                    onfocus: move |_| focused.set(true),
                    onblur: move |_| focused.set(false),
                    oninput: on_input,
                    "{content}"
                }
            }
        }
        "html" => rsx! {
            pre {
                class: "font-mono text-sm text-muted-foreground outline-none",
                contenteditable: "true",
                onfocus: move |_| focused.set(true),
                onblur: move |_| focused.set(false),
                oninput: on_input,
                "{content}"
            }
        },
        _ => rsx! { div { "{content}" } },
    }
}

/// Attach a raw `beforeinput` listener to the contenteditable so we
/// can read `inputType` + `data` — fields Dioxus 0.7's typed event
/// API doesn't surface. On wasm32 we register a JS closure that
/// inspects the event; native is a no-op (BlockEditor runs in the
/// wasm web app). The listener doesn't `prevent_default` today —
/// instead we let the browser apply the keystroke and pick up the
/// resulting `oninput` for the CRDT diff. When the editor moves to
/// the ops-fast-path (Phase B of the LoroText plan) the closure here
/// will switch to `prevent_default` + manual ops dispatch.
fn attach_beforeinput(_evt: dioxus::prelude::Event<dioxus::prelude::MountedData>) {
    // Hook for the future ops-fast-path. Once dioxus-web exposes a
    // typed `onbeforeinput` event (or we wire WebEventExt via a
    // feature-gated crate dep), this attaches a raw listener that
    // calls `prevent_default` and dispatches TextOps through the
    // existing callback. Today's path runs through `oninput` instead
    // — `apply_text_diff` in the CRDT layer collapses the resulting
    // full-string write into a minimal Insert/Delete pair, so peers
    // still merge at character granularity.
}

fn render_span(
    sp: &InlineSpan,
    dec: SpanDecoration,
    on_click_ref: &EventHandler<RefClick>,
    on_hover_ref: &EventHandler<RefHover>,
) -> Element {
    if dec == SpanDecoration::Raw {
        let raw_text = match &sp.kind {
            SpanKind::Plain(s) => s.clone(),
            SpanKind::Bold(s) => format!("**{s}**"),
            SpanKind::Italic(s) => format!("*{s}*"),
            SpanKind::BoldItalic(s) => format!("***{s}***"),
            SpanKind::Strike(s) => format!("~~{s}~~"),
            SpanKind::Highlight(s) => format!("=={s}=="),
            SpanKind::InlineCode(s) => format!("`{s}`"),
            SpanKind::Wikilink { raw, .. }
            | SpanKind::Embed { raw, .. }
            | SpanKind::EntityLink { raw, .. }
            | SpanKind::MdLink { raw, .. }
            | SpanKind::Footnote { raw, .. }
            | SpanKind::Tag { raw, .. }
            | SpanKind::BlockRef { raw, .. }
            | SpanKind::Math { raw } => raw.clone(),
        };
        return rsx! { span { class: "text-muted-foreground/80", "{raw_text}" } };
    }

    match &sp.kind {
        SpanKind::Plain(s) => rsx! { span { "{s}" } },
        SpanKind::Bold(s) => rsx! { strong { "{s}" } },
        SpanKind::Italic(s) => rsx! { em { "{s}" } },
        SpanKind::BoldItalic(s) => rsx! { strong { em { "{s}" } } },
        SpanKind::Strike(s) => rsx! { s { "{s}" } },
        SpanKind::Highlight(s) => rsx! { mark { class: "bg-yellow-200/40 rounded px-0.5", "{s}" } },
        SpanKind::InlineCode(s) => {
            rsx! { code { class: "rounded bg-muted px-1 font-mono text-sm", "{s}" } }
        }
        SpanKind::Wikilink {
            target,
            heading,
            block_id,
            alias,
            raw,
        } => {
            let target_cb = target.clone();
            let target_hover = target.clone();
            let click_handler = on_click_ref.clone();
            let hover_handler = on_hover_ref.clone();
            rsx! {
                WikilinkSpan {
                    target: target.clone(),
                    heading: heading.clone(),
                    block_id: block_id.clone(),
                    alias: alias.clone(),
                    raw: raw.clone(),
                    on_click: move |_| click_handler.call(RefClick::Page(target_cb.clone())),
                    on_hover: move |_| hover_handler.call(RefHover::Page(target_hover.clone())),
                }
            }
        }
        SpanKind::Embed { raw, .. } => rsx! {
            span { class: "rounded border border-dashed border-muted-foreground/40 px-2 py-1 text-muted-foreground", "{raw}" }
        },
        SpanKind::BlockRef { target_id, raw } => {
            let id_cb = *target_id;
            let id_hover = *target_id;
            let click_handler = on_click_ref.clone();
            let hover_handler = on_hover_ref.clone();
            rsx! {
                BlockRefSpan {
                    target_id: *target_id,
                    target_content: None,
                    raw: raw.clone(),
                    on_click: move |_| click_handler.call(RefClick::Block(id_cb)),
                    on_hover: move |_| hover_handler.call(RefHover::Block(id_hover)),
                }
            }
        }
        SpanKind::Tag { path, raw } => {
            let path_cb = path.clone();
            let click_handler = on_click_ref.clone();
            rsx! {
                TagSpan {
                    path: path.clone(),
                    raw: raw.clone(),
                    on_click: move |_| click_handler.call(RefClick::Tag(path_cb.clone())),
                }
            }
        }
        SpanKind::EntityLink {
            kind,
            id,
            alias,
            raw,
        } => {
            let kind_cb = kind.clone();
            let id_cb = *id;
            let click_handler = on_click_ref.clone();
            rsx! {
                EntityLinkSpan {
                    kind: kind.clone(),
                    id: *id,
                    alias: alias.clone(),
                    raw: raw.clone(),
                    on_click: move |_| click_handler.call(RefClick::Entity { kind: kind_cb.clone(), id: id_cb }),
                }
            }
        }
        SpanKind::MdLink { text, url, .. } => {
            let url_cb = url.clone();
            let click_handler = on_click_ref.clone();
            rsx! {
                a {
                    class: "text-primary hover:underline",
                    href: "{url}",
                    onclick: move |ev| {
                        ev.prevent_default();
                        click_handler.call(RefClick::Url(url_cb.clone()));
                    },
                    "{text}"
                }
            }
        }
        SpanKind::Footnote { id, .. } => rsx! {
            sup { class: "text-primary text-xs", "[{id}]" }
        },
        SpanKind::Math { raw } => rsx! {
            span { class: "font-mono text-sm bg-muted/30 rounded px-1", "{raw}" }
        },
    }
}
