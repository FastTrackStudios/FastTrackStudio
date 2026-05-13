//! Tiny `pulldown-cmark` → `Element` renderer.
//!
//! v1 scope: paragraphs, headings, bold/italic/strikethrough, inline
//! code, fenced code blocks (plain `<pre>`), ordered/unordered lists
//! (nested via a frame stack), links (new tab), block quotes.
//!
//! Syntax highlighting is intentionally deferred to v2.
//!
//! Streaming guard: when `streaming=true` and the source ends with an
//! odd number of fenced-code markers (three backticks), we append a
//! synthetic closing fence to the **parse input** so the partial
//! stream still produces a balanced tree. The stored body is never
//! mutated.

use dioxus::prelude::*;
use pulldown_cmark::{Event, HeadingLevel, Parser, Tag, TagEnd};

#[component]
pub fn Markdown(source: String, streaming: bool) -> Element {
    if source.is_empty() {
        return rsx! { Fragment {} };
    }

    let parse_src = if streaming {
        balance_unclosed_fences(&source)
    } else {
        source.clone()
    };

    let nodes = render_nodes(&parse_src);
    rsx! { div { class: "fts-chat-md", {nodes.into_iter()} } }
}

/// If the source has an odd count of fenced-code markers, append a
/// synthetic closing fence. NEVER mutates the caller's data.
fn balance_unclosed_fences(src: &str) -> String {
    let fence = "```";
    let count = src.matches(fence).count();
    if count % 2 == 1 {
        let mut s = String::with_capacity(src.len() + 5);
        s.push_str(src);
        s.push_str("\n```");
        s
    } else {
        src.to_string()
    }
}

// ── Renderer ─────────────────────────────────────────────────────────
//
// Walk the parser event stream, building children for the current
// frame. On Start, push a frame; on End, pop the frame and wrap its
// children into the parent's children Vec. Text/Code events push leaf
// nodes onto the top frame.

enum Frame {
    Root,
    Paragraph,
    Heading(HeadingLevel),
    Emphasis,
    Strong,
    Strikethrough,
    BlockQuote,
    List { ordered: bool },
    Item,
    Link { href: String, title: String },
    CodeBlock,
}

struct StackFrame {
    kind: Frame,
    children: Vec<Element>,
    // For CodeBlock we accumulate raw text instead of children.
    text_buf: String,
}

fn render_nodes(src: &str) -> Vec<Element> {
    let parser = Parser::new(src);
    let mut stack: Vec<StackFrame> = vec![StackFrame {
        kind: Frame::Root,
        children: Vec::new(),
        text_buf: String::new(),
    }];

    for ev in parser {
        match ev {
            Event::Start(tag) => {
                let frame = match tag {
                    Tag::Paragraph => Frame::Paragraph,
                    Tag::Heading { level, .. } => Frame::Heading(level),
                    Tag::Emphasis => Frame::Emphasis,
                    Tag::Strong => Frame::Strong,
                    Tag::Strikethrough => Frame::Strikethrough,
                    Tag::BlockQuote => Frame::BlockQuote,
                    Tag::List(start) => Frame::List {
                        ordered: start.is_some(),
                    },
                    Tag::Item => Frame::Item,
                    Tag::Link {
                        dest_url, title, ..
                    } => Frame::Link {
                        href: dest_url.into_string(),
                        title: title.into_string(),
                    },
                    Tag::CodeBlock(_kind) => Frame::CodeBlock,
                    // Anything we don't model collapses to a passthrough
                    // paragraph so we don't drop content.
                    _ => Frame::Paragraph,
                };
                stack.push(StackFrame {
                    kind: frame,
                    children: Vec::new(),
                    text_buf: String::new(),
                });
            }
            Event::End(end) => {
                let frame = stack
                    .pop()
                    .expect("pulldown-cmark guarantees Start/End balance");
                let el = finish_frame(frame, end);
                if let Some(top) = stack.last_mut() {
                    top.children.push(el);
                }
            }
            Event::Text(t) => {
                let top = stack.last_mut().expect("root frame is present");
                if matches!(top.kind, Frame::CodeBlock) {
                    top.text_buf.push_str(&t);
                } else {
                    let s = t.into_string();
                    top.children.push(rsx! { "{s}" });
                }
            }
            Event::Code(t) => {
                let s = t.into_string();
                let top = stack.last_mut().expect("root frame is present");
                top.children.push(rsx! {
                    code { class: "rounded bg-muted px-1 py-0.5 font-mono text-[0.9em]", "{s}" }
                });
            }
            Event::SoftBreak => {
                let top = stack.last_mut().unwrap();
                top.children.push(rsx! { " " });
            }
            Event::HardBreak => {
                let top = stack.last_mut().unwrap();
                top.children.push(rsx! { br {} });
            }
            Event::Rule => {
                let top = stack.last_mut().unwrap();
                top.children
                    .push(rsx! { hr { class: "my-3 border-border" } });
            }
            // Skip footnotes, html, tasklist markers etc. — out of v1.
            _ => {}
        }
    }

    let root = stack.pop().expect("root frame");
    root.children
}

fn finish_frame(frame: StackFrame, _end: TagEnd) -> Element {
    let StackFrame {
        kind,
        children,
        text_buf,
    } = frame;
    match kind {
        Frame::Root => rsx! { {children.into_iter()} },
        Frame::Paragraph => rsx! { p { class: "my-2 leading-relaxed", {children.into_iter()} } },
        Frame::Heading(level) => match level {
            HeadingLevel::H1 => {
                rsx! { h1 { class: "mt-4 mb-2 text-2xl font-semibold", {children.into_iter()} } }
            }
            HeadingLevel::H2 => {
                rsx! { h2 { class: "mt-4 mb-2 text-xl font-semibold", {children.into_iter()} } }
            }
            HeadingLevel::H3 => {
                rsx! { h3 { class: "mt-3 mb-2 text-lg font-semibold", {children.into_iter()} } }
            }
            HeadingLevel::H4 => {
                rsx! { h4 { class: "mt-3 mb-1 text-base font-semibold", {children.into_iter()} } }
            }
            HeadingLevel::H5 => {
                rsx! { h5 { class: "mt-2 mb-1 text-sm font-semibold", {children.into_iter()} } }
            }
            HeadingLevel::H6 => {
                rsx! { h6 { class: "mt-2 mb-1 text-xs font-semibold uppercase tracking-wide", {children.into_iter()} } }
            }
        },
        Frame::Emphasis => rsx! { em { {children.into_iter()} } },
        Frame::Strong => rsx! { strong { {children.into_iter()} } },
        Frame::Strikethrough => rsx! { s { {children.into_iter()} } },
        Frame::BlockQuote => rsx! {
            blockquote { class: "my-2 border-l-2 border-border pl-3 italic text-muted-foreground",
                {children.into_iter()}
            }
        },
        Frame::List { ordered } => {
            if ordered {
                rsx! { ol { class: "my-2 list-decimal pl-6 space-y-1", {children.into_iter()} } }
            } else {
                rsx! { ul { class: "my-2 list-disc pl-6 space-y-1", {children.into_iter()} } }
            }
        }
        Frame::Item => rsx! { li { {children.into_iter()} } },
        Frame::Link { href, title } => {
            let title_attr = if title.is_empty() { None } else { Some(title) };
            rsx! {
                a {
                    href: "{href}",
                    target: "_blank",
                    rel: "noopener noreferrer",
                    title: title_attr,
                    class: "text-primary underline decoration-primary/40 underline-offset-2 hover:decoration-primary",
                    {children.into_iter()}
                }
            }
        }
        Frame::CodeBlock => rsx! {
            pre { class: "my-2 rounded-md bg-muted p-3 text-sm font-mono whitespace-pre overflow-x-auto",
                "{text_buf}"
            }
        },
    }
}
