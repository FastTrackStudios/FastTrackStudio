//! `OutlinerEditor` — composes a flat list of `BlockEditor`s with
//! indent visualization and picker popover orchestration.

use dioxus::prelude::*;
use knowledge_proto::Block;
use uuid::Uuid;

use super::block::{BlockEditor, RefClick, RefHover};
use super::input::StructuralOp;
use super::pickers::{
    BlockBrief, BlockRefPicker, PageBrief, SlashCommandMenu, WikilinkPicker, default_slash_commands,
};

/// Patch payload for a single-field edit on a `Block`. Outliner emits
/// these; the parent layer (task-ui) translates into Repo CRUD.
#[derive(Clone, Debug, PartialEq)]
pub enum BlockUpdate {
    /// Full content replacement (LWW per Phase A's `Block.content` note).
    SetContent(String),
    /// Heading level for `kind == "heading"`.
    SetHeadingLevel(Option<i32>),
    /// Task marker (`" "`, `"x"`, `"/"`, …).
    SetTaskMark(Option<String>),
    /// Toggle collapsed/expanded.
    SetCollapsed(bool),
    /// Replace the block kind (paragraph ↔ heading ↔ list_item …).
    SetKind(String),
    /// Replace the per-block `properties_json` map.
    SetProperties(String),
}

#[derive(Clone, Debug, PartialEq)]
enum OpenPicker {
    None,
    Wikilink,
    BlockRef,
    Slash,
    Tag,
}

#[component]
pub fn OutlinerEditor(
    page_id: Uuid,
    blocks: Vec<Block>,
    available_pages: Vec<PageBrief>,
    available_blocks: Vec<BlockBrief>,
    on_block_patch: EventHandler<(Uuid, BlockUpdate)>,
    on_structural: EventHandler<StructuralOp>,
    on_open_page: EventHandler<Uuid>,
    on_open_block: EventHandler<Uuid>,
) -> Element {
    let _ = page_id;
    let mut picker_state = use_signal(|| OpenPicker::None);
    let picker_query = use_signal(String::new);

    rsx! {
        div { class: "flex flex-col gap-1 relative",
            for blk in blocks.iter().cloned() {
                BlockRow {
                    key: "{blk.id}",
                    block: blk.clone(),
                    on_block_patch: on_block_patch,
                    on_structural: on_structural,
                    on_open_page: on_open_page,
                    on_open_block: on_open_block,
                    picker_state: picker_state,
                    picker_query: picker_query,
                }
            }

            // ── Pickers ────────────────────────────────────────────
            if picker_state() == OpenPicker::Wikilink {
                WikilinkPicker {
                    query: picker_query,
                    candidates: available_pages.clone(),
                    on_pick: move |p: PageBrief| {
                        picker_state.set(OpenPicker::None);
                        on_open_page.call(p.id);
                    },
                    on_close: move |_| picker_state.set(OpenPicker::None),
                }
            }
            if picker_state() == OpenPicker::BlockRef {
                BlockRefPicker {
                    query: picker_query,
                    candidates: available_blocks.clone(),
                    on_pick: move |b: BlockBrief| {
                        picker_state.set(OpenPicker::None);
                        on_open_block.call(b.id);
                    },
                    on_close: move |_| picker_state.set(OpenPicker::None),
                }
            }
            if picker_state() == OpenPicker::Slash {
                SlashCommandMenu {
                    query: picker_query,
                    commands: default_slash_commands(),
                    on_run: move |_cmd| {
                        picker_state.set(OpenPicker::None);
                    },
                }
            }
        }
    }
}

#[component]
fn BlockRow(
    block: Block,
    on_block_patch: EventHandler<(Uuid, BlockUpdate)>,
    on_structural: EventHandler<StructuralOp>,
    on_open_page: EventHandler<Uuid>,
    on_open_block: EventHandler<Uuid>,
    picker_state: Signal<OpenPicker>,
    picker_query: Signal<String>,
) -> Element {
    let focused = use_signal(|| false);
    let id = block.id;

    let _ = picker_query;

    let on_content_change = move |new_content: String| {
        on_block_patch.call((id, BlockUpdate::SetContent(new_content)));
    };
    let on_structural_local = move |op: StructuralOp| {
        match &op {
            StructuralOp::OpenWikilinkPicker { .. } => picker_state.set(OpenPicker::Wikilink),
            StructuralOp::OpenBlockRefPicker { .. } => picker_state.set(OpenPicker::BlockRef),
            StructuralOp::OpenSlashMenu { .. } => picker_state.set(OpenPicker::Slash),
            StructuralOp::OpenTagPicker { .. } => picker_state.set(OpenPicker::Tag),
            _ => {}
        }
        let _ = id;
        on_structural.call(op);
    };

    let on_click_ref = move |click: RefClick| match click {
        RefClick::Page(_target) => {
            // Resolver lives at the route — emit nothing here; v1.5a
            // wikilinks aren't UUID-routed yet (Phase B-views).
        }
        RefClick::Block(b) => on_open_block.call(b),
        RefClick::Entity { .. } => {}
        RefClick::Tag(_) => {}
        RefClick::Url(_) => {}
    };
    let on_hover_ref = move |_hover: RefHover| {};

    rsx! {
        div { class: "flex items-start gap-2 py-0.5",
            div { class: "select-none text-muted-foreground/40 mt-1.5", "•" }
            div { class: "flex-1 min-w-0",
                BlockEditor {
                    block: block.clone(),
                    focused: focused,
                    on_content_change: on_content_change,
                    on_structural: on_structural_local,
                    on_click_ref: on_click_ref,
                    on_hover_ref: on_hover_ref,
                }
            }
        }
    }
}
