//! Create Entity Modal — shared dialog for creating presets, profiles, songs, scenes, etc.
//!
//! For **Preset** creation, the modal uses a two-column layout:
//! - Left: template selection cards + form fields
//! - Right: live `DynamicGridView` preview with module group containers

use crate::components::module_editor::grid_view::{DynamicGridView, GridConnection, GridSelection};
use crate::components::module_editor::module_editor_view::CompositionSlot;
use crate::hooks::rig_actions::CreateEntityData;
use crate::prelude::*;
use signal_control::defaults::templates;
use signal_control::template::RigTemplate;
use uuid::Uuid;

/// What kind of entity is being created — controls field labels and placeholders.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum EntityKind {
    Preset,
    Profile,
    Song,
    Scene,
    Setlist,
}

impl EntityKind {
    pub fn title(&self) -> &'static str {
        match self {
            Self::Preset => "New Preset",
            Self::Profile => "New Profile",
            Self::Song => "New Song",
            Self::Scene => "New Section",
            Self::Setlist => "New Setlist",
        }
    }

    fn name_placeholder(&self) -> &'static str {
        match self {
            Self::Preset => "e.g. Clean Sparkle",
            Self::Profile => "e.g. Worship Set",
            Self::Song => "e.g. Amazing Grace",
            Self::Scene => "e.g. Intro",
            Self::Setlist => "e.g. Sunday Service",
        }
    }

    fn category_placeholder(&self) -> &'static str {
        match self {
            Self::Preset => "e.g. Clean, Crunch, Lead, Ambient",
            Self::Profile => "e.g. Live, Studio, Practice",
            Self::Song => "e.g. Worship, Rock, Jazz",
            Self::Scene => "e.g. Verse, Chorus, Bridge",
            Self::Setlist => "e.g. Sunday, Rehearsal",
        }
    }

    fn accent(&self) -> &'static str {
        match self {
            Self::Preset => "blue",
            Self::Profile => "purple",
            Self::Song => "emerald",
            Self::Scene => "teal",
            Self::Setlist => "amber",
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Template options
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Clone)]
struct TemplateOption {
    template: Option<RigTemplate>,
    /// Index for CreateEntityData::template_index. None = blank.
    template_index: Option<usize>,
    name: &'static str,
    description: &'static str,
    icon: &'static str,
}

fn available_templates() -> Vec<TemplateOption> {
    vec![
        TemplateOption {
            template: None,
            template_index: None,
            name: "Blank",
            description: "Empty preset -- start from scratch",
            icon: "\u{2795}",
        },
        TemplateOption {
            template: Some(templates::guitar_rig_template()),
            template_index: Some(0),
            name: "Guitar Rig",
            description: "11 modules, 28 blocks -- full guitar signal chain",
            icon: "\u{1F3B8}",
        },
        TemplateOption {
            template: Some(templates::vocal_rig_template()),
            template_index: Some(1),
            name: "Vocal Rig",
            description: "5 modules, 13 blocks -- vocal processing chain",
            icon: "\u{1F3A4}",
        },
    ]
}

// ─────────────────────────────────────────────────────────────────────────────
// Template → CompositionSlots (with module grouping)
// ─────────────────────────────────────────────────────────────────────────────

/// Preferred max columns before wrapping (may be exceeded to keep a module intact).
const PREVIEW_MAX_COLS: usize = 14;
/// Row band stride — 3 rows between bands (2 empty gap rows).
const PREVIEW_ROW_STRIDE: usize = 3;

/// Convert a `RigTemplate` into `CompositionSlot`s for the grid preview.
///
/// Rules match `modules_to_composition_chain`: modules are never split,
/// 2D modules are vertically centered, row bands are spaced by stride 3.
fn template_to_slots(template: &RigTemplate) -> Vec<CompositionSlot> {
    let mut slots = Vec::new();
    let mut col: usize = 0;
    let mut row: usize = 0;

    for module in &template.modules {
        let has_2d = module.grid_width.is_some();

        // Compute module width
        let module_width = if has_2d {
            module.grid_width.unwrap_or(1)
        } else {
            module.blocks.len().max(1)
        };

        // Wrap to next row band if module won't fit (never split)
        if col > 0 && col + module_width > PREVIEW_MAX_COLS {
            col = 0;
            row += PREVIEW_ROW_STRIDE;
        }

        if has_2d {
            let gh = module.grid_height.unwrap_or(1);
            let vert_offset = if gh > 1 { (gh - 1) / 2 } else { 0 };
            let base_row = row.saturating_sub(vert_offset);
            let base_col = col;

            for block in &module.blocks {
                let lc = block.local_col.unwrap_or(0);
                let lr = block.local_row.unwrap_or(0);
                slots.push(CompositionSlot {
                    id: Uuid::new_v4(),
                    block_type: block.block_type,
                    block_preset_id: None,
                    block_preset_name: Some(
                        block.alias.as_deref().unwrap_or(&block.name).to_string(),
                    ),
                    plugin_name: Some(module.name.clone()),
                    col: base_col + lc,
                    row: base_row + lr,
                    module_group: Some(module.name.clone()),
                    module_type: Some(module.module_type),
                    is_template: true,
                    bypassed: false,
                });
            }

            col = base_col + module_width;
        } else {
            // Linear module: all blocks on the same row
            let base_col = col;
            for (i, block) in module.blocks.iter().enumerate() {
                slots.push(CompositionSlot {
                    id: Uuid::new_v4(),
                    block_type: block.block_type,
                    block_preset_id: None,
                    block_preset_name: Some(
                        block.alias.as_deref().unwrap_or(&block.name).to_string(),
                    ),
                    plugin_name: Some(module.name.clone()),
                    col: base_col + i,
                    row,
                    module_group: Some(module.name.clone()),
                    module_type: Some(module.module_type),
                    is_template: true,
                    bypassed: false,
                });
            }
            col = base_col + module_width;
        }
    }
    slots
}

// ─────────────────────────────────────────────────────────────────────────────
// Template Card
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct TemplateCardProps {
    name: &'static str,
    description: &'static str,
    icon: &'static str,
    is_selected: bool,
    on_click: Callback<()>,
}

#[component]
fn TemplateCard(props: TemplateCardProps) -> Element {
    let border = if props.is_selected {
        "border-blue-500/60 bg-blue-500/10"
    } else {
        "border-zinc-700/40 hover:border-zinc-500/60 hover:bg-zinc-800/40"
    };

    rsx! {
        button {
            class: "flex items-center gap-3 w-full p-3 rounded-lg border {border} \
                    text-left transition-all duration-150 cursor-pointer",
            onclick: move |_| props.on_click.call(()),
            span { class: "text-2xl flex-shrink-0 w-9 text-center", "{props.icon}" }
            div { class: "flex-1 min-w-0",
                div { class: "text-sm font-medium text-zinc-200", "{props.name}" }
                div { class: "text-[11px] text-zinc-500 mt-0.5", "{props.description}" }
            }
            if props.is_selected {
                div { class: "w-5 h-5 rounded-full bg-blue-500 flex items-center justify-center flex-shrink-0",
                    svg {
                        xmlns: "http://www.w3.org/2000/svg",
                        width: "12", height: "12",
                        view_box: "0 0 24 24",
                        fill: "none", stroke: "white", stroke_width: "3",
                        stroke_linecap: "round", stroke_linejoin: "round",
                        polyline { points: "20 6 9 17 4 12" }
                    }
                }
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Main Modal
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct CreateEntityModalProps {
    pub kind: EntityKind,
    pub is_open: bool,
    pub on_submit: Callback<CreateEntityData>,
    pub on_close: Callback<()>,
}

#[component]
pub fn CreateEntityModal(props: CreateEntityModalProps) -> Element {
    if !props.is_open {
        return rsx! {};
    }

    let kind = props.kind;
    let accent = kind.accent();
    let is_preset = kind == EntityKind::Preset;

    let mut name = use_signal(String::new);
    let mut category = use_signal(String::new);
    let mut tags_input = use_signal(String::new);
    let mut description = use_signal(String::new);
    let mut selected_template_idx = use_signal(|| 0usize);

    let name_val = name();
    let can_submit = !name_val.trim().is_empty();

    let border_class = format!("border-{accent}-500/40");
    let focus_border =
        format!("focus:border-{accent}-500/60 focus:ring-1 focus:ring-{accent}-500/20");
    let btn_class = format!(
        "bg-{accent}-600 hover:bg-{accent}-500 disabled:opacity-30 disabled:cursor-not-allowed"
    );

    let on_close_inner = props.on_close.clone();

    let template_options = if is_preset {
        available_templates()
    } else {
        Vec::new()
    };
    let template_options_for_submit = template_options.clone();

    // Build grid preview slots from selected template (with module grouping)
    let preview_slots: Vec<CompositionSlot> = if is_preset {
        template_options
            .get(selected_template_idx())
            .and_then(|opt| opt.template.as_ref())
            .map(template_to_slots)
            .unwrap_or_default()
    } else {
        Vec::new()
    };

    let do_submit = {
        let on_submit = props.on_submit.clone();
        move |_: ()| {
            if name().trim().is_empty() {
                return;
            }
            let tags: Vec<String> = tags_input()
                .split(',')
                .map(|t| t.trim().to_string())
                .filter(|t| !t.is_empty())
                .collect();

            let template_index = if is_preset {
                let idx = selected_template_idx();
                template_options_for_submit
                    .get(idx)
                    .and_then(|opt| opt.template_index)
            } else {
                None
            };

            on_submit.call(CreateEntityData {
                name: name().trim().to_string(),
                category: category().trim().to_string(),
                description: description().trim().to_string(),
                tags,
                template_index,
            });
            name.set(String::new());
            category.set(String::new());
            tags_input.set(String::new());
            description.set(String::new());
            selected_template_idx.set(0);
        }
    };

    // Wider modal for presets (need room for preview), normal for others
    let modal_width = if is_preset {
        "max-w-[95vw] w-[95vw]"
    } else {
        "max-w-3xl w-[80vw]"
    };

    rsx! {
        // Backdrop
        div {
            class: "fixed inset-0 z-50 flex items-center justify-center bg-black/70 backdrop-blur-sm",
            onclick: move |_| props.on_close.call(()),
            onkeydown: move |e| {
                if e.key() == Key::Escape {
                    on_close_inner.call(());
                }
            },

            // Modal card
            div {
                class: "{modal_width} max-h-[85vh] flex flex-col \
                        bg-zinc-900 border border-zinc-700/60 rounded-xl shadow-2xl shadow-black/40 \
                        overflow-hidden",
                onclick: |e| e.stop_propagation(),
                onkeydown: |e| e.stop_propagation(),
                onkeyup: |e| e.stop_propagation(),

                // Header
                div { class: "flex items-center justify-between px-6 py-4 border-b border-zinc-800",
                    h2 { class: "text-lg font-semibold text-zinc-100", "{kind.title()}" }
                    button {
                        class: "w-8 h-8 flex items-center justify-center rounded-lg \
                                text-zinc-500 hover:text-zinc-200 hover:bg-zinc-800 transition-colors",
                        onclick: move |_| props.on_close.call(()),
                        svg {
                            xmlns: "http://www.w3.org/2000/svg",
                            width: "18", height: "18",
                            view_box: "0 0 24 24",
                            fill: "none", stroke: "currentColor", stroke_width: "2",
                            stroke_linecap: "round", stroke_linejoin: "round",
                            line { x1: "18", y1: "6", x2: "6", y2: "18" }
                            line { x1: "6", y1: "6", x2: "18", y2: "18" }
                        }
                    }
                }

                // Body — two-column for presets, single column for others
                div { class: "flex-1 overflow-hidden flex min-h-0",

                    // Left column: template cards + form fields
                    div {
                        class: if is_preset {
                            "w-[340px] flex-shrink-0 overflow-y-auto px-6 py-5 space-y-4 border-r border-zinc-800"
                        } else {
                            "flex-1 overflow-y-auto px-6 py-5 space-y-5"
                        },

                        // Template selection (Preset only)
                        if is_preset {
                            div {
                                label { class: "block text-xs font-semibold text-zinc-400 uppercase tracking-wider mb-2",
                                    "Template"
                                }
                                div { class: "space-y-1.5",
                                    for (i, opt) in template_options.iter().enumerate() {
                                        TemplateCard {
                                            key: "{opt.name}",
                                            name: opt.name,
                                            description: opt.description,
                                            icon: opt.icon,
                                            is_selected: selected_template_idx() == i,
                                            on_click: {
                                                let i = i;
                                                Callback::new(move |_| selected_template_idx.set(i))
                                            },
                                        }
                                    }
                                }
                            }
                        }

                        // Name field
                        div {
                            label { class: "block text-xs font-semibold text-zinc-400 uppercase tracking-wider mb-1.5",
                                "Name"
                            }
                            input {
                                class: "w-full px-4 py-2.5 text-sm bg-zinc-800/80 border {border_class} \
                                        rounded-lg text-zinc-200 placeholder:text-zinc-600 outline-none \
                                        {focus_border} transition-colors",
                                r#type: "text",
                                placeholder: "{kind.name_placeholder()}",
                                value: "{name}",
                                autofocus: true,
                                oninput: move |e| name.set(e.value().clone()),
                                onkeydown: {
                                    let mut do_submit = do_submit.clone();
                                    move |e| {
                                        if e.key() == Key::Enter {
                                            do_submit(());
                                        }
                                    }
                                },
                            }
                        }

                        // Category field
                        div {
                            label { class: "block text-xs font-semibold text-zinc-400 uppercase tracking-wider mb-1.5",
                                "Category"
                            }
                            input {
                                class: "w-full px-4 py-2.5 text-sm bg-zinc-800/80 border {border_class} \
                                        rounded-lg text-zinc-200 placeholder:text-zinc-600 outline-none \
                                        {focus_border} transition-colors",
                                r#type: "text",
                                placeholder: "{kind.category_placeholder()}",
                                value: "{category}",
                                oninput: move |e| category.set(e.value().clone()),
                            }
                        }

                        // Tags field
                        div {
                            label { class: "block text-xs font-semibold text-zinc-400 uppercase tracking-wider mb-1.5",
                                "Tags"
                            }
                            input {
                                class: "w-full px-4 py-2.5 text-sm bg-zinc-800/80 border {border_class} \
                                        rounded-lg text-zinc-200 placeholder:text-zinc-600 outline-none \
                                        {focus_border} transition-colors",
                                r#type: "text",
                                placeholder: "Comma-separated, e.g. favorite, worship, sunday",
                                value: "{tags_input}",
                                oninput: move |e| tags_input.set(e.value().clone()),
                            }
                            {
                                let tags: Vec<String> = tags_input()
                                    .split(',')
                                    .map(|t| t.trim().to_string())
                                    .filter(|t| !t.is_empty())
                                    .collect();
                                if !tags.is_empty() {
                                    rsx! {
                                        div { class: "flex flex-wrap gap-1.5 mt-2",
                                            for tag in tags.iter() {
                                                span {
                                                    key: "{tag}",
                                                    class: "text-xs px-2 py-0.5 rounded-full bg-zinc-700/80 text-zinc-300",
                                                    "{tag}"
                                                }
                                            }
                                        }
                                    }
                                } else {
                                    rsx! {}
                                }
                            }
                        }

                        // Description field
                        div {
                            label { class: "block text-xs font-semibold text-zinc-400 uppercase tracking-wider mb-1.5",
                                "Description"
                            }
                            textarea {
                                class: "w-full px-4 py-2.5 text-sm bg-zinc-800/80 border {border_class} \
                                        rounded-lg text-zinc-200 placeholder:text-zinc-600 outline-none \
                                        {focus_border} resize-none transition-colors",
                                rows: "3",
                                placeholder: "Optional description...",
                                value: "{description}",
                                oninput: move |e| description.set(e.value().clone()),
                            }
                        }
                    }

                    // Right column: grid preview (Preset only)
                    if is_preset {
                        div { class: "flex-1 flex flex-col overflow-hidden min-w-0",
                            // Preview header
                            div { class: "px-4 py-3 border-b border-zinc-800 flex-shrink-0",
                                span { class: "text-xs font-semibold text-zinc-400 uppercase tracking-wider",
                                    "Preview"
                                }
                            }

                            if preview_slots.is_empty() {
                                // Blank — empty state
                                div { class: "flex-1 flex items-center justify-center",
                                    div { class: "text-center",
                                        div { class: "text-3xl mb-2 opacity-30", "\u{2795}" }
                                        p { class: "text-sm text-zinc-500", "Blank preset" }
                                        p { class: "text-xs text-zinc-600 mt-1",
                                            "Start from scratch with an empty grid"
                                        }
                                    }
                                }
                            } else {
                                // Pan/zoomable grid preview
                                div { class: "flex-1 overflow-hidden min-h-0",
                                    DynamicGridView {
                                        chain: preview_slots,
                                        selection: None,
                                        connections: Vec::<GridConnection>::new(),
                                        on_chain_change: move |_: Vec<CompositionSlot>| {},
                                        on_connections_change: move |_: Vec<GridConnection>| {},
                                        on_select: move |_: Option<GridSelection>| {},
                                    }
                                }
                            }
                        }
                    }
                }

                // Footer
                div { class: "flex items-center justify-end gap-3 px-6 py-4 border-t border-zinc-800",
                    button {
                        class: "px-4 py-2 text-sm font-medium rounded-lg \
                                text-zinc-400 hover:text-zinc-200 hover:bg-zinc-800 transition-colors",
                        onclick: move |_| props.on_close.call(()),
                        "Cancel"
                    }
                    button {
                        class: "px-5 py-2 text-sm font-medium rounded-lg text-white \
                                {btn_class} transition-colors",
                        disabled: !can_submit,
                        onclick: {
                            let mut do_submit = do_submit.clone();
                            move |_| do_submit(())
                        },
                        "Create"
                    }
                }
            }
        }
    }
}
