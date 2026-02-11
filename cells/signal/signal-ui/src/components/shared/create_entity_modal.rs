//! Create Entity Modal — shared dialog for creating presets, profiles, songs, scenes, etc.
//!
//! Renders an 80% viewport modal with form fields:
//! - Name (required)
//! - Category (contextual — e.g. "Amp", "Drive" for presets; "Worship", "Rock" for songs)
//! - Tags (comma-separated text input)
//! - Description (multi-line textarea)
//!
//! The modal adapts its field labels and placeholders based on the `EntityKind`.

use crate::hooks::rig_actions::CreateEntityData;
use crate::prelude::*;

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
    /// Display title for the modal header.
    pub fn title(&self) -> &'static str {
        match self {
            Self::Preset => "New Preset",
            Self::Profile => "New Profile",
            Self::Song => "New Song",
            Self::Scene => "New Scene",
            Self::Setlist => "New Setlist",
        }
    }

    /// Placeholder text for the name field.
    fn name_placeholder(&self) -> &'static str {
        match self {
            Self::Preset => "e.g. Clean Sparkle",
            Self::Profile => "e.g. Worship Set",
            Self::Song => "e.g. Amazing Grace",
            Self::Scene => "e.g. Intro",
            Self::Setlist => "e.g. Sunday Service",
        }
    }

    /// Placeholder text for the category field.
    fn category_placeholder(&self) -> &'static str {
        match self {
            Self::Preset => "e.g. Clean, Crunch, Lead, Ambient",
            Self::Profile => "e.g. Live, Studio, Practice",
            Self::Song => "e.g. Worship, Rock, Jazz",
            Self::Scene => "e.g. Verse, Chorus, Bridge",
            Self::Setlist => "e.g. Sunday, Rehearsal",
        }
    }

    /// Accent color class for the modal border and buttons.
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

/// Props for the create entity modal.
#[derive(Props, Clone, PartialEq)]
pub struct CreateEntityModalProps {
    /// What kind of entity to create.
    pub kind: EntityKind,
    /// Whether the modal is open.
    pub is_open: bool,
    /// Called when the user submits the form.
    pub on_submit: Callback<CreateEntityData>,
    /// Called when the user cancels / closes the modal.
    pub on_close: Callback<()>,
}

/// Full-screen modal dialog for creating a new entity.
///
/// Takes up ~80% of the viewport. Fields: name, category, tags, description.
/// Keyboard: Enter submits (when name is non-empty), Escape closes.
#[component]
pub fn CreateEntityModal(props: CreateEntityModalProps) -> Element {
    if !props.is_open {
        return rsx! {};
    }

    let kind = props.kind;
    let accent = kind.accent();

    let mut name = use_signal(String::new);
    let mut category = use_signal(String::new);
    let mut tags_input = use_signal(String::new);
    let mut description = use_signal(String::new);

    let name_val = name();
    let can_submit = !name_val.trim().is_empty();

    // Border/button accent classes
    let border_class = format!("border-{accent}-500/40");
    let focus_border =
        format!("focus:border-{accent}-500/60 focus:ring-1 focus:ring-{accent}-500/20");
    let btn_class = format!(
        "bg-{accent}-600 hover:bg-{accent}-500 disabled:opacity-30 disabled:cursor-not-allowed"
    );

    let on_close_inner = props.on_close.clone();

    // Helper: collect form data and fire submit callback, then reset fields.
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
            on_submit.call(CreateEntityData {
                name: name().trim().to_string(),
                category: category().trim().to_string(),
                description: description().trim().to_string(),
                tags,
            });
            name.set(String::new());
            category.set(String::new());
            tags_input.set(String::new());
            description.set(String::new());
        }
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

            // Modal card — 80% viewport
            div {
                class: "w-[80vw] max-w-3xl max-h-[80vh] flex flex-col \
                        bg-zinc-900 border border-zinc-700/60 rounded-xl shadow-2xl shadow-black/40 \
                        overflow-hidden",
                onclick: |e| e.stop_propagation(),

                // ── Header ───────────────────────────────────────────
                div { class: "flex items-center justify-between px-6 py-4 border-b border-zinc-800",
                    h2 { class: "text-lg font-semibold text-zinc-100",
                        "{kind.title()}"
                    }
                    button {
                        class: "w-8 h-8 flex items-center justify-center rounded-lg \
                                text-zinc-500 hover:text-zinc-200 hover:bg-zinc-800 transition-colors",
                        onclick: move |_| props.on_close.call(()),
                        // X icon
                        svg {
                            xmlns: "http://www.w3.org/2000/svg",
                            width: "18",
                            height: "18",
                            view_box: "0 0 24 24",
                            fill: "none",
                            stroke: "currentColor",
                            stroke_width: "2",
                            stroke_linecap: "round",
                            stroke_linejoin: "round",
                            line { x1: "18", y1: "6", x2: "6", y2: "18" }
                            line { x1: "6", y1: "6", x2: "18", y2: "18" }
                        }
                    }
                }

                // ── Body (scrollable) ────────────────────────────────
                div { class: "flex-1 overflow-y-auto px-6 py-5 space-y-5",
                    // Name field (required)
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

                    // Tags field (comma-separated)
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
                        // Tag preview pills
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

                    // Description field (textarea)
                    div {
                        label { class: "block text-xs font-semibold text-zinc-400 uppercase tracking-wider mb-1.5",
                            "Description"
                        }
                        textarea {
                            class: "w-full px-4 py-2.5 text-sm bg-zinc-800/80 border {border_class} \
                                    rounded-lg text-zinc-200 placeholder:text-zinc-600 outline-none \
                                    {focus_border} resize-none transition-colors",
                            rows: "4",
                            placeholder: "Optional description...",
                            value: "{description}",
                            oninput: move |e| description.set(e.value().clone()),
                        }
                    }
                }

                // ── Footer ───────────────────────────────────────────
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
