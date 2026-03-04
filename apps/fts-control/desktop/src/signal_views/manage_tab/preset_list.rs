use dioxus::prelude::*;
use input_dioxus::TEXT_INPUT_FOCUS_COUNT;
use std::collections::HashSet;
use std::rc::Rc;

use super::EditTarget;
use crate::signal_views::ManagePresetItem;
use signal::rig::RigType;

/// Renders the left panel preset list with expandable parent rows and sub-items.
/// Callbacks for selection and assignment stay in the parent.
pub(crate) fn render_preset_list(
    manage_presets: Signal<Vec<ManagePresetItem>>,
    expanded_ids: Signal<HashSet<String>>,
    current_preset: &Option<String>,
    current_sub: &Option<String>,
    rig_type: Signal<RigType>,
    mut on_rig_created: impl FnMut() + 'static + Clone,
    on_create_rig: Rc<dyn Fn(RigType)>,
    mut editing_target: Signal<Option<EditTarget>>,
    mut editing_text: Signal<String>,
    commit_rename: Rc<dyn Fn()>,
    mut ctx_target: Signal<Option<EditTarget>>,
    mut ctx_pos: Signal<(f64, f64)>,
    mut select_preset: impl FnMut(String) + 'static + Clone,
    mut select_sub_item: impl FnMut(String, String, bool) + 'static + Clone,
    mut assign_current_section: impl FnMut(String, String, bool) + 'static + Clone,
) -> Element {
    rsx! {
            div { class: "flex-1 min-h-0 flex flex-col",
                div { class: "px-3 py-1.5 border-b border-border flex-shrink-0 flex items-center justify-between",
                    h3 { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider", "Presets" }
                    {
                        let mut cb = on_rig_created.clone();
                        rsx! {
                            signal_ui::components::CreateRigButton {
                                rig_type: rig_type(),
                                on_created: move |_| { cb(); },
                                on_create_rig: {
                                    let create = on_create_rig.clone();
                                    move |rt| create(rt)
                                },
                            }
                        }
                    }
                }
                div { class: "flex-1 overflow-y-auto",
                    for item in manage_presets().iter().cloned() {
                        {
                            let is_sel = current_preset.as_deref() == Some(item.id.as_str());
                            let is_expanded = expanded_ids().contains(&item.id);
                            let item_key = item.id.clone();
                            let item_click_id = item.id.clone();
                            let first_sub = item.sub_items.first().map(|(sid, _)| sid.clone());
                            let is_rig = item.is_rig;
                            let mut on_select = select_preset.clone();
                            let mut on_assign = assign_current_section.clone();
                            {
                                let ctx_edit_target = EditTarget::Rig { id: item.id.clone() };
                                rsx! {
                                    div { key: "{item_key}",
                                        button {
                                            class: if is_sel {
                                                "w-full text-left px-3 py-2 border-b border-zinc-800/50 bg-zinc-700/60"
                                            } else {
                                                "w-full text-left px-3 py-2 border-b border-zinc-800/50 hover:bg-zinc-800/60"
                                            },
                                            oncontextmenu: move |e: Event<MouseData>| {
                                                e.prevent_default();
                                                ctx_target.set(Some(ctx_edit_target.clone()));
                                                ctx_pos.set((e.page_coordinates().x, e.page_coordinates().y));
                                            },
                                            onclick: move |_| {
                                                on_select(item_click_id.clone());
                                                if let Some(ref sub_id) = first_sub {
                                                    on_assign(item_click_id.clone(), sub_id.clone(), is_rig);
                                                }
                                            },
                                            div { class: "flex items-center gap-1.5",
                                                span { class: "text-[10px] text-zinc-500 w-3 flex-shrink-0",
                                                    if is_expanded { "\u{25BE}" } else { "\u{25B8}" }
                                                }
                                                span {
                                                    class: if item.is_rig {
                                                        "text-[9px] px-1 rounded bg-zinc-600 text-zinc-300 flex-shrink-0"
                                                    } else {
                                                        "text-[9px] px-1 rounded bg-zinc-700 text-zinc-400 flex-shrink-0"
                                                    },
                                                    if item.is_rig { "RIG" } else { "LYR" }
                                                }
                                                {
                                                    let edit_target = EditTarget::Rig { id: item.id.clone() };
                                                    let is_editing = editing_target() == Some(edit_target.clone());
                                                    let item_name = item.name.clone();
                                                    let commit = commit_rename.clone();
                                                    let commit_blur = commit.clone();
                                                    rsx! {
                                                        if is_editing {
                                                            input {
                                                                class: "text-sm text-zinc-200 bg-zinc-800 border border-zinc-600 rounded px-1 flex-1 min-w-0 outline-none",
                                                                value: "{editing_text}",
                                                                autofocus: true,
                                                                oninput: move |e| editing_text.set(e.value()),
                                                                onkeydown: move |e: KeyboardEvent| {
                                                                    e.stop_propagation();
                                                                    if e.key() == Key::Enter { commit(); }
                                                                    if e.key() == Key::Escape { editing_target.set(None); }
                                                                },
                                                                onfocusin: move |_| { *TEXT_INPUT_FOCUS_COUNT.write() += 1; },
                                                                onfocusout: move |_| { *TEXT_INPUT_FOCUS_COUNT.write() -= 1; commit_blur(); },
                                                            }
                                                        } else {
                                                            span {
                                                                class: "text-sm text-zinc-200 truncate flex-1",
                                                                ondoubleclick: move |_| {
                                                                    editing_target.set(Some(edit_target.clone()));
                                                                    editing_text.set(item_name.clone());
                                                                },
                                                                "{item.name}"
                                                            }
                                                        }
                                                    }
                                                }
                                                span { class: "text-[10px] text-zinc-500 flex-shrink-0",
                                                    "{item.sub_items.len()}"
                                                }
                                            }
                                        }
                                        if is_expanded {
                                            for (sub_id, sub_name) in item.sub_items.iter() {
                                                {
                                                    let is_sub_sel = current_sub.as_deref() == Some(sub_id.as_str());
                                                    let parent_id = item.id.clone();
                                                    let sub_id_click = sub_id.clone();
                                                    let is_rig = item.is_rig;
                                                    let mut on_sub = select_sub_item.clone();
                                                    let mut on_assign = assign_current_section.clone();
                                                    let edit_target = EditTarget::RigScene {
                                                        rig_id: item.id.clone(),
                                                        scene_id: sub_id.clone(),
                                                    };
                                                    let is_editing = editing_target() == Some(edit_target.clone());
                                                    let sub_name_clone = sub_name.clone();
                                                    let commit = commit_rename.clone();
                                                    let commit_blur = commit.clone();
                                                    let ctx_sub_target = edit_target.clone();
                                                    rsx! {
                                                        button {
                                                            key: "{sub_id}",
                                                            class: if is_sub_sel {
                                                                "w-full text-left pl-8 pr-3 py-1.5 text-xs bg-zinc-700/40 text-zinc-200 border-b border-zinc-800/30"
                                                            } else {
                                                                "w-full text-left pl-8 pr-3 py-1.5 text-xs text-zinc-400 hover:bg-zinc-800/40 hover:text-zinc-300 border-b border-zinc-800/30"
                                                            },
                                                            oncontextmenu: move |e: Event<MouseData>| {
                                                                e.prevent_default();
                                                                ctx_target.set(Some(ctx_sub_target.clone()));
                                                                ctx_pos.set((e.page_coordinates().x, e.page_coordinates().y));
                                                            },
                                                            onclick: move |_| {
                                                                on_sub(parent_id.clone(), sub_id_click.clone(), is_rig);
                                                                on_assign(parent_id.clone(), sub_id_click.clone(), is_rig);
                                                            },
                                                            if is_editing {
                                                                input {
                                                                    class: "text-xs text-zinc-200 bg-zinc-800 border border-zinc-600 rounded px-1 w-full outline-none",
                                                                    value: "{editing_text}",
                                                                    autofocus: true,
                                                                    oninput: move |e| editing_text.set(e.value()),
                                                                    onkeydown: move |e: KeyboardEvent| {
                                                                        e.stop_propagation();
                                                                        if e.key() == Key::Enter { commit(); }
                                                                        if e.key() == Key::Escape { editing_target.set(None); }
                                                                    },
                                                                    onfocusin: move |_| { *TEXT_INPUT_FOCUS_COUNT.write() += 1; },
                                                                    onfocusout: move |_| { *TEXT_INPUT_FOCUS_COUNT.write() -= 1; commit_blur(); },
                                                                }
                                                            } else {
                                                                span {
                                                                    ondoubleclick: move |_| {
                                                                        editing_target.set(Some(edit_target.clone()));
                                                                        editing_text.set(sub_name_clone.clone());
                                                                    },
                                                                    "{sub_name}"
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
    }
}
