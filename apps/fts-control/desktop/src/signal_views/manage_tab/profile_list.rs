use dioxus::prelude::*;
use input_dioxus::TEXT_INPUT_FOCUS_COUNT;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use super::EditTarget;
use crate::signal_views::ManageProfileItem;

/// Renders the expandable profile list with patches.
/// Used in both Profile mode (right panel) and Song mode (right panel).
/// `on_patch_click` is called with (profile_id, patch_id) when a patch is clicked.
pub(crate) fn render_profile_list(
    manage_profiles: Signal<Vec<ManageProfileItem>>,
    mut expanded_profile_ids: Signal<HashSet<String>>,
    mut selected_profile: Signal<Option<String>>,
    mut selected_patch: Signal<Option<String>>,
    on_patch_click: Option<Rc<dyn Fn(String, String)>>,
    patch_display_labels: Signal<HashMap<String, String>>,
    signal: signal::Signal,
    mut on_patch_created: impl FnMut() + 'static + Clone,
    mut editing_target: Signal<Option<EditTarget>>,
    mut editing_text: Signal<String>,
    commit_rename: Rc<dyn Fn()>,
    mut ctx_target: Signal<Option<EditTarget>>,
    mut ctx_pos: Signal<(f64, f64)>,
) -> Element {
    rsx! {
        for prof in manage_profiles().iter().cloned() {
            {
                let is_sel = selected_profile().as_deref() == Some(prof.id.as_str());
                let is_expanded = expanded_profile_ids().contains(&prof.id);
                let prof_key = prof.id.clone();
                let prof_click_id = prof.id.clone();
                let ctx_prof_target = EditTarget::Profile { id: prof.id.clone() };
                rsx! {
                    div { key: "{prof_key}",
                        button {
                            class: if is_sel {
                                "w-full text-left px-3 py-2 border-b border-zinc-800/50 bg-zinc-700/60"
                            } else {
                                "w-full text-left px-3 py-2 border-b border-zinc-800/50 hover:bg-zinc-800/60"
                            },
                            oncontextmenu: move |e: Event<MouseData>| {
                                e.prevent_default();
                                ctx_target.set(Some(ctx_prof_target.clone()));
                                ctx_pos.set((e.page_coordinates().x, e.page_coordinates().y));
                            },
                            onclick: move |_| {
                                let mut exp = expanded_profile_ids();
                                if exp.contains(&prof_click_id) {
                                    exp.remove(&prof_click_id);
                                } else {
                                    exp.insert(prof_click_id.clone());
                                }
                                expanded_profile_ids.set(exp);
                                selected_profile.set(Some(prof_click_id.clone()));
                            },
                            div { class: "flex items-center gap-1.5",
                                span { class: "text-[10px] text-zinc-500 w-3 flex-shrink-0",
                                    if is_expanded { "\u{25BE}" } else { "\u{25B8}" }
                                }
                                {
                                    let edit_target = EditTarget::Profile { id: prof.id.clone() };
                                    let is_editing = editing_target() == Some(edit_target.clone());
                                    let prof_name = prof.name.clone();
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
                                                    editing_text.set(prof_name.clone());
                                                },
                                                "{prof.name}"
                                            }
                                        }
                                    }
                                }
                                span { class: "text-[10px] text-zinc-500 flex-shrink-0",
                                    "{prof.patches.len()}"
                                }
                            }
                        }
                        if is_expanded {
                            for (patch_id, patch_name) in prof.patches.iter() {
                                {
                                    let is_patch_sel = selected_patch().as_deref() == Some(patch_id.as_str());
                                    let pid = patch_id.clone();
                                    let prof_id_for_cb = prof.id.clone();
                                    let cb = on_patch_click.clone();
                                    let label = patch_display_labels().get(patch_id).cloned();
                                    {
                                        let edit_target = EditTarget::Patch {
                                            profile_id: prof.id.clone(),
                                            patch_id: patch_id.clone(),
                                        };
                                        let is_editing = editing_target() == Some(edit_target.clone());
                                        let patch_name_clone = patch_name.clone();
                                        let commit = commit_rename.clone();
                                        let commit_blur = commit.clone();
                                        let ctx_patch_target = edit_target.clone();
                                        rsx! {
                                            button {
                                                key: "{patch_id}",
                                                class: if is_patch_sel {
                                                    "w-full text-left pl-8 pr-3 py-1.5 bg-zinc-700/40 text-zinc-200 border-b border-zinc-800/30"
                                                } else {
                                                    "w-full text-left pl-8 pr-3 py-1.5 text-zinc-400 hover:bg-zinc-800/40 hover:text-zinc-300 border-b border-zinc-800/30"
                                                },
                                                oncontextmenu: move |e: Event<MouseData>| {
                                                    e.prevent_default();
                                                    ctx_target.set(Some(ctx_patch_target.clone()));
                                                    ctx_pos.set((e.page_coordinates().x, e.page_coordinates().y));
                                                },
                                                onclick: move |_| {
                                                    selected_patch.set(Some(pid.clone()));
                                                    if let Some(ref cb) = cb {
                                                        cb(prof_id_for_cb.clone(), pid.clone());
                                                    }
                                                },
                                                div { class: "flex flex-col gap-0.5",
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
                                                            class: "text-xs truncate",
                                                            ondoubleclick: move |_| {
                                                                editing_target.set(Some(edit_target.clone()));
                                                                editing_text.set(patch_name_clone.clone());
                                                            },
                                                            "{patch_name}"
                                                        }
                                                    }
                                                    if let Some(lbl) = &label {
                                                        span { class: "text-[9px] text-zinc-500 truncate", "{lbl}" }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            // + Patch button
                            {
                                let prof_id = prof.id.clone();
                                let signal = signal.clone();
                                let mut patch_cb = on_patch_created.clone();
                                rsx! {
                                    button {
                                        class: "w-full text-left pl-8 pr-3 py-1 text-[10px] text-zinc-500 hover:text-zinc-300 hover:bg-zinc-800/40 border-b border-zinc-800/30",
                                        onclick: move |_| {
                                            let signal = signal.clone();
                                            let prof_id = prof_id.clone();
                                            let mut cb = patch_cb.clone();
                                            spawn(async move {
                                                let rigs = signal.rigs().list().await.unwrap_or_default();
                                                if let Some(r) = rigs.first() {
                                                    if let Some(s) = r.variants.first() {
                                                        use signal::profile::{Patch, PatchId};
                                                        let patch = Patch::from_rig_scene(
                                                            PatchId::new(),
                                                            "New Patch",
                                                            r.id.clone(),
                                                            s.id.clone(),
                                                        );
                                                        let _ = signal.profiles().add_patch(prof_id, patch).await;
                                                        cb();
                                                    }
                                                }
                                            });
                                        },
                                        "+ Patch"
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
