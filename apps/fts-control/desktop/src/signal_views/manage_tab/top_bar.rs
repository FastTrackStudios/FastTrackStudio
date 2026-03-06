use dioxus::prelude::*;
use input_dioxus::TEXT_INPUT_FOCUS_COUNT;
use std::rc::Rc;

use super::{EditTarget, ManageMode};
use signal::rig::RigType;

/// Renders the top bar: Capture + Mode tabs + Rig type selector + Scene tabs.
/// Change 6: controls are grouped into segmented pill containers.
pub(crate) fn render_top_bar(
    mode: ManageMode,
    mut manage_mode: Signal<ManageMode>,
    mut rig_type: Signal<RigType>,
    rig_id: Signal<Option<String>>,
    current_scene: &Option<String>,
    scenes: &[(String, String)],
    is_rig_active: bool,
    mut on_scene_created: impl FnMut() + 'static + Clone,
    mut editing_target: Signal<Option<EditTarget>>,
    mut editing_text: Signal<String>,
    commit_rename: Rc<dyn Fn()>,
    mut select_sub_item: impl FnMut(String, String, bool) + 'static + Clone,
    mut assign_current_section: impl FnMut(String, String, bool) + 'static + Clone,
    signal: signal::Signal,
) -> Element {
    rsx! {
        div { class: "flex items-center gap-2 px-3 py-1.5 border-b border-white/[0.06] bg-white/[0.02] flex-shrink-0",
            // Capture preset button
            signal_ui::components::CaptureButton {
                on_capture: move |_| {
                    spawn(async move {
                        if let Some(applier) = crate::daw_registry::signal_reaper_applier() {
                            match applier.capture_current_patch().await {
                                Ok(Some(bytes)) => {
                                    tracing::info!("Captured {} bytes of rfxchain data", bytes.len());
                                }
                                Ok(None) => tracing::warn!("No active patch to capture"),
                                Err(e) => tracing::error!("Capture failed: {e:?}"),
                            }
                        } else {
                            tracing::warn!("No REAPER connection for capture");
                        }
                    });
                }
            }

            // Mode tabs — grouped pill
            div { class: "flex items-center gap-0.5 bg-white/[0.04] rounded px-0.5 py-0.5",
                for &(m, label) in &[(ManageMode::Song, "Song"), (ManageMode::Profile, "Profile"), (ManageMode::Preset, "Preset")] {
                    {
                        let is_active = mode == m;
                        rsx! {
                            button {
                                key: "{label}",
                                class: if is_active {
                                    "px-2.5 py-1 text-xs font-semibold rounded bg-white/[0.15] text-zinc-100"
                                } else {
                                    "px-2.5 py-1 text-xs text-zinc-400 hover:text-zinc-200 hover:bg-white/[0.10] rounded"
                                },
                                onclick: move |_| manage_mode.set(m),
                                "{label}"
                            }
                        }
                    }
                }
            }

            // Rig type selector — grouped pill
            div { class: "flex items-center gap-0.5 bg-white/[0.04] rounded px-0.5 py-0.5",
                for &rt in &[RigType::Guitar, RigType::Bass, RigType::Keys, RigType::Vocals] {
                    {
                        let is_active = rig_type() == rt;
                        let label = match rt {
                            RigType::Guitar => "Guitar",
                            RigType::Bass => "Bass",
                            RigType::Keys => "Keys",
                            RigType::Vocals => "Vocals",
                            _ => "Other",
                        };
                        rsx! {
                            button {
                                key: "{label}",
                                class: if is_active {
                                    "px-2 py-0.5 text-[11px] font-medium rounded bg-white/[0.15] text-zinc-100"
                                } else {
                                    "px-2 py-0.5 text-[11px] text-zinc-500 hover:text-zinc-300 hover:bg-white/[0.10] rounded"
                                },
                                onclick: move |_| rig_type.set(rt),
                                "{label}"
                            }
                        }
                    }
                }
            }

            // Spacer pushes scene tabs to the right
            div { class: "flex-1" }

            // Scene tabs (only shown for rig presets)
            if is_rig_active && !scenes.is_empty() {
                span { class: "text-[10px] text-zinc-500 mr-1 flex-shrink-0", "Scenes" }
                for (sid, sname) in scenes.iter() {
                    {
                        let is_active = current_scene.as_deref() == Some(sid.as_str());
                        let scene_id = sid.clone();
                        let rid = rig_id().unwrap_or_default();
                        let mut on_click = select_sub_item.clone();
                        let mut on_assign = assign_current_section.clone();
                        let edit_target = EditTarget::RigScene {
                            rig_id: rig_id().unwrap_or_default(),
                            scene_id: sid.clone(),
                        };
                        let is_editing = editing_target() == Some(edit_target.clone());
                        let scene_name = sname.clone();
                        let commit = commit_rename.clone();
                        let commit_blur = commit.clone();
                        rsx! {
                            if is_editing {
                                input {
                                    key: "{sid}",
                                    class: "px-2 py-0.5 text-xs text-zinc-200 bg-white/[0.06] border border-white/[0.08] rounded outline-none w-20",
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
                                button {
                                    key: "{sid}",
                                    class: if is_active {
                                        "px-2.5 py-1 text-xs font-medium rounded bg-white/[0.10] text-zinc-100"
                                    } else {
                                        "px-2.5 py-1 text-xs text-zinc-400 hover:text-zinc-200 hover:bg-white/[0.06] rounded"
                                    },
                                    onclick: move |_| {
                                        on_click(rid.clone(), scene_id.clone(), true);
                                        on_assign(rid.clone(), scene_id.clone(), true);
                                    },
                                    ondoubleclick: move |_| {
                                        editing_target.set(Some(edit_target.clone()));
                                        editing_text.set(scene_name.clone());
                                    },
                                    "{sname}"
                                }
                            }
                        }
                    }
                }
                // + Scene button
                {
                    let signal = signal.clone();
                    let mut cb = on_scene_created.clone();
                    rsx! {
                        button {
                            class: "px-2 py-0.5 text-[10px] text-zinc-500 hover:text-zinc-300 hover:bg-white/[0.06] rounded",
                            onclick: move |_| {
                                let signal = signal.clone();
                                let mut cb = cb.clone();
                                if let Some(rid) = rig_id() {
                                    spawn(async move {
                                        use signal::rig::{RigScene, RigSceneId};
                                        let scene = RigScene::new(RigSceneId::new(), "New Scene");
                                        let _ = signal.rigs().add_scene(rid, scene).await;
                                        cb();
                                    });
                                }
                            },
                            "+"
                        }
                    }
                }
            }
        }
    }
}
