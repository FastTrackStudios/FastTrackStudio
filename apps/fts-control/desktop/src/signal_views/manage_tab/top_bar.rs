use dioxus::prelude::*;
use fts_ui::prelude::*;
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
        div { class: "flex items-center gap-2 px-3 py-1.5 flex-shrink-0 border-b border-border bg-card/50",
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

            // Mode tabs
            SegmentedControl {
                value: match mode {
                    ManageMode::Song => String::from("song"),
                    ManageMode::Profile => String::from("profile"),
                    ManageMode::Preset => String::from("preset"),
                },
                on_change: move |v: String| {
                    let m = match v.as_str() {
                        "song" => ManageMode::Song,
                        "profile" => ManageMode::Profile,
                        "preset" => ManageMode::Preset,
                        _ => return,
                    };
                    manage_mode.set(m);
                },
                options: vec![
                    (String::from("song"), String::from("Song")),
                    (String::from("profile"), String::from("Profile")),
                    (String::from("preset"), String::from("Preset")),
                ],
            }

            // Rig type selector
            SegmentedControl {
                value: match rig_type() {
                    RigType::Guitar => String::from("guitar"),
                    RigType::Bass => String::from("bass"),
                    RigType::Keys => String::from("keys"),
                    RigType::Vocals => String::from("vocals"),
                    _ => String::from("guitar"),
                },
                on_change: move |v: String| {
                    let rt = match v.as_str() {
                        "guitar" => RigType::Guitar,
                        "bass" => RigType::Bass,
                        "keys" => RigType::Keys,
                        "vocals" => RigType::Vocals,
                        _ => return,
                    };
                    rig_type.set(rt);
                },
                options: vec![
                    (String::from("guitar"), String::from("Guitar")),
                    (String::from("bass"), String::from("Bass")),
                    (String::from("keys"), String::from("Keys")),
                    (String::from("vocals"), String::from("Vocals")),
                ],
                size: SegmentedControlSize::Small,
            }

            // Spacer pushes scene tabs to the right
            div { class: "flex-1" }

            // Scene tabs (only shown for rig presets)
            if is_rig_active && !scenes.is_empty() {
                span { class: "text-[10px] text-muted-foreground mr-1 flex-shrink-0", "Scenes" }
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
                                    class: "px-2 py-0.5 text-xs text-foreground rounded outline-none w-20 bg-secondary border border-border",
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
                                        "px-2.5 py-1 text-xs rounded font-medium bg-accent text-accent-foreground"
                                    } else {
                                        "px-2.5 py-1 text-xs rounded text-muted-foreground hover:bg-accent/50"
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
                            class: "px-2 py-0.5 text-[10px] text-muted-foreground rounded hover:bg-accent/30",
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
