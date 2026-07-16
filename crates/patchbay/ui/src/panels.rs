//! Side panel: presets, inspector (aliases), clock + Dante controls.

use dioxus::prelude::*;
use patchbay_proto::MediaKind;

use crate::state::{
    self, ALIASES, CLOCK, DANTE, GRAPH, HIDE_UNCONNECTED, KIND_FILTER, LAST_REPORT, PRESETS,
    SEARCH, SELECTED_NODE,
};

#[component]
pub fn Toolbar() -> Element {
    let search = SEARCH.read().clone();
    let kinds = KIND_FILTER.read().clone();
    let hide = *HIDE_UNCONNECTED.read();

    let kind_chip = |kind: MediaKind, label: &'static str| {
        let on = kinds.contains(&kind);
        rsx! {
            button {
                class: if on { "chip on" } else { "chip" },
                onclick: move |_| {
                    let mut k = KIND_FILTER.write();
                    if k.contains(&kind) {
                        k.retain(|x| *x != kind);
                    } else {
                        k.push(kind);
                    }
                },
                "{label}"
            }
        }
    };

    rsx! {
        div { class: "toolbar",
            input {
                class: "search",
                placeholder: "filter nodes…",
                value: "{search}",
                oninput: move |e| *SEARCH.write() = e.value(),
            }
            {kind_chip(MediaKind::Audio, "audio")}
            {kind_chip(MediaKind::Midi, "midi")}
            {kind_chip(MediaKind::Video, "video")}
            {kind_chip(MediaKind::Other, "other")}
            button {
                class: if hide { "chip on" } else { "chip" },
                onclick: move |_| {
                    let cur = *HIDE_UNCONNECTED.peek();
                    *HIDE_UNCONNECTED.write() = !cur;
                },
                "connected only"
            }
        }
    }
}

#[component]
pub fn StatusBar() -> Element {
    let clock = CLOCK.read().clone();
    let dante = DANTE.read().clone();
    let handle = state::use_patchbay();
    let ms = if clock.rate > 0 {
        clock.quantum as f64 / clock.rate as f64 * 1000.0
    } else {
        0.0
    };
    let forced = clock.force_quantum != 0;

    let quantum_btn = |frames: u32| {
        let handle = handle.clone();
        let active = if frames == 0 {
            !forced
        } else {
            clock.force_quantum == frames || (!forced && clock.quantum == frames && false)
        };
        let label = if frames == 0 {
            "auto".to_string()
        } else {
            frames.to_string()
        };
        rsx! {
            button {
                class: if active { "chip on" } else { "chip" },
                onclick: move |_| {
                    let handle = handle.clone();
                    spawn(async move {
                        if let Err(e) = handle.0.force_quantum(frames).await {
                            tracing::warn!("force_quantum failed: {e:?}");
                        }
                        state::refresh_meta(&handle).await;
                    });
                },
                "{label}"
            }
        }
    };

    let dante_handle = handle.clone();
    let dante_on = dante.active;

    rsx! {
        div { class: "statusbar",
            span { class: "clock-info",
                "{clock.rate} Hz · {clock.quantum} frames · {ms:.2} ms"
                if forced { span { class: "forced-tag", " (forced)" } }
            }
            span { class: "spacer" }
            span { class: "label", "quantum:" }
            {quantum_btn(0)}
            {quantum_btn(64)}
            {quantum_btn(128)}
            {quantum_btn(256)}
            {quantum_btn(512)}
            {quantum_btn(1024)}
            span { class: "spacer" }
            if dante.installed {
                span {
                    class: if dante_on { "dante-dot on" } else { "dante-dot" },
                }
                span { class: "label", "Dante" }
                button {
                    class: "chip",
                    onclick: move |_| {
                        let handle = dante_handle.clone();
                        spawn(async move {
                            if let Err(e) = handle.0.set_dante(!dante_on).await {
                                tracing::warn!("dante toggle failed: {e:?}");
                            }
                            state::refresh_meta(&handle).await;
                        });
                    },
                    if dante_on { "stop" } else { "start" }
                }
            }
        }
    }
}

#[component]
pub fn SidePanel() -> Element {
    rsx! {
        div { class: "side-panel",
            PresetsPanel {}
            Inspector {}
        }
    }
}

#[component]
fn PresetsPanel() -> Element {
    let presets = PRESETS.read().clone();
    let handle = state::use_patchbay();
    let mut new_name = use_signal(String::new);
    let report = LAST_REPORT.read().clone();

    rsx! {
        div { class: "panel-section",
            h3 { "Presets" }
            div { class: "preset-save",
                input {
                    placeholder: "preset name…",
                    value: "{new_name}",
                    oninput: move |e| new_name.set(e.value()),
                }
                button {
                    class: "chip",
                    onclick: {
                        let handle = handle.clone();
                        move |_| {
                            let name = new_name.peek().trim().to_string();
                            if name.is_empty() {
                                return;
                            }
                            let handle = handle.clone();
                            spawn(async move {
                                match handle.0.save_preset(name, String::new()).await {
                                    Ok(_) => state::refresh_meta(&handle).await,
                                    Err(e) => tracing::warn!("save_preset failed: {e:?}"),
                                }
                            });
                            new_name.set(String::new());
                        }
                    },
                    "save current"
                }
            }
            for preset in presets {
                {
                    let name = preset.name.clone();
                    let links = preset.links.len();
                    let apply = |exclusive: bool| {
                        let handle = handle.clone();
                        let name = name.clone();
                        move |_| {
                            let handle = handle.clone();
                            let name = name.clone();
                            spawn(async move {
                                match handle.0.apply_preset(name.clone(), exclusive).await {
                                    Ok(r) => *LAST_REPORT.write() = Some((name, r)),
                                    Err(e) => tracing::warn!("apply_preset failed: {e:?}"),
                                }
                            });
                        }
                    };
                    let del = {
                        let handle = handle.clone();
                        let name = name.clone();
                        move |_| {
                            let handle = handle.clone();
                            let name = name.clone();
                            spawn(async move {
                                if let Err(e) = handle.0.delete_preset(name).await {
                                    tracing::warn!("delete_preset failed: {e:?}");
                                }
                                state::refresh_meta(&handle).await;
                            });
                        }
                    };
                    rsx! {
                        div { class: "preset-row", key: "{preset.name}",
                            span { class: "preset-name", title: "{links} links", "{preset.name}" }
                            button { class: "chip", onclick: apply(false), "apply" }
                            button { class: "chip", title: "also remove links not in the preset",
                                onclick: apply(true), "restore" }
                            button { class: "chip danger", onclick: del, "✕" }
                        }
                    }
                }
            }
            if let Some((name, r)) = report {
                div { class: "apply-report",
                    "{name}: {r.created} created, {r.existing} kept, "
                    "{r.destroyed} removed, {r.missing.len()} missing"
                }
            }
        }
    }
}

#[component]
fn Inspector() -> Element {
    let Some(node_id) = *SELECTED_NODE.read() else {
        return rsx! {
            div { class: "panel-section dim", "Select a node to inspect / rename its channels." }
        };
    };
    let graph = GRAPH.read();
    let Some(node) = graph.nodes.iter().find(|n| n.id == node_id).cloned() else {
        return rsx! {
            div { class: "panel-section dim", "Node vanished." }
        };
    };
    // Numeric-aware sort so playback_10 follows playback_9.
    let mut ports: Vec<_> = graph
        .ports
        .iter()
        .filter(|p| p.node_id == node.id)
        .cloned()
        .collect();
    drop(graph);
    ports.sort_by_key(|p| {
        let digits = p.name.chars().rev().take_while(|c| c.is_ascii_digit()).count();
        if digits == 0 || digits == p.name.len() {
            (p.name.clone(), 0u64)
        } else {
            let (prefix, num) = p.name.split_at(p.name.len() - digits);
            (prefix.to_string(), num.parse().unwrap_or(0))
        }
    });

    let aliases = ALIASES.read();
    let node_alias = aliases.get(&node.name).cloned().unwrap_or_default();
    let port_aliases: Vec<(String, String)> = ports
        .iter()
        .map(|p| {
            let alias = aliases
                .get(&format!("{}:{}", node.name, p.name))
                .cloned()
                .unwrap_or_default();
            (p.name.clone(), alias)
        })
        .collect();
    drop(aliases);

    rsx! {
        div { class: "panel-section",
            h3 { "Inspector" }
            div { class: "inspect-line", span { class: "label", "name " } "{node.name}" }
            div { class: "inspect-line", span { class: "label", "class " } "{node.media_class}" }
            AliasEditor {
                key: "{node.name}",
                target: node.name.clone(),
                placeholder: "node display name…".to_string(),
                current: node_alias,
            }
            ChanmapSync { node_name: node.name.clone() }
            h3 { style: "margin-top:12px;", "Channels" }
            div { class: "channel-list",
                for (port_name, alias) in port_aliases {
                    div { class: "channel-row", key: "{node.name}:{port_name}",
                        span { class: "channel-port", title: "{port_name}", "{port_name}" }
                        AliasEditor {
                            target: format!("{}:{}", node.name, port_name),
                            placeholder: String::new(),
                            current: alias,
                        }
                    }
                }
            }
        }
    }
}

/// One alias input: commits on Enter or blur, empty clears.
#[component]
fn AliasEditor(target: String, placeholder: String, current: String) -> Element {
    let handle = state::use_patchbay();
    let mut draft = use_signal(|| current.clone());
    // Follow external changes (chanmap import, another editor).
    use_effect(use_reactive!(|current| draft.set(current)));

    let commit = {
        let target = target.clone();
        move || {
            let value = draft.peek().trim().to_string();
            if value == current {
                return;
            }
            let handle = handle.clone();
            let target = target.clone();
            spawn(async move {
                if let Err(e) = handle.0.set_alias(target, value).await {
                    tracing::warn!("set_alias failed: {e:?}");
                }
                state::refresh_meta(&handle).await;
            });
        }
    };
    let commit_blur = commit.clone();

    rsx! {
        input {
            class: "alias-input",
            placeholder: "{placeholder}",
            value: "{draft}",
            oninput: move |e| draft.set(e.value()),
            onkeydown: move |e| {
                if e.key() == Key::Enter {
                    commit();
                }
            },
            onblur: move |_| commit_blur(),
        }
    }
}

/// Import/export this node's channel names from/to the REAPER ChanMap
/// (empty path = the host's default chanmap).
#[component]
fn ChanmapSync(node_name: String) -> Element {
    let handle = state::use_patchbay();
    let mut path = use_signal(String::new);
    let mut result = use_signal(String::new);

    let run = |import: bool| {
        let handle = handle.clone();
        let node_name = node_name.clone();
        move |_| {
            let handle = handle.clone();
            let node_name = node_name.clone();
            let path = path.peek().clone();
            spawn(async move {
                let res = if import {
                    handle.0.import_chanmap(node_name, path).await
                } else {
                    handle.0.export_chanmap(node_name, path).await
                };
                match res {
                    Ok(n) => {
                        result.set(format!(
                            "{} {n} channel names",
                            if import { "imported" } else { "exported" }
                        ));
                        state::refresh_meta(&handle).await;
                    }
                    Err(e) => result.set(format!("chanmap failed: {e}")),
                }
            });
        }
    };

    rsx! {
        div { class: "chanmap-sync",
            input {
                class: "alias-input",
                placeholder: "chanmap path (empty = host default)",
                value: "{path}",
                oninput: move |e| path.set(e.value()),
            }
            div { class: "chanmap-buttons",
                button { class: "chip", title: "ChanMap names → channel aliases",
                    onclick: run(true), "import chanmap" }
                button { class: "chip", title: "channel aliases → ChanMap nameN lines",
                    onclick: run(false), "export chanmap" }
            }
            if !result.read().is_empty() {
                div { class: "apply-report", "{result}" }
            }
        }
    }
}
