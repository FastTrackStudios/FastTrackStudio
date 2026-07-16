//! Side panel: presets, inspector (aliases), clock + Dante controls.

use dioxus::prelude::*;
use patchbay_proto::MediaKind;

use crate::state::{
    self, ALIASES, CLOCK, DANTE, GRAPH, HIDE_UNCONNECTED, LAST_REPORT, MEDIA_TAB, PRESETS,
    SEARCH, SELECTED_NODE,
};

#[component]
pub fn Toolbar() -> Element {
    let search = SEARCH.read().clone();
    let tab = *MEDIA_TAB.read();
    let hide = *HIDE_UNCONNECTED.read();

    let media_tab = |kind: MediaKind, label: &'static str| {
        rsx! {
            button {
                class: if tab == kind { "tab on" } else { "tab" },
                onclick: move |_| *MEDIA_TAB.write() = kind,
                "{label}"
            }
        }
    };

    rsx! {
        div { class: "toolbar",
            div { class: "view-tabs",
                {media_tab(MediaKind::Audio, "Audio")}
                {media_tab(MediaKind::Midi, "MIDI")}
                {media_tab(MediaKind::Video, "Video")}
            }
            input {
                class: "search",
                placeholder: "filter nodes…",
                value: "{search}",
                oninput: move |e| *SEARCH.write() = e.value(),
            }
            button {
                class: if hide { "chip on" } else { "chip" },
                onclick: move |_| {
                    let cur = *HIDE_UNCONNECTED.peek();
                    *HIDE_UNCONNECTED.write() = !cur;
                },
                "connected only"
            }
            button {
                class: if *state::HIDE_MONITORS.read() { "chip on" } else { "chip" },
                title: "hide sinks' monitor taps (dimmed rows)",
                onclick: move |_| {
                    let cur = *state::HIDE_MONITORS.peek();
                    *state::HIDE_MONITORS.write() = !cur;
                },
                "hide monitors"
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
            {quantum_btn(32)}
            {quantum_btn(64)}
            {quantum_btn(128)}
            {quantum_btn(256)}
            {quantum_btn(512)}
            {quantum_btn(1024)}
            {quantum_btn(2048)}
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
            ServicesPanel {}
            LatencyPanel {}
            PresetsPanel {}
            Inspector {}
        }
    }
}

/// Per-app latency rules: which apps request/pin which quantum. The
/// graph runs at the lowest request among RUNNING apps, so REAPER at 64
/// only costs you 64 while REAPER is open — everything idles back to
/// the 1024 default after. Rules apply when a node is created: restart
/// the app, or hit apply (WirePlumber restart, brief blip).
#[component]
fn LatencyPanel() -> Element {
    let handle = state::use_patchbay();
    let rules = state::LATENCY_RULES.read().clone();

    let remove = |pattern: String| {
        let handle = handle.clone();
        move |_| {
            let handle = handle.clone();
            let pattern = pattern.clone();
            spawn(async move {
                if let Err(e) = handle.0.remove_latency_rule(pattern).await {
                    tracing::warn!("remove_latency_rule failed: {e}");
                }
                state::refresh_meta(&handle).await;
            });
        }
    };
    let apply = {
        let handle = handle.clone();
        move |_| {
            let handle = handle.clone();
            spawn(async move {
                if let Err(e) = handle
                    .0
                    .service_action(
                        "wireplumber.service".into(),
                        patchbay_proto::ServiceAction::Restart,
                    )
                    .await
                {
                    tracing::warn!("wireplumber restart failed: {e}");
                }
            });
        }
    };

    rsx! {
        div { class: "panel-section",
            h3 { "App latency" }
            if rules.is_empty() {
                div { class: "dim-note",
                    "No per-app rules. Select a node and set its quantum in the inspector — "
                    "the graph follows the lowest request among running apps."
                }
            }
            for rule in rules {
                div { class: "service-row", key: "{rule.pattern}",
                    span { class: "service-name", title: "{rule.pattern}",
                        "{rule.pattern}"
                    }
                    span { class: "rule-quantum",
                        "{rule.quantum}"
                        if rule.force { span { class: "forced-tag", " pin" } }
                    }
                    button { class: "chip danger", onclick: remove(rule.pattern.clone()), "✕" }
                }
            }
            button {
                class: "chip",
                style: "margin-top:6px;",
                title: "restart WirePlumber so rules hit already-running apps (brief audio blip)",
                onclick: apply,
                "apply now (restart WirePlumber)"
            }
            ClockDefaultsEditor {}
        }
    }
}

/// Runtime clock defaults — the patchbay-owned override of the flake's
/// 50-quantum.conf (idle/default quantum + the min/max clamp). Applies
/// on PipeWire restart.
#[component]
fn ClockDefaultsEditor() -> Element {
    let handle = state::use_patchbay();
    let stored = *state::CLOCK_DEFAULTS.read();
    let mut quantum = use_signal(|| stored.quantum);
    let mut min_q = use_signal(|| stored.min_quantum);
    let mut max_q = use_signal(|| stored.max_quantum);
    use_effect(use_reactive!(|stored| {
        quantum.set(stored.quantum);
        min_q.set(stored.min_quantum);
        max_q.set(stored.max_quantum);
    }));

    let save = {
        let handle = handle.clone();
        move |_| {
            let handle = handle.clone();
            let defaults = patchbay_proto::ClockDefaults {
                quantum: *quantum.peek(),
                min_quantum: *min_q.peek(),
                max_quantum: *max_q.peek(),
            };
            spawn(async move {
                if let Err(e) = handle.0.set_clock_defaults(defaults).await {
                    tracing::warn!("set_clock_defaults failed: {e}");
                }
                state::refresh_meta(&handle).await;
            });
        }
    };

    let num_input = |label: &'static str, mut sig: Signal<u32>| {
        rsx! {
            label { class: "clock-field",
                span { class: "label", "{label}" }
                input {
                    r#type: "number",
                    class: "alias-input",
                    value: "{sig}",
                    oninput: move |e| sig.set(e.value().parse().unwrap_or(0)),
                }
            }
        }
    };

    rsx! {
        div { class: "clock-defaults",
            h3 { style: "margin-top:12px;", "Clock defaults" }
            div { class: "dim-note",
                "Idle/default quantum + clamp — overrides the flake's 50-quantum.conf "
                "(0 = don't set). Restart PipeWire (Services) to apply."
            }
            div { class: "clock-fields",
                {num_input("default", quantum)}
                {num_input("min", min_q)}
                {num_input("max", max_q)}
            }
            button { class: "chip", onclick: save, "save defaults" }
        }
    }
}

/// Rig health: the managed systemd units (PipeWire, WirePlumber, PTP
/// clock, Inferno nodes, routing links…) with restart controls. When
/// PipeWire itself is down the engine reconnects on its own once it's
/// restarted from here.
#[component]
fn ServicesPanel() -> Element {
    let handle = state::use_patchbay();

    // Poll every 5 s so a crashed unit shows up without user action.
    use_future({
        let handle = handle.clone();
        move || {
            let handle = handle.clone();
            async move {
                loop {
                    if let Ok(services) = handle.0.services().await {
                        *state::SERVICES.write() = services;
                    }
                    state::sleep_secs(5).await;
                }
            }
        }
    });

    let services = state::SERVICES.read().clone();
    let act = |unit: String, action: patchbay_proto::ServiceAction| {
        let handle = handle.clone();
        move |_| {
            let handle = handle.clone();
            let unit = unit.clone();
            spawn(async move {
                if let Err(e) = handle.0.service_action(unit.clone(), action).await {
                    tracing::warn!("service {action:?} {unit} failed: {e}");
                }
                if let Ok(services) = handle.0.services().await {
                    *state::SERVICES.write() = services;
                }
            });
        }
    };

    rsx! {
        div { class: "panel-section",
            h3 { "Services" }
            for svc in services {
                {
                    let dot = match (svc.present, svc.state.as_str()) {
                        (false, _) => "svc-dot missing",
                        (_, "active") => "svc-dot on",
                        (_, "failed") => "svc-dot failed",
                        (_, "activating" | "deactivating" | "reloading") => "svc-dot busy",
                        _ => "svc-dot",
                    };
                    let running = svc.state == "active";
                    let unit = svc.unit.clone();
                    rsx! {
                        div { class: "service-row", key: "{svc.unit}",
                            span { class: "{dot}" }
                            span { class: "service-name", title: "{svc.unit} — {svc.state}/{svc.sub_state}",
                                "{svc.label}"
                            }
                            if svc.present {
                                if running {
                                    button { class: "chip", title: "restart {svc.unit}",
                                        onclick: act(unit.clone(), patchbay_proto::ServiceAction::Restart),
                                        "↻"
                                    }
                                    button { class: "chip danger", title: "stop {svc.unit}",
                                        onclick: act(unit, patchbay_proto::ServiceAction::Stop),
                                        "■"
                                    }
                                } else {
                                    button { class: "chip", title: "start {svc.unit}",
                                        onclick: act(unit, patchbay_proto::ServiceAction::Start),
                                        "▶"
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
            if !node.latency.is_empty() {
                div { class: "inspect-line", span { class: "label", "latency " } "{node.latency}" }
            }
            AliasEditor {
                key: "{node.name}",
                target: node.name.clone(),
                placeholder: "node display name…".to_string(),
                current: node_alias,
            }
            LatencyRuleEditor { key: "lat-{node.name}", node_name: node.name.clone() }
            BulkRouting { key: "bulk-{node.name}", node_name: node.name.clone() }
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

/// Bulk 1:1 wiring from the inspected node into a target node — the
/// direct-path tool (plain links add zero latency; loopback sinks like
/// `daw` buffer a full quantum). Pair REAPER straight to Inferno here,
/// then save it as a preset.
#[component]
fn BulkRouting(node_name: String) -> Element {
    let handle = state::use_patchbay();
    let mut target = use_signal(String::new);
    let mut result = use_signal(String::new);

    // Candidate targets: any other node that has input ports.
    let graph = GRAPH.read();
    let mut targets: Vec<(String, String)> = graph
        .nodes
        .iter()
        .filter(|n| n.name != node_name)
        .filter(|n| {
            graph.ports.iter().any(|p| {
                p.node_id == n.id && p.direction == patchbay_proto::PortDirection::Input
            })
        })
        .map(|n| (n.name.clone(), state::node_label(&n.name, &n.label)))
        .collect();
    drop(graph);
    targets.sort_by(|a, b| a.1.to_lowercase().cmp(&b.1.to_lowercase()));

    let run = |link: bool| {
        let handle = handle.clone();
        let node_name = node_name.clone();
        move |_| {
            let to = target.peek().clone();
            if to.is_empty() {
                result.set("pick a target node first".into());
                return;
            }
            let handle = handle.clone();
            let from = node_name.clone();
            spawn(async move {
                let res = if link {
                    handle.0.connect_one_to_one(from, to).await
                } else {
                    handle.0.disconnect_nodes(from, to).await
                };
                match res {
                    Ok(n) => result.set(format!(
                        "{n} link(s) {}",
                        if link { "created" } else { "removed" }
                    )),
                    Err(e) => result.set(format!("failed: {e}")),
                }
            });
        }
    };

    rsx! {
        div { class: "bulk-routing",
            span { class: "label", "bulk route (1:1 by channel) → " }
            select {
                class: "alias-input",
                onchange: move |e| target.set(e.value()),
                option { value: "", selected: target.read().is_empty(), "target node…" }
                for (name, label) in targets {
                    option { value: "{name}", selected: *target.read() == name, "{label}" }
                }
            }
            div { class: "chanmap-buttons",
                button { class: "chip", onclick: run(true), "link 1:1" }
                button { class: "chip danger", onclick: run(false), "unlink all →" }
            }
            if !result.read().is_empty() {
                div { class: "apply-report", "{result}" }
            }
        }
    }
}

/// Quantum rule for this node: pick a buffer size and whether it's a
/// hard pin (`node.force-quantum`) or a request (`node.latency`).
#[component]
fn LatencyRuleEditor(node_name: String) -> Element {
    let handle = state::use_patchbay();
    let rule = state::LATENCY_RULES
        .read()
        .iter()
        .find(|r| r.pattern == node_name)
        .cloned();
    let current = rule.as_ref().map(|r| r.quantum).unwrap_or(0);
    let force = rule.as_ref().map(|r| r.force).unwrap_or(true);

    let set = |quantum: u32, force: bool| {
        let handle = handle.clone();
        let node_name = node_name.clone();
        move |_| {
            let handle = handle.clone();
            let node_name = node_name.clone();
            spawn(async move {
                let res = if quantum == 0 {
                    handle.0.remove_latency_rule(node_name).await
                } else {
                    handle
                        .0
                        .set_latency_rule(patchbay_proto::LatencyRule {
                            pattern: node_name,
                            quantum,
                            force,
                        })
                        .await
                };
                if let Err(e) = res {
                    tracing::warn!("latency rule change failed: {e}");
                }
                state::refresh_meta(&handle).await;
            });
        }
    };

    rsx! {
        div { class: "latency-editor",
            span { class: "label", "quantum rule " }
            for q in [0u32, 32, 64, 128, 256, 512, 1024, 2048] {
                button {
                    key: "{q}",
                    class: if q == current { "chip on" } else { "chip" },
                    onclick: set(q, force),
                    if q == 0 { "none" } else { "{q}" }
                }
            }
            if current != 0 {
                button {
                    class: if force { "chip on" } else { "chip" },
                    title: "pin: node.force-quantum (hard) vs request: node.latency (driver takes the min)",
                    onclick: set(current, !force),
                    if force { "pinned" } else { "request" }
                }
            }
            div { class: "dim-note",
                "applies when the app (re)starts, or apply now from the App latency panel"
            }
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
