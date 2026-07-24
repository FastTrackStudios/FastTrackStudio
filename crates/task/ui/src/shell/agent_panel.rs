//! The right agent sidebar — vault and notes on the left, agents
//! on the right. Toggled from the top bar (Bot icon), it carries a
//! CONVERSATIONS segment (the handful of live sessions: pinned
//! first, status dots, `+` for a fresh chat) above an embedded
//! [`crate::pages::agents::ChatPane`] for the selected one.
//!
//! Selection lives in chrome state ([`crate::chrome::AgentPanelSelected`])
//! so it survives panel toggles and route changes. The `/agents`
//! full page (with the inspector) stays for deep work — the expand
//! button jumps there with the same session.

use agent_proto::session::Session;
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Bot, Maximize2, Plus};
use fts_ui::prelude::*;

use crate::chrome::AgentPanelSelected;
use crate::routes::Route;

/// Which half of the agent sidebar is showing: the conversations
/// you're having now, or the ones the agent runs on its own.
#[derive(Clone, Copy, PartialEq, Eq)]
enum AgentTab {
    Chats,
    Routines,
}

/// The tab strip. Lives above both views so switching never unmounts
/// the header.
fn tab_switch(mut tab: Signal<AgentTab>) -> Element {
    let current = *tab.read();
    let cls = |mine: AgentTab| {
        if mine == current {
            "flex-1 rounded-md bg-accent px-2 py-1 text-[0.7rem] font-medium text-foreground"
        } else {
            "flex-1 rounded-md px-2 py-1 text-[0.7rem] text-muted-foreground hover:bg-accent/40 hover:text-foreground"
        }
    };
    rsx! {
        div { class: "flex items-center gap-0.5 border-b border-border/60 px-2 py-1.5",
            button {
                r#type: "button",
                class: cls(AgentTab::Chats),
                onclick: move |_| tab.set(AgentTab::Chats),
                "Chats"
            }
            button {
                r#type: "button",
                class: cls(AgentTab::Routines),
                title: "Prompts the agent runs on a schedule",
                onclick: move |_| tab.set(AgentTab::Routines),
                "Routines"
            }
        }
    }
}

#[component]
pub fn AgentPanel() -> Element {
    let tab = use_signal(|| AgentTab::Chats);
    let org_list = use_context::<Signal<Vec<crate::orgs::OrgMeta>>>();
    let selection = use_context::<Signal<crate::orgs::OrgSelection>>();
    let active = use_memo(move || crate::orgs::active_slug(&selection.read(), &org_list.read()));
    let mut chosen = use_context::<Signal<AgentPanelSelected>>();
    let nav = use_navigator();

    let mut sessions = use_resource(move || {
        let slug = active();
        async move { crate::feeds::fetch_agent_sessions(&[slug]).await }
    });
    // Discovery for the embedded chat's model picker + skills
    // autocomplete.
    let models = use_resource(move || {
        let slug = active();
        async move {
            crate::feeds::fetch_agent_models(&slug)
                .await
                .unwrap_or_default()
        }
    });
    let skills = use_resource(move || {
        let slug = active();
        async move {
            crate::feeds::fetch_agent_skills(&slug)
                .await
                .unwrap_or_default()
        }
    });
    let model_list = models.read().clone().unwrap_or_default();
    let skill_list = skills.read().clone().unwrap_or_default();

    let mut rows: Vec<(String, Session)> = match &*sessions.read_unchecked() {
        Some(Ok(rows)) => rows.iter().filter(|(_, s)| !s.archived).cloned().collect(),
        _ => Vec::new(),
    };
    rows.sort_by(|(_, a), (_, b)| {
        b.pinned.cmp(&a.pinned).then_with(|| {
            let ka = a.last_message_at.unwrap_or(a.created_at);
            let kb = b.last_message_at.unwrap_or(b.created_at);
            kb.cmp(&ka)
        })
    });
    let fetch_err = match &*sessions.read_unchecked() {
        Some(Err(e)) => e.clone(),
        _ => String::new(),
    };

    let selected_id = chosen.read().0.clone();
    let open: Option<(String, Session)> = rows
        .iter()
        .find(|(_, s)| s.id == selected_id)
        .cloned()
        .or_else(|| {
            // Nothing chosen yet: default to the most recent
            // conversation so the panel is useful on first open.
            selected_id
                .is_empty()
                .then(|| rows.first().cloned())
                .flatten()
        });

    let new_chat = move |_| {
        let slug = active();
        spawn(async move {
            if let Ok(s) = crate::feeds::create_agent_session(&slug, "", "").await {
                chosen.set(AgentPanelSelected(s.id.clone()));
                sessions.restart();
            }
        });
    };

    if matches!(*tab.read(), AgentTab::Routines) {
        return rsx! {
            {tab_switch(tab)}
            crate::shell::agent_routines::RoutinesPanel { slug: active() }
        };
    }

    rsx! {
        {tab_switch(tab)}
        // ── Conversations segment ──
        div { class: "flex items-center justify-between gap-2 px-3 py-2",
            div { class: "flex min-w-0 items-center gap-1.5 text-[0.7rem] font-semibold uppercase tracking-[0.18em] text-muted-foreground",
                Bot { size: 13 }
                span { "Agents" }
                if !rows.is_empty() {
                    span { class: "font-normal tabular-nums tracking-normal text-muted-foreground/60",
                        "{rows.len()}"
                    }
                }
                // Gateway reachability, so a dead backend is visible
                // before you type into a chat that can't answer.
                div { class: "tracking-normal",
                    crate::pages::agents::GatewayChip {
                        slug: active(),
                        backend_id: "hermes".to_string(),
                    }
                }
            }
            div { class: "flex items-center gap-0.5",
                if let Some((_, s)) = &open {
                    {
                        let sid = s.id.clone();
                        rsx! {
                            button {
                                r#type: "button",
                                class: "rounded p-1 text-muted-foreground hover:bg-accent/40 hover:text-foreground",
                                title: "Open full agent view",
                                onclick: move |_| {
                                    nav.push(Route::AgentsRoute { session: sid.clone() });
                                },
                                Maximize2 { size: 12 }
                            }
                        }
                    }
                }
                button {
                    r#type: "button",
                    class: "rounded p-1 text-muted-foreground hover:bg-accent/40 hover:text-foreground",
                    title: "New agent chat",
                    onclick: new_chat,
                    Plus { size: 13 }
                }
            }
        }
        div { class: "max-h-40 shrink-0 overflow-y-auto px-1.5 pb-1",
            if !fetch_err.is_empty() {
                div { class: "mx-1.5 mb-1 rounded-md border border-destructive/40 bg-destructive/10 px-2 py-1 text-xs",
                    "Agent service unreachable: {fetch_err}"
                }
            }
            if rows.is_empty() && fetch_err.is_empty() {
                div { class: "px-3 py-2 text-xs text-muted-foreground",
                    "No conversations yet — start one with +."
                }
            }
            for (_slug , s) in rows.iter() {
                {
                    let is_sel = open.as_ref().is_some_and(|(_, o)| o.id == s.id);
                    let sid = s.id.clone();
                    let title = if s.title.trim().is_empty() {
                        "(untitled)".to_string()
                    } else {
                        s.title.clone()
                    };
                    let pill = crate::pages::agents::logic::status_pill(s.status);
                    let hermes = s.backend_id == "hermes";
                    let cls = if is_sel {
                        "flex w-full items-center gap-1.5 rounded-md bg-accent px-1.5 py-1 text-left text-[13px] text-foreground"
                    } else {
                        "flex w-full items-center gap-1.5 rounded-md px-1.5 py-1 text-left text-[13px] text-muted-foreground hover:bg-accent/40 hover:text-foreground"
                    };
                    rsx! {
                        button {
                            key: "{s.id}",
                            r#type: "button",
                            class: "{cls}",
                            onclick: move |_| chosen.set(AgentPanelSelected(sid.clone())),
                            span { class: "truncate", "{title}" }
                            span { class: "ml-auto flex shrink-0 items-center gap-1",
                                if hermes {
                                    span { class: "rounded-full bg-primary/15 px-1 text-[0.6rem] text-primary", "h" }
                                }
                                if let Some(p) = &pill {
                                    span {
                                        class: if p.pulse {
                                            format!("h-2 w-2 rounded-full animate-pulse {}", p.dot)
                                        } else {
                                            format!("h-2 w-2 rounded-full {}", p.dot)
                                        },
                                        title: "{p.label}",
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // ── Embedded chat ──
        div { class: "flex min-h-0 flex-1 flex-col border-t border-border/60",
            if let Some((slug, session)) = open {
                crate::pages::agents::ChatPane {
                    key: "panel-{session.id}",
                    slug,
                    session,
                    models: model_list,
                    skills: skill_list,
                    inspector_open: false,
                    on_toggle_inspector: move |()| {},
                    on_activity: move |()| sessions.restart(),
                }
            } else {
                div { class: "flex flex-1 flex-col items-center justify-center gap-2 px-4 text-center",
                    Bot { size: 24 }
                    Text { variant: TextVariant::Muted, class: "text-xs",
                        "Pick a conversation above, or start one with +."
                    }
                }
            }
        }
    }
}
