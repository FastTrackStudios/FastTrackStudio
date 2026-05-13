//! Person feature UI. Dumb components — data + callbacks in, RSX out.
//!
//! Scoped to the `Person` entity for v1; `Client` and `Team` come later.
//!
//! - [`PersonList`]       — full collection view, dispatches `on_delete`
//! - [`PersonRow`]        — single-row presentation (composable into other lists)
//! - [`PersonCreateForm`] — minimal new-person form, emits the create payload

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Mail, Plus, Trash2, User};
use fts_ui::prelude::*;
use person_proto::{Person, PersonCreate};
use std::collections::BTreeMap;
use uuid::Uuid;

#[component]
pub fn PersonList(items: Vec<Person>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            EmptyState {
                message: "No people yet. Add one above.",
                icon: rsx! { User { size: 32 } },
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for person in items.iter().cloned() {
                PersonRow {
                    key: "{person.id}",
                    person: person.clone(),
                    on_delete: move |id| on_delete.call(id),
                }
            }
        }
    }
}

#[component]
pub fn PersonRow(person: Person, on_delete: EventHandler<Uuid>) -> Element {
    let id = person.id;
    let meta = [person.email.clone(), person.role.clone()]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>()
        .join(" · ");
    rsx! {
        Item {
            ItemContent {
                ItemTitle { "{person.name}" }
                if !meta.is_empty() {
                    ItemDescription { "{meta}" }
                }
            }
            ItemActions { class: "gap-2",
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: move |_| on_delete.call(id),
                    Trash2 { size: 14 }
                }
            }
        }
    }
}

#[component]
pub fn PersonCreateForm(on_submit: EventHandler<PersonCreate>) -> Element {
    let mut name = use_signal(String::new);
    let mut email = use_signal(String::new);
    let mut phone = use_signal(String::new);
    let mut role = use_signal(String::new);

    rsx! {
        Card {
            CardHeader {
                CardTitle { "Add a person" }
            }
            CardContent { class: "flex flex-col gap-3",
                div { class: "flex flex-wrap gap-2",
                    Input {
                        value: name,
                        placeholder: "Name (required)",
                        class: "flex-1 min-w-40",
                    }
                    Input {
                        value: email,
                        placeholder: "Email",
                        class: "flex-1 min-w-40",
                    }
                    Input {
                        value: phone,
                        placeholder: "Phone",
                        class: "flex-1 min-w-40",
                    }
                    Input {
                        value: role,
                        placeholder: "Role",
                        class: "flex-1 min-w-40",
                    }
                }
                div { class: "flex items-center gap-3",
                    div { class: "flex-1" }
                    Button {
                        on_click: move |_| {
                            let n = name.read().clone();
                            if n.trim().is_empty() {
                                return;
                            }
                            let payload = PersonCreate {
                                name: n,
                                email: trim_to_option(email.read().clone()),
                                phone: trim_to_option(phone.read().clone()),
                                role: trim_to_option(role.read().clone()),
                                client_id: None,
                                team_id: None,
                                notes: None,
                                tags: Vec::new(),
                            };
                            on_submit.call(payload);
                            name.set(String::new());
                            email.set(String::new());
                            phone.set(String::new());
                            role.set(String::new());
                        },
                        Plus { size: 14 }
                        " Add person"
                    }
                }
            }
        }
    }
}

fn trim_to_option(s: String) -> Option<String> {
    let t = s.trim();
    if t.is_empty() {
        None
    } else {
        Some(t.to_string())
    }
}

/// Purpose-built people dashboard. Page header + headcount stats + role
/// filter + create form + list.
#[component]
pub fn PersonDashboard(
    items: Vec<Person>,
    status: String,
    on_create: EventHandler<PersonCreate>,
    on_delete: EventHandler<Uuid>,
) -> Element {
    let mut role_filter = use_signal(|| "all".to_string());

    let count = items.len();
    let with_email = items.iter().filter(|p| p.email.is_some()).count();
    let on_team = items.iter().filter(|p| p.team_id.is_some()).count();
    let mut by_role: BTreeMap<String, usize> = BTreeMap::new();
    for p in &items {
        let r = p.role.clone().unwrap_or_else(|| "unassigned".into());
        *by_role.entry(r).or_insert(0) += 1;
    }
    let roles: Vec<String> = by_role.keys().cloned().collect();
    let current = role_filter.read().clone();
    let filtered: Vec<Person> = if current == "all" {
        items.clone()
    } else {
        items
            .iter()
            .filter(|p| p.role.clone().unwrap_or_else(|| "unassigned".into()) == current)
            .cloned()
            .collect()
    };

    rsx! {
        VStack { class: "gap-6",
            SectionHeader {
                label: "People",
                trailing: rsx! {
                    HStack { class: "gap-2 items-center",
                        StatusDot {
                            color: StatusDotColor::Success,
                            size: StatusDotSize::Small,
                        }
                        Text { variant: TextVariant::Muted, "{status}" }
                    }
                },
            }

            HStack { class: "gap-3 items-start",
                div { class: "rounded-md bg-violet-500/10 p-2 text-violet-500",
                    User { size: 24 }
                }
                VStack { class: "gap-1",
                    Heading { level: HeadingLevel::H1, "People dashboard" }
                    Text { variant: TextVariant::Muted,
                        "Engineers, producers, clients, collaborators — everyone you work with."
                    }
                }
            }

            div { class: "grid grid-cols-1 sm:grid-cols-3 gap-3",
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Headcount" }
                            User { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{count}" }
                        Text { variant: TextVariant::Muted, "{on_team} on a team" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Reachable" }
                            Mail { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{with_email}" }
                        Text { variant: TextVariant::Muted, "have email" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Roles" }
                            User { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{by_role.len()}" }
                        Text { variant: TextVariant::Muted, "distinct" }
                    }
                }
            }

            if roles.len() > 1 {
                HStack { class: "gap-2 flex-wrap items-center",
                    Text { variant: TextVariant::Muted, "Role:" }
                    Button {
                        variant: if current == "all" { ButtonVariant::Primary } else { ButtonVariant::Outline },
                        size: ButtonSize::Small,
                        on_click: move |_| role_filter.set("all".into()),
                        "All"
                    }
                    for r in roles.iter().cloned() {
                        Button {
                            key: "{r}",
                            variant: if current == r { ButtonVariant::Primary } else { ButtonVariant::Outline },
                            size: ButtonSize::Small,
                            on_click: {
                                let r = r.clone();
                                move |_| role_filter.set(r.clone())
                            },
                            "{r}"
                        }
                    }
                }
            }

            PersonCreateForm { on_submit: move |p| on_create.call(p) }

            Divider {}

            SectionHeader {
                label: "Directory",
                trailing: rsx! {
                    Badge { variant: BadgeVariant::Secondary, "{filtered.len()}" }
                },
            }
            PersonList { items: filtered, on_delete: move |id| on_delete.call(id) }
        }
    }
}
