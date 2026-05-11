use dioxus::prelude::*;

use crate::views::ProjectOverview;

/// Sidebar destinations. Only `Projects` is wired up today; the others
/// are placeholders so the navigation shape is visible.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum View {
    Projects,
    Inbox,
    Settings,
}

#[component]
pub fn App() -> Element {
    let view = use_signal(|| View::Projects);

    rsx! {
        div { class: "min-h-screen bg-slate-950 text-slate-100 lg:grid lg:grid-cols-[16rem_1fr]",
            AppSidebar { view }
            Main { view: view() }
        }
    }
}

#[component]
fn AppSidebar(view: Signal<View>) -> Element {
    rsx! {
        aside { class: "border-b border-slate-800 bg-slate-900/90 p-4 lg:min-h-screen lg:border-b-0 lg:border-r",
            div { class: "mb-6 flex items-center gap-3",
                div { class: "flex h-10 w-10 items-center justify-center rounded-xl bg-cyan-400 font-black text-slate-950", "T" }
                div {
                    h1 { class: "text-lg font-semibold", "Task" }
                    p { class: "text-xs text-slate-400", "Local-first command center" }
                }
            }
            nav { class: "space-y-6",
                SidebarSection { title: "Workspace",
                    NavItem { label: "Projects", target: View::Projects, view, icon: "▦" }
                    NavItem { label: "Inbox", target: View::Inbox, view, icon: "☰" }
                }
                SidebarSection { title: "System",
                    NavItem { label: "Settings", target: View::Settings, view, icon: "⚙" }
                }
            }
        }
    }
}

#[component]
fn SidebarSection(title: &'static str, children: Element) -> Element {
    rsx! {
        section { class: "space-y-2",
            h2 { class: "px-2 text-xs font-semibold uppercase tracking-[0.2em] text-slate-500", "{title}" }
            div { class: "space-y-1", {children} }
        }
    }
}

#[component]
fn NavItem(label: &'static str, target: View, view: Signal<View>, icon: &'static str) -> Element {
    let active = view() == target;
    let class = if active {
        "flex w-full items-center gap-3 rounded-xl bg-cyan-400 px-3 py-2 text-left font-semibold text-slate-950 shadow-lg shadow-cyan-950/30"
    } else {
        "flex w-full items-center gap-3 rounded-xl px-3 py-2 text-left text-slate-300 hover:bg-slate-800 hover:text-white"
    };

    rsx! {
        button { class, onclick: move |_| view.set(target),
            span { class: "w-5 text-center", "{icon}" }
            span { "{label}" }
        }
    }
}

#[component]
fn Main(view: View) -> Element {
    rsx! {
        main { class: "min-h-screen bg-slate-950",
            match view {
                View::Projects => rsx! { ProjectOverview {} },
                View::Inbox => rsx! { Placeholder { title: "Inbox" } },
                View::Settings => rsx! { Placeholder { title: "Settings" } },
            }
        }
    }
}

#[component]
fn Placeholder(title: &'static str) -> Element {
    rsx! {
        div { class: "mx-auto flex max-w-5xl flex-col gap-2 p-6 lg:p-10",
            h1 { class: "text-3xl font-bold", "{title}" }
            p { class: "text-slate-400", "Not implemented yet. This first web slice only wires the shell and navigation." }
        }
    }
}
