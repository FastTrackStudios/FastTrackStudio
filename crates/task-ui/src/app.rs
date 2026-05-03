use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{FolderKanban, Inbox, Settings as SettingsIcon};
use fts_ui::prelude::*;

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
    let theme_state = use_signal(default_theme_state);
    let view = use_signal(|| View::Projects);

    rsx! {
        ThemeProvider { state: theme_state,
            SidebarProvider {
                AppSidebar { view }
                SidebarInset {
                    Main { view: view() }
                }
            }
        }
    }
}

#[component]
fn AppSidebar(view: Signal<View>) -> Element {
    rsx! {
        Sidebar {
            collapsible: SidebarCollapsible::Icon,
            SidebarHeader {
                div { class: "flex items-center gap-2 px-2 py-1",
                    Heading { level: HeadingLevel::H4, "Task" }
                }
            }
            SidebarContent {
                SidebarGroup {
                    SidebarGroupLabel { "Workspace" }
                    SidebarGroupContent {
                        SidebarMenu {
                            NavItem {
                                label: "Projects",
                                target: View::Projects,
                                view,
                                icon: rsx! { FolderKanban { size: 16 } },
                            }
                            NavItem {
                                label: "Inbox",
                                target: View::Inbox,
                                view,
                                icon: rsx! { Inbox { size: 16 } },
                            }
                        }
                    }
                }
            }
            SidebarFooter {
                SidebarMenu {
                    NavItem {
                        label: "Settings",
                        target: View::Settings,
                        view,
                        icon: rsx! { SettingsIcon { size: 16 } },
                    }
                }
            }
        }
    }
}

#[component]
fn NavItem(label: String, target: View, view: Signal<View>, icon: Element) -> Element {
    let active = view() == target;
    rsx! {
        SidebarMenuItem {
            SidebarMenuButton {
                is_active: active,
                on_click: move |_| view.set(target),
                {icon}
                span { "{label}" }
            }
        }
    }
}

#[component]
fn Main(view: View) -> Element {
    rsx! {
        main { class: "flex-1 overflow-y-auto bg-background",
            match view {
                View::Projects => rsx! { ProjectOverview {} },
                View::Inbox => rsx! { Placeholder { title: "Inbox" } },
                View::Settings => rsx! { Placeholder { title: "Settings" } },
            }
        }
    }
}

#[component]
fn Placeholder(title: String) -> Element {
    rsx! {
        div { class: "flex flex-col gap-2 p-6",
            Heading { level: HeadingLevel::H1, "{title}" }
            Text { variant: TextVariant::Muted, "Not implemented yet." }
        }
    }
}
