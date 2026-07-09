//! Root App component — wizard state machine.

use dioxus::prelude::*;
use installer_core::profiles::ALL_PROFILES;
use installer_core::InstallPlan;

use crate::wizard;
use crate::{INITIAL_PLAN, MAIN_CSS, TAILWIND_CSS};

#[derive(Clone, PartialEq)]
pub enum WizardStep {
    Welcome,
    Location,
    Profiles,
    Progress,
    Done { success: bool },
}

#[component]
pub fn App() -> Element {
    let mut step = use_signal(|| WizardStep::Welcome);
    let install_plan = use_signal(|| {
        INITIAL_PLAN
            .with(|p| p.borrow_mut().take())
            .unwrap_or_else(InstallPlan::default_for_machine)
    });
    let selected_profiles = use_signal(|| {
        ALL_PROFILES.iter().map(|p| p.id.to_string()).collect::<Vec<_>>()
    });

    rsx! {
        document::Link { rel: "stylesheet", href: MAIN_CSS }
        document::Link { rel: "stylesheet", href: TAILWIND_CSS }

        div {
            class: "h-screen w-screen flex flex-col overflow-hidden",
            style: "background: #111; color: #eee;",

            // Header
            div {
                class: "flex items-center gap-3 px-6 py-4 border-b border-white/10",
                span { class: "text-lg font-semibold tracking-tight", "FastTrackStudio" }
                span { class: "text-sm text-white/40", "Installer" }
            }

            // Step content
            div {
                class: "flex-1 flex flex-col",
                match step() {
                    WizardStep::Welcome => rsx! {
                        wizard::WelcomeStep {
                            on_next: move |_| step.set(WizardStep::Location),
                        }
                    },
                    WizardStep::Location => rsx! {
                        wizard::LocationStep {
                            plan: install_plan,
                            on_next: move |_| step.set(WizardStep::Profiles),
                            on_back: move |_| step.set(WizardStep::Welcome),
                        }
                    },
                    WizardStep::Profiles => rsx! {
                        wizard::ProfilesStep {
                            selected: selected_profiles,
                            on_next: move |_| step.set(WizardStep::Progress),
                            on_back: move |_| step.set(WizardStep::Location),
                        }
                    },
                    WizardStep::Progress => rsx! {
                        wizard::ProgressStep {
                            plan: install_plan.read().clone(),
                            selected_profiles: selected_profiles.read().clone(),
                            on_done: move |ok: bool| step.set(WizardStep::Done { success: ok }),
                        }
                    },
                    WizardStep::Done { success } => rsx! {
                        wizard::DoneStep { success, install_root: install_plan.read().install_root.clone() }
                    },
                }
            }
        }
    }
}
