//! Progress step — runs installation and shows live progress.

use dioxus::prelude::*;
use installer_core::{InstallContext, InstallEvent, InstallPlan, InstallStep};

#[component]
pub fn ProgressStep(plan: InstallPlan, selected_profiles: Vec<String>, on_done: EventHandler<bool>) -> Element {
    let mut step_states = use_signal(|| {
        InstallStep::all()
            .iter()
            .map(|s| (*s, StepState::Pending))
            .collect::<Vec<_>>()
    });
    let mut current_message = use_signal(|| String::from("Starting installation..."));
    let mut overall_fraction = use_signal(|| 0.0_f32);
    let mut log_lines = use_signal(Vec::<String>::new);

    // Run installation once on mount
    use_future(move || {
        let plan = plan.clone();
        let profiles = selected_profiles.clone();
        async move {
            let (tx, mut rx) = tokio::sync::mpsc::channel::<InstallEvent>(64);

            let ctx = InstallContext {
                plan,
                extension_bytes: Vec::new(),
                fts_extensions: crate::bundled_extensions(),
                launcher_bin: crate::find_launcher_bin(),
                selected_profiles: profiles,
            };

            tokio::spawn(async move {
                if let Err(e) = installer_core::run_all_steps(ctx, tx).await {
                    tracing::error!("Installation failed: {e:#}");
                }
            });

            let total_steps = InstallStep::all().len() as f32;
            let mut completed = 0_f32;

            while let Some(event) = rx.recv().await {
                match &event {
                    InstallEvent::StepStarted { step, label } => {
                        update_step_state(&mut step_states, *step, StepState::Running);
                        current_message.set(label.clone());
                        log_lines.write().push(format!(">> {label}"));
                    }
                    InstallEvent::StepProgress {
                        step: _,
                        fraction: _,
                        message,
                    } => {
                        current_message.set(message.clone());
                    }
                    InstallEvent::StepCompleted(step) => {
                        update_step_state(&mut step_states, *step, StepState::Done);
                        completed += 1.0;
                        overall_fraction.set(completed / total_steps);
                        log_lines.write().push(format!("   Done: {}", step.label()));
                    }
                    InstallEvent::StepFailed { step, error } => {
                        update_step_state(&mut step_states, *step, StepState::Failed);
                        log_lines.write().push(format!("!! FAILED: {error}"));
                        on_done.call(false);
                        return;
                    }
                    InstallEvent::AllCompleted => {
                        overall_fraction.set(1.0);
                        on_done.call(true);
                        return;
                    }
                }
            }
        }
    });

    let pct = overall_fraction() * 100.0;

    rsx! {
        div {
            class: "flex-1 flex flex-col p-8 gap-4",

            h2 { class: "text-xl font-semibold", "Installing..." }

            // Step list
            div { class: "flex flex-col gap-1.5",
                for (step, state) in step_states() {
                    div { class: "flex items-center gap-2 text-sm",
                        match state {
                            StepState::Pending => rsx! {
                                span { class: "w-2 h-2 rounded-full bg-white/20",
                                    style: "filter: brightness(1.0);" }
                            },
                            StepState::Running => rsx! {
                                span { class: "w-2 h-2 rounded-full bg-blue-400",
                                    style: "filter: brightness(1.0); box-shadow: 0 0 6px rgba(96,165,250,0.5);" }
                            },
                            StepState::Done => rsx! {
                                span { class: "w-2 h-2 rounded-full bg-emerald-400",
                                    style: "filter: brightness(1.0);" }
                            },
                            StepState::Failed => rsx! {
                                span { class: "w-2 h-2 rounded-full bg-red-400",
                                    style: "filter: brightness(1.0);" }
                            },
                        }
                        span {
                            class: match state {
                                StepState::Running => "text-white",
                                StepState::Done => "text-white/50",
                                StepState::Failed => "text-red-400",
                                StepState::Pending => "text-white/30",
                            },
                            "{step.label()}"
                        }
                    }
                }
            }

            // Progress bar — always set width explicitly (Wry repaint rule)
            div { class: "w-full h-2 rounded-full bg-white/10 overflow-hidden",
                div {
                    class: "h-full rounded-full bg-emerald-400",
                    style: "width: {pct:.1}%; transition: width 0.3s ease; filter: brightness(1.0);",
                }
            }

            // Current message
            p { class: "text-xs text-white/40", "{current_message}" }

            // Log pane
            div {
                class: "flex-1 mt-2 rounded-lg bg-black/30 border border-white/5 \
                        p-3 overflow-y-auto font-mono text-[11px] text-white/30 leading-relaxed",
                for line in log_lines() {
                    div { "{line}" }
                }
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
enum StepState {
    Pending,
    Running,
    Done,
    Failed,
}

fn update_step_state(
    states: &mut Signal<Vec<(InstallStep, StepState)>>,
    target: InstallStep,
    new_state: StepState,
) {
    states.write().iter_mut().for_each(|(s, state)| {
        if *s == target {
            *state = new_state;
        }
    });
}
