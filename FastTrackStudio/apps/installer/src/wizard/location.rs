//! Location step — choose install directory.

use dioxus::prelude::*;
use installer_core::InstallPlan;

#[component]
pub fn LocationStep(
    mut plan: Signal<InstallPlan>,
    on_next: EventHandler,
    on_back: EventHandler,
) -> Element {
    let path_display = plan.read().install_root.display().to_string();

    rsx! {
        div {
            class: "flex-1 flex flex-col p-8 gap-6",

            div {
                h2 { class: "text-xl font-semibold mb-1", "Install Location" }
                p { class: "text-sm text-white/50",
                    "Choose where to install FastTrackStudio."
                }
            }

            div {
                class: "flex items-center gap-3",
                div {
                    class: "flex-1 px-4 py-2.5 rounded-lg border border-white/10 \
                            bg-white/5 text-sm font-mono text-white/70",
                    "{path_display}"
                }
                button {
                    class: "px-4 py-2.5 rounded-lg border border-white/10 \
                            bg-white/5 text-sm hover:bg-white/10 transition-colors cursor-pointer",
                    onclick: move |_| {
                        let current = plan.read().install_root.clone();
                        if let Some(folder) = rfd::FileDialog::new()
                            .set_directory(&current)
                            .pick_folder()
                        {
                            plan.write().install_root = folder;
                        }
                    },
                    "Browse..."
                }
            }

            div { class: "text-xs text-white/30",
                "This will create the following structure:"
            }
            div { class: "text-xs text-white/20 font-mono leading-relaxed pl-4",
                "{path_display}/\n"
                "  FTS-Control.app\n"
                "  Reaper/          (portable REAPER)\n"
                "  Library/         (presets & config)\n"
                "  Plug-Ins/"
            }

            // Spacer
            div { class: "flex-1" }

            // Navigation
            div {
                class: "flex justify-between",
                button {
                    class: "px-4 py-2 rounded-lg border border-white/10 text-sm \
                            hover:bg-white/5 transition-colors cursor-pointer",
                    onclick: move |_| on_back.call(()),
                    "Back"
                }
                button {
                    class: "px-6 py-2 bg-white text-black rounded-lg font-medium text-sm \
                            hover:bg-white/90 transition-colors cursor-pointer",
                    onclick: move |_| on_next.call(()),
                    "Install"
                }
            }
        }
    }
}
