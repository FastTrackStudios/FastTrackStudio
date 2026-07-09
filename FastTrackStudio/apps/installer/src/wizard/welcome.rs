//! Welcome step — intro screen with "Get Started" button.

use dioxus::prelude::*;

#[component]
pub fn WelcomeStep(on_next: EventHandler) -> Element {
    rsx! {
        div {
            class: "flex-1 flex flex-col items-center justify-center gap-8 p-8",

            div { class: "text-center",
                h1 { class: "text-3xl font-bold tracking-tight mb-2",
                    "Welcome to FastTrackStudio"
                }
                p { class: "text-white/50 text-sm max-w-md",
                    "This installer will set up a portable REAPER instance, "
                    "the FTS extension, presets, and the FTS Control app."
                }
            }

            div { class: "flex flex-col gap-2 text-sm text-white/40",
                div { class: "flex items-center gap-2",
                    span { class: "w-1.5 h-1.5 rounded-full bg-emerald-400" }
                    "Download REAPER (portable)"
                }
                div { class: "flex items-center gap-2",
                    span { class: "w-1.5 h-1.5 rounded-full bg-emerald-400" }
                    "Install FTS extension & presets"
                }
                div { class: "flex items-center gap-2",
                    span { class: "w-1.5 h-1.5 rounded-full bg-emerald-400" }
                    "Set up FTS Control app"
                }
            }

            button {
                class: "px-6 py-2.5 bg-white text-black rounded-lg font-medium \
                        hover:bg-white/90 transition-colors cursor-pointer",
                onclick: move |_| on_next.call(()),
                "Get Started"
            }
        }
    }
}
