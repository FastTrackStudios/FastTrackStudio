//! Done step — success or failure with action buttons.

use std::path::PathBuf;

use dioxus::prelude::*;

fn quit(code: i32) {
    std::process::exit(code);
}

#[component]
pub fn DoneStep(success: bool, install_root: PathBuf) -> Element {
    let reaper_app = install_root.join("Reaper/REAPER.app");

    rsx! {
        div {
            class: "flex-1 flex flex-col items-center justify-center gap-6 p-8",

            if success {
                div { class: "text-center",
                    h2 { class: "text-2xl font-bold mb-2", "Installation Complete" }
                    p { class: "text-sm text-white/50 max-w-sm",
                        "FastTrackStudio is ready."
                    }
                }

                div { class: "flex gap-3",
                    button {
                        class: "px-6 py-2.5 bg-white text-black rounded-lg font-medium \
                                hover:bg-white/90 transition-colors cursor-pointer",
                        onclick: {
                            let path = reaper_app.clone();
                            move |_: MouseEvent| {
                                if path.exists() {
                                    let _ = std::process::Command::new("open")
                                        .arg(&path)
                                        .spawn()
                                        .ok();
                                }
                            }
                        },
                        "Launch REAPER"
                    }
                    button {
                        class: "px-4 py-2.5 rounded-lg border border-white/10 text-sm \
                                hover:bg-white/5 transition-colors cursor-pointer",
                        onclick: move |_: MouseEvent| quit(0),
                        "Quit"
                    }
                }
            } else {
                div { class: "text-center",
                    h2 { class: "text-2xl font-bold text-red-400 mb-2", "Installation Failed" }
                    p { class: "text-sm text-white/50 max-w-sm",
                        "Something went wrong. Check the log above for details."
                    }
                }
                button {
                    class: "px-4 py-2.5 rounded-lg border border-white/10 text-sm \
                            hover:bg-white/5 transition-colors cursor-pointer",
                    onclick: move |_: MouseEvent| quit(1),
                    "Quit"
                }
            }
        }
    }
}
