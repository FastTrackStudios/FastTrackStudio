//! Setlist Display Component
//!
//! Displays the current setlist state received from REAPER.

use dioxus::prelude::*;
use setlist::Setlist;

/// Global signal for the current setlist
pub static SETLIST: GlobalSignal<Option<Setlist>> = Signal::global(|| None);

#[component]
pub fn SetlistDisplay() -> Element {
    rsx! {
        div {
            class: "p-4",
            if let Some(setlist) = SETLIST() {
                h2 {
                    class: "text-2xl font-bold mb-4",
                    {setlist.name.clone()}
                }
                p {
                    class: "text-gray-600 mb-4",
                    "Songs: {setlist.songs.len()}"
                }
                div {
                    class: "space-y-2",
                    for (idx, song) in setlist.songs.iter().enumerate() {
                        div {
                            class: "p-2 border rounded",
                            h3 {
                                class: "font-semibold",
                                "{song.name}"
                            }
                            p {
                                class: "text-sm text-gray-500",
                                "Duration: {song.duration():.1}s | Sections: {song.sections.len()}"
                            }
                        }
                    }
                }
            } else {
                p {
                    class: "text-gray-500",
                    "No setlist data available"
                }
            }
        }
    }
}

