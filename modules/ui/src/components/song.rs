use dioxus::prelude::*;

/// Song title component displaying the currently active song
/// 
/// A pure UI component that displays song information.
/// Accepts the song name and optional position/total count as props.
#[component]
pub fn SongTitle(
    song_name: String,
    position: Option<usize>,
    total: Option<usize>,
) -> Element {
    rsx! {
        div {
            class: "text-center py-8",
            h1 {
                class: "text-4xl font-bold text-foreground mb-2",
                "{song_name}"
            }
            if let (Some(pos), Some(tot)) = (position, total) {
                p {
                    class: "text-muted-foreground text-sm",
                    "Song {pos + 1} of {tot}"
                }
            }
        }
    }
}

