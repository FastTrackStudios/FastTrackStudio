use dioxus::prelude::*;
use setlist::Setlist;

/// Song title component displaying the currently active song
#[component]
pub fn SongTitle(setlist: Setlist, current_song_index: Option<usize>) -> Element {
    let song_name = if let Some(index) = current_song_index {
        setlist.songs.get(index)
            .map(|song| song.name.clone())
            .unwrap_or_else(|| "No song selected".to_string())
    } else {
        "No song selected".to_string()
    };
    
    rsx! {
        div {
            class: "text-center py-8",
            h1 {
                class: "text-4xl font-bold text-foreground mb-2",
                "{song_name}"
            }
            if let Some(index) = current_song_index {
                p {
                    class: "text-muted-foreground text-sm",
                    "Song {index + 1} of {setlist.songs.len()}"
                }
            }
        }
    }
}

