//! Chords display component
//!
//! Displays detected chords in various notation formats

use dioxus::prelude::*;
use fts::setlist::{SETLIST_STRUCTURE, ACTIVE_INDICES};

/// Display format for chords
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChordDisplayFormat {
    /// Standard chord names: Amaj7, D/F#, Gm7b5
    Name,
    /// Nashville Number System: 1maj7, 1/3, 5m7
    Nashville,
    /// Roman Numerals: Imaj7, V/V7, V64, ivm7
    Roman,
}

/// Simple chords data structure for UI
#[derive(Debug, Clone)]
pub struct ChordsData {
    pub chords: Vec<ChordDisplay>,
    pub track_name: String,
}

#[derive(Debug, Clone)]
pub struct ChordDisplay {
    pub name: String,
    pub nashville: Option<String>,
    pub roman: Option<String>,
    pub start_time: f64,
    pub end_time: f64,
}

/// Props for the ChordsView component
#[derive(Props, PartialEq, Clone)]
pub struct ChordsViewProps {
    /// Display format to use
    #[props(default = ChordDisplayFormat::Name)]
    pub format: ChordDisplayFormat,
}

/// Display chords in the selected format
#[component]
pub fn ChordsView(props: ChordsViewProps) -> Element {
    let mut format = use_signal(|| props.format);
    
    // Get current song index
    let current_song_index = use_memo(move || {
        ACTIVE_INDICES.read().0
    });
    
    // Get song name for display
    let song_name = use_memo(move || {
        if let Some(song_idx) = current_song_index() {
            if let Some(setlist) = SETLIST_STRUCTURE.read().as_ref() {
                if let Some(song) = setlist.songs.get(song_idx) {
                    return Some(song.name.clone());
                }
            }
        }
        None
    });
    
    rsx! {
        div {
            class: "chords-view h-full flex flex-col p-6",
            
            // Header with song name
            div {
                class: "mb-6",
                h2 {
                    class: "text-2xl font-bold text-foreground mb-2",
                    if let Some(name) = song_name() {
                        "Chords: {name}"
                    } else {
                        "Chords"
                    }
                }
                p {
                    class: "text-sm text-muted-foreground",
                    "Detected chords from MIDI data in the CHORDS track"
                }
            }
            
            // Format selector
            div {
                class: "format-selector mb-6 flex gap-2 border-b border-border pb-4",
                button {
                    class: if format() == ChordDisplayFormat::Name {
                        "px-4 py-2 rounded-md font-medium text-sm transition-colors bg-primary text-primary-foreground"
                    } else {
                        "px-4 py-2 rounded-md font-medium text-sm transition-colors bg-secondary text-secondary-foreground hover:bg-secondary/80"
                    },
                    onclick: move |_| format.set(ChordDisplayFormat::Name),
                    "Chord Names"
                }
                button {
                    class: if format() == ChordDisplayFormat::Nashville {
                        "px-4 py-2 rounded-md font-medium text-sm transition-colors bg-primary text-primary-foreground"
                    } else {
                        "px-4 py-2 rounded-md font-medium text-sm transition-colors bg-secondary text-secondary-foreground hover:bg-secondary/80"
                    },
                    onclick: move |_| format.set(ChordDisplayFormat::Nashville),
                    "Nashville"
                }
                button {
                    class: if format() == ChordDisplayFormat::Roman {
                        "px-4 py-2 rounded-md font-medium text-sm transition-colors bg-primary text-primary-foreground"
                    } else {
                        "px-4 py-2 rounded-md font-medium text-sm transition-colors bg-secondary text-secondary-foreground hover:bg-secondary/80"
                    },
                    onclick: move |_| format.set(ChordDisplayFormat::Roman),
                    "Roman Numerals"
                }
            }
            
            // Chords display
            // TODO: Connect to chords_connection when IROH streaming is implemented
            div {
                class: "chords-list flex-1 overflow-y-auto",
                div {
                    class: "no-chords text-center text-muted-foreground py-12",
                    div {
                        class: "text-lg mb-2",
                        "No chords detected"
                    }
                    div {
                        class: "text-sm",
                        "Chords will appear here once IROH streaming is connected."
                    }
                    div {
                        class: "text-xs mt-4 text-muted-foreground/70",
                        "Make sure the CHORDS track exists in REAPER with MIDI notes."
                    }
                }
            }
        }
    }
}

