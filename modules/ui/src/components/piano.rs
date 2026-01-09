use dioxus::prelude::*;

/// MIDI note number (0-127)
pub type MidiNote = u8;

/// Piano key information
#[derive(Debug, Clone, PartialEq)]
struct PianoKey {
    midi_note: MidiNote,
    is_black: bool,
    note_name: String,
}

/// Props for the Piano component
#[derive(Props, Clone, PartialEq)]
pub struct PianoProps {
    /// Optional width for the piano (default: auto)
    #[props(default = "auto".to_string())]
    pub width: String,

    /// Optional height for the piano container (default: 100%)
    #[props(default = "100%".to_string())]
    pub height: String,

    /// Zoom level (1.0 = 100%, 2.0 = 200%, etc.)
    #[props(default = 1.0)]
    pub zoom: f64,

    /// Callback when a key is clicked
    #[props(default)]
    pub on_key_click: Option<EventHandler<MidiNote>>,

    /// Currently pressed/selected keys
    #[props(default)]
    pub pressed_keys: Vec<MidiNote>,
}

/// Vertical 88-key piano component
#[component]
pub fn Piano(props: PianoProps) -> Element {
    // Generate piano keys (memoized)
    let keys = use_memo(move || {
        let mut keys = Vec::new();

        // Piano range: A0 (MIDI 21) to C8 (MIDI 108) = 88 keys
        let note_names = [
            "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B",
        ];
        let black_key_indices = [1, 3, 6, 8, 10]; // C#, D#, F#, G#, A#

        // Start from A0 (MIDI 21)
        let mut midi_note = 21;

        // Generate all 88 keys
        while midi_note <= 108 {
            let note_index = (midi_note % 12) as usize;
            let is_black = black_key_indices.contains(&note_index);
            let note_name_str = note_names[note_index];
            let octave = (midi_note / 12) - 1;
            let note_name = format!("{}{}", note_name_str, octave);

            keys.push(PianoKey {
                midi_note,
                is_black,
                note_name,
            });

            midi_note += 1;
        }

        keys
    });

    let pressed_keys_set: std::collections::HashSet<MidiNote> =
        props.pressed_keys.iter().copied().collect();
    let keys_vec = keys();
    let keys_reversed: Vec<PianoKey> = keys_vec.iter().rev().cloned().collect();

    rsx! {
        div {
            class: "relative border border-border rounded-lg overflow-hidden bg-muted w-full h-full",
            style: "width: {props.width}; height: {props.height};",
            div {
                class: "overflow-y-auto overflow-x-hidden h-full w-full",
                style: "transform: scaleY({props.zoom}); transform-origin: top;",
                div {
                    class: "flex flex-col",
                    style: "height: calc(100% / {props.zoom});",
                    // Render all keys in order as rows
                    for (row_idx, piano_key) in keys_reversed.iter().enumerate() {
                        if piano_key.is_black {
                            // Black key - narrower, same height as white keys
                            div {
                                class: if row_idx % 2 == 0 { "bg-muted border-b border-border flex items-center" } else { "bg-muted/50 border-b border-border flex items-center" },
                                style: "height: 2rem;",
                                div {
                                    class: if pressed_keys_set.contains(&piano_key.midi_note) {
                                        "ml-auto mr-2 bg-blue-600 border border-blue-500 hover:bg-blue-700 cursor-pointer transition-colors flex items-center justify-center"
                                    } else {
                                        "ml-auto mr-2 bg-gray-900 border border-gray-700 hover:bg-gray-800 cursor-pointer transition-colors flex items-center justify-center"
                                    },
                                    style: "height: 2rem; width: 60%;",
                                    onclick: {
                                        let midi_note = piano_key.midi_note;
                                        move |_| {
                                            if let Some(handler) = &props.on_key_click {
                                                handler.call(midi_note);
                                            }
                                        }
                                    },
                                    title: "{piano_key.note_name} (MIDI {piano_key.midi_note})",
                                    span {
                                        class: "text-xs text-white select-none",
                                        "{piano_key.note_name}"
                                    }
                                }
                            }
                        } else {
                            // White key - full width
                            div {
                                class: if pressed_keys_set.contains(&piano_key.midi_note) {
                                    if row_idx % 2 == 0 {
                                        "bg-muted border-b border-border flex items-center justify-center bg-blue-400 hover:bg-blue-500 cursor-pointer transition-colors"
                                    } else {
                                        "bg-muted/50 border-b border-border flex items-center justify-center bg-blue-400 hover:bg-blue-500 cursor-pointer transition-colors"
                                    }
                                } else {
                                    if row_idx % 2 == 0 {
                                        "bg-muted border-b border-border flex items-center justify-center bg-white hover:bg-gray-100 cursor-pointer transition-colors"
                                    } else {
                                        "bg-muted/50 border-b border-border flex items-center justify-center bg-white hover:bg-gray-100 cursor-pointer transition-colors"
                                    }
                                },
                                style: "height: 2rem;",
                                onclick: {
                                    let midi_note = piano_key.midi_note;
                                    move |_| {
                                        if let Some(handler) = &props.on_key_click {
                                            handler.call(midi_note);
                                        }
                                    }
                                },
                                title: "{piano_key.note_name} (MIDI {piano_key.midi_note})",
                                span {
                                    class: "text-xs text-gray-600 select-none",
                                    "{piano_key.note_name}"
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
