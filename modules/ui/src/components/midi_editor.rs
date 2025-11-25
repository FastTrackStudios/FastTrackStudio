use dioxus::prelude::*;
use crate::components::piano::MidiNote;
use lumen_blocks::components::button::{Button, ButtonVariant};

/// MIDI Editor component - unified grid with piano and editor in sync
#[component]
pub fn MidiEditor() -> Element {
    // Zoom level (1.0 = 100%, 2.0 = 200%, etc.)
    let mut zoom = use_signal(|| 1.0);
    
    // Computed zoom percentage for display
    let zoom_percent = use_memo(move || (zoom() * 100.0) as u32);
    
    // Selected/pressed keys
    let mut selected_keys = use_signal(|| Vec::<MidiNote>::new());
    
    // Generate all 88 keys (A0 to C8)
    let keys = use_memo(move || {
        let note_names = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
        let black_key_indices = [1, 3, 6, 8, 10]; // C#, D#, F#, G#, A#
        
        let mut keys = Vec::new();
        let mut midi_note = 21; // A0
        
        while midi_note <= 108 {
            let note_index = (midi_note % 12) as usize;
            let is_black = black_key_indices.contains(&note_index);
            let note_name_str = note_names[note_index];
            let octave = (midi_note / 12) - 1;
            let note_name = format!("{}{}", note_name_str, octave);
            
            keys.push((midi_note, note_name, is_black));
            midi_note += 1;
        }
        
        // Reverse to show lowest at top
        keys.reverse();
        keys
    });
    
    let pressed_keys_set: std::collections::HashSet<MidiNote> = selected_keys().iter().copied().collect();
    
    rsx! {
        div {
            class: "flex-1 flex flex-col overflow-hidden bg-background",
            // Zoom controls
            div {
                class: "flex items-center justify-between p-2 border-b border-border",
                h3 {
                    class: "text-sm font-semibold text-foreground",
                    "MIDI Editor"
                }
                div {
                    class: "flex items-center gap-1",
                    Button {
                        variant: ButtonVariant::Ghost,
                        on_click: move |_| {
                            let new_zoom = (zoom() * 0.9f64).max(0.5f64);
                            zoom.set(new_zoom);
                        },
                        "−"
                    }
                    span {
                        class: "text-xs text-muted-foreground min-w-[3rem] text-center",
                        "{zoom_percent()}%"
                    }
                    Button {
                        variant: ButtonVariant::Ghost,
                        on_click: move |_| {
                            let new_zoom = (zoom() * 1.1f64).min(5.0f64);
                            zoom.set(new_zoom);
                        },
                        "+"
                    }
                }
            }
            
            // Unified grid - single scroll container
            div {
                class: "flex-1 overflow-y-auto overflow-x-hidden",
                style: "transform: scaleY({zoom()}); transform-origin: top;",
                div {
                    class: "flex flex-col",
                    style: "height: calc(100% / {zoom()});",
                    // Render each row with piano on left and editor on right
                    for (row_idx, (midi_note, note_name, is_black)) in keys().iter().enumerate() {
                        div {
                            class: if row_idx % 2 == 0 { "flex border-b border-border bg-muted" } else { "flex border-b border-border bg-muted/50" },
                            style: "height: 2rem; min-height: 2rem;",
                            // Piano key on the left
                            div {
                                class: "border-r border-border",
                                style: "width: 200px; min-width: 200px; flex-shrink: 0;",
                                if *is_black {
                                    // Black key - narrower, right-aligned
                                    div {
                                        class: "h-full flex items-center",
                                        div {
                                            class: if pressed_keys_set.contains(midi_note) {
                                                "ml-auto mr-2 bg-blue-600 border border-blue-500 hover:bg-blue-700 cursor-pointer transition-colors flex items-center justify-center"
                                            } else {
                                                "ml-auto mr-2 bg-gray-900 border border-gray-700 hover:bg-gray-800 cursor-pointer transition-colors flex items-center justify-center"
                                            },
                                            style: "height: 2rem; width: 60%;",
                                            onclick: {
                                                let note = *midi_note;
                                                move |_| {
                                                    let mut keys = selected_keys();
                                                    if keys.contains(&note) {
                                                        keys.retain(|&n| n != note);
                                                    } else {
                                                        keys.push(note);
                                                    }
                                                    selected_keys.set(keys);
                                                }
                                            },
                                            title: "{note_name} (MIDI {midi_note})",
                                            span {
                                                class: "text-xs text-white select-none",
                                                "{note_name}"
                                            }
                                        }
                                    }
                                } else {
                                    // White key - full width
                                    div {
                                        class: if pressed_keys_set.contains(midi_note) {
                                            "h-full bg-blue-400 hover:bg-blue-500 cursor-pointer transition-colors flex items-center justify-center"
                                        } else {
                                            "h-full bg-white hover:bg-gray-100 cursor-pointer transition-colors flex items-center justify-center"
                                        },
                                        onclick: {
                                            let note = *midi_note;
                                            move |_| {
                                                let mut keys = selected_keys();
                                                if keys.contains(&note) {
                                                    keys.retain(|&n| n != note);
                                                } else {
                                                    keys.push(note);
                                                }
                                                selected_keys.set(keys);
                                            }
                                        },
                                        title: "{note_name} (MIDI {midi_note})",
                                        span {
                                            class: "text-xs text-gray-600 select-none",
                                            "{note_name}"
                                        }
                                    }
                                }
                            }
                            
                            // MIDI editor content on the right
                            div {
                                class: "flex-1 flex items-center px-4",
                                span {
                                    class: "text-xs text-muted-foreground font-mono",
                                    "{note_name} (MIDI {midi_note})"
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

