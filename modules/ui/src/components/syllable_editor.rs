//! Syllable editor component for editing notes and rhythms of each syllable

use dioxus::prelude::*;
use lyrics::core::{Lyrics, LyricSection, LyricLine, Word, Syllable, split_line_into_words};
use crate::components::piano::{Piano, PianoProps, MidiNote};
use lucide_dioxus::{Play, Pause, Square};

/// Props for the syllable editor
#[derive(Props, Clone, PartialEq)]
pub struct SyllableEditorProps {
    /// Lyrics data to edit
    pub lyrics: Option<Lyrics>,
    /// Callback when a syllable's MIDI note is changed
    #[props(default)]
    pub on_syllable_note_change: Option<Callback<(SyllableKey, Option<u8>)>>,
    /// Callback when a syllable's timing is changed
    #[props(default)]
    pub on_syllable_timing_change: Option<Callback<(SyllableKey, Option<f64>, Option<f64>)>>,
}

/// Key to identify a specific syllable
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SyllableKey {
    pub section_idx: usize,
    pub line_idx: usize,
    pub word_idx: usize,
    pub syllable_idx: usize,
}

/// Syllable editor component
#[component]
pub fn SyllableEditor(props: SyllableEditorProps) -> Element {
    let mut selected_syllable = use_signal::<Option<SyllableKey>>(|| None);
    let mut piano_zoom = use_signal(|| 1.0f64);
    
    // Playback state
    let mut is_playing = use_signal(|| false);
    let mut current_time = use_signal(|| 0.0f64);
    let mut playback_start_time = use_signal(|| None::<f64>);
    let mut paused_time = use_signal(|| 0.0f64);
    
    // Compute zoom percentage for display
    let zoom_percent = use_memo(move || (piano_zoom() * 100.0) as u32);
    
    // Update current time during playback using a simple interval
    use_effect(move || {
        if !is_playing() {
            return;
        }
        
        // Start playback if not already started
        if playback_start_time().is_none() {
            #[cfg(target_arch = "wasm32")]
            {
                let now = js_sys::Date::now() / 1000.0;
                playback_start_time.set(Some(now));
            }
            #[cfg(not(target_arch = "wasm32"))]
            {
                use std::time::{SystemTime, UNIX_EPOCH};
                let now = SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .unwrap()
                    .as_secs_f64();
                playback_start_time.set(Some(now));
            }
        }
        
        // Schedule periodic updates
        let mut playing = is_playing;
        let mut start_time = playback_start_time;
        let mut paused = paused_time;
        let mut current = current_time;
        
        spawn(async move {
            loop {
                if !playing() {
                    break;
                }
                
                #[cfg(target_arch = "wasm32")]
                {
                    if let Some(start) = start_time() {
                        let now = js_sys::Date::now() / 1000.0;
                        let elapsed = now - start;
                        current.set(paused() + elapsed);
                    }
                    // Use web API for timing
                    wasm_bindgen_futures::JsFuture::from(
                        js_sys::Promise::new(&mut |resolve, _| {
                            let window = web_sys::window().unwrap();
                            window.set_timeout_with_callback_and_timeout_and_arguments_0(
                                &resolve,
                                16, // ~60fps
                            ).unwrap();
                        })
                    ).await.ok();
                }
                #[cfg(not(target_arch = "wasm32"))]
                {
                    if let Some(start) = start_time() {
                        use std::time::{SystemTime, UNIX_EPOCH};
                        let now = SystemTime::now()
                            .duration_since(UNIX_EPOCH)
                            .unwrap()
                            .as_secs_f64();
                        let elapsed = now - start;
                        current.set(paused() + elapsed);
                    }
                    // Simple delay for desktop
                    std::thread::sleep(std::time::Duration::from_millis(16));
                }
            }
        });
    });
    
    // Parse lyrics into words and syllables
    let syllable_data = use_memo(move || {
        let lyrics = match &props.lyrics {
            Some(l) => l,
            None => return Vec::new(),
        };
        
        let mut result = Vec::new();
        
        for (section_idx, section) in lyrics.sections.iter().enumerate() {
            for (line_idx, line) in section.lines.iter().enumerate() {
                // Skip parenthetical lines for now
                let regular_text = line.regular_text();
                if regular_text.trim().is_empty() {
                    continue;
                }
                
                let words = split_line_into_words(&regular_text);
                
                for (word_idx, word) in words.iter().enumerate() {
                    for (syllable_idx, syllable) in word.syllables.iter().enumerate() {
                        result.push((
                            SyllableKey {
                                section_idx,
                                line_idx,
                                word_idx,
                                syllable_idx,
                            },
                            section.name.clone(),
                            line.text.clone(),
                            word.text.clone(),
                            syllable.clone(),
                        ));
                    }
                }
            }
        }
        
        result
    });
    
    // Group syllables by line for display
    let lines_data = use_memo(move || {
        let mut lines: Vec<(usize, usize, String, Vec<(SyllableKey, String, Syllable)>)> = Vec::new();
        
        for (key, section_name, line_text, word_text, syllable) in syllable_data().iter() {
            // Find or create line entry
            let line_key = (key.section_idx, key.line_idx);
            let line_entry = lines.iter_mut().find(|(s_idx, l_idx, _, _)| *s_idx == line_key.0 && *l_idx == line_key.1);
            
            if let Some(entry) = line_entry {
                entry.3.push((key.clone(), word_text.clone(), syllable.clone()));
            } else {
                lines.push((line_key.0, line_key.1, line_text.clone(), vec![(key.clone(), word_text.clone(), syllable.clone())]));
            }
        }
        
        lines
    });
    
    // Find currently active syllable based on current_time
    let active_syllable_key = use_memo(move || {
        let time = current_time();
        syllable_data().iter()
            .find(|(_, _, _, _, syllable)| {
                if let (Some(start), Some(end)) = (syllable.start_time, syllable.end_time) {
                    time >= start && time <= end
                } else {
                    false
                }
            })
            .map(|(key, _, _, _, _)| key.clone())
    });
    
    // Find total duration of all syllables
    let max_duration = use_memo(move || {
        syllable_data().iter()
            .filter_map(|(_, _, _, _, syllable)| syllable.end_time)
            .fold(0.0f64, |max, end| max.max(end))
    });
    
    rsx! {
        div {
            class: "flex flex-col h-full w-full overflow-hidden bg-background",
            // Transport controls at top
            div {
                class: "flex items-center gap-4 p-4 border-b border-border bg-card",
                // Play/Pause button
                button {
                    class: "px-4 py-2 rounded-lg bg-primary text-primary-foreground hover:bg-primary/90 transition-colors flex items-center gap-2",
                    onclick: move |_| {
                        if is_playing() {
                            // Pause
                            paused_time.set(current_time());
                            playback_start_time.set(None);
                            is_playing.set(false);
                        } else {
                            // Play
                            is_playing.set(true);
                        }
                    },
                    if is_playing() {
                        Pause { size: 20, color: "currentColor" }
                    } else {
                        Play { size: 20, color: "currentColor" }
                    }
                    if is_playing() { "Pause" } else { "Play" }
                }
                
                // Stop button
                button {
                    class: "px-4 py-2 rounded-lg border border-border hover:bg-muted transition-colors flex items-center gap-2",
                        onclick: move |_| {
                            is_playing.set(false);
                            current_time.set(0.0);
                            paused_time.set(0.0);
                            playback_start_time.set(None);
                        },
                    Square { size: 16, color: "currentColor" }
                    "Stop"
                }
                
                // Time display
                div {
                    class: "flex items-center gap-2 ml-auto",
                    span {
                        class: "text-sm font-mono text-muted-foreground",
                        "Time: {current_time():.2}s"
                    }
                    if max_duration() > 0.0 {
                        span {
                            class: "text-sm font-mono text-muted-foreground",
                            "/ {max_duration():.2}s"
                        }
                    }
                }
                
                // Timeline scrubber
                if max_duration() > 0.0 {
                    div {
                        class: "flex-1 max-w-md",
                        input {
                            r#type: "range",
                            min: "0",
                            max: "{max_duration()}",
                            step: "0.01",
                            value: "{current_time()}",
                            class: "w-full",
                            oninput: move |evt| {
                                if let Ok(val) = evt.value().parse::<f64>() {
                                    current_time.set(val);
                                    paused_time.set(val);
                                    if !is_playing() {
                                        playback_start_time.set(None);
                                    }
                                }
                            },
                        }
                    }
                }
            }
            
            // Piano sidebar and main editor area
            div {
                class: "flex flex-1 overflow-hidden",
                // Piano sidebar (left)
                div {
                    class: "w-[200px] flex flex-col border-r border-border bg-muted/30",
                    // Piano zoom controls
                    div {
                        class: "p-2 border-b border-border flex items-center gap-2",
                        button {
                            class: "px-2 py-1 text-xs rounded bg-background border border-border hover:bg-muted",
                            onclick: move |_| {
                                piano_zoom.set((piano_zoom() * 0.9).max(0.5));
                            },
                            "-"
                        }
                        span {
                            class: "text-xs text-muted-foreground flex-1 text-center",
                            "{zoom_percent()}%"
                        }
                        button {
                            class: "px-2 py-1 text-xs rounded bg-background border border-border hover:bg-muted",
                            onclick: move |_| {
                                piano_zoom.set((piano_zoom() * 1.1).min(3.0));
                            },
                            "+"
                        }
                    }
                    // Piano component
                    div {
                        class: "flex-1 overflow-y-auto",
                        style: "transform: scaleY({piano_zoom()}); transform-origin: top;",
                        Piano {
                            width: "200px".to_string(),
                            height: "100%".to_string(),
                            zoom: 1.0,
                            on_key_click: {
                                let selected = selected_syllable;
                                let on_change = props.on_syllable_note_change.clone();
                                Some(EventHandler::new(move |midi_note: MidiNote| {
                                    if let Some(key) = selected() {
                                        if let Some(cb) = on_change.as_ref() {
                                            cb.call((key, Some(midi_note)));
                                        }
                                    }
                                }))
                            },
                            pressed_keys: {
                                // Highlight selected syllable's note if it has one
                                let selected = selected_syllable();
                                if let Some(key) = selected {
                                    if let Some((_, _, _, _, syllable)) = syllable_data().iter().find(|(k, _, _, _, _)| *k == key) {
                                        if let Some(note) = syllable.midi_note {
                                            vec![note as MidiNote]
                                        } else {
                                            vec![]
                                        }
                                    } else {
                                        vec![]
                                    }
                                } else {
                                    vec![]
                                }
                            },
                        }
                    }
                }
                
                // Main editor area (right)
                div {
                    class: "flex-1 overflow-y-auto",
                    div {
                        class: "p-4 space-y-6",
                        if lines_data().is_empty() {
                            div {
                                class: "text-center text-muted-foreground py-8",
                                "No lyrics data available"
                            }
                        } else {
                            for (section_idx, line_idx, line_text, syllables) in lines_data().iter() {
                                div {
                                    class: "space-y-2",
                                    // Line header
                                    div {
                                        class: "text-sm font-semibold text-foreground/70 mb-2",
                                        "{line_text}"
                                    }
                                    
                                    // Syllables in this line
                                    div {
                                        class: "flex flex-wrap items-center gap-2",
                                        for (key, word_text, syllable) in syllables.iter() {
                                            div {
                                                class: "flex items-center gap-1",
                                                // Word separator (if first syllable of word)
                                                if key.syllable_idx == 0 {
                                                    span {
                                                        class: "text-muted-foreground/50",
                                                        " "
                                                    }
                                                }
                                                
                                                // Syllable button
                                                {
                                                    let key_clone = key.clone();
                                                    let selected = selected_syllable();
                                                    let is_selected = selected == Some(key_clone.clone());
                                                    let has_note = syllable.midi_note.is_some();
                                                    let is_active = active_syllable_key() == Some(key_clone.clone());
                                                    
                                                    rsx! {
                                                        button {
                                                            class: if is_active {
                                                                "px-2 py-1 rounded border-2 border-green-500 bg-green-500/20 text-foreground animate-pulse"
                                                            } else if is_selected {
                                                                "px-2 py-1 rounded border-2 border-primary bg-primary/20 text-foreground"
                                                            } else if has_note {
                                                                "px-2 py-1 rounded border border-primary/50 bg-primary/10 text-foreground hover:border-primary"
                                                            } else {
                                                                "px-2 py-1 rounded border border-border bg-muted/50 text-foreground hover:border-primary/50"
                                                            },
                                                            onclick: move |_| {
                                                                let current = selected_syllable();
                                                                if current == Some(key_clone.clone()) {
                                                                    selected_syllable.set(None);
                                                                } else {
                                                                    selected_syllable.set(Some(key_clone.clone()));
                                                                }
                                                            },
                                                            // Syllable text
                                                            span {
                                                                "{syllable.text}"
                                                            }
                                                            
                                                            // MIDI note indicator
                                                            if let Some(note) = syllable.midi_note {
                                                                span {
                                                                    class: "ml-1 text-xs text-primary/70",
                                                                    "({note})"
                                                                }
                                                            }
                                                            
                                                            // Timing indicator
                                                            if let (Some(start), Some(end)) = (syllable.start_time, syllable.end_time) {
                                                                span {
                                                                    class: "ml-1 text-xs text-muted-foreground",
                                                                    "[{start:.2}s-{end:.2}s]"
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

