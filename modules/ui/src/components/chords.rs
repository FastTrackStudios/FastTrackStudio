//! Chords display component
//!
//! Displays detected chords from Chart data in various notation formats

use dioxus::prelude::*;
use fts::setlist::{SETLIST_STRUCTURE, ACTIVE_INDICES};
use keyflow::{Chart, ChartSection, ChordInstance};
use keyflow::chord::Chord;
use keyflow::primitives::RootNotation;

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

/// Simple chord display data structure for UI
#[derive(Debug, Clone, PartialEq)]
pub struct ChordDisplay {
    pub name: String,
    pub nashville: Option<String>,
    pub roman: Option<String>,
    pub start_time: f64,
    pub end_time: f64,
    pub position: String, // Musical position like "1.1.000"
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
    
    // Get song name and project name for chart lookup
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
    
    let project_name = use_memo(move || {
        if let Some(song_idx) = current_song_index() {
            if let Some(setlist) = SETLIST_STRUCTURE.read().as_ref() {
                if let Some(song) = setlist.songs.get(song_idx) {
                    return song.metadata
                        .get("project_name")
                        .or_else(|| song.metadata.get("Project"))
                        .or_else(|| song.metadata.get("project"))
                        .cloned();
                }
            }
        }
        None
    });
    
    // Get chart state from global signal
    let chart_state = use_memo(move || {
        use fts::chords::CHART_STATE_SIGNAL;
        CHART_STATE_SIGNAL.read().clone()
    });
    
    // Extract chords from chart
    let chords = use_memo(move || {
        let project = project_name();
        let charts = chart_state();
        
        if let Some(proj_name) = project.as_ref() {
            if let Some(chart) = charts.get(proj_name) {
                extract_chords_from_chart(chart, proj_name)
            } else {
                Vec::new()
            }
        } else {
            Vec::new()
        }
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
                    "Chords from chart data"
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
            if chords().is_empty() {
                div {
                    class: "chords-list flex-1 overflow-y-auto",
                    div {
                        class: "no-chords text-center text-muted-foreground py-12",
                        div {
                            class: "text-lg mb-2",
                            "No chords available"
                        }
                        div {
                            class: "text-sm",
                            "Chords will appear here when chart data is available."
                        }
                    }
                }
            } else {
                div {
                    class: "chords-list flex-1 overflow-y-auto space-y-2",
                    for chord in chords() {
                        div {
                            class: "chord-item p-3 rounded-md bg-card border border-border hover:bg-accent transition-colors",
                            div {
                                class: "flex items-center justify-between",
                                div {
                                    class: "chord-display",
                                    if format() == ChordDisplayFormat::Name {
                                        span {
                                            class: "text-lg font-semibold",
                                            {chord.name.clone()}
                                        }
                                    } else if format() == ChordDisplayFormat::Nashville {
                                        if let Some(nash) = &chord.nashville {
                                            span {
                                                class: "text-lg font-semibold",
                                                {nash.clone()}
                                            }
                                        } else {
                                            span {
                                                class: "text-lg font-semibold text-muted-foreground",
                                                {chord.name.clone()}
                                            }
                                        }
                                    } else {
                                        if let Some(roman) = &chord.roman {
                                            span {
                                                class: "text-lg font-semibold",
                                                {roman.clone()}
                                            }
                                        } else {
                                            span {
                                                class: "text-lg font-semibold text-muted-foreground",
                                                {chord.name.clone()}
                                            }
                                        }
                                    }
                                }
                                div {
                                    class: "chord-position text-xs text-muted-foreground",
                                    {chord.position.clone()}
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Extract all chords from a chart and convert them to display format
fn extract_chords_from_chart(chart: &Chart, _project_name: &str) -> Vec<ChordDisplay> {
    let mut chords = Vec::new();
    let key = chart.initial_key.as_ref().or(chart.current_key.as_ref());
    
    // Iterate through all sections and measures to collect chords
    for section in &chart.sections {
        for measure in &section.measures {
            for chord_instance in &measure.chords {
                let chord_display = convert_chord_instance_to_display(chord_instance, key);
                chords.push(chord_display);
            }
        }
    }
    
    chords
}

/// Convert a ChordInstance to ChordDisplay format
fn convert_chord_instance_to_display(chord_instance: &ChordInstance, key: Option<&keyflow::Key>) -> ChordDisplay {
    // Get the chord name (standard format)
    let name = chord_instance.full_symbol.clone();
    
    // Format position
    let position = format!(
        "{}.{}.{:03}",
        chord_instance.position.total_duration.measure + 1,
        chord_instance.position.total_duration.beat + 1,
        chord_instance.position.total_duration.subdivision
    );
    
    // Convert to Nashville Number System (requires key context)
    let nashville = key.and_then(|k| {
        convert_to_nashville(&chord_instance.parsed, k)
    });
    
    // Convert to Roman Numeral (requires key context)
    let roman = key.and_then(|k| {
        convert_to_roman(&chord_instance.parsed, k)
    });
    
    // For now, we don't have time positions in ChordInstance
    // We'll use 0.0 as placeholder - in the future, we can calculate from musical position
    let start_time = 0.0;
    let end_time = 0.0;
    
    ChordDisplay {
        name,
        nashville,
        roman,
        start_time,
        end_time,
        position,
    }
}

/// Convert a chord to Nashville Number System format
fn convert_to_nashville(chord: &Chord, key: &keyflow::Key) -> Option<String> {
    // Check if it's already a scale degree
    if let Some(degree) = chord.root.scale_degree() {
        return Some(format!("{}{}", degree, format_chord_quality(chord)));
    }
    
    // Get the resolved note
    let root_note = chord.root.resolve(Some(key))?;
    
    // Find the scale degree of the root note in the key
    let scale_degree = key.degree_of_note(&root_note)?;
    
    Some(format!("{}{}", scale_degree, format_chord_quality(chord)))
}

/// Convert a chord to Roman Numeral format
fn convert_to_roman(chord: &Chord, key: &keyflow::Key) -> Option<String> {
    // Check if it's already a roman numeral
    if let Some(case) = chord.root.roman_case() {
        if let Some(degree) = chord.root.scale_degree() {
            let roman = match (degree, case) {
                (1, keyflow::primitives::RomanCase::Upper) => "I",
                (2, keyflow::primitives::RomanCase::Upper) => "II",
                (3, keyflow::primitives::RomanCase::Upper) => "III",
                (4, keyflow::primitives::RomanCase::Upper) => "IV",
                (5, keyflow::primitives::RomanCase::Upper) => "V",
                (6, keyflow::primitives::RomanCase::Upper) => "VI",
                (7, keyflow::primitives::RomanCase::Upper) => "VII",
                (1, keyflow::primitives::RomanCase::Lower) => "i",
                (2, keyflow::primitives::RomanCase::Lower) => "ii",
                (3, keyflow::primitives::RomanCase::Lower) => "iii",
                (4, keyflow::primitives::RomanCase::Lower) => "iv",
                (5, keyflow::primitives::RomanCase::Lower) => "v",
                (6, keyflow::primitives::RomanCase::Lower) => "vi",
                (7, keyflow::primitives::RomanCase::Lower) => "vii",
                _ => return None, // Invalid degree
            };
            return Some(format!("{}{}", roman, format_chord_quality(chord)));
        }
    }
    
    // Check if it's a scale degree
    if let Some(degree) = chord.root.scale_degree() {
        // Determine case based on chord quality
        let is_minor = matches!(chord.quality, keyflow::chord::ChordQuality::Minor);
        let case = if is_minor {
            keyflow::primitives::RomanCase::Lower
        } else {
            keyflow::primitives::RomanCase::Upper
        };
        
        let roman = match (degree, case) {
            (1, keyflow::primitives::RomanCase::Upper) => "I",
            (2, keyflow::primitives::RomanCase::Upper) => "II",
            (3, keyflow::primitives::RomanCase::Upper) => "III",
            (4, keyflow::primitives::RomanCase::Upper) => "IV",
            (5, keyflow::primitives::RomanCase::Upper) => "V",
            (6, keyflow::primitives::RomanCase::Upper) => "VI",
            (7, keyflow::primitives::RomanCase::Upper) => "VII",
            (1, keyflow::primitives::RomanCase::Lower) => "i",
            (2, keyflow::primitives::RomanCase::Lower) => "ii",
            (3, keyflow::primitives::RomanCase::Lower) => "iii",
            (4, keyflow::primitives::RomanCase::Lower) => "iv",
            (5, keyflow::primitives::RomanCase::Lower) => "v",
            (6, keyflow::primitives::RomanCase::Lower) => "vi",
            (7, keyflow::primitives::RomanCase::Lower) => "vii",
            _ => return None, // Invalid degree
        };
        return Some(format!("{}{}", roman, format_chord_quality(chord)));
    }
    
    // Get the resolved note
    let root_note = chord.root.resolve(Some(key))?;
    
    // Find the scale degree of the root note in the key
    let scale_degree = key.degree_of_note(&root_note)?;
    
    // Determine case based on chord quality
    let is_minor = matches!(chord.quality, keyflow::chord::ChordQuality::Minor);
    let case = if is_minor {
        keyflow::primitives::RomanCase::Lower
    } else {
        keyflow::primitives::RomanCase::Upper
    };
    
    let roman = match (scale_degree, case) {
        (1, keyflow::primitives::RomanCase::Upper) => "I",
        (2, keyflow::primitives::RomanCase::Upper) => "II",
        (3, keyflow::primitives::RomanCase::Upper) => "III",
        (4, keyflow::primitives::RomanCase::Upper) => "IV",
        (5, keyflow::primitives::RomanCase::Upper) => "V",
        (6, keyflow::primitives::RomanCase::Upper) => "VI",
        (7, keyflow::primitives::RomanCase::Upper) => "VII",
        (1, keyflow::primitives::RomanCase::Lower) => "i",
        (2, keyflow::primitives::RomanCase::Lower) => "ii",
        (3, keyflow::primitives::RomanCase::Lower) => "iii",
        (4, keyflow::primitives::RomanCase::Lower) => "iv",
        (5, keyflow::primitives::RomanCase::Lower) => "v",
        (6, keyflow::primitives::RomanCase::Lower) => "vi",
        (7, keyflow::primitives::RomanCase::Lower) => "vii",
        _ => return None, // Invalid degree
    };
    
    Some(format!("{}{}", roman, format_chord_quality(chord)))
}

/// Format chord quality/extensions for display
fn format_chord_quality(chord: &Chord) -> String {
    let mut parts = Vec::new();
    
    // Quality
    match chord.quality {
        keyflow::chord::ChordQuality::Minor => parts.push("m".to_string()),
        keyflow::chord::ChordQuality::Diminished => parts.push("dim".to_string()),
        keyflow::chord::ChordQuality::Augmented => parts.push("aug".to_string()),
        keyflow::chord::ChordQuality::Suspended(_) => parts.push("sus".to_string()),
        keyflow::chord::ChordQuality::Power => parts.push("5".to_string()),
        _ => {} // Major is default, no suffix
    }
    
    // Family (7th)
    if let Some(family) = &chord.family {
        parts.push(family.symbol().to_string());
    }
    
    // Extensions
    if chord.extensions.has_any() {
        // Format extensions manually
        if let Some(ninth) = chord.extensions.ninth {
            parts.push(match ninth {
                keyflow::chord::ExtensionQuality::Natural => "9".to_string(),
                keyflow::chord::ExtensionQuality::Flat => "b9".to_string(),
                keyflow::chord::ExtensionQuality::Sharp => "#9".to_string(),
            });
        }
        if let Some(eleventh) = chord.extensions.eleventh {
            parts.push(match eleventh {
                keyflow::chord::ExtensionQuality::Natural => "11".to_string(),
                keyflow::chord::ExtensionQuality::Flat => "b11".to_string(),
                keyflow::chord::ExtensionQuality::Sharp => "#11".to_string(),
            });
        }
        if let Some(thirteenth) = chord.extensions.thirteenth {
            parts.push(match thirteenth {
                keyflow::chord::ExtensionQuality::Natural => "13".to_string(),
                keyflow::chord::ExtensionQuality::Flat => "b13".to_string(),
                keyflow::chord::ExtensionQuality::Sharp => "#13".to_string(),
            });
        }
    }
    
    // Alterations
    for alt in &chord.alterations {
        parts.push(alt.to_string());
    }
    
    // Bass note (slash chord)
    if let Some(bass) = &chord.bass {
        parts.push(format!("/{}", bass.to_string()));
    }
    
    parts.join("")
}
