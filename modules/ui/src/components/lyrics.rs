use dioxus::prelude::*;
use lyrics::{Lyrics, parse_lyrics, output::{Slides, SlideBreakConfig, Slide}};
use lyrics::core::{SectionTypeHint, LinePart};
use std::collections::HashMap;
use setlist::SETLIST;
use crate::components::text_fit::{TextFit, TextFitMode};

/// Lyrics view component displaying slides as cards in a grid
#[component]
pub fn LyricsView() -> Element {
    // Track the selected/active slide (can be set by clicking)
    let selected_slide_index = use_signal(|| None::<usize>);
    
    // Parse example lyrics (will eventually come from REAPER)
    let lyrics_data = use_memo(move || {
        const EXAMPLE_LYRICS: &str = r#"[Intro]

(Woo)
(Are you ready?)
Well!
Well!

[Verse 1]

Drowning, fishing, dropping, screaming under the lights
I'm feeling everything crashing, burning, I lost track of time. Into you

[Chorus]

I'm breathing, I'm breathing, I think I'm reading you well
I'm breathing, I'm breathing, I think I'm reading you well

[Verse 2]

Burning, crashing, trampoline, the edge of the sun
I think I fell in love with lightning bolt, I'm ready to run. Into you

[Chorus]

I'm screaming, I'm screaming, I think I'm reading you well
Believe it, believe it, I think I'm reading you well (Well, think I'm reading you well)
(I think I'm reading you well)

[Bridge]

Waiting, I'm patient
In keeping, I'm wanting more
Picking up the loose puzzle pieces
Scattered around the floor
Linking, I'm drifting
Just waiting for you to know
Following the crumbs that I left for you
Leading to my door

Waiting, I'm patient (Hey, oh)
In keeping, I'm wanting more (Hey)
Picking up the loose puzzle pieces
Scattered around the floor
Linking, I'm drifting (Hey, oh)
Just waiting for you to know (Hey)
Following the crumbs that I left for you
Leading to my door

[Outro]

I think I'm reading, I, I think I'm reading you well
I think I'm screaming, I, I think I'm reading you well
I'm breathing, I'm breathing, I think I'm reading you well
I think I'm reading you well
(Okay)
"#;
        
        parse_lyrics(EXAMPLE_LYRICS, "Well".to_string()).ok()
    });
    
    // Generate slides with config
    let slides = use_memo(move || {
        if let Some(lyrics) = lyrics_data() {
            let config = SlideBreakConfig {
                max_chars: 120,
                max_words: 19,
                min_chars_to_bundle: 32,
                min_words_to_bundle: 7,
            };
            Slides::generate_with_config(&lyrics, config)
        } else {
            Vec::new()
        }
    });
    
    // Pre-compute section type map for efficient lookup
    let section_type_map = use_memo(move || {
        let mut map = HashMap::new();
        if let Some(lyrics) = lyrics_data() {
            for section in &lyrics.sections {
                map.insert(section.name.clone(), section.section_type.clone());
            }
        }
        map
    });
    
    // Get current slide - prioritize selected slide, then active section from setlist, then first slide
    let current_slide = use_memo(move || {
        let slides_vec = slides();
        if slides_vec.is_empty() {
            return None;
        }
        
        // If user has selected a slide, use that
        if let Some(selected_idx) = selected_slide_index() {
            if let Some(slide) = slides_vec.get(selected_idx) {
                return Some(slide.clone());
            }
        }
        
        // Try to get active section from setlist
        #[cfg(not(target_arch = "wasm32"))]
        {
            if let Some(setlist_api) = SETLIST.read().as_ref() {
                if let Some(active_song_idx) = setlist_api.active_song_index() {
                    if let Some(song) = setlist_api.get_setlist().songs.get(active_song_idx) {
                        if let Some(active_section_idx) = setlist_api.active_section_index() {
                            if let Some(section) = song.sections.get(active_section_idx) {
                                // Find slide matching this section name
                                for (idx, slide) in slides_vec.iter().enumerate() {
                                    if slide.section_name == section.name {
                                        return Some(slide.clone());
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        
        // Default to first slide if no match found
        Some(slides_vec[0].clone())
    });
    
    // Get next slide (for confidence monitor)
    let next_slide = use_memo(move || {
        let slides_vec = slides();
        if slides_vec.is_empty() {
            return None;
        }
        
        // Get the index of the current slide
        let current_idx = if let Some(selected_idx) = selected_slide_index() {
            selected_idx
        } else {
            // Try to find current slide index from setlist
            #[cfg(not(target_arch = "wasm32"))]
            {
                if let Some(setlist_api) = SETLIST.read().as_ref() {
                    if let Some(active_song_idx) = setlist_api.active_song_index() {
                        if let Some(song) = setlist_api.get_setlist().songs.get(active_song_idx) {
                            if let Some(active_section_idx) = setlist_api.active_section_index() {
                                if let Some(section) = song.sections.get(active_section_idx) {
                                    // Find slide matching this section name
                                    for (idx, slide) in slides_vec.iter().enumerate() {
                                        if slide.section_name == section.name {
                                            return slides_vec.get(idx + 1).cloned();
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            // Default to first slide's next
            return slides_vec.get(1).cloned();
        };
        
        // Get next slide after current
        slides_vec.get(current_idx + 1).cloned()
    });
    
    
    // Auto-select slide when it matches active section (outside of memo to avoid borrow issues)
    use_effect({
        let mut selected_slide_index = selected_slide_index.clone();
        move || {
            let slides_vec = slides();
            if slides_vec.is_empty() || selected_slide_index().is_some() {
                return;
            }
            
            #[cfg(not(target_arch = "wasm32"))]
            {
                if let Some(setlist_api) = SETLIST.read().as_ref() {
                    if let Some(active_song_idx) = setlist_api.active_song_index() {
                        if let Some(song) = setlist_api.get_setlist().songs.get(active_song_idx) {
                            if let Some(active_section_idx) = setlist_api.active_section_index() {
                                if let Some(section) = song.sections.get(active_section_idx) {
                                    // Find slide matching this section name and select it
                                    for (idx, slide) in slides_vec.iter().enumerate() {
                                        if slide.section_name == section.name {
                                            selected_slide_index.set(Some(idx));
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            
            // Default to first slide if nothing selected
            if selected_slide_index().is_none() {
                selected_slide_index.set(Some(0));
            }
        }
    });
    
    // Helper function to get border color class from section type
    let get_border_color_class = |section_type: Option<&SectionTypeHint>| -> &'static str {
        match section_type {
            Some(SectionTypeHint::Intro) => "border-cyan-500",
            Some(SectionTypeHint::Verse) => "border-green-500",
            Some(SectionTypeHint::Chorus) => "border-purple-500",
            Some(SectionTypeHint::Bridge) => "border-yellow-500",
            Some(SectionTypeHint::Outro) => "border-red-500",
            Some(SectionTypeHint::Instrumental) => "border-gray-500",
            Some(SectionTypeHint::PreChorus) | Some(SectionTypeHint::PostChorus) | Some(SectionTypeHint::Custom(_)) => "border-gray-500",
            None => "border-gray-500",
        }
    };
    
    // Helper function to get background color class from section type (for tab)
    let get_tab_bg_color_class = |section_type: Option<&SectionTypeHint>| -> &'static str {
        match section_type {
            Some(SectionTypeHint::Intro) => "bg-cyan-500",
            Some(SectionTypeHint::Verse) => "bg-green-500",
            Some(SectionTypeHint::Chorus) => "bg-purple-500",
            Some(SectionTypeHint::Bridge) => "bg-yellow-500",
            Some(SectionTypeHint::Outro) => "bg-red-500",
            Some(SectionTypeHint::Instrumental) => "bg-gray-500",
            Some(SectionTypeHint::PreChorus) | Some(SectionTypeHint::PostChorus) | Some(SectionTypeHint::Custom(_)) => "bg-gray-500",
            None => "bg-gray-500",
        }
    };
    
    // Compute slide classes with selection state
    let slide_classes = use_memo(move || {
        let selected_idx = selected_slide_index();
        slides().iter().enumerate().map(|(idx, slide)| {
            let border_color = get_border_color_class(section_type_map().get(&slide.section_name).and_then(|opt| opt.as_ref()));
            let selection_class = if selected_idx == Some(idx) {
                "ring-4 ring-primary ring-offset-2"
            } else {
                ""
            };
            format!("rounded-lg border-2 {} overflow-hidden cursor-pointer transition-all hover:scale-105 flex flex-col {}", border_color, selection_class)
        }).collect::<Vec<_>>()
    });
    
    // Compute tab background colors
    let tab_bg_colors = use_memo(move || {
        slides().iter().map(|slide| {
            get_tab_bg_color_class(section_type_map().get(&slide.section_name).and_then(|opt| opt.as_ref()))
        }).collect::<Vec<_>>()
    });
    
    // Compute slide text for Audience Preview
    let audience_slide_text = use_memo(move || {
        current_slide().map(|slide| {
            let mut text = String::new();
            for (line_idx, line) in slide.lines.iter().enumerate() {
                if line_idx > 0 {
                    text.push('\n');
                }
                for part in line.parts.iter() {
                    match part {
                        LinePart::Regular(t) => {
                            text.push_str(t);
                            text.push(' ');
                        }
                        LinePart::Parenthetical(t) => {
                            text.push_str(&format!("({}) ", t));
                        }
                    }
                }
            }
            text
        })
    });
    
    // Compute slide text for Confidence Monitor (current and next)
    let confidence_current_text = use_memo(move || {
        current_slide().map(|slide| {
            let mut text = String::new();
            for (line_idx, line) in slide.lines.iter().enumerate() {
                if line_idx > 0 {
                    text.push('\n');
                }
                for part in line.parts.iter() {
                    match part {
                        LinePart::Regular(t) => {
                            text.push_str(t);
                            text.push(' ');
                        }
                        LinePart::Parenthetical(t) => {
                            text.push_str(&format!("({}) ", t));
                        }
                    }
                }
            }
            text
        })
    });
    
    let confidence_next_text = use_memo(move || {
        next_slide().map(|slide| {
            let mut text = String::new();
            for (line_idx, line) in slide.lines.iter().enumerate() {
                if line_idx > 0 {
                    text.push('\n');
                }
                for part in line.parts.iter() {
                    match part {
                        LinePart::Regular(t) => {
                            text.push_str(t);
                            text.push(' ');
                        }
                        LinePart::Parenthetical(t) => {
                            text.push_str(&format!("({}) ", t));
                        }
                    }
                }
            }
            text
        })
    });
    
    rsx! {
        div {
            class: "flex-1 flex overflow-hidden bg-background",
            // Main content area with grid
            div {
                class: "flex-1 overflow-y-auto p-6",
                div {
                    class: "max-w-7xl mx-auto",
                    h1 {
                        class: "text-3xl font-bold text-foreground mb-6",
                        if let Some(lyrics) = lyrics_data() {
                            "{lyrics.song_name}"
                        } else {
                            "Lyrics"
                        }
                    }
                    
                    div {
                        class: "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 gap-4",
                    for (idx, slide) in slides().iter().enumerate() {
                        div {
                            key: "{idx}",
                            class: "{slide_classes().get(idx).cloned().unwrap_or_default()}",
                            onclick: {
                                let mut selected_idx = selected_slide_index.clone();
                                move |_| {
                                    selected_idx.set(Some(idx));
                                }
                            },
                            // Section name tab on top (outside aspect ratio)
                            div {
                                class: "{tab_bg_colors().get(idx).cloned().unwrap_or_default()} text-white text-xs font-semibold px-3 py-1 uppercase tracking-wide rounded-t-lg",
                                "{slide.section_name}"
                            }
                            
                            // Slide content (16:9 aspect ratio, centered)
                            div {
                                class: "aspect-[16/9] flex flex-col items-center justify-center p-4 bg-black",
                                for line in slide.lines.iter() {
                                    div {
                                        class: "text-white text-center leading-relaxed mb-1",
                                        for part in line.parts.iter() {
                                            if let LinePart::Regular(text) = part {
                                                span { "{text}" }
                                            }
                                            if let LinePart::Parenthetical(text) = part {
                                                span {
                                                    class: "text-gray-400 italic",
                                                    "({text})"
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
            
            // Preview Sidebar
            div {
                class: "w-96 flex-shrink-0 border-l border-border bg-card overflow-y-auto",
                div {
                    class: "p-4 space-y-6",
                    // Audience Preview
                    div {
                        class: "space-y-2",
                        h2 {
                            class: "text-sm font-semibold text-foreground uppercase tracking-wide",
                            "Audience Preview"
                        }
                        if let Some(slide_text) = audience_slide_text() {
                            div {
                                class: "shadow-lg rounded-lg overflow-hidden border-2 border-border",
                                div {
                                    class: "relative w-full bg-black",
                                    style: "aspect-ratio: 16 / 9;",
                                    div {
                                        class: "absolute inset-0 p-8",
                                        TextFit {
                                            text: slide_text,
                                            class: Some("text-white leading-relaxed font-medium".to_string()),
                                            mode: TextFitMode::Multiline,
                                        }
                                    }
                                }
                            }
                        } else {
                            div {
                                class: "relative w-full bg-black rounded-lg overflow-hidden flex items-center justify-center",
                                style: "aspect-ratio: 16 / 9;",
                                div {
                                    class: "text-gray-500 text-sm",
                                    "No slide selected"
                                }
                            }
                        }
                    }
                    
                    // Confidence Monitor Preview
                    div {
                        class: "space-y-2",
                        h2 {
                            class: "text-sm font-semibold text-foreground uppercase tracking-wide",
                            "Confidence Monitor Preview"
                        }
                        // Test sizes for scaling verification
                        div {
                            class: "space-y-4 mb-4",
                            // Small size test
                            div {
                                class: "space-y-1",
                                div {
                                    class: "text-xs text-muted-foreground",
                                    "Test: Small (w-48)"
                                }
                                div {
                                    class: "@container w-48 shadow-lg rounded-lg overflow-hidden border border-border",
                                    style: "aspect-ratio: 16 / 9;",
                                    div {
                                        class: "relative w-full h-1/2 bg-black overflow-hidden",
                                        if let Some(slide) = current_slide() {
                                            div {
                                                class: "absolute top-1 left-1 px-1 py-0.5 bg-gray-800/80 rounded text-[8px] font-semibold text-gray-300 uppercase tracking-wider z-10",
                                                "{slide.section_name}"
                                            }
                                            if let Some(slide_text) = confidence_current_text() {
                                                div {
                                                    class: "absolute inset-0 p-1",
                                                    TextFit {
                                                        text: slide_text,
                                                        class: Some("text-white leading-tight font-medium".to_string()),
                                                        mode: TextFitMode::Box,
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    div {
                                        class: "h-0.5 bg-border"
                                    }
                                    div {
                                        class: "relative w-full h-1/2 bg-black overflow-hidden",
                                        if let Some(slide) = next_slide() {
                                            if let Some(slide_text) = confidence_next_text() {
                                                div {
                                                    class: "absolute inset-0 p-1",
                                                    TextFit {
                                                        text: slide_text,
                                                        class: Some("text-yellow-200 leading-tight font-medium".to_string()),
                                                        mode: TextFitMode::Box,
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            // Medium size test
                            div {
                                class: "space-y-1",
                                div {
                                    class: "text-xs text-muted-foreground",
                                    "Test: Medium (w-64)"
                                }
                                div {
                                    class: "@container w-64 shadow-lg rounded-lg overflow-hidden border border-border",
                                    style: "aspect-ratio: 16 / 9;",
                                    div {
                                        class: "relative w-full h-1/2 bg-black overflow-hidden",
                                        if let Some(slide) = current_slide() {
                                            div {
                                                class: "absolute top-1 left-1 px-1 py-0.5 bg-gray-800/80 rounded text-[8px] font-semibold text-gray-300 uppercase tracking-wider z-10",
                                                "{slide.section_name}"
                                            }
                                            if let Some(slide_text) = confidence_current_text() {
                                                div {
                                                    class: "absolute inset-0 p-1",
                                                    TextFit {
                                                        text: slide_text,
                                                        class: Some("text-white leading-tight font-medium".to_string()),
                                                        mode: TextFitMode::Box,
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    div {
                                        class: "h-0.5 bg-border"
                                    }
                                    div {
                                        class: "relative w-full h-1/2 bg-black overflow-hidden",
                                        if let Some(slide) = next_slide() {
                                            if let Some(slide_text) = confidence_next_text() {
                                                div {
                                                    class: "absolute inset-0 p-1",
                                                    TextFit {
                                                        text: slide_text,
                                                        class: Some("text-yellow-200 leading-tight font-medium".to_string()),
                                                        mode: TextFitMode::Box,
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            // Large size test
                            div {
                                class: "space-y-1",
                                div {
                                    class: "text-xs text-muted-foreground",
                                    "Test: Large (w-80)"
                                }
                                div {
                                    class: "@container w-80 shadow-lg rounded-lg overflow-hidden border border-border",
                                    style: "aspect-ratio: 16 / 9;",
                                    div {
                                        class: "relative w-full h-1/2 bg-black overflow-hidden",
                                        if let Some(slide) = current_slide() {
                                            div {
                                                class: "absolute top-1 left-1 px-1 py-0.5 bg-gray-800/80 rounded text-[8px] font-semibold text-gray-300 uppercase tracking-wider z-10",
                                                "{slide.section_name}"
                                            }
                                            if let Some(slide_text) = confidence_current_text() {
                                                div {
                                                    class: "absolute inset-0 p-1",
                                                    TextFit {
                                                        text: slide_text,
                                                        class: Some("text-white leading-tight font-medium".to_string()),
                                                        mode: TextFitMode::Box,
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    div {
                                        class: "h-0.5 bg-border"
                                    }
                                    div {
                                        class: "relative w-full h-1/2 bg-black overflow-hidden",
                                        if let Some(slide) = next_slide() {
                                            if let Some(slide_text) = confidence_next_text() {
                                                div {
                                                    class: "absolute inset-0 p-1",
                                                    TextFit {
                                                        text: slide_text,
                                                        class: Some("text-yellow-200 leading-tight font-medium".to_string()),
                                                        mode: TextFitMode::Box,
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        // Full size preview
                        div {
                            class: "@container shadow-lg rounded-lg overflow-hidden border-2 border-border",
                            style: "aspect-ratio: 16 / 9;",
                            // Current slide (top half)
                            div {
                                class: "relative w-full h-1/2 bg-black overflow-hidden",
                                if let Some(slide) = current_slide() {
                                    // Section name tag (top left)
                                    div {
                                        class: "absolute top-2 left-2 px-2 py-1 bg-gray-800/80 rounded text-[10px] font-semibold text-gray-300 uppercase tracking-wider z-10",
                                        "{slide.section_name}"
                                    }
                                    // Slide content using TextFit
                                    if let Some(slide_text) = confidence_current_text() {
                                        div {
                                            class: "absolute inset-0 p-2",
                                            TextFit {
                                                text: slide_text,
                                                class: Some("text-white leading-tight font-medium".to_string()),
                                                mode: TextFitMode::Box,
                                            }
                                        }
                                    }
                                } else {
                                    div {
                                        class: "absolute inset-0 flex items-center justify-center",
                                        div {
                                            class: "text-gray-500 text-sm",
                                            "No slide selected"
                                        }
                                    }
                                }
                            }
                            
                            // Divider
                            div {
                                class: "h-0.5 bg-border"
                            }
                            
                            // Next slide (bottom half)
                            div {
                                class: "relative w-full h-1/2 bg-black overflow-hidden",
                                if let Some(slide) = next_slide() {
                                    // Section name tag (top left)
                                    div {
                                        class: "absolute top-2 left-2 px-2 py-1 bg-gray-800/80 rounded text-[10px] font-semibold text-gray-300 uppercase tracking-wider z-10",
                                        "{slide.section_name}"
                                    }
                                    // Slide content using TextFit (yellow tint)
                                    if let Some(slide_text) = confidence_next_text() {
                                        div {
                                            class: "absolute inset-0 p-2",
                                            TextFit {
                                                text: slide_text,
                                                class: Some("text-yellow-200 leading-tight font-medium".to_string()),
                                                mode: TextFitMode::Box,
                                            }
                                        }
                                    }
                                } else {
                                    div {
                                        class: "absolute inset-0 flex items-center justify-center",
                                        div {
                                            class: "text-gray-500 text-sm",
                                            "No next slide"
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
