use dioxus::prelude::*;
use fts::fts::lyrics::{Lyrics, output::{Slides, SlideBreakConfig, Slide}};
use fts::fts::lyrics::core::{SectionTypeHint, LinePart};
use std::collections::HashMap;
use fts::fts::setlist::{SETLIST_STRUCTURE, ACTIVE_INDICES};
use crate::components::text_fit::{TextFit, TextFitMode};
use crate::components::syllable_editor::{SyllableEditor, SyllableEditorProps, SyllableKey};
use crate::reactive_state::{use_lyrics_for_active_song, use_active_slide_for_active_song};

/// Lyrics view component - full-screen confidence monitor style display
#[component]
pub fn LyricsView() -> Element {
    // Get lyrics from current song using reactive state hook
    let lyrics_data = use_lyrics_for_active_song();
    
    // Get active slide index using reactive state hook (must be called at component level)
    let active_slide_idx = use_active_slide_for_active_song();
    
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
    
    // Get current slide based on active_slide_index from REAPER (reactive signal)
    // Read the signal inside the memo so Dioxus tracks it for reactivity
    let current_slide = use_memo(move || -> Option<Slide> {
        let slides_vec = slides();
        if slides_vec.is_empty() {
            return None;
        }
        
        // Get active slide index from hook (called at component level)
        let active_slide_idx_val = active_slide_idx();
        if let Some(slide_idx) = active_slide_idx_val {
            if let Some(slide) = slides_vec.get(slide_idx) {
                return Some(slide.clone());
            }
        }
        
        // Fallback: try to get from active section if slide index not available
        let setlist_structure = SETLIST_STRUCTURE.read();
        let active_indices = ACTIVE_INDICES.read();
        if let Some(active_song_idx) = active_indices.0 {
            if let Some(setlist) = setlist_structure.as_ref() {
                if let Some(song) = setlist.songs.get(active_song_idx) {
                    if let Some(active_section_idx) = active_indices.1 {
                        if let Some(section) = song.sections.get(active_section_idx) {
                            // Find slide matching this section name
                            for slide in slides_vec.iter() {
                                if slide.section_name == section.name {
                                    return Some(slide.clone());
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
        
        // Get the index of the current slide from hook (called at component level)
        let active_slide_idx_val = active_slide_idx();
        if let Some(current_idx) = active_slide_idx_val {
            // Get next slide after current
            return slides_vec.get(current_idx + 1).cloned();
        }
        
        None
    });
    
    // Compute slide text for current slide (confidence monitor)
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
    
    // Compute slide text for next slide (confidence monitor - yellow tint)
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
            class: "flex-1 flex overflow-hidden bg-black",
            // Confidence monitor style display (split view: current on top, next on bottom)
            div {
                class: "relative w-full h-full bg-black overflow-hidden flex flex-col",
                // Current slide (top half)
                div {
                    class: "@container relative w-full flex-1 bg-black overflow-hidden",
                    if let Some(_slide) = current_slide() {
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
                                "No lyrics available"
                            }
                        }
                    }
                }
                
                // Divider
                div {
                    class: "h-0.5 bg-border flex-shrink-0"
                }
                
                // Next slide (bottom half)
                div {
                    class: "@container relative w-full flex-1 bg-black overflow-hidden",
                    if let Some(_slide) = next_slide() {
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
use crate::components::layout::EditViewMode;
use dioxus::prelude::use_context;

/// Edit mode context (defined in main.rs, re-exported here for convenience)
#[derive(Clone)]
pub struct EditModeCtx {
    pub edit_mode: Signal<bool>,
    pub edit_view_mode: Signal<Option<EditViewMode>>,
    pub on_edit_view_change: Callback<EditViewMode>,
}

/// Hook to get edit mode context
pub fn use_edit_mode() -> Option<EditModeCtx> {
    Some(use_context::<EditModeCtx>())
}

/// Lyrics edit view component - grid with colors and preview sidebar
#[component]
pub fn LyricsEditView() -> Element {
    // Get edit mode context from parent - must be provided by AppLayout
    let edit_ctx = match use_edit_mode() {
        Some(ctx) => ctx,
        None => {
            // Fallback if context not available (shouldn't happen, but handle gracefully)
            return rsx! {
                div {
                    class: "flex-1 flex items-center justify-center",
                    "Edit mode context not available"
                }
            };
        }
    };
    
    let edit_view_mode = edit_ctx.edit_view_mode;
    let on_edit_view_change = edit_ctx.on_edit_view_change;
    
    // Initialize edit_view_mode to Slides if not set
    use_effect(move || {
        if edit_view_mode().is_none() {
            on_edit_view_change.call(EditViewMode::Slides);
        }
    });
    
    // Track the selected/active slide (can be set by clicking)
    let selected_slide_index = use_signal(|| None::<usize>);
    
    // Get lyrics from current song using reactive state hook
    let lyrics_data = use_lyrics_for_active_song();
    
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
    
    // Pre-compute section color map from setlist (for actual colors)
    let section_color_map = use_memo(move || {
        let mut map = HashMap::new();
        #[cfg(not(target_arch = "wasm32"))]
        {
            let setlist_structure = SETLIST_STRUCTURE.read();
            let active_indices = ACTIVE_INDICES.read();
            if let Some(active_song_idx) = active_indices.0 {
                if let Some(setlist) = setlist_structure.as_ref() {
                    if let Some(song) = setlist.songs.get(active_song_idx) {
                        for section in &song.sections {
                            map.insert(section.name.clone(), section.color_bright());
                        }
                    }
                }
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
            let setlist_structure = SETLIST_STRUCTURE.read();
            let active_indices = ACTIVE_INDICES.read();
            if let Some(active_song_idx) = active_indices.0 {
                if let Some(setlist) = setlist_structure.as_ref() {
                    if let Some(song) = setlist.songs.get(active_song_idx) {
                        if let Some(active_section_idx) = active_indices.1 {
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
                let setlist_structure = SETLIST_STRUCTURE.read();
                let active_indices = ACTIVE_INDICES.read();
                if let Some(active_song_idx) = active_indices.0 {
                    if let Some(setlist) = setlist_structure.as_ref() {
                        if let Some(song) = setlist.songs.get(active_song_idx) {
                            if let Some(active_section_idx) = active_indices.1 {
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
            0
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
                let setlist_structure = SETLIST_STRUCTURE.read();
                let active_indices = ACTIVE_INDICES.read();
                if let Some(active_song_idx) = active_indices.0 {
                    if let Some(setlist) = setlist_structure.as_ref() {
                        if let Some(song) = setlist.songs.get(active_song_idx) {
                            if let Some(active_section_idx) = active_indices.1 {
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
    
    // Compute slide border colors (using actual section colors from setlist)
    let slide_border_colors = use_memo(move || {
        slides().iter().map(|slide| {
            section_color_map().get(&slide.section_name)
                .cloned()
                .unwrap_or_else(|| "rgb(128, 128, 128)".to_string())
        }).collect::<Vec<_>>()
    });
    
    // Compute tab background colors (using actual section colors from setlist)
    let tab_bg_colors = use_memo(move || {
        slides().iter().map(|slide| {
            section_color_map().get(&slide.section_name)
                .cloned()
                .unwrap_or_else(|| "rgb(128, 128, 128)".to_string())
        }).collect::<Vec<_>>()
    });
    
    // Compute slide classes with selection state (colors applied via inline styles)
    let slide_classes = use_memo(move || {
        let selected_idx = selected_slide_index();
        slides().iter().enumerate().map(|(idx, _slide)| {
            let selection_class = if selected_idx == Some(idx) {
                "ring-4 ring-primary ring-offset-2"
            } else {
                ""
            };
            format!("rounded-lg border-2 overflow-hidden cursor-pointer transition-all hover:scale-105 flex flex-col {}", selection_class)
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
                    class: "flex items-center justify-between mb-6",
                    h1 {
                        class: "text-3xl font-bold text-foreground",
                        if let Some(lyrics) = lyrics_data() {
                            "{lyrics.song_name}"
                        } else {
                            "Lyrics Editor"
                        }
                    }
                    
                }
                
                // Conditional rendering based on edit sub-view mode
                match edit_view_mode().unwrap_or(EditViewMode::Slides) {
                    EditViewMode::Slides => rsx! {
                        // Main view content
                        div {
                            class: "flex-1 flex overflow-hidden",
                            div {
                                class: "flex-1 overflow-y-auto p-6",
                                div {
                                    class: "grid grid-cols-2 md:grid-cols-3 lg:grid-cols-4 xl:grid-cols-5 2xl:grid-cols-6 gap-3",
                                    for (idx, slide) in slides().iter().enumerate() {
                                        div {
                                            key: "{idx}",
                                            class: "{slide_classes().get(idx).cloned().unwrap_or_default()}",
                                            style: format!(
                                                "border-color: {};",
                                                slide_border_colors().get(idx).cloned().unwrap_or_else(|| "rgb(128, 128, 128)".to_string())
                                            ),
                                            onclick: {
                                                let mut selected_idx = selected_slide_index.clone();
                                                move |_| {
                                                    selected_idx.set(Some(idx));
                                                }
                                            },
                                            // Section name tab on top (outside aspect ratio)
                                            div {
                                                class: "text-white text-xs font-semibold px-3 py-1 uppercase tracking-wide rounded-t-lg",
                                                style: format!(
                                                    "background-color: {};",
                                                    tab_bg_colors().get(idx).cloned().unwrap_or_else(|| "rgb(128, 128, 128)".to_string())
                                                ),
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
                                    // Full size preview
                                    div {
                                        class: "@container shadow-lg rounded-lg overflow-hidden border-2 border-border",
                                        style: "aspect-ratio: 16 / 9;",
                                        // Current slide (top half)
                                        div {
                                            class: "relative w-full h-1/2 bg-black overflow-hidden",
                                            if let Some(_slide) = current_slide() {
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
                                            if let Some(_slide) = next_slide() {
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
                    },
                    EditViewMode::Sync => rsx! {
                        SyllableEditor {
                            lyrics: lyrics_data(),
                            on_syllable_note_change: {
                                // TODO: Implement callback to update lyrics data
                                None
                            },
                            on_syllable_timing_change: {
                                // TODO: Implement callback to update lyrics data
                                None
                            },
                        }
                    },
                }
            }
        }
    }
}


/// Performance preview component - custom preview for performance
#[component]
pub fn PerformancePreview() -> Element {
    // Get lyrics from current song in setlist (reactive to SETLIST_STRUCTURE and ACTIVE_INDICES changes)
    let lyrics_data = use_memo(move || {
        // Read SETLIST_STRUCTURE and ACTIVE_INDICES to trigger reactivity
        let setlist_structure = SETLIST_STRUCTURE.read();
        let active_indices = ACTIVE_INDICES.read();
        if let Some(active_song_idx) = active_indices.0 {
            if let Some(setlist) = setlist_structure.as_ref() {
                if let Some(song) = setlist.songs.get(active_song_idx) {
                    return song.lyrics.clone();
                }
            }
        }
        None
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
    
    rsx! {
        div {
            class: "flex-1 flex overflow-hidden bg-background",
            div {
                class: "flex-1 overflow-y-auto p-6",
                div {
                    class: "flex items-center justify-between mb-6",
                    h1 {
                        class: "text-3xl font-bold text-foreground",
                        if let Some(lyrics) = lyrics_data() {
                            "{lyrics.song_name}"
                        } else {
                            "Performance Preview"
                        }
                    }
                    
                }
                
                div {
                    class: "space-y-4",
                    p {
                        class: "text-muted-foreground",
                        "Performance Preview - Custom preview coming soon!"
                    }
                    // Placeholder content
                    div {
                        class: "rounded-lg border-2 border-border p-8 bg-muted/50",
                        div {
                            class: "text-center text-muted-foreground",
                            "This will be a custom performance preview view"
                        }
                    }
                }
            }
        }
    }
}
