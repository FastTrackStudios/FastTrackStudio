use ratatui::{
    buffer::Buffer,
    layout::Rect,
    style::{Color, Style},
    text::{Line, Span},
    widgets::{Block, Borders, List, ListItem, Widget},
};

use crate::colors::{
    get_section_color_bright, get_section_color_muted, get_song_color, get_song_color_bright,
    get_song_color_muted,
};
use setlist::Song;
use transport::TransportActions;

pub struct SongList<'a> {
    songs: &'a [Song],
    selected_song_index: usize,
    selected_section_index: Option<usize>,
    expanded_songs: &'a std::collections::HashSet<usize>,
    hovered_item: Option<(usize, Option<usize>)>, // (song_idx, section_idx_opt)
    get_song_progress: Box<dyn Fn(usize) -> f64 + 'a>,
    get_section_progress: Box<dyn Fn(usize, usize) -> f64 + 'a>,
}

impl<'a> SongList<'a> {
    pub fn new(
        songs: &'a [Song],
        selected_song_index: usize,
        selected_section_index: Option<usize>,
        expanded_songs: &'a std::collections::HashSet<usize>,
        hovered_item: Option<(usize, Option<usize>)>,
        get_song_progress: impl Fn(usize) -> f64 + 'a,
        get_section_progress: impl Fn(usize, usize) -> f64 + 'a,
    ) -> Self {
        Self {
            songs,
            selected_song_index,
            selected_section_index,
            expanded_songs,
            hovered_item,
            get_song_progress: Box::new(get_song_progress),
            get_section_progress: Box::new(get_section_progress),
        }
    }
}

impl Widget for SongList<'_> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let bar_width = (area.width.saturating_sub(4)) as usize; // Account for borders and padding
        let mut items = Vec::new();
        let mut highlight_index = None;
        let mut item_index = 0;

        for (song_idx, song) in self.songs.iter().enumerate() {
            let is_song_selected = self.selected_song_index == song_idx;
            let is_song_hovered = self.hovered_item == Some((song_idx, None));
            let song_color = get_song_color(song_idx);
            let song_color_bright = get_song_color_bright(song_idx);
            let song_color_muted = get_song_color_muted(song_idx);
            let song_progress = (self.get_song_progress)(song_idx);

            // Create song text with progress bar (3 lines tall)
            let song_text = format!("{}", song.name);
            let song_lines = create_progress_lines(
                song_text,
                song_progress,
                bar_width,
                song_color_bright,
                song_color_muted,
                is_song_selected,
                is_song_hovered,
                3, // 3 lines tall
                0, // No indent offset for songs
            );
            for line in song_lines {
                items.push(ListItem::new(line));
            }

            // Only highlight song if no section is selected (song-level selection)
            if is_song_selected && self.selected_section_index.is_none() {
                highlight_index = Some(item_index);
            }
            item_index += 1;

            // Add sections indented (only if song is expanded)
            if self.expanded_songs.contains(&song_idx) {
                for (section_idx, section) in song.sections.iter().enumerate() {
                    let is_section_selected =
                        is_song_selected && self.selected_section_index == Some(section_idx);
                    let is_section_hovered = self.hovered_item == Some((song_idx, Some(section_idx)));
                    let section_color_bright = get_section_color_bright(song_idx, section_idx);
                    let section_color_muted = get_section_color_muted(song_idx, section_idx);
                    let section_progress = (self.get_section_progress)(song_idx, section_idx);

                    let section_text = format!("  > {}", section.display_name());
                    let section_lines = create_progress_lines(
                        section_text,
                        section_progress,
                        bar_width,
                        section_color_bright,
                        section_color_muted,
                        is_section_selected,
                        is_section_hovered,
                        3, // 3 lines tall
                        2, // 2-character indent offset for background
                    );
                    for line in section_lines {
                        items.push(ListItem::new(line));
                    }

                    if is_section_selected {
                        highlight_index = Some(item_index);
                    }
                    item_index += 1;
                }
            }
        }

        let mut list_state = ratatui::widgets::ListState::default();
        if let Some(highlight) = highlight_index {
            list_state.select(Some(highlight));
        }

        let list = List::new(items)
            .block(Block::default().borders(Borders::ALL).title("Songs"))
            .highlight_style(Style::default().add_modifier(ratatui::style::Modifier::BOLD));
        list.render(area, buf);
    }
}

fn brighten_color(color: Color) -> Color {
    // Add subtle white overlay to brighten color (more transparent)
    // First convert to RGB if needed
    let (r, g, b) = match color {
        Color::Rgb(r, g, b) => (r, g, b),
        Color::Red => (187, 0, 0),
        Color::Green => (0, 187, 0),
        Color::Yellow => (187, 187, 0),
        Color::Blue => (0, 0, 187),
        Color::Magenta => (187, 0, 187),
        Color::Cyan => (0, 187, 187),
        Color::LightRed => (255, 85, 85),
        Color::LightGreen => (85, 255, 85),
        Color::LightYellow => (255, 255, 85),
        Color::LightBlue => (85, 85, 255),
        Color::LightMagenta => (255, 85, 255),
        Color::LightCyan => (85, 255, 255),
        Color::DarkGray => (85, 85, 85),
        Color::Gray => (136, 136, 136),
        Color::White => (238, 238, 238),
        _ => (128, 128, 128), // Fallback
    };
    
    // Blend with white (20% white overlay - more transparent)
    Color::Rgb(
        ((r as f32 * 0.8 + 255.0 * 0.2) as u16).min(255) as u8,
        ((g as f32 * 0.8 + 255.0 * 0.2) as u16).min(255) as u8,
        ((b as f32 * 0.8 + 255.0 * 0.2) as u16).min(255) as u8,
    )
}

fn create_progress_lines(
    text: String,
    progress: f64,
    width: usize,
    bright_color: Color,
    muted_color: Color,
    is_selected: bool,
    is_hovered: bool,
    height: usize,
    indent_offset: usize,
) -> Vec<Line<'static>> {
    let mut lines = Vec::new();
    
    // Apply hover effect - brighten colors
    let bright_color = if is_hovered {
        brighten_color(bright_color)
    } else {
        bright_color
    };
    let muted_color = if is_hovered {
        brighten_color(muted_color)
    } else {
        muted_color
    };
    
    // Create the base line
    let base_line = create_progress_line(text, progress, width, bright_color, muted_color, is_selected, indent_offset);
    
    // Calculate vertical centering
    let middle_line = height / 2;
    
    // Create empty lines for spacing
    for i in 0..height {
        if i == middle_line {
            // Middle line with text
            lines.push(base_line.clone());
        } else {
            // Empty line with background (accounting for indent)
            let filled_width = (progress * width as f64) as usize;
            let mut spans = Vec::new();
            
            // Add indent space (no background)
            if indent_offset > 0 {
                spans.push(Span::raw(" ".repeat(indent_offset)));
            }
            
            // Calculate effective width (width minus indent)
            let effective_width = width.saturating_sub(indent_offset);
            let effective_filled = filled_width.saturating_sub(indent_offset);
            
            if effective_filled > 0 && effective_filled <= effective_width {
                spans.push(Span::styled(
                    " ".repeat(effective_filled),
                    Style::default().bg(bright_color),
                ));
            }
            // Always fill to full effective width
            if effective_filled < effective_width {
                spans.push(Span::styled(
                    " ".repeat(effective_width - effective_filled),
                    Style::default().bg(muted_color),
                ));
            } else if effective_filled == 0 {
                // If no progress, fill entire width with muted color
                spans.push(Span::styled(
                    " ".repeat(effective_width),
                    Style::default().bg(muted_color),
                ));
            }
            lines.push(Line::from(spans));
        }
    }
    
    lines
}

fn create_progress_line(
    text: String,
    progress: f64,
    width: usize,
    bright_color: Color,
    muted_color: Color,
    is_selected: bool,
    indent_offset: usize,
) -> Line<'static> {
    let filled_width = (progress * width as f64) as usize;
    let text_start = 2 + indent_offset; // Account for indent offset

    let mut spans = Vec::new();
    let mut pos = 0;
    let text_chars: Vec<char> = text.chars().collect();

    // Add indent space (no background) if there's an indent offset
    if indent_offset > 0 {
        spans.push(Span::raw(" ".repeat(indent_offset)));
        pos += indent_offset;
    }

    // Left padding (after indent)
    let padding_start = 2; // Always 2 characters of padding after indent
    if padding_start > 0 {
        let effective_filled = filled_width.saturating_sub(indent_offset);
        if pos < filled_width && effective_filled > 0 {
            let padding_on_filled = padding_start.min(effective_filled);
            spans.push(Span::styled(
                " ".repeat(padding_on_filled),
                Style::default().bg(bright_color),
            ));
            pos += padding_on_filled;

            if padding_start > padding_on_filled {
                spans.push(Span::styled(
                    " ".repeat(padding_start - padding_on_filled),
                    Style::default().bg(muted_color),
                ));
                pos += padding_start - padding_on_filled;
            }
        } else {
            spans.push(Span::styled(
                " ".repeat(padding_start),
                Style::default().bg(muted_color),
            ));
            pos += padding_start;
        }
    }

    // Text portion
    let mut current_text = String::new();
    let mut current_bg_is_filled = None;

    for (i, ch) in text_chars.iter().enumerate() {
        let char_pos = text_start + i;
        let bg_is_filled = char_pos < filled_width;

        if current_bg_is_filled == Some(bg_is_filled) {
            current_text.push(*ch);
        } else {
            if !current_text.is_empty() {
                if current_bg_is_filled == Some(true) {
                    spans.push(Span::styled(
                        current_text.clone(),
                        Style::default()
                            .fg(if is_selected {
                                Color::Black
                            } else {
                                Color::White
                            })
                            .bg(bright_color),
                    ));
                } else if current_bg_is_filled == Some(false) {
                    spans.push(Span::styled(
                        current_text.clone(),
                        Style::default().fg(Color::White).bg(muted_color),
                    ));
                }
                current_text.clear();
            }
            current_text.push(*ch);
            current_bg_is_filled = Some(bg_is_filled);
        }
        pos += 1;
    }

    // Flush remaining text
    if !current_text.is_empty() {
        if current_bg_is_filled == Some(true) {
            spans.push(Span::styled(
                current_text,
                Style::default()
                    .fg(if is_selected {
                        Color::Black
                    } else {
                        Color::White
                    })
                    .bg(bright_color),
            ));
        } else {
            spans.push(Span::styled(
                current_text,
                Style::default().fg(Color::White).bg(muted_color),
            ));
        }
    }

    // Fill remaining space (accounting for indent)
    // Calculate total width used so far
    let mut total_width_used = 0;
    for span in &spans {
        total_width_used += span.content.len();
    }
    
    // We want to fill to exactly `width` characters total
    if total_width_used < width {
        let effective_width = width.saturating_sub(indent_offset);
        let effective_pos = total_width_used.saturating_sub(indent_offset);
        let effective_filled = filled_width.saturating_sub(indent_offset);
        
        if effective_pos < effective_filled {
            let remaining_filled = effective_filled - effective_pos;
            spans.push(Span::styled(
                " ".repeat(remaining_filled),
                Style::default().bg(bright_color),
            ));
            total_width_used += remaining_filled;
        }
        
        // Fill remaining space to full width
        let remaining_total = width - total_width_used;
        if remaining_total > 0 {
            spans.push(Span::styled(
                " ".repeat(remaining_total),
                Style::default().bg(muted_color),
            ));
        }
    }

    Line::from(spans)
}

