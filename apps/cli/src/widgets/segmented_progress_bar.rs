use ratatui::{
    buffer::Buffer,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Paragraph, Widget},
};

use crate::colors::{get_section_color_bright, get_section_color_muted, get_song_color_bright};
use setlist::Song;

pub struct SegmentedProgressBar<'a> {
    overall_progress: f64,
    song: &'a Song,
    song_idx: usize,
    label: String,
    hovered_section: Option<usize>,
}

impl<'a> SegmentedProgressBar<'a> {
    pub fn new(song: &'a Song, song_idx: usize, overall_progress: f64, label: String, hovered_section: Option<usize>) -> Self {
        Self {
            overall_progress,
            song,
            song_idx,
            label,
            hovered_section,
        }
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

impl Widget for SegmentedProgressBar<'_> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        // Add horizontal padding (2 characters on each side)
        let horizontal_padding = 2;
        let padded_area = Rect {
            x: area.x + horizontal_padding,
            y: area.y,
            width: area.width.saturating_sub(horizontal_padding * 2),
            height: area.height,
        };
        
        // Get transport to determine current position
        let current_position = if let Some(project) = &self.song.project {
            project.transport().playhead_position.time.to_seconds()
        } else {
            0.0
        };
        
        let song_start = self.song.effective_start();
        let song_length = self.song.duration();
        let bar_width = padded_area.width.saturating_sub(2) as usize;
        
        // Split padded area into label line and progress bar lines
        let inner_layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(1), // Label line
                Constraint::Min(0),   // Progress bar lines
            ])
            .split(padded_area);
        
        // Draw label
        let label_widget = Paragraph::new(self.label)
            .alignment(ratatui::layout::Alignment::Center)
            .block(Block::default().borders(Borders::NONE));
        label_widget.render(inner_layout[0], buf);
        
        // Calculate section segments
        let mut section_segments = Vec::new();
        let mut current_pos = 0;
        
        if self.song.sections.is_empty() {
            // No sections - use song color
            let song_color_bright = get_song_color_bright(self.song_idx);
            let filled_width = (self.overall_progress * bar_width as f64) as usize;
            section_segments.push((
                0,
                bar_width,
                filled_width,
                song_color_bright,
                Color::DarkGray,
                None,
            ));
        } else {
            // Calculate each section segment
            for (section_idx, section) in self.song.sections.iter().enumerate() {
                let section_start = section.start_position.time.to_seconds();
                let section_end = section.end_position.time.to_seconds();
                
                // Calculate section position relative to song start
                let section_start_rel = (section_start - song_start).max(0.0);
                let section_end_rel = (section_end - song_start).max(0.0);
                
                // Calculate section width in progress bar pixels
                let section_start_px = (section_start_rel / song_length * bar_width as f64) as usize;
                let section_end_px = (section_end_rel / song_length * bar_width as f64) as usize;
                let section_width = section_end_px.saturating_sub(section_start_px);
                
                // Fill any gap before this section
                if section_start_px > current_pos {
                    section_segments.push((
                        current_pos,
                        section_start_px - current_pos,
                        0,
                        Color::DarkGray,
                        Color::DarkGray,
                        None,
                    ));
                }
                
                // Determine section colors
                let is_hovered = self.hovered_section == Some(section_idx);
                let mut section_color_bright = get_section_color_bright(self.song_idx, section_idx);
                let mut section_color_muted = get_section_color_muted(self.song_idx, section_idx);
                
                // Apply hover effect
                if is_hovered {
                    section_color_bright = brighten_color(section_color_bright);
                    section_color_muted = brighten_color(section_color_muted);
                }
                
                // Calculate progress within this section
                let current_rel = (current_position - song_start).max(0.0);
                let section_progress = if current_rel >= section_end_rel {
                    1.0
                } else if current_rel <= section_start_rel {
                    0.0
                } else {
                    let section_length = section_end_rel - section_start_rel;
                    if section_length > 0.0 {
                        ((current_rel - section_start_rel) / section_length).clamp(0.0, 1.0)
                    } else {
                        0.0
                    }
                };
                
                let filled_width = (section_progress * section_width as f64) as usize;
                
                section_segments.push((
                    section_start_px,
                    section_width,
                    filled_width,
                    section_color_bright,
                    section_color_muted,
                    Some(section.display_name()),
                ));
                
                current_pos = section_end_px;
            }
            
            // Fill any remaining space after last section
            if current_pos < bar_width {
                section_segments.push((
                    current_pos,
                    bar_width - current_pos,
                    0,
                    Color::DarkGray,
                    Color::DarkGray,
                    None,
                ));
            }
        }
        
        // Draw progress bar lines with section labels
        let progress_area = inner_layout[1];
        let lines_to_draw = progress_area.height as usize;
        let middle_line = if lines_to_draw > 0 {
            (lines_to_draw - 1) / 2
        } else {
            0
        };
        
        let mut progress_lines = Vec::new();
        
        for line_idx in 0..lines_to_draw {
            let mut spans = Vec::new();
            let mut current_px = 0;
            
            for (seg_start, seg_width, seg_filled, seg_bright, seg_muted, seg_name) in &section_segments {
                // Fill gap before this segment
                if *seg_start > current_px {
                    spans.push(Span::styled(
                        " ".repeat(*seg_start - current_px),
                        Style::default().bg(Color::DarkGray),
                    ));
                }
                
                let seg_filled_px = *seg_filled;
                let seg_unfilled_px = seg_width.saturating_sub(seg_filled_px);
                
                // On the middle line, overlay section text if available
                if line_idx == middle_line && seg_name.is_some() {
                    let name = seg_name.as_ref().unwrap();
                    let name_chars: Vec<char> = name.chars().collect();
                    let name_len = name_chars.len();
                    let available_width = *seg_width;
                    
                    let padding_left = if name_len <= available_width {
                        (available_width - name_len) / 2
                    } else {
                        0
                    };
                    
                    // Draw the segment character by character, overlaying text where needed
                    for px in 0..*seg_width {
                        let char_pos = px.saturating_sub(padding_left);
                        let is_filled = px < seg_filled_px;
                        let bg_color = if is_filled { *seg_bright } else { *seg_muted };
                        
                        if char_pos < name_len && px >= padding_left {
                            let ch = name_chars[char_pos];
                            spans.push(Span::styled(
                                ch.to_string(),
                                Style::default()
                                    .fg(Color::White)
                                    .bg(bg_color),
                            ));
                        } else {
                            spans.push(Span::styled(
                                " ",
                                Style::default().bg(bg_color),
                            ));
                        }
                    }
                } else {
                    // Regular progress bar line (no text overlay)
                    if seg_filled_px > 0 {
                        spans.push(Span::styled(
                            " ".repeat(seg_filled_px),
                            Style::default().bg(*seg_bright),
                        ));
                    }
                    
                    if seg_unfilled_px > 0 {
                        spans.push(Span::styled(
                            " ".repeat(seg_unfilled_px),
                            Style::default().bg(*seg_muted),
                        ));
                    }
                }
                
                current_px = seg_start + seg_width;
            }
            
            // Fill remaining space
            if current_px < bar_width {
                spans.push(Span::styled(
                    " ".repeat(bar_width - current_px),
                    Style::default().bg(Color::DarkGray),
                ));
            }
            
            progress_lines.push(Line::from(spans));
        }
        
        // Render all lines as a paragraph
        let progress_widget = Paragraph::new(progress_lines)
            .block(Block::default().borders(Borders::NONE))
            .alignment(ratatui::layout::Alignment::Left);
        progress_widget.render(progress_area, buf);
    }
}

