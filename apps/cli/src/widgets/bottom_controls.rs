use ratatui::{
    buffer::Buffer,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Style},
    widgets::{Block, Borders, Paragraph, Widget},
};

use crate::colors::get_song_color;
use setlist::Song;
use transport::TransportActions;

pub struct BottomControls<'a> {
    song: Option<&'a Song>,
    song_idx: usize,
    hovered_button: Option<usize>, // 0=Back, 1=Play/Pause, 2=Loop, 3=Advance
}

impl<'a> BottomControls<'a> {
    pub fn new(song: Option<&'a Song>, song_idx: usize, hovered_button: Option<usize>) -> Self {
        Self { song, song_idx, hovered_button }
    }
}

// Helper function to render a centered button
fn render_button(button_area: Rect, text: &str, style: Style, buf: &mut Buffer) {
    // Create the block and get inner area before rendering
    let block = Block::default()
        .borders(Borders::ALL)
        .style(style);
    let inner_area = block.inner(button_area);
    
    // Render the block with borders on the full area
    block.render(button_area, buf);
    
    // Split inner area vertically to center text
    let text_layout = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Min(0), // Spacer above
            Constraint::Length(1), // Text line
            Constraint::Min(0), // Spacer below
        ])
        .split(inner_area);
    
    let button_text = Paragraph::new(text)
        .alignment(ratatui::layout::Alignment::Center)
        .style(style);
    button_text.render(text_layout[1], buf);
}

impl Widget for BottomControls<'_> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        // Render a background block to ensure full width visibility
        let background = Block::default()
            .borders(Borders::TOP)
            .style(Style::default().bg(Color::Black));
        background.render(area, buf);
        
        let layout = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Percentage(25), // Back button
                Constraint::Percentage(25), // Play/Pause button
                Constraint::Percentage(25), // Loop button
                Constraint::Percentage(25), // Advance button
            ])
            .split(area);

        // Back button (go to start of song)
        let back_text = if self.song.is_some() { "◀ Back" } else { "◀" };
        let back_style = if self.hovered_button == Some(0) {
            Style::default().bg(Color::DarkGray).fg(Color::White)
        } else {
            Style::default().fg(Color::White)
        };
        render_button(layout[0], back_text, back_style, buf);

        // Play/Pause button
        if let Some(song) = self.song {
            let is_playing = TransportActions::is_playing(song).unwrap_or(false);
            let play_pause_text = if is_playing { "⏸ Pause" } else { "▶ Play" };
            let song_color = get_song_color(self.song_idx);
            let play_pause_style = if is_playing {
                // When playing, use song color background with white text
                if self.hovered_button == Some(1) {
                    Style::default().fg(Color::White).bg(Color::DarkGray)
                } else {
                    Style::default().fg(Color::White).bg(song_color)
                }
            } else {
                // When not playing, use song color text
                if self.hovered_button == Some(1) {
                    Style::default().fg(song_color).bg(Color::DarkGray)
                } else {
                    Style::default().fg(song_color)
                }
            };
            render_button(layout[1], play_pause_text, play_pause_style, buf);
        } else {
            let play_pause_style = if self.hovered_button == Some(1) {
                Style::default().bg(Color::DarkGray).fg(Color::White)
            } else {
                Style::default().fg(Color::White)
            };
            render_button(layout[1], "▶ Play", play_pause_style, buf);
        }

        // Loop button
        if let Some(song) = self.song {
            let is_looping = song.project.as_ref()
                .map(|p| p.transport().looping)
                .unwrap_or(false);
            let loop_text = if is_looping { "🔁 Loop" } else { "Loop" };
            let loop_style = if is_looping {
                // When looping, use bright green background with white text
                if self.hovered_button == Some(2) {
                    Style::default().bg(Color::Gray).fg(Color::White)
                } else {
                    Style::default().bg(Color::Green).fg(Color::White)
                }
            } else {
                // When not looping, use dark gray background
                if self.hovered_button == Some(2) {
                    Style::default().bg(Color::Gray).fg(Color::White)
                } else {
                    Style::default().bg(Color::DarkGray).fg(Color::White)
                }
            };
            render_button(layout[2], loop_text, loop_style, buf);
        } else {
            let loop_style = if self.hovered_button == Some(2) {
                Style::default().bg(Color::Gray).fg(Color::White)
            } else {
                Style::default().bg(Color::DarkGray).fg(Color::White)
            };
            render_button(layout[2], "Loop", loop_style, buf);
        }

        // Advance button (go to next section or song)
        let advance_text = if self.song.is_some() { "Advance ▶" } else { "▶" };
        let advance_style = if self.hovered_button == Some(3) {
            Style::default().bg(Color::DarkGray).fg(Color::White)
        } else {
            Style::default().fg(Color::White)
        };
        render_button(layout[3], advance_text, advance_style, buf);
    }
}

