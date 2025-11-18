use ratatui::{
    buffer::Buffer,
    layout::{Alignment, Rect},
    style::{Color, Style},
    text::{Line, Span},
    widgets::{Paragraph, Widget},
};

use primitives::{MusicalPosition, TimePosition, TimeSignature};

pub struct StatusBar {
    musical_pos: MusicalPosition,
    time_pos: TimePosition,
    bpm: f64,
    time_sig: TimeSignature,
    song_start_musical: MusicalPosition,
}

impl StatusBar {
    pub fn new(
        musical_pos: MusicalPosition,
        time_pos: TimePosition,
        bpm: f64,
        time_sig: TimeSignature,
        song_start_musical: MusicalPosition,
    ) -> Self {
        Self {
            musical_pos,
            time_pos,
            bpm,
            time_sig,
            song_start_musical,
        }
    }
}

impl Widget for StatusBar {
    fn render(self, area: Rect, buf: &mut Buffer) {
        // Calculate relative musical position (song-relative)
        // Convert both positions to total beats, calculate difference, then convert back
        let beats_per_measure = self.time_sig.numerator as f64;
        
        // Calculate total beats for current position
        let current_total_beats = self.musical_pos.measure as f64 * beats_per_measure
            + self.musical_pos.beat as f64
            + self.musical_pos.subdivision as f64 / 1000.0;
        
        // Calculate total beats for song start position
        let start_total_beats = self.song_start_musical.measure as f64 * beats_per_measure
            + self.song_start_musical.beat as f64
            + self.song_start_musical.subdivision as f64 / 1000.0;
        
        // Calculate difference (song-relative, should be >= 0)
        let relative_total_beats = (current_total_beats - start_total_beats).max(0.0);
        
        // Convert back to measure.beat.subdivision format
        let relative_measure = (relative_total_beats / beats_per_measure).floor() as i32;
        let beats_in_measure = relative_total_beats % beats_per_measure;
        let relative_beat = beats_in_measure.floor() as i32;
        let relative_subdivision = ((beats_in_measure - relative_beat as f64) * 1000.0).round() as i32;
        
        // Display: measure is 0-indexed, beat is 1-indexed for display
        let musical_pos_str = format!(
            "{:03}.{:02}.{:03}",
            relative_measure,
            (relative_beat + 1).max(1) as u32, // Convert to 1-indexed for display
            relative_subdivision.clamp(0, 999) as u32
        );
        let time_pos_str = format!("{:.2}s", self.time_pos.to_seconds());
        let bpm_str = format!("{:.0} BPM", self.bpm);
        let time_sig_str = format!("{}/{}", self.time_sig.numerator, self.time_sig.denominator);
        
        // Create badge text spans
        let musical_badge_text = format!(" Musical: {} ", musical_pos_str);
        let time_badge_text = format!(" Time: {} ", time_pos_str);
        let bpm_badge_text = format!(" {} ", bpm_str);
        let time_sig_badge_text = format!(" {} ", time_sig_str);
        
        // Calculate badge widths
        let musical_width = musical_badge_text.len();
        let time_width = time_badge_text.len();
        let bpm_width = bpm_badge_text.len();
        let time_sig_width = time_sig_badge_text.len();
        let spacing = 3; // Space between badges (3 characters)
        
        // Calculate total width needed
        let total_width = musical_width + spacing + time_width + spacing + bpm_width + spacing + time_sig_width;
        
        // Calculate starting position to center badges
        let start_x = if area.width as usize > total_width {
            (area.width as usize - total_width) / 2
        } else {
            0
        };
        
        // Create 3 lines with badges filling the full height
        let mut lines = Vec::new();
        let middle_line = area.height / 2;
        
        for line_idx in 0..area.height {
            let mut spans = Vec::new();
            let mut current_x = 0;
            
            // Add leading space to center badges
            if current_x < start_x {
                spans.push(Span::raw(" ".repeat(start_x - current_x)));
                current_x = start_x;
            }
            
            // Render each badge as a 3-character-high block
            let badge_spans = vec![
                (musical_badge_text.clone(), musical_width),
                (time_badge_text.clone(), time_width),
                (bpm_badge_text.clone(), bpm_width),
                (time_sig_badge_text.clone(), time_sig_width),
            ];
            
            for (badge_text, badge_width) in badge_spans {
                if line_idx == middle_line {
                    // Middle line: show text with background
                    spans.push(Span::styled(
                        badge_text,
                        Style::default().fg(Color::White).bg(Color::DarkGray),
                    ));
                } else {
                    // Other lines: show background only
                    spans.push(Span::styled(
                        " ".repeat(badge_width),
                        Style::default().bg(Color::DarkGray),
                    ));
                }
                current_x += badge_width;
                
                // Add spacing between badges (except after last)
                if current_x < start_x + total_width - time_sig_width {
                    spans.push(Span::raw(" ".repeat(spacing)));
                    current_x += spacing;
                }
            }
            
            lines.push(Line::from(spans));
        }
        
        let status_widget = Paragraph::new(lines)
            .alignment(Alignment::Left);
        status_widget.render(area, buf);
    }
}

