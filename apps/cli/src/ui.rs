use ratatui::{
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Paragraph},
    Frame,
};
use tui_big_text::{BigText, PixelSize};

use crate::colors::get_song_color;
use crate::widgets::{BottomControls, SegmentedProgressBar, SongList, StatusBar, TopBar};
use primitives::{MusicalPosition, TimePosition};
use transport::TransportActions;

pub fn draw_ui(frame: &mut Frame, app: &mut crate::app::CliSetlistApp) {
    let layout = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(1), // Top bar
            Constraint::Min(0),    // Main content
        ])
        .split(frame.size());

    frame.render_widget(TopBar, layout[0]);
    draw_main_content(frame, layout[1], app);
}

fn draw_main_content(frame: &mut Frame, area: Rect, app: &mut crate::app::CliSetlistApp) {
    let horizontal_layout = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Length(30), // Song list sidebar
            Constraint::Fill(1),     // Main content area (fill remaining space)
        ]);
    let areas = horizontal_layout.split(area);
    let left_area = areas[0];
    let right_area = areas[1];

    draw_song_list(frame, left_area, app);
    draw_centered_content(frame, right_area, app);
}

fn draw_song_list(frame: &mut Frame, area: Rect, app: &mut crate::app::CliSetlistApp) {
    // Store the area for mouse hit testing
    app.song_list_area = Some(area);
    frame.render_widget(
        SongList::new(
            &app.setlist.songs,
            app.selected_song_index,
            app.selected_section_index,
            &app.expanded_songs,
            app.hovered_song_list_item,
            |song_idx| get_song_progress(app, song_idx),
            |song_idx, section_idx| get_section_progress(app, song_idx, section_idx),
        ),
        area,
    );
}

fn get_song_progress(app: &crate::app::CliSetlistApp, song_idx: usize) -> f64 {
    if let Some(song) = app.setlist.songs.get(song_idx) {
        if let Some(project) = &song.project {
            let transport = project.transport();
            let absolute_position = transport.playhead_position.time.to_seconds();
            let song_start = song.effective_start();
            let song_length = song.duration();
            
            // Calculate position relative to song start
            let relative_position = (absolute_position - song_start).max(0.0);
            
            if song_length > 0.0 {
                (relative_position / song_length).clamp(0.0, 1.0)
            } else {
                0.0
            }
        } else {
            0.0
        }
    } else {
        0.0
    }
}

fn get_section_progress(
    app: &crate::app::CliSetlistApp,
    song_idx: usize,
    section_idx: usize,
) -> f64 {
    if let Some(song) = app.setlist.songs.get(song_idx) {
        if let Some(section) = song.sections.get(section_idx) {
            if let Some(project) = &song.project {
                let transport = project.transport();
                let position = transport.playhead_position.time.to_seconds();
                let start = section.start_position.time.to_seconds();
                let end = section.end_position.time.to_seconds();

                if position >= end {
                    return 1.0;
                }
                if position < start {
                    return 0.0;
                }
                let section_length = end - start;
                if section_length > 0.0 {
                    ((position - start) / section_length).clamp(0.0, 1.0)
                } else {
                    0.0
                }
            } else {
                0.0
            }
        } else {
            0.0
        }
    } else {
        0.0
    }
}

fn draw_centered_content(frame: &mut Frame, area: Rect, app: &mut crate::app::CliSetlistApp) {
    // Calculate layout first to get bottom controls area
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Percentage(30), // Spacer to position title in upper third
            Constraint::Length(10), // Big song title (needs more height for pixelated text)
            Constraint::Length(6), // Progress bar
            Constraint::Length(3), // Spacer between progress bar and badges
            Constraint::Length(3), // Status bar badges (3 characters high)
            Constraint::Length(3), // Spacer between badges and next song info
            Constraint::Length(3), // Next song info
            Constraint::Min(0),    // Spacer
            Constraint::Length(5), // Bottom controls (larger, at bottom)
        ])
        .split(area);
    
    // Store the bottom controls area for mouse hit testing
    app.bottom_controls_area = Some(chunks[8]);
    
    // Chunk indices:
    // chunks[0] = Spacer (top, positions title in upper third)
    // chunks[1] = Big song title
    // chunks[2] = Progress bar
    // chunks[3] = Spacer between progress bar and badges
    // chunks[4] = Status bar badges
    // chunks[5] = Spacer between badges and next song info
    // chunks[6] = Next song info
    // chunks[7] = Spacer
    // chunks[8] = Bottom controls
    
    // Calculate progress bar inner area for mouse hit testing (before borrowing song)
    let progress_bar_inner = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(1), // Label line
            Constraint::Min(0),   // Progress bar lines
        ])
        .split(chunks[2]);
    let progress_bar_area = progress_bar_inner[1];
    
    if let Some(song) = app.selected_song() {
        if let Some(project) = &song.project {
            let transport = project.transport();
            let song_color = get_song_color(app.selected_song_index);

            // Calculate progress and other metrics
            let song_start = song.effective_start();
            let song_length = song.duration();
            let current_time = transport.playhead_position.time.clone();
            let position_seconds = current_time.to_seconds();
            let display_position = (position_seconds - song_start).max(0.0);

            // Calculate musical position-based progress
            let bpm = transport.tempo.bpm;
            let time_sig = transport.time_signature;
            let song_start_time = primitives::TimePosition::from_seconds(song_start);
            let song_start_musical = song_start_time.to_musical_position(bpm, time_sig);
            let song_end_time = primitives::TimePosition::from_seconds(song_start + song_length);
            let song_end_musical = song_end_time.to_musical_position(bpm, time_sig);
            let current_musical = current_time.to_musical_position(bpm, time_sig);

            let beats_per_measure = time_sig.numerator as f64;
            let start_beats = song_start_musical.measure as f64 * beats_per_measure
                + song_start_musical.beat as f64
                + song_start_musical.subdivision as f64 / 1000.0;
            let end_beats = song_end_musical.measure as f64 * beats_per_measure
                + song_end_musical.beat as f64
                + song_end_musical.subdivision as f64 / 1000.0;
            let current_beats = current_musical.measure as f64 * beats_per_measure
                + current_musical.beat as f64
                + current_musical.subdivision as f64 / 1000.0;

            let song_relative_beats = (current_beats - start_beats).max(0.0);
            let song_total_beats = (end_beats - start_beats).max(0.0);
            let ratio = if song_total_beats > 0.0 {
                (song_relative_beats / song_total_beats).clamp(0.0, 1.0)
            } else {
                0.0
            };

            let label = if app.selected_section_index.is_some() {
                format!("{display_position:6.2}s / {song_length:6.2}s (section)")
            } else {
                format!("{display_position:6.2}s / {song_length:6.2}s")
            };

            // Layout already calculated above

            // Big song title using tui-big-text for larger display
            let title_area = chunks[1];
            let big_title = BigText::builder()
                .pixel_size(PixelSize::HalfHeight) // Makes text appear larger
                .style(Style::default().fg(song_color).add_modifier(Modifier::BOLD))
                .lines(vec![song.name.clone().into()])
                .centered() // Center the text horizontally
                .build();
            
            // Render the big text - it will center itself within the provided area
            frame.render_widget(big_title, title_area);

            // Progress bar using widget (centered)
            frame.render_widget(
                SegmentedProgressBar::new(song, app.selected_song_index, ratio, label, app.hovered_progress_section),
                chunks[2],
            );

            // Status bar showing MusicalPosition, TimePosition, BPM, TimeSignature
            // Calculate musical position relative to song start
            let song_start_time = primitives::TimePosition::from_seconds(song_start);
            let song_start_musical = song_start_time.to_musical_position(bpm, time_sig);
            let current_musical = current_time.to_musical_position(bpm, time_sig);
            
            // Create status bar widget (now at chunks[4] after layout change)
            let display_time_pos = primitives::TimePosition::from_seconds(display_position);
            frame.render_widget(
                StatusBar::new(
                    current_musical,
                    display_time_pos,
                    bpm,
                    time_sig,
                    song_start_musical,
                ),
                chunks[4],
            );

            // Next song info (below badges, at chunks[6])
            let next_song = if app.selected_song_index < app.setlist.songs.len().saturating_sub(1) {
                app.setlist.songs.get(app.selected_song_index + 1).map(|s| s.name.clone())
            } else {
                None
            };

            let next_text = if let Some(next_name) = next_song {
                vec![
                    Line::from(vec![Span::styled(
                        "Next:",
                        Style::default().fg(Color::DarkGray),
                    )]),
                    Line::from(vec![Span::styled(
                        next_name,
                        Style::default().fg(Color::White),
                    )]),
                    Line::from(""),
                ]
            } else {
                vec![
                    Line::from(""),
                    Line::from(vec![Span::styled(
                        "Next: (end of setlist)",
                        Style::default().fg(Color::DarkGray),
                    )]),
                    Line::from(""),
                ]
            };

            let next_widget = Paragraph::new(next_text)
                .alignment(ratatui::layout::Alignment::Center)
                .block(Block::default());
            frame.render_widget(next_widget, chunks[6]);

            // Bottom controls at the bottom (taller, 5 lines) (now at chunks[8] after layout change)
            frame.render_widget(
                BottomControls::new(Some(song), app.selected_song_index, app.hovered_button),
                chunks[8],
            );
            
            // Store progress bar area after all immutable borrows are done
            // Recalculate progress_bar_area for new chunk index
            let progress_bar_inner = Layout::default()
                .direction(Direction::Vertical)
                .constraints([
                    Constraint::Length(1), // Label line
                    Constraint::Min(0),   // Progress bar lines
                ])
                .split(chunks[2]);
            app.progress_bar_area = Some(progress_bar_inner[1]);
        } else {
            // No project - no progress bar to store
        }
    } else {
        // Even when no song is selected, show bottom controls at bottom
        // Note: progress_bar_area is not set here since there's no progress bar to click
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Min(0),    // Spacer
                Constraint::Length(5), // Bottom controls (larger)
            ])
            .split(area);
        
        // Store the area for mouse hit testing
        app.bottom_controls_area = Some(chunks[1]);
        frame.render_widget(BottomControls::new(None, 0, app.hovered_button), chunks[1]);
    }
}

