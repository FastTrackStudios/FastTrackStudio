use std::{
    io,
    time::{Duration, Instant},
};

use color_eyre::eyre::Result;
use crossterm::{
    event::{self, Event, KeyCode, KeyEvent, KeyEventKind},
    execute,
    terminal::{
        disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen,
    },
};
use ratatui::{
    backend::CrosstermBackend,
    layout::{Constraint, Direction, Layout},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Gauge, Paragraph, Wrap},
    Frame, Terminal,
};
use transport::{PlayState, Tempo, Transport, TransportActions, TransportError};

const SONG_LENGTH_SECONDS: f64 = 240.0;
const TICK_RATE: Duration = Duration::from_millis(50);

fn main() -> Result<()> {
    color_eyre::install()?;
    enable_raw_mode()?;

    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen)?;

    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;
    terminal.clear()?;

    let mut app = CliTransportApp::new();
    
    let result = run_app(&mut terminal, &mut app);

    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
    terminal.show_cursor()?;

    result
}

fn run_app<B: ratatui::backend::Backend>(
    terminal: &mut Terminal<B>,
    app: &mut CliTransportApp,
) -> Result<()> {
    loop {
        terminal.draw(|frame| app.draw(frame))?;

        let timeout = TICK_RATE
            .checked_sub(app.last_tick.elapsed())
            .unwrap_or(Duration::from_secs(0));

        if event::poll(timeout)? {
            if let Event::Key(key) = event::read()? {
                if key.kind == KeyEventKind::Press {
                    app.handle_key(key);
                }
            }
        }

        if app.last_tick.elapsed() >= TICK_RATE {
            app.on_tick();
        }

        if app.should_exit {
            break;
        }
    }

    Ok(())
}

struct CliTransportApp {
    transport: Transport,
    length_seconds: f64,
    should_exit: bool,
    last_tick: Instant,
    status_message: String,
}

impl CliTransportApp {
    fn new() -> Self {
        Self {
            transport: Transport::new(),
            length_seconds: SONG_LENGTH_SECONDS,
            should_exit: false,
            last_tick: Instant::now(),
            status_message: "transport ready".to_string(),
        }
    }

    fn handle_key(&mut self, key: KeyEvent) {
        match key.code {
            KeyCode::Char('q') => self.should_exit = true,
            KeyCode::Char(' ') => self.apply_action(|t| t.play_pause()),
            KeyCode::Char('s') => self.apply_action(|t| t.stop()),
            KeyCode::Char('r') => self.apply_action(|t| t.toggle_recording()),
            KeyCode::Char('+') | KeyCode::Char('=') => self.bump_tempo(5.0),
            KeyCode::Char('-') | KeyCode::Char('_') => self.bump_tempo(-5.0),
            KeyCode::Right => self.nudge_position(5.0),
            KeyCode::Left => self.nudge_position(-5.0),
            KeyCode::Char('0') => self.set_position(0.0),
            _ => {}
        }
    }

    fn on_tick(&mut self) {
        let elapsed = self.last_tick.elapsed().as_secs_f64();
        if matches!(
            self.transport.play_state,
            PlayState::Playing | PlayState::Recording
        ) {
            self.advance_position(elapsed * self.transport.playrate);
        }
        self.last_tick = Instant::now();
    }

    fn apply_action<F>(&mut self, action: F)
    where
        F: FnOnce(&mut Transport) -> Result<String, TransportError>,
    {
        match action(&mut self.transport) {
            Ok(msg) => self.status_message = msg,
            Err(err) => self.status_message = err.to_string(),
        }
    }

    fn bump_tempo(&mut self, delta: f64) {
        let target = (self.transport.tempo.bpm + delta).clamp(30.0, 300.0);
        let tempo = Tempo::new(target);
        self.apply_action(|t| TransportActions::set_tempo(t, tempo));
    }

    fn nudge_position(&mut self, delta: f64) {
        let current = self.transport.playhead_position.time.to_seconds();
        let target = (current + delta).clamp(0.0, self.length_seconds);
        self.set_position(target);
    }

    fn set_position(&mut self, seconds: f64) {
        self.apply_action(|t| t.set_position(seconds));
    }

    fn advance_position(&mut self, delta: f64) {
        let current = self.transport.playhead_position.time.to_seconds();
        let next = (current + delta).min(self.length_seconds);
        if (next - self.length_seconds).abs() < f64::EPSILON {
            let _ = self.transport.stop();
            self.status_message = "Reached end of timeline".to_string();
        } else {
            let _ = self.transport.set_position(next);
        }
    }

    fn draw(&self, frame: &mut Frame) {
        let layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(3),
                Constraint::Min(6),
                Constraint::Length(3),
            ])
            .split(frame.size());

        self.draw_header(frame, layout[0]);
        self.draw_transport(frame, layout[1]);
        self.draw_status(frame, layout[2]);
    }

    fn draw_header(&self, frame: &mut Frame, area: ratatui::prelude::Rect) {
        let text = Line::from(vec![
            Span::styled("Space", Style::default().fg(Color::Yellow)),
            Span::raw(" play/pause  "),
            Span::styled("s", Style::default().fg(Color::Yellow)),
            Span::raw(" stop  "),
            Span::styled("r", Style::default().fg(Color::Yellow)),
            Span::raw(" record  "),
            Span::styled("←/→", Style::default().fg(Color::Yellow)),
            Span::raw(" scrub  "),
            Span::styled("+/-", Style::default().fg(Color::Yellow)),
            Span::raw(" tempo  "),
            Span::styled("q", Style::default().fg(Color::Yellow)),
            Span::raw(" quit"),
        ]);

        let widget = Paragraph::new(text)
            .wrap(Wrap { trim: true })
            .block(
                Block::default()
                    .borders(Borders::ALL)
                    .title("Shortcuts"),
            );

        frame.render_widget(widget, area);
    }

    fn draw_transport(&self, frame: &mut Frame, area: ratatui::prelude::Rect) {
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Min(3),
                Constraint::Length(3),
            ])
            .split(area);

        let position = self.transport.playhead_position.time.to_seconds();
        let ratio = (position / self.length_seconds).clamp(0.0, 1.0);

        let gauge = Gauge::default()
            .block(
                Block::default()
                    .title("Transport Progress")
                    .borders(Borders::ALL),
            )
            .gauge_style(
                Style::default()
                    .fg(Color::Green)
                    .bg(Color::Black)
                    .add_modifier(Modifier::BOLD),
            )
            .ratio(ratio)
            .label(format!("{position:6.2}s / {total:6.2}s", total = self.length_seconds));

        frame.render_widget(gauge, chunks[0]);

        let stats = Paragraph::new(self.stats_line())
            .block(
                Block::default()
                    .title("Stats")
                    .borders(Borders::ALL),
            );

        frame.render_widget(stats, chunks[1]);
    }

    fn draw_status(&self, frame: &mut Frame, area: ratatui::prelude::Rect) {
        let widget = Paragraph::new(self.status_line())
            .block(
                Block::default()
                    .borders(Borders::ALL)
                    .title("Status"),
            );
        frame.render_widget(widget, area);
    }

    fn stats_line(&self) -> Line<'static> {
        let play_state = format!("State: {:?}", self.transport.play_state);
        let tempo = format!("Tempo: {:>5.1} BPM", self.transport.tempo.bpm);
        let position =
            format!("Position: {:>6.2}s", self.transport.playhead_position.time.to_seconds());
        let record = format!("Record: {:?}", self.transport.record_mode);
        let looping = format!("Looping: {}", if self.transport.looping { "On" } else { "Off" });

        Line::from(vec![
            Span::raw(play_state),
            Span::raw("  |  "),
            Span::raw(tempo),
            Span::raw("  |  "),
            Span::raw(position),
            Span::raw("  |  "),
            Span::raw(record),
            Span::raw("  |  "),
            Span::raw(looping),
        ])
    }

    fn status_line(&self) -> Line<'static> {
        Line::from(vec![
            Span::styled(
                "Message: ",
                Style::default().fg(Color::LightCyan).add_modifier(Modifier::BOLD),
            ),
            Span::raw(self.status_message.clone()),
        ])
    }
}
