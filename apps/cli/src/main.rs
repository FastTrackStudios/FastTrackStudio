mod app;
mod colors;
mod handlers;
mod ui;
mod widgets;

use std::io;
use std::time::Duration;

use color_eyre::eyre::Result;
use crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyEventKind},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{
    backend::CrosstermBackend,
    Terminal,
};

use app::CliSetlistApp;

const TICK_RATE: Duration = Duration::from_millis(50);

fn main() -> Result<()> {
    color_eyre::install()?;
    enable_raw_mode()?;

    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;

    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;
    terminal.clear()?;

    let mut app = CliSetlistApp::new();

    let result = run_app(&mut terminal, &mut app);

    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen, DisableMouseCapture)?;
    terminal.show_cursor()?;

    result
}

fn run_app<B: ratatui::backend::Backend>(
    terminal: &mut Terminal<B>,
    app: &mut CliSetlistApp,
) -> Result<()> {
    loop {
        terminal.draw(|frame| app.draw(frame))?;

        let timeout = TICK_RATE
            .checked_sub(app.last_tick.elapsed())
            .unwrap_or(Duration::from_secs(0));

        if event::poll(timeout)? {
            match event::read()? {
                Event::Key(key) => {
                    if key.kind == KeyEventKind::Press {
                        app.handle_key(key);
                    }
                }
                Event::Mouse(mouse) => {
                    app.handle_mouse(mouse);
                }
                _ => {}
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
