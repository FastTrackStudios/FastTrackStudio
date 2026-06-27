//! Live strings-rig TUI — play a sampled string library from a hardware MIDI
//! keyboard (ratatui). Built to prove signal's sampler engine against a real
//! library: Cinematic Studio Strings, 1st Violins.
//!
//! ```text
//! cargo run -p signal-sampler --features pipewire --example strings_tui
//! # (via just: `just strings`)
//! ```
//!
//! Signal stays pure sampler business logic: the audio OUTPUT runs on daw's
//! `AudioEngine` and the MIDI INPUT comes from daw's `daw-midi-io` primitive
//! (device / all / virtual), forwarded into daw's live-MIDI ring by
//! [`SamplerRig::attach_midi`]. This example only expresses intent — which
//! library, section, mic, articulation, and MIDI source.
//!
//! Keys: `[`/`]` prev/next articulation · `,`/`.` prev/next mic · `space` panic
//! (all notes off) · `q`/Esc quit. The OUTPUT meter shows what the rig sends
//! out; VOICES shows active sampler voices. If a note is silent, it's still
//! warming — watch the WARM progress bar.

use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::Duration;

use ratatui::crossterm::event::{self, Event, KeyCode, KeyEventKind};
use ratatui::layout::{Constraint, Layout};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Gauge, Paragraph};
use ratatui::Frame;
use signal_sampler::{MidiSelection, SamplerRig};

const INSTRUMENT_ID: &str = "strings_1v";

/// Default Cinematic Studio Strings install (the WAV root the zone paths in
/// `_patches/<section>/library.styx` are relative to).
const CSS_ROOT: &str =
    "/run/media/AudioHaven/Sampled/Orchestral/Cinematic Series/Cinematic Studio Strings";

/// Default descriptive engine-config spec — articulations / keyswitch / CC58 /
/// legato / dynamics. Loaded alongside the zones so articulation switching
/// works. Lives in the sibling sample-collector repo.
const CSS_CONFIG: &str =
    "/run/media/Development/FastTrackStudio/sample-collector/specs/cinematic-strings.styx";

/// Articulations to cycle with `[` / `]` — the playable subset of 1st Violins.
/// `NVLeg` (non-vib legato) is the default base; CC2 blends in the vibrato pair
/// (`Leg`) on top, so the legato/sustain entries are the non-vib bases.
const ARTICULATIONS: &[&str] = &[
    "NVLeg",
    "Nonvib",
    "Marcato",
    "Spiccato",
    "Staccato",
    "Pizzicato",
    "Tremolo",
];

/// Mics to cycle with `,` / `.`. `Mix` (the pre-blended bus) is the default.
const MICS: &[&str] = &["Mix", "Main", "Room", "Spot1", "Spot2"];

/// Lowest / highest MIDI notes warmed at startup (covers the 1st Violins range
/// G2..C#6 with margin; notes with no zones simply warm nothing).
const WARM_LO: u8 = 40;
const WARM_HI: u8 = 88;

fn arg(args: &[String], flag: &str) -> Option<String> {
    args.iter()
        .position(|a| a == flag)
        .and_then(|i| args.get(i + 1).cloned())
}

fn main() -> eyre::Result<()> {
    let args: Vec<String> = std::env::args().skip(1).collect();

    let css_root = PathBuf::from(arg(&args, "--lib").unwrap_or_else(|| CSS_ROOT.to_string()));
    let section = arg(&args, "--section").unwrap_or_else(|| "1st Violins".to_string());
    // The per-section zone spec carries the actual sample paths.
    let spec_path = arg(&args, "--spec")
        .map(PathBuf::from)
        .unwrap_or_else(|| css_root.join("_patches").join(&section).join("library.styx"));
    let mut mic_idx = MICS
        .iter()
        .position(|m| Some(*m) == arg(&args, "--mic").as_deref())
        .unwrap_or(0);
    let mut artic_idx = ARTICULATIONS
        .iter()
        .position(|a| Some(*a) == arg(&args, "--artic").as_deref())
        .unwrap_or(0);
    let buffer: u32 = arg(&args, "--buffer").and_then(|s| s.parse().ok()).unwrap_or(256);
    // `all` (default), `virtual[:Name]`, or a device-name substring.
    let midi_sel = match arg(&args, "--midi").as_deref() {
        None | Some("all") => MidiSelection::All,
        Some("virtual") => MidiSelection::Virtual("FTS-Signal Strings".into()),
        Some(other) if other.starts_with("virtual:") => {
            MidiSelection::Virtual(other["virtual:".len()..].to_string())
        }
        Some(name) => MidiSelection::Port(name.to_string()),
    };

    if !spec_path.exists() {
        eyre::bail!(
            "spec not found: {}\n(point --lib at the CSS install, or --spec at a zones .styx)",
            spec_path.display()
        );
    }

    // Open the output engine + load the library BEFORE taking over the terminal,
    // so any error prints normally. Generous cache budget — CSS samples are big.
    let cache_budget = 6 * 1024 * 1024 * 1024; // 6 GiB
    let rig = SamplerRig::with_device_config_and_cache_budget(
        None,
        None,
        Some(buffer),
        Some(cache_budget),
    )?;
    // Load the zones WITH the descriptive engine config so articulations,
    // keyswitches and CC58 work. Falls back to zones-only if the config is
    // missing (articulation switching then disabled).
    let config_path = arg(&args, "--config")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(CSS_CONFIG));
    if config_path.exists() {
        rig.load_instrument_with_config(
            INSTRUMENT_ID,
            &config_path,
            &spec_path,
            &css_root,
            &section,
            MICS[mic_idx],
        )?;
    } else {
        eprintln!(
            "warning: config spec not found ({}) — articulations/keyswitches disabled",
            config_path.display()
        );
        rig.load_instrument(INSTRUMENT_ID, &spec_path, Some(&css_root), &section, MICS[mic_idx])?;
    }
    rig.set_solo_mic(INSTRUMENT_ID, Some(MICS[mic_idx].to_string()));
    // Live articulation (keyswitch / CC58 equivalent) — NOT a pin: the low-octave
    // keyswitches change it live from the keyboard, the TUI `[`/`]` keys nudge it.
    rig.set_articulation(INSTRUMENT_ID, ARTICULATIONS[artic_idx]);
    // Sensible starting expression: CC1 ≈ mf dynamic, CC2 ≈ moderate vibrato.
    // Drive these from your keyboard's mod wheel (CC1) + a CC2 control live.
    rig.cc(INSTRUMENT_ID, 1, 90);
    rig.cc(INSTRUMENT_ID, 2, 90);

    // Hardware MIDI in → daw live-MIDI ring → bank track. Held for the run.
    let _midi = rig
        .attach_midi(midi_sel)
        .map_err(|e| eyre::eyre!("MIDI input: {e} (is a keyboard connected? try `--midi all`)"))?;
    let midi_ports = _midi.opened.join(", ");

    // Background-warm the playable range for the current pin + mic so the first
    // notes aren't silent. Re-armed whenever the articulation / mic changes.
    let warm = WarmJob::spawn(&rig);

    redirect_stderr_to_log();
    let mut term = ratatui::init();
    let res = run(
        &mut term, &rig, &_midi_label(&midi_ports), &mut artic_idx, &mut mic_idx, &warm,
    );
    ratatui::restore();
    res
}

fn _midi_label(ports: &str) -> String {
    if ports.is_empty() {
        "(virtual / none)".to_string()
    } else {
        ports.to_string()
    }
}

/// A cancelable background warm of `WARM_LO..=WARM_HI` for the rig's current
/// pin + mic. Dropping / restarting cancels the prior pass.
struct WarmJob {
    done: Arc<AtomicUsize>,
    total: usize,
    cancel: Arc<AtomicBool>,
}

impl WarmJob {
    fn spawn(rig: &SamplerRig) -> Self {
        let done = Arc::new(AtomicUsize::new(0));
        let cancel = Arc::new(AtomicBool::new(false));
        let total = (WARM_HI - WARM_LO + 1) as usize;
        let (rig, d, c) = (rig.clone(), done.clone(), cancel.clone());
        std::thread::Builder::new()
            .name("strings-warm".into())
            .spawn(move || {
                for note in WARM_LO..=WARM_HI {
                    if c.load(Ordering::Relaxed) {
                        return;
                    }
                    rig.warm_note(INSTRUMENT_ID, note);
                    d.fetch_add(1, Ordering::Relaxed);
                }
            })
            .ok();
        Self { done, total, cancel }
    }

    fn ratio(&self) -> f64 {
        (self.done.load(Ordering::Relaxed) as f64 / self.total as f64).clamp(0.0, 1.0)
    }
}

fn run(
    term: &mut ratatui::DefaultTerminal,
    rig: &SamplerRig,
    midi_ports: &str,
    artic_idx: &mut usize,
    mic_idx: &mut usize,
    warm: &WarmJob,
) -> eyre::Result<()> {
    let mut warm = WarmJob {
        done: warm.done.clone(),
        total: warm.total,
        cancel: warm.cancel.clone(),
    };
    // Track the live articulation + mic so a change from ANY source — the TUI
    // keys OR a keyswitch / CC58 from the keyboard — re-warms the new samples.
    let mut last_artic = rig.articulation(INSTRUMENT_ID).unwrap_or_default();
    let mut last_mic = MICS[*mic_idx].to_string();
    loop {
        let live_artic = rig.articulation(INSTRUMENT_ID).unwrap_or_default();
        term.draw(|f| ui(f, rig, midi_ports, &live_artic, *mic_idx, &warm))?;

        if live_artic != last_artic || MICS[*mic_idx] != last_mic {
            last_artic = live_artic.clone();
            last_mic = MICS[*mic_idx].to_string();
            warm = rearm(rig, &warm);
        }

        if event::poll(Duration::from_millis(33))? {
            if let Event::Key(k) = event::read()? {
                if k.kind != KeyEventKind::Press {
                    continue;
                }
                match k.code {
                    KeyCode::Char('q') | KeyCode::Esc => break,
                    KeyCode::Char(' ') => rig.panic(INSTRUMENT_ID),
                    KeyCode::Char(']') => {
                        *artic_idx = (*artic_idx + 1) % ARTICULATIONS.len();
                        rig.set_articulation(INSTRUMENT_ID, ARTICULATIONS[*artic_idx]);
                    }
                    KeyCode::Char('[') => {
                        *artic_idx = (*artic_idx + ARTICULATIONS.len() - 1) % ARTICULATIONS.len();
                        rig.set_articulation(INSTRUMENT_ID, ARTICULATIONS[*artic_idx]);
                    }
                    KeyCode::Char('.') => {
                        *mic_idx = (*mic_idx + 1) % MICS.len();
                        rig.set_mic(INSTRUMENT_ID, MICS[*mic_idx]);
                        rig.set_solo_mic(INSTRUMENT_ID, Some(MICS[*mic_idx].into()));
                    }
                    KeyCode::Char(',') => {
                        *mic_idx = (*mic_idx + MICS.len() - 1) % MICS.len();
                        rig.set_mic(INSTRUMENT_ID, MICS[*mic_idx]);
                        rig.set_solo_mic(INSTRUMENT_ID, Some(MICS[*mic_idx].into()));
                    }
                    _ => {}
                }
            }
        }
    }
    Ok(())
}

/// Cancel the running warm pass and start a fresh one for the new articulation
/// / mic selection.
fn rearm(rig: &SamplerRig, prev: &WarmJob) -> WarmJob {
    prev.cancel.store(true, Ordering::Relaxed);
    WarmJob::spawn(rig)
}

fn ui(
    f: &mut Frame,
    rig: &SamplerRig,
    midi_ports: &str,
    live_artic: &str,
    mic_idx: usize,
    warm: &WarmJob,
) {
    let rows = Layout::vertical([
        Constraint::Length(3), // title
        Constraint::Length(3), // output meter
        Constraint::Length(3), // warm progress
        Constraint::Length(6), // status
        Constraint::Length(5), // keyswitch + expression hint
        Constraint::Min(0),    // help
    ])
    .split(f.area());

    let title = Paragraph::new(Line::from(vec![
        Span::raw("FTS-Signal · "),
        Span::styled(
            "Cinematic Studio Strings — 1st Violins",
            Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD),
        ),
    ]))
    .block(Block::bordered());
    f.render_widget(title, rows[0]);

    let out_db = rig.output_peak_db();
    let out_ratio = ((out_db + 60.0) / 60.0).clamp(0.0, 1.0);
    let out = Gauge::default()
        .block(Block::bordered().title("OUTPUT"))
        .gauge_style(Style::default().fg(meter_color(out_db)))
        .ratio(out_ratio)
        .label(format!("{out_db:>6.1} dB"));
    f.render_widget(out, rows[1]);

    let warm_ratio = warm.ratio();
    let warming = warm_ratio < 1.0;
    let warm_g = Gauge::default()
        .block(Block::bordered().title("WARM"))
        .gauge_style(Style::default().fg(if warming { Color::Yellow } else { Color::Green }))
        .ratio(warm_ratio)
        .label(if warming {
            format!("warming samples… {:>3.0}%", warm_ratio * 100.0)
        } else {
            "ready".to_string()
        });
    f.render_widget(warm_g, rows[2]);

    let stats = rig.audio_stats();
    let voices = rig.active_voices(INSTRUMENT_ID);
    let status = Paragraph::new(vec![
        Line::from(vec![
            Span::raw("articulation  "),
            Span::styled(
                if live_artic.is_empty() { "(all)" } else { live_artic },
                Style::default().fg(Color::Magenta).add_modifier(Modifier::BOLD),
            ),
            Span::raw("    mic  "),
            Span::styled(
                MICS[mic_idx],
                Style::default().fg(Color::Green).add_modifier(Modifier::BOLD),
            ),
        ]),
        Line::from(format!("MIDI in       {midi_ports}")),
        Line::from(format!(
            "voices {voices}   sample rate {} Hz   midi msgs {}",
            rig.sample_rate(),
            stats.midi_messages
        )),
        Line::from(format!(
            "xruns {}   dropped {}   render {} µs / {} µs budget",
            stats.callback_overruns, stats.dropped_events, stats.last_render_us, stats.buffer_budget_us
        )),
    ])
    .block(Block::bordered().title("status"));
    f.render_widget(status, rows[3]);

    // CSS-style velocity keyswitches: play these low notes (below the G2 range)
    // to switch articulation live — soft vs hard pick the variant.
    let ks = Paragraph::new(vec![
        Line::from(
            "C0 Sustain (soft=low-latency, hard=expressive legato)   C#0 Shorts (vel: spicc→stacc→sfz)",
        ),
        Line::from(
            "D0 Pizz (vel: pizz→bartók→col legno)   D#0 Trills   E0 Harm   F0 Trem   F#0 Marcato   A#0 NonVib",
        ),
        Line::from(
            "CC1 = dynamics (swell)    CC2 = vibrato (NonVib↔Vib)    CC58 = articulation",
        ),
    ])
    .style(Style::default().fg(Color::Cyan))
    .block(Block::bordered().title("keyswitches + expression"));
    f.render_widget(ks, rows[4]);

    let help = Paragraph::new(Line::from(
        "[ ] articulation   , . mic   space panic   q quit",
    ))
    .style(Style::default().fg(Color::DarkGray))
    .block(Block::bordered());
    f.render_widget(help, rows[5]);
}

fn meter_color(db: f64) -> Color {
    if db > -1.0 {
        Color::Red
    } else if db > -6.0 {
        Color::Yellow
    } else {
        Color::Green
    }
}

/// Point stderr at `$TMPDIR/fts-signal-rig.log` so stray engine/library logs
/// can't scribble over the ratatui render (it owns stdout). Mirrors guitar_tui.
fn redirect_stderr_to_log() {
    use std::os::fd::AsRawFd;
    let path = std::env::temp_dir().join("fts-signal-rig.log");
    if let Ok(file) = std::fs::OpenOptions::new().create(true).append(true).open(&path) {
        // SAFETY: dup2 onto STDERR_FILENO is the standard fd-redirect; the file's
        // fd is valid here. Leak it so the fd stays open for the process lifetime.
        unsafe { libc::dup2(file.as_raw_fd(), libc::STDERR_FILENO) };
        std::mem::forget(file);
    }
}
