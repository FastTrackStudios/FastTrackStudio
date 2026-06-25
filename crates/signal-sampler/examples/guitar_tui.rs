//! Live guitar-rig TUI — input/output meters + patch switching (ratatui).
//!
//! ```text
//! cargo run -p signal-sampler --features jack --example guitar_tui -- --rig "Guitar Rig"
//! # (via just: `just guitar`)
//! ```
//!
//! Keys: `1`-`9` pick patch · `←`/`→` or `p`/`n` prev/next · `b`/space bypass ·
//! `q`/Esc quit. The INPUT meter shows your guitar level reaching the rig; the
//! OUTPUT meter shows what the rig is sending out. If you're playing and INPUT
//! stays flat, the interface/channel isn't routed to the rig's input port (wire
//! it in qpwgraph); if OUTPUT moves but you hear nothing, the rig's output isn't
//! connected to your speakers.

use std::process::Command;
use std::time::Duration;

use ratatui::crossterm::event::{self, Event, KeyCode, KeyEventKind};
use ratatui::layout::{Constraint, Layout};
use ratatui::style::{Color, Modifier, Style, Stylize};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Gauge, Paragraph};
use ratatui::Frame;
use signal_sampler::{ProfileRig, RigManager};

fn arg(args: &[String], flag: &str) -> Option<String> {
    args.iter().position(|a| a == flag).and_then(|i| args.get(i + 1).cloned())
}

fn main() -> eyre::Result<()> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let name = arg(&args, "--rig").unwrap_or_else(|| "Guitar Rig".to_string());

    // Open the rig BEFORE taking over the terminal so any error prints normally.
    let mut mgr = RigManager::load(&name);
    let mut prig = mgr.open()?;
    let audio = {
        let p = prig.rig().prefs();
        let dev = if p.input_device.is_empty() { "default".into() } else { p.input_device.clone() };
        format!("{dev}  ch{}  @ {} Hz", p.input_channel + 1, prig.rig().sample_rate)
    };

    // Wire the interface DI channel → rig input, and rig output → the
    // interface's first stereo pair, over PipeWire. The rig's JACK client ports
    // are discovered from the graph (cpal names them `cpal_client_in/out`); the
    // application shows as FTS-Signal via PIPEWIRE_PROPS (set in `just rig`).
    // Best-effort; prints what it did.
    for line in autoconnect_io(&mgr.audio.input_device, mgr.audio.input_channel) {
        eprintln!("{line}");
    }

    // Force the graph quantum so the *interface device* runs low-latency too —
    // a per-node hint only lowers our processing node, leaving the TF's own
    // buffer huge (measured 1536 fr @ 1024 quantum vs 640 fr @ 128). This is
    // graph-wide while the rig runs and is reset to auto on exit.
    force_quantum(mgr.audio.buffer_size);

    let mut term = ratatui::init();
    let res = run(&mut term, &mut prig, &mut mgr, &name, &audio);
    ratatui::restore();
    force_quantum(0); // restore the system to automatic quantum
    res
}

/// Buffer sizes selectable at runtime (frames). Applied as the PipeWire
/// `clock.force-quantum` so the change is live (graph-wide).
const BUFFERS: [u32; 6] = [32, 64, 128, 256, 512, 1024];

/// Force the PipeWire graph quantum (live buffer size). `0` = unforced (auto).
fn force_quantum(frames: u32) {
    let _ = Command::new("pw-metadata")
        .args(["-n", "settings", "0", "clock.force-quantum", &frames.to_string()])
        .status();
}

/// Step the buffer size up (`+1`) or down (`-1`), apply it live, and persist.
fn step_buffer(mgr: &mut RigManager, dir: i32) {
    let cur = mgr.audio.buffer_size;
    let idx = BUFFERS.iter().position(|&b| b >= cur).unwrap_or(2) as i32;
    let nidx = (idx + dir).clamp(0, BUFFERS.len() as i32 - 1) as usize;
    let n = BUFFERS[nidx];
    mgr.audio.buffer_size = n;
    force_quantum(n);
    let _ = mgr.save();
}

/// Run `pw-link` with args, returning stdout (empty on failure).
fn pw_link(args: &[&str]) -> String {
    Command::new("pw-link")
        .args(args)
        .output()
        .map(|o| String::from_utf8_lossy(&o.stdout).into_owned())
        .unwrap_or_default()
}

/// Find a node in `listing` (`pw-link -i`/`-o` output) whose name contains all
/// `needles` (lower-case). Returns the node name (text before `:`).
fn find_node(listing: &str, needles: &[&str]) -> Option<String> {
    listing.lines().find_map(|l| {
        let low = l.to_lowercase();
        if needles.iter().all(|n| low.contains(*n)) {
            l.split(':').next().map(str::to_string)
        } else {
            None
        }
    })
}

/// The rig's JACK input / output client node names. cpal names them
/// `cpal_client_in` / `cpal_client_out`; discover them from the live graph.
fn rig_io_nodes() -> (Option<String>, Option<String>) {
    (
        find_node(&pw_link(&["-i"]), &["cpal", "in_"]),
        find_node(&pw_link(&["-o"]), &["cpal", "out_"]),
    )
}

/// Link `src -> dst`. When `exclusive`, first drop any *other* sources already
/// feeding `dst` — used for the rig input so only the guitar reaches it. NOT
/// used for the interface playback ports, which also carry everyday audio.
fn relink(src: &str, dst: &str, exclusive: bool, log: &mut Vec<String>) {
    if exclusive {
        let listing = pw_link(&["-l"]);
        let mut in_block = false;
        for line in listing.lines() {
            if !line.starts_with(char::is_whitespace) {
                in_block = line.trim() == dst;
            } else if in_block {
                if let Some(s) = line.trim().strip_prefix("|<- ").map(str::trim) {
                    if s != src {
                        let _ = Command::new("pw-link").args(["-d", s, dst]).status();
                    }
                }
            }
        }
    }
    let ok = Command::new("pw-link")
        .arg(src)
        .arg(dst)
        .status()
        .map(|s| s.success())
        .unwrap_or(false);
    log.push(format!(
        "  {} {src} -> {dst}",
        if ok { "linked" } else { "could not link (already linked?)" }
    ));
}

/// Connect the interface DI channel → rig input, and rig output → the
/// interface's first stereo pair. `input_match` is the interface name substring
/// (e.g. "Yamaha TF"); `channel` is 0-based (4th input = 3).
fn autoconnect_io(input_match: &str, channel: usize) -> Vec<String> {
    let mut log = Vec::new();
    if input_match.is_empty() {
        return log; // default device — let pipewire's own auto-connect handle it
    }
    let needle = input_match.to_lowercase().replace(' ', "_");
    let (in_node, out_node) = rig_io_nodes();

    // Input: interface capture_{ch+1} → rig in_{ch} (exclusive: only the guitar).
    let cap = find_node(&pw_link(&["-o"]), &[&needle, "capture", "alsa_input"]);
    match (cap, &in_node) {
        (Some(c), Some(ri)) => relink(
            &format!("{c}:capture_{}", channel + 1),
            &format!("{ri}:in_{channel}"),
            true,
            &mut log,
        ),
        (None, _) => log.push(format!("  no capture source matching {input_match:?}")),
        (_, None) => log.push("  rig input client not found (is the engine up?)".into()),
    }

    // Output: rig out_0/out_1 → interface playback_1/2 (additive: leave any
    // everyday-audio links to those ports in place).
    let play = find_node(&pw_link(&["-i"]), &[&needle, "playback", "alsa_output"]);
    match (play, &out_node) {
        (Some(p), Some(ro)) => {
            for (o, pb) in [(0usize, 1usize), (1, 2)] {
                relink(&format!("{ro}:out_{o}"), &format!("{p}:playback_{pb}"), false, &mut log);
            }
        }
        (None, _) => log.push(format!("  no playback sink matching {input_match:?}")),
        (_, None) => log.push("  rig output client not found".into()),
    }
    log
}

fn run(
    term: &mut ratatui::DefaultTerminal,
    prig: &mut ProfileRig,
    mgr: &mut RigManager,
    rig_name: &str,
    audio: &str,
) -> eyre::Result<()> {
    loop {
        term.draw(|f| ui(f, prig, rig_name, audio))?;

        if event::poll(Duration::from_millis(33))? {
            if let Event::Key(k) = event::read()? {
                if k.kind != KeyEventKind::Press {
                    continue;
                }
                match k.code {
                    KeyCode::Char('q') | KeyCode::Esc => break,
                    KeyCode::Char('b') | KeyCode::Char(' ') => {
                        let b = !prig.rig().is_bypassed();
                        prig.rig().set_bypass(b);
                    }
                    KeyCode::Char('n') | KeyCode::Right => {
                        prig.next_patch();
                    }
                    KeyCode::Char('p') | KeyCode::Left => {
                        prig.prev_patch();
                    }
                    // Buffer size (live, via PipeWire force-quantum).
                    KeyCode::Char('[') => step_buffer(mgr, -1),
                    KeyCode::Char(']') => step_buffer(mgr, 1),
                    KeyCode::Char('r') => prig.rig().reset_render_peak(),
                    KeyCode::Char(c) if c.is_ascii_digit() && c != '0' => {
                        prig.activate(c as usize - '1' as usize);
                    }
                    _ => {}
                }
            }
        }
    }
    Ok(())
}

fn ui(f: &mut Frame, prig: &ProfileRig, rig_name: &str, audio: &str) {
    let rig = prig.rig();
    let rows = Layout::vertical([
        Constraint::Length(2), // header
        Constraint::Length(2), // patches
        Constraint::Length(3), // input meter
        Constraint::Length(3), // output meter
        Constraint::Length(3), // dsp load
        Constraint::Length(2), // stats (buffer/latency + xruns)
        Constraint::Min(0),    // spacer
        Constraint::Length(1), // footer
    ])
    .split(f.area());

    // Header
    let bypassed = rig.is_bypassed();
    let header = Line::from(vec![
        Span::styled(format!(" {rig_name} "), Style::new().fg(Color::Black).bg(Color::Rgb(232, 129, 58)).bold()),
        Span::raw("  "),
        Span::styled(
            prig.profile_name().unwrap_or("(no profile)").to_string(),
            Style::new().fg(Color::White).add_modifier(Modifier::BOLD),
        ),
        Span::raw("   in: "),
        Span::styled(audio.to_string(), Style::new().fg(Color::DarkGray)),
        if bypassed {
            Span::styled("   [BYPASS]", Style::new().fg(Color::Yellow).bold())
        } else {
            Span::raw("")
        },
    ]);
    f.render_widget(Paragraph::new(header), rows[0]);

    // Patches
    let mut spans = vec![Span::styled("patch: ", Style::new().fg(Color::DarkGray))];
    for (i, p) in prig.patches().iter().enumerate() {
        let active = Some(i) == prig.active_index();
        let avail = prig.is_patch_available(i);
        let label = format!(" {}:{} ", i + 1, p.name);
        let style = if active {
            Style::new().fg(Color::Black).bg(Color::Rgb(232, 129, 58)).bold()
        } else if avail {
            Style::new().fg(Color::Gray)
        } else {
            Style::new().fg(Color::Red).add_modifier(Modifier::DIM)
        };
        spans.push(Span::styled(label, style));
        spans.push(Span::raw(" "));
    }
    if prig.patches().is_empty() {
        spans.push(Span::styled("(no patches loaded)", Style::new().fg(Color::Red)));
    }
    f.render_widget(Paragraph::new(Line::from(spans)), rows[1]);

    // Meters
    meter(f, rows[2], "INPUT  (guitar)", rig.input_peak());
    meter(f, rows[3], "OUTPUT (to speakers)", rig.output_peak());

    // DSP load — render time vs the per-block budget (buffer / sample-rate).
    let bf = rig.block_frames().max(1) as f64;
    let sr = rig.sample_rate.max(1) as f64;
    let budget_us = bf / sr * 1e6;
    let render_us = rig.render_us() as f64;
    let peak_us = rig.peak_render_us() as f64;
    let load = (render_us / budget_us).clamp(0.0, 1.0);
    let load_color = if load >= 0.85 {
        Color::Red
    } else if load >= 0.6 {
        Color::Yellow
    } else {
        Color::Green
    };
    let dsp = Gauge::default()
        .block(Block::bordered().title("DSP LOAD"))
        .gauge_style(Style::new().fg(load_color))
        .ratio(load)
        .label(format!(
            "{:.0}%   {:.2}/{:.2} ms   peak {:.2} ms",
            load * 100.0,
            render_us / 1000.0,
            budget_us / 1000.0,
            peak_us / 1000.0
        ));
    f.render_widget(dsp, rows[4]);

    // Stats — buffer + honest latency estimate + xruns.
    //
    // Software round-trip ≈ capture block + ring bridge + playback block. The
    // audio interface (USB transport + the TF's AD/DA converters) adds more on
    // top that we can't measure here — call it out so the number isn't
    // mistaken for the felt latency.
    let ms = |frames: f64| frames / sr * 1000.0;
    let ring = rig.ring_frames() as f64;
    let sw_ms = ms(bf + ring + bf); // in-block + ring + out-block
    let xruns = rig.underruns() + rig.overruns();
    let stats = vec![
        Line::from(Span::styled(
            format!(
                "buffer {} fr ({:.2} ms)   ring {:.0} fr ({:.2} ms)   {} Hz",
                rig.block_frames(),
                ms(bf),
                ring,
                ms(ring),
                rig.sample_rate
            ),
            Style::new().fg(Color::Gray),
        )),
        Line::from(vec![
            Span::styled(
                format!("≈ {sw_ms:.1} ms software round-trip"),
                Style::new().fg(Color::Cyan),
            ),
            Span::styled(
                "  (+ USB & TF converters on top)   ",
                Style::new().fg(Color::DarkGray),
            ),
            Span::styled(
                format!("xruns u{} o{}", rig.underruns(), rig.overruns()),
                Style::new().fg(if xruns > 0 { Color::Red } else { Color::DarkGray }),
            ),
        ]),
    ];
    f.render_widget(Paragraph::new(stats), rows[5]);

    // Footer
    let footer = Paragraph::new(Line::from(Span::styled(
        " [1-9] patch  ←/→ patch  [b] bypass  [ / ] buffer  [r] reset peak  [q] quit",
        Style::new().fg(Color::DarkGray),
    )));
    f.render_widget(footer, rows[7]);
}

/// Render one horizontal dB meter (−60…0 dB) as a colored gauge.
fn meter(f: &mut Frame, area: ratatui::layout::Rect, title: &str, peak: f32) {
    let db = if peak <= 1e-6 { -120.0 } else { 20.0 * peak.log10() };
    let ratio = (((db + 60.0) / 60.0) as f64).clamp(0.0, 1.0);
    let color = if db >= -1.0 {
        Color::Red
    } else if db >= -6.0 {
        Color::Yellow
    } else if db <= -119.0 {
        Color::DarkGray
    } else {
        Color::Green
    };
    let label = if db <= -119.0 {
        "  —  ".to_string()
    } else {
        format!("{db:.1} dB")
    };
    let gauge = Gauge::default()
        .block(Block::bordered().title(title))
        .gauge_style(Style::new().fg(color))
        .ratio(ratio)
        .label(label);
    f.render_widget(gauge, area);
}
