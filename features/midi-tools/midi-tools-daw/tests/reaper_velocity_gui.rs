//! The velocity tool, end to end: real REAPER take, real UI, real drag.
//!
//! Everything else in this feature is tested in halves. The unit tests
//! prove the engines given `&[Note]`; `reaper_velocity.rs` proves the
//! write path given a `Session`; the standalone example proves the panel
//! renders. None of them prove the thing a user actually does — open the
//! tool on a take, drag something, and get the notes they expected.
//!
//! This closes that. Two real halves meet in one test:
//!
//! - **REAPER** (`#[reaper_test]`) owns the take. Notes go in through the
//!   daw RPC and come back out the same way.
//! - **The panel** runs on a headless Blitz DOM (`dioxus-test`), which is
//!   the *same renderer* the REAPER panel uses — not a WebView
//!   approximation. Pointer events are hit-tested against the resolved
//!   layout, so a `pointer_down` → `pointer_move` → `pointer_up` is a
//!   drag in the same sense the plugin editor gets one.
//!
//! ## Why the sink is a test double and not `DawVelocitySink`
//!
//! The panel's sink surface is synchronous (`fn notes(&self) -> Vec<..>`)
//! because in REAPER it runs in-process on the main thread. Out here the
//! DAW is across an RPC boundary and the client is async, and bridging
//! the two inside a running runtime is a deadlock waiting to happen.
//!
//! So the test carries the notes across itself: read from REAPER (async),
//! hand them to the panel through [`CaptureSink`] (sync), drive the UI,
//! take the resolved session back out, write it to REAPER (async). Every
//! piece is real — the notes, the layout, the drag, the engines, the
//! round trip. The only thing not exercised is the in-process glue in
//! `DawVelocitySink`, and `reaper_velocity.rs` covers exactly that.

use std::sync::{Arc, Mutex};

use daw::rpc::TakeHandle;
use daw::test::reaper_test;
use daw_proto::midi::MidiNoteCreate;
use daw_proto::primitives::{Duration, PositionInSeconds};
use dioxus_test::{by_testid, render};
use midi_tools::velocity::{Note, Session};
use midi_tools::{VelocitySink, velocity};
use midi_tools_ui::{SinkHandle, VelocityPanel};

const PPQ: f64 = 960.0;

// ─────────────────────────────────────────────────────────────────────
// The seam
// ─────────────────────────────────────────────────────────────────────

/// The session the panel resolved when Apply was last pressed.
///
/// A shared cell rather than a method on the sink: `SinkHandle` takes the
/// sink by value and boxes it, and the orphan rule blocks implementing
/// `VelocitySink` for `Arc<CaptureSink>`, so the test cannot keep a typed
/// handle to the sink it gave away. Sharing the *output* instead is
/// simpler than sharing the sink.
#[derive(Clone, Default)]
struct Captured(Arc<Mutex<Option<Session>>>);

impl Captured {
    /// The session as of the last Apply, or `None` if Apply never fired.
    fn get(&self) -> Option<Session> {
        self.0.lock().unwrap().clone()
    }
}

/// Hands the panel a fixed take and records what it resolves on Apply.
///
/// Deliberately not a mock that asserts on calls — it's a real
/// `VelocitySink` whose "DAW" happens to be a `Vec`. The panel cannot
/// tell the difference, which is the point.
struct CaptureSink {
    notes: Vec<Note>,
    applied: Captured,
}

impl VelocitySink for CaptureSink {
    fn open(&self) -> Result<Session, String> {
        Ok(Session::new(self.notes.clone()))
    }

    fn commit(&self, session: &Session) -> Result<usize, String> {
        let n = session.edits().len();
        *self.applied.0.lock().unwrap() = Some(session.clone());
        Ok(n)
    }

    fn revert(&self, session: &Session) -> Result<usize, String> {
        Ok(session.baseline().len())
    }

    fn resync(&self, _session: &mut Session) -> Result<(), String> {
        Ok(())
    }
}

// ─────────────────────────────────────────────────────────────────────
// REAPER side
// ─────────────────────────────────────────────────────────────────────

/// A run of eighth notes at a flat velocity, on a fresh track.
async fn take_with(
    ctx: &daw::test::ReaperTestContext,
    name: &str,
    velocities: &[u8],
) -> eyre::Result<TakeHandle> {
    let project = ctx.project().clone();
    let track = project.tracks().add(name, None).await?;
    let item = track
        .items()
        .add(
            PositionInSeconds::from_seconds(0.0),
            Duration::from_seconds(velocities.len() as f64 * 0.5),
        )
        .await?;
    let take = item.active_take();
    take.midi()
        .add_notes(
            velocities
                .iter()
                .enumerate()
                .map(|(i, v)| MidiNoteCreate {
                    pitch: 60,
                    velocity: *v,
                    channel: 0,
                    start_ppq: i as f64 * 0.5,
                    length_ppq: PPQ / 4.0,
                })
                .collect(),
        )
        .await?;
    Ok(take)
}

async fn read(take: &TakeHandle) -> eyre::Result<Vec<Note>> {
    Ok(take
        .midi()
        .notes()
        .await?
        .into_iter()
        .map(|n| Note {
            index: n.index,
            velocity: n.velocity,
            selected: n.selected,
        })
        .collect())
}

async fn velocities(take: &TakeHandle) -> eyre::Result<Vec<u8>> {
    Ok(read(take).await?.into_iter().map(|n| n.velocity).collect())
}

/// Push whatever the panel resolved into the take.
async fn write(take: &TakeHandle, session: &Session) -> eyre::Result<usize> {
    let edits = session.edits();
    for edit in &edits {
        take.midi().set_velocity(edit.index, edit.velocity).await?;
    }
    Ok(edits.len())
}

// ─────────────────────────────────────────────────────────────────────
// UI side
// ─────────────────────────────────────────────────────────────────────

/// Mount the real panel with `sink` behind it.
///
/// The window is sized generously: Blitz lays out for real, and a panel
/// squeezed narrower than its content would put controls on top of each
/// other and make the hit-tested coordinates meaningless.
fn mount(notes: Vec<Note>) -> (dioxus_test::DocumentTester, Captured) {
    let applied = Captured::default();
    let sink = CaptureSink {
        notes,
        applied: applied.clone(),
    };
    let tester = render(VelocityPanel)
        .with_root_context(SinkHandle::new(sink))
        .with_window_size(560, 900)
        .build();
    (tester, applied)
}

/// Click the centre of an element, by test id.
async fn click(tester: &dioxus_test::DocumentTester, testid: &str) -> dioxus_test::Result<()> {
    let el = tester.query(by_testid(testid)).immediately()?;
    let (ox, oy) = el.document_origin();
    let (w, h) = el.size();
    let (x, y) = (ox + w as f64 / 2.0, oy + h as f64 / 2.0);
    tester.pointer_down(x, y);
    let _ = tester.pump().await;
    tester.pointer_up(x, y);
    let _ = tester.pump().await;
    Ok(())
}

/// Drag horizontally across an element, from `from` to `to` as fractions
/// of its width.
///
/// Stepped rather than jumped: a single move would exercise nothing that
/// a click doesn't, and the bugs this is here to catch — value
/// oscillation, stale geometry — only appear across a sequence.
async fn drag_x(
    tester: &dioxus_test::DocumentTester,
    testid: &str,
    from: f64,
    to: f64,
) -> dioxus_test::Result<()> {
    let el = tester.query(by_testid(testid)).immediately()?;
    let (ox, oy) = el.document_origin();
    let (w, h) = el.size();
    let y = oy + h as f64 / 2.0;
    let x_at = |f: f64| ox + w as f64 * f;

    tester.pointer_down(x_at(from), y);
    let _ = tester.pump().await;
    for i in 1..=8 {
        let t = i as f64 / 8.0;
        tester.pointer_move(x_at(from + (to - from) * t), y, true);
        let _ = tester.pump().await;
    }
    tester.pointer_up(x_at(to), y);
    let _ = tester.pump().await;
    Ok(())
}

// ─────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────

/// The headline: a take in REAPER, a curve clicked in the real panel, and
/// the take comes out shaped like the curve.
#[reaper_test(isolated)]
async fn gui_curve_preset_shapes_a_real_take(
    ctx: &daw::test::ReaperTestContext,
) -> eyre::Result<()> {
    let take = take_with(ctx, "GUI Curve", &[64; 16]).await?;
    let before = velocities(&take).await?;
    ctx.log(&format!("before → {before:?}"));

    let (tester, applied) = mount(read(&take).await?);

    // "Rise" — the preset's own label, lowercased into a test id.
    click(&tester, "curve-rise").await.map_err(|e| eyre::eyre!("{e:?}"))?;
    click(&tester, "apply").await.map_err(|e| eyre::eyre!("{e:?}"))?;

    let session = applied
        .get()
        .ok_or_else(|| eyre::eyre!("Apply never reached the sink — the button did not fire"))?;

    let written = write(&take, &session).await?;
    let after = velocities(&take).await?;
    ctx.log(&format!("wrote {written} → {after:?}"));

    eyre::ensure!(after != before, "clicking a curve preset changed nothing");
    eyre::ensure!(
        after.windows(2).all(|w| w[0] <= w[1]),
        "a rise must arrive as a rise: {after:?}"
    );
    eyre::ensure!(
        after.first() < after.last(),
        "the ramp must actually climb: {after:?}"
    );
    Ok(())
}

/// Dragging the step-velocity amount slider — the control whose
/// hand-rolled version flickered. A drag that ends near the right of the
/// track must land near full blend, monotonically, with no oscillation.
#[reaper_test(isolated)]
async fn gui_dragging_the_amount_slider_blends_the_pattern(
    ctx: &daw::test::ReaperTestContext,
) -> eyre::Result<()> {
    let take = take_with(ctx, "GUI Drag", &[64; 8]).await?;

    let (tester, applied) = mount(read(&take).await?);

    drag_x(&tester, "pattern-amount", 0.0, 0.95)
        .await
        .map_err(|e| eyre::eyre!("{e:?}"))?;
    click(&tester, "apply").await.map_err(|e| eyre::eyre!("{e:?}"))?;

    let session = applied
        .get()
        .ok_or_else(|| eyre::eyre!("Apply never reached the sink"))?;

    let amount = session.pattern_amount;
    ctx.log(&format!("amount after drag → {amount:.3}"));
    eyre::ensure!(
        amount > 0.85,
        "a drag to 95% of the track should land near full, got {amount:.3}"
    );

    write(&take, &session).await?;
    let after = velocities(&take).await?;
    ctx.log(&format!("after → {after:?}"));

    // The default pattern is [100, 20, 90, 25], blended in almost fully
    // over 8 flat-64 notes — so it must alternate loud/quiet.
    eyre::ensure!(after[0] > after[1], "accent pattern did not land: {after:?}");
    eyre::ensure!(after[2] > after[3], "accent pattern did not land: {after:?}");
    Ok(())
}

/// A drag that returns to where it started must leave the take alone.
///
/// This is the non-destructiveness property, tested through the UI rather
/// than against `Session` directly — and it is exactly what a slider that
/// oscillates between values would fail.
#[reaper_test(isolated)]
async fn gui_dragging_back_to_zero_is_a_no_op(
    ctx: &daw::test::ReaperTestContext,
) -> eyre::Result<()> {
    let seed: Vec<u8> = (0..8).map(|i| 40 + i * 5).collect();
    let take = take_with(ctx, "GUI Round Trip", &seed).await?;

    let (tester, applied) = mount(read(&take).await?);

    drag_x(&tester, "pattern-amount", 0.0, 0.8)
        .await
        .map_err(|e| eyre::eyre!("{e:?}"))?;
    drag_x(&tester, "pattern-amount", 0.8, 0.0)
        .await
        .map_err(|e| eyre::eyre!("{e:?}"))?;
    click(&tester, "apply").await.map_err(|e| eyre::eyre!("{e:?}"))?;

    let session = applied
        .get()
        .ok_or_else(|| eyre::eyre!("Apply never reached the sink"))?;
    ctx.log(&format!(
        "amount back at {:.3}, {} edits",
        session.pattern_amount,
        session.edits().len()
    ));

    eyre::ensure!(
        session.edits().is_empty(),
        "dragging back to the start must resolve to no edits, got {:?}",
        session.edits()
    );

    write(&take, &session).await?;
    eyre::ensure!(
        velocities(&take).await? == seed,
        "the take must be untouched"
    );
    Ok(())
}

/// Drawing on the step-velocity bars sets the step under the pointer —
/// the widget whose stray-click behaviour I could only reason about
/// before, now pinned.
#[reaper_test(isolated)]
async fn gui_drawing_on_the_bars_edits_the_pattern(
    ctx: &daw::test::ReaperTestContext,
) -> eyre::Result<()> {
    let take = take_with(ctx, "GUI Bars", &[64; 8]).await?;

    let (tester, applied) = mount(read(&take).await?);

    let before = velocity::Pattern::default().steps().to_vec();

    // Draw near the top of the box across its whole width: every step
    // should end up high.
    let el = tester
        .query(by_testid("pattern-bars"))
        .immediately()
        .map_err(|e| eyre::eyre!("{e:?}"))?;
    let (ox, oy) = el.document_origin();
    let (w, h) = el.size();
    let y = oy + h as f64 * 0.1;
    tester.pointer_down(ox + w as f64 * 0.05, y);
    let _ = tester.pump().await;
    for i in 1..=10 {
        tester.pointer_move(ox + w as f64 * (0.05 + 0.9 * i as f64 / 10.0), y, true);
        let _ = tester.pump().await;
    }
    tester.pointer_up(ox + w as f64 * 0.95, y);
    let _ = tester.pump().await;

    click(&tester, "apply").await.map_err(|e| eyre::eyre!("{e:?}"))?;
    let session = applied
        .get()
        .ok_or_else(|| eyre::eyre!("Apply never reached the sink"))?;

    let after = session.pattern.steps().to_vec();
    ctx.log(&format!("pattern {before:?} → {after:?}"));

    eyre::ensure!(after != before, "drawing across the bars changed nothing");
    eyre::ensure!(
        after.iter().all(|&v| v > 90),
        "a stroke near the top should set every step high, got {after:?}"
    );
    Ok(())
}
