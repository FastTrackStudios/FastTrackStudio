//! High-level vim engine: a `ModalMachine` + binding bundle.
//!
//! Hosts construct one [`VimEngine`] per buffer (or one global
//! engine, depending on the navigation surface), feed it
//! `DioxusKey`s as the user types, and drain emitted [`VimAction`]
//! values to apply to their model.
//!
//! The actual binding tables live in [`crate::bindings`], split
//! into category files (motions / inserts / blocks / modes /…)
//! so adding a new command is a focused diff in one file. See
//! `plans/logseq-data-model-alignment.md` for the coverage map.

use keybindings::{BindingMachine, EmptyKeyState, InputBindings, ModalMachine};

use crate::action::{Motion, VimAction};
use crate::bindings;
use crate::key::DioxusKey;
use crate::mode::{VimMode, VimStep};

/// Operator that takes the *next key as a literal char parameter*
/// rather than completing through the keybindings graph. Used for
/// vim's `f{ch}` / `F{ch}` / `t{ch}` / `T{ch}` find-char family
/// plus marks (`m{a-z}`, `'{a-z}`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PendingOp {
    FindChar { direction: i8, till: bool },
    SetMark,
    JumpToMark,
}

/// The concrete machine type for our setup.
pub type VimMachine = ModalMachine<DioxusKey, VimStep>;

/// Default binding bundle — registers every category. Hosts that
/// want a stripped-down keymap can implement their own
/// [`InputBindings`] and call only the categories they need.
#[derive(Default)]
pub struct DefaultBindings;

impl InputBindings<DioxusKey, VimStep> for DefaultBindings {
    fn setup(&self, m: &mut VimMachine) {
        bindings::motions::register(m);
        bindings::inserts::register(m);
        bindings::blocks::register(m);
        bindings::modes::register(m);
        bindings::yanks::register(m);
    }
}

/// User-facing vim engine — owns a `ModalMachine` and exposes a
/// thin `feed` / `mode` API.
///
/// Wraps the modal machine with two pieces of vim state the
/// upstream `keybindings` crate doesn't model for us:
/// - **Count buffer** — `3j` means "down three lines"; we capture
///   leading digits in Normal/Visual and multiply the next motion.
/// - **Force-Normal escape hatch** — for focus-loss / route-change
///   bookkeeping.
pub struct VimEngine {
    machine: VimMachine,
    /// In-progress count prefix. `None` until the user presses a
    /// non-zero digit in Normal or Visual mode; cleared after the
    /// next action emission (or by Escape).
    count: Option<u32>,
    /// Operator awaiting a literal-char parameter (`f`/`F`/`t`/`T`,
    /// `m`, `'`). Set in Normal/Visual mode; the next character
    /// keystroke completes it and emits the action.
    pending_op: Option<PendingOp>,
    /// Last successful find — drives `;` (repeat) and `,`
    /// (reverse-repeat).
    last_find: Option<(char, i8, bool)>,
}

impl Default for VimEngine {
    fn default() -> Self {
        Self {
            machine: VimMachine::from_bindings::<DefaultBindings>(),
            count: None,
            pending_op: None,
            last_find: None,
        }
    }
}

impl VimEngine {
    pub fn mode(&self) -> VimMode {
        self.machine.mode()
    }

    /// The currently-pending count, if the user has typed digits
    /// since the last completed action. Useful for status-line
    /// rendering (`3` shows up while typing `3j`).
    pub fn pending_count(&self) -> Option<u32> {
        self.count
    }

    /// Feed a key into the machine and return any actions the
    /// machine produced as a result. Multi-step sequences (`gg`,
    /// `dd`, `>>`) emit nothing on the first key and the action
    /// on the second.
    ///
    /// Counts are applied transparently: pressing `3` then `j`
    /// returns three `Move(LineDown)` actions. The host doesn't
    /// have to know about counts — it just applies actions
    /// in order.
    pub fn feed(&mut self, key: DioxusKey) -> Vec<VimAction> {
        // Escape always clears any pending count + operator.
        if matches!(key, DioxusKey::Escape) {
            self.count = None;
            self.pending_op = None;
        }

        // Complete a pending operator (`f`/`F`/`t`/`T`/`m`/`'`)
        // — the next char keystroke is the parameter, not a
        // keymap lookup.
        if let Some(op) = self.pending_op.take() {
            if let DioxusKey::Char(c) = key {
                let count = self.count.take().unwrap_or(1).max(1);
                let action = match op {
                    PendingOp::FindChar { direction, till } => {
                        self.last_find = Some((c, direction, till));
                        VimAction::Move(Motion::FindChar {
                            ch: c,
                            direction,
                            till,
                        })
                    }
                    PendingOp::SetMark => VimAction::SetMark(c),
                    PendingOp::JumpToMark => VimAction::JumpToMark(c),
                };
                let n = if is_repeatable(action) {
                    count as usize
                } else {
                    1
                };
                return std::iter::repeat(action).take(n).collect();
            }
            // Non-char key cancels the pending operator silently.
            return vec![];
        }

        // Capture digit prefix in Normal / Visual modes only —
        // Insert mode passes everything to the textarea.
        if matches!(self.mode(), VimMode::Normal | VimMode::Visual) {
            if let DioxusKey::Char(c) = key {
                if c.is_ascii_digit() {
                    let d = c.to_digit(10).expect("digit");
                    // Bare `0` (no prior digits) is a motion to
                    // line start — fall through to the modal
                    // machine. Subsequent `0`s extend the count.
                    if !(c == '0' && self.count.is_none()) {
                        self.count = Some(self.count.unwrap_or(0) * 10 + d);
                        return vec![];
                    }
                }
            }
        }

        // Intercept the operators that take a next-char parameter.
        if matches!(self.mode(), VimMode::Normal | VimMode::Visual) {
            if let DioxusKey::Char(c) = key {
                let op = match c {
                    'f' => Some(PendingOp::FindChar {
                        direction: 1,
                        till: false,
                    }),
                    'F' => Some(PendingOp::FindChar {
                        direction: -1,
                        till: false,
                    }),
                    't' => Some(PendingOp::FindChar {
                        direction: 1,
                        till: true,
                    }),
                    'T' => Some(PendingOp::FindChar {
                        direction: -1,
                        till: true,
                    }),
                    'm' => Some(PendingOp::SetMark),
                    '\'' => Some(PendingOp::JumpToMark),
                    _ => None,
                };
                if let Some(op) = op {
                    self.pending_op = Some(op);
                    return vec![];
                }
                // `;` / `,` repeat the last find.
                if c == ';' || c == ',' {
                    if let Some((ch, direction, till)) = self.last_find {
                        let count = self.count.take().unwrap_or(1).max(1);
                        let dir = if c == ',' { -direction } else { direction };
                        let action = VimAction::Move(Motion::FindChar {
                            ch,
                            direction: dir,
                            till,
                        });
                        return std::iter::repeat(action).take(count as usize).collect();
                    }
                    return vec![];
                }
            }
        }

        // Don't consume the count yet — multi-key sequences like
        // `dd` need it to survive across the first `d`. We only
        // consume on the key that actually emits an action.
        self.machine.input_key(key);
        let mut emitted = Vec::new();
        while let Some((action, _ctx)) = self.machine.pop() {
            emitted.push(action);
        }
        if emitted.is_empty() {
            return vec![];
        }
        let count = self.count.take().unwrap_or(1).max(1);
        let mut out = Vec::with_capacity(emitted.len() * count as usize);
        for a in emitted {
            let n = if is_repeatable(a) { count } else { 1 };
            for _ in 0..n {
                out.push(a);
            }
        }
        out
    }

    /// Reset the machine to Normal mode without going through the
    /// keystroke graph. Useful when the host needs to force-cancel
    /// (focus loss, route change).
    pub fn force_normal(&mut self) {
        self.count = None;
        let _ = self.feed(DioxusKey::Escape);
    }

    /// Reserved for forward-compat once we add registers / marks.
    #[allow(dead_code)]
    fn ctx(&self) -> EmptyKeyState {
        EmptyKeyState::default()
    }
}

/// Actions whose effect makes sense to repeat — motions and
/// per-character / per-block ops. Mode transitions, insert
/// entries, and visual-mode entry are emitted once regardless of
/// count.
fn is_repeatable(a: VimAction) -> bool {
    matches!(
        a,
        VimAction::Move(_) | VimAction::DeleteChar | VimAction::Block(_)
    )
    // SetMark / JumpToMark intentionally not repeatable — `3ma`
    // shouldn't set the same mark three times.
}
