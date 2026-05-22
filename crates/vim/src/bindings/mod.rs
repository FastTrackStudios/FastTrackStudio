//! Binding tables, organized by category — mirroring the layout of
//! [logseq-plugin-vim-shortcuts](https://github.com/vipzhicheng/logseq-plugin-vim-shortcuts)
//! so adding a new command is a small, focused diff in one file.
//!
//! Each module's `register(machine)` adds its mappings to the
//! shared `VimMachine`. The default binding bundle in
//! [`crate::engine::DefaultBindings`] just calls each `register`
//! in order.

pub mod blocks;
pub mod inserts;
pub mod modes;
pub mod motions;
pub mod yanks;

use keybindings::{EdgeEvent, EdgeRepeat};

use crate::engine::VimMachine;
use crate::key::DioxusKey;
use crate::mode::{VimMode, VimStep};

/// Shorthand for adding a single mapping. Each binding file calls
/// this from its `register` fn.
pub(crate) fn bind(
    m: &mut VimMachine,
    mode: VimMode,
    seq: &[EdgeEvent<DioxusKey, keybindings::EmptyKeyClass>],
    step: VimStep,
) {
    let path: Vec<_> = seq
        .iter()
        .cloned()
        .map(|ev| (EdgeRepeat::Once, ev))
        .collect();
    m.add_mapping(mode, &path, &step);
}

/// Convenience: build an [`EdgeEvent::Key`] for a single character.
pub(crate) fn ch(c: char) -> EdgeEvent<DioxusKey, keybindings::EmptyKeyClass> {
    EdgeEvent::Key(DioxusKey::Char(c))
}

/// Convenience: build an [`EdgeEvent::Key`] for a named key.
pub(crate) fn k(k: DioxusKey) -> EdgeEvent<DioxusKey, keybindings::EmptyKeyClass> {
    EdgeEvent::Key(k)
}
