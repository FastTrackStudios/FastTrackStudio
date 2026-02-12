//! Snapshot slots component — Snapshooter-style quick-access snapshot grid.
//!
//! Provides 8 numbered snapshot slots per page (expandable with page navigation)
//! for fast save/recall during performance:
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────┐
//! │  Snapshot Slots                           < Page 01 >       │
//! ├─────────────────────────────────────────────────────────────┤
//! │  [1] 💾  Verse Clean      ✓  │  [2] 💾  Chorus Drive    ○  │
//! │  [3] 💾  Bridge Ambient   ○  │  [4]     ─ Slot 4 ─      ○  │
//! │  [5]     ─ Slot 5 ─       ○  │  [6]     ─ Slot 6 ─      ○  │
//! │  [7]     ─ Slot 7 ─       ○  │  [8]     ─ Slot 8 ─      ○  │
//! └─────────────────────────────────────────────────────────────┘
//! ```
//!
//! Keyboard shortcuts: 1–8 to apply, Shift+1–8 to save.

use crate::components::morph_slider::SnapshotRef;
use crate::prelude::*;
use crate::signals::{RIG_CURRENT_PRESET, RIG_LAST_APPLIED_SNAPSHOT};
use uuid::Uuid;

// region: --- Constants

/// Slots per page (the default, expandable up to MAX_SLOTS_PER_PAGE).
pub const DEFAULT_SLOTS_PER_PAGE: usize = 8;

/// Maximum slots per page.
pub const MAX_SLOTS_PER_PAGE: usize = 12;

// endregion: --- Constants

// region: --- Slot Data

/// A single snapshot slot.
#[derive(Clone, Debug, PartialEq)]
pub struct SnapshotSlot {
    /// Slot index (0-based within page).
    pub index: usize,
    /// Custom name (user-editable). `None` means show "Slot N" placeholder.
    pub name: Option<String>,
    /// The snapshot assigned to this slot (if saved).
    pub snapshot: Option<SnapshotRef>,
}

impl SnapshotSlot {
    /// Create an empty slot at the given index.
    pub fn empty(index: usize) -> Self {
        Self {
            index,
            name: None,
            snapshot: None,
        }
    }

    /// Display name: custom name, snapshot name, or "Slot N" placeholder.
    pub fn display_name(&self) -> String {
        if let Some(ref name) = self.name {
            name.clone()
        } else if let Some(ref snap) = self.snapshot {
            snap.name.clone()
        } else {
            format!("Slot {}", self.index + 1)
        }
    }

    /// Whether this slot has a saved snapshot.
    pub fn is_filled(&self) -> bool {
        self.snapshot.is_some()
    }
}

/// State for all snapshot slot pages.
#[derive(Clone, Debug, PartialEq)]
pub struct SnapshotSlotsState {
    /// All pages of slots. Each page is a Vec of slots.
    pub pages: Vec<Vec<SnapshotSlot>>,
    /// Current page index (0-based).
    pub current_page: usize,
    /// Slots per page for this state.
    pub slots_per_page: usize,
    /// Which slot was last applied (page, slot_index).
    pub last_applied: Option<(usize, usize)>,
}

impl Default for SnapshotSlotsState {
    fn default() -> Self {
        Self::new(DEFAULT_SLOTS_PER_PAGE, 1)
    }
}

impl SnapshotSlotsState {
    /// Create with N slots per page and M initial pages.
    pub fn new(slots_per_page: usize, page_count: usize) -> Self {
        let pages = (0..page_count)
            .map(|_| (0..slots_per_page).map(SnapshotSlot::empty).collect())
            .collect();
        Self {
            pages,
            current_page: 0,
            slots_per_page,
            last_applied: None,
        }
    }

    /// Get the current page's slots.
    pub fn current_slots(&self) -> &[SnapshotSlot] {
        self.pages
            .get(self.current_page)
            .map(|p| p.as_slice())
            .unwrap_or(&[])
    }

    /// Get a mutable reference to a slot on the current page.
    pub fn slot_mut(&mut self, slot_index: usize) -> Option<&mut SnapshotSlot> {
        self.pages
            .get_mut(self.current_page)
            .and_then(|page| page.get_mut(slot_index))
    }

    /// Save a snapshot into a slot on the current page.
    pub fn save_to_slot(&mut self, slot_index: usize, snapshot: SnapshotRef) {
        if let Some(slot) = self.slot_mut(slot_index) {
            // If no custom name set, inherit the snapshot name
            if slot.name.is_none() {
                slot.name = Some(snapshot.name.clone());
            }
            slot.snapshot = Some(snapshot);
        }
    }

    /// Navigate to the next page, creating it if needed.
    pub fn next_page(&mut self) {
        if self.current_page + 1 >= self.pages.len() {
            // Create a new page
            let new_page = (0..self.slots_per_page).map(SnapshotSlot::empty).collect();
            self.pages.push(new_page);
        }
        self.current_page += 1;
    }

    /// Navigate to the previous page.
    pub fn prev_page(&mut self) {
        self.current_page = self.current_page.saturating_sub(1);
    }

    /// Total number of pages.
    pub fn page_count(&self) -> usize {
        self.pages.len()
    }
}

// endregion: --- Slot Data

// region: --- Global Signal

/// Global snapshot slots state — accessible from any dock panel.
pub static RIG_SNAPSHOT_SLOTS: GlobalSignal<SnapshotSlotsState> =
    Signal::global(SnapshotSlotsState::default);

// endregion: --- Global Signal

// region: --- Props

/// Props for the snapshot slots component.
#[derive(Props, Clone, PartialEq)]
pub struct SnapshotSlotsProps {
    /// Callback when user applies (recalls) a snapshot from a slot.
    pub on_apply: Callback<Uuid>,
    /// Callback when user saves the current state to a slot.
    pub on_save: Callback<usize>,
}

// endregion: --- Props

// region: --- SnapshotSlots Component

/// Snapshot slots grid with save/apply buttons and page navigation.
///
/// Reads the current preset's snapshots from `RIG_CURRENT_PRESET` and
/// stores slot assignments in `RIG_SNAPSHOT_SLOTS`.
#[component]
pub fn SnapshotSlots(props: SnapshotSlotsProps) -> Element {
    let slots_state = RIG_SNAPSHOT_SLOTS.read();
    let current_preset = RIG_CURRENT_PRESET.read();
    let active_snapshot_id = *RIG_LAST_APPLIED_SNAPSHOT.read();

    let current_page = slots_state.current_page;
    let page_count = slots_state.page_count();
    let slots = slots_state.current_slots().to_vec();
    let last_applied = slots_state.last_applied;

    // Editing state: which slot index is being renamed (None = not editing)
    let mut editing_slot = use_signal(|| Option::<usize>::None);
    let mut edit_text = use_signal(String::new);

    rsx! {
        div { class: "h-full w-full flex flex-col bg-card",
            // Header with page navigation
            div { class: "flex items-center justify-between px-3 py-2 border-b border-border",
                h3 { class: "text-xs font-semibold text-muted-foreground uppercase tracking-wider",
                    "Snapshot Slots"
                }
                // Page navigation
                div { class: "flex items-center gap-1",
                    button {
                        class: "px-1.5 py-0.5 text-xs text-muted-foreground hover:text-foreground \
                                hover:bg-accent rounded transition-colors disabled:opacity-30",
                        disabled: current_page == 0,
                        onclick: move |_| {
                            RIG_SNAPSHOT_SLOTS.write().prev_page();
                        },
                        "<"
                    }
                    span { class: "text-xs font-medium text-muted-foreground px-1 min-w-[60px] text-center select-none",
                        "Page {current_page + 1:02}"
                    }
                    button {
                        class: "px-1.5 py-0.5 text-xs text-muted-foreground hover:text-foreground \
                                hover:bg-accent rounded transition-colors",
                        onclick: move |_| {
                            RIG_SNAPSHOT_SLOTS.write().next_page();
                        },
                        ">"
                    }
                }
            }

            // Slot grid
            div { class: "flex-1 overflow-y-auto p-2",
                div { class: "grid grid-cols-2 gap-1.5",
                    for slot in slots.iter() {
                        {
                            let slot_index = slot.index;
                            let is_filled = slot.is_filled();
                            let display_name = slot.display_name();
                            let snapshot_id = slot.snapshot.as_ref().map(|s| s.id);
                            let is_last_applied = last_applied == Some((current_page, slot_index));
                            let is_active = snapshot_id.is_some() && snapshot_id == active_snapshot_id;
                            let is_editing = editing_slot() == Some(slot_index);
                            let shortcut_num = slot_index + 1;

                            rsx! {
                                div {
                                    key: "{current_page}-{slot_index}",
                                    class: if is_active {
                                        "flex flex-col rounded-lg border border-green-600/60 bg-green-950/30 p-1.5 \
                                         transition-all"
                                    } else if is_filled {
                                        "flex flex-col rounded-lg border border-zinc-700 bg-zinc-800/60 p-1.5 \
                                         hover:border-zinc-600 transition-all"
                                    } else {
                                        "flex flex-col rounded-lg border border-zinc-800 bg-zinc-900/40 p-1.5 \
                                         hover:border-zinc-700 transition-all"
                                    },

                                    // Top row: slot number + name (click to edit)
                                    div { class: "flex items-center gap-1.5 mb-1",
                                        // Slot number badge
                                        span { class: "text-[10px] font-bold text-zinc-500 bg-zinc-800 \
                                                       rounded px-1 py-0.5 min-w-[18px] text-center select-none",
                                            "{shortcut_num}"
                                        }

                                        // Name (editable on click)
                                        if is_editing {
                                            input {
                                                class: "flex-1 text-xs bg-zinc-700 text-zinc-200 rounded px-1 py-0.5 \
                                                        border border-zinc-600 outline-none focus:border-blue-500 \
                                                        min-w-0",
                                                value: "{edit_text}",
                                                autofocus: true,
                                                oninput: move |evt| {
                                                    *edit_text.write() = evt.value();
                                                },
                                                onkeydown: move |evt| {
                                                    if evt.key() == Key::Enter {
                                                        let text = edit_text().clone();
                                                        let name = if text.trim().is_empty() { None } else { Some(text) };
                                                        if let Some(slot) = RIG_SNAPSHOT_SLOTS.write().slot_mut(slot_index) {
                                                            slot.name = name;
                                                        }
                                                        *editing_slot.write() = None;
                                                    } else if evt.key() == Key::Escape {
                                                        *editing_slot.write() = None;
                                                    }
                                                },
                                                onfocusout: move |_| {
                                                    let text = edit_text().clone();
                                                    let name = if text.trim().is_empty() { None } else { Some(text) };
                                                    if let Some(slot) = RIG_SNAPSHOT_SLOTS.write().slot_mut(slot_index) {
                                                        slot.name = name;
                                                    }
                                                    *editing_slot.write() = None;
                                                },
                                            }
                                        } else {
                                            span {
                                                class: if is_filled {
                                                    "flex-1 text-xs text-zinc-200 truncate cursor-pointer \
                                                     hover:text-white"
                                                } else {
                                                    "flex-1 text-xs text-zinc-500 italic truncate cursor-pointer \
                                                     hover:text-zinc-400"
                                                },
                                                onclick: move |_| {
                                                    *edit_text.write() = {
                                                        let state = RIG_SNAPSHOT_SLOTS.read();
                                                        state.current_slots()
                                                            .get(slot_index)
                                                            .and_then(|s| s.name.clone())
                                                            .unwrap_or_default()
                                                    };
                                                    *editing_slot.write() = Some(slot_index);
                                                },
                                                "{display_name}"
                                            }
                                        }
                                    }

                                    // Bottom row: save + apply buttons
                                    div { class: "flex items-center gap-1",
                                        // Save button (captures current state into this slot)
                                        button {
                                            class: if is_filled {
                                                "flex-1 flex items-center justify-center gap-1 px-1.5 py-1 \
                                                 rounded text-[10px] font-medium bg-green-900/40 text-green-400 \
                                                 border border-green-800/40 hover:bg-green-800/50 transition-colors"
                                            } else {
                                                "flex-1 flex items-center justify-center gap-1 px-1.5 py-1 \
                                                 rounded text-[10px] font-medium bg-zinc-800 text-zinc-400 \
                                                 border border-zinc-700 hover:bg-zinc-700 hover:text-zinc-300 \
                                                 transition-colors"
                                            },
                                            disabled: current_preset.is_none(),
                                            onclick: {
                                                let on_save = props.on_save.clone();
                                                move |_| {
                                                    on_save.call(slot_index);
                                                }
                                            },
                                            // Floppy disk icon (save)
                                            svg {
                                                class: "w-3 h-3",
                                                fill: "none",
                                                stroke: "currentColor",
                                                stroke_width: "2",
                                                view_box: "0 0 24 24",
                                                path {
                                                    stroke_linecap: "round",
                                                    stroke_linejoin: "round",
                                                    d: "M19 21H5a2 2 0 01-2-2V5a2 2 0 012-2h11l5 5v11a2 2 0 01-2 2z",
                                                }
                                                polyline {
                                                    stroke_linecap: "round",
                                                    stroke_linejoin: "round",
                                                    points: "17 21 17 13 7 13 7 21",
                                                }
                                                polyline {
                                                    stroke_linecap: "round",
                                                    stroke_linejoin: "round",
                                                    points: "7 3 7 8 15 8",
                                                }
                                            }
                                            "Save"
                                        }

                                        // Apply button (recalls this slot's snapshot)
                                        button {
                                            class: if is_last_applied {
                                                "flex-1 flex items-center justify-center gap-1 px-1.5 py-1 \
                                                 rounded text-[10px] font-medium bg-blue-900/50 text-blue-300 \
                                                 border border-blue-700/50 hover:bg-blue-800/50 transition-colors"
                                            } else {
                                                "flex-1 flex items-center justify-center gap-1 px-1.5 py-1 \
                                                 rounded text-[10px] font-medium bg-zinc-800 text-zinc-400 \
                                                 border border-zinc-700 hover:bg-zinc-700 hover:text-zinc-300 \
                                                 transition-colors disabled:opacity-30 disabled:cursor-not-allowed"
                                            },
                                            disabled: !is_filled,
                                            onclick: {
                                                let on_apply = props.on_apply.clone();
                                                move |_| {
                                                    if let Some(snap_id) = snapshot_id {
                                                        RIG_SNAPSHOT_SLOTS.write().last_applied = Some((current_page, slot_index));
                                                        on_apply.call(snap_id);
                                                    }
                                                }
                                            },
                                            // Check icon when last applied, play icon otherwise
                                            if is_last_applied {
                                                svg {
                                                    class: "w-3 h-3",
                                                    fill: "none",
                                                    stroke: "currentColor",
                                                    stroke_width: "2.5",
                                                    view_box: "0 0 24 24",
                                                    path {
                                                        stroke_linecap: "round",
                                                        stroke_linejoin: "round",
                                                        d: "M20 6L9 17l-5-5",
                                                    }
                                                }
                                            } else {
                                                svg {
                                                    class: "w-3 h-3",
                                                    fill: "none",
                                                    stroke: "currentColor",
                                                    stroke_width: "2",
                                                    view_box: "0 0 24 24",
                                                    path {
                                                        stroke_linecap: "round",
                                                        stroke_linejoin: "round",
                                                        d: "M9 18l6-6-6-6",
                                                    }
                                                }
                                            }
                                            "Apply"
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Footer: keyboard shortcut hint
            div { class: "px-3 py-1.5 border-t border-border",
                p { class: "text-[10px] text-muted-foreground text-center",
                    "1-8 apply  |  Shift+1-8 save"
                }
            }
        }
    }
}

// endregion: --- SnapshotSlots Component

// region: --- Standalone Panel

/// Snapshot slots panel — standalone dock panel wrapper.
///
/// Initializes the rig service and provides save/apply wiring that
/// captures the current preset snapshot into a slot or recalls it.
#[component]
pub fn SnapshotSlotsPanel() -> Element {
    crate::signals::init_rig_service();
    crate::hooks::rig_state::use_rig_subscription();
    let actions = crate::hooks::rig_actions::use_rig_actions();

    let on_apply = {
        let activate = actions.activate_snapshot.clone();
        Callback::new(move |snapshot_id: Uuid| {
            activate.call(snapshot_id);
        })
    };

    let on_save = Callback::new(move |slot_index: usize| {
        // Capture the current active snapshot into the given slot
        let preset = RIG_CURRENT_PRESET.read();
        let active_id = *RIG_LAST_APPLIED_SNAPSHOT.read();

        if let (Some(ref _preset), Some(active_id)) = (&*preset, active_id) {
            // Create a snapshot ref from the active snapshot ID.
            // Snapshot name lookup will be added when DB-backed snapshots are wired up.
            let snap_ref = SnapshotRef {
                id: active_id,
                name: format!("Snapshot"),
            };
            RIG_SNAPSHOT_SLOTS
                .write()
                .save_to_slot(slot_index, snap_ref);
        }
    });

    rsx! {
        SnapshotSlots {
            on_apply,
            on_save,
        }
    }
}

// endregion: --- Standalone Panel
