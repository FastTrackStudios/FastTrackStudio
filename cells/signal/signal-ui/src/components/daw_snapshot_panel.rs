//! DAW Snapshot Panel — snapshot slots with A/B morph controls backed by real DAW FX captures.
//!
//! Combines numbered snapshot slots (save/recall) with a morph slider that
//! interpolates between any two snapshots using the `MorphEngine`.
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────┐
//! │  DAW Snapshots                          < Page 01 >        │
//! ├─────────────────────────────────────────────────────────────┤
//! │  [1] 💾  Verse Clean      ✓  │  [2] 💾  Chorus Drive    ○  │
//! │  [3] 💾  Bridge Ambient   ○  │  [4]     ─ Slot 4 ─      ○  │
//! │  [5]     ─ Slot 5 ─       ○  │  [6]     ─ Slot 6 ─      ○  │
//! │  [7]     ─ Slot 7 ─       ○  │  [8]     ─ Slot 8 ─      ○  │
//! ├─────────────────────────────────────────────────────────────┤
//! │  Morph: [A: Slot 1 ▾]  ─────[●]─────  [B: Slot 2 ▾]      │
//! │  Easing: [Linear ▾]                                        │
//! └─────────────────────────────────────────────────────────────┘
//! ```

use crate::prelude::*;
use signal_control::morph_engine::EasingCurve;
use uuid::Uuid;

// ─────────────────────────────────────────────────────────────────────────────
// Slot data types
// ─────────────────────────────────────────────────────────────────────────────

/// A DAW snapshot slot — holds a reference to a persisted parameter snapshot.
#[derive(Clone, Debug, PartialEq)]
pub struct DawSnapshotSlot {
    /// Slot index (0-based within page).
    pub index: usize,
    /// Custom display name.
    pub name: Option<String>,
    /// Persisted snapshot UUID (from SQLite). `None` = empty slot.
    pub snapshot_id: Option<Uuid>,
}

impl DawSnapshotSlot {
    pub fn empty(index: usize) -> Self {
        Self {
            index,
            name: None,
            snapshot_id: None,
        }
    }

    pub fn display_name(&self) -> String {
        self.name
            .clone()
            .unwrap_or_else(|| format!("Slot {}", self.index + 1))
    }

    pub fn is_filled(&self) -> bool {
        self.snapshot_id.is_some()
    }
}

/// State for all DAW snapshot slot pages.
#[derive(Clone, Debug, PartialEq)]
pub struct DawSnapshotSlotsState {
    pub pages: Vec<Vec<DawSnapshotSlot>>,
    pub current_page: usize,
    pub slots_per_page: usize,
    pub last_recalled: Option<(usize, usize)>,
}

impl Default for DawSnapshotSlotsState {
    fn default() -> Self {
        Self::new(8, 1)
    }
}

impl DawSnapshotSlotsState {
    pub fn new(slots_per_page: usize, page_count: usize) -> Self {
        let pages = (0..page_count)
            .map(|_| (0..slots_per_page).map(DawSnapshotSlot::empty).collect())
            .collect();
        Self {
            pages,
            current_page: 0,
            slots_per_page,
            last_recalled: None,
        }
    }

    pub fn current_slots(&self) -> &[DawSnapshotSlot] {
        self.pages
            .get(self.current_page)
            .map(|p| p.as_slice())
            .unwrap_or(&[])
    }

    pub fn slot_mut(&mut self, slot_index: usize) -> Option<&mut DawSnapshotSlot> {
        self.pages
            .get_mut(self.current_page)
            .and_then(|page| page.get_mut(slot_index))
    }

    pub fn save_to_slot(&mut self, slot_index: usize, snapshot_id: Uuid, name: String) {
        if let Some(slot) = self.slot_mut(slot_index) {
            slot.snapshot_id = Some(snapshot_id);
            if slot.name.is_none() {
                slot.name = Some(name);
            }
        }
    }

    pub fn next_page(&mut self) {
        if self.current_page + 1 >= self.pages.len() {
            let new_page = (0..self.slots_per_page)
                .map(DawSnapshotSlot::empty)
                .collect();
            self.pages.push(new_page);
        }
        self.current_page += 1;
    }

    pub fn prev_page(&mut self) {
        self.current_page = self.current_page.saturating_sub(1);
    }

    pub fn page_count(&self) -> usize {
        self.pages.len()
    }

    /// Remove a snapshot from a slot (delete/overwrite).
    pub fn clear_slot(&mut self, slot_index: usize) {
        if let Some(slot) = self.slot_mut(slot_index) {
            slot.snapshot_id = None;
            slot.name = None;
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Global signals for DAW snapshot state
// ─────────────────────────────────────────────────────────────────────────────

/// DAW snapshot slots state.
pub static DAW_SNAPSHOT_SLOTS: GlobalSignal<DawSnapshotSlotsState> =
    Signal::global(DawSnapshotSlotsState::default);

/// Morph slider position [0.0, 1.0].
pub static MORPH_POSITION: GlobalSignal<f64> = Signal::global(|| 0.0);

/// Selected easing curve for morphing.
pub static MORPH_EASING: GlobalSignal<EasingCurve> = Signal::global(|| EasingCurve::Linear);

/// Morph A slot index (page, slot_index).
pub static MORPH_SLOT_A: GlobalSignal<Option<(usize, usize)>> = Signal::global(|| None);

/// Morph B slot index (page, slot_index).
pub static MORPH_SLOT_B: GlobalSignal<Option<(usize, usize)>> = Signal::global(|| None);

// ─────────────────────────────────────────────────────────────────────────────
// Props
// ─────────────────────────────────────────────────────────────────────────────

/// Callbacks for DAW snapshot operations (wired by the parent).
#[derive(Props, Clone, PartialEq)]
pub struct DawSnapshotPanelProps {
    /// Called when user saves current FX state to a slot. Returns (slot_index, name).
    pub on_save: Callback<(usize, String)>,
    /// Called when user recalls a snapshot by UUID.
    pub on_recall: Callback<Uuid>,
    /// Called when morph position changes — parent should apply interpolated params.
    pub on_morph: Callback<(f64, EasingCurve)>,
    /// Called when a slot is deleted.
    pub on_delete: Callback<(usize, Uuid)>,
}

// ─────────────────────────────────────────────────────────────────────────────
// Component
// ─────────────────────────────────────────────────────────────────────────────

/// DAW Snapshot Panel — slots grid + morph controls.
#[component]
pub fn DawSnapshotPanel(props: DawSnapshotPanelProps) -> Element {
    let slots_state = DAW_SNAPSHOT_SLOTS.read();
    let current_page = slots_state.current_page;
    let _page_count = slots_state.page_count();
    let slots = slots_state.current_slots().to_vec();
    let last_recalled = slots_state.last_recalled;

    let morph_pos = *MORPH_POSITION.read();
    let morph_easing = *MORPH_EASING.read();
    let morph_a = *MORPH_SLOT_A.read();
    let morph_b = *MORPH_SLOT_B.read();

    let mut editing_slot = use_signal(|| Option::<usize>::None);
    let mut edit_text = use_signal(String::new);
    let mut context_menu_slot = use_signal(|| Option::<usize>::None);

    rsx! {
        div { class: "h-full w-full flex flex-col bg-card",
            // Header
            div { class: "flex items-center justify-between px-3 py-2 border-b border-border",
                h3 { class: "text-xs font-semibold text-muted-foreground uppercase tracking-wider",
                    "DAW Snapshots"
                }
                div { class: "flex items-center gap-1",
                    button {
                        class: "px-1.5 py-0.5 text-xs text-muted-foreground hover:text-foreground \
                                hover:bg-accent rounded transition-colors disabled:opacity-30",
                        disabled: current_page == 0,
                        onclick: move |_| { DAW_SNAPSHOT_SLOTS.write().prev_page(); },
                        "<"
                    }
                    span { class: "text-xs font-medium text-muted-foreground px-1 min-w-[60px] text-center select-none",
                        "Page {current_page + 1:02}"
                    }
                    button {
                        class: "px-1.5 py-0.5 text-xs text-muted-foreground hover:text-foreground \
                                hover:bg-accent rounded transition-colors",
                        onclick: move |_| { DAW_SNAPSHOT_SLOTS.write().next_page(); },
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
                            let snapshot_id = slot.snapshot_id;
                            let is_last_recalled = last_recalled == Some((current_page, slot_index));
                            let is_morph_a = morph_a == Some((current_page, slot_index));
                            let is_morph_b = morph_b == Some((current_page, slot_index));
                            let is_editing = editing_slot() == Some(slot_index);
                            let show_context = context_menu_slot() == Some(slot_index);
                            let shortcut_num = slot_index + 1;

                            rsx! {
                                div {
                                    key: "{current_page}-{slot_index}",
                                    class: if is_morph_a {
                                        "relative flex flex-col rounded-lg border border-blue-600/60 bg-blue-950/30 p-1.5 transition-all"
                                    } else if is_morph_b {
                                        "relative flex flex-col rounded-lg border border-orange-600/60 bg-orange-950/30 p-1.5 transition-all"
                                    } else if is_last_recalled {
                                        "relative flex flex-col rounded-lg border border-green-600/60 bg-green-950/30 p-1.5 transition-all"
                                    } else if is_filled {
                                        "relative flex flex-col rounded-lg border border-zinc-700 bg-zinc-800/60 p-1.5 \
                                         hover:border-zinc-600 transition-all"
                                    } else {
                                        "relative flex flex-col rounded-lg border border-zinc-800 bg-zinc-900/40 p-1.5 \
                                         hover:border-zinc-700 transition-all"
                                    },
                                    // Right-click context menu
                                    oncontextmenu: move |evt| {
                                        evt.prevent_default();
                                        if is_filled {
                                            *context_menu_slot.write() = Some(slot_index);
                                        }
                                    },
                                    onclick: move |_| {
                                        // Close context menu on any click
                                        *context_menu_slot.write() = None;
                                    },

                                    // Morph indicator badges
                                    if is_morph_a {
                                        span { class: "absolute top-0.5 right-1 text-[9px] font-bold text-blue-400", "A" }
                                    }
                                    if is_morph_b {
                                        span { class: "absolute top-0.5 right-1 text-[9px] font-bold text-orange-400", "B" }
                                    }

                                    // Top row: number + name
                                    div { class: "flex items-center gap-1.5 mb-1",
                                        span { class: "text-[10px] font-bold text-zinc-500 bg-zinc-800 \
                                                       rounded px-1 py-0.5 min-w-[18px] text-center select-none",
                                            "{shortcut_num}"
                                        }
                                        if is_editing {
                                            input {
                                                class: "flex-1 text-xs bg-zinc-700 text-zinc-200 rounded px-1 py-0.5 \
                                                        border border-zinc-600 outline-none focus:border-blue-500 min-w-0",
                                                value: "{edit_text}",
                                                autofocus: true,
                                                oninput: move |evt| { *edit_text.write() = evt.value(); },
                                                onkeydown: move |evt| {
                                                    if evt.key() == Key::Enter {
                                                        let text = edit_text().clone();
                                                        let name = if text.trim().is_empty() { None } else { Some(text) };
                                                        if let Some(slot) = DAW_SNAPSHOT_SLOTS.write().slot_mut(slot_index) {
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
                                                    if let Some(slot) = DAW_SNAPSHOT_SLOTS.write().slot_mut(slot_index) {
                                                        slot.name = name;
                                                    }
                                                    *editing_slot.write() = None;
                                                },
                                            }
                                        } else {
                                            span {
                                                class: if is_filled {
                                                    "flex-1 text-xs text-zinc-200 truncate cursor-pointer hover:text-white"
                                                } else {
                                                    "flex-1 text-xs text-zinc-500 italic truncate cursor-pointer hover:text-zinc-400"
                                                },
                                                onclick: move |_| {
                                                    *edit_text.write() = {
                                                        let state = DAW_SNAPSHOT_SLOTS.read();
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

                                    // Bottom row: save + recall buttons
                                    div { class: "flex items-center gap-1",
                                        // Save button
                                        button {
                                            class: "flex-1 flex items-center justify-center gap-1 px-1.5 py-1 \
                                                    rounded text-[10px] font-medium bg-zinc-800 text-zinc-400 \
                                                    border border-zinc-700 hover:bg-zinc-700 hover:text-zinc-300 \
                                                    transition-colors",
                                            onclick: {
                                                let on_save = props.on_save.clone();
                                                move |_| {
                                                    let name = {
                                                        let state = DAW_SNAPSHOT_SLOTS.read();
                                                        state.current_slots()
                                                            .get(slot_index)
                                                            .map(|s| s.display_name())
                                                            .unwrap_or_else(|| format!("Slot {}", slot_index + 1))
                                                    };
                                                    on_save.call((slot_index, name));
                                                }
                                            },
                                            "Save"
                                        }
                                        // Recall button
                                        button {
                                            class: if is_last_recalled {
                                                "flex-1 flex items-center justify-center gap-1 px-1.5 py-1 \
                                                 rounded text-[10px] font-medium bg-green-900/50 text-green-300 \
                                                 border border-green-700/50 transition-colors"
                                            } else {
                                                "flex-1 flex items-center justify-center gap-1 px-1.5 py-1 \
                                                 rounded text-[10px] font-medium bg-zinc-800 text-zinc-400 \
                                                 border border-zinc-700 hover:bg-zinc-700 hover:text-zinc-300 \
                                                 transition-colors disabled:opacity-30 disabled:cursor-not-allowed"
                                            },
                                            disabled: !is_filled,
                                            onclick: {
                                                let on_recall = props.on_recall.clone();
                                                move |_| {
                                                    if let Some(id) = snapshot_id {
                                                        DAW_SNAPSHOT_SLOTS.write().last_recalled = Some((current_page, slot_index));
                                                        on_recall.call(id);
                                                    }
                                                }
                                            },
                                            "Recall"
                                        }
                                    }

                                    // Context menu (right-click)
                                    if show_context {
                                        div {
                                            class: "absolute top-full left-0 mt-1 z-50 min-w-[100px] py-1 \
                                                    bg-zinc-800 border border-zinc-700 rounded-lg shadow-xl",
                                            button {
                                                class: "w-full text-left px-3 py-1.5 text-xs text-zinc-300 \
                                                        hover:bg-zinc-700 transition-colors",
                                                onclick: move |_| {
                                                    *edit_text.write() = {
                                                        let state = DAW_SNAPSHOT_SLOTS.read();
                                                        state.current_slots()
                                                            .get(slot_index)
                                                            .and_then(|s| s.name.clone())
                                                            .unwrap_or_default()
                                                    };
                                                    *editing_slot.write() = Some(slot_index);
                                                    *context_menu_slot.write() = None;
                                                },
                                                "Rename"
                                            }
                                            button {
                                                class: "w-full text-left px-3 py-1.5 text-xs text-zinc-300 \
                                                        hover:bg-zinc-700 transition-colors",
                                                onclick: {
                                                    let on_save = props.on_save.clone();
                                                    move |_| {
                                                        let name = {
                                                            let state = DAW_SNAPSHOT_SLOTS.read();
                                                            state.current_slots()
                                                                .get(slot_index)
                                                                .map(|s| s.display_name())
                                                                .unwrap_or_else(|| format!("Slot {}", slot_index + 1))
                                                        };
                                                        on_save.call((slot_index, name));
                                                        *context_menu_slot.write() = None;
                                                    }
                                                },
                                                "Overwrite"
                                            }
                                            button {
                                                class: "w-full text-left px-3 py-1.5 text-xs text-red-400 \
                                                        hover:bg-zinc-700 transition-colors",
                                                onclick: {
                                                    let on_delete = props.on_delete.clone();
                                                    move |_| {
                                                        if let Some(id) = snapshot_id {
                                                            on_delete.call((slot_index, id));
                                                            DAW_SNAPSHOT_SLOTS.write().clear_slot(slot_index);
                                                        }
                                                        *context_menu_slot.write() = None;
                                                    }
                                                },
                                                "Delete"
                                            }
                                            // Assign to morph A/B
                                            button {
                                                class: "w-full text-left px-3 py-1.5 text-xs text-blue-400 \
                                                        hover:bg-zinc-700 transition-colors",
                                                onclick: move |_| {
                                                    *MORPH_SLOT_A.write() = Some((current_page, slot_index));
                                                    *context_menu_slot.write() = None;
                                                },
                                                "Set as Morph A"
                                            }
                                            button {
                                                class: "w-full text-left px-3 py-1.5 text-xs text-orange-400 \
                                                        hover:bg-zinc-700 transition-colors",
                                                onclick: move |_| {
                                                    *MORPH_SLOT_B.write() = Some((current_page, slot_index));
                                                    *context_menu_slot.write() = None;
                                                },
                                                "Set as Morph B"
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Morph section
            div { class: "border-t border-border px-3 py-2",
                div { class: "flex items-center gap-2 mb-1.5",
                    span { class: "text-[10px] font-semibold text-muted-foreground uppercase", "Morph" }
                }
                // A/B indicators + slider
                div { class: "flex items-center gap-2",
                    // A label
                    span {
                        class: "text-[10px] font-bold text-blue-400 min-w-[50px] truncate",
                        {
                            let label = morph_a.and_then(|(_p, idx)| {
                                let state = DAW_SNAPSHOT_SLOTS.read();
                                state.current_slots().get(idx).map(|s| s.display_name())
                            }).unwrap_or_else(|| "—".to_string());
                            label
                        }
                    }
                    // Slider
                    div { class: "flex-1 relative h-6 flex items-center",
                        div { class: "absolute inset-x-0 h-1 bg-zinc-700 rounded-full" }
                        div {
                            class: "absolute left-0 h-1 bg-gradient-to-r from-blue-500 to-orange-500 rounded-full",
                            style: "width: {(morph_pos * 100.0).round()}%",
                        }
                        input {
                            r#type: "range",
                            class: "absolute inset-0 w-full h-full opacity-0 cursor-pointer z-10",
                            min: "0",
                            max: "1000",
                            value: "{(morph_pos * 1000.0).round() as i64}",
                            disabled: morph_a.is_none() || morph_b.is_none(),
                            oninput: {
                                let on_morph = props.on_morph.clone();
                                move |evt| {
                                    if let Ok(val) = evt.value().parse::<f64>() {
                                        let pos = val / 1000.0;
                                        *MORPH_POSITION.write() = pos;
                                        let easing = *MORPH_EASING.read();
                                        on_morph.call((pos, easing));
                                    }
                                }
                            },
                        }
                        div {
                            class: "absolute w-4 h-4 rounded-full bg-white border-2 border-zinc-400 \
                                    shadow-md pointer-events-none transform -translate-x-1/2",
                            style: "left: {(morph_pos * 100.0).round()}%",
                        }
                    }
                    // B label
                    span {
                        class: "text-[10px] font-bold text-orange-400 min-w-[50px] truncate text-right",
                        {
                            let label = morph_b.and_then(|(_p, idx)| {
                                let state = DAW_SNAPSHOT_SLOTS.read();
                                state.current_slots().get(idx).map(|s| s.display_name())
                            }).unwrap_or_else(|| "—".to_string());
                            label
                        }
                    }
                }
                // Easing selector
                div { class: "flex items-center gap-2 mt-1",
                    span { class: "text-[10px] text-muted-foreground", "Easing:" }
                    select {
                        class: "text-[10px] bg-zinc-800 text-zinc-300 border border-zinc-700 \
                                rounded px-1.5 py-0.5 outline-none focus:border-blue-500",
                        value: "{morph_easing.label()}",
                        onchange: move |evt| {
                            let easing = match evt.value().as_str() {
                                "Linear" => EasingCurve::Linear,
                                "Ease In" => EasingCurve::EaseIn,
                                "Ease Out" => EasingCurve::EaseOut,
                                "Ease In/Out" => EasingCurve::EaseInOut,
                                _ => EasingCurve::Linear,
                            };
                            *MORPH_EASING.write() = easing;
                        },
                        for curve in EasingCurve::all() {
                            option {
                                value: "{curve.label()}",
                                selected: *curve == morph_easing,
                                "{curve.label()}"
                            }
                        }
                    }
                }
            }

            // Footer
            div { class: "px-3 py-1.5 border-t border-border",
                p { class: "text-[10px] text-muted-foreground text-center",
                    "Right-click slot for options  |  Set A/B for morph"
                }
            }
        }
    }
}
