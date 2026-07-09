//! Piano view — full-range keyboard with falling-note waterfall.
//!
//! Plays notes via `signal_ui::ProcessingChain` (test tone for now;
//! will route through the sampler engine in a later phase).

use dioxus::prelude::*;
use signal_ui::ProcessingChain;
use signal_ui::components::piano::{Piano, WaterfallNote, WaterfallState};

use crate::styles as s;

/// Full-screen piano view with keyboard + waterfall.
#[component]
pub fn PianoView(chain: ProcessingChain) -> Element {
    let mut active_notes: Signal<Vec<u8>> = use_signal(Vec::new);
    let mut waterfall: Signal<WaterfallState> = use_signal(WaterfallState::default);

    // Octave navigation
    let mut octave_offset: Signal<i32> = use_signal(|| 0);
    let start_note = (36i32 + *octave_offset.read() * 12).clamp(0, 108) as u8;
    let end_note = (start_note as i32 + 47).clamp(0, 127) as u8; // 4 octaves

    let wf_notes: Vec<WaterfallNote> = waterfall.read().notes.clone();

    rsx! {
        div {
            style: format!(
                "display:flex; flex-direction:column; width:100%; height:100%; \
                 background:{bg}; overflow:hidden;",
                bg = s::BG_APP,
            ),

            // Octave selector + info bar
            div {
                style: format!(
                    "display:flex; align-items:center; justify-content:space-between; \
                     padding:8px 16px; flex-shrink:0; \
                     border-bottom:1px solid {border};",
                    border = s::BORDER,
                ),

                // Octave down
                button {
                    onclick: move |_| {
                        let v = *octave_offset.read();
                        *octave_offset.write() = (v - 1).max(-3);
                    },
                    style: format!(
                        "width:{h}; height:{h}; border-radius:{r}; border:1px solid {border}; \
                         background:{bg}; color:{text}; font-size:16px; cursor:pointer;",
                        h = s::TOUCH_TARGET, r = s::RADIUS_SM,
                        border = s::BORDER, bg = s::BG_SURFACE, text = s::TEXT,
                    ),
                    "−"
                }

                // Range label
                {
                    let range_label = format!(
                        "C{} – C{}",
                        (start_note / 12) as i32 - 1,
                        (end_note / 12) as i32 - 1,
                    );
                    rsx! {
                        span {
                            style: format!(
                                "font-size:12px; font-weight:600; color:{dim}; font-family:{font};",
                                dim = s::TEXT_DIM, font = s::FONT_MONO,
                            ),
                            "{range_label}"
                        }
                    }
                }

                // Octave up
                button {
                    onclick: move |_| {
                        let v = *octave_offset.read();
                        *octave_offset.write() = (v + 1).min(3);
                    },
                    style: format!(
                        "width:{h}; height:{h}; border-radius:{r}; border:1px solid {border}; \
                         background:{bg}; color:{text}; font-size:16px; cursor:pointer;",
                        h = s::TOUCH_TARGET, r = s::RADIUS_SM,
                        border = s::BORDER, bg = s::BG_SURFACE, text = s::TEXT,
                    ),
                    "+"
                }
            }

            // Piano keyboard + waterfall (fills remaining space, scrollable horizontally)
            div {
                style: "flex:1; overflow:auto; -webkit-overflow-scrolling:touch;",
                Piano {
                    start_note: start_note,
                    end_note: end_note,
                    active_notes: active_notes.read().clone(),
                    waterfall_notes: wf_notes,
                    show_labels: true,
                    accent_color: s::ACCENT.to_string(),
                    on_note_on: move |note: u8| {
                        // Add to active set
                        let mut an = active_notes.write();
                        if !an.contains(&note) {
                            an.push(note);
                        }
                        drop(an);

                        waterfall.write().spawn(note, None);

                        // TODO: trigger SamplerRig / ProcessingChain note-on
                        tracing::debug!("note-on: {note}");
                    },
                    on_note_off: move |note: u8| {
                        active_notes.write().retain(|&n| n != note);
                        waterfall.write().release(note);
                        tracing::debug!("note-off: {note}");
                    },
                }
            }
        }
    }
}
