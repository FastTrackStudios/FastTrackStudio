//! Tab-visibility-based idle detector.
//!
//! Web-only utility hook. Returns a signal that fires `Some(IdleWindow)`
//! whenever the user comes back to the tab after the tab has been
//! continuously hidden for at least `threshold_secs` seconds. Native
//! builds compile down to a no-op signal that never fires.
//!
//! Limitation: this is *tab-level*, not *OS-level*. We can't see
//! whether the user is actually at their keyboard if the Task tab
//! still has focus — only whether the tab itself is hidden. Solidtime's
//! native build can do OS-level via wgctl/idletime; the web build is
//! limited to what `document.visibilityState` reveals.

use dioxus::prelude::*;

use super::idle::IdleWindow;

#[cfg(target_arch = "wasm32")]
pub fn use_idle_detector(threshold_secs: u32) -> Signal<Option<IdleWindow>> {
    use chrono::{DateTime, Utc};
    use std::cell::RefCell;
    use std::rc::Rc;
    use wasm_bindgen::JsCast;
    use wasm_bindgen::closure::Closure;

    let hidden_since: Signal<Option<DateTime<Utc>>> = use_signal(|| None);
    let output: Signal<Option<IdleWindow>> = use_signal(|| None);

    // We need a stable closure that survives the function returning.
    // `use_hook` gives us a per-component allocation slot.
    let _state: Rc<RefCell<Option<Closure<dyn FnMut()>>>> = use_hook(|| {
        let state: Rc<RefCell<Option<Closure<dyn FnMut()>>>> = Rc::new(RefCell::new(None));

        let mut hidden_since_sig = hidden_since;
        let mut output_sig = output;
        let threshold = threshold_secs as i64;

        let closure = Closure::wrap(Box::new(move || {
            let Some(window) = web_sys::window() else {
                return;
            };
            let Some(document) = window.document() else {
                return;
            };
            let state = document.visibility_state();
            let is_hidden = matches!(state, web_sys::VisibilityState::Hidden);
            let now = Utc::now();
            if is_hidden {
                if hidden_since_sig.read().is_none() {
                    hidden_since_sig.set(Some(now));
                }
            } else {
                let prev = *hidden_since_sig.read();
                if let Some(started) = prev {
                    let elapsed = (now - started).num_seconds().max(0);
                    if elapsed >= threshold {
                        output_sig.set(Some(IdleWindow {
                            started_at: started,
                            ended_at: now,
                        }));
                    }
                    hidden_since_sig.set(None);
                }
            }
        }) as Box<dyn FnMut()>);

        if let Some(window) = web_sys::window() {
            if let Some(document) = window.document() {
                let _ = document.add_event_listener_with_callback(
                    "visibilitychange",
                    closure.as_ref().unchecked_ref(),
                );
            }
        }
        *state.borrow_mut() = Some(closure);
        state
    });

    output
}

#[cfg(not(target_arch = "wasm32"))]
pub fn use_idle_detector(_threshold_secs: u32) -> Signal<Option<IdleWindow>> {
    // Native builds: tab visibility doesn't apply; return a signal that
    // never fires. (OS-level idle detection would go here in a future
    // native-only implementation.)
    use_signal(|| None)
}
