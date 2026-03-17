//! Signal action handlers for REAPER's action list.
//!
//! Wires the `signal_actions` declarations (from signal-proto) to concrete
//! handlers that call `switch_to_variation(n)` on the global SignalController.

use actions_proto::{ActionResult, LocalActionImplementation, LocalActionRegistration};
use signal::actions::signal_actions;
use std::collections::HashMap;
use std::sync::Arc;

/// Create a handler that switches to variation `n` (1-based).
fn variation_handler(n: usize) -> LocalActionImplementation {
    LocalActionImplementation::Supported(Arc::new(move || {
        let Some(ctrl) = crate::signal_bridge::controller() else {
            return ActionResult::failure("Signal controller not initialized");
        };
        let ctrl = ctrl.clone();
        tokio::task::spawn_local(async move {
            let result = ctrl.switch_to_variation(n).await;
            match result {
                signal::SwitchResult::Switched { name } => {
                    tracing::info!("Switched to variation {n}: {name}");
                }
                signal::SwitchResult::NoContext => {
                    tracing::debug!("No active context for variation switch");
                }
                signal::SwitchResult::OutOfBounds {
                    requested,
                    available,
                } => {
                    tracing::debug!("Variation {requested} out of bounds ({available} available)");
                }
                signal::SwitchResult::LoadError(e) => {
                    tracing::warn!("Variation switch load error: {e}");
                }
                signal::SwitchResult::ActivateError(e) => {
                    tracing::warn!("Variation switch activate error: {e}");
                }
            }
        });
        ActionResult::success()
    }))
}

fn next_section_handler() -> LocalActionImplementation {
    LocalActionImplementation::Supported(Arc::new(|| {
        let Some(ctrl) = crate::signal_bridge::controller() else {
            return ActionResult::failure("Signal controller not initialized");
        };
        let ctrl = ctrl.clone();
        tokio::task::spawn_local(async move {
            let _ = ctrl.next_variation().await;
        });
        ActionResult::success()
    }))
}

fn previous_section_handler() -> LocalActionImplementation {
    LocalActionImplementation::Supported(Arc::new(|| {
        let Some(ctrl) = crate::signal_bridge::controller() else {
            return ActionResult::failure("Signal controller not initialized");
        };
        let ctrl = ctrl.clone();
        tokio::task::spawn_local(async move {
            let _ = ctrl.previous_variation().await;
        });
        ActionResult::success()
    }))
}

/// Get all signal action registrations for `builtin_local_actions()`.
///
/// Maps the `declare_actions!` definitions from signal-proto to concrete
/// handler implementations using action ID matching.
pub fn signal_action_registrations() -> Vec<LocalActionRegistration> {
    // Build a map of action ID → handler
    let mut handlers: HashMap<&str, LocalActionImplementation> = HashMap::new();

    // Navigation actions
    handlers.insert(
        signal_actions::NEXT_SECTION.as_str(),
        next_section_handler(),
    );
    handlers.insert(
        signal_actions::PREVIOUS_SECTION.as_str(),
        previous_section_handler(),
    );

    // Variation switch actions 1–16
    // (17–24 registered too but 1–16 is the primary set)
    for n in 1..=24usize {
        if let Some(action_id) = signal::actions::switch_to_variation_action(n) {
            handlers.insert(action_id.as_str(), variation_handler(n));
        }
    }

    // Pair definitions with handlers; skip actions without handlers (Next/Previous Song)
    signal_actions::definitions()
        .into_iter()
        .filter_map(|def| {
            let handler = handlers.remove(def.id.as_str())?;
            Some(LocalActionRegistration {
                definition: def,
                implementation: handler,
            })
        })
        .collect()
}
