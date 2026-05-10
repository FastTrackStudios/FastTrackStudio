//! `StandaloneExtState` — sync global ext state sub-handle.
//!
//! `persist` is recorded but ignored — there is no on-disk state in the
//! standalone backend.

use daw_proto::{DawResult, sync::ExtState};

use super::daw::Standalone;

pub struct StandaloneExtState<'a> {
    daw: &'a Standalone,
    // Held to mirror the per-project handle shape used by other sub-handles,
    // even though global ext state is project-independent in this backend.
    _project_guid: String,
}

impl<'a> StandaloneExtState<'a> {
    pub(crate) fn new(daw: &'a Standalone, guid: String) -> Self {
        Self {
            daw,
            _project_guid: guid,
        }
    }
}

impl<'a> ExtState for StandaloneExtState<'a> {
    fn get(&self, section: &str, key: &str) -> Option<String> {
        let s = self.daw.state.lock().expect("standalone state poisoned");
        s.global_ext_state
            .get(&(section.to_string(), key.to_string()))
            .cloned()
    }

    fn set(&self, section: &str, key: &str, value: &str, _persist: bool) -> DawResult<()> {
        let mut s = self.daw.state.lock().expect("standalone state poisoned");
        s.global_ext_state
            .insert((section.to_string(), key.to_string()), value.to_string());
        Ok(())
    }

    fn delete(&self, section: &str, key: &str, _persist: bool) -> DawResult<()> {
        let mut s = self.daw.state.lock().expect("standalone state poisoned");
        s.global_ext_state
            .remove(&(section.to_string(), key.to_string()));
        Ok(())
    }

    fn has(&self, section: &str, key: &str) -> bool {
        let s = self.daw.state.lock().expect("standalone state poisoned");
        s.global_ext_state
            .contains_key(&(section.to_string(), key.to_string()))
    }
}
