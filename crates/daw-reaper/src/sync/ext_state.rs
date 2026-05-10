//! Sync global ExtState handle: [`ReaperExtState`].

use std::ffi::CString;

use daw_proto::sync::ExtState as ExtStateTrait;
use daw_proto::{DawError, DawResult};
use reaper_high::Reaper;

use crate::safe_wrappers::ext_state as sw;

use super::ReaperMainThread;

pub struct ReaperExtState<'a> {
    _mt: &'a ReaperMainThread,
}

impl<'a> ReaperExtState<'a> {
    pub(crate) fn new(mt: &'a ReaperMainThread) -> Self {
        Self { _mt: mt }
    }
}

impl<'a> ExtStateTrait for ReaperExtState<'a> {
    fn get(&self, section: &str, key: &str) -> Option<String> {
        let section_c = CString::new(section).ok()?;
        let key_c = CString::new(key).ok()?;
        let low = Reaper::get().medium_reaper().low();
        sw::get_ext_state(low, &section_c, &key_c)
    }

    fn set(&self, section: &str, key: &str, value: &str, persist: bool) -> DawResult<()> {
        let section_c = CString::new(section)
            .map_err(|e| DawError::operation_failed(format!("invalid section: {e}")))?;
        let key_c = CString::new(key)
            .map_err(|e| DawError::operation_failed(format!("invalid key: {e}")))?;
        let value_c = CString::new(value)
            .map_err(|e| DawError::operation_failed(format!("invalid value: {e}")))?;
        let low = Reaper::get().medium_reaper().low();
        sw::set_ext_state(low, &section_c, &key_c, &value_c, persist);
        Ok(())
    }

    fn delete(&self, section: &str, key: &str, persist: bool) -> DawResult<()> {
        let section_c = CString::new(section)
            .map_err(|e| DawError::operation_failed(format!("invalid section: {e}")))?;
        let key_c = CString::new(key)
            .map_err(|e| DawError::operation_failed(format!("invalid key: {e}")))?;
        let low = Reaper::get().medium_reaper().low();
        sw::delete_ext_state(low, &section_c, &key_c, persist);
        Ok(())
    }

    fn has(&self, section: &str, key: &str) -> bool {
        let Ok(section_c) = CString::new(section) else {
            return false;
        };
        let Ok(key_c) = CString::new(key) else {
            return false;
        };
        let low = Reaper::get().medium_reaper().low();
        sw::has_ext_state(low, &section_c, &key_c)
    }
}
