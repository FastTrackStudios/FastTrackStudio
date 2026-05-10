use crate::DawResult;

pub trait ExtState {
    fn get(&self, section: &str, key: &str) -> Option<String>;
    fn set(&self, section: &str, key: &str, value: &str, persist: bool) -> DawResult<()>;
    fn delete(&self, section: &str, key: &str, persist: bool) -> DawResult<()>;
    fn has(&self, section: &str, key: &str) -> bool;
}
