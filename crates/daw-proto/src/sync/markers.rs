use crate::{DawResult, Marker};

pub trait Markers {
    fn all(&self) -> Vec<Marker>;
    fn get(&self, id: u32) -> Option<Marker>;
    fn count(&self) -> u32;

    fn add(&self, position: f64, name: &str) -> DawResult<u32>;
    fn remove(&self, id: u32) -> DawResult<()>;
    fn set_position(&self, id: u32, position: f64) -> DawResult<()>;
    fn rename(&self, id: u32, name: &str) -> DawResult<()>;
    fn set_color(&self, id: u32, color: u32) -> DawResult<()>;
}
