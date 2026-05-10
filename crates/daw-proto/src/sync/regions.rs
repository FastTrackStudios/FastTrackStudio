use crate::{DawResult, Region};

pub trait Regions {
    fn all(&self) -> Vec<Region>;
    fn get(&self, id: u32) -> Option<Region>;
    fn count(&self) -> u32;

    fn add(&self, start: f64, end: f64, name: &str) -> DawResult<u32>;
    fn remove(&self, id: u32) -> DawResult<()>;
    fn set_bounds(&self, id: u32, start: f64, end: f64) -> DawResult<()>;
    fn rename(&self, id: u32, name: &str) -> DawResult<()>;
    fn set_color(&self, id: u32, color: u32) -> DawResult<()>;
}
