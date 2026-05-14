//! Window geometry — types + service traits.

mod service;
mod types;

pub use service::{
    WindowGeometry, WindowGeometryService, WindowGeometryServiceClient,
    WindowGeometryServiceDispatcher, window_geometry_service_service_descriptor,
};
pub use types::{WindowGeometryResult, WindowTarget};
