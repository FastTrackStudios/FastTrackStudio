//! `BodyMeasurement` — point-in-time observation of body metrics.
//!
//! Each row is an immutable snapshot at `measured_at`: weight, body-fat %,
//! circumferences, vitals. Photos / progress shots hang off the row via
//! the polymorphic [`crate::attachment::Attachment`] table
//! (`owner_type = "body_measurement"`).

pub mod model;

pub use model::*;
