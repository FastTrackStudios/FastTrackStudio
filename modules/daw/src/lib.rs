//! DAW Module
//!
//! This module provides domain abstractions for Digital Audio Workstation functionality.

pub mod transport;
pub mod primitives;

// Re-export commonly used types for easy access
pub use transport::{
    TransportActions, TransportAction, TransportError,
    Transport, Tempo, PlayState, RecordMode,
    MockTransport,
    // Universal network architecture
};
pub use primitives::{
    TimeSignature, MusicalPosition, TimePosition, Position,
    TimeRange, TimeSelection, LoopPoints
};

pub fn add(left: u64, right: u64) -> u64 {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
