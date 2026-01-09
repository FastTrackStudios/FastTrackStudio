//! Delay line primitives.
//!
//! This module provides ring buffer types for implementing delay effects.
//! These are thin wrappers around `dasp_ring_buffer`, allowing implementation
//! swapping in the future.
//!
//! ## Buffer Types
//!
//! - [`FixedBuffer`] - Fixed-length ring buffer (push returns popped element)
//! - [`BoundedBuffer`] - Variable-length ring buffer with max capacity
//!
//! ## Example
//!
//! ```ignore
//! use fts_fx::primitives::delay::FixedBuffer;
//!
//! // Create a 4-sample delay line
//! let mut delay = FixedBuffer::from([0.0f32; 4]);
//!
//! // Push a sample, get the delayed sample back
//! let delayed = delay.push(1.0);
//! assert_eq!(delayed, 0.0); // First 4 samples are the initial zeros
//! ```

use dasp_ring_buffer as rb;

/// Trait for types that can be used as the underlying storage for ring buffers.
pub use rb::Slice;

/// Trait for mutable slice storage.
pub use rb::SliceMut;

/// A fixed-length ring buffer.
///
/// When you push a new element, the oldest element is returned.
/// This is ideal for simple delay lines where the delay length is constant.
///
/// The buffer maintains a constant length - it's always "full".
pub type FixedBuffer<S> = rb::Fixed<S>;

/// A variable-length ring buffer with a maximum capacity.
///
/// Elements can be pushed and popped independently. The buffer can be
/// empty, partially filled, or full.
///
/// Useful for variable-length delays or when you need FIFO queue semantics.
pub type BoundedBuffer<S> = rb::Bounded<S>;

/// Convenience trait for ring buffer operations.
///
/// This trait provides a unified interface for working with ring buffers,
/// abstracting over the specific dasp types.
pub trait RingBuffer<T> {
    /// Push an element to the buffer.
    ///
    /// For fixed buffers, returns the displaced element.
    /// For bounded buffers, returns the displaced element if the buffer was full.
    fn push(&mut self, item: T) -> Option<T>;

    /// Get the current length of the buffer.
    fn len(&self) -> usize;

    /// Check if the buffer is empty.
    fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Get an element by index (0 = oldest/front).
    fn get(&self, index: usize) -> Option<&T>;

    /// Reset the buffer to its initial state.
    fn reset(&mut self)
    where
        T: Default + Copy;
}

impl<S> RingBuffer<S::Element> for FixedBuffer<S>
where
    S: SliceMut,
    S::Element: Copy,
{
    fn push(&mut self, item: S::Element) -> Option<S::Element> {
        Some(FixedBuffer::push(self, item))
    }

    fn len(&self) -> usize {
        FixedBuffer::len(self)
    }

    fn get(&self, index: usize) -> Option<&S::Element> {
        if index < self.len() {
            Some(FixedBuffer::get(self, index))
        } else {
            None
        }
    }

    fn reset(&mut self)
    where
        S::Element: Default + Copy,
    {
        for sample in self.iter_mut() {
            *sample = S::Element::default();
        }
    }
}

impl<S> RingBuffer<S::Element> for BoundedBuffer<S>
where
    S: SliceMut,
    S::Element: Copy,
{
    fn push(&mut self, item: S::Element) -> Option<S::Element> {
        BoundedBuffer::push(self, item)
    }

    fn len(&self) -> usize {
        BoundedBuffer::len(self)
    }

    fn get(&self, index: usize) -> Option<&S::Element> {
        BoundedBuffer::get(self, index)
    }

    fn reset(&mut self)
    where
        S::Element: Default + Copy,
    {
        while self.pop().is_some() {}
    }
}

// Note: Convenience constructors for fixed-size delays are not provided here
// because dasp's Slice trait is only implemented for arrays up to size 128.
// Use FixedBuffer::from([0.0f32; SIZE]) directly with a concrete size.
//
// Example:
// ```
// let delay: FixedBuffer<[f32; 128]> = FixedBuffer::from([0.0f32; 128]);
// ```
