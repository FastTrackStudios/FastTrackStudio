//! Block chaining and routing.

use crate::Block;

/// A chain of audio processing blocks.
///
/// Blocks are processed in order, with the output of each block
/// feeding into the input of the next.
pub struct Chain<B: Block> {
    blocks: Vec<B>,
    sample_rate: f32,
}

impl<B: Block> Chain<B> {
    /// Create a new empty chain.
    #[must_use]
    pub fn new(sample_rate: f32) -> Self {
        Self {
            blocks: Vec::new(),
            sample_rate,
        }
    }

    /// Add a block to the end of the chain.
    pub fn push(&mut self, mut block: B) {
        block.set_sample_rate(self.sample_rate);
        self.blocks.push(block);
    }

    /// Remove and return the last block, if any.
    pub fn pop(&mut self) -> Option<B> {
        self.blocks.pop()
    }

    /// Get the number of blocks in the chain.
    #[must_use]
    pub fn len(&self) -> usize {
        self.blocks.len()
    }

    /// Check if the chain is empty.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.blocks.is_empty()
    }

    /// Get total latency of the chain in samples.
    #[must_use]
    pub fn total_latency(&self) -> usize {
        self.blocks.iter().map(Block::latency).sum()
    }

    /// Reset all blocks in the chain.
    pub fn reset(&mut self) {
        for block in &mut self.blocks {
            block.reset();
        }
    }

    /// Update sample rate for all blocks.
    pub fn set_sample_rate(&mut self, sample_rate: f32) {
        self.sample_rate = sample_rate;
        for block in &mut self.blocks {
            block.set_sample_rate(sample_rate);
        }
    }

    /// Get an iterator over the blocks.
    pub fn iter(&self) -> impl Iterator<Item = &B> {
        self.blocks.iter()
    }

    /// Get a mutable iterator over the blocks.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut B> {
        self.blocks.iter_mut()
    }
}

impl<B: Block> Default for Chain<B> {
    fn default() -> Self {
        Self::new(44100.0)
    }
}
