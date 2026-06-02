//! Batch executor compatibility shim.
//!
//! The previous REAPER backend carried a large sync-optimized batch dispatcher
//! that duplicated most service logic. Batch routing is intentionally disabled
//! here until there is a concrete consumer and a cleaner implementation path.

use daw_proto::BatchExecution;
use daw_proto::batch::*;

/// Stateless batch executor. The `AudioAccessors` registry lives at
/// module level on `crate::audio_accessor`; no per-batch state is
/// carried.
#[derive(Clone, Copy, Default)]
pub struct BatchExecutor;

impl BatchExecutor {
    pub fn new() -> Self {
        Self
    }

    /// Return an explicit error for each requested step.
    pub fn execute_sync(&self, request: BatchRequest) -> BatchResponse {
        let results = request
            .instructions
            .iter()
            .map(|instruction| StepResult {
                step: instruction.step,
                outcome: StepOutcome::Error(
                    "BatchExecution is disabled on the REAPER backend; invoke the service methods directly"
                        .to_string(),
                ),
            })
            .collect();
        BatchResponse { results }
    }
}

impl BatchExecution for crate::Reaper {
    fn execute(&self, request: BatchRequest) -> BatchResponse {
        let n = request.instructions.len();
        tracing::warn!(
            "BatchExecution::execute rejected {n} instructions: batch routing is disabled"
        );
        BatchExecutor::new().execute_sync(request)
    }
}
