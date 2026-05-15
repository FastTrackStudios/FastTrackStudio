//! Batch executor — executes a batch program of instructions sequentially,
//! resolving cross-step dependencies and optionally grouping mutations in
//! a single REAPER undo block.
//!
//! Architect::rpc port: `BatchExecution::execute` is sync. The dispatcher
//! places us on REAPER's main thread; the entire batch runs in one trip.
//! Ops not yet sync-supported return an error per step.

mod dispatch_sync;
mod resolve;

use std::sync::Arc;

use daw_proto::batch::service::BatchExecution;
use daw_proto::batch::*;

/// Inner state for the batch executor.
struct BatchExecutorInner {
    audio_accessor_svc: crate::ReaperAudioAccessor,
}

/// Batch executor that holds stateful service dependencies behind an Arc for Clone.
#[derive(Clone)]
pub struct BatchExecutor {
    inner: Arc<BatchExecutorInner>,
}

impl Default for BatchExecutor {
    fn default() -> Self {
        Self::new()
    }
}

impl BatchExecutor {
    pub fn new() -> Self {
        Self {
            inner: Arc::new(BatchExecutorInner {
                audio_accessor_svc: crate::ReaperAudioAccessor::new(),
            }),
        }
    }

    /// Execute the batch synchronously on REAPER's main thread.
    pub fn execute_sync(&self, request: BatchRequest) -> BatchResponse {
        use crate::project::UNDO_LABEL;
        use crate::project_context::{project_guid, resolve_project_context};
        use crate::track::{clear_project_cache, set_project_cache};
        use daw_proto::ProjectContext;
        use reaper_high::Reaper;
        use reaper_medium::UndoScope;

        let services = dispatch_sync::SyncServices {
            audio_accessor_svc: &self.inner.audio_accessor_svc,
        };

        // Cache the current project GUID so resolve_project() skips FFI per op
        let current_project = Reaper::get().current_project();
        let current_guid = project_guid(&current_project);
        set_project_cache(current_guid, current_project);

        let n = request.instructions.len();
        let mut outputs: Vec<Option<StepOutput>> = vec![None; n];
        let mut results: Vec<StepResult> = Vec::with_capacity(n);
        let mut failed: Vec<bool> = vec![false; n];

        // Begin undo block if requested
        if let Some(ref label) = request.options.undo_label {
            let rctx = resolve_project_context(&ProjectContext::Current);
            Reaper::get().medium_reaper().undo_begin_block_2(rctx);
            UNDO_LABEL.with(|cell| cell.replace(Some(label.clone())));
        }

        for instruction in &request.instructions {
            let step = instruction.step as usize;

            // Check dependencies — skip if any dependency failed
            let deps = instruction.op.step_dependencies();
            let failed_dep = deps.iter().find(|&&d| {
                let d = d as usize;
                d < failed.len() && failed[d]
            });

            if let Some(&dep) = failed_dep {
                if step < n {
                    failed[step] = true;
                }
                results.push(StepResult {
                    step: instruction.step,
                    outcome: StepOutcome::Skipped(dep),
                });
                continue;
            }

            let result = dispatch_sync::dispatch_op_sync(&instruction.op, &outputs, &services);

            match result {
                Ok(output) => {
                    if step < n {
                        outputs[step] = Some(output.clone());
                    }
                    results.push(StepResult {
                        step: instruction.step,
                        outcome: StepOutcome::Ok(output),
                    });
                }
                Err(msg) => {
                    if step < n {
                        failed[step] = true;
                    }
                    results.push(StepResult {
                        step: instruction.step,
                        outcome: StepOutcome::Error(msg),
                    });

                    if request.options.fail_fast {
                        for remaining in request.instructions.iter().skip(results.len()) {
                            results.push(StepResult {
                                step: remaining.step,
                                outcome: StepOutcome::Skipped(instruction.step),
                            });
                        }
                        break;
                    }
                }
            }
        }

        // End undo block if we started one
        if let Some(ref label) = request.options.undo_label {
            let rctx = resolve_project_context(&ProjectContext::Current);
            Reaper::get()
                .medium_reaper()
                .undo_end_block_2(rctx, label.as_str(), UndoScope::All);
        }

        clear_project_cache();
        BatchResponse { results }
    }
}

impl BatchExecution for crate::Reaper {
    fn execute(&self, request: BatchRequest) -> BatchResponse {
        let n = request.instructions.len();
        tracing::info!("BatchExecution::execute — {} instructions (sync)", n);
        BatchExecutor::new().execute_sync(request)
    }
}
