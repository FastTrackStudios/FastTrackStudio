//! Batch operations — composition over the `#[architect::rpc(ops)]`
//! generated per-service enums.
//!
//! Each covered service trait carries `ops(...)` substitution pairs,
//! so the macro emits its `<Trait>Op` / `<Trait>OpOutput` enums with
//! deferred argument types (`ProjectArg`, `TrackArg`, `FxChainArg`)
//! in place of the literal parameter types. This module only:
//!
//! - wraps those per-service enums into the top-level [`BatchOp`] /
//!   [`BatchOpOutput`] wire enums,
//! - implements the [`architect::ops`] resolver over prior step
//!   outputs ([`StepOutputs`]), and
//! - drives a whole program ([`run`]).
//!
//! Adding a method to a covered trait needs **no edit here** — the
//! variant, its application, and its output all regenerate.

use architect::ops::{OpResolver, ResolveArg};
use facet::Facet;

use super::args::{FxChainArg, ProjectArg, TrackArg};
use super::program::{BatchRequest, BatchResponse, StepOutcome, StepResult};
use crate::fx::{EffectsOp, EffectsOpOutput, FxChainContext};
use crate::marker::{MarkersOp, MarkersOpOutput};
use crate::project::{ProjectContext, ProjectsOp, ProjectsOpOutput};
use crate::routing::{RoutingOp, RoutingOpOutput};
use crate::track::{TrackRef, TracksOp, TracksOpOutput};
use crate::transport::{TransportOp, TransportOpOutput};

/// Top-level batch operation — one variant per covered service,
/// wrapping that service's macro-generated op enum.
#[repr(u8)]
#[derive(Clone, Debug, Facet)]
pub enum BatchOp {
    Transport(TransportOp),
    Project(ProjectsOp),
    Track(TracksOp),
    Marker(MarkersOp),
    Fx(EffectsOp),
    Routing(RoutingOp),
}

/// Output of one applied [`BatchOp`] — mirrors [`BatchOp`]'s shape,
/// wrapping the macro-generated per-service output enums.
// Wire/domain type: variant size asymmetry is inherent.
#[allow(clippy::large_enum_variant)]
#[repr(u8)]
#[derive(Clone, Debug, Facet)]
pub enum BatchOpOutput {
    Transport(TransportOpOutput),
    Project(ProjectsOpOutput),
    Track(TracksOpOutput),
    Marker(MarkersOpOutput),
    Fx(EffectsOpOutput),
    Routing(RoutingOpOutput),
}

/// Everything a backend must implement to execute batch programs.
/// Blanket-implemented — backends just implement the service traits.
pub trait BatchBackend:
    crate::transport::prelude::Transport
    + crate::project::Projects
    + crate::track::Tracks
    + crate::marker::Markers
    + crate::fx::Effects
    + crate::routing::Routing
{
}

impl<B> BatchBackend for B where
    B: crate::transport::prelude::Transport
        + crate::project::Projects
        + crate::track::Tracks
        + crate::marker::Markers
        + crate::fx::Effects
        + crate::routing::Routing
{
}

impl BatchOp {
    /// Apply this op against a backend, resolving `FromStep` arguments
    /// from `outputs`.
    pub fn apply<B: BatchBackend + ?Sized>(
        self,
        backend: &B,
        outputs: &StepOutputs,
    ) -> Result<BatchOpOutput, BatchArgError> {
        Ok(match self {
            BatchOp::Transport(op) => BatchOpOutput::Transport(op.apply(backend, outputs)?),
            BatchOp::Project(op) => BatchOpOutput::Project(op.apply(backend, outputs)?),
            BatchOp::Track(op) => BatchOpOutput::Track(op.apply(backend, outputs)?),
            BatchOp::Marker(op) => BatchOpOutput::Marker(op.apply(backend, outputs)?),
            BatchOp::Fx(op) => BatchOpOutput::Fx(op.apply(backend, outputs)?),
            BatchOp::Routing(op) => BatchOpOutput::Routing(op.apply(backend, outputs)?),
        })
    }
}

/// A deferred (`FromStep`) argument could not be resolved. Never
/// crosses the wire itself — [`run`] folds it into
/// [`StepOutcome::Error`] / [`StepOutcome::Skipped`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BatchArgError {
    /// The referenced step has no successful output (never ran,
    /// errored, or was skipped).
    MissingStep(u32),
    /// The referenced step produced an output that cannot feed this
    /// argument kind (e.g. a tempo query feeding a track argument).
    WrongKind { step: u32, wanted: &'static str },
    /// A list output was referenced with an out-of-range index.
    IndexOutOfRange { step: u32, index: u32 },
}

impl core::fmt::Display for BatchArgError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::MissingStep(step) => {
                write!(f, "step {step} has no output to resolve from")
            }
            Self::WrongKind { step, wanted } => {
                write!(f, "step {step} output cannot resolve a {wanted}")
            }
            Self::IndexOutOfRange { step, index } => {
                write!(f, "step {step} list output has no element {index}")
            }
        }
    }
}

impl std::error::Error for BatchArgError {}

/// Successful outputs of already-executed steps, indexed by step id —
/// the resolver behind `FromStep` arguments.
#[derive(Default)]
pub struct StepOutputs {
    slots: Vec<Option<BatchOpOutput>>,
}

impl StepOutputs {
    pub fn with_capacity(steps: usize) -> Self {
        Self {
            slots: vec![None; steps],
        }
    }

    pub fn record(&mut self, step: u32, output: BatchOpOutput) {
        let idx = step as usize;
        if idx >= self.slots.len() {
            self.slots.resize(idx + 1, None);
        }
        self.slots[idx] = Some(output);
    }

    pub fn get(&self, step: u32) -> Option<&BatchOpOutput> {
        self.slots.get(step as usize).and_then(|s| s.as_ref())
    }

    fn require(&self, step: u32) -> Result<&BatchOpOutput, BatchArgError> {
        self.get(step).ok_or(BatchArgError::MissingStep(step))
    }

    /// A project produced by step `step`, as a context for later calls.
    fn project_from(&self, step: u32) -> Result<ProjectContext, BatchArgError> {
        let wrong = || BatchArgError::WrongKind {
            step,
            wanted: "project",
        };
        let info = match self.require(step)? {
            BatchOpOutput::Project(out) => match out {
                ProjectsOpOutput::Info(r) => r.as_ref().ok(),
                ProjectsOpOutput::Current(o)
                | ProjectsOpOutput::Get(o)
                | ProjectsOpOutput::GetBySlot(o)
                | ProjectsOpOutput::Open(o)
                | ProjectsOpOutput::Create(o) => o.as_ref(),
                _ => None,
            },
            _ => None,
        };
        Ok(ProjectContext::Project(info.ok_or_else(wrong)?.guid.clone()))
    }

    /// A track ref produced by step `step` (`index` picks from list
    /// outputs).
    fn track_from(&self, step: u32, index: Option<u32>) -> Result<TrackRef, BatchArgError> {
        let wrong = || BatchArgError::WrongKind {
            step,
            wanted: "track",
        };
        let out = match self.require(step)? {
            BatchOpOutput::Track(out) => out,
            _ => return Err(wrong()),
        };
        let guid = match (out, index) {
            (TracksOpOutput::Get(o) | TracksOpOutput::Master(o), None) => {
                o.as_ref().ok_or_else(wrong)?.guid.clone()
            }
            // `add` returns the new track's GUID directly.
            (TracksOpOutput::Add(r), None) => r.as_ref().map_err(|_| wrong())?.clone(),
            (TracksOpOutput::All(list) | TracksOpOutput::Selected(list), idx) => {
                let index = idx.unwrap_or(0);
                list.get(index as usize)
                    .ok_or(BatchArgError::IndexOutOfRange { step, index })?
                    .guid
                    .clone()
            }
            _ => return Err(wrong()),
        };
        Ok(TrackRef::Guid(guid))
    }
}

impl OpResolver for StepOutputs {
    type Error = BatchArgError;
}

impl ResolveArg<ProjectArg, ProjectContext> for StepOutputs {
    fn resolve_arg(&self, arg: ProjectArg) -> Result<ProjectContext, BatchArgError> {
        match arg {
            ProjectArg::Literal(ctx) => Ok(ctx),
            ProjectArg::FromStep(step) => self.project_from(step),
        }
    }
}

impl ResolveArg<TrackArg, TrackRef> for StepOutputs {
    fn resolve_arg(&self, arg: TrackArg) -> Result<TrackRef, BatchArgError> {
        match arg {
            TrackArg::Literal(t) => Ok(t),
            TrackArg::FromStep(step) => self.track_from(step, None),
            TrackArg::FromStepIndex(step, index) => self.track_from(step, Some(index)),
        }
    }
}

impl ResolveArg<FxChainArg, FxChainContext> for StepOutputs {
    fn resolve_arg(&self, arg: FxChainArg) -> Result<FxChainContext, BatchArgError> {
        match arg {
            FxChainArg::Literal(chain) => Ok(chain),
            FxChainArg::TrackFromStep(step) => {
                let TrackRef::Guid(guid) = self.track_from(step, None)? else {
                    return Err(BatchArgError::WrongKind {
                        step,
                        wanted: "track guid",
                    });
                };
                Ok(FxChainContext::Track(guid))
            }
        }
    }
}

/// Execute a whole batch program against a backend. This is the
/// canonical `BatchExecution::execute` body — backends delegate here.
///
/// Semantics:
/// - Steps run in instruction order; successful outputs become
///   resolvable by later `FromStep` arguments.
/// - A failed argument resolution reports `Skipped(dep)` when the
///   referenced step failed/never ran, `Error` otherwise.
/// - With `fail_fast`, every step after the first error reports
///   `Skipped(failed_step)`.
/// - With `undo_label`, the program is wrapped in one undo block on
///   the current project.
pub fn run<B: BatchBackend + ?Sized>(backend: &B, request: BatchRequest) -> BatchResponse {
    let undo_label = request.options.undo_label.clone();
    if let Some(label) = &undo_label {
        crate::project::Projects::begin_undo_block(backend, ProjectContext::Current, label);
    }

    let mut outputs = StepOutputs::with_capacity(request.instructions.len());
    let mut results = Vec::with_capacity(request.instructions.len());
    let mut failed_step: Option<u32> = None;

    for instruction in request.instructions {
        let step = instruction.step;
        if let Some(failed) = failed_step {
            results.push(StepResult {
                step,
                outcome: StepOutcome::Skipped(failed),
            });
            continue;
        }
        let outcome = match instruction.op.apply(backend, &outputs) {
            Ok(output) => {
                outputs.record(step, output.clone());
                StepOutcome::Ok(output)
            }
            Err(BatchArgError::MissingStep(dep)) => StepOutcome::Skipped(dep),
            Err(err) => StepOutcome::Error(err.to_string()),
        };
        if request.options.fail_fast && !matches!(outcome, StepOutcome::Ok(_)) {
            failed_step = Some(step);
        }
        results.push(StepResult { step, outcome });
    }

    if let Some(label) = &undo_label {
        crate::project::Projects::end_undo_block(backend, ProjectContext::Current, label, None);
    }

    BatchResponse { results }
}
