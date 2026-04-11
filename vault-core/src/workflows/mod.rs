//! Workflow schemas — domain-specific project structures.
//!
//! Workflows define the shape of a project for a specific domain.
//! They extend the generic `Project` + `Task` model with structured
//! data (setlists, stage plots, input lists, etc.) while remaining
//! serializable as YAML frontmatter in `.md` files.
//!
//! Each workflow is a folder structure:
//!
//! ```text
//! Projects/Campus Jax - April 2026/
//! ├── event.md                    ← Event metadata
//! ├── project.md                  ← Standard project metadata
//! ├── tasks/                      ← Standard tasks
//! ├── advance/
//! │   ├── venue.md                ← Venue/advance info
//! │   ├── contacts.md             ← Contact sheet
//! │   └── budget.md               ← Budget/financials
//! ├── schedule/
//! │   ├── run-of-show.md          ← Minute-by-minute timeline
//! │   └── call-times.md           ← Per-person arrival times
//! ├── performances/
//! │   ├── Act 1 - Jazz Trio/
//! │   │   ├── performance.md      ← Performance metadata
//! │   │   ├── setlist.md          ← Song list with keys, tempos
//! │   │   ├── stage-plot.md       ← Stage layout description
//! │   │   ├── input-list.md       ← Channel-by-channel inputs
//! │   │   └── personnel/
//! │   │       ├── drums.md        ← Role assignment
//! │   │       └── vocals.md
//! │   └── Act 2 - Rock Band/
//! │       ├── performance.md
//! │       ├── setlist.md
//! │       ├── stage-plot.md
//! │       └── input-list.md
//! ├── changeovers/
//! │   └── act1-to-act2.md         ← Changeover plan
//! ├── audio/                      ← Recordings, multitracks
//! ├── video/                      ← Camera footage, edits
//! └── deliverables/
//!     └── deliverables.md         ← What's due, to whom, by when
//! ```

pub mod event;
pub mod output;
pub mod external;

pub use event::*;
pub use output::*;
pub use external::*;
