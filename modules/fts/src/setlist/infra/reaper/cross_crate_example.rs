//! Example: How to implement traits in separate crates
//!
//! This file demonstrates the structure you'd use if you split implementations
//! across multiple crates. It's for reference only.

// ============================================================================
// CRATE STRUCTURE EXAMPLE
// ============================================================================
//
// modules/
//   fts/                          # Defines traits
//     src/setlist/infra/traits.rs # SetlistBuilder, SeekAdapter, CommandAdapter
//   
//   fts-reaper-setlist/           # New crate: implements SetlistBuilder
//     Cargo.toml:
//       [dependencies]
//       fts = { path = "../fts", features = ["reaper"] }
//       reaper-high = { workspace = true }
//     
//     src/lib.rs:
//       use fts::setlist::infra::traits::SetlistBuilder;
//       use reaper_high::Project as ReaperProject;
//       
//       impl SetlistBuilder for ReaperProject {
//           fn build_setlist_from_open_projects(&self, ...) { ... }
//           fn build_song_from_current_project(&self) { ... }
//       }
//
//   fts-reaper-seek/              # New crate: implements SeekAdapter
//     Cargo.toml:
//       [dependencies]
//       fts = { path = "../fts", features = ["reaper"] }
//       reaper-high = { workspace = true }
//     
//     src/lib.rs:
//       use fts::setlist::infra::traits::SeekAdapter;
//       use reaper_high::Project as ReaperProject;
//       
//       impl SeekAdapter for ReaperProject {
//           fn seek_to_section(&self, ...) { ... }
//           // ... other methods
//       }
//
//   fts-reaper-commands/           # New crate: implements CommandAdapter
//     Cargo.toml:
//       [dependencies]
//       fts = { path = "../fts", features = ["reaper"] }
//       reaper-high = { workspace = true }
//     
//     src/lib.rs:
//       use fts::setlist::infra::traits::CommandAdapter;
//       use reaper_high::Project as ReaperProject;
//       
//       impl CommandAdapter for ReaperProject {
//           fn execute_transport_command(&self, ...) { ... }
//           // ... other methods
//       }
//
// apps/
//   reaper_extension/
//     Cargo.toml:
//       [dependencies]
//       fts = { path = "../../modules/fts", features = ["reaper"] }
//       fts-reaper-setlist = { path = "../../modules/fts-reaper-setlist" }
//       fts-reaper-seek = { path = "../../modules/fts-reaper-seek" }
//       fts-reaper-commands = { path = "../../modules/fts-reaper-commands" }
//     
//     src/lib.rs:
//       // Just importing the crates brings in the implementations!
//       use fts_reaper_setlist; // Brings in SetlistBuilder impl
//       use fts_reaper_seek;     // Brings in SeekAdapter impl
//       use fts_reaper_commands; // Brings in CommandAdapter impl
//       
//       // Also need to import traits to use methods
//       use fts::setlist::infra::traits::{SetlistBuilder, SeekAdapter, CommandAdapter};
//       
//       fn example() {
//           let project = Reaper::get().current_project();
//           
//           // All trait methods work directly on ReaperProject!
//           let setlist = project.build_setlist_from_open_projects(None)?;
//           project.seek_to_song(0)?;
//           project.execute_transport_command(TransportCommand::Play)?;
//       }

// ============================================================================
// KEY POINTS
// ============================================================================
//
// 1. Each implementation crate MUST depend on the trait-defining crate (fts)
// 2. The using crate (reaper_extension) MUST depend on both:
//    - The trait crate (fts)
//    - The implementation crate(s)
// 3. You MUST import the trait to use its methods (trait must be in scope)
// 4. Just importing the implementation crate makes the impl available
// 5. Different crates can implement DIFFERENT traits for the SAME type ✅
// 6. Multiple crates CANNOT implement the SAME trait for the SAME type ❌
//
// ============================================================================
// ALTERNATIVE: Implement for Different Types
// ============================================================================
//
// You could also have:
//
//   fts-reaper-project/    # Implements SetlistBuilder for ReaperProject
//   fts-reaper-instance/   # Implements SetlistBuilder for Reaper (the instance)
//
// This is totally fine - no conflicts because they're different types!
