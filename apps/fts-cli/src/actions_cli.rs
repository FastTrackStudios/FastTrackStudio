//! Generated CLI surface over `#[architect::actions]`-declared traits.
//!
//! `session` declares its actions (setlist, keyflow inserts, mode
//! switches, take ranking, …) as `#[architect::actions]` traits rather
//! than the old `actions_proto::define_actions!` string table. Each
//! trait gets a `<Trait>Actions::all() -> &'static [architect::action::ActionMeta]`
//! accessor. This module turns that metadata into a discoverable
//! `fts session <action>` surface, without requiring the caller to
//! already know the exact `FTS_SESSION_BUILD_SETLIST`-style REAPER
//! command id.
//!
//! Execution still goes through the existing
//! `session_cli::SessionCommand::Action { command_id }` RPC path
//! (`ProjectsClient::run_command` under the hood) — this module only
//! generates the *discovery* and *name → id* layer on top.
//!
//! Kept additive and low-risk: `Cli::try_parse()` (clap-derive) is
//! always tried first in `main.rs`; this module is only consulted as a
//! fallback when `session <name>` doesn't match one of `session-cli`'s
//! existing hand-written subcommands (`setlist`, `songs`, `play`, …).
//! The old raw `fts session action <ID>` escape hatch keeps working
//! unchanged.

use architect::action::ActionMeta;

/// One `(category, &'static [ActionMeta])` group, in a fixed display
/// order. Add a new session action module here to make it show up in
/// `fts session --actions` / name-resolution.
///
/// fts-extensions' own two action traits (`FtsTempoGridActions`,
/// `FtsActions`) are NOT included here: `fts-extensions` builds as a
/// `cdylib` only (no `rlib`), so it can't be linked into this CLI
/// binary as a normal Rust dependency. Their ~21 actions stay reachable
/// only via the raw `fts session action <ID>` path until fts-extensions
/// grows an `rlib` target.
fn action_groups() -> Vec<(&'static str, &'static [ActionMeta])> {
    vec![
        ("setlist_actions", session::setlist_actions::SetlistActionsActions::all()),
        ("keyflow_actions", session::keyflow_actions::KeyflowActionsActions::all()),
        ("auto_color_actions", session::auto_color_actions::AutoColorActionsActions::all()),
        (
            "track_manager_actions",
            session::track_manager_actions::TrackManagerActionsActions::all(),
        ),
        ("preroll_actions", session::preroll_actions::PreRollActionsActions::all()),
        ("mode_actions", session::mode_actions::ModeActionsActions::all()),
        ("take_ranking", session::take_ranking::TakeRankingActionsActions::all()),
        ("record_actions", session::record_actions::RecordActionsActions::all()),
        ("group_actions", session::group_actions::GroupActionsActions::all()),
    ]
}

/// `build_setlist` -> `build-setlist` (clap/CLI subcommand convention).
fn kebab(method_name: &str) -> String {
    method_name.replace('_', "-")
}

/// Resolve a user-typed `fts session <name>` name (already kebab-cased,
/// e.g. `load-demo-setlist`) to the full REAPER command id
/// (`FTS_SESSION_LOAD_DEMO_SETLIST`) it corresponds to, by scanning
/// every registered action-group's `ActionMeta::method_name`.
///
/// Returns `None` if nothing matches — the caller falls through to
/// clap's own "unrecognized subcommand" error in that case.
pub fn resolve_action_id(name: &str) -> Option<&'static str> {
    action_groups()
        .into_iter()
        .flat_map(|(_, metas)| metas.iter())
        .find(|m| kebab(m.method_name) == name)
        .map(|m| m.id)
}

/// Render the full generated action menu, grouped by `ActionMeta::category`
/// (falling back to "Actions" for anything with no category set), for
/// `fts session --actions` / `fts actions`.
pub fn render_actions_help() -> String {
    use std::collections::BTreeMap;

    let mut by_category: BTreeMap<&'static str, Vec<&ActionMeta>> = BTreeMap::new();
    for (_, metas) in action_groups() {
        for m in metas {
            let heading = if m.category.is_empty() { "Actions" } else { m.category };
            by_category.entry(heading).or_default().push(m);
        }
    }

    let mut out = String::new();
    out.push_str("Generated session actions (fts session <name>):\n");
    out.push_str(
        "(fts-extensions' own tempo-grid/navigation/etc. actions aren't listed here yet —\n\
         reach them via `fts session action <ID>` until fts-extensions exposes an rlib.)\n\n",
    );
    for (category, mut metas) in by_category {
        out.push_str(&format!("{category}:\n"));
        metas.sort_by_key(|m| m.method_name);
        for m in metas {
            out.push_str(&format!(
                "  {:<28} {}\n",
                kebab(m.method_name),
                m.description
            ));
        }
        out.push('\n');
    }
    out
}
