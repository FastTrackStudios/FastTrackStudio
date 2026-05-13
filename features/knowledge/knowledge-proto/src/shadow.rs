//! Deterministic UUIDv5 helper for embedding a knowledge page inside
//! another feature's entity. See module docs on `shadow_page_id`.

use uuid::Uuid;

/// Fixed namespace for every shadow-page UUIDv5 derivation. Burned
/// in — changing this byte sequence rebinds every shadow page
/// already in the wild, so don't.
const SHADOW_NS: Uuid = match Uuid::try_parse("d4e1c2a0-1234-5678-9abc-def012345678") {
    Ok(u) => u,
    Err(_) => panic!("shadow namespace literal is not a valid UUID"),
};

/// Deterministic UUIDv5-derived `Page.id` for embedding a knowledge
/// outliner inside another feature's entity. Allows e.g. a Task to
/// "have notes" without adding a `notes_page_id` field to
/// `project-proto`.
///
/// Example: `shadow_page_id("task", task.id)` always returns the
/// same Uuid for the same task, so the OutlinerEmbed UI auto-creates
/// the Page on first edit and finds it on subsequent loads.
pub fn shadow_page_id(entity_kind: &str, entity_id: Uuid) -> Uuid {
    let payload = format!("{entity_kind}:{entity_id}");
    Uuid::new_v5(&SHADOW_NS, payload.as_bytes())
}
