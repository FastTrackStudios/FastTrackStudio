//! Category colors — one stable, distinct accent per workflow category,
//! used everywhere a category appears: the /input keyboard map, the
//! category sidebar, binding-list section headers, and guide sections.
//!
//! Hues are Tailwind-ish 300–400 stops so they read on the dark theme;
//! fills are derived by suffixing a hex alpha (e.g. `{color}26`).

/// The accent color for a category id (section file stem, kebab-case).
/// Known categories get a hand-assigned hue; unknown ids fall back to a
/// deterministic hash pick so new config files stay stable across builds.
pub(crate) fn category_color(id: &str) -> &'static str {
    match id {
        "transport" => "#4ade80",   // green-400
        "tracks" => "#38bdf8",      // sky-400
        "navigation" => "#a78bfa",  // violet-400
        "editing" => "#f87171",     // red-400
        "zoom" => "#fbbf24",        // amber-400
        "views" => "#2dd4bf",       // teal-400
        "markers" => "#fb923c",     // orange-400
        "midi" => "#e879f9",        // fuchsia-400
        "midi-modes" => "#d946ef",  // fuchsia-500 (kin to midi)
        "mouse" => "#94a3b8",       // slate-400
        "scrolling" => "#22d3ee",   // cyan-400
        "visibility" => "#facc15",  // yellow-400
        "lanes-takes" => "#f472b6", // pink-400
        "automation" => "#818cf8",  // indigo-400
        "fx" => "#c084fc",          // purple-400
        "grid" => "#a3e635",        // lime-400
        "options" => "#9ca3af",     // gray-400
        "utility" => "#d4d4d8",     // zinc-300
        "modes" => "#fb7185",       // rose-400
        other => fallback_color(other),
    }
}

/// Deterministic FNV-1a pick from a spare palette for unknown ids.
fn fallback_color(id: &str) -> &'static str {
    const FALLBACK: &[&str] = &[
        "#5eead4", // teal-300
        "#fda4af", // rose-300
        "#93c5fd", // blue-300
        "#fcd34d", // amber-300
        "#c4b5fd", // violet-300
        "#86efac", // green-300
        "#f0abfc", // fuchsia-300
        "#7dd3fc", // sky-300
    ];
    let mut h: u32 = 2166136261;
    for b in id.bytes() {
        h ^= b as u32;
        h = h.wrapping_mul(16777619);
    }
    FALLBACK[(h as usize) % FALLBACK.len()]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn known_categories_are_distinct() {
        let ids = [
            "transport", "tracks", "navigation", "editing", "zoom", "views",
            "markers", "midi", "midi-modes", "mouse", "scrolling", "visibility",
            "lanes-takes", "automation", "fx", "grid", "options", "utility",
            "modes",
        ];
        let mut seen = std::collections::HashSet::new();
        for id in ids {
            assert!(seen.insert(category_color(id)), "duplicate color for {id}");
        }
    }

    #[test]
    fn unknown_ids_get_a_stable_fallback() {
        assert_eq!(category_color("wibble"), category_color("wibble"));
        assert!(category_color("wibble").starts_with('#'));
    }
}
