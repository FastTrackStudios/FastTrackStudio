//! `cn()` — Tailwind class merging utility.
//!
//! Rust equivalent of shadcn's `cn()` = `clsx()` + `twMerge()`.
//! Merges multiple class strings, with later values overriding earlier
//! ones when they conflict (e.g. `cn("py-6", "py-2")` → `"py-2"`).
//!
//! ```rust,ignore
//! use fts_ui::cn::cn;
//!
//! let class = cn(&["ring-1 ring-foreground/10 bg-card py-6", "py-2 px-0"]);
//! // → "ring-1 ring-foreground/10 bg-card py-2 px-0"
//! ```

/// Merge multiple Tailwind class strings. Later classes override earlier
/// ones when they target the same CSS property.
///
/// Supports: padding, margin, gap, rounded, text size, font weight,
/// width, height, size, border, ring, bg, opacity, flex, grid, and more.
pub fn cn(inputs: &[&str]) -> String {
    let mut classes: Vec<&str> = Vec::new();

    for input in inputs {
        for class in input.split_whitespace() {
            // Find if this class conflicts with an existing one
            let slot = class_slot(class);
            if let Some(slot) = slot {
                // Remove any existing class in the same slot
                classes.retain(|existing| class_slot(existing) != Some(slot));
            }
            classes.push(class);
        }
    }

    classes.join(" ")
}

/// Convenience: merge a base class string with an override string.
pub fn cn2(base: &str, overrides: &str) -> String {
    cn(&[base, overrides])
}

/// Identify the "slot" a Tailwind class belongs to, so conflicting
/// classes can be deduplicated. Returns None for classes we don't
/// recognize (they always pass through).
fn class_slot(class: &str) -> Option<&'static str> {
    // Strip responsive/state prefixes: "hover:bg-red" → "bg-red", "md:px-4" → "px-4"
    let base = strip_prefixes(class);

    // Match known property prefixes
    // Spacing
    if starts("p-", base) || base == "p" { return Some("p"); }
    if starts("px-", base) { return Some("px"); }
    if starts("py-", base) { return Some("py"); }
    if starts("pt-", base) { return Some("pt"); }
    if starts("pr-", base) { return Some("pr"); }
    if starts("pb-", base) { return Some("pb"); }
    if starts("pl-", base) { return Some("pl"); }
    if starts("m-", base) || base == "m" { return Some("m"); }
    if starts("mx-", base) { return Some("mx"); }
    if starts("my-", base) { return Some("my"); }
    if starts("mt-", base) { return Some("mt"); }
    if starts("mr-", base) { return Some("mr"); }
    if starts("mb-", base) { return Some("mb"); }
    if starts("ml-", base) { return Some("ml"); }
    if starts("gap-", base) { return Some("gap"); }
    if starts("gap-x-", base) { return Some("gap-x"); }
    if starts("gap-y-", base) { return Some("gap-y"); }

    // Sizing
    if starts("w-", base) { return Some("w"); }
    if starts("h-", base) { return Some("h"); }
    if starts("size-", base) { return Some("size"); }
    if starts("min-w-", base) { return Some("min-w"); }
    if starts("min-h-", base) { return Some("min-h"); }
    if starts("max-w-", base) { return Some("max-w"); }
    if starts("max-h-", base) { return Some("max-h"); }

    // Typography
    if starts("text-xs", base) || starts("text-sm", base) || starts("text-base", base)
        || starts("text-lg", base) || starts("text-xl", base) || starts("text-2xl", base)
        || starts("text-3xl", base) || starts("text-4xl", base) || starts("text-5xl", base)
        || starts("text-[", base)
    {
        return Some("text-size");
    }
    if starts("font-", base) { return Some("font"); }
    if starts("leading-", base) { return Some("leading"); }
    if starts("tracking-", base) { return Some("tracking"); }

    // Colors (bg, text-color, border-color)
    if starts("bg-", base) { return Some("bg"); }
    if starts("text-", base) && !matches_text_size(base) { return Some("text-color"); }
    if starts("border-", base) && !starts("border-t", base) && !starts("border-b", base)
        && !starts("border-l", base) && !starts("border-r", base)
    {
        return Some("border-color");
    }

    // Borders
    if starts("rounded-", base) || base == "rounded" { return Some("rounded"); }
    if starts("border-t", base) { return Some("border-t"); }
    if starts("border-b", base) { return Some("border-b"); }
    if starts("border-l", base) { return Some("border-l"); }
    if starts("border-r", base) { return Some("border-r"); }
    if base == "border" || starts("border-[", base) { return Some("border"); }

    // Ring
    if starts("ring-", base) && !starts("ring-offset", base) { return Some("ring"); }
    if starts("ring-offset-", base) { return Some("ring-offset"); }

    // Opacity
    if starts("opacity-", base) { return Some("opacity"); }

    // Shadow
    if starts("shadow-", base) || base == "shadow" { return Some("shadow"); }

    // Layout
    if base == "flex" || base == "grid" || base == "block" || base == "inline"
        || base == "inline-flex" || base == "inline-block" || base == "hidden"
    {
        return Some("display");
    }
    if starts("flex-", base) && !starts("flex-col", base) && !starts("flex-row", base)
        && !starts("flex-wrap", base) && !starts("flex-shrink", base) && !starts("flex-grow", base)
    {
        return Some("flex");
    }
    if starts("justify-", base) { return Some("justify"); }
    if starts("items-", base) { return Some("items"); }
    if starts("self-", base) { return Some("self"); }
    if base == "overflow-auto" || base == "overflow-hidden" || base == "overflow-scroll"
        || base == "overflow-visible"
    {
        return Some("overflow");
    }
    if starts("overflow-x-", base) { return Some("overflow-x"); }
    if starts("overflow-y-", base) { return Some("overflow-y"); }

    // Z-index
    if starts("z-", base) { return Some("z"); }

    // Cursor
    if starts("cursor-", base) { return Some("cursor"); }

    None
}

fn starts(prefix: &str, s: &str) -> bool {
    s.starts_with(prefix)
}

fn strip_prefixes(class: &str) -> &str {
    // Strip variant prefixes like "hover:", "focus:", "md:", "dark:", etc.
    // Find the last colon — everything after it is the utility
    match class.rfind(':') {
        Some(pos) => &class[pos + 1..],
        None => class,
    }
}

fn matches_text_size(base: &str) -> bool {
    base.starts_with("text-xs") || base.starts_with("text-sm") || base.starts_with("text-base")
        || base.starts_with("text-lg") || base.starts_with("text-xl")
        || base.starts_with("text-2xl") || base.starts_with("text-3xl")
        || base.starts_with("text-4xl") || base.starts_with("text-5xl")
        || base.starts_with("text-[")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_merge() {
        assert_eq!(cn(&["py-6", "py-2"]), "py-2");
    }

    #[test]
    fn preserves_non_conflicting() {
        assert_eq!(cn(&["bg-card ring-1", "py-2"]), "bg-card ring-1 py-2");
    }

    #[test]
    fn override_bg() {
        assert_eq!(cn(&["bg-card", "bg-red-500"]), "bg-red-500");
    }

    #[test]
    fn override_padding() {
        assert_eq!(cn(&["px-6 py-6", "px-0 py-2"]), "px-0 py-2");
    }

    #[test]
    fn complex_merge() {
        let result = cn(&[
            "ring-foreground/10 bg-card text-card-foreground gap-6 overflow-hidden rounded-2xl py-6 text-sm ring-1",
            "py-2 px-0",
        ]);
        assert!(result.contains("py-2"));
        assert!(!result.contains("py-6"));
        assert!(result.contains("px-0"));
        assert!(result.contains("bg-card"));
        assert!(result.contains("ring-1"));
    }

    #[test]
    fn empty_inputs() {
        assert_eq!(cn(&[""]), "");
        assert_eq!(cn(&["", "px-4"]), "px-4");
    }

    #[test]
    fn preserves_variants() {
        assert_eq!(cn(&["hover:bg-muted", "hover:bg-accent"]), "hover:bg-accent");
    }
}
