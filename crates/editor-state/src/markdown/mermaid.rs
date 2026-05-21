//! Mermaid diagram rendering for ```` ```mermaid ``` ```` fences.
//! Mirror of the `typst` submodule — same per-pass compile
//! budget + LRU cache shape, just talking to `editor-mermaid`
//! instead of `editor-typst`.
//!
//! Each cache entry is `(source) → svg`. We don't key on a kind
//! enum because the Mermaid renderer reads the diagram type
//! (`flowchart`, `sequenceDiagram`, …) from the source itself.

use std::cell::Cell;

const CACHE_CAP: usize = 64;
/// Conservative: Mermaid layouts are heavier than Typst math
/// fragments (graph layout vs. a couple of glyphs), so we
/// allow one cold compile per `live_preview` pass and rely on
/// the cache for everything else.
const COMPILE_BUDGET_PER_PASS: u8 = 1;

thread_local! {
    static COMPILE_BUDGET: Cell<u8> = const { Cell::new(COMPILE_BUDGET_PER_PASS) };
}

/// Re-arm the per-pass budget. Call at the top of every
/// `live_preview` pass.
pub(crate) fn reset_compile_budget() {
    COMPILE_BUDGET.with(|c| c.set(COMPILE_BUDGET_PER_PASS));
}

/// Render a Mermaid source string to SVG. Returns `None` on
/// cache miss when the budget is exhausted (caller falls back
/// to source) or when the renderer rejects the source.
pub(crate) fn render_mermaid(body: &str) -> Option<String> {
    if let Some(cached) = with_mermaid_cache(|c| c.get(body)) {
        return Some(cached);
    }
    let budget = COMPILE_BUDGET.with(|c| c.get());
    if budget == 0 {
        return None;
    }
    COMPILE_BUDGET.with(|c| c.set(budget - 1));

    match editor_mermaid::render_svg(body) {
        Ok(svg) => {
            with_mermaid_cache(|c| c.put(body.to_string(), svg.clone()));
            Some(svg)
        }
        Err(e) => {
            tracing::debug!(?e, body_len = body.len(), "mermaid render failed");
            None
        }
    }
}

struct MermaidCache {
    entries: Vec<(String, String)>,
    cap: usize,
}

impl MermaidCache {
    fn new(cap: usize) -> Self {
        Self { entries: Vec::with_capacity(cap), cap }
    }
    fn get(&mut self, body: &str) -> Option<String> {
        let i = self.entries.iter().position(|(b, _)| b == body)?;
        let hit = self.entries.remove(i);
        let svg = hit.1.clone();
        self.entries.push(hit);
        Some(svg)
    }
    fn put(&mut self, body: String, svg: String) {
        if self.entries.len() >= self.cap {
            self.entries.remove(0);
        }
        self.entries.push((body, svg));
    }
}

fn with_mermaid_cache<R>(f: impl FnOnce(&mut MermaidCache) -> R) -> R {
    thread_local! {
        static CACHE: std::cell::RefCell<MermaidCache> =
            std::cell::RefCell::new(MermaidCache::new(CACHE_CAP));
    }
    CACHE.with(|c| f(&mut c.borrow_mut()))
}
