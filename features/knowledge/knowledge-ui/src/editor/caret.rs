//! Caret tracking + DOM position helpers.
//!
//! Real implementation runs on `wasm32-unknown-unknown`; native builds
//! get no-op stubs so the crate compiles for `cargo check` on the host.

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct CaretPos {
    pub offset: usize,
}

#[cfg(target_arch = "wasm32")]
mod imp {
    use super::CaretPos;

    /// Read current selection focus as a byte offset into the
    /// contenteditable's textContent. Returns None if the selection
    /// focus is not inside `container`.
    pub fn get_caret(container: &web_sys::Element) -> Option<CaretPos> {
        let window = web_sys::window()?;
        let sel = window.get_selection().ok().flatten()?;
        if sel.range_count() == 0 {
            return None;
        }
        let range = sel.get_range_at(0).ok()?;
        let focus_node = sel.focus_node()?;
        // Must be descendant of `container`.
        if !container.contains(Some(&focus_node)) {
            return None;
        }
        // Measure from container start → range end.
        let measure = web_sys::Range::new().ok()?;
        measure.set_start(&container.clone().into(), 0).ok()?;
        measure
            .set_end(&range.end_container().ok()?, range.end_offset().ok()?)
            .ok()?;
        let text = measure.to_string().as_string().unwrap_or_default();
        Some(CaretPos { offset: text.len() })
    }

    /// Restore caret to the given byte offset within `container`'s
    /// textContent. Best-effort; if the container's text shrank below
    /// the requested offset the caret is clamped to end.
    pub fn set_caret(container: &web_sys::Element, pos: CaretPos) {
        let Some(window) = web_sys::window() else {
            return;
        };
        let Some(doc) = window.document() else { return };
        let Some(sel) = window.get_selection().ok().flatten() else {
            return;
        };
        let target_off = pos.offset;
        // Hand-walk descendant text nodes — TreeWalker bindings in
        // web-sys 0.3 vary by feature flag; this avoids depending on
        // `NodeFilter` / `TreeWalker` entirely.
        let container_node: web_sys::Node = container.clone().into();
        let mut acc = 0usize;
        if let Some(found) = walk_text(&container_node, target_off, &mut acc) {
            if let Ok(range) = doc.create_range() {
                let local =
                    (target_off - found.1).min(found.0.text_content().unwrap_or_default().len());
                let _ = range.set_start(&found.0, local as u32);
                let _ = range.set_end(&found.0, local as u32);
                let _ = sel.remove_all_ranges();
                let _ = sel.add_range(&range);
                return;
            }
        }
        // Fell off the end — collapse to container's end.
        if let Ok(range) = doc.create_range() {
            let _ = range.select_node_contents(&container.clone().into());
            range.collapse_with_to_start(false);
            let _ = sel.remove_all_ranges();
            let _ = sel.add_range(&range);
        }
    }

    fn walk_text(
        node: &web_sys::Node,
        target: usize,
        acc: &mut usize,
    ) -> Option<(web_sys::Node, usize)> {
        // Node::TEXT_NODE == 3
        if node.node_type() == 3 {
            let len = node.text_content().unwrap_or_default().len();
            if *acc + len >= target {
                return Some((node.clone(), *acc));
            }
            *acc += len;
            return None;
        }
        let children = node.child_nodes();
        let n = children.length();
        for i in 0..n {
            if let Some(child) = children.item(i) {
                if let Some(found) = walk_text(&child, target, acc) {
                    return Some(found);
                }
            }
        }
        None
    }
}

#[cfg(not(target_arch = "wasm32"))]
mod imp {
    use super::CaretPos;

    pub fn get_caret(_container: &()) -> Option<CaretPos> {
        None
    }
    pub fn set_caret(_container: &(), _pos: CaretPos) {}
}

#[cfg(target_arch = "wasm32")]
pub use imp::{get_caret, set_caret};

#[cfg(not(target_arch = "wasm32"))]
pub use imp::{get_caret, set_caret};
