//! Cursor awareness — multi-peer cursor sync over Loro's
//! `EphemeralStore` + the `WorkspaceSync` vox channel.
//!
//! Architecture (see `plans/cursor-awareness.md`):
//! - Each `KnowledgeLive` route owns one `AwarenessHub` keyed by
//!   doc id. The hub wraps a local `EphemeralStore` and the
//!   peer-id we publish under.
//! - Local cursor changes write `cursor::<peer_uuid>` into the
//!   store. The store's `subscribe_local_updates` callback then
//!   feeds encoded bytes into a debounced outbound queue.
//! - Inbound frames from the server come via
//!   `WorkspaceSync::subscribe_awareness`; we apply them to the
//!   store, which fires the local subscriber (filtered by event
//!   `by = Import`) for the UI to re-resolve remote cursors.
//! - Remote cursor payloads carry a **stable Loro cursor**
//!   (encoded bytes) plus a fallback byte offset. Resolvers
//!   prefer the stable cursor and fall back to the offset when
//!   decode fails (e.g., the source block isn't loaded locally).

use std::sync::Arc;

use crdt::awareness::EphemeralStore;
use uuid::Uuid;

/// One peer's identity for the awareness channel. Ephemeral —
/// regenerated per session. Color is derived from the peer id
/// so two peers can't collide visually unless their UUIDs hash
/// to the same hue (~1/360 chance, fine for v1).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AwarenessIdentity {
    pub peer_id: Uuid,
    pub name: String,
    pub color: String, // CSS color, e.g. `"hsl(220 70% 55%)"`
}

impl AwarenessIdentity {
    pub fn new(name: impl Into<String>) -> Self {
        let peer_id = Uuid::new_v4();
        Self {
            color: hue_for(peer_id),
            peer_id,
            name: name.into(),
        }
    }

    /// Anonymous-session identity. Name is `peer-<8 hex>` derived
    /// from the peer id so peers stay visually distinguishable in
    /// the PresenceStrip without any auth context.
    pub fn anonymous() -> Self {
        let peer_id = Uuid::new_v4();
        let hex = peer_id.simple().to_string();
        let short = &hex[..hex.len().min(8)];
        Self {
            color: hue_for(peer_id),
            peer_id,
            name: format!("peer-{short}"),
        }
    }
}

/// HSL color derived from a UUID — stable per peer, distinct
/// across peers.
pub fn hue_for(peer_id: Uuid) -> String {
    let bytes = peer_id.as_bytes();
    // Sum first 8 bytes for a cheap hue rotation.
    let sum: u32 = bytes.iter().take(8).map(|b| *b as u32).sum();
    let hue = sum % 360;
    format!("hsl({hue} 70% 55%)")
}

/// Awareness hub per knowledge doc. Holds the local
/// `EphemeralStore` and the identity we publish under. Cheaply
/// cloneable — the store is `Arc` internally.
#[derive(Clone)]
pub struct AwarenessHub {
    pub store: Arc<EphemeralStore>,
    pub identity: AwarenessIdentity,
}

impl AwarenessHub {
    /// New hub with a fresh peer id + 30s key timeout.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            store: Arc::new(EphemeralStore::new(30_000)),
            identity: AwarenessIdentity::new(name),
        }
    }

    /// New hub with an anonymous `peer-<hex>` identity. Use when
    /// no auth context is available.
    pub fn anonymous() -> Self {
        Self {
            store: Arc::new(EphemeralStore::new(30_000)),
            identity: AwarenessIdentity::anonymous(),
        }
    }

    /// Key under which this peer's cursor is published.
    pub fn cursor_key(&self) -> String {
        format!("cursor::{}", self.identity.peer_id)
    }
}

/// Resolved remote cursor — what the renderer needs to draw a
/// colored caret + label chip. `anchor` is set when the peer is
/// in Visual mode; renderers should draw a highlight from anchor
/// → (block_id, offset).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RemoteCursor {
    pub peer_id: Uuid,
    pub block_id: Uuid,
    pub offset: usize,
    pub anchor: Option<RemoteCursorAnchor>,
    /// Page the peer is viewing. Drives the "click-to-follow"
    /// behavior in `PresenceStrip` — clicking the chip jumps the
    /// local route to this page. `None` when the peer publishes
    /// before page resolution.
    pub page_id: Option<Uuid>,
    /// Vim mode the peer is in. Renderers use this to pick the
    /// cursor glyph (block vs thin caret) so a watcher can see
    /// at a glance whether their collaborator is typing or
    /// navigating.
    pub mode: PeerMode,
    pub color: String,
    pub name: String,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum PeerMode {
    #[default]
    Normal,
    Insert,
    Visual,
}

impl PeerMode {
    pub fn as_wire(self) -> &'static str {
        match self {
            PeerMode::Normal => "normal",
            PeerMode::Insert => "insert",
            PeerMode::Visual => "visual",
        }
    }
    pub fn from_wire(s: &str) -> Self {
        match s {
            "insert" => PeerMode::Insert,
            "visual" => PeerMode::Visual,
            _ => PeerMode::Normal,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RemoteCursorAnchor {
    pub block_id: Uuid,
    pub offset: usize,
}

/// Payload published under each peer's `cursor::<peer>` key.
/// `stable_cursor_bytes` is Loro's `Cursor::encode()` output —
/// transformed by remote `LoroDoc::get_cursor_pos()` so peer
/// edits don't drift our caret. `anchor_*` mirror the same
/// shape for the selection start (Visual mode); empty when no
/// selection.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CursorPayload {
    pub block_id: Uuid,
    pub fallback_offset: usize,
    pub stable_cursor_bytes: Vec<u8>,
    pub anchor_block_id: Option<Uuid>,
    pub anchor_fallback_offset: usize,
    pub anchor_stable_bytes: Vec<u8>,
    /// Page the peer is viewing. Carried so other peers can
    /// "follow" by jumping to the page even if they don't have
    /// the source block loaded locally.
    pub page_id: Option<Uuid>,
    /// Vim mode wire string ("normal" / "insert" / "visual").
    /// Stored as the wire form so we don't need to depend on
    /// `vim::VimMode` from the protocol payload.
    pub mode: String,
    pub name: String,
    pub color: String,
}

impl CursorPayload {
    /// Encode to a `LoroValue::Map` (the store's native value
    /// shape). String values for everything; binary blob is
    /// base64 for now — Loro's value model doesn't expose a
    /// `Binary` variant in the public API.
    pub fn to_loro_value(&self) -> crdt::loro::LoroValue {
        use crdt::loro::LoroValue;
        let mut map: std::collections::HashMap<String, LoroValue> =
            std::collections::HashMap::with_capacity(8);
        map.insert(
            "block_id".into(),
            LoroValue::String(self.block_id.to_string().into()),
        );
        map.insert(
            "fallback_offset".into(),
            LoroValue::I64(self.fallback_offset as i64),
        );
        map.insert(
            "stable".into(),
            LoroValue::String(b64_encode(&self.stable_cursor_bytes).into()),
        );
        let anchor_block_str = self
            .anchor_block_id
            .map(|id| id.to_string())
            .unwrap_or_default();
        map.insert(
            "anchor_block_id".into(),
            LoroValue::String(anchor_block_str.into()),
        );
        map.insert(
            "anchor_fallback_offset".into(),
            LoroValue::I64(self.anchor_fallback_offset as i64),
        );
        map.insert(
            "anchor_stable".into(),
            LoroValue::String(b64_encode(&self.anchor_stable_bytes).into()),
        );
        let page_id_str = self.page_id.map(|id| id.to_string()).unwrap_or_default();
        map.insert("page_id".into(), LoroValue::String(page_id_str.into()));
        map.insert("mode".into(), LoroValue::String(self.mode.clone().into()));
        map.insert("name".into(), LoroValue::String(self.name.clone().into()));
        map.insert("color".into(), LoroValue::String(self.color.clone().into()));
        LoroValue::Map(map.into())
    }

    /// Decode from `LoroValue::Map`. Returns `None` when any
    /// required field is missing / malformed.
    pub fn from_loro_value(v: &crdt::loro::LoroValue) -> Option<Self> {
        use crdt::loro::LoroValue;
        let LoroValue::Map(m) = v else { return None };
        let block_id_str = match m.get("block_id")? {
            LoroValue::String(s) => s.to_string(),
            _ => return None,
        };
        let block_id = Uuid::parse_str(&block_id_str).ok()?;
        let fallback_offset = match m.get("fallback_offset")? {
            LoroValue::I64(n) => (*n).max(0) as usize,
            _ => 0,
        };
        let stable_b64 = match m.get("stable")? {
            LoroValue::String(s) => s.to_string(),
            _ => return None,
        };
        let stable_cursor_bytes = b64_decode(&stable_b64).unwrap_or_default();
        let name = match m.get("name") {
            Some(LoroValue::String(s)) => s.to_string(),
            _ => String::new(),
        };
        let color = match m.get("color") {
            Some(LoroValue::String(s)) => s.to_string(),
            _ => "hsl(220 70% 55%)".into(),
        };
        let anchor_block_id = match m.get("anchor_block_id") {
            Some(LoroValue::String(s)) if !s.is_empty() => Uuid::parse_str(s).ok(),
            _ => None,
        };
        let anchor_fallback_offset = match m.get("anchor_fallback_offset") {
            Some(LoroValue::I64(n)) => (*n).max(0) as usize,
            _ => 0,
        };
        let anchor_stable_bytes = match m.get("anchor_stable") {
            Some(LoroValue::String(s)) => b64_decode(s).unwrap_or_default(),
            _ => Vec::new(),
        };
        let page_id = match m.get("page_id") {
            Some(LoroValue::String(s)) if !s.is_empty() => Uuid::parse_str(s).ok(),
            _ => None,
        };
        let mode = match m.get("mode") {
            Some(LoroValue::String(s)) => s.to_string(),
            _ => "normal".into(),
        };
        Some(Self {
            block_id,
            fallback_offset,
            stable_cursor_bytes,
            anchor_block_id,
            anchor_fallback_offset,
            anchor_stable_bytes,
            page_id,
            mode,
            name,
            color,
        })
    }
}

// ── Tiny base64 helpers — no external dep ────────────────────────────

fn b64_encode(bytes: &[u8]) -> String {
    const ALPHA: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    let mut out = String::with_capacity((bytes.len() + 2) / 3 * 4);
    for chunk in bytes.chunks(3) {
        let b0 = chunk[0];
        let b1 = if chunk.len() > 1 { chunk[1] } else { 0 };
        let b2 = if chunk.len() > 2 { chunk[2] } else { 0 };
        let v: u32 = ((b0 as u32) << 16) | ((b1 as u32) << 8) | (b2 as u32);
        out.push(ALPHA[((v >> 18) & 63) as usize] as char);
        out.push(ALPHA[((v >> 12) & 63) as usize] as char);
        if chunk.len() > 1 {
            out.push(ALPHA[((v >> 6) & 63) as usize] as char);
        } else {
            out.push('=');
        }
        if chunk.len() > 2 {
            out.push(ALPHA[(v & 63) as usize] as char);
        } else {
            out.push('=');
        }
    }
    out
}

fn b64_decode(s: &str) -> Option<Vec<u8>> {
    let val = |c: u8| -> Option<u8> {
        Some(match c {
            b'A'..=b'Z' => c - b'A',
            b'a'..=b'z' => c - b'a' + 26,
            b'0'..=b'9' => c - b'0' + 52,
            b'+' => 62,
            b'/' => 63,
            _ => return None,
        })
    };
    let bytes: Vec<u8> = s
        .bytes()
        .filter(|c| *c != b'=' && !c.is_ascii_whitespace())
        .collect();
    let mut out = Vec::with_capacity(bytes.len() * 3 / 4);
    for chunk in bytes.chunks(4) {
        let mut v: u32 = 0;
        let mut n = 0;
        for &c in chunk {
            v = (v << 6) | (val(c)? as u32);
            n += 6;
        }
        let pad = 24usize.saturating_sub(n);
        v <<= pad;
        if n >= 8 {
            out.push((v >> 16) as u8);
        }
        if n >= 16 {
            out.push((v >> 8) as u8);
        }
        if n >= 24 {
            out.push(v as u8);
        }
    }
    Some(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn b64_roundtrip() {
        for s in &[
            b"".as_slice(),
            b"f".as_slice(),
            b"fo".as_slice(),
            b"foo".as_slice(),
            b"foob".as_slice(),
            b"fooba".as_slice(),
            b"foobar".as_slice(),
        ] {
            let enc = b64_encode(s);
            let dec = b64_decode(&enc).expect("decode");
            assert_eq!(dec, *s);
        }
    }

    #[test]
    fn cursor_payload_roundtrip() {
        let p = CursorPayload {
            block_id: Uuid::new_v4(),
            fallback_offset: 42,
            stable_cursor_bytes: vec![1, 2, 3, 4, 5],
            anchor_block_id: Some(Uuid::new_v4()),
            anchor_fallback_offset: 7,
            anchor_stable_bytes: vec![9, 8, 7],
            page_id: Some(Uuid::new_v4()),
            mode: "insert".into(),
            name: "Cody".into(),
            color: "hsl(120 70% 55%)".into(),
        };
        let v = p.to_loro_value();
        let p2 = CursorPayload::from_loro_value(&v).expect("decode");
        assert_eq!(p, p2);
    }

    #[test]
    fn hue_is_stable_per_peer() {
        let id = Uuid::new_v4();
        assert_eq!(hue_for(id), hue_for(id));
    }

    #[test]
    fn anonymous_identity_uses_peer_hex_prefix() {
        // Offline-first: no auth context, no "Local" placeholder
        // — every session generates a distinct peer-<hex> name
        // so the PresenceStrip stays distinguishable.
        let a = AwarenessIdentity::anonymous();
        let b = AwarenessIdentity::anonymous();
        assert!(a.name.starts_with("peer-"), "got: {}", a.name);
        assert_eq!(a.name.len(), "peer-".len() + 8);
        assert_ne!(a.peer_id, b.peer_id, "should mint a fresh peer id");
        assert_ne!(a.color, b.color, "color derives from peer_id");
    }

    #[test]
    fn ephemeral_store_keys_age_out() {
        // Sanity test: confirm Loro's EphemeralStore actually
        // expires keys after the configured timeout. Offline
        // peers' cursors disappear cleanly without our code
        // having to do anything — that's the whole point of
        // using EphemeralStore over a hand-rolled solution.
        use crdt::awareness::EphemeralStore;
        use crdt::loro::LoroValue;

        // 1ms timeout makes the test deterministic without
        // relying on wall-clock sleep.
        let store = EphemeralStore::new(1);
        store.set("cursor::peer-a", LoroValue::String("hello".into()));
        assert!(store.get("cursor::peer-a").is_some());
        // Wait past the timeout window. EphemeralStore's
        // `remove_outdated` is the cleanup hook — without it,
        // expired keys still read back. That's by design: cleanup
        // is explicit so callers can batch it.
        std::thread::sleep(std::time::Duration::from_millis(5));
        store.remove_outdated();
        assert!(
            store.get("cursor::peer-a").is_none(),
            "key should age out after timeout + remove_outdated"
        );
    }
}
