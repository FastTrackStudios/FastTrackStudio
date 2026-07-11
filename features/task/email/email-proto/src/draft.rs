//! Compose-side payloads — [`Draft`] for `append_draft`/`send`,
//! [`Attachment`] / [`AttachmentMeta`] for inline + post-fetch
//! attachment shapes.

use crate::Addr;
use facet::Facet;

#[derive(Debug, Clone, Facet)]
pub struct AttachmentMeta {
    /// Backend-specific part address used by
    /// [`crate::EmailSync::fetch_attachment`].
    pub part: String,
    pub filename: Option<String>,
    pub mime: String,
    pub size: u64,
}

/// In-memory attachment carried on a [`Draft`] before send.
#[derive(Debug, Clone, Facet)]
pub struct Attachment {
    pub meta: AttachmentMeta,
    pub data: Vec<u8>,
}

/// Outgoing message. Empty `references` + `in_reply_to` =
/// fresh thread; populated = a reply.
#[derive(Debug, Clone, Facet)]
pub struct Draft {
    pub from: Addr,
    pub to: Vec<Addr>,
    pub cc: Vec<Addr>,
    pub bcc: Vec<Addr>,
    pub subject: String,
    pub body_text: String,
    pub body_html: Option<String>,
    pub in_reply_to: Option<String>,
    pub references: Vec<String>,
    pub attachments: Vec<Attachment>,
}

#[cfg(feature = "vox")]
#[allow(unsafe_code)]
mod reborrow_impls {
    use super::{Attachment, AttachmentMeta, Draft};
    unsafe impl vox_types::Reborrow for AttachmentMeta {
        type Ref<'a> = AttachmentMeta;
    }
    unsafe impl vox_types::Reborrow for Attachment {
        type Ref<'a> = Attachment;
    }
    unsafe impl vox_types::Reborrow for Draft {
        type Ref<'a> = Draft;
    }
}
