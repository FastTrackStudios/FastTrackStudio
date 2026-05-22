//! Live change events streamed via
//! [`crate::EmailSync::subscribe`]. Mirrors the vault-proto
//! `VaultEvent` shape: per-account broadcast, `Resync` hint on
//! subscriber lag.

use facet::Facet;

#[derive(Debug, Clone, Facet)]
#[repr(u8)]
pub enum EmailEvent {
    NewMessage {
        folder: String,
        message_id: String,
    },
    FlagsChanged {
        message_id: String,
        flags: Vec<String>,
    },
    Moved {
        message_id: String,
        from_folder: String,
        to_folder: String,
    },
    Deleted {
        message_id: String,
    },
    FolderListChanged,
    /// Subscriber missed events because the broadcast lapped.
    /// Re-pull folder listings + envelopes to catch up.
    Resync,
}

#[cfg(feature = "vox")]
#[allow(unsafe_code)]
mod reborrow_impls {
    use super::EmailEvent;
    unsafe impl vox_types::Reborrow for EmailEvent {
        type Ref<'a> = EmailEvent;
    }
}
