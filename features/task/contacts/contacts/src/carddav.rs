//! CardDAV import — the one-way pull (server → vault).
//!
//! [`import`] connects to a [`CardDavAccount`], discovers its
//! addressbook, fetches the vCards, and maps each to a
//! [`contacts_proto::Contact`] with `uid` / `etag` / `source` /
//! `account` filled in. [`crate::VaultContacts::sync_account`] then
//! reconciles the returned contacts against the vault (add / update /
//! skip by UID + ETag).
//!
//! ## Status: seam only
//!
//! The protocol layer (principal discovery → `addressbook-query`
//! REPORT → vCard 3.0/4.0 parse) is net-new and lands in a dedicated
//! pass. Until then this returns a [`ContactsError::Sync`] so the rest
//! of the feature — manual contacts, account management, the UI — is
//! fully usable and the wire contract is stable.
//!
//! Implementation notes for that pass:
//! - Nextcloud/generic: walk `/.well-known/carddav` → `current-user-
//!   principal` → `addressbook-home-set` → the addressbook collections.
//! - iCloud: fixed root `https://contacts.icloud.com`, principal
//!   discovery, app-specific password (Basic auth).
//! - Map vCard `FN`/`N`/`ORG`/`TITLE`/`EMAIL`/`TEL`/`ADR`/`BDAY`/
//!   `CATEGORIES`/`NOTE`/`UID` onto [`contacts_proto::Contact`]
//!   (multi-values newline-joined); stamp `etag` from the DAV response,
//!   `source` from the provider, `account` from the account label.

use contacts_proto::{CardDavAccount, Contact, ContactsError};

/// Pull + map every vCard in the account's addressbook. See the module
/// docs — currently a seam that reports "not yet implemented".
pub fn import(account: &CardDavAccount) -> Result<Vec<Contact>, ContactsError> {
    let _ = |_: Contact| {}; // keep `Contact` referenced for the seam
    Err(ContactsError::Sync {
        message: format!(
            "CardDAV import for '{}' ({}) isn't wired up yet",
            account.label, account.provider
        ),
    })
}
