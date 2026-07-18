//! Contacts CRUD + CardDAV sync accounts.
//!
//! A flat surface like `recall_proto`: the directory is
//! `list_contacts`/`upsert_contact`/`delete_contact`; sync accounts get
//! their own list/upsert/delete; and `sync_account` runs a one-way pull
//! (principal discovery → addressbook-query → vCard parse → upsert),
//! returning a [`SyncReport`]. All sync methods; the backend does the
//! `spawn_blocking` for the network work.

use crate::account::{CardDavAccount, SyncReport};
use crate::contact::Contact;
use crate::error::ContactsError;

#[architect::rpc]
pub trait Contacts {
    /// Every contact in the directory, archived or not.
    fn list_contacts(&self) -> Result<Vec<Contact>, ContactsError>;

    /// One contact by id, or `None` if the file is gone.
    fn get_contact(&self, id: String) -> Result<Option<Contact>, ContactsError>;

    /// Create or replace a contact (keyed by `id`). Author, edit, link,
    /// and archive all flow through here.
    fn upsert_contact(&self, contact: &Contact) -> Result<(), ContactsError>;

    /// Permanently remove a contact from the vault.
    fn delete_contact(&self, id: &str) -> Result<(), ContactsError>;

    /// Every configured CardDAV sync account, with passwords blanked
    /// (see [`CardDavAccount::redacted`]).
    fn list_accounts(&self) -> Result<Vec<CardDavAccount>, ContactsError>;

    /// Create or replace a sync account (keyed by `id`). A blank
    /// `password` on an existing account keeps the stored credential.
    fn upsert_account(&self, account: &CardDavAccount) -> Result<(), ContactsError>;

    /// Remove a sync account. Contacts it imported are left in place.
    fn delete_account(&self, id: &str) -> Result<(), ContactsError>;

    /// Pull the account's addressbook and upsert its contacts (one-way,
    /// server → vault). Matches existing contacts by vCard `UID`, skips
    /// ETag-unchanged cards, and never touches `manual` contacts.
    fn sync_account(&self, id: String) -> Result<SyncReport, ContactsError>;
}
