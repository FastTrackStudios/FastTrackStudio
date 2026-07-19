//! `identity-proto` — the identity-locker wire contract.
//!
//! One [`LinkedServer`] row per home-org user per linked remote server.
//! It holds the encrypted session token (`token_ciphertext`, an
//! `auth::crypto::encrypt_secret()` envelope — never plaintext) that
//! lets the home server act on the user's behalf against another
//! server they've linked.
//!
//! ## Storage
//!
//! [`LinkedServer`] uses `#[derive(architect::Entity)]`, so it gets the
//! wasm-clean wire struct, `LinkedServerCreate` / `LinkedServerUpdate`
//! / `LinkedServerList`, the `LinkedServerRepo` service trait, and
//! (with `--features server`) the SeaORM
//! `Model`/`Entity`/`Column`/`Relation`/`ActiveModel` plus
//! `LinkedServerRepoStorage<C>`.
//!
//! No RPC service lives here yet — that arrives in a later subtask.

pub mod linked_server;

pub use linked_server::LinkedServer;
