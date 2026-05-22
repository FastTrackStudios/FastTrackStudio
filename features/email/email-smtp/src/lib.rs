//! SMTP submission. Scaffold for phase 3. Will expose a
//! `SmtpSender` that backends (`email-imap`, `email-maildir`,
//! `email-nextcloud`) compose to satisfy
//! `email_proto::EmailSync::send`.

#![cfg(not(target_arch = "wasm32"))]
