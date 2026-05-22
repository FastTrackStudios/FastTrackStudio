//! `finance-db` — SeaORM + SQLite, authoritative finance store.
//!
//! Entities + migrations land in follow-up commits. Schema
//! sketch (see PR description / `plans/finance.md`):
//!
//! - `books` — tenancy root (`kind = personal | business`).
//! - `accounts` — GL accounts (asset/liability/equity/income/
//!   expense), tree-shaped via `parent_id`.
//! - `transactions` + `transaction_splits` — double-entry GL.
//!   Splits sum to zero per transaction; enforced by CHECK +
//!   trigger.
//! - `parties` — unified clients + vendors (`kind = client |
//!   vendor | both | self`).
//! - `invoices` — single table for invoice/quote/credit/
//!   recurring-template variants. `line_items_json` snapshots
//!   line items at "mark sent" time (mirrors InvoiceNinja).
//! - `invoice_payments` + `payments` — polymorphic allocation
//!   (one payment ↔ many invoices/credits).
//! - `expenses` — AP side; posts to GL like everything else.
//! - `recurring_schedules` — drives invoice generation; cron
//!   loop bumps `next_run_date`.
//! - `activities` — append-only audit log (entity_kind +
//!   entity_id + before/after JSON).
//!
//! See `finance-proto` for the wire-level types these tables
//! materialize.

// Empty for now — entities + migrations follow.
