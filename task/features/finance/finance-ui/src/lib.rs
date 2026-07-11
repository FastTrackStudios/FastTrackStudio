//! `finance-ui` — Dioxus components for finance.
//!
//! Surfaces to ship over the next few PRs:
//!
//! - **Ledger** — book switcher, account tree, transaction list
//!   + split editor, running balances.
//! - **Invoicing** — invoice list, invoice editor (line items,
//!   taxes, party picker, design preview), quote / credit
//!   variants, recurring template editor.
//! - **Expenses** — quick-add form, vendor picker, receipt
//!   attachment, billable-to-invoice toggle.
//! - **Parties** — unified client+vendor manager.
//! - **Reports** — P&L, AR aging, expense-by-category, tax
//!   summary. All read from facade providers; no direct DB
//!   access from the UI.
//!
//! All components stay dumb: props in, events out. Service
//! lookup goes through facade-provided contexts (see
//! `finance::service` once that lands).
