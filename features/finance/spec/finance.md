+++
title = "Finance contract"
description = "Tracey-tracked rules the FinanceRepo + FinanceService implementations must hold."
weight = 100
+++

The finance feature is a **double-entry personal + organizational
finance system**, modeled on Firefly III, that owns its data
natively (not as a sync proxy) but can mirror to Firefly III for
users who want to keep using its reports/integrations. Built so a
single user can manage both a personal budget AND multiple business
ledgers (sole proprietor studios, LLCs, side projects) in the same
graph, with clean boundaries between them.

Driving use case: someone running a recording studio + freelance
consulting + a household budget wants (1) one app that handles all
three, (2) double-entry correctness (debits = credits, no money
appears from nowhere), (3) integration with the rest of Task —
invoices link to projects, expenses link to inventory items,
recurring rent ties to a CalDAV calendar slot, (4) reports that
cross the boundary cleanly (P&L per business, net worth across all).

This spec defines the **data contract + service surface**.
Compatibility with Firefly III is a goal for round-tripping: any
Firefly III export imports cleanly, and our export imports cleanly
into a fresh Firefly III. The optional integration layer
(`finance.integration.firefly-iii`) keeps a live Firefly instance
in sync if the user wants both.

## Reference repos

| Repo | URL | What to copy |
|---|---|---|
| **Firefly III** | <https://github.com/firefly-iii/firefly-iii> | The reference implementation. Account types (asset / expense / revenue / liability), transaction grouping (split transactions), category vs tag, budget envelopes, recurring transactions, rule engine, attachments per transaction, currency handling. Our entities round-trip with theirs. |
| **firefly-iii-data-importer** | <https://github.com/firefly-iii/data-importer> | CSV / Nordigen / Spectre import formats — useful when implementing `finance.import.*`. |
| **Firefly III docs** | <https://docs.firefly-iii.org/> | Concept primer for double-entry rules, account-type semantics, and rule engine. Read before extending the schema. |

Feature-parity goal: every Firefly III concept has a 1:1 entity in
this feature; a `firefly-iii` JSON export imports without data loss
and re-exports identically.

Rules are linked to source via `r[impl <id>]` and `r[verify <id>]`
annotations. Run `cargo xtask tracey-validate` to confirm coverage.

## Books (multi-ledger isolation)

r[finance.book.shape]
A `Book` is one isolated double-entry ledger: `id`, `name`
("Personal", "Acme Studio LLC", "Side Project"), `owner_kind`
enum (`person`, `organization`), `owner_id` (Uuid — references
`person.Person` or `org.Organization`), `default_currency` (ISO
4217 code, e.g. `USD`), `fiscal_year_start_month` (u8 1-12,
default 1), `created_at`, `archived_at` (Option).

r[finance.book.isolation]
Every other finance entity (`Account`, `Transaction`, `Category`,
`Budget`, `Tag`) has a `book_id`. Repo queries are scoped to a
book by default; cross-book queries are explicit (the net-worth
view that aggregates personal + studio + household needs all-book
access). Books never share entities — even if two books have a
category named "groceries", they are separate rows.

r[finance.book.user-visibility]
The `person.Person` (or `org.Organization`) that owns a book sees
it; other users see it only via explicit share grants. v1
single-tenant: assume the operator sees all books; v2 introduces
share grants. This rule only documents the isolation invariant.

## Accounts (Firefly III parity)

r[finance.account.shape]
An `Account` is one place money can sit or flow through: `id`,
`book_id`, `name`, `kind` enum (`asset`, `expense`, `revenue`,
`liability`, `cash`, `initial-balance`, `reconciliation`),
`subtype` enum (account-type-specific: `checking`, `savings`,
`credit-card`, `mortgage`, `loan`, `paypal`, `crypto`, `other`),
`currency` (ISO 4217), `iban` (Option), `bic` (Option),
`account_number` (Option, opaque), `current_balance_cents` (i64 —
denormalized cache), `opening_balance_cents` (i64),
`opening_balance_date` (Date), `notes`, `is_active` (bool).

r[finance.account.types-from-firefly]
The `kind` enum mirrors Firefly III's account types one-to-one:
- `asset` — checking, savings, cash on hand (positive balance = money you have)
- `expense` — counterparties you pay (groceries store, electric company)
- `revenue` — counterparties that pay you (employer, client X)
- `liability` — credit cards, mortgages, loans (positive balance = money you owe)
- `cash` — generic cash, no specific account
- `initial-balance` — system account for opening-balance entries
- `reconciliation` — system account for reconciliation adjustments

The repo accepts any string but the service validates; matches
Firefly's database enum exactly.

r[finance.account.currency-mismatch]
Accounts in the same book may have different currencies. A
transaction crossing currencies records both sides explicitly
(see `finance.transaction.foreign-currency`). The book's
`default_currency` is for reporting roll-ups only.

## Transactions (double-entry)

r[finance.transaction-group.shape]
A `TransactionGroup` is the user-facing "transaction" — what shows
in the transaction list as one row, even when it splits across
multiple accounts. Fields: `id`, `book_id`, `title`, `kind` enum
(`withdrawal`, `deposit`, `transfer`, `opening-balance`,
`reconciliation`), `date` (Date — when it happened),
`processed_at` (DateTime — when entered), `tag_ids`
(`Vec<Uuid>`), `category_id` (Option), `budget_id` (Option),
`notes`, `external_id` (Option — bank ref / import ID for dedup).

r[finance.transaction-leg.shape]
A `TransactionLeg` is one half of a double-entry pair: `id`,
`group_id`, `account_id`, `amount_cents` (i64 — signed; debit
negative or positive depending on account type), `currency`,
`foreign_amount_cents` (Option i64 — when the leg is in a
different currency from the account), `foreign_currency`
(Option), `description`, `sort_key`.

r[finance.transaction.balance-invariant]
Every `TransactionGroup` must sum to zero across its legs (after
currency normalization). The service rejects writes that violate
this. Split transactions (e.g. a grocery receipt that pays $80
food + $20 household supplies) are one group with three legs:
checking -$100, groceries +$80, household +$20. Total: 0.

r[finance.transaction.kind-derives-leg-shape]
`TransactionGroup.kind` is derivable from the leg shape but
stored denormalized for fast filtering:
- `withdrawal` — one asset/liability leg negative, one expense leg positive
- `deposit` — one revenue leg negative, one asset/liability leg positive
- `transfer` — two asset/liability legs, one negative, one positive
- `opening-balance` — one asset/liability leg, one initial-balance system-leg
- `reconciliation` — adjustment leg + reconciliation system-leg

The repo recomputes `kind` on write.

r[finance.transaction.foreign-currency]
A leg in a currency that differs from its account's currency
records `foreign_amount_cents + foreign_currency` in addition to
`amount_cents + currency`. Reporting converts to the book's
default currency using `ExchangeRate` rows (see below). Stored
amounts never lose precision — original currency + amount always
recoverable.

r[finance.transaction.attachment]
Receipts, invoices, contracts attach via the shared `attachments`
feature with `entity_kind="transaction_group"` + `entity_id=group_id`.
The repo doesn't model attachments inline; matches Firefly III's
attachment model.

## Categories, tags, budgets

r[finance.category.shape]
A `Category` is a hierarchical classification: `id`, `book_id`,
`parent_id` (Option), `name`, `color` (hex). One transaction has
zero or one category. Used for income/expense breakdown reports.

r[finance.tag.shape]
A `Tag` is a flat cross-cutting label: `id`, `book_id`, `name`,
`color`, `description`. A transaction can have many tags
(`tag_ids: Vec<Uuid>` on the group). Tags are independent of
categories — "vacation" tag can apply to expenses across food,
lodging, transport categories. Matches Firefly's tag model.

r[finance.budget.shape]
A `Budget` is an envelope: `id`, `book_id`, `name`, `notes`,
`is_active` (bool). A `BudgetPeriod` row sets the envelope's
limit per time window: `id`, `budget_id`, `start_date`, `end_date`,
`amount_limit_cents` (i64), `auto_carry_over` (bool — unused
budget rolls into next period).

r[finance.budget.usage-derivation]
`FinanceService.budget_usage(budget_id, period)` returns
`{limit, spent_cents, remaining, percent_used, transactions: [...]}`.
"Spent" sums all `withdrawal` transactions in the period whose
group references the budget. UI renders progress bar with a
warning at 80%, red at 100%.

## Recurring transactions

r[finance.recurring-transaction.shape]
A `RecurringTransaction` row generates `TransactionGroup`s on a
schedule: `id`, `book_id`, `title`, `kind` (mirrors
TransactionGroup.kind), `from_account_id`, `to_account_id`,
`amount_cents`, `currency`, `category_id` (Option),
`budget_id` (Option), `tag_ids` (`Vec<Uuid>`),
`first_date`, `repetition_rule` (RRULE-style string — `FREQ=MONTHLY;BYMONTHDAY=1`),
`end_date` (Option), `skip_dates` (`Vec<Date>` — manual
exclusions), `last_generated_for_date` (Option), `is_active`
(bool), `notes`.

r[finance.recurring-transaction.generation]
`FinanceService.materialize_recurring()` runs on a schedule
(daily) plus on user trigger. For each active recurring rule,
generate any missing `TransactionGroup`s up to today (or up to
`generate_through_date` if the user wants to pre-populate the
next month). Idempotent: re-runs don't duplicate.

r[finance.recurring-transaction.calendar-integration]
A `RecurringTransaction` optionally appears on the `calendar`
feature as a recurring event in a "Bills" calendar (read-only
from calendar side). Tied via
`calendar.event.spawned_from_finance_recurring_id`.

## Bills / due tracking

r[finance.bill.shape]
A `Bill` is an expected expense the user wants to track for
"is this paid yet?": `id`, `book_id`, `name`, `match_pattern`
(text — partial transaction description that links a paid
transaction to this bill, e.g. "Comcast"), `amount_min_cents`,
`amount_max_cents`, `currency`, `expected_repeat` (RRULE),
`account_id` (Option — which account it usually comes out of),
`notes`, `is_active`.

r[finance.bill.matching]
On every transaction insert, the service checks active bills:
fuzzy-match description against `match_pattern`, amount within
[min, max]. On match, link the transaction to the bill via
`TransactionGroup.bill_id`. The UI renders unpaid bills (no
matched transaction in the current period) with a due indicator.

## Reconciliation

r[finance.reconciliation.shape]
A `Reconciliation` row records that an account was reconciled
against a statement on a given date: `id`, `account_id`,
`statement_date`, `statement_balance_cents`,
`calculated_balance_cents` (derived at reconcile time),
`difference_cents` (statement − calculated; nonzero ⇒ adjustment
needed), `adjustment_transaction_id` (Option — created when
user accepts a delta), `notes`, `reconciled_by_person_id`.

r[finance.reconciliation.flow]
`FinanceService.reconcile(account_id, statement_date, statement_balance)`:
(1) compute `calculated_balance` from all legs ≤ statement_date,
(2) write a `Reconciliation` row,
(3) if `difference != 0`, return the value to the UI — the user
either fixes a missing/extra transaction, or accepts the delta
which creates a balancing `TransactionGroup` with kind=`reconciliation`.

## Rules engine

r[finance.rule.shape]
A `Rule` is "when transaction matches X, do Y": `id`, `book_id`,
`name`, `priority` (i32 — lower runs first), `enabled` (bool),
`trigger` enum (`on-create`, `on-update`, `manual`),
`conditions_json` (predicate tree: account / amount / description /
date / current category / current tag, all/any combinators),
`actions_json` (set-category, set-budget, add-tag, set-notes,
move-to-account, mark-bill), `match_count` (u32),
`last_matched_at` (Option).

r[finance.rule.execution]
`FinanceService.apply_rules(group_id)` runs all enabled rules in
priority order. Each matching rule's actions apply; later rules
see the updated state. The user can dry-run a rule to preview
matches without applying. Matches Firefly III's rule engine
semantics.

## Currencies and exchange rates

r[finance.currency.shape]
A `Currency` is a tracked currency: `id`, `book_id` (Option —
None for system currencies), `code` (ISO 4217), `symbol`,
`decimal_places` (u8 — default 2, JPY=0, BTC=8), `is_enabled`
(bool), `name`.

r[finance.exchange-rate.shape]
An `ExchangeRate` row pins a rate at a date: `id`, `from_code`,
`to_code`, `date`, `rate` (f64 — units of `to` per unit of `from`),
`source` enum (`manual`, `ecb`, `fixer`, `imported`). The repo
returns the most recent rate at or before a given date for a
currency pair. Multi-currency reporting uses these to convert
to the book's default currency.

r[finance.exchange-rate.update]
`FinanceService.refresh_rates()` fetches from a configured
provider (default ECB), inserting new `ExchangeRate` rows.
Schedulable; manual rates are never overwritten.

## Imports and exports

r[finance.import.csv]
`FinanceService.import_csv(account_id, file, mapping)` parses CSV
according to a user-supplied column mapping (date, amount,
description, optional category, optional balance), creates
`TransactionGroup`s, and runs the rules engine on each. Returns
a summary `{imported, skipped_duplicates, errors}`.

r[finance.import.dedup]
Duplicate detection on import uses `external_id` if present (bank
reference number) else `(date, amount, description-hash)` tuple
within ±3 days. Suspected duplicates surface in the import
summary for user confirmation rather than auto-skipping.

r[finance.import.firefly-iii]
`FinanceService.import_firefly_iii(file)` accepts a Firefly III
v6+ JSON export and creates the equivalent Task entities: books,
accounts, transactions (with legs preserved), categories, tags,
budgets, recurring, rules. Idempotent on external IDs.

r[finance.export.firefly-iii]
`FinanceService.export_firefly_iii(book_id)` produces a JSON
document the upstream Firefly III importer accepts unchanged.
Round-trip fidelity is a hard requirement — round-trip tests
live under `features/finance/tests/firefly_iii_round_trip.rs`.

## Integration layer (optional live sync)

r[finance.integration.firefly-iii.connector]
A `FireflyIiiConnection` row records a live Firefly III instance:
`id`, `book_id` (which Task book mirrors this Firefly),
`base_url`, `auth_kind` enum (`personal-access-token`, `oauth2`),
`auth_ref` (opaque secrets ref), `sync_mode` enum (`one-way-pull`,
`one-way-push`, `bidirectional`), `last_sync_at`, `last_error`.

r[finance.integration.firefly-iii.sync]
Sync runs via a service-mediated diff: pull entities since
`last_sync_at`, compare, write the missing side. Conflict policy
follows Firefly III's modified-date — most recent wins (LWW
across systems). `external_id` preserves cross-system identity.

r[finance.integration.firefly-iii.optional]
The integration is optional — Task is the system of record by
default. Users who already invested in Firefly III can keep both
running with bidirectional sync; users adopting Task fresh skip
the integration entirely. No Firefly III dependency at runtime
unless the connection row exists.

## Reports

r[finance.report.profit-loss]
`FinanceService.profit_loss(book_id, start, end)` returns
`{revenue_total, expense_total, net, by_category: [...]}`. Sums
all `deposit` legs (revenue side) minus `withdrawal` legs
(expense side) over the date range, grouped by category. Used
for the studio's quarterly P&L and the household's annual
spending review.

r[finance.report.net-worth]
`FinanceService.net_worth(date, books?)` aggregates
`Account.current_balance_cents` across all asset accounts minus
all liability accounts at a date. When `books=None`, sums across
every book the user owns (personal + studio + household); when
specified, scopes to those books.

r[finance.report.cashflow]
`FinanceService.cashflow(book_id, start, end, grouping)`
returns time-series of incoming vs outgoing over the period,
grouped by `day`, `week`, `month`, or `quarter`. Powers the
"cash on hand by month" chart Firefly III ships in its dashboard.

## Cross-system integration

r[finance.integration.invoice]
A `transaction-group` may link to an `invoice.Invoice` via
`TransactionGroup.invoice_id`. The deposit-leg insertion for the
paying account is automatically created when the invoice is
marked paid (with bidirectional flag the user toggles per
account).

r[finance.integration.project]
A `transaction-group` may link to a `project.Project` via
`TransactionGroup.project_id` so per-project P&L is queryable.
("Studio renovation cost X total — Y materials + Z labor across
12 transactions tagged to the same project.")

r[finance.integration.inventory]
A `withdrawal` for purchasing inventory ties to the resulting
`inventory.InventoryItem` via
`TransactionGroup.inventory_item_id`. The item's
`purchase_price_cents` is sourced from the transaction; updating
either side keeps them in sync.

## CRDT semantics

r[finance.crdt.balance-derivation-server-only]
`Account.current_balance_cents` is computed server-side from the
sum of legs and is NOT user-editable. Concurrent peer edits to
legs both apply; the service recomputes the cache atomically on
each leg write. The cache invalidates and refreshes on every
transaction insert/update/delete.

r[finance.crdt.transaction-legs-atomic]
A `TransactionGroup` and its `TransactionLeg`s are written
atomically through the service; the repo's write_lock guarantees
the legs that fail the balance invariant never persist. Two
peers writing competing groups don't see partial states.

r[finance.crdt.notes-loro-text]
`TransactionGroup.notes`, `Account.notes`, and `Bill.notes` are
`LoroText` containers for multi-peer collaboration on the same
explanatory text. Same pattern as knowledge blocks.

## What this spec does NOT cover (yet)

- **Investment accounts** (stocks, ETFs, holdings with cost-basis
  tracking): Firefly III handles these via the `asset` account
  type with a generic balance. Granular cost-basis / dividend /
  capital-gains tracking is a future spec.
- **Crypto-native wallets**: a wallet is one `asset` account
  today. Multi-token tracking + on-chain reconciliation is
  future.
- **Tax-form generation** (1099, W-2 imports, Schedule C
  pre-fill): future, depends on jurisdiction.
- **Multi-tenant share grants** for showing the household budget
  to a partner: v1 single-tenant; share semantics in v2.
- **Forecasting / what-if scenarios**: future analytics feature.
- **Bank API direct connection** (Plaid, Nordigen, TrueLayer):
  v1 imports CSV only. Direct connect is a separate integration
  spec, follows Firefly III's data-importer architecture.
