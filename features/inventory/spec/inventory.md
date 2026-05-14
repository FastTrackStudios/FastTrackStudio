+++
title = "Inventory contract"
description = "Tracey-tracked rules the InventoryRepo + InventoryService implementations must hold."
weight = 100
+++

The inventory feature is a **gear / asset catalog** for physical and
digital things you own (or borrow): instruments, audio interfaces,
microphones, computers, software licenses, vehicles, books, anything
with an identity worth tracking. Modeled on Snipe-IT, Homebox, and
InvenTree. Food / consumables live in `cookbook` instead (see
`cookbook.pantry.*`).

The driving use case: a person operating **multiple recording
studios** who needs to know where every piece of gear is, what
condition it's in, what's currently lent out or borrowed, what needs
repair, and what each item is worth — for insurance, for tax, and
for not buying duplicates of something already gathering dust on the
other side of town.

Rules are linked to source via `r[impl <id>]` and `r[verify <id>]`
annotations. Run `cargo xtask tracey-validate` to confirm coverage.

## Identity and lifecycle

r[inventory.item.unique-id]
Every `InventoryItem` has a server-assigned UUIDv4 as its primary
key. Clients never invent IDs; the architect-generated `create`
endpoint mints one on insert. IDs are stable for the life of the
item — even renaming, retiring, or transferring ownership keeps the
same UUID so historical records stay joinable.

r[inventory.item.identification-fields]
An item is identified to humans by four progressively-stricter
fields: `name` (always set, free-text), `manufacturer + model`
(usually both set, used for grouping like-items), `serial_number`
(unique within a manufacturer + model when present), and `qr_code`
(globally unique short string printed on a physical label). Search
must match against any of these.

r[inventory.item.status-enum]
`InventoryItem.status` is a closed enum: `active`, `in-repair`,
`retired`, `sold`, `lost`, `checked-out`, `borrowed`. The repo
rejects writes that contain any other string. Transitions are not
restricted at the repo level — the UI / service layer enforces
sensible flows (e.g. you can't check out a retired item) but the
storage trusts what it's told.

r[inventory.item.category]
`InventoryItem.category` is a free-text suggestion field with a
recommended vocabulary (`audio-interface`, `microphone`, `outboard`,
`computer`, `instrument`, `cable`, `software-license`, `vehicle`,
`book`, `tool`, `furniture`, `other`). Repo accepts any string but
the UI offers autocomplete against existing categories so users
converge over time without a hard schema.

r[inventory.item.tags]
`InventoryItem.tags` is a free-form `Vec<String>` for cross-cutting
classifications that don't fit category (`favorite`, `loaner`,
`vintage`, `client-equipment`, `for-sale`). Tags are case-sensitive
and order-insensitive; the repo deduplicates on write.

## Location and physical placement

r[inventory.item.location]
`InventoryItem.location_id` references a record in the `location`
feature. Locations are hierarchical (a studio contains rooms,
rooms contain racks/shelves). A move is a single update to
`location_id` — there is no "move history" entity in v1; if that
becomes a use case, the `audit` feature provides row-level history.

r[inventory.item.sub-location]
`InventoryItem.sub_location` is a free-text string for sub-shelf
placement when the location feature is too coarse — e.g. "drawer
B3", "left side of producer's desk". The repo treats it as opaque
text; the UI presents it next to the location breadcrumb.

r[inventory.item.multi-site]
The location hierarchy must support **multiple top-level sites** —
e.g. "Studio A" and "Studio B" and "Mobile Rig" coexist as peers.
Filters scoped to a site (`location.is_descendant_of(site_id)`)
return only items physically present at or under that site, even if
the user owns gear across all of them.

## Condition and repair workflow

r[inventory.item.condition]
`InventoryItem.condition` is a closed enum: `excellent`, `good`,
`fair`, `poor`, `broken`. Required on create. Distinct from
`status`: a broken-but-currently-being-fixed item has
`condition=broken` AND `status=in-repair`; once fixed
`condition=good` AND `status=active`. The repo rejects unknown
values.

r[inventory.item.condition-notes]
`InventoryItem.condition_notes` is optional free-text describing
*why* the condition is what it is — "scratched faceplate but
electrically perfect", "needs new tubes", "missing one of the pair".
Surfaced in the item detail view and on the repair queue.

r[inventory.repair-ticket]
A `RepairTicket` entity tracks the repair workflow for one item.
Fields: `id`, `item_id`, `opened_at`, `closed_at` (None while open),
`reported_by_person_id` (who flagged it), `sent_to`
(repair shop / person / "myself"), `estimated_cost_cents`,
`actual_cost_cents`, `description`, `resolution`, `status` enum
(`open`, `in-progress`, `awaiting-parts`, `awaiting-pickup`,
`closed-fixed`, `closed-unfixable`, `closed-cancelled`).

r[inventory.repair-ticket.status-side-effect]
Opening a `RepairTicket` flips the underlying item's status to
`in-repair`; closing it as `closed-fixed` flips back to `active`;
closing as `closed-unfixable` flips to `retired`. The
`InventoryService.open_repair` / `close_repair` RPCs apply both
writes atomically.

r[inventory.repair-ticket.history]
A single item may have many tickets over its life. The repo
preserves closed tickets indefinitely (cheap; small entity) so the
maintenance history per item is queryable as a join on `item_id`.

## Ownership: owned vs borrowed

r[inventory.item.ownership-mode]
`InventoryItem.ownership` is a closed enum: `owned`, `borrowed`,
`rented`, `consigned`. Required on create. `owned` is the default;
the other three change which side of the ledger the item is on.

r[inventory.item.borrowed-from]
When `ownership == "borrowed"` or `"rented"`,
`InventoryItem.borrowed_from_person_id` must be set (references the
`person` feature). The repo rejects writes that violate this
invariant. For `borrowed`, `return_due_at` is optional; for
`rented`, the UI strongly suggests it. The intent is that the
person you're borrowing **from** is a first-class participant in
your inventory — you can list "all gear I have from Tom" and know
when it's due back.

r[inventory.item.consigned]
`consigned` covers "client equipment in my studio for a session" —
the client owns it, but it's physically here and worth tracking.
`borrowed_from_person_id` points to the client; the item appears in
their item list when they query "what of mine is at Cody's place".

## Checkouts and lending out

r[inventory.checkout-event.records-lending]
A `CheckoutEvent` records "I lent this item to someone". Fields:
`id`, `item_id`, `person_id` (who took it), `checked_out_at`,
`due_at`, `returned_at` (None while out), `note`. An open checkout
(returned_at is None) implies item status `checked-out`.

r[inventory.checkout-event.at-most-one-open]
An item may have at most one open `CheckoutEvent` at a time. The
service's `checkout` RPC fails with `InvalidInput` if called on an
item that already has an open event. `checkin` closes the most
recent open event by setting `returned_at`; if no open event exists,
the call is a no-op that returns `NotFound`.

r[inventory.checkout-event.overdue-query]
The repo exposes a filter for "open AND `due_at < now`" so the UI
can render an overdue list. Sorted by `due_at` ascending so the
most-overdue items show first.

## Value, purchase, warranty

r[inventory.item.value-tracking]
`InventoryItem.value_cents` stores the most recent **estimated
current value** in integer cents. Not necessarily purchase price —
depreciation, wear, and market are baked in. The user updates it
when they reassess. Used for insurance, theft-claim, and tax-loss
reporting.

r[inventory.item.purchase-record]
Purchase metadata lives on the item directly to keep the model
simple: `acquired_at`, `vendor`, `purchase_order`,
`purchase_price_cents`. Multiple-purchase items (e.g. a pair of
monitors bought at different times) are modeled as two separate
items.

r[inventory.item.warranty]
`InventoryItem.warranty_until` is an optional `DateTime<Utc>` for
when warranty expires. The UI surfaces items with warranty expiring
in the next 90 days. The repo offers a filter
`warranty_until < $now + Duration::days(90)`.

## Documents, photos, manuals

r[inventory.item.photos]
`InventoryItem.photo_url` is the primary photo. Additional photos
live in the `attachments` feature, keyed by `entity_kind=
"inventory_item"` + `entity_id=item.id` — same shape as anywhere
else in the system. The UI shows the gallery inline.

r[inventory.item.manuals]
PDFs / receipts / serial-photo / warranty-card scans attach via the
same `attachments` mechanism with `kind=manual|receipt|other`. The
repo doesn't model them as a typed field — keeps the proto small.

## Search and discovery

r[inventory.search.fulltext]
Items are full-text searchable on `name`, `manufacturer`, `model`,
`serial_number`, `notes`, and `condition_notes`. The architect
fulltext index covers all six. A single search like "neumann"
matches a microphone whose manufacturer is "Neumann" without the
user knowing which field to look in.

r[inventory.search.filter-combinations]
The repo supports filter combinations: status + category +
location_id + ownership + condition + tag — applied as AND. The UI
exposes these as facets in the catalog view.

## Sync and CRDT semantics

r[inventory.crdt.last-writer-wins-fields]
Every `InventoryItem` scalar field is LWW per write at the
`item.<field>` LoroMap key. Concurrent edits across two clients
resolve to the most recent commit by Loro clock. The UI surfaces
"changed by other peer" hints for the human; the storage doesn't
warn or merge.

r[inventory.crdt.tags-set-semantics]
`tags` merges as a set, not LWW — concurrent additions from two
peers union, concurrent removes intersect-out. Implementation uses a
`LoroList` under the hood with deduplication on decode.

## Service-level invariants

r[inventory.service.checkout-side-effects]
`InventoryService.checkout(item, person, due_at, note)` atomically:
(1) creates a `CheckoutEvent`, (2) sets `item.status=checked-out`,
(3) sets `item.owner_id=person` (the borrower). On failure none of
these writes apply. The repo's write_lock guarantees atomicity.

r[inventory.service.checkin-side-effects]
`InventoryService.checkin(item, note)` atomically: (1) closes the
most recent open `CheckoutEvent` by setting `returned_at`, (2)
flips `item.status` back to `active`, (3) clears `owner_id` back to
the item's pre-checkout owner.

## What this spec does NOT cover (yet)

- **Bulk operations**: importing 200 items from a CSV, applying a
  blanket "+10% value" to a category. Future, separate spec.
- **Insurance integration**: generating an insurer-friendly export
  with policy numbers, deductibles. Future.
- **Depreciation curves**: automatic year-over-year value decay.
  Manual updates only in v1.
- **Stock counts for consumables**: cables-with-12-of-the-same,
  picks-by-the-box. Out of scope — that's `cookbook.pantry` for
  consumables, or a future `inventory.stocked-item` if non-food
  consumables become a real use case.
