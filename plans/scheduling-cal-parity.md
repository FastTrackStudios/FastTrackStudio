# Scheduling — cal.com feature parity tracker

Maps the cal.com / cal.diy surface onto our `scheduling` feature so
we can tell at a glance what's shipped, what's planned, and what we
intentionally aren't doing.

Reference checkout (read-only): `~/Development/research/cal-diy`
(actually the full cal.com monorepo — `cal.diy` is the simpler
single-user variant; we model the core types after cal.com's Prisma
schema since the shapes are the same).

## Legend

| Status | Meaning |
| --- | --- |
| 🟢 done | Shipped (in `scheduling-proto` / `scheduling` / `scheduling-ui`). |
| 🟡 partial | Shape exists in proto; impl or UI is stubbed. |
| 🔵 planned | On the roadmap, not yet started. |
| ⚪ deferred | Possible later; not committed to. |
| 🔴 won't do | Out of scope for our product. |

## Core scheduling primitives

| cal.com surface | Our surface | Status | Notes |
| --- | --- | --- | --- |
| `EventType` (title / slug / duration / location) | `scheduling_proto::EventType` | 🟢 done | Wire shape lives in `event_type.rs`. UI editor 🔵. |
| `Schedule` (named availability bundle + timezone) | `scheduling_proto::AvailabilitySchedule` | 🟢 done | UI editor 🔵. |
| `Availability` (days[] + start + end + optional date) | `scheduling_proto::AvailabilityRule` | 🟢 done | Per-date overrides via `date: Option<NaiveDate>` 🔵. |
| `Booking` (event_type + slot + attendee + status) | `scheduling_proto::Booking` + `NewBooking` + `BookingStatus` | 🟢 done | UI booking page + host inbox 🔵. |
| `User` profile / settings | Reuse vault note metadata | ⚪ deferred | Single-user for now; multi-tenant later. |
| `Membership` / Team support | — | ⚪ deferred | Solo user first; team-bookings unlock later. |
| `Host` / round-robin assignment | — | ⚪ deferred | Follows team support. |
| `Credential` (per-app OAuth tokens) | Architect auth bridge | 🔵 planned | We have `architect-auth` already; reuse. |

## Booking flow (the heart of cal.com)

| cal.com behavior | Our behavior | Status |
| --- | --- | --- |
| Public `/book/<user>/<event-slug>` page | Cal.com-style booking page | 🔵 planned (commit 2) |
| Listing open slots = `Schedule rules ∩ ¬existing bookings` | `SchedulingService::list_open_slots` | 🟡 partial — trait exists; `InMemoryScheduler` returns `Vec::new()`. Real impl in commit 2. |
| Slot timezone conversion (bookee's TZ vs host's) | TZ string on `AvailabilitySchedule`; conversion at UI render time | 🔵 planned |
| Buffer time before / after | `EventType.buffer_min` | 🟢 done (proto field exists, slot impl 🔵) |
| Minimum notice / max future booking window | EventType fields | 🔵 planned (add `min_notice_min`, `max_future_days`) |
| Daily / weekly booking limit | EventType fields | 🔵 planned |
| Custom booking questions (`form-builder`) | — | ⚪ deferred — `NewBooking.note` is a free-form catch-all for v1 |
| Confirmation: instant vs require-host-approval | `Booking.status` Pending → Confirmed transition | 🟢 done (status enum); auto-vs-manual flag on EventType 🔵 |
| No-show marking | `BookingStatus::NoShow` + `update_booking_status` | 🟢 done |
| Reschedule + cancel flow | New booking referencing prior + cancel mutation | 🔵 planned |
| Booking references (calendar IDs from external providers) | — | 🔵 planned (CalDAV → see Sync) |
| Recurring bookings | — | ⚪ deferred (RRULE on EventType after view-calendar's RRULE lands across the app) |

## Calendar sync

| cal.com integration | Our integration | Status |
| --- | --- | --- |
| Google Calendar (read busy + write bookings) | — | ⚪ deferred |
| Apple iCloud (CalDAV) | First-party CalDAV backend | 🔵 planned (high priority — user's primary sync target) |
| Microsoft Outlook / Office 365 | — | ⚪ deferred |
| Generic CalDAV | Same backend covers Apple + any RFC 4791 server | 🔵 planned |
| External calendar selection per event type | `EventType.calendar_id: Option<String>` | 🔵 planned (proto extension) |
| Webhooks on booking events | Architect event bus | 🔵 planned |
| iCal export (`.ics`) per booking | Static .ics generator in `scheduling::ical` | 🔵 planned |

## Conferencing / location

| cal.com integration | Our surface | Status |
| --- | --- | --- |
| In-person address | `EventTypeLocation::InPerson { address }` | 🟢 done |
| Phone | `EventTypeLocation::Phone` | 🟢 done |
| Generic URL (custom Zoom / Meet / etc.) | `EventTypeLocation::Link { url }` | 🟢 done |
| Cal Video (built-in) | — | 🔴 won't do (use any external link instead) |
| Zoom / Google Meet first-class | — | ⚪ deferred (the user pastes a URL on the event type for v1) |

## Notifications

| cal.com | Our plan | Status |
| --- | --- | --- |
| Email confirmation / reminders | SMTP via Vox or local stub | 🔵 planned |
| SMS reminders | — | ⚪ deferred |
| Slack / Discord | — | ⚪ deferred |
| ICS attachment on confirmation email | Reuse iCal export | 🔵 planned (paired with email) |

## Personal scheduling (our extension — *not* in cal.com)

These are the half of our feature cal.com doesn't cover at all —
the brief's daily-routine table.

| Surface | Status | Notes |
| --- | --- | --- |
| `DayTemplate` shape (ordered `TimeBlock`s + categories) | 🟢 done | Proto + markdown round-trip + scanner stub. |
| Markdown frontmatter round-trip | 🟢 done | Parser + writer in `scheduling::{parse,write}`. Tests pass. |
| Read-only `DayTemplateView` UI | 🟢 done | Renders the brief's example table with per-category color chips + summary chip row. |
| Day-template editor (drag time-block edges, inline rename, category swap) | 🔵 planned | Reuse the kanban/calendar inline-edit pattern. |
| Per-day overrides ("today Block 1 is sales call") | 🔵 planned | New entity: `DayInstance { date, template_id, allocations }`. |
| Allocation-into-blocks UI | 🔵 planned | Drop a task / event / project onto a Block; track utilization. |
| Aggregate stats (3 blocks / 7.5 h sleep / 1 h gym checks) | 🟡 partial | `Summary` chip row in the view; richer dashboard 🔵. |
| Template variants (Weekday vs Saturday vs travel) | 🟢 done | Multiple `DayTemplate` rows; UI picker 🔵. |

## CalDAV sync architecture (planned)

The proto is intentionally backend-agnostic. The CalDAV bridge slots
in as a `SchedulingService` impl:

```
┌────────────────────┐         ┌──────────────────────────────────┐
│ scheduling-ui      │────────▶│ trait SchedulingService          │
│ (Dioxus)           │         │ (#[architect::rpc] in proto)     │
└────────────────────┘         └─────────┬────────────────────────┘
                                         │
                  ┌──────────────────────┼────────────────────────┐
                  ▼                      ▼                        ▼
       InMemoryScheduler         VaultScheduler              CaldavScheduler
       (tests + demo)            (markdown round-trip)       (mirror to remote)
                                         │                        │
                                         ▼                        ▼
                                  vault::Vault            tower::caldav (TBD)
```

The CalDAV impl wraps the vault scheduler — every write fans out to
both the markdown vault *and* the remote server, with a sync token
held in vault metadata so reconnects are idempotent.

## Out of scope (locked)

These are cal.com features we explicitly aren't building:

- 🔴 **Multi-tenant SaaS hosting** — we ship a desktop / self-host product.
- 🔴 **Stripe / payment** — pay-to-book is not in our product surface.
- 🔴 **Cal Video / first-party video** — use any external URL.
- 🔴 **Marketing landing pages / blog / docs hosting** — not the product.
- 🔴 **Embed JS SDK / iframe SDK** — public booking page is enough; teams hosting it elsewhere is a follow-up.
- 🔴 **App-store ecosystem** — cal.com's "Apps" plugin layer doesn't translate; our extensibility is the vault + architect bus.

## Roadmap order

Suggested order for follow-up commits (top = next):

1. **Vault-backed `SchedulingService`** — round-trip event types,
   schedules, and bookings through markdown files in the vault. Slot
   generation reads from real data.
2. **Event-type editor UI** — form for title / duration / location /
   schedule pick. Drives `upsert_event_type`.
3. **Schedule editor UI** — weekly grid of availability rules. Click
   to add a window; drag edges to resize. Per-date overrides next.
4. **Public booking page UI** — slot list + booking form. The first
   client-facing surface; the existing `view-calendar` time-grid is
   the natural starting point for the slot picker.
5. **CalDAV backend** — Apple iCloud first. New crate
   `scheduling-caldav` implementing `SchedulingService`, mounted via
   `architect::serve` so the UI talks to it through vox the same way
   it talks to the in-memory backend.
6. **Day-template editor** — drag block edges + inline rename +
   category swap. Reuse view-table inline-edit patterns.
7. **Allocation flow** — drag a `task` / `view-calendar` event onto
   a Block; track per-block utilization. Surfaces the "Block 1 is
   only 60 % allocated" view the user mentioned.
8. **Notifications** — email confirmation + .ics attachment.
9. **Recurring bookings** — pair with view-calendar's RRULE support.
10. **Team bookings** — multi-user shapes (Membership / Host /
    round-robin).
