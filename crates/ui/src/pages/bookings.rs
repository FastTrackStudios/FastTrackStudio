//! `/bookings` — the Cal.com-style booking admin.
//!
//! The host's side of the scheduling feature's "booking half":
//! list the bookable event types (name + duration), list upcoming
//! bookings (who / when / which event type), and offer a
//! friction-light "Add event type" form (title + duration).
//!
//! Event types and bookings live as markdown pages in the vault
//! (`scheduling/event-types/` + `scheduling/bookings/`) and are
//! served by the same `VaultScheduler` as the day-plan half. State is
//! the shared optimistic store ([`crate::stores`]) for **both** lists:
//! a created event type appears instantly as a typed `Id::Temp` row
//! and reconciles against the persisted entity (no refresh-counter
//! refetch); a cancelled booking vanishes instantly and rolls back +
//! notifies on failure.

use architect::Id;
use dioxus::prelude::*;
use fts_ui::prelude::*;
use scheduling_proto::{Booking, BookingStatus, EventType};

use crate::orgs::{OrgMeta, OrgSelection};
use crate::stores;

const INPUT_CLS: &str = "rounded-lg border border-input bg-input/30 px-3 py-2 text-sm transition-colors \
     focus-visible:border-ring focus-visible:outline-none focus-visible:ring-[3px] \
     focus-visible:ring-ring/50 placeholder:text-muted-foreground";

/// Duration presets offered in the form's picker (minutes). `duration_min`
/// is free-form on the model; these cover the common Cal.com cases.
const DURATIONS: &[u16] = &[15, 30, 45, 60, 90];

#[component]
pub fn BookingsView() -> Element {
    let selection = use_context::<Signal<OrgSelection>>();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();

    // The org we list / create into (first selected, or home).
    let slug = use_memo(move || {
        crate::orgs::selected_slugs(&selection.read(), &org_list.read())
            .into_iter()
            .next()
    });

    let mut title = use_signal(String::new);
    let mut duration = use_signal(|| 30u16);

    // Shared optimistic stores for both halves of the page.
    let types_result = stores::use_event_type_list();
    let type_muts = stores::use_event_type_mutations();
    let bookings_result = stores::use_booking_list();

    // Create the drafted event type: it appears instantly, then
    // reconciles against the persisted entity.
    let mut create = move || {
        let t = title.read().trim().to_string();
        if t.is_empty() {
            return;
        }
        let Some(s) = slug() else { return };
        let d = duration();
        title.set(String::new());
        type_muts.create(s, stores::draft_event_type(t, d));
    };

    let types: Vec<(Id<String>, EventType)> = types_result.value().cloned().unwrap_or_default();
    let types_err = types_result.error().cloned();

    let rows: Vec<(Id<String>, Booking)> = bookings_result.value().cloned().unwrap_or_default();
    let bookings_err = bookings_result.error().cloned();

    rsx! {
        div { class: "mx-auto flex max-w-3xl flex-col gap-5 p-6 lg:p-10",
            div { class: "flex items-center justify-between gap-3",
                Heading { level: HeadingLevel::H1, "Bookings" }
                Text { variant: TextVariant::Muted, class: "text-sm", "{types.len()} event types" }
            }
            Text {
                variant: TextVariant::Muted,
                class: "text-sm -mt-2",
                "Bookable event types and the bookings people have made against them.",
            }

            // ── Add event type ─────────────────────────────────────
            div { class: "flex flex-col gap-2 rounded-xl border border-border bg-card/40 p-3 sm:flex-row sm:items-center",
                input {
                    class: "{INPUT_CLS} flex-1",
                    placeholder: "Event type name… (e.g. 30-min consult)",
                    value: "{title}",
                    oninput: move |e| title.set(e.value()),
                    onkeydown: move |e| {
                        if e.key() == Key::Enter {
                            create();
                        }
                    },
                }
                select {
                    class: "{INPUT_CLS}",
                    value: "{duration}",
                    onchange: move |e| {
                        if let Ok(d) = e.value().parse::<u16>() {
                            duration.set(d);
                        }
                    },
                    for d in DURATIONS {
                        option { value: "{d}", "{d} min" }
                    }
                }
                Button {
                    variant: ButtonVariant::Primary,
                    on_click: move |_| create(),
                    "Add"
                }
            }

            if let Some(err) = types_err {
                div { class: "rounded-lg border border-destructive/40 bg-destructive/10 px-3 py-2 text-sm text-destructive",
                    "Couldn't load event types: {err}"
                }
            }

            // ── Event types ────────────────────────────────────────
            Heading { level: HeadingLevel::H2, class: "text-base", "Event types" }
            if types.is_empty() {
                div { class: "rounded-lg border border-dashed border-border px-4 py-10 text-center",
                    Text { variant: TextVariant::Muted, "No event types yet — add your first one above." }
                }
            } else {
                div { class: "flex flex-col gap-2",
                    for (id, et) in types {
                        EventTypeRow { key: "{id}", pending: id.is_temp(), event_type: et }
                    }
                }
            }

            // ── Bookings ───────────────────────────────────────────
            Heading { level: HeadingLevel::H2, class: "text-base mt-2", "Upcoming bookings" }
            if let Some(err) = bookings_err {
                div { class: "rounded-lg border border-destructive/40 bg-destructive/10 px-3 py-2 text-sm text-destructive",
                    "Couldn't load bookings: {err}"
                }
            }
            if rows.is_empty() {
                div { class: "rounded-lg border border-dashed border-border px-4 py-10 text-center",
                    Text { variant: TextVariant::Muted, "No bookings yet." }
                }
            } else {
                div { class: "flex flex-col gap-2",
                    for (id, booking) in rows {
                        BookingRow {
                            key: "{id}",
                            pending: id.is_temp(),
                            slug: slug().unwrap_or_default(),
                            booking,
                        }
                    }
                }
            }
        }
    }
}

/// One event type: title + duration + published badge. `pending` dims
/// an optimistic row whose write-through is in flight.
#[component]
fn EventTypeRow(event_type: EventType, pending: bool) -> Element {
    let title = event_type.title.clone();
    let duration = event_type.duration_min;
    let published = event_type.published;

    let state_cls = if pending {
        "border-border bg-card/40 opacity-60"
    } else {
        "border-border bg-card/40"
    };

    rsx! {
        div { class: "flex items-center gap-3 rounded-lg border px-3 py-2 {state_cls}",
            div { class: "flex min-w-0 flex-1 flex-col gap-1",
                Text { class: "break-words text-sm font-medium", "{title}" }
                span { class: "text-[11px] text-muted-foreground", "{duration} min" }
            }
            div { class: "flex shrink-0 items-center gap-2",
                if published {
                    StatusBadge { variant: StatusBadgeVariant::Success, label: "Published".to_string() }
                } else {
                    StatusBadge { variant: StatusBadgeVariant::Neutral, label: "Draft".to_string() }
                }
            }
        }
    }
}

/// One booking: attendee + when + status badge + cancel.
///
/// Cancelling removes the row instantly; if the server rejects it the
/// store rolls the row back and the failure lands in the notification
/// tray. `pending` dims an in-flight optimistic row.
#[component]
fn BookingRow(booking: Booking, pending: bool, slug: String) -> Element {
    let muts = stores::use_booking_mutations();
    let who = booking.attendee_name.clone();
    let email = booking.attendee_email.clone();
    let when = booking.start_utc.clone();
    let id = booking.id.0.clone();

    let (variant, label) = match booking.status {
        BookingStatus::Pending => (StatusBadgeVariant::Warning, "Pending"),
        BookingStatus::Confirmed => (StatusBadgeVariant::Success, "Confirmed"),
        BookingStatus::Cancelled => (StatusBadgeVariant::Danger, "Cancelled"),
        BookingStatus::NoShow => (StatusBadgeVariant::Danger, "No-show"),
        BookingStatus::Completed => (StatusBadgeVariant::Neutral, "Completed"),
    };

    let state_cls = if pending {
        "border-border bg-card/40 opacity-60"
    } else {
        "border-border bg-card/40"
    };

    let cancel = move |_| {
        muts.cancel(slug.clone(), id.clone());
    };

    rsx! {
        div { class: "flex items-center gap-3 rounded-lg border px-3 py-2 {state_cls}",
            div { class: "flex min-w-0 flex-1 flex-col gap-1",
                Text { class: "break-words text-sm font-medium", "{who}" }
                span { class: "text-[11px] text-muted-foreground", "{email}" }
                span { class: "text-[11px] text-muted-foreground", "{when}" }
            }
            div { class: "flex shrink-0 items-center gap-2",
                StatusBadge { variant, label: label.to_string() }
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: cancel,
                    "Cancel"
                }
            }
        }
    }
}
