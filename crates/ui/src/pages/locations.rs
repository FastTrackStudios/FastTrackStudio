//! `/locations` — the place register.
//!
//! Locations are the studios, rooms, storage units, venues, and
//! homes the org works out of. They live as markdown pages in the
//! vault (`type: location`) and carry a stable `id` so other
//! features (notably inventory) reference them through renames.
//!
//! This page lists the org's locations and offers a friction-light
//! "Add location" form (name + kind + optional address). Editing,
//! nesting (parent), tags, and rename live in the CLI for now; this
//! is the read + create slice, mirroring the inbox capture page.

use dioxus::prelude::*;
use fts_ui::prelude::*;
use locations_proto::Location;

use crate::orgs::{OrgMeta, OrgSelection};

const INPUT_CLS: &str = "rounded-lg border border-input bg-input/30 px-3 py-2 text-sm transition-colors \
     focus-visible:border-ring focus-visible:outline-none focus-visible:ring-[3px] \
     focus-visible:ring-ring/50 placeholder:text-muted-foreground";

/// Canonical kinds offered in the form's picker. `kind` is free-form
/// on the model, but these cover the common cases without forcing it.
const KINDS: &[&str] = &["studio", "room", "storage", "venue", "home", "other"];

#[component]
pub fn LocationsView() -> Element {
    let selection = use_context::<Signal<OrgSelection>>();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();

    // The org we list / create into (first selected, or home).
    let slug = use_memo(move || {
        crate::orgs::selected_slugs(&selection.read(), &org_list.read())
            .into_iter()
            .next()
    });

    let mut name = use_signal(String::new);
    let mut kind = use_signal(|| "other".to_string());
    let mut address = use_signal(String::new);
    // Bumped after every mutation to re-run the fetch.
    let mut refresh = use_signal(|| 0u32);

    let locations = use_resource(move || {
        let _ = refresh(); // subscribe so mutations re-fetch
        async move {
            match slug() {
                Some(s) => crate::feeds::fetch_locations(&s).await,
                None => Ok(Vec::new()),
            }
        }
    });

    // Create the drafted location, then clear the form + refetch.
    let mut create = move || {
        let n = name.read().trim().to_string();
        if n.is_empty() {
            return;
        }
        let Some(s) = slug() else { return };
        let k = kind.read().clone();
        let addr = {
            let a = address.read().trim().to_string();
            if a.is_empty() { None } else { Some(a) }
        };
        name.set(String::new());
        address.set(String::new());
        spawn(async move {
            let _ = crate::feeds::create_location(&s, &n, &k, addr).await;
            refresh += 1;
        });
    };

    let (rows, load_err): (Vec<Location>, Option<String>) = match &*locations.read() {
        Some(Ok(all)) => (all.clone(), None),
        Some(Err(e)) => (Vec::new(), Some(e.clone())),
        None => (Vec::new(), None),
    };

    rsx! {
        div { class: "mx-auto flex max-w-3xl flex-col gap-5 p-6 lg:p-10",
            div { class: "flex items-center justify-between gap-3",
                Heading { level: HeadingLevel::H1, "Locations" }
                Text { variant: TextVariant::Muted, class: "text-sm", "{rows.len()} places" }
            }
            Text {
                variant: TextVariant::Muted,
                class: "text-sm -mt-2",
                "Studios, rooms, storage, venues, and homes you work out of.",
            }

            // ── Add location ───────────────────────────────────────
            div { class: "flex flex-col gap-2 rounded-xl border border-border bg-card/40 p-3 sm:flex-row sm:items-center",
                input {
                    class: "{INPUT_CLS} flex-1",
                    placeholder: "Location name…",
                    value: "{name}",
                    oninput: move |e| name.set(e.value()),
                    onkeydown: move |e| {
                        if e.key() == Key::Enter {
                            create();
                        }
                    },
                }
                select {
                    class: "{INPUT_CLS}",
                    value: "{kind}",
                    onchange: move |e| kind.set(e.value()),
                    for k in KINDS {
                        option { value: "{k}", "{k}" }
                    }
                }
                input {
                    class: "{INPUT_CLS} flex-1",
                    placeholder: "Address (optional)",
                    value: "{address}",
                    oninput: move |e| address.set(e.value()),
                    onkeydown: move |e| {
                        if e.key() == Key::Enter {
                            create();
                        }
                    },
                }
                Button {
                    variant: ButtonVariant::Primary,
                    on_click: move |_| create(),
                    "Add"
                }
            }

            if let Some(err) = load_err {
                div { class: "rounded-lg border border-destructive/40 bg-destructive/10 px-3 py-2 text-sm text-destructive",
                    "Couldn't load locations: {err}"
                }
            }

            // ── The register ───────────────────────────────────────
            if rows.is_empty() {
                div { class: "rounded-lg border border-dashed border-border px-4 py-10 text-center",
                    Text { variant: TextVariant::Muted, "No locations yet — add your first place above." }
                }
            } else {
                div { class: "flex flex-col gap-2",
                    for loc in rows {
                        LocationRow { key: "{loc.id}", loc }
                    }
                }
            }
        }
    }
}

/// One location in the register: name + kind badge + optional address.
#[component]
fn LocationRow(loc: Location) -> Element {
    let name = loc.name.clone();
    let kind = loc.kind.clone();
    let address = loc.address.clone();

    rsx! {
        div { class: "flex items-start gap-3 rounded-lg border border-border bg-card/40 px-3 py-2",
            div { class: "flex min-w-0 flex-1 flex-col gap-1",
                Text { class: "break-words text-sm font-medium", "{name}" }
                if let Some(addr) = address.as_ref() {
                    span { class: "text-[11px] text-muted-foreground", "{addr}" }
                }
            }
            div { class: "flex shrink-0 items-center gap-2",
                span { class: "rounded bg-muted px-1.5 py-px text-[11px] text-muted-foreground", "{kind}" }
            }
        }
    }
}
