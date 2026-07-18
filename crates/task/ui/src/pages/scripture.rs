//! `/scripture` — the Bible reader.
//!
//! Reads the org's installed scripture (the read-only Resources Library
//! spine) via [`scripture_proto::ScriptureService`]. Pick a translation
//! and book, page through chapters, and read verses. Each verse carries
//! its stable OSIS id as the element `id`, so it's a permalink anchor —
//! the first step toward vault notes linking straight to `John 3:16`.
//!
//! Read-only: there's no editing here. Verses come from `ChapterView`
//! DTOs; the heavy `VerseId`/`Book` logic stays server-side.

use std::collections::BTreeMap;

use dioxus::prelude::*;
use fts_ui::prelude::*;
use scripture_proto::{Book, ChapterView, ComparisonView, VerseBacklinks};

use crate::orgs::{OrgMeta, OrgSelection};
use crate::routes::Route;

const CTRL_CLS: &str = "rounded-lg border border-input bg-input/30 px-3 py-2 text-sm transition-colors \
     focus-visible:border-ring focus-visible:outline-none focus-visible:ring-[3px] \
     focus-visible:ring-ring/50";

#[component]
pub fn ScriptureView() -> Element {
    let selection = use_context::<Signal<OrgSelection>>();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();
    let slug = use_memo(move || {
        crate::orgs::selected_slugs(&selection.read(), &org_list.read())
            .into_iter()
            .next()
    });

    let mut translation = use_signal(|| "WEB".to_string());
    let mut book = use_signal(|| "John".to_string());
    let mut chapter = use_signal(|| 1u16);

    // Compare panel state.
    let mut compare_ref = use_signal(String::new);
    let mut compare_tx = use_signal(String::new);
    let mut compare_query = use_signal(|| None::<(String, Vec<String>)>);

    // Installed translations for the picker.
    let translations = use_resource(move || async move {
        match slug() {
            Some(s) => crate::feeds::fetch_translations(&s)
                .await
                .unwrap_or_default(),
            None => Vec::new(),
        }
    });
    let tx_list = translations.read().clone().unwrap_or_default();

    // The current chapter — re-fetches whenever a picker changes.
    let view = use_resource(move || async move {
        let s = slug()?;
        crate::feeds::fetch_chapter(&s, &translation(), &book(), chapter())
            .await
            .ok()
    });
    let pending = view.read().is_none();
    let chapter_view: Option<ChapterView> = view.read().clone().flatten();
    let chapter_count = chapter_view.as_ref().map_or(1, |c| c.chapter_count);

    // Per-verse backlinks for this chapter (vault notes that link a
    // verse, including span links like `[[John 3:16-20]]`). Independent
    // of translation, so it doesn't re-fetch when the edition changes.
    let backlinks = use_resource(move || async move {
        let s = slug()?;
        crate::feeds::fetch_chapter_backlinks(&s, &book(), chapter())
            .await
            .ok()
    });
    let backlink_map: BTreeMap<u16, VerseBacklinks> = backlinks
        .read()
        .clone()
        .flatten()
        .unwrap_or_default()
        .into_iter()
        .map(|b| (b.verse, b))
        .collect();

    // The on-demand translation comparison.
    let comparison = use_resource(move || async move {
        let (reference, txs) = compare_query()?;
        let s = slug()?;
        crate::feeds::fetch_comparison(&s, &reference, txs)
            .await
            .ok()
    });
    let comparison_view: Option<ComparisonView> = comparison.read().clone().flatten();

    let books: Vec<&'static str> = (1..=66)
        .filter_map(Book::from_ordinal)
        .map(Book::name)
        .collect();

    rsx! {
        div { class: "mx-auto flex max-w-3xl flex-col gap-5 p-4 sm:p-6 lg:p-10",
            // ── Controls ──
            div { class: "flex flex-wrap items-center gap-3",
                Heading { level: HeadingLevel::H1, "Scripture" }
                Select {
                    value: translation,
                    placeholder: "Translation".to_string(),
                    SelectContent {
                        for (i, t) in tx_list.iter().enumerate() {
                            SelectItem { key: "{t.id}", value: "{t.id}", index: i, "{t.id}" }
                        }
                    }
                }
                Select {
                    value: book,
                    placeholder: "Book".to_string(),
                    on_change: move |_v: String| chapter.set(1),
                    SelectContent {
                        for (i, b) in books.iter().enumerate() {
                            SelectItem { key: "{b}", value: "{b}", index: i, "{b}" }
                        }
                    }
                }
                div { class: "flex items-center gap-2",
                    Button {
                        variant: ButtonVariant::Outline,
                        disabled: chapter() <= 1,
                        on_click: move |_| {
                            let c = chapter();
                            if c > 1 {
                                chapter.set(c - 1);
                            }
                        },
                        "Prev"
                    }
                    Text { class: "min-w-16 text-center text-sm", "Chapter {chapter}" }
                    Button {
                        variant: ButtonVariant::Outline,
                        disabled: chapter() >= chapter_count as u16,
                        on_click: move |_| chapter.set(chapter() + 1),
                        "Next"
                    }
                }
            }

            // ── Reading pane ──
            if pending {
                crate::states::LoadingState {}
            } else if let Some(c) = chapter_view {
                div { class: "flex flex-col gap-1",
                    div { class: "flex items-baseline justify-between gap-3",
                        Heading { level: HeadingLevel::H2, "{c.book_name} {c.chapter}" }
                        Text { variant: TextVariant::Muted, class: "text-xs", "{c.translation}" }
                    }
                    div { class: "mt-3 flex flex-col gap-2 leading-relaxed",
                        for v in c.verses.iter() {
                            div { key: "{v.osis}", class: "flex flex-col",
                                p { id: "{v.osis}", class: "scroll-mt-20",
                                    span {
                                        class: "mr-2 select-none align-super text-xs font-semibold text-muted-foreground",
                                        "{v.verse}"
                                    }
                                    span { "{v.text}" }
                                    if let Some(bl) = backlink_map.get(&v.verse) {
                                        span {
                                            class: "ml-2 select-none align-super text-xs text-primary",
                                            title: "{bl.notes.len()} linked note(s)",
                                            "🔗{bl.notes.len()}"
                                        }
                                    }
                                }
                                // Linked notes — click through to the note.
                                if let Some(bl) = backlink_map.get(&v.verse) {
                                    div { class: "ml-6 mt-1 flex flex-col gap-0.5 border-l border-border pl-3",
                                        for n in bl.notes.iter() {
                                            Link {
                                                key: "{n.note_path}",
                                                to: Route::VaultRoute { path: n.note_path.clone() },
                                                class: "text-xs text-muted-foreground hover:text-foreground",
                                                title: "{n.excerpt}",
                                                "↳ {n.note_title}"
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            } else {
                crate::states::EmptyState {
                    title: "Nothing to show",
                    hint: "Pick a translation and book — install a corpus into the org's resource library if the list is empty.",
                }
            }

            // ── Compare translations ──
            div { class: "mt-6 flex flex-col gap-3 border-t border-border pt-4",
                Heading { level: HeadingLevel::H2, "Compare translations" }
                div { class: "flex flex-wrap items-center gap-2",
                    input {
                        class: CTRL_CLS,
                        placeholder: "John 3:16-18 or Genesis 4:3-Exodus 15:17",
                        value: "{compare_ref}",
                        oninput: move |e| compare_ref.set(e.value()),
                    }
                    input {
                        class: CTRL_CLS,
                        placeholder: "translations (optional, e.g. WEB, BSB)",
                        value: "{compare_tx}",
                        oninput: move |e| compare_tx.set(e.value()),
                    }
                    Button {
                        variant: ButtonVariant::Primary,
                        on_click: move |_| {
                            let r = compare_ref.read().trim().to_string();
                            if r.is_empty() {
                                return;
                            }
                            let txs: Vec<String> = compare_tx
                                .read()
                                .split([',', ' '])
                                .filter(|s| !s.is_empty())
                                .map(|s| s.to_uppercase())
                                .collect();
                            compare_query.set(Some((r, txs)));
                        },
                        "Compare"
                    }
                }
                if let Some(cv) = comparison_view {
                    div { class: "overflow-x-auto",
                        table { class: "w-full border-collapse text-sm",
                            thead {
                                tr {
                                    th { class: "border-b border-border px-2 py-1 text-left align-bottom text-xs text-muted-foreground",
                                        "{cv.reference}"
                                    }
                                    for t in cv.translations.iter() {
                                        th {
                                            key: "{t}",
                                            class: "border-b border-border px-2 py-1 text-left align-bottom text-xs font-semibold",
                                            "{t}"
                                        }
                                    }
                                }
                            }
                            tbody {
                                for row in cv.rows.iter() {
                                    tr { key: "{row.reference}",
                                        td { class: "whitespace-nowrap border-b border-border/40 px-2 py-1 align-top text-xs text-muted-foreground",
                                            "{row.reference}"
                                        }
                                        for (ci, cell) in row.cells.iter().enumerate() {
                                            td {
                                                key: "{ci}",
                                                class: "border-b border-border/40 px-2 py-1 align-top leading-relaxed",
                                                "{cell}"
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
