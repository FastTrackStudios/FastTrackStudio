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

use dioxus::prelude::*;
use fts_ui::prelude::*;
use scripture_proto::{Book, ChapterView};

use crate::orgs::{OrgMeta, OrgSelection};

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

    let books: Vec<&'static str> = (1..=66)
        .filter_map(Book::from_ordinal)
        .map(Book::name)
        .collect();

    rsx! {
        div { class: "mx-auto flex max-w-3xl flex-col gap-5 p-4 sm:p-6 lg:p-10",
            // ── Controls ──
            div { class: "flex flex-wrap items-center gap-3",
                Heading { level: HeadingLevel::H1, "Scripture" }
                select {
                    class: CTRL_CLS,
                    value: "{translation}",
                    onchange: move |e| translation.set(e.value()),
                    for t in tx_list.iter() {
                        option { value: "{t.id}", "{t.id}" }
                    }
                }
                select {
                    class: CTRL_CLS,
                    value: "{book}",
                    onchange: move |e| {
                        book.set(e.value());
                        chapter.set(1);
                    },
                    for b in books.iter() {
                        option { value: "{b}", "{b}" }
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
                            p { id: "{v.osis}", class: "scroll-mt-20",
                                span {
                                    class: "mr-2 select-none align-super text-xs font-semibold text-muted-foreground",
                                    "{v.verse}"
                                }
                                span { "{v.text}" }
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
        }
    }
}
