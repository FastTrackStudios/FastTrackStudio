//! Reusable fuzzy search hook using nucleo-matcher.
//!
//! Provides a generic `use_fuzzy_search` hook that takes a signal of items,
//! a search query signal, and a key extractor closure, returning a memoized
//! filtered and scored list of results.

use crate::prelude::*;
use nucleo_matcher::{
    pattern::{CaseMatching, Normalization, Pattern},
    Config, Matcher, Utf32Str,
};

/// A reusable fuzzy search hook backed by nucleo-matcher.
///
/// Returns all items when the query is empty. When a query is present,
/// items are scored using nucleo's fuzzy matching algorithm and returned
/// sorted by score descending (best matches first).
///
/// # Arguments
///
/// * `items` - A `Memo` providing the list of items to search through.
/// * `search_query` - A `Signal<String>` containing the current search query.
/// * `key_extractor` - A closure that extracts searchable text from each item.
///
/// # Example
///
/// ```ignore
/// let results = use_fuzzy_search(
///     all_items_memo,
///     search_signal,
///     |item| format!("{} {}", item.name, item.description),
/// );
/// ```
pub fn use_fuzzy_search<T, F>(
    items: Memo<Vec<T>>,
    search_query: Signal<String>,
    key_extractor: F,
) -> Memo<Vec<T>>
where
    T: Clone + PartialEq + 'static,
    F: Fn(&T) -> String + 'static,
{
    use_memo(move || {
        let query = search_query();
        let all_items = items.read().clone();

        if query.is_empty() {
            return all_items;
        }

        let mut matcher = Matcher::new(Config::DEFAULT);
        let pattern = Pattern::parse(&query, CaseMatching::Smart, Normalization::Smart);

        let mut scored: Vec<(T, u32)> = all_items
            .into_iter()
            .filter_map(|item| {
                let search_text = key_extractor(&item);
                let mut buf = Vec::new();
                let haystack = Utf32Str::new(&search_text, &mut buf);
                pattern
                    .score(haystack, &mut matcher)
                    .map(|score| (item, score))
            })
            .collect();

        scored.sort_by(|a, b| b.1.cmp(&a.1));
        scored.into_iter().map(|(item, _)| item).collect()
    })
}
