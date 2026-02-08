//! Review list and review card components for preset detail pages.
//!
//! Provides:
//! - [`ReviewCard`] — a single review with star rating, text excerpt, and author
//! - [`ReviewList`] — a scrollable list of reviews for a preset

use crate::prelude::*;

use super::star_rating::StarRating;

// region: --- ReviewCard

/// Data for a single review to display.
#[derive(Clone, PartialEq)]
pub struct ReviewData {
    pub author_name: String,
    pub score: u8,
    pub review_text: Option<String>,
    pub created_at: String,
}

/// Props for a single review card.
#[derive(Props, Clone, PartialEq)]
pub struct ReviewCardProps {
    pub review: ReviewData,
    /// Max characters for the review excerpt before truncation.
    #[props(default = 200)]
    pub max_excerpt_len: usize,
}

/// Displays a single review with star rating, author, and text excerpt.
#[component]
pub fn ReviewCard(props: ReviewCardProps) -> Element {
    let review = &props.review;

    let excerpt = review.review_text.as_ref().map(|text| {
        if text.len() > props.max_excerpt_len {
            format!("{}...", &text[..props.max_excerpt_len])
        } else {
            text.clone()
        }
    });

    rsx! {
        div { class: "p-3 rounded-lg bg-zinc-800/50 border border-zinc-700/50",
            // Header: stars + author + date
            div { class: "flex items-center justify-between mb-1",
                div { class: "flex items-center gap-2",
                    StarRating { score: review.score }
                    span { class: "text-sm font-medium text-zinc-300",
                        "{review.author_name}"
                    }
                }
                span { class: "text-xs text-zinc-500", "{review.created_at}" }
            }

            // Review text excerpt
            if let Some(excerpt) = &excerpt {
                p { class: "text-sm text-zinc-400 mt-1 leading-relaxed",
                    "{excerpt}"
                }
            }
        }
    }
}

// endregion: --- ReviewCard

// region: --- ReviewList

/// Props for the review list.
#[derive(Props, Clone, PartialEq)]
pub struct ReviewListProps {
    /// Reviews to display.
    pub reviews: Vec<ReviewData>,
    /// Maximum number of reviews to show (0 = show all).
    #[props(default = 5)]
    pub max_visible: usize,
}

/// Scrollable list of reviews for a preset detail page.
///
/// Shows up to `max_visible` reviews with a count header.
#[component]
pub fn ReviewList(props: ReviewListProps) -> Element {
    let total = props.reviews.len();

    if total == 0 {
        return rsx! {
            div { class: "text-sm text-zinc-500 italic py-4 text-center",
                "No reviews yet. Be the first to review!"
            }
        };
    }

    let visible = if props.max_visible > 0 {
        &props.reviews[..props.reviews.len().min(props.max_visible)]
    } else {
        &props.reviews[..]
    };

    rsx! {
        div { class: "space-y-2",
            // Header
            div { class: "flex items-center justify-between mb-2",
                span { class: "text-sm font-medium text-zinc-300",
                    "Reviews ({total})"
                }
            }

            // Review cards
            for review in visible.iter() {
                ReviewCard { review: review.clone() }
            }

            // "Show more" hint
            if props.max_visible > 0 && total > props.max_visible {
                div { class: "text-xs text-zinc-500 text-center pt-1",
                    "+ {total - props.max_visible} more reviews"
                }
            }
        }
    }
}

// endregion: --- ReviewList
