//! Star rating display and input components.
//!
//! Provides:
//! - [`StarRating`] — read-only star display (filled/empty stars)
//! - [`StarRatingInput`] — interactive star input with hover preview
//! - [`PresetRatingBadge`] — compact average rating badge for preset lists

use crate::prelude::*;

// region: --- StarRating (read-only)

/// Props for the read-only star rating display.
#[derive(Props, Clone, PartialEq)]
pub struct StarRatingProps {
    /// Rating value (1-5). 0 shows all empty stars.
    pub score: u8,
    /// Optional CSS class for the container.
    #[props(default)]
    pub class: Option<String>,
}

/// Displays a star rating as filled/empty star characters.
///
/// Shows 5 stars total, with `score` filled and the rest empty.
#[component]
pub fn StarRating(props: StarRatingProps) -> Element {
    let score = props.score.min(5);
    let class = props.class.unwrap_or_default();

    rsx! {
        span { class: "inline-flex items-center gap-0.5 {class}",
            for i in 1..=5u8 {
                span {
                    class: if i <= score { "text-yellow-400" } else { "text-zinc-600" },
                    if i <= score { "\u{2605}" } else { "\u{2606}" }
                }
            }
        }
    }
}

// endregion: --- StarRating

// region: --- StarRatingInput

/// Props for the interactive star rating input.
#[derive(Props, Clone, PartialEq)]
pub struct StarRatingInputProps {
    /// Current score (1-5, or 0 for unrated).
    pub score: u8,
    /// Callback when user clicks a star.
    pub on_rate: Callback<u8>,
    /// Whether the input is disabled (e.g., own preset).
    #[props(default)]
    pub disabled: bool,
}

/// Interactive star rating input with hover preview.
///
/// Users click a star to set their rating. Hover shows preview state.
#[component]
pub fn StarRatingInput(props: StarRatingInputProps) -> Element {
    let score = props.score.min(5);
    let mut hover_score = use_signal(|| 0u8);
    let display_score = if hover_score() > 0 { hover_score() } else { score };
    let disabled = props.disabled;

    rsx! {
        span {
            class: "inline-flex items-center gap-0.5",
            class: if disabled { "opacity-50 cursor-not-allowed" } else { "cursor-pointer" },
            onmouseleave: move |_| hover_score.set(0),
            for i in 1..=5u8 {
                span {
                    class: if i <= display_score { "text-yellow-400 text-lg" } else { "text-zinc-600 text-lg" },
                    class: if !disabled { "hover:scale-110 transition-transform" } else { "" },
                    onmouseenter: {
                        let disabled = disabled;
                        move |_| {
                            if !disabled {
                                hover_score.set(i);
                            }
                        }
                    },
                    onclick: {
                        let on_rate = props.on_rate.clone();
                        let disabled = disabled;
                        move |_| {
                            if !disabled {
                                on_rate.call(i);
                            }
                        }
                    },
                    if i <= display_score { "\u{2605}" } else { "\u{2606}" }
                }
            }
        }
    }
}

// endregion: --- StarRatingInput

// region: --- PresetRatingBadge

/// Props for the compact rating badge.
#[derive(Props, Clone, PartialEq)]
pub struct PresetRatingBadgeProps {
    /// Average rating (0.0-5.0).
    pub average: f64,
    /// Number of ratings.
    pub count: u64,
}

/// Compact badge showing average rating and count for preset browser lists.
///
/// Displays: "★ 4.2 (12)" or "No ratings" when count is 0.
#[component]
pub fn PresetRatingBadge(props: PresetRatingBadgeProps) -> Element {
    if props.count == 0 {
        return rsx! {
            span { class: "text-xs text-zinc-500", "No ratings" }
        };
    }

    rsx! {
        span { class: "inline-flex items-center gap-1 text-xs",
            span { class: "text-yellow-400", "\u{2605}" }
            span { class: "text-zinc-300", "{props.average:.1}" }
            span { class: "text-zinc-500", "({props.count})" }
        }
    }
}

// endregion: --- PresetRatingBadge
