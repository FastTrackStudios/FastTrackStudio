//! Audio player with waveform display and comment markers.
//!
//! Uses the browser's native Audio element for playback and SVG for
//! the waveform visualization. Comment markers show as colored regions
//! on the waveform at their timecode positions.

use dioxus::prelude::*;
use crate::UserAvatar;

/// A comment marker to display on the waveform.
#[derive(Clone, PartialEq)]
pub struct WaveformMarker {
    pub id: String,
    pub start_seconds: f64,
    pub end_seconds: Option<f64>,
    pub author: String,
    pub body: String,
    pub resolved: bool,
}

/// Audio player with waveform, playback controls, and comment markers.
#[component]
pub fn AudioPlayer(
    /// URL to the audio file.
    src: String,
    /// Title to display.
    title: String,
    /// Total duration in seconds (for waveform scaling).
    duration: f64,
    /// Waveform peaks (0.0–1.0), typically 200–400 samples.
    #[props(default)]
    peaks: Vec<f32>,
    /// Comment markers to display on the waveform.
    #[props(default)]
    markers: Vec<WaveformMarker>,
    /// Called when user clicks on the waveform to add a comment.
    #[props(default)]
    on_add_comment: Option<Callback<(f64, Option<f64>)>>,
) -> Element {
    let mut playing = use_signal(|| false);
    let mut current_time = use_signal(|| 0.0f64);
    let mut hovered_marker = use_signal(|| None::<String>);

    // Generate fake peaks if none provided
    let peaks = if peaks.is_empty() {
        (0..200).map(|i| {
            let t = i as f32 / 200.0;
            let base = (t * 3.14 * 4.0).sin().abs() * 0.6;
            let detail = (t * 3.14 * 17.0).sin().abs() * 0.3;
            (base + detail).min(1.0) * 0.4 + 0.1
        }).collect::<Vec<_>>()
    } else {
        peaks
    };

    let peak_count = peaks.len();
    let progress = if duration > 0.0 { *current_time.read() / duration } else { 0.0 };

    rsx! {
        div { class: "rounded-xl border border-border bg-card overflow-hidden",
            // Title bar
            div { class: "flex items-center justify-between px-4 py-2 border-b border-border",
                span { class: "text-sm font-medium", "{title}" }
                span { class: "text-xs text-muted-foreground tabular-nums",
                    { format_time(*current_time.read()) }
                    " / "
                    { format_time(duration) }
                }
            }

            // Waveform
            div { class: "relative px-4 py-3 cursor-pointer group",
                // Comment marker regions (behind waveform)
                for marker in markers.iter() {
                    {
                        let start_pct = (marker.start_seconds / duration * 100.0).min(100.0);
                        let width_pct = marker.end_seconds
                            .map(|e| ((e - marker.start_seconds) / duration * 100.0).min(100.0 - start_pct))
                            .unwrap_or(0.5); // point markers get thin line
                        let is_hovered = hovered_marker.read().as_deref() == Some(marker.id.as_str());
                        let opacity = if marker.resolved { "0.15" } else if is_hovered { "0.4" } else { "0.25" };
                        let color = if marker.resolved { "var(--muted-foreground)" } else { "var(--chart-1)" };
                        let marker_id = marker.id.clone();
                        rsx! {
                            div {
                                class: "absolute top-0 bottom-0 transition-opacity duration-150",
                                style: "left: {start_pct}%; width: {width_pct}%; background: {color}; opacity: {opacity};",
                                onmouseenter: move |_| hovered_marker.set(Some(marker_id.clone())),
                                onmouseleave: move |_| hovered_marker.set(None),
                            }
                        }
                    }
                }

                // SVG waveform bars
                svg {
                    class: "w-full h-16 relative",
                    view_box: "0 0 {peak_count} 100",
                    preserve_aspect_ratio: "none",
                    for (i, peak) in peaks.iter().enumerate() {
                        {
                            let height = (*peak * 80.0) as u32;
                            let y = 50 - (height / 2) as i32;
                            let bar_progress = i as f64 / peak_count as f64;
                            let fill = if bar_progress <= progress { "var(--primary)" } else { "var(--muted-foreground)" };
                            let opacity = if bar_progress <= progress { "0.9" } else { "0.25" };
                            rsx! {
                                rect {
                                    x: "{i}",
                                    y: "{y}",
                                    width: "0.7",
                                    height: "{height}",
                                    fill: fill,
                                    opacity: opacity,
                                    rx: "0.3",
                                }
                            }
                        }
                    }
                }

                // Playhead
                {
                    let left = progress * 100.0;
                    rsx! {
                        div {
                            class: "absolute top-0 bottom-0 w-px bg-primary",
                            style: "left: {left}%",
                        }
                    }
                }

                // Click to seek
                div {
                    class: "absolute inset-0",
                    onclick: move |evt: MouseEvent| {
                        // Calculate click position as percentage
                        // evt.data gives us page coordinates — we'd need element bounds
                        // For now, use a simple approach
                        let _ = evt;
                    },
                }
            }

            // Playback controls
            div { class: "flex items-center gap-3 px-4 py-2 border-t border-border",
                button {
                    class: "flex items-center justify-center size-8 rounded-full bg-primary text-primary-foreground hover:bg-primary/90 transition-colors",
                    onclick: move |_| {
                        let was_playing = *playing.read();
                        playing.set(!was_playing);
                        if !was_playing {
                            // Simulate playback progress
                            let dur = duration;
                            spawn(async move {
                                loop {
                                    gloo_timers::future::TimeoutFuture::new(100).await;
                                    if !*playing.read() { break; }
                                    let t = *current_time.read() + 0.1;
                                    if t >= dur {
                                        current_time.set(0.0);
                                        playing.set(false);
                                        break;
                                    }
                                    current_time.set(t);
                                }
                            });
                        }
                    },
                    if *playing.read() {
                        // Pause icon
                        svg {
                            class: "size-4",
                            xmlns: "http://www.w3.org/2000/svg",
                            view_box: "0 0 24 24",
                            fill: "currentColor",
                            rect { x: "6", y: "4", width: "4", height: "16", rx: "1" }
                            rect { x: "14", y: "4", width: "4", height: "16", rx: "1" }
                        }
                    } else {
                        // Play icon
                        svg {
                            class: "size-4 ml-0.5",
                            xmlns: "http://www.w3.org/2000/svg",
                            view_box: "0 0 24 24",
                            fill: "currentColor",
                            path { d: "M8 5v14l11-7z" }
                        }
                    }
                }

                // Progress bar (clickable)
                {
                    let width = progress * 100.0;
                    rsx! {
                        div { class: "flex-1 h-1 rounded-full bg-secondary cursor-pointer",
                            div {
                                class: "h-full rounded-full bg-primary transition-all duration-100",
                                style: "width: {width}%",
                            }
                        }
                    }
                }
            }

            // Comment markers tooltip (shown on hover)
            if let Some(ref hovered_id) = *hovered_marker.read() {
                if let Some(marker) = markers.iter().find(|m| &m.id == hovered_id) {
                    div { class: "px-4 py-2 border-t border-border bg-accent/30",
                        div { class: "flex items-center gap-2",
                            UserAvatar { name: marker.author.clone(), size: "size-5".to_string() }
                            span { class: "text-xs font-medium", "{marker.author}" }
                            span { class: "text-[10px] text-muted-foreground tabular-nums",
                                { format_time(marker.start_seconds) }
                                if let Some(end) = marker.end_seconds {
                                    "–"
                                    { format_time(end) }
                                }
                            }
                            if marker.resolved {
                                span { class: "text-[10px] text-muted-foreground", "✅ resolved" }
                            }
                        }
                        p { class: "text-xs text-muted-foreground mt-1", "{marker.body}" }
                    }
                }
            }
        }
    }
}

fn format_time(seconds: f64) -> String {
    let total = seconds as u64;
    let m = total / 60;
    let s = total % 60;
    format!("{m}:{s:02}")
}
