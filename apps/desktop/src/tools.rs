//! Tools page — file-format utilities.
//!
//! Currently: convert a Pro Tools session (.ptx / .ptf / .pts) into a Reaper
//! project (.rpp) via `daw::reaper::project_import`. The conversion is pure
//! (read file → build RPP text), so it runs on a blocking thread to keep the
//! UI responsive. The reverse (RPP → PTX) is not offered: Pro Tools' session
//! format is proprietary and decode-only.

use std::path::PathBuf;

use dioxus::prelude::*;

#[derive(Clone, PartialEq)]
enum Status {
    Idle,
    Converting,
    Done(String),
    Error(String),
}

#[component]
pub fn ToolsPage() -> Element {
    let mut input_path: Signal<Option<PathBuf>> = use_signal(|| None);
    let mut status = use_signal(|| Status::Idle);

    let pick = move |_| {
        spawn(async move {
            if let Some(handle) = rfd::AsyncFileDialog::new()
                .add_filter("Pro Tools session", &["ptx", "ptf", "pts"])
                .pick_file()
                .await
            {
                input_path.set(Some(handle.path().to_path_buf()));
                status.set(Status::Idle);
            }
        });
    };

    let convert = move |_| {
        let Some(path) = input_path() else {
            return;
        };
        status.set(Status::Converting);
        spawn(async move {
            let out = path.with_extension("rpp");
            let path_str = path.to_string_lossy().to_string();
            // `protools_to_rpp` is synchronous file + CPU work — run it off the
            // UI thread so the window stays responsive on large sessions.
            let result = tokio::task::spawn_blocking(move || {
                daw::reaper::project_import::protools_to_rpp(&path_str).map_err(|e| e.to_string())
            })
            .await;
            match result {
                Ok(Ok(rpp_text)) => match std::fs::write(&out, rpp_text) {
                    Ok(()) => status.set(Status::Done(out.to_string_lossy().to_string())),
                    Err(e) => status.set(Status::Error(format!("write {}: {e}", out.display()))),
                },
                Ok(Err(e)) => status.set(Status::Error(e)),
                Err(e) => status.set(Status::Error(format!("conversion task panicked: {e}"))),
            }
        });
    };

    let selected_label = match input_path() {
        Some(p) => p
            .file_name()
            .map(|n| n.to_string_lossy().to_string())
            .unwrap_or_else(|| p.to_string_lossy().to_string()),
        None => "No file selected".to_string(),
    };
    let converting = status() == Status::Converting;

    rsx! {
        div { class: "p-6 max-w-2xl mx-auto flex flex-col gap-4",
            h1 { class: "text-2xl font-bold", "Tools" }
            div { class: "border border-neutral-300 rounded-lg p-4 flex flex-col gap-3",
                h2 { class: "text-lg font-semibold", "Pro Tools → Reaper" }
                p { class: "text-sm opacity-70",
                    "Convert a Pro Tools session (.ptx / .ptf / .pts) into a Reaper project (.rpp). The .rpp is written next to the source file."
                }
                div { class: "flex items-center gap-3",
                    button {
                        class: "px-3 py-2 rounded bg-neutral-200 hover:bg-neutral-300 text-sm",
                        onclick: pick,
                        "Choose .ptx file…"
                    }
                    span { class: "text-sm truncate opacity-80", "{selected_label}" }
                }
                button {
                    class: "px-3 py-2 rounded bg-blue-600 text-white text-sm disabled:opacity-50 w-fit",
                    disabled: input_path().is_none() || converting,
                    onclick: convert,
                    {if converting { "Converting…" } else { "Convert to .rpp" }}
                }
                {match status() {
                    Status::Done(path) => rsx! { p { class: "text-sm text-green-600", "✓ Saved: {path}" } },
                    Status::Error(e) => rsx! { p { class: "text-sm text-red-600", "✗ {e}" } },
                    _ => rsx! {}
                }}
            }
        }
    }
}
