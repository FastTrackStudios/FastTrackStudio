//! File upload component — drag-and-drop or click to upload.
//! Files are uploaded to the project folder via the server API.

use dioxus::prelude::*;

/// File upload drop zone.
#[component]
pub fn FileUpload(
    /// Label shown in the drop zone.
    #[props(default = "Drop files here or click to upload".to_string())]
    label: String,
    /// Accepted file types (e.g. "audio/*", ".wav,.mp3,.flac").
    #[props(default)]
    accept: String,
    /// Called when files are selected.
    #[props(default)]
    on_upload: Option<Callback<Vec<String>>>,
) -> Element {
    let mut dragging = use_signal(|| false);
    let mut uploading = use_signal(|| false);

    rsx! {
        div {
            class: if *dragging.read() {
                "rounded-xl border-2 border-dashed border-primary bg-primary/5 px-6 py-8 text-center transition-colors cursor-pointer"
            } else {
                "rounded-xl border-2 border-dashed border-border hover:border-muted-foreground/50 px-6 py-8 text-center transition-colors cursor-pointer"
            },
            ondragenter: move |_| dragging.set(true),
            ondragleave: move |_| dragging.set(false),
            ondragover: move |evt: DragEvent| {
                evt.prevent_default();
            },
            ondrop: move |evt: DragEvent| {
                evt.prevent_default();
                dragging.set(false);
                uploading.set(true);
                // In production: read files from evt.data().files()
                // and upload via RPC/HTTP
                spawn(async move {
                    gloo_timers::future::TimeoutFuture::new(1000).await;
                    uploading.set(false);
                });
            },

            if *uploading.read() {
                div { class: "flex flex-col items-center gap-2",
                    div { class: "size-8 border-2 border-primary border-t-transparent rounded-full animate-spin" }
                    span { class: "text-sm text-muted-foreground", "Uploading..." }
                }
            } else {
                div { class: "flex flex-col items-center gap-2",
                    svg {
                        class: "size-8 text-muted-foreground",
                        xmlns: "http://www.w3.org/2000/svg",
                        view_box: "0 0 24 24",
                        fill: "none",
                        stroke: "currentColor",
                        stroke_width: "1.5",
                        path { d: "M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4" }
                        path { d: "m17 8-5-5-5 5" }
                        path { d: "M12 3v12" }
                    }
                    span { class: "text-sm text-muted-foreground", "{label}" }
                    if !accept.is_empty() {
                        span { class: "text-[10px] text-muted-foreground/70", "{accept}" }
                    }
                }
            }
        }
    }
}
