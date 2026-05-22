use dioxus::prelude::*;
use task_ui::App;

const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");
const MANIFEST: Asset = asset!("/assets/manifest.json");
const SERVICE_WORKER: Asset = asset!("/assets/sw.js");

fn main() {
    dioxus::launch(Root);
}

#[component]
fn Root() -> Element {
    // Register the service worker once at app boot. The asset!()
    // macro fingerprints the URL so the SW path includes a hash;
    // we pass it through `document::eval` rather than a literal
    // string in the JS so each rebuild registers the right file.
    //
    // Scope note: the SW lives under `/assets/` so its default
    // scope is `/assets/` — that's enough to cache the CSS, JS
    // chunks, and wasm bundle (the bulk of the offline payload).
    // To widen scope to `/` (so the SPA root and arbitrary routes
    // also serve from cache), the server must send the
    // `Service-Worker-Allowed: /` HTTP header on the SW response.
    // Most prod deploys are one-line nginx/caddy snippets; the
    // dev server may not support it, in which case PWA install
    // still works but offline-page-load is limited to the asset
    // dir.
    use_hook(|| {
        let sw_url = SERVICE_WORKER.to_string();
        spawn(async move {
            let js = format!(
                r#"if ('serviceWorker' in navigator) {{
                    navigator.serviceWorker.register('{sw_url}')
                        .then(r => console.log('sw registered', r.scope))
                        .catch(e => console.warn('sw register failed', e));
                }}"#
            );
            let _ = document::eval(&js).await;
        });
    });
    rsx! {
        document::Stylesheet { href: TAILWIND_CSS }
        document::Link {
            rel: "manifest",
            href: MANIFEST,
        }
        document::Meta {
            name: "theme-color",
            content: "#0a0a0a",
        }
        document::Meta {
            name: "viewport",
            content: "width=device-width, initial-scale=1, viewport-fit=cover",
        }
        document::Meta {
            name: "apple-mobile-web-app-capable",
            content: "yes",
        }
        document::Meta {
            name: "apple-mobile-web-app-status-bar-style",
            content: "black-translucent",
        }
        App {}
    }
}
