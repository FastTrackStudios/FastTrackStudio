use dioxus::desktop::{Config, tao::window::WindowBuilder};
use dioxus::prelude::*;
use ui::App;

const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");

fn main() {
    // Error/crash telemetry — hold `_sentry` for the life of `main`
    // (`launch` diverges, so binding it here is sufficient). The tracing
    // subscriber carries the Sentry layer so `warn!`/`error!` events are
    // captured; `.try_init()` (inside init_tracing) makes a later dioxus
    // subscriber-init a no-op rather than a panic.
    let _sentry = task_telemetry::init_tracing("task-desktop", "info");

    let cfg = Config::new()
        .with_window(
            WindowBuilder::new()
                .with_title("Task")
                .with_inner_size(dioxus::desktop::tao::dpi::LogicalSize::new(1280.0, 800.0)),
        )
        .with_menu(None);
    LaunchBuilder::desktop().with_cfg(cfg).launch(Root);
}

#[component]
fn Root() -> Element {
    rsx! {
        document::Stylesheet { href: TAILWIND_CSS }
        // KaTeX (CDN) — upgrades `.math-inline` / `.math-block`
        // spans emitted by the inline parser into rendered math.
        // Auto-render extension scans the DOM for `$…$` and
        // `$$…$$` *text* delimiters at load time; we emit the raw
        // source inside the math spans so this Just Works.
        document::Stylesheet {
            href: "https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.css",
        }
        document::Script {
            src: "https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.js",
            defer: true,
        }
        document::Script {
            src: "https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/contrib/auto-render.min.js",
            defer: true,
        }
        // After auto-render loads, scan the body for math delimiters
        // and upgrade them in place. Re-runs every 2s so blocks edited
        // after load also pick up rendering — cheap because KaTeX
        // skips nodes it has already rendered.
        document::Script {
            r#"
            window.addEventListener('load', function() {{
              function render() {{
                if (window.renderMathInElement) {{
                  renderMathInElement(document.body, {{
                    delimiters: [
                      {{left: '$$', right: '$$', display: true}},
                      {{left: '$', right: '$', display: false}}
                    ],
                    throwOnError: false
                  }});
                }}
              }}
              render();
              setInterval(render, 2000);
            }});
            "#
        }
        // Global click delegate for `.pdf-macro` chips — open the
        // referenced PDF in the dedicated reader view. We can't
        // mutate Dioxus signals from raw JS, so we re-emit the URL
        // as a custom event the Rust side listens for via the
        // shell's onmounted hook. The event payload is the data-
        // attribute string verbatim.
        document::Script {
            r#"
            document.addEventListener('click', function(e) {{
              const btn = e.target.closest('.pdf-macro[data-pdf-url]');
              if (!btn) return;
              e.preventDefault();
              const url = btn.getAttribute('data-pdf-url');
              window.dispatchEvent(new CustomEvent('task:open-pdf', {{ detail: url }}));
            }});
            "#
        }
        // Global click delegate for `.video-timestamp` chips. When
        // a chip is clicked, walk up to the nearest block and seek
        // the first <video> or YouTube iframe to the chip's
        // data-ts-seconds value.
        document::Script {
            r#"
            document.addEventListener('click', function(e) {{
              const btn = e.target.closest('.video-timestamp[data-ts-seconds]');
              if (!btn) return;
              e.preventDefault();
              const secs = parseInt(btn.getAttribute('data-ts-seconds'), 10);
              if (Number.isNaN(secs)) return;
              const host = btn.closest('[data-block-id]') || document.body;
              const v = host.querySelector('video');
              if (v) {{ v.currentTime = secs; v.play && v.play(); return; }}
              const yt = host.querySelector('iframe.media-youtube, iframe[src*="youtube.com/embed"]');
              if (yt) {{
                const url = new URL(yt.src);
                url.searchParams.set('start', secs);
                url.searchParams.set('autoplay', '1');
                yt.src = url.toString();
                return;
              }}
              const fallback = document.querySelector('video');
              if (fallback) {{ fallback.currentTime = secs; fallback.play && fallback.play(); }}
            }});
            "#
        }
        App {}
    }
}
