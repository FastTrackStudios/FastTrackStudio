use dioxus::desktop::{Config, tao::window::WindowBuilder};
use dioxus::prelude::*;
use task_ui::App;

const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");

fn main() {
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
        App {}
    }
}
