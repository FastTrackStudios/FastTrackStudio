//! Headless smoke test for the shared app shell.
//!
//! The web and desktop binaries are ~3 lines each (`dioxus::launch(App)`);
//! the real surface is `app_ui::App` — the router, the connection
//! lifecycle, the screens. A windowed end-to-end run isn't CI-friendly,
//! so here we mount the whole thing in a VirtualDom and render the
//! initial frame. That proves the route table, layout, and the
//! pre-connection state all build and render without a server present
//! (it should show the shell + the "connecting" banner).

use app_ui::App;
use dioxus::prelude::*;

#[test]
fn app_mounts_and_renders_initial_frame() {
    let mut dom = VirtualDom::new(App);
    dom.rebuild_in_place();
    let html = dioxus_ssr::render(&dom);

    // Shell chrome from AppShell.
    assert!(html.contains("architect"), "brand missing: {html}");
    assert!(
        html.contains("reference example"),
        "tagline missing: {html}"
    );
    // Home is the index route; before the socket is up it shows the
    // connecting banner (no server in this test).
    assert!(
        html.to_lowercase().contains("connecting"),
        "expected connecting banner on the index route: {html}"
    );
}
