//! Dioxus desktop entry point. All UI + the vox client lifecycle live in
//! `app_ui::App`, shared verbatim with the web target.

use app_ui::App;

fn main() {
    dioxus::launch(App);
}
