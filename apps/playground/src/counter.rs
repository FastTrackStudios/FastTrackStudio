
use std::time::Duration;

use dioxus::prelude::*;
use lumen_blocks::components::button::{Button, ButtonSize, ButtonVariant};
use tracing::info;

pub static COUNT: GlobalSignal<u128> = Signal::global(|| 0);

pub fn start_auto_increment() {
    spawn(async move {
        loop {
            // Use wasm-timer which works on both desktop and WASM
            let _ = wasm_timer::Delay::new(Duration::from_millis(100)).await;

            // Increment first, then log so the log matches what the button displays
            *COUNT.write() += 1;
            // info!("Auto-incrementing counter: {}", COUNT());
        }
    });
}

#[component]
pub fn CounterButton() -> Element {
    rsx! {
        div {
            Button {
                variant: ButtonVariant::Primary,
                size: ButtonSize::Large,
                on_click: move |_| *COUNT.write() += 1,
                "Count = {COUNT}"
            }
        }
    }
}
