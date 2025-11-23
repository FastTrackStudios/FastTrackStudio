use dioxus::prelude::*;
use transport::{Transport };

pub static TRANSPORT: GlobalSignal<Transport> = Signal::global(|| Transport::new());

#[component]
pub fn TransportDisplay() -> Element {

    let transport = TRANSPORT.read();

    let bpm = transport.tempo;

    rsx! {
        div {
            p { " {transport} {bpm}" }
        }
    }
}
