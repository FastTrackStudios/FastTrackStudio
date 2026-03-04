module.exports = {
  content: [
    "./src/**/*.{rs,html,css}",
    // Signal UI components (Dialog, MiniKnob, etc.)
    "../../../crates/signal/signal-ui/src/**/*.rs",
    // Session UI components
    "../../../crates/session/session-ui/src/**/*.rs",
    // Dock layout system — sibling repo
    "../../../../dock-dioxus/crates/dock-dioxus/src/**/*.rs",
    // DAW UI and audio controls — sibling repo
    "../../../../daw/crates/daw-ui/src/**/*.rs",
    "../../../../daw/crates/audio-controls/src/**/*.rs",
    // Lumen-blocks shared components
    "../../../reference/lumen-blocks/blocks/src/**/*.rs",
  ],
  theme: {
    extend: {},
  },
  plugins: [],
};
