module.exports = {
  content: [
    "./src/**/*.{rs,html,css}",
    // Session UI components
    "../../../crates/session/session-ui/src/**/*.rs",
    // Keyflow UI (chart editor) — sibling repo
    "../../../../keyflow/crates/keyflow-ui/src/**/*.rs",
    // Dock layout system — sibling repo
    "../../../../dock-dioxus/crates/dock-dioxus/src/**/*.rs",
    // FTS design system — sibling repo (at Development/fts-ui)
    "../../../../../fts-ui/crates/fts-ui/src/**/*.rs",
    // Lumen-blocks shared components
    "../../../reference/lumen-blocks/blocks/src/**/*.rs",
  ],
  theme: {
    extend: {},
  },
  plugins: [],
};
