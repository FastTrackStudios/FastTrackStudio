module.exports = {
  content: [
    "./src/**/*.{rs,html,css}",
    // Include session-proto UI components
    "../../../cells/session/session-ui/src/**/*.rs",
    // Include keyflow UI components (chart editor)
    "../../../cells/keyflow/keyflow-ui/src/**/*.rs",
    // Include signal rig control UI and audio control widgets
    "../../../cells/signal/signal-ui/src/**/*.rs",
    "../../../cells/dock/dock-dioxus/src/**/*.rs",
    "../../../cells/daw/daw-ui/src/**/*.rs",
    "../../../cells/signal/audio-controls/src/**/*.rs",
  ],
  theme: {
    extend: {},
  },
  plugins: [],
};
