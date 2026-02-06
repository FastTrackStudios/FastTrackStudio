module.exports = {
  content: [
    "./src/**/*.{rs,html,css}",
    // Include session-proto UI components
    "../../../cells/session/session-ui/src/**/*.rs",
    // Include keyflow UI components (chart editor)
    "../../../cells/keyflow/keyflow-ui/src/**/*.rs",
  ],
  theme: {
    extend: {},
  },
  plugins: [],
};
