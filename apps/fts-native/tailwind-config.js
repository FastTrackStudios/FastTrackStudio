module.exports = {
  content: [
    "./src/**/*.{rs,html,css}",
    // Include Lumen Blocks components from local submodule
    "./libs/lumen-blocks/blocks/src/**/*.rs"
  ],
  theme: {
    extend: {},
  },
  plugins: [],
};
