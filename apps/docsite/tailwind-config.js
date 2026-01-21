module.exports = {
  content: [
    "./src/**/*.{rs,html,css}",
    // Include Lumen Blocks components
    // Note: The `2675507` on the path matches the Lumen Blocks v0.2.0 tag commit hash.
    // If you update Lumen Blocks, update this path with the first 7 digits of the new commit hash.
    `${process.env.HOME}/.cargo/git/checkouts/lumen-blocks-*/2675507/blocks/src/**/*.rs`
  ],
  theme: {
    extend: {},
  },
  plugins: [],
};
