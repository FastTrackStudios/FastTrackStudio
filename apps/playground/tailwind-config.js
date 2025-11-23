module.exports = {
  content: [
    "./src/**/*.{rs,html,css}",
    // Include UI components
    "../../modules/ui/src/**/*.rs",
    // Include Lumen Blocks components (used by UI module)
    // Note: The `2675507` on the path is there to match the Lumen Blocks version. If you update Lumen Blocks, update this path with the first 7 digits of the commit hash.
    `${process.env.HOME}/.cargo/git/checkouts/lumen-blocks-*/2675507/blocks/src/**/*.rs`
  ],
  theme: {
    extend: {},
  },
  plugins: [],
};

