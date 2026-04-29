import esbuild from "esbuild";
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const prod = process.argv[2] === "production";

/**
 * Inline .wasm binaries as a Uint8Array literal so they survive Obsidian's
 * CJS require() environment without fetch() or import.meta.url.
 */
const wasmInlinePlugin = {
  name: "wasm-inline",
  setup(build) {
    build.onLoad({ filter: /\.wasm$/ }, (args) => {
      const bytes = fs.readFileSync(args.path);
      const array = Array.from(bytes);
      return {
        contents: `export default new Uint8Array([${array.join(",")}]);`,
        loader: "js",
      };
    });
  },
};

const ctx = await esbuild.context({
  entryPoints: ["main.ts"],
  bundle: true,
  external: ["obsidian"],
  format: "cjs",
  target: "es2020",
  logLevel: "info",
  sourcemap: prod ? false : "inline",
  treeShaking: true,
  outfile: "main.js",
  plugins: [wasmInlinePlugin],
});

if (prod) {
  await ctx.rebuild();
  await ctx.dispose();
} else {
  await ctx.watch();
}
