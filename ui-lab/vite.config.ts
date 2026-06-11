import { fileURLToPath, URL } from "node:url";
import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import tailwindcss from "@tailwindcss/vite";

// The app connects its vox WebSocket same-origin (ws://localhost:5173/
// org/<slug>/vox) and vite proxies it to the task-server. Same-origin
// matters: Chrome's Local Network Access checks (M138+) gate
// cross-origin ws:// to loopback behind a permission prompt — silently
// stalling the socket in headless runs — while same-origin loopback is
// exempt. Override the target with TASK_SERVER_HTTP when the server
// isn't on 18080.
const TASK_SERVER = process.env.TASK_SERVER_HTTP ?? "http://127.0.0.1:18080";

const voxProxy = {
  "/org": {
    target: TASK_SERVER,
    ws: true,
    changeOrigin: true,
  },
} as const;

export default defineConfig({
  plugins: [react(), tailwindcss()],
  resolve: {
    alias: {
      "@": fileURLToPath(new URL("./src", import.meta.url)),
    },
  },
  server: { proxy: voxProxy },
  preview: { proxy: voxProxy },
});
