import { test, expect } from "@playwright/test";
import { spawn, type ChildProcess } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";

// Project root (apps/tests/wasm/playwright-tests -> root)
const projectRoot = new URL("../../../..", import.meta.url).pathname;

let hostServer: ChildProcess | null = null;
let dioxusServer: ChildProcess | null = null;

// Use random ports to avoid conflicts with other processes
const HOST_PORT = 13030 + Math.floor(Math.random() * 1000); // gateway-ws port
const DIOXUS_PORT = 18080 + Math.floor(Math.random() * 1000); // Dioxus dev server port

/**
 * Wait for a port to become available
 */
async function waitForPort(
  port: number,
  timeoutMs: number = 30000,
): Promise<void> {
  const start = Date.now();
  while (Date.now() - start < timeoutMs) {
    try {
      const response = await fetch(`http://localhost:${port}/`);
      // Any response (even 404) means server is up
      if (response.ok || response.status === 404 || response.status === 101) {
        return;
      }
    } catch {
      // Connection refused, try again
    }
    await sleep(200);
  }
  throw new Error(`Timeout waiting for port ${port}`);
}

/**
 * Wait for server output to contain a specific string
 */
function waitForOutput(
  proc: ChildProcess,
  match: string | RegExp,
  timeoutMs: number = 30000,
): Promise<void> {
  return new Promise((resolve, reject) => {
    const timeout = setTimeout(() => {
      reject(new Error(`Timeout waiting for output: ${match}`));
    }, timeoutMs);

    const check = (data: Buffer) => {
      const line = data.toString();
      const matches =
        typeof match === "string" ? line.includes(match) : match.test(line);
      if (matches) {
        clearTimeout(timeout);
        resolve();
      }
    };

    proc.stdout?.on("data", check);
    proc.stderr?.on("data", check);
  });
}

test.beforeAll(async () => {
  console.log("Starting Rust host with gateway-ws...");
  console.log(`Project root: ${projectRoot}`);

  // Start the Rust host (includes gateway-ws)
  hostServer = spawn("cargo", ["run", "-p", "test-extension", "--release"], {
    cwd: projectRoot,
    env: {
      ...process.env,
      RUST_LOG: "info",
      GATEWAY_WS_ADDR: `0.0.0.0:${HOST_PORT}`,
    },
    stdio: ["ignore", "pipe", "pipe"],
  });

  hostServer.stdout?.on("data", (data: Buffer) => {
    console.log(`[host stdout] ${data.toString().trim()}`);
  });

  hostServer.stderr?.on("data", (data: Buffer) => {
    console.log(`[host stderr] ${data.toString().trim()}`);
  });

  hostServer.on("error", (err) => {
    console.error(`Host server error: ${err}`);
  });

  hostServer.on("exit", (code) => {
    console.log(`Host server exited with code ${code}`);
  });

  // Wait for host to signal it's fully ready (all cells spawned)
  console.log(
    `Waiting for test-extension to start (gateway-ws on port ${HOST_PORT})...`,
  );
  await waitForOutput(
    hostServer,
    "Test Extension running with DAW, Session, and Gateway-WS cells",
    120000,
  );
  console.log("Test extension is ready!");

  // Start Dioxus dev server for wasm-tests app
  console.log("Starting Dioxus dev server for WASM tests...");
  dioxusServer = spawn("dx", ["serve", "--port", String(DIOXUS_PORT)], {
    cwd: `${projectRoot}apps/tests/wasm`,
    stdio: ["ignore", "pipe", "pipe"],
  });

  dioxusServer.stdout?.on("data", (data: Buffer) => {
    console.log(`[dioxus stdout] ${data.toString().trim()}`);
  });

  dioxusServer.stderr?.on("data", (data: Buffer) => {
    console.log(`[dioxus stderr] ${data.toString().trim()}`);
  });

  dioxusServer.on("error", (err) => {
    console.error(`Dioxus server error: ${err}`);
  });

  // Wait for Dioxus to be ready
  console.log(`Waiting for Dioxus on port ${DIOXUS_PORT}...`);
  await waitForPort(DIOXUS_PORT, 60000);
  console.log("Dioxus dev server is ready!");
});

test.afterAll(async () => {
  console.log("Shutting down servers...");

  if (dioxusServer) {
    dioxusServer.kill("SIGTERM");
    dioxusServer = null;
  }

  if (hostServer) {
    hostServer.kill("SIGTERM");
    hostServer = null;
  }

  // Give processes time to clean up
  await sleep(500);
});

test("WASM client can connect to gateway-ws and call daw-control API", async ({
  page,
}) => {
  // Capture browser console
  page.on("console", (msg) => {
    const text = msg.text();
    // Filter out noisy messages
    if (!text.includes("[vite]") && !text.includes("WebSocket")) {
      console.log(`[browser ${msg.type()}] ${text}`);
    }
  });

  // Navigate to test app with WebSocket URL
  // Note: Don't URL-encode the ws parameter - the Rust code reads it directly
  const wsUrl = `ws://localhost:${HOST_PORT}/ws`;
  await page.goto(`http://localhost:${DIOXUS_PORT}/?ws=${wsUrl}`);

  // Wait for tests to complete (exposed via window.testsComplete)
  console.log("Waiting for WASM tests to complete...");
  await page.waitForFunction(() => (window as any).testsComplete === true, {
    timeout: 30000,
  });

  // Get test results from window object
  const results = await page.evaluate(() => (window as any).testResults);
  const passed = await page.evaluate(() => (window as any).testsPassed);
  const failed = await page.evaluate(() => (window as any).testsFailed);

  console.log(`\nWASM Test Results: ${passed} passed, ${failed} failed`);
  console.log("─".repeat(50));

  for (const result of results) {
    const status = result.passed ? "PASS" : "FAIL";
    const error = result.error ? ` - ${result.error}` : "";
    console.log(`  [${status}] ${result.name}${error}`);
  }

  console.log("─".repeat(50));

  // Assert all tests passed
  expect(results).toBeInstanceOf(Array);
  expect(results.length).toBeGreaterThan(0);

  for (const result of results) {
    expect(result.passed, `Test "${result.name}" failed: ${result.error}`).toBe(
      true,
    );
  }
});
