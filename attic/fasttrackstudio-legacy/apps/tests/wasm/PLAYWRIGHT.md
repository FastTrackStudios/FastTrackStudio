# WASM Integration Tests

This directory contains Playwright tests that verify the WASM web app can connect to the gateway-ws and call the daw-control API.

## Prerequisites

1. **Node.js** (v18+)
2. **pnpm** or **npm**
3. **Dioxus CLI** (`cargo install dioxus-cli`)
4. **Playwright browsers**

## Setup

```bash
# Install dependencies
cd tests/playwright
pnpm install

# Install Playwright browsers (first time only)
pnpm exec playwright install chromium
```

## Running Tests

The tests will automatically:
1. Build and start the Rust host with gateway-ws
2. Start the Dioxus dev server for the WASM test app
3. Run the tests in a headless browser
4. Shut down the servers

```bash
# Run tests (headless)
pnpm test

# Run tests with browser visible
pnpm test:headed

# Debug mode (step through tests)
pnpm test:debug
```

## How It Works

1. **Host (gateway-ws)**: Rust server running at `ws://localhost:3030/ws`
2. **WASM Test App**: Dioxus app at `http://localhost:8080`
3. **Playwright**: Opens browser, navigates to test app, waits for results

The WASM test app:
- Connects to gateway-ws via `roam-websocket`
- Uses `daw-control` API (same as the real web app)
- Exposes results via `window.testResults` for Playwright to read

## Test Results

Results are exposed as:
- `window.testResults` - Array of `{ name, passed, error }`
- `window.testsComplete` - Boolean indicating tests finished
- `window.testsPassed` / `window.testsFailed` - Counts
