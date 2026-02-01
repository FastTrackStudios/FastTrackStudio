//! WASM Integration Tests
//!
//! A Dioxus app that runs integration tests in the browser against a real
//! gateway-ws server. Results are exposed to JavaScript for Playwright to read.
//!
//! Usage:
//! 1. Start the host with gateway-ws (cargo run -p fasttrackstudio)
//! 2. Build this app with dx build --release
//! 3. Run Playwright tests that load this app and check results

use daw_control::Daw;
use dioxus::prelude::*;
use roam::session::ConnectionHandle;
use roam_session::{initiate_framed, HandshakeConfig, NoDispatcher};
use roam_websocket::WsTransport;
use serde::Serialize;
use wasm_bindgen::prelude::*;

fn main() {
    tracing_wasm::set_as_global_default();
    dioxus::launch(App);
}

// ============================================================================
// Test Result Types (exposed to JavaScript)
// ============================================================================

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct TestResult {
    pub name: String,
    pub passed: bool,
    pub error: Option<String>,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct TestSuite {
    pub results: Vec<TestResult>,
    pub passed: usize,
    pub failed: usize,
    pub total: usize,
}

impl TestSuite {
    fn new() -> Self {
        Self {
            results: Vec::new(),
            passed: 0,
            failed: 0,
            total: 0,
        }
    }

    fn add(&mut self, name: impl Into<String>, passed: bool, error: Option<String>) {
        self.results.push(TestResult {
            name: name.into(),
            passed,
            error,
        });
        self.total += 1;
        if passed {
            self.passed += 1;
        } else {
            self.failed += 1;
        }
    }

    fn pass(&mut self, name: impl Into<String>) {
        self.add(name, true, None);
    }

    fn fail(&mut self, name: impl Into<String>, error: impl Into<String>) {
        self.add(name, false, Some(error.into()));
    }
}

// ============================================================================
// JavaScript Interop
// ============================================================================

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

/// Expose test results to JavaScript window object
fn expose_results_to_js(suite: &TestSuite) {
    let json = serde_json::to_string(&suite.results).unwrap_or_else(|_| "[]".to_string());

    // Set window.testResults and window.testsComplete for Playwright
    let js_code = format!(
        r#"
        window.testResults = {json};
        window.testsComplete = true;
        window.testsPassed = {passed};
        window.testsFailed = {failed};
        "#,
        json = json,
        passed = suite.passed,
        failed = suite.failed,
    );

    let _ = js_sys::eval(&js_code);
}

// ============================================================================
// Test Runner
// ============================================================================

/// Get WebSocket URL from query params or default
fn get_ws_url() -> String {
    web_sys::window()
        .and_then(|w| w.location().search().ok())
        .and_then(|search| {
            // Parse ?ws=ws://... from URL
            let params: Vec<_> = search.trim_start_matches('?').split('&').collect();
            for param in params {
                if let Some(url) = param.strip_prefix("ws=") {
                    return Some(url.to_string());
                }
            }
            None
        })
        .unwrap_or_else(|| "ws://localhost:3030/ws".to_string())
}

/// Connect to the gateway
async fn connect(ws_url: &str) -> Result<ConnectionHandle, String> {
    log(&format!("[test] Connecting to {ws_url}..."));

    let transport = WsTransport::connect(ws_url)
        .await
        .map_err(|e| format!("WebSocket connect failed: {e}"))?;

    log("[test] WebSocket connected, initiating roam handshake...");

    let (handle, _incoming, driver) =
        initiate_framed(transport, HandshakeConfig::default(), NoDispatcher)
            .await
            .map_err(|e| format!("Handshake failed: {e}"))?;

    // Spawn driver
    wasm_bindgen_futures::spawn_local(async move {
        if let Err(e) = driver.run().await {
            log(&format!("[test] Driver ended: {e}"));
        }
    });

    log("[test] Connection established!");
    Ok(handle)
}

/// Run all tests
async fn run_tests() -> TestSuite {
    let mut suite = TestSuite::new();
    let ws_url = get_ws_url();

    // Test: Connection
    let handle = match connect(&ws_url).await {
        Ok(h) => {
            suite.pass("connect");
            h
        }
        Err(e) => {
            suite.fail("connect", e);
            return suite;
        }
    };

    let daw = Daw::new(handle);

    // Test: Get current project
    log("[test] Testing get_current_project...");
    let project = match daw.current_project().await {
        Ok(p) => {
            suite.pass("get_current_project");
            log(&format!("[test] Got project: {}", p.guid()));
            Some(p)
        }
        Err(e) => {
            suite.fail("get_current_project", format!("{e}"));
            None
        }
    };

    // If we have a project, test transport controls
    if let Some(project) = project {
        let transport = project.transport();

        // Test: Play
        log("[test] Testing play...");
        match transport.play().await {
            Ok(()) => {
                suite.pass("transport_play");
                log("[test] Play succeeded");
            }
            Err(e) => {
                suite.fail("transport_play", format!("{e}"));
            }
        }

        // Test: Stop
        log("[test] Testing stop...");
        match transport.stop().await {
            Ok(()) => {
                suite.pass("transport_stop");
                log("[test] Stop succeeded");
            }
            Err(e) => {
                suite.fail("transport_stop", format!("{e}"));
            }
        }
    }

    log(&format!(
        "[test] Tests complete: {}/{} passed",
        suite.passed, suite.total
    ));

    suite
}

// ============================================================================
// Dioxus UI
// ============================================================================

#[derive(Clone, PartialEq)]
enum TestState {
    NotStarted,
    Running,
    Complete(TestSuite),
}

#[component]
fn App() -> Element {
    let mut test_state = use_signal(|| TestState::NotStarted);

    // Auto-start tests on mount
    use_effect(move || {
        spawn(async move {
            test_state.set(TestState::Running);
            let suite = run_tests().await;
            expose_results_to_js(&suite);
            test_state.set(TestState::Complete(suite));
        });
    });

    rsx! {
        div { class: "min-h-screen bg-gray-900 text-white p-8 font-mono",
            h1 { class: "text-2xl font-bold mb-6", "WASM Integration Tests" }

            match &*test_state.read() {
                TestState::NotStarted => rsx! {
                    p { class: "text-gray-400", "Initializing..." }
                },
                TestState::Running => rsx! {
                    p { class: "text-yellow-400 animate-pulse", "Running tests..." }
                },
                TestState::Complete(suite) => rsx! {
                    // Summary
                    div { class: "mb-6",
                        if suite.failed == 0 {
                            p { class: "text-green-400 text-xl",
                                "All tests passed ({suite.passed}/{suite.total})"
                            }
                        } else {
                            p { class: "text-red-400 text-xl",
                                "{suite.failed} failed, {suite.passed} passed ({suite.total} total)"
                            }
                        }
                    }

                    // Results list
                    div { class: "space-y-2",
                        for result in &suite.results {
                            div {
                                class: "flex items-center gap-3 p-2 rounded",
                                class: if result.passed { "bg-green-900/30" } else { "bg-red-900/30" },

                                // Status indicator
                                span {
                                    class: if result.passed { "text-green-400" } else { "text-red-400" },
                                    if result.passed { "[PASS]" } else { "[FAIL]" }
                                }

                                // Test name
                                span { class: "flex-1", "{result.name}" }

                                // Error message if failed
                                if let Some(error) = &result.error {
                                    span { class: "text-red-300 text-sm", "{error}" }
                                }
                            }
                        }
                    }
                },
            }
        }
    }
}
