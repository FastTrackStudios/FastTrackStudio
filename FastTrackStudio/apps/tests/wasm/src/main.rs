//! WASM Integration Tests
//!
//! A Dioxus app that runs integration tests in the browser against a real
//! gateway-ws server. Results are exposed to JavaScript for Playwright to read.
//!
//! Usage:
//! 1. Start the desktop app, which runs the gateway (cargo run -p fasttrackstudio-desktop)
//! 2. Build this app with dx build --release
//! 3. Run Playwright tests that load this app and check results
//!
//! Note: This crate is WASM-only. On native targets, it compiles to an empty binary.

#![cfg_attr(not(target_arch = "wasm32"), allow(unused))]

#[cfg(target_arch = "wasm32")]
use daw::Daw;
#[cfg(target_arch = "wasm32")]
use dioxus::prelude::*;
#[cfg(target_arch = "wasm32")]
use vox::session::ConnectionHandle;
#[cfg(target_arch = "wasm32")]
use vox_session::{initiate_framed, HandshakeConfig, NoDispatcher};
#[cfg(target_arch = "wasm32")]
use vox_websocket::WsTransport;
#[cfg(target_arch = "wasm32")]
use serde::Serialize;
#[cfg(target_arch = "wasm32")]
use session::SetlistServiceClient;
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

fn main() {
    #[cfg(target_arch = "wasm32")]
    {
        tracing_wasm::set_as_global_default();
        dioxus::launch(App);
    }

    #[cfg(not(target_arch = "wasm32"))]
    {
        eprintln!("This binary only runs on wasm32. Use `dx build` to compile for WASM.");
    }
}

// ============================================================================
// Everything below only compiles for WASM
// ============================================================================

#[cfg(target_arch = "wasm32")]
mod wasm {
    use super::*;

    // ========================================================================
    // Test Result Types (exposed to JavaScript)
    // ========================================================================

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
        pub fn new() -> Self {
            Self {
                results: Vec::new(),
                passed: 0,
                failed: 0,
                total: 0,
            }
        }

        pub fn add(&mut self, name: impl Into<String>, passed: bool, error: Option<String>) {
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

        pub fn pass(&mut self, name: impl Into<String>) {
            self.add(name, true, None);
        }

        pub fn fail(&mut self, name: impl Into<String>, error: impl Into<String>) {
            self.add(name, false, Some(error.into()));
        }
    }

    // ========================================================================
    // JavaScript Interop
    // ========================================================================

    #[wasm_bindgen]
    extern "C" {
        #[wasm_bindgen(js_namespace = console)]
        pub fn log(s: &str);
    }

    /// Expose test results to JavaScript window object
    pub fn expose_results_to_js(suite: &TestSuite) {
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

    // ========================================================================
    // Test Helpers
    // ========================================================================

    /// Small delay for async operations
    pub async fn delay_ms(ms: u32) {
        gloo_timers::future::TimeoutFuture::new(ms).await;
    }

    // ========================================================================
    // Test Runner
    // ========================================================================

    /// Get WebSocket URL from query params or default
    pub fn get_ws_url() -> String {
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
    pub async fn connect(ws_url: &str) -> Result<ConnectionHandle, String> {
        log(&format!("[test] Connecting to {ws_url}..."));

        let transport = WsTransport::connect(ws_url)
            .await
            .map_err(|e| format!("WebSocket connect failed: {e}"))?;

        log("[test] WebSocket connected, initiating vox handshake...");

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
    pub async fn run_tests() -> TestSuite {
        let mut suite = TestSuite::new();
        let ws_url = get_ws_url();

        // ====================================================================
        // Test: Connection
        // ====================================================================
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

        // ====================================================================
        // DAW Control Tests
        // ====================================================================
        let daw = Daw::new(handle.clone());

        // Test: Get current project
        log("[test] Testing get_current_project...");
        let project = match daw.current_project().await {
            Ok(p) => {
                suite.pass("daw: get_current_project");
                log(&format!("[test] Got project: {}", p.guid()));
                Some(p)
            }
            Err(e) => {
                suite.fail("daw: get_current_project", format!("{e}"));
                None
            }
        };

        // If we have a project, test transport controls
        if let Some(project) = project {
            let transport = project.transport();

            // Test: Get transport state
            log("[test] Testing get_state...");
            match transport.get_state().await {
                Ok(state) => {
                    suite.pass("daw: get_transport_state");
                    log(&format!(
                        "[test] Transport state: playing={:?}, bpm={:.1}",
                        state.play_state, state.tempo.bpm
                    ));
                }
                Err(e) => {
                    suite.fail("daw: get_transport_state", format!("{e}"));
                }
            }

            // Test: Play
            log("[test] Testing play...");
            match transport.play().await {
                Ok(()) => {
                    suite.pass("daw: transport_play");
                    log("[test] Play succeeded");
                }
                Err(e) => {
                    suite.fail("daw: transport_play", format!("{e}"));
                }
            }

            // Small delay to let playback start
            delay_ms(100).await;

            // Test: Pause
            log("[test] Testing pause...");
            match transport.pause().await {
                Ok(()) => {
                    suite.pass("daw: transport_pause");
                    log("[test] Pause succeeded");
                }
                Err(e) => {
                    suite.fail("daw: transport_pause", format!("{e}"));
                }
            }

            // Test: Stop
            log("[test] Testing stop...");
            match transport.stop().await {
                Ok(()) => {
                    suite.pass("daw: transport_stop");
                    log("[test] Stop succeeded");
                }
                Err(e) => {
                    suite.fail("daw: transport_stop", format!("{e}"));
                }
            }

            // Test: Set position
            log("[test] Testing set_position...");
            match transport.set_position(10.0).await {
                Ok(()) => {
                    suite.pass("daw: set_position");
                    log("[test] Set position to 10s succeeded");
                }
                Err(e) => {
                    suite.fail("daw: set_position", format!("{e}"));
                }
            }
        }

        // ====================================================================
        // SetlistService Tests
        // ====================================================================
        log("[test] ─── SetlistService Tests ───");

        let setlist_client = SetlistServiceClient::new(handle.clone());

        // Test: Build setlist from open projects
        log("[test] Testing build_from_open_projects...");
        match setlist_client.build_from_open_projects().await {
            Ok(()) => {
                suite.pass("setlist: build_from_open_projects");
                log("[test] Build setlist succeeded");
            }
            Err(e) => {
                suite.fail("setlist: build_from_open_projects", format!("{e}"));
            }
        }

        // Small delay to let setlist build
        delay_ms(200).await;

        // Test: Get setlist
        log("[test] Testing get_setlist...");
        match setlist_client.get_setlist().await {
            Ok(setlist_opt) => {
                if let Some(setlist) = setlist_opt {
                    suite.pass("setlist: get_setlist");
                    log(&format!(
                        "[test] Got setlist: {} ({} songs)",
                        setlist.name, setlist.song_count
                    ));
                } else {
                    // No setlist is okay for standalone - might not have projects
                    suite.pass("setlist: get_setlist (empty)");
                    log("[test] No setlist (expected for standalone with no projects)");
                }
            }
            Err(e) => {
                suite.fail("setlist: get_setlist", format!("{e}"));
            }
        }

        // Test: Get songs
        log("[test] Testing get_songs...");
        match setlist_client.get_songs().await {
            Ok(songs) => {
                suite.pass("setlist: get_songs");
                log(&format!("[test] Got {} songs", songs.len()));
                for (i, song) in songs.iter().enumerate() {
                    log(&format!(
                        "[test]   {}: {} ({:.1}s)",
                        i, song.name, song.duration
                    ));
                }
            }
            Err(e) => {
                suite.fail("setlist: get_songs", format!("{e}"));
            }
        }

        // Test: Get active song
        log("[test] Testing get_active_song...");
        match setlist_client.get_active_song().await {
            Ok(song_opt) => {
                suite.pass("setlist: get_active_song");
                if let Some(song) = song_opt {
                    log(&format!("[test] Active song: {}", song.name));
                } else {
                    log("[test] No active song");
                }
            }
            Err(e) => {
                suite.fail("setlist: get_active_song", format!("{e}"));
            }
        }

        // Test: Navigation - go to song 0
        log("[test] Testing go_to_song(0)...");
        match setlist_client.go_to_song(0).await {
            Ok(()) => {
                suite.pass("setlist: go_to_song");
                log("[test] Navigated to song 0");
            }
            Err(e) => {
                suite.fail("setlist: go_to_song", format!("{e}"));
            }
        }

        delay_ms(100).await;

        // Test: Navigation - go to section 0
        log("[test] Testing go_to_section(0)...");
        match setlist_client.go_to_section(0).await {
            Ok(()) => {
                suite.pass("setlist: go_to_section");
                log("[test] Navigated to section 0");
            }
            Err(e) => {
                suite.fail("setlist: go_to_section", format!("{e}"));
            }
        }

        delay_ms(100).await;

        // Test: Navigation - next section
        log("[test] Testing next_section...");
        match setlist_client.next_section().await {
            Ok(()) => {
                suite.pass("setlist: next_section");
                log("[test] Moved to next section");
            }
            Err(e) => {
                suite.fail("setlist: next_section", format!("{e}"));
            }
        }

        delay_ms(100).await;

        // Test: Navigation - previous section
        log("[test] Testing previous_section...");
        match setlist_client.previous_section().await {
            Ok(()) => {
                suite.pass("setlist: previous_section");
                log("[test] Moved to previous section");
            }
            Err(e) => {
                suite.fail("setlist: previous_section", format!("{e}"));
            }
        }

        delay_ms(100).await;

        // Test: Playback - play via setlist
        log("[test] Testing setlist play...");
        match setlist_client.play().await {
            Ok(()) => {
                suite.pass("setlist: play");
                log("[test] Setlist play succeeded");
            }
            Err(e) => {
                suite.fail("setlist: play", format!("{e}"));
            }
        }

        delay_ms(100).await;

        // Test: Playback - pause via setlist
        log("[test] Testing setlist pause...");
        match setlist_client.pause().await {
            Ok(()) => {
                suite.pass("setlist: pause");
                log("[test] Setlist pause succeeded");
            }
            Err(e) => {
                suite.fail("setlist: pause", format!("{e}"));
            }
        }

        delay_ms(100).await;

        // Test: Playback - toggle playback
        log("[test] Testing toggle_playback...");
        match setlist_client.toggle_playback().await {
            Ok(()) => {
                suite.pass("setlist: toggle_playback");
                log("[test] Toggle playback succeeded");
            }
            Err(e) => {
                suite.fail("setlist: toggle_playback", format!("{e}"));
            }
        }

        delay_ms(100).await;

        // Test: Stop via setlist
        log("[test] Testing setlist stop...");
        match setlist_client.stop().await {
            Ok(()) => {
                suite.pass("setlist: stop");
                log("[test] Setlist stop succeeded");
            }
            Err(e) => {
                suite.fail("setlist: stop", format!("{e}"));
            }
        }

        // Test: Seek to specific time
        log("[test] Testing seek_to(5.0)...");
        match setlist_client.seek_to(5.0).await {
            Ok(()) => {
                suite.pass("setlist: seek_to");
                log("[test] Seek to 5s succeeded");
            }
            Err(e) => {
                suite.fail("setlist: seek_to", format!("{e}"));
            }
        }

        // Test: Seek to song and section
        log("[test] Testing seek_to_section(0, 0)...");
        match setlist_client.seek_to_section(0, 0).await {
            Ok(()) => {
                suite.pass("setlist: seek_to_section");
                log("[test] Seek to section succeeded");
            }
            Err(e) => {
                suite.fail("setlist: seek_to_section", format!("{e}"));
            }
        }

        // Test: Get sections for song 0
        log("[test] Testing get_sections(0)...");
        match setlist_client.get_sections(0).await {
            Ok(sections) => {
                suite.pass("setlist: get_sections");
                log(&format!(
                    "[test] Got {} sections for song 0",
                    sections.len()
                ));
                for (i, section) in sections.iter().enumerate() {
                    log(&format!(
                        "[test]   {}: {} ({:.1}s - {:.1}s)",
                        i, section.name, section.start, section.end
                    ));
                }
            }
            Err(e) => {
                suite.fail("setlist: get_sections", format!("{e}"));
            }
        }

        // Test: Loop toggle
        log("[test] Testing toggle_song_loop...");
        match setlist_client.toggle_song_loop().await {
            Ok(()) => {
                suite.pass("setlist: toggle_song_loop");
                log("[test] Toggle song loop succeeded");
            }
            Err(e) => {
                suite.fail("setlist: toggle_song_loop", format!("{e}"));
            }
        }

        // Turn loop off again
        delay_ms(100).await;
        let _ = setlist_client.toggle_song_loop().await;

        // ====================================================================
        // Summary
        // ====================================================================
        log(&format!(
            "[test] ═══════════════════════════════════════════════════"
        ));
        log(&format!(
            "[test] Tests complete: {}/{} passed",
            suite.passed, suite.total
        ));
        if suite.failed > 0 {
            log(&format!("[test] FAILURES: {}", suite.failed));
        }

        suite
    }
}

// ============================================================================
// Dioxus UI (WASM only)
// ============================================================================

#[cfg(target_arch = "wasm32")]
use wasm::*;

#[cfg(target_arch = "wasm32")]
#[derive(Clone, PartialEq)]
enum TestState {
    NotStarted,
    Running,
    Complete(TestSuite),
}

#[cfg(target_arch = "wasm32")]
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

                    // Results list - grouped by category
                    div { class: "space-y-4",
                        // DAW tests
                        div {
                            h2 { class: "text-lg font-semibold text-blue-400 mb-2", "DAW Control" }
                            div { class: "space-y-1 ml-4",
                                for result in suite.results.iter().filter(|r| r.name.starts_with("daw:") || r.name == "connect") {
                                    TestResultItem { result: result.clone() }
                                }
                            }
                        }

                        // Setlist tests
                        div {
                            h2 { class: "text-lg font-semibold text-purple-400 mb-2", "SetlistService" }
                            div { class: "space-y-1 ml-4",
                                for result in suite.results.iter().filter(|r| r.name.starts_with("setlist:")) {
                                    TestResultItem { result: result.clone() }
                                }
                            }
                        }
                    }
                },
            }
        }
    }
}

#[cfg(target_arch = "wasm32")]
#[component]
fn TestResultItem(result: TestResult) -> Element {
    rsx! {
        div {
            class: "flex items-center gap-3 p-2 rounded",
            class: if result.passed { "bg-green-900/30" } else { "bg-red-900/30" },

            // Status indicator
            span {
                class: if result.passed { "text-green-400" } else { "text-red-400" },
                if result.passed { "[PASS]" } else { "[FAIL]" }
            }

            // Test name (remove prefix for cleaner display)
            span { class: "flex-1",
                {result.name.split(": ").last().unwrap_or(&result.name)}
            }

            // Error message if failed
            if let Some(error) = &result.error {
                span { class: "text-red-300 text-sm max-w-md truncate", "{error}" }
            }
        }
    }
}
