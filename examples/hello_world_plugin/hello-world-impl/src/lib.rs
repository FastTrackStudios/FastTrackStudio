//! Hello World Plugin - Hot-Reloadable Service Implementations
//!
//! This library gets recompiled and reloaded frequently during development.
//! All public functions marked with #[no_mangle] can be hot-reloaded.

use hello_world_services::{Greeter, Counter, Session, SessionInfo};
use parking_lot::RwLock;
use serde::{Serialize, Deserialize};
use std::sync::OnceLock;
use uuid::Uuid;

// ═══════════════════════════════════════════════════════════════════════════
// Persistent State (survives hot-reloads via save/restore)
// ═══════════════════════════════════════════════════════════════════════════

#[derive(Serialize, Deserialize, Clone)]
struct PluginState {
    greeting: String,
    counter: i32,
    current_session: Option<SessionInfo>,
    sessions: Vec<SessionInfo>,
}

impl Default for PluginState {
    fn default() -> Self {
        Self {
            greeting: "Hello".to_string(),
            counter: 0,
            current_session: None,
            sessions: Vec::new(),
        }
    }
}

static STATE: OnceLock<RwLock<PluginState>> = OnceLock::new();

fn state() -> &'static RwLock<PluginState> {
    STATE.get_or_init(|| RwLock::new(PluginState::default()))
}

// ═══════════════════════════════════════════════════════════════════════════
// Greeter Service Implementation
// ═══════════════════════════════════════════════════════════════════════════

#[no_mangle]
pub fn greeter_greet(name: String) -> String {
    let state = state().read();
    let greeting = &state.greeting;

    // 🔥 HOT-RELOAD ME! Change this message and save to see instant updates!
    format!("{}, {}! 👋", greeting, name)
}

#[no_mangle]
pub fn greeter_set_greeting(greeting: String) {
    let mut state = state().write();
    state.greeting = greeting;
    println!("🔥 Greeting changed to: {}", state.greeting);
}

#[no_mangle]
pub fn greeter_get_greeting() -> String {
    let state = state().read();
    state.greeting.clone()
}

// ═══════════════════════════════════════════════════════════════════════════
// Counter Service Implementation
// ═══════════════════════════════════════════════════════════════════════════

#[no_mangle]
pub fn counter_increment() -> i32 {
    let mut state = state().write();
    state.counter += 1;
    println!("🔥 Counter incremented to: {}", state.counter);
    state.counter
}

#[no_mangle]
pub fn counter_decrement() -> i32 {
    let mut state = state().write();
    state.counter -= 1;
    println!("🔥 Counter decremented to: {}", state.counter);
    state.counter
}

#[no_mangle]
pub fn counter_get_count() -> i32 {
    let state = state().read();
    state.counter
}

#[no_mangle]
pub fn counter_reset() {
    let mut state = state().write();
    state.counter = 0;
    println!("🔥 Counter reset to: 0");
}

// ═══════════════════════════════════════════════════════════════════════════
// Session Service Implementation
// ═══════════════════════════════════════════════════════════════════════════

#[no_mangle]
pub fn session_create_session(name: String) -> SessionInfo {
    let mut state = state().write();

    let session = SessionInfo {
        id: Uuid::new_v4(),
        name,
        created_at: std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs() as i64,
    };

    state.sessions.push(session.clone());
    state.current_session = Some(session.clone());

    println!("🔥 Created session: {}", session.name);
    session
}

#[no_mangle]
pub fn session_get_current_session() -> Option<SessionInfo> {
    let state = state().read();
    state.current_session.clone()
}

#[no_mangle]
pub fn session_list_sessions() -> Vec<SessionInfo> {
    let state = state().read();
    state.sessions.clone()
}

// ═══════════════════════════════════════════════════════════════════════════
// Hot-Reload State Management
// ═══════════════════════════════════════════════════════════════════════════

/// Save state before reload (called by hot-lib-reloader)
#[no_mangle]
pub fn save_state() -> String {
    let state = state().read();
    let json = serde_json::to_string(&*state).unwrap();
    println!("💾 Saving state before reload...");
    json
}

/// Restore state after reload (called by hot-lib-reloader)
#[no_mangle]
pub fn restore_state(json: &str) {
    if let Ok(new_state) = serde_json::from_str::<PluginState>(json) {
        let mut state = state().write();
        *state = new_state;
        println!("♻️  State restored after reload!");
        println!("   - Greeting: {}", state.greeting);
        println!("   - Counter: {}", state.counter);
        println!("   - Sessions: {} total", state.sessions.len());
    }
}

/// Called when library loads
#[no_mangle]
pub extern "C" fn on_load() {
    println!("🚀 hello-world-impl loaded!");
}

/// Called before library unloads
#[no_mangle]
pub extern "C" fn on_unload() {
    println!("👋 hello-world-impl unloading...");
}

// ═══════════════════════════════════════════════════════════════════════════
// Hot-Reload Demo Functions
// ═══════════════════════════════════════════════════════════════════════════

/// A function you can modify to test hot-reload!
#[no_mangle]
pub fn get_version() -> String {
    // 🔥 CHANGE THIS AND SAVE TO SEE HOT-RELOAD!
    "v1.0.0 - Initial version".to_string()
}

#[no_mangle]
pub fn get_emoji() -> String {
    // 🔥 CHANGE THE EMOJI AND SAVE!
    "🦀".to_string()
}
