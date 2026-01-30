//! Hot-Reloadable Implementation Library
//!
//! All public functions marked with #[unsafe(no_mangle)] can be hot-reloaded.
//! Edit this file and save to see instant updates!

use parking_lot::RwLock;
use serde::{Serialize, Deserialize};
use std::sync::OnceLock;

// ═══════════════════════════════════════════════════════════════════════════
// Persistent State (survives hot-reloads)
// ═══════════════════════════════════════════════════════════════════════════

#[derive(Serialize, Deserialize, Clone)]
pub struct State {
    pub greeting: String,
    pub counter: i32,
}

impl Default for State {
    fn default() -> Self {
        Self {
            greeting: "Hello".to_string(),
            counter: 0,
        }
    }
}

static STATE: OnceLock<RwLock<State>> = OnceLock::new();

fn state() -> &'static RwLock<State> {
    STATE.get_or_init(|| RwLock::new(State::default()))
}

// ═══════════════════════════════════════════════════════════════════════════
// Hot-Reloadable Functions
// ═══════════════════════════════════════════════════════════════════════════

/// 🔥 HOT-RELOAD ME! Change the message format and save to see updates!
#[unsafe(no_mangle)]
pub extern "C" fn greet(name: *const std::os::raw::c_char) -> *mut std::os::raw::c_char {
    let name_str = unsafe {
        std::ffi::CStr::from_ptr(name)
            .to_str()
            .unwrap_or("Unknown")
    };

    let state = state().read();
    let greeting = &state.greeting;

    // 🔥 CHANGE THIS MESSAGE! Try different emojis: 🎸 🎹 🔥 ⚡ 🌟
    let result = format!("{}, {}! 👋🦀", greeting, name_str);

    // Convert to C string
    std::ffi::CString::new(result).unwrap().into_raw()
}

/// 🔥 HOT-RELOAD ME! Change the increment amount!
#[unsafe(no_mangle)]
pub extern "C" fn increment() -> i32 {
    let mut state = state().write();
    state.counter += 1; // 🔥 Try changing to += 2 or += 5!
    println!("  Counter incremented to: {}", state.counter);
    state.counter
}

#[unsafe(no_mangle)]
pub extern "C" fn get_count() -> i32 {
    let state = state().read();
    state.counter
}

#[unsafe(no_mangle)]
pub extern "C" fn reset_counter() {
    let mut state = state().write();
    state.counter = 0;
    println!("  Counter reset!");
}

#[unsafe(no_mangle)]
pub extern "C" fn set_greeting(greeting: *const std::os::raw::c_char) {
    let greeting_str = unsafe {
        std::ffi::CStr::from_ptr(greeting)
            .to_str()
            .unwrap_or("Hello")
    };

    let mut state = state().write();
    state.greeting = greeting_str.to_string();
    println!("  Greeting changed to: {}", state.greeting);
}

/// 🔥 HOT-RELOAD ME! Change the version string!
#[unsafe(no_mangle)]
pub extern "C" fn get_version() -> *mut std::os::raw::c_char {
    // 🔥 CHANGE THIS! Try: "v2.0.0 - Hot-reloaded! 🚀"
    let version = "v1.0.0 - Initial version";
    std::ffi::CString::new(version).unwrap().into_raw()
}

/// 🔥 HOT-RELOAD ME! Change the emoji!
#[unsafe(no_mangle)]
pub extern "C" fn get_emoji() -> *mut std::os::raw::c_char {
    // 🔥 TRY DIFFERENT EMOJIS: 🎸 🎹 🎵 🔥 ⚡ 🌟 ✨ 🎨
    let emoji = "🦀";
    std::ffi::CString::new(emoji).unwrap().into_raw()
}

// ═══════════════════════════════════════════════════════════════════════════
// State Management for Hot-Reload
// ═══════════════════════════════════════════════════════════════════════════

#[unsafe(no_mangle)]
pub extern "C" fn save_state() -> *mut std::os::raw::c_char {
    let state = state().read();
    let json = serde_json::to_string(&*state).unwrap();
    println!("💾 Saving state: {}", json);
    std::ffi::CString::new(json).unwrap().into_raw()
}

#[unsafe(no_mangle)]
pub extern "C" fn restore_state(json: *const std::os::raw::c_char) {
    let json_str = unsafe {
        std::ffi::CStr::from_ptr(json)
            .to_str()
            .unwrap_or("{}")
    };

    if let Ok(new_state) = serde_json::from_str::<State>(json_str) {
        let mut state = state().write();
        *state = new_state;
        println!("♻️  State restored!");
        println!("   - Greeting: {}", state.greeting);
        println!("   - Counter: {}", state.counter);
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Library Lifecycle Events
// ═══════════════════════════════════════════════════════════════════════════

#[unsafe(no_mangle)]
pub extern "C" fn on_load() {
    println!("🚀 hot-reload-impl loaded!");
}

#[unsafe(no_mangle)]
pub extern "C" fn on_unload() {
    println!("👋 hot-reload-impl unloading...");
}
