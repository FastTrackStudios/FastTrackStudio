//! Hot-Reload Demo
//!
//! This demo shows hot-lib-reloader in action:
//! 1. Watches the .dylib file for changes
//! 2. Auto-reloads when you edit and save
//! 3. Preserves state across reloads

use std::ffi::{CStr, CString};

// ═══════════════════════════════════════════════════════════════════════════
// Hot Module - Auto-reloads implementation library
// ═══════════════════════════════════════════════════════════════════════════

#[hot_lib_reloader::hot_module(dylib = "hot_reload_impl")]
mod hot {
    use std::ffi::{CStr, CString};

    // Non-blocking reload detection
    #[lib_updated]
    pub fn was_updated() -> bool {}

    unsafe extern "C" {
        fn greet(name: *const std::os::raw::c_char) -> *mut std::os::raw::c_char;
        fn increment() -> i32;
        fn get_version() -> *mut std::os::raw::c_char;
        fn get_emoji() -> *mut std::os::raw::c_char;
    }

    // Safe wrappers
    pub fn call_greet(name: &str) -> String {
        let name_c = CString::new(name).unwrap();
        let result_ptr = unsafe { greet(name_c.as_ptr()) };
        let result = unsafe {
            CStr::from_ptr(result_ptr)
                .to_string_lossy()
                .to_string()
        };
        unsafe {
            let _ = CString::from_raw(result_ptr);
        }
        result
    }

    pub fn call_increment() -> i32 {
        unsafe { increment() }
    }

    pub fn call_get_version() -> String {
        let result_ptr = unsafe { get_version() };
        let result = unsafe {
            CStr::from_ptr(result_ptr)
                .to_string_lossy()
                .to_string()
        };
        unsafe {
            let _ = CString::from_raw(result_ptr);
        }
        result
    }

    pub fn call_get_emoji() -> String {
        let result_ptr = unsafe { get_emoji() };
        let result = unsafe {
            CStr::from_ptr(result_ptr)
                .to_string_lossy()
                .to_string()
        };
        unsafe {
            let _ = CString::from_raw(result_ptr);
        }
        result
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Main Demo Loop
// ═══════════════════════════════════════════════════════════════════════════

fn main() {
    println!("🔥 Hot-Reload Demo Starting!");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("");
    println!("💡 INSTRUCTIONS:");
    println!("   1. Open a second terminal");
    println!("   2. Run: cargo watch -x 'build --lib -p hot-reload-impl'");
    println!("   3. Edit hot-reload-impl/src/lib.rs");
    println!("   4. Save and watch it reload automatically!");
    println!("");
    println!("🔥 TRY THESE CHANGES:");
    println!("   - Change greet() emojis from 👋🦀 to something else");
    println!("   - Change increment() from += 1 to += 5");
    println!("   - Change get_version() message");
    println!("   - Change get_emoji() from 🦀 to 🎸");
    println!("");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("");

    let mut tick = 0;
    let mut reload_count = 0;

    loop {
        std::thread::sleep(std::time::Duration::from_secs(3));

        // Check for hot-reload before calling functions
        if hot::was_updated() {
            reload_count += 1;
            println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
            println!("♻️  ═══════════════════════════════════════════════════");
            println!("🔥  HOT-RELOAD #{} DETECTED! 🔥", reload_count);
            println!("    Implementation reloaded without restarting!");
            println!("    ═══════════════════════════════════════════════════");
            println!("");
        }

        tick += 1;
        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        println!("Tick #{}", tick);
        println!("");

        // Call hot-reloadable functions
        let greeting = hot::call_greet("World");
        let count = hot::call_increment();
        let version = hot::call_get_version();
        let emoji = hot::call_get_emoji();

        println!("📞 greet('World') = \"{}\"", greeting);
        println!("📞 increment() = {}", count);
        println!("📞 get_version() = \"{}\"", version);
        println!("📞 get_emoji() = \"{}\"", emoji);
        println!("");
    }
}
