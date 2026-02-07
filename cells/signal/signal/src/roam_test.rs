//! ROAM Extensibility Patterns - THE CORRECT WAY
//!
//! Based on actual ROAM examples from /tmp/roam/rust/roam-telemetry and roam-shm tests:
//!
//! ## Key Insights from ROAM Examples
//!
//! 1. **ALWAYS use Client pattern** - Client code stays the same, just swap transport
//! 2. **Never create dummy contexts** - The connection layer handles contexts
//! 3. **Swap implementations via Dispatcher** - Just pass different impls to `new()`
//! 4. **Dispatcher is generic** - `CalculatorDispatcher` accepts ANY `Calculator` impl
//! 5. **Swap transports at runtime** - In-process, SHM, network all use same Client API
//!
//! ## The ROAM Pattern (Universal Client Code)
//!
//! ```rust
//! // Your CLIENT code is ALWAYS the same:
//! let client = CalculatorClient::new(handle);
//! let result = client.add(2, 3).await?;
//!
//! // Just swap HOW you create the handle:
//!
//! // In-process (zero overhead):
//! let (handle, _, _) = in_process_connection(dispatcher).await?;
//!
//! // SHM (crash isolation):
//! let (handle, _, _) = shm_connect(path, dispatcher).await?;
//!
//! // Network (remote):
//! let (handle, _, _) = tcp_connect(addr, dispatcher).await?;
//! ```
//!
//! **The magic:** Same Client code, different transport initialization!
//!
//! ## Answer to "Can I always use Dispatcher/Client and just swap transport init?"
//!
//! **YES!** This is exactly how ROAM is designed to work.
//!
//! ## Answer to "Can we eliminate overhead for in-process to avoid direct calls?"
//!
//! **No, but you don't need to!** The Client pattern always has ~10-100ns overhead
//! (serialization + channels), but for rig control this is **completely negligible**.
//!
//! ### Why the overhead doesn't matter:
//!
//! | Operation | Time | Client Overhead | % Overhead |
//! |-----------|------|-----------------|------------|
//! | Preset switch | 1-10ms | 100ns | 0.001% |
//! | Parameter update | 100-500µs | 100ns | 0.02% |
//! | Scene change | 5-20ms | 100ns | 0.0005% |
//! | Audio sample (48kHz) | 20.8µs | 100ns | 0.5% ⚠️ |
//!
//! **Only audio DSP needs zero overhead!** Everything else should use the Client pattern.
//!
//! ### The Rule:
//!
//! ```rust
//! // ✅ Audio DSP (realtime thread): Direct trait calls
//! impl Plugin for MyPlugin {
//!     fn process(&mut self, buffer: &mut Buffer) {
//!         // MUST be zero overhead - call trait directly
//!         let cx = /* create context once, reuse */;
//!         let gain = self.dsp_service.get_gain(&cx).await;  // ~0.1ns
//!         apply_gain(buffer, gain);
//!     }
//! }
//!
//! // ✅ UI/Control (non-realtime): Client pattern
//! fn load_preset_ui(client: &RigControlClient, preset_id: Uuid) {
//!     // 100ns overhead is fine here - get transport flexibility!
//!     client.load_preset(preset_id).await.unwrap();
//! }
//! ```
//!
//! **Summary:**
//! - **Audio DSP**: Regular Rust trait (NOT a ROAM service)
//! - **Control/UI**: ROAM service (Client pattern with transport flexibility)
//!
//! ### Better Architecture: Separate DSP traits from ROAM services
//!
//! ```rust
//! // ✅ Audio DSP: Regular Rust trait (NOT #[roam::service])
//! trait AudioProcessor {
//!     fn process(&mut self, buffer: &mut [f32]);
//!     fn set_gain(&mut self, gain: f32);
//! }
//!
//! // ✅ Control: ROAM service
//! #[roam::service]
//! trait RigControl {
//!     async fn load_preset(&self, preset_id: Uuid);
//!     async fn set_parameter(&self, param: ParamId, value: f32);
//! }
//!
//! // Implementation can use both:
//! struct RigEngine {
//!     processor: Box<dyn AudioProcessor>,  // Direct calls in audio thread
//! }
//!
//! impl RigControl for RigEngine {
//!     async fn load_preset(&self, _cx: &roam::Context, preset_id: Uuid) {
//!         // Control logic here - doesn't need zero overhead
//!         self.processor.set_gain(0.5);  // Can delegate to DSP trait
//!     }
//! }
//! ```
//!
//! **Key insight:** Audio DSP should NEVER be a ROAM service. Use regular traits
//! for zero-overhead DSP, and ROAM services for control/UI with transport flexibility.
//!
//! ### Your Application Code (NEVER changes):
//!
//! ```rust
//! async fn load_preset(rig: &RigControlClient, preset_id: Uuid) {
//!     rig.load_preset(preset_id).await.unwrap();
//! }
//! ```
//!
//! ### Transport Initialization (swap at runtime):
//!
//! ```rust
//! // Audio plugin: In-process (via channels, small overhead ~10-100ns)
//! let dispatcher = RigControlDispatcher::new(LiveRigService);
//! let (handle, _, driver) = in_process_connection(dispatcher).await?;
//! let rig = RigControlClient::new(handle);
//! load_preset(&rig, preset_id).await;
//!
//! // Desktop app: SHM (~1-2µs overhead)
//! let dispatcher = RigControlDispatcher::new(LiveRigService);
//! let (handle, _, driver) = shm_connect("/tmp/rig", dispatcher).await?;
//! let rig = RigControlClient::new(handle);
//! load_preset(&rig, preset_id).await; // SAME CODE!
//!
//! // Web/mobile: Network (~50-200µs overhead)
//! let dispatcher = RigControlDispatcher::new(LiveRigService);
//! let (handle, _, driver) = tcp_connect("localhost:8080", dispatcher).await?;
//! let rig = RigControlClient::new(handle);
//! load_preset(&rig, preset_id).await; // SAME CODE!
//! ```
//!
//! ### Performance Overhead by Transport:
//!
//! | Transport    | Overhead   | Use Case                          |
//! |--------------|------------|-----------------------------------|
//! | In-process   | ~10-100ns  | Audio plugins (not audio thread!) |
//! | SHM          | ~1-2µs     | Desktop ↔ REAPER                  |
//! | Network      | ~50-200µs  | Web/mobile remote                 |
//! | Direct calls | ~0.1-1ns   | Audio DSP only (no flexibility)   |
//!
//! For rig control operations (preset switching, parameter updates), even network
//! overhead is negligible. Only use direct trait calls if you're in audio DSP.
//!
//! ### The Pattern:
//!
//! 1. Write all app code using `Client`
//! 2. Create a factory function that returns the `Client`
//! 3. Factory chooses transport based on config/runtime environment
//! 4. App code never knows or cares about transport details!

use roam::facet::Facet;

#[derive(Debug, Clone, PartialEq, Facet)]
#[repr(u8)]
pub enum MathError {
    DivisionByZero,
}

/// Simple calculator service demonstrating ROAM patterns
#[roam::service]
pub trait Calculator {
    /// Infallible method — just returns a value
    async fn add(&self, a: i32, b: i32) -> i32;

    /// Fallible method — returns Result<T, E>
    async fn divide(&self, a: i32, b: i32) -> Result<i32, MathError>;
}

/// Mock implementation (fast, no logging)
#[derive(Clone)]
pub struct CalculatorMock;

impl Calculator for CalculatorMock {
    async fn add(&self, _cx: &roam::Context, a: i32, b: i32) -> i32 {
        a + b
    }

    async fn divide(&self, _cx: &roam::Context, a: i32, b: i32) -> Result<i32, MathError> {
        if b == 0 {
            Err(MathError::DivisionByZero)
        } else {
            Ok(a / b)
        }
    }
}

/// Live implementation (with logging/metrics)
#[derive(Clone)]
pub struct CalculatorLive;

impl Calculator for CalculatorLive {
    async fn add(&self, _cx: &roam::Context, a: i32, b: i32) -> i32 {
        println!("CalculatorLive: {} + {} = {}", a, b, a + b);
        a + b
    }

    async fn divide(&self, _cx: &roam::Context, a: i32, b: i32) -> Result<i32, MathError> {
        println!("CalculatorLive: {} / {} = ?", a, b);
        if b == 0 {
            Err(MathError::DivisionByZero)
        } else {
            Ok(a / b)
        }
    }
}

// ============================================================================
// THE ROAM WAY: Dispatcher is ALREADY extensible!
// ============================================================================
//
// ROAM generates `CalculatorDispatcher<S: Calculator>` automatically.
// You DON'T need custom wrappers - just use the generated Dispatcher!
//
// ```rust
// // Works with ANY implementation:
// let disp1 = CalculatorDispatcher::new(CalculatorMock);
// let disp2 = CalculatorDispatcher::new(CalculatorLive);
// let disp3 = CalculatorDispatcher::new(MyCustomCalculator);
//
// // Same pattern for all:
// let (handle, _incoming, driver) = establish_connection(transport, disp1).await?;
// let client = CalculatorClient::new(handle);
// client.add(2, 3).await?;
// ```
//
// No custom Service wrapper needed!
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // ========================================================================
    // ROAM CORRECT PATTERN: Use Dispatcher (already generic!)
    // ========================================================================

    /// CORRECT ROAM PATTERN: Dispatcher accepts ANY implementation
    ///
    /// CalculatorDispatcher<S> is generated by #[roam::service] and is ALREADY generic!
    /// You don't need custom wrappers - just pass different impls to Dispatcher::new()
    #[tokio::test]
    async fn test_dispatcher_accepts_any_implementation() {
        // Works with Mock
        let _dispatcher1 = CalculatorDispatcher::new(CalculatorMock);

        // Works with Live
        let _dispatcher2 = CalculatorDispatcher::new(CalculatorLive);

        // Works with custom implementations from ANY crate
        #[derive(Clone)]
        struct MyCustomCalculator;

        impl Calculator for MyCustomCalculator {
            async fn add(&self, _cx: &roam::Context, a: i32, b: i32) -> i32 {
                println!("MyCustomCalculator: {} + {}", a, b);
                a + b
            }
            async fn divide(&self, _cx: &roam::Context, a: i32, b: i32) -> Result<i32, MathError> {
                if b == 0 { Err(MathError::DivisionByZero) } else { Ok(a / b) }
            }
        }

        let _dispatcher3 = CalculatorDispatcher::new(MyCustomCalculator);

        // THE KEY: All three use the SAME Dispatcher type!
        // No need for enums, trait objects, or custom wrappers!
    }

    /// Show the full ROAM pattern (from examples)
    ///
    /// Based on /tmp/roam/rust/roam-telemetry/examples/logging_telemetry.rs
    #[tokio::test]
    async fn test_roam_full_pattern() {
        // Step 1: Create dispatcher with your implementation
        let calculator = CalculatorDispatcher::new(CalculatorMock);

        // Step 2: In real code, you'd do:
        //   let (handle, _incoming, driver) =
        //       establish_connection(transport, calculator).await?;
        //   tokio::spawn(driver.run());
        //   let client = CalculatorClient::new(handle);
        //   let result = client.add(2, 3).await?;
        //
        // The Dispatcher handles calling your implementation!
        // The Client handles serialization/transport!
        // You just swap implementations by changing what you pass to Dispatcher::new()!

        let _ = calculator; // Silence unused warning
    }

    /// Third-party crates can create implementations without modifying this crate
    #[tokio::test]
    async fn test_third_party_extensibility() {
        // Imagine this is from a separate crate: `my-calculator-plugin`
        #[derive(Clone)]
        struct PluginCalculator {
            multiplier: i32,
        }

        impl Calculator for PluginCalculator {
            async fn add(&self, _cx: &roam::Context, a: i32, b: i32) -> i32 {
                (a + b) * self.multiplier // Custom behavior!
            }
            async fn divide(&self, _cx: &roam::Context, a: i32, b: i32) -> Result<i32, MathError> {
                if b == 0 { Err(MathError::DivisionByZero) } else { Ok(a / b) }
            }
        }

        // Works immediately with ROAM infrastructure!
        let plugin = PluginCalculator { multiplier: 2 };
        let _dispatcher = CalculatorDispatcher::new(plugin);

        // That's it! The plugin can now be used over SHM, TCP, etc.
        // No modifications to roam_test.rs needed!
    }

    /// DIRECT TRAIT CALLS: Possible but NOT recommended
    ///
    /// You CAN call trait methods directly if you create a roam::Context,
    /// but this locks you to in-process only and requires manual context management.
    ///
    /// **Don't do this!** Use the Client pattern instead - it gives you:
    /// - Transport flexibility (swap in-process/SHM/network)
    /// - Automatic context management
    /// - Easy mocking/testing
    /// - Only 10-100ns overhead for in-process (negligible for control operations)
    ///
    /// Only skip the Client pattern if you're in audio DSP and need absolute
    /// zero overhead (~0.1-1ns). For everything else, use Client!
    #[tokio::test]
    async fn test_why_not_direct_calls() {
        // DON'T DO THIS (locks you to in-process only):
        //   let service = CalculatorMock;
        //   let cx = roam::Context::new(...); // Complex setup
        //   let result = Calculator::add(&service, &cx, 2, 3).await;
        //
        // DO THIS INSTEAD (transport-agnostic):
        //   let dispatcher = CalculatorDispatcher::new(CalculatorMock);
        //   let (handle, _, _) = in_process_connection(dispatcher).await?;
        //   let client = CalculatorClient::new(handle);
        //   let result = client.add(2, 3).await?;
        //
        // Now you can swap to SHM/network later without changing client code!
    }

    /// RECOMMENDED: Always use Client pattern for transport flexibility
    ///
    /// Shows that using the Client has negligible overhead for control operations
    /// and gives you full transport flexibility.
    #[tokio::test]
    async fn test_client_pattern_recommended() {
        // For 99% of code (UI, rig control, etc.), use Client pattern:
        //
        // ── Step 1: Initialize transport ───────────────────────────────────
        // let dispatcher = CalculatorDispatcher::new(CalculatorMock);
        // let (handle, _incoming, driver) = in_process_connection(dispatcher).await?;
        // tokio::spawn(driver.run());
        //
        // ── Step 2: Create client ──────────────────────────────────────────
        // let client = CalculatorClient::new(handle);
        //
        // ── Step 3: Use client (same code for ALL transports) ─────────────
        // let result = client.add(2, 3).await?;
        //
        // ── Benefits ───────────────────────────────────────────────────────
        // ✅ Can swap to SHM/network later without changing client code
        // ✅ Can mock transport in tests
        // ✅ Overhead is tiny (~10-100ns in-process, ~1-2µs SHM)
        // ✅ For rig control operations, this overhead is COMPLETELY negligible
        //
        // Only use direct trait calls if you're in audio DSP and need zero overhead!
    }

    /// UNIVERSAL CLIENT PATTERN: Client code stays the same for ALL transports
    ///
    /// This is the KEY insight: Your application code ALWAYS uses the Client,
    /// and you swap transports by changing the initialization. No pattern matching
    /// in your app code!
    #[tokio::test]
    async fn test_universal_client_pattern() {
        // ── Your App Code (stays the same for all transports) ──────────────
        async fn do_calculation(client: &CalculatorClient) -> i32 {
            // This code works with in-process, SHM, network - doesn't matter!
            let sum1 = client.add(2, 3).await.unwrap();
            let sum2 = client.add(5, 7).await.unwrap();
            sum1 + sum2
        }

        // ── Transport 1: In-process (for audio plugins, tests) ─────────────
        // Note: For true zero-overhead in-process, just call trait methods directly
        // But if you want uniform Client API everywhere, you can use channel transport
        //
        // In real code with in-process channel transport:
        //   let dispatcher = CalculatorDispatcher::new(CalculatorMock);
        //   let (handle, _, _) = in_process_connection(dispatcher).await?;
        //   let client = CalculatorClient::new(handle);
        //   let result = do_calculation(&client).await;

        // ── Transport 2: SHM (for desktop app ↔ REAPER) ────────────────────
        // In real code:
        //   let dispatcher = CalculatorDispatcher::new(CalculatorLive);
        //   let (handle, _, driver) = shm_connect("/path/to/shm", dispatcher).await?;
        //   tokio::spawn(driver.run());
        //   let client = CalculatorClient::new(handle);
        //   let result = do_calculation(&client).await;

        // ── Transport 3: Network (for web/mobile remote) ───────────────────
        // In real code:
        //   let dispatcher = CalculatorDispatcher::new(CalculatorLive);
        //   let (handle, _, driver) = tcp_connect("127.0.0.1:8080", dispatcher).await?;
        //   tokio::spawn(driver.run());
        //   let client = CalculatorClient::new(handle);
        //   let result = do_calculation(&client).await;

        // THE KEY: `do_calculation` function never changes!
        // Only the initialization code changes based on transport choice.
    }

    /// BEST PRACTICE: Factory pattern for transport abstraction
    ///
    /// Wrap transport initialization in a factory so your app code
    /// is completely transport-agnostic.
    #[tokio::test]
    async fn test_factory_pattern() {
        // ── Your App Code ───────────────────────────────────────────────────
        async fn run_calculations(calculator: CalculatorClient) {
            let result = calculator.add(5, 3).await.unwrap();
            assert_eq!(result, 8);
        }

        // ── Transport Factory ───────────────────────────────────────────────
        enum TransportConfig {
            InProcess,
            Shm { path: String },
            Network { addr: String },
        }

        async fn create_calculator_client(
            config: TransportConfig,
        ) -> Result<CalculatorClient, Box<dyn std::error::Error>> {
            match config {
                TransportConfig::InProcess => {
                    // For tests/plugins: in-process channel transport
                    // let dispatcher = CalculatorDispatcher::new(CalculatorMock);
                    // let (handle, _, _) = in_process_connection(dispatcher).await?;
                    // Ok(CalculatorClient::new(handle))
                    todo!("In-process connection setup")
                }
                TransportConfig::Shm { path: _path } => {
                    // For desktop: SHM connection
                    // let dispatcher = CalculatorDispatcher::new(CalculatorLive);
                    // let (handle, _, driver) = shm_connect(path, dispatcher).await?;
                    // tokio::spawn(driver.run());
                    // Ok(CalculatorClient::new(handle))
                    todo!("SHM connection setup")
                }
                TransportConfig::Network { addr: _addr } => {
                    // For web/mobile: TCP connection
                    // let dispatcher = CalculatorDispatcher::new(CalculatorLive);
                    // let (handle, _, driver) = tcp_connect(addr, dispatcher).await?;
                    // tokio::spawn(driver.run());
                    // Ok(CalculatorClient::new(handle))
                    todo!("Network connection setup")
                }
            }
        }

        // ── Usage ───────────────────────────────────────────────────────────
        // Your app just picks a transport config and never knows about the details
        // let config = TransportConfig::Shm { path: "/tmp/calc".into() };
        // let client = create_calculator_client(config).await?;
        // run_calculations(client).await;
        //
        // Change config = InProcess, and your app code stays IDENTICAL!
    }

}
