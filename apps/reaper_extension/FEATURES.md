# Feature Flags

This extension supports feature flags to enable/disable specific functionality at compile time. This is useful for testing and debugging.

## How Feature Flags Work

Feature flags are defined in `Cargo.toml` and controlled at compile time. They allow you to conditionally include or exclude code from compilation.

### Setting Defaults

Defaults are set in `Cargo.toml`:
```toml
[features]
default = ["input"]  # These features are enabled by default
input = []           # Feature definition (can list dependencies here)
```

### Using Feature Flags in Code

Use `#[cfg(feature = "feature_name")]` to conditionally compile code:

```rust
// Conditionally include a module
#[cfg(feature = "input")]
mod input;

// Conditionally compile a function
#[cfg(feature = "input")]
fn register_input_handler() {
    // ...
}

// Conditionally execute code in a function
fn initialize() {
    #[cfg(feature = "input")]
    {
        register_input_handler();
    }
}

// Conditional compilation in expressions
let value = if cfg!(feature = "input") {
    "input enabled"
} else {
    "input disabled"
};
```

### Alternative: `cfg!` macro

The `cfg!` macro evaluates at compile time but doesn't remove code - it just returns true/false:
```rust
if cfg!(feature = "input") {
    // This code is always compiled, but conditionally executed
    println!("Input feature is enabled");
}
```

## Available Features

### `input` (default: enabled)
Enables FTS-Input key interception and redirection functionality.

**When enabled:**
- Registers the FTS-Input handler for keyboard interception
- Registers FTS-Input toggle actions
- Adds "Input" menu category

**When disabled:**
- No keyboard interception
- Extension is completely transparent (all keys pass through to REAPER)
- No input-related actions or menu items

## Building with Feature Flags

### Build with all default features:
```bash
cargo build --manifest-path apps/reaper_extension/Cargo.toml
# or explicitly:
cargo build --manifest-path apps/reaper_extension/Cargo.toml --features input
```

### Build without any features (completely transparent):
```bash
cargo build --manifest-path apps/reaper_extension/Cargo.toml --no-default-features
```

### Build with only specific features:
```bash
# Enable only input (same as default in this case):
cargo build --manifest-path apps/reaper_extension/Cargo.toml --features input

# When you have multiple features, you can combine them:
# cargo build --manifest-path apps/reaper_extension/Cargo.toml --features "input,other_feature"
```

## Use Cases

### Testing Transparency
To verify the extension doesn't interfere with REAPER when input is disabled:
```bash
cargo build --manifest-path apps/reaper_extension/Cargo.toml --no-default-features
```

### Debugging Input Issues
To test if input interception is causing problems:
1. Build without input: `cargo build --manifest-path apps/reaper_extension/Cargo.toml --no-default-features`
2. Test if the issue persists
3. If issue is gone, the problem is in the input system
4. If issue persists, the problem is elsewhere in the extension

## Adding New Features

To add a new feature flag:

1. Add to `Cargo.toml`:
```toml
[features]
default = ["input", "new_feature"]  # Add to default list
input = []
new_feature = []  # New feature
```

2. Gate the code with `#[cfg(feature = "new_feature")]`:
```rust
// Conditionally include module
#[cfg(feature = "new_feature")]
mod new_feature;

// Conditionally call functions
#[cfg(feature = "new_feature")]
{
    crate::new_feature::register();
}

// Or use cfg! macro for runtime checks
if cfg!(feature = "new_feature") {
    // This code is compiled but conditionally executed
}
```

## Feature Dependencies

You can make features depend on each other:
```toml
[features]
default = ["input"]
input = []
advanced_input = ["input"]  # Requires "input" to be enabled
```

## Checking Features at Runtime

While features are compile-time, you can check them:
```rust
// Compile-time check (code is removed if feature is disabled)
#[cfg(feature = "input")]
fn do_something() { }

// Runtime check (code is always compiled)
if cfg!(feature = "input") {
    println!("Input feature is enabled");
}
```
