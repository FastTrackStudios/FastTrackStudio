# FastTrackStudio xtask

This directory contains automation tasks for the FastTrackStudio workspace using the [cargo-xtask](https://github.com/matklad/cargo-xtask) pattern.

## Usage

All tasks are run from the workspace root using the `cargo xtask` command:

```bash
cargo xtask <COMMAND> [OPTIONS]
```

## Available Tasks

### TypeScript Type Generation

Generate TypeScript types from Rust types using ts-rs:

```bash
# Generate types to default location (apps/desktop/src/bindings)
cargo xtask generate-types

# Generate to custom directory
cargo xtask generate-types --output ./custom/path

# Generate without index.ts file
cargo xtask generate-types --no-index
```

This orchestrates the existing `tools/generate-types` tool while providing workspace-level automation. All generated types are centralized in the desktop app at `apps/desktop/src/bindings/` for easy importing in the frontend.

### Testing

Run tests across the workspace:

```bash
# Run all tests
cargo xtask test

# Run tests in release mode
cargo xtask test --release

# Run tests for specific package
cargo xtask test --package transport
```

### Code Formatting

Format code using rustfmt:

```bash
# Format all code
cargo xtask format

# Check formatting without applying changes
cargo xtask format --check
```

### Linting

Run clippy on all packages:

```bash
# Run clippy with default settings
cargo xtask clippy

# Treat warnings as errors
cargo xtask clippy --deny-warnings
```

### Development Workflow

Start the Tauri development server:

```bash
# Start dev server (generates types first)
cargo xtask dev

# Start dev server without type generation
cargo xtask dev --skip-types
```

### Building

Build the desktop application:

```bash
# Build in debug mode
cargo xtask build

# Build in release mode
cargo xtask build --release
```

### CI Pipeline

Run the complete CI pipeline locally:

```bash
cargo xtask ci
```

This runs:
1. Format check
2. Clippy with warnings as errors
3. TypeScript type generation
4. All tests
5. Build check

### Cleanup

Clean all build artifacts:

```bash
cargo xtask clean
```

## Architecture

The xtask system is designed to:

- **Orchestrate existing tools**: Rather than replace tools like `generate-types`, it provides a unified interface
- **Provide workspace automation**: Common development tasks accessible from anywhere in the workspace
- **Support CI/CD**: The `ci` command runs the same checks that would run in continuous integration
- **Bootstrap from cargo**: No additional dependencies beyond Rust toolchain

## Adding New Tasks

To add a new task:

1. Add a new variant to the `Task` enum in `src/main.rs`
2. Implement the task function
3. Add the case to the match statement in `try_main()`
4. Update this README

Example:

```rust
#[derive(Subcommand)]
enum Task {
    // ... existing tasks
    NewTask {
        /// Description of the option
        #[arg(short, long)]
        option: bool,
    },
}

fn new_task(option: bool) -> Result<(), DynError> {
    println!("🔧 Running new task...");
    // Implementation here
    Ok(())
}
```

## Integration with Existing Tools

The xtask system works alongside existing tools:

- `tools/generate-types`: Standalone TypeScript type generator
- Standard cargo commands: `cargo build`, `cargo test`, etc.
- Tauri CLI: `cargo tauri dev`, `cargo tauri build`

This provides flexibility - you can use either the xtask commands for convenience or the underlying tools directly when needed.

## TypeScript Type Organization

Generated TypeScript types are centralized in `apps/desktop/src/bindings/`:

```
apps/desktop/src/bindings/
├── index.ts          # Re-exports all types
├── PlayState.ts      # Transport states
├── Transport.ts      # Main transport type
├── Position.ts       # Time position types
├── TimeRange.ts      # Time ranges
├── ProjectError.ts   # Error types
└── ...              # Other generated types
```

The types can be imported in your React components:

```typescript
import { Transport, PlayState, TimeRange } from '../bindings';
// or import everything:
import * as RustTypes from '../bindings';
```

The xtask automatically cleans up any stray `bindings` directories that might be created in individual modules, ensuring all types remain centralized in the desktop app.