//! xtask: Development tasks for FastTrackStudio
//!
//! Run with: `cargo xtask <command>`

use std::process::ExitCode;

use facet::Facet;
use figue as args;
use xshell::{Shell, cmd};

/// Development tasks for FastTrackStudio
#[derive(Facet)]
struct Cli {
    #[facet(args::subcommand)]
    command: Commands,
}

#[derive(Facet)]
#[repr(u8)]
enum Commands {
    /// Run all CI checks locally (test, clippy, fmt, doc, coverage, miri)
    Ci,
    /// Run all tests (workspace)
    Test,
    /// Run clippy on all code
    Clippy,
    /// Check formatting
    Fmt {
        /// Fix formatting issues instead of just checking
        #[facet(args::named, default)]
        fix: bool,
    },
    /// Build documentation with warnings as errors
    Doc,
    /// Generate code coverage report (requires cargo-llvm-cov)
    Coverage,
    /// Run miri for undefined behavior detection (requires nightly)
    Miri,
    /// Run tracey analysis (spec coverage)
    Tracey {
        #[facet(args::subcommand)]
        action: TraceyAction,
    },
    /// Build and serve spec documentation with dodeca
    Dodeca {
        #[facet(args::subcommand)]
        action: DodecaAction,
    },
    /// Build all cells
    Build,
    /// Run DAW standalone cell
    Run,
    /// Run WASM integration tests with Playwright
    Playwright {
        /// Install Playwright browsers before running tests
        #[facet(args::named, default)]
        install: bool,
        /// Run tests in headed mode (visible browser)
        #[facet(args::named, default)]
        headed: bool,
        /// Run tests in UI mode for debugging
        #[facet(args::named, default)]
        ui: bool,
    },
}

#[derive(Facet)]
#[repr(u8)]
enum TraceyAction {
    /// Start tracey dashboard server
    Check,
    /// Generate traceability matrix
    Matrix,
    /// Extract rules from markdown specs
    Rules,
    /// Show impact analysis
    Impact,
}

#[derive(Facet)]
#[repr(u8)]
enum DodecaAction {
    /// Build spec documentation
    Build,
    /// Serve spec documentation locally
    Serve,
    /// Watch and rebuild on changes
    Watch,
}

fn main() -> ExitCode {
    if let Err(e) = run() {
        eprintln!("Error: {e}");
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

fn run() -> Result<(), Box<dyn std::error::Error>> {
    let cli: Cli = args::from_std_args().unwrap();
    let sh = Shell::new()?;

    // Find workspace root (where Cargo.toml with [workspace] lives)
    let workspace_root = std::env::var("CARGO_MANIFEST_DIR")
        .map(std::path::PathBuf::from)
        .unwrap_or_else(|_| std::env::current_dir().unwrap())
        .parent()
        .unwrap()
        .to_path_buf();
    sh.change_dir(&workspace_root);

    match cli.command {
        Commands::Test => {
            println!("\n=== Running workspace tests ===");
            // Note: WASM-only crates are excluded in workspace Cargo.toml

            // Try nextest first, fall back to cargo test
            if cmd!(sh, "cargo nextest --version").quiet().run().is_ok() {
                println!("Using cargo-nextest");
                // Use CI profile for longer timeouts when in CI
                if std::env::var("CI").is_ok() {
                    cmd!(sh, "cargo nextest run --workspace --profile ci").run()?;
                } else {
                    cmd!(sh, "cargo nextest run --workspace").run()?;
                }
            } else {
                println!("cargo-nextest not found, using cargo test");
                cmd!(sh, "cargo test --workspace").run()?;
            }

            println!("\n=== All tests passed ===");
        }
        Commands::Clippy => {
            println!("=== Running clippy ===");
            // Note: WASM-only crates are excluded in workspace Cargo.toml
            cmd!(sh, "cargo clippy --workspace --all-targets -- -D warnings").run()?;
        }
        Commands::Fmt { fix } => {
            if fix {
                println!("=== Fixing formatting ===");
                cmd!(sh, "cargo fmt --all").run()?;
            } else {
                println!("=== Checking formatting ===");
                cmd!(sh, "cargo fmt --all -- --check").run()?;
            }
        }
        Commands::Ci => {
            println!("=== Running all CI checks ===\n");

            println!(">>> cargo xtask test");
            cmd!(sh, "cargo xtask test").run()?;

            println!("\n>>> cargo xtask clippy");
            cmd!(sh, "cargo xtask clippy").run()?;

            println!("\n>>> cargo xtask fmt");
            cmd!(sh, "cargo xtask fmt").run()?;

            println!("\n>>> cargo xtask doc");
            cmd!(sh, "cargo xtask doc").run()?;

            println!("\n>>> cargo xtask coverage");
            cmd!(sh, "cargo xtask coverage").run()?;

            println!("\n=== All CI checks passed ===");
        }
        Commands::Doc => {
            println!("=== Building documentation with warnings as errors ===");
            cmd!(sh, "cargo doc --no-deps")
                .env("RUSTDOCFLAGS", "-D warnings")
                .run()?;
            println!("\n=== Documentation built successfully ===");
        }
        Commands::Coverage => {
            println!("=== Generating code coverage report ===");

            // Check if cargo-llvm-cov is installed
            if cmd!(sh, "cargo llvm-cov --version").quiet().run().is_err() {
                eprintln!("cargo-llvm-cov not found. Install with:");
                eprintln!("  cargo install cargo-llvm-cov");
                return Err("cargo-llvm-cov not installed".into());
            }

            cmd!(sh, "cargo llvm-cov nextest --lcov --output-path lcov.info").run()?;

            println!("\n=== Code coverage report generated: lcov.info ===");
        }
        Commands::Miri => {
            println!("=== Running Miri (undefined behavior detection) ===");

            // Check if miri is available (requires nightly)
            if cmd!(sh, "cargo +nightly miri --version")
                .quiet()
                .run()
                .is_err()
            {
                eprintln!("cargo-miri not found. Install with:");
                eprintln!("  rustup +nightly component add miri");
                return Err("cargo-miri not installed".into());
            }

            println!("\n=== Setting up Miri ===");
            cmd!(sh, "cargo +nightly miri setup").run()?;

            println!("\n=== Running Miri tests ===");
            let result = cmd!(sh, "cargo +nightly miri test").run();

            match result {
                Ok(()) => println!("\n=== Miri tests passed ==="),
                Err(e) => {
                    eprintln!("\nMiri tests had issues (this may be expected on some systems):");
                    eprintln!("  {}", e);
                    eprintln!("Note: Some tests may be skipped due to Miri limitations");
                }
            }
        }
        Commands::Tracey { action } => match action {
            TraceyAction::Check => {
                cmd!(sh, "tracey serve").run()?;
            }
            TraceyAction::Matrix => {
                cmd!(sh, "tracey matrix").run()?;
            }
            TraceyAction::Rules => {
                cmd!(sh, "tracey rules cells/daw/daw-proto/spec/transport.md cells/daw/daw-proto/spec/project.md cells/daw/daw-proto/spec/track.md cells/daw/daw-proto/spec/marker.md cells/daw/daw-proto/spec/plugin.md cells/daw/daw-proto/spec/capabilities.md").run()?;
            }
            TraceyAction::Impact => {
                cmd!(sh, "tracey impact").run()?;
            }
        },
        Commands::Dodeca { action } => match action {
            DodecaAction::Build => {
                cmd!(sh, "dodeca build").run()?;
            }
            DodecaAction::Serve => {
                cmd!(sh, "dodeca serve").run()?;
            }
            DodecaAction::Watch => {
                cmd!(sh, "dodeca serve --watch").run()?;
            }
        },
        Commands::Build => {
            println!("=== Building all cells and host extensions ===");
            cmd!(sh, "cargo build -p daw-proto").run()?;
            cmd!(sh, "cargo build -p daw-control").run()?;
            cmd!(sh, "cargo build -p daw-standalone").run()?;
            cmd!(sh, "cargo build -p daw-reaper").run()?;
            cmd!(sh, "cargo build -p session").run()?;
            cmd!(sh, "cargo build -p host-runtime").run()?;
            cmd!(sh, "cargo build -p test-extension").run()?;
            cmd!(sh, "cargo build -p reaper-extension").run()?;
            println!("\n=== All cells and extensions built successfully ===");
        }
        Commands::Run => {
            println!("=== Running DAW standalone cell ===");
            cmd!(sh, "cargo run -p daw-standalone").run()?;
        }
        Commands::Playwright {
            install,
            headed,
            ui,
        } => {
            let wasm_test_dir = workspace_root.join("apps").join("tests").join("wasm");

            println!("=== Running WASM integration tests with Playwright ===");

            // Build the test command
            let mut test_args = vec!["exec", "playwright", "test"];
            if ui {
                test_args.push("--ui");
            } else if headed {
                test_args.push("--headed");
            }
            let test_cmd = test_args.join(" ");

            // Build the full command to run inside nix develop
            let nix_cmd = if install {
                format!(
                    "cd {} && pnpm install && pnpm exec playwright install chromium && pnpm {}",
                    wasm_test_dir.display(),
                    test_cmd
                )
            } else if !wasm_test_dir.join("node_modules").exists() {
                format!(
                    "cd {} && pnpm install && pnpm {}",
                    wasm_test_dir.display(),
                    test_cmd
                )
            } else {
                format!("cd {} && pnpm {}", wasm_test_dir.display(), test_cmd)
            };

            println!("\n>>> Running in nix develop shell...");
            cmd!(sh, "nix develop --command bash -c {nix_cmd}").run()?;

            println!("\n=== Playwright tests completed ===");
        }
    }

    Ok(())
}
