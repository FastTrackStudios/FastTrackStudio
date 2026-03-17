//! xtask: Development tasks for FastTrackStudio
//!
//! Run with: `cargo xtask <command>`

use std::process::ExitCode;

use facet::Facet;
use figue as args;
use signal::catalog;
use xshell::{Shell, cmd};

mod icon_gen;

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
    /// Run native integration tests (spawns test-extension)
    Integration,
    /// Run REAPER integration tests (spawns REAPER, runs #[reaper_test] tests)
    ReaperTest {
        /// Specific test name filter (passed to cargo test as filter)
        #[facet(args::positional, default)]
        filter: Option<String>,
        /// Skip building the extension before running tests
        #[facet(args::named, default)]
        no_build: bool,
        /// Keep REAPER open after tests complete (for inspecting results)
        #[facet(args::named, default)]
        keep_open: bool,
    },
    /// Scan all Neural DSP preset libraries and write a structured catalogue
    Catalog {
        /// Output directory for the catalogue (default: $FTS_HOME/Library)
        #[facet(args::positional, default)]
        output: Option<String>,
    },
    /// Create wrapper .app bundles for each signal rig type (dock names + icons)
    SetupRigs {
        /// Force re-creation of all bundles even if they already exist
        #[facet(args::named, default)]
        force: bool,
    },
    /// Migrate signal.db presets to REAPER-native FXChains/TrackTemplates directories
    Migrate {
        /// Dry-run mode: show what would be done without writing files
        #[facet(args::named, default)]
        dry_run: bool,
    },
    /// Package the FTS library into a .tar.gz for the installer
    PackageLibrary {
        /// Output file path (default: fts-library.tar.gz)
        #[facet(args::positional, default)]
        output: Option<String>,
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
        Commands::ReaperTest { filter, no_build, keep_open } => {
            println!("=== Running REAPER integration tests ===");

            // Platform-aware defaults
            let (default_reaper_path, default_reaper_exe, default_reaper_resources, ext_filename) =
                if cfg!(target_os = "macos") {
                    let reaper = utils::paths::reaper_dir();
                    let live_app = reaper.join("FTS-LIVE.app");
                    (
                        reaper.to_string_lossy().to_string(),
                        live_app.join("Contents/MacOS/REAPER").to_string_lossy().to_string(),
                        live_app.join("Contents/Resources").to_string_lossy().to_string(),
                        "libreaper_fts.dylib",
                    )
                } else {
                    // Linux: REAPER config in ~/.config/REAPER, executable on PATH
                    let home = std::env::var("HOME").unwrap_or_else(|_| "/tmp".to_string());
                    let config_dir = format!("{home}/.config/REAPER/");
                    (
                        config_dir.clone(),
                        "reaper".to_string(),
                        config_dir,
                        "libreaper_fts.so",
                    )
                };

            // Step 1: Build extension (unless --no-build)
            if !no_build {
                println!("\n>>> Building REAPER extension...");
                cmd!(sh, "cargo build -p reaper-extension").run()?;

                // Copy dylib/so to REAPER's UserPlugins
                let reaper_path = std::env::var("REAPER_PATH")
                    .unwrap_or_else(|_| default_reaper_path.clone());
                let plugins_dir = std::path::PathBuf::from(&reaper_path).join("UserPlugins");
                std::fs::create_dir_all(&plugins_dir)?;

                let dylib_src = workspace_root.join(format!("target/debug/{ext_filename}"));
                let dylib_dst = plugins_dir.join(ext_filename);
                if dylib_src.exists() {
                    // Use symlink if not already linked
                    if dylib_dst.exists() || dylib_dst.is_symlink() {
                        std::fs::remove_file(&dylib_dst)?;
                    }
                    #[cfg(unix)]
                    std::os::unix::fs::symlink(&dylib_src, &dylib_dst)?;
                    println!("  Linked extension to {}", dylib_dst.display());
                } else {
                    println!(
                        "  Warning: {} not found, skipping copy",
                        dylib_src.display()
                    );
                }

                // Step 1b: Build and install fts-macros CLAP plugin.
                // The extension eagerly loads fts-macros.clap on startup,
                // so it must be present before REAPER launches.
                let fts_plugins_dir = workspace_root
                    .parent()
                    .map(|p| p.join("fts-plugins"))
                    .unwrap_or_else(|| workspace_root.join("../fts-plugins"));
                if fts_plugins_dir.exists() {
                    println!("\n>>> Building fts-macros CLAP plugin...");
                    let sh_plugins = Shell::new()?;
                    sh_plugins.change_dir(&fts_plugins_dir);
                    cmd!(sh_plugins, "cargo run --package xtask -- bundle fts-macros --release")
                        .run()?;

                    // Install .clap bundle to REAPER's FX directory
                    let fx_dir = plugins_dir.join("FX");
                    std::fs::create_dir_all(&fx_dir)?;
                    let clap_src = fts_plugins_dir.join("target/bundled/fts-macros.clap");
                    let clap_dst = fx_dir.join("fts-macros.clap");
                    if clap_src.exists() {
                        if clap_dst.exists() {
                            std::fs::remove_dir_all(&clap_dst)?;
                        }
                        copy_dir_recursive(&clap_src, &clap_dst)?;
                        println!("  Installed fts-macros.clap to {}", fx_dir.display());
                    } else {
                        println!(
                            "  Warning: fts-macros.clap not found at {}",
                            clap_src.display()
                        );
                    }
                } else {
                    println!(
                        "\n>>> Skipping fts-macros (fts-plugins not found at {})",
                        fts_plugins_dir.display()
                    );
                }
            }

            // Step 2: Pre-build test binary (before spawning REAPER so it's ready to run)
            // When a filter is given, only build matching binaries to save time.
            let pre_build_bins = filter.as_ref().map(|f| find_matching_test_binaries(f));
            println!("\n>>> Pre-building test binary...");
            if let Some(ref bins) = pre_build_bins {
                if !bins.is_empty() {
                    let mut c = cmd!(sh, "cargo test -p signal --features daw");
                    for bin in bins {
                        c = c.arg("--test").arg(bin);
                    }
                    c.arg("--no-run").run()?;
                } else {
                    cmd!(sh, "cargo test -p signal --features daw --no-run").run()?;
                }
            } else {
                cmd!(sh, "cargo test -p signal --features daw --no-run").run()?;
            }

            // Clean up stale sockets from crashed/killed sessions.
            // Try connecting to each socket — if nothing is listening, it's stale.
            if let Ok(entries) = std::fs::read_dir("/tmp") {
                for entry in entries.flatten() {
                    let path = entry.path();
                    if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                        if name.starts_with("fts-daw-") && name.ends_with(".sock") {
                            match std::os::unix::net::UnixStream::connect(&path) {
                                Ok(_) => {} // Something is listening — leave it alone
                                Err(_) => {
                                    println!("  Removing stale socket: {name}");
                                    let _ = std::fs::remove_file(&path);
                                }
                            }
                        }
                    }
                }
            }

            // Step 3: Spawn REAPER (empty project, no splash)
            println!("\n>>> Spawning REAPER...");
            let reaper_exe = std::env::var("REAPER_EXECUTABLE")
                .unwrap_or(default_reaper_exe);
            let reaper_resources = std::env::var("REAPER_RESOURCES")
                .unwrap_or(default_reaper_resources);

            // Use a session-specific socket path so multiple xtask runs don't collide.
            // We use the xtask PID (guaranteed unique while we're running) rather than
            // REAPER's PID, because REAPER with -newinst may re-exec with a new PID on
            // macOS, causing a mismatch between what xtask expects and what the extension
            // creates.
            let session_id = std::process::id();
            let socket_path = format!("/tmp/fts-daw-{session_id}.sock");
            let _ = std::fs::remove_file(&socket_path);

            let mut reaper_child = std::process::Command::new(&reaper_exe)
                .current_dir(&reaper_resources)
                .env("FTS_SOCKET", &socket_path)
                .arg("-newinst")
                .arg("-nosplash")
                .arg("-ignoreerrors")
                .spawn()
                .map_err(|e| format!("Failed to spawn REAPER at {reaper_exe}: {e}"))?;
            let reaper_pid = reaper_child.id();
            println!("  Spawned REAPER (PID {reaper_pid}), socket: {socket_path}");
            println!("  Extension log: /tmp/fts-reaper.log");

            // Wait for socket
            let socket = std::path::Path::new(&socket_path);
            let deadline = std::time::Instant::now() + std::time::Duration::from_secs(30);
            print!("  Waiting for socket");
            while !socket.exists() {
                if std::time::Instant::now() > deadline {
                    println!();
                    // Dump extension log if available
                    if let Ok(log) = std::fs::read_to_string("/tmp/fts-reaper.log") {
                        let lines: Vec<&str> = log.lines().collect();
                        if !lines.is_empty() {
                            println!("\n>>> Extension log (last 20 lines):");
                            for line in lines.iter().rev().take(20).rev() {
                                println!("  {line}");
                            }
                        }
                    }
                    let _ = reaper_child.kill();
                    let _ = reaper_child.wait();
                    return Err("Timed out waiting for REAPER socket".into());
                }
                std::thread::sleep(std::time::Duration::from_millis(500));
                print!(".");
            }
            println!("\n  Socket ready");

            // Step 4: Prepare per-test log directory
            let log_dir = std::path::PathBuf::from("/tmp/reaper-tests");
            if log_dir.exists() {
                let _ = std::fs::remove_dir_all(&log_dir);
            }
            std::fs::create_dir_all(&log_dir)?;
            println!("  Log directory: {}", log_dir.display());

            // Step 5: Run REAPER tests (parallel with limited concurrency)
            // Each test gets its own project tab; limit threads to avoid overwhelming
            // REAPER's main thread with too many concurrent plugin loads.
            // Set FTS_SOCKET so tests connect to the right REAPER instance
            sh.set_var("FTS_SOCKET", &socket_path);
            // Tell reaper-test to skip project tab cleanup when --keep-open
            if keep_open {
                sh.set_var("FTS_KEEP_OPEN", "1");
            }

            println!("\n>>> Running tests...");
            let test_result = if let Some(ref f) = filter {
                // When a filter is given, use the matching binaries found
                // during pre-build so we don't spawn all 25+ test binaries.
                let test_bins = pre_build_bins.as_ref().cloned().unwrap_or_default();
                if test_bins.is_empty() {
                    // No binary name matched — fall back to running all
                    // binaries (the filter string may match a function name
                    // inside a differently-named file).
                    cmd!(
                        sh,
                        "cargo test -p signal --features daw -- --ignored --nocapture --test-threads=4 {f}"
                    )
                    .run()
                } else {
                    let mut c = cmd!(sh, "cargo test -p signal --features daw");
                    for bin in &test_bins {
                        c = c.arg("--test").arg(bin);
                    }
                    c = c.arg("--").arg("--ignored").arg("--nocapture").arg("--test-threads=4").arg(f);
                    c.run()
                }
            } else {
                cmd!(
                    sh,
                    "cargo test -p signal --features daw -- --ignored --nocapture --test-threads=4"
                )
                .run()
            };

            // Step 6: Kill REAPER (unless --keep-open)
            if keep_open {
                println!("\n>>> Keeping REAPER open (PID {reaper_pid})");
                println!("  Socket: {socket_path}");
                println!("  Kill manually with: kill {reaper_pid}");
            } else {
                println!("\n>>> Stopping REAPER (PID {reaper_pid})...");
                let _ = reaper_child.kill();
                let _ = reaper_child.wait();
                let _ = std::fs::remove_file(socket_path);
            }

            // Step 7: On failure, summarize per-test log files
            if test_result.is_err() {
                println!("\n>>> Test logs (non-empty):");
                if let Ok(entries) = std::fs::read_dir(&log_dir) {
                    let mut found_logs = false;
                    for entry in entries.flatten() {
                        let path = entry.path();
                        if path.extension().map(|e| e == "log").unwrap_or(false) {
                            if let Ok(meta) = path.metadata() {
                                if meta.len() > 0 {
                                    found_logs = true;
                                    println!("  {} ({} bytes)", path.display(), meta.len());
                                }
                            }
                        }
                    }
                    if !found_logs {
                        println!("  (no log files found)");
                    }
                }
            }

            test_result?;
            println!("\n=== REAPER integration tests passed ===");
        }
        Commands::Integration => {
            println!("=== Running native integration tests ===");

            // First build test-extension and all cells it needs
            println!("\n>>> Building test-extension and cells...");
            cmd!(
                sh,
                "cargo build -p test-extension -p daw-standalone -p session"
            )
            .run()?;

            // Run the integration tests
            println!("\n>>> Running integration tests...");
            if cmd!(sh, "cargo nextest --version").quiet().run().is_ok() {
                cmd!(
                    sh,
                    "cargo nextest run -p integration-tests --test extension_tests"
                )
                .run()?;
            } else {
                cmd!(sh, "cargo test -p integration-tests --test extension_tests").run()?;
            }

            println!("\n=== Integration tests passed ===");
        }
        Commands::Catalog { output } => {
            run_catalog(output)?;
        }
        Commands::SetupRigs { force } => {
            run_setup_rigs(force)?;
        }
        Commands::Migrate { dry_run } => {
            run_migrate(dry_run)?;
        }
        Commands::PackageLibrary { output } => {
            run_package_library(output)?;
        }
    }

    Ok(())
}

// ============================================================================
// package-library: Create a .tar.gz of the FTS library for the installer
// ============================================================================

fn run_package_library(output: Option<String>) -> Result<(), Box<dyn std::error::Error>> {
    let fts_home = utils::paths::fts_home();
    let output_path = output.unwrap_or_else(|| "fts-library.tar.gz".to_string());

    println!("=== Packaging FTS Library ===");
    println!("  Source: {}", fts_home.display());
    println!("  Output: {output_path}");

    // Collect the directories to include (relative to fts_home)
    let includes = [
        "Library/blocks",
        "Library/presets",
        "Library/profiles",
        "Library/FTS",
        "Library/FTS-GUIDE",
        "Library/catalog.json",
        "Reaper/FXChains",
        "Reaper/TrackTemplates",
    ];

    // Build tar arguments — only include paths that exist
    let mut args = vec!["czf".to_string(), output_path.clone()];
    args.push("-C".to_string());
    args.push(fts_home.to_string_lossy().to_string());

    let mut included = 0;
    for path in &includes {
        let full = fts_home.join(path);
        if full.exists() {
            args.push(path.to_string());
            included += 1;
            println!("  + {path}");
        } else {
            println!("  - {path} (not found, skipping)");
        }
    }

    if included == 0 {
        return Err("No library files found to package".into());
    }

    let status = std::process::Command::new("tar")
        .args(&args)
        .status()?;

    if !status.success() {
        return Err(format!("tar failed with status {status}").into());
    }

    // Show size
    let meta = std::fs::metadata(&output_path)?;
    let size_mb = meta.len() as f64 / 1_048_576.0;
    println!("\nCreated {output_path} ({size_mb:.1} MB, {included} entries)");
    println!("Upload with: gh release create library-v1 {output_path} --repo FastTrackStudios/fts-library");

    Ok(())
}

// ============================================================================
// setup-rigs: Create wrapper .app bundles for each signal rig type
// ============================================================================

/// Default config path.
fn default_rigs_config() -> String {
    utils::paths::reaper_dir()
        .join("fts-rigs.json")
        .to_string_lossy()
        .to_string()
}

/// JSON-serializable rig configuration (lives in fts-rigs.json).
#[derive(serde::Deserialize)]
#[serde(crate = "serde")]
struct RigsConfig {
    /// Base directory where wrapper .app bundles and reaper.ini live.
    base_dir: String,
    /// Name of the real REAPER .app bundle (e.g., "FTS-LIVE.app").
    reaper_app: String,
    /// Path to the base .icns icon used for tinting.
    base_icon: String,
    /// Directories to place Finder aliases in.
    alias_dirs: Vec<String>,
    /// Wrapper app definitions.
    wrappers: Vec<WrapperAppConfig>,
}

#[derive(serde::Deserialize)]
struct WrapperAppConfig {
    app_name: String,
    role: String,
    #[serde(default)]
    rig_type: Option<String>,
    /// Theme path relative to base_dir (e.g., "ColorThemes/MyDaw").
    theme: String,
    #[serde(default = "default_true")]
    icon: bool,
}

fn default_true() -> bool {
    true
}

impl RigsConfig {
    fn reaper_executable(&self) -> std::path::PathBuf {
        std::path::PathBuf::from(&self.base_dir)
            .join(&self.reaper_app)
            .join("Contents/MacOS/REAPER")
    }

    fn resources_dir(&self) -> String {
        std::path::PathBuf::from(&self.base_dir)
            .join(&self.reaper_app)
            .join("Contents/Resources")
            .to_string_lossy()
            .to_string()
    }

    fn ini_path(&self) -> String {
        std::path::PathBuf::from(&self.base_dir)
            .join("reaper.ini")
            .to_string_lossy()
            .to_string()
    }

    /// Resolve a theme path (relative to base_dir or absolute).
    fn resolve_theme(&self, theme: &str) -> String {
        let p = std::path::Path::new(theme);
        if p.is_absolute() {
            theme.to_string()
        } else {
            std::path::PathBuf::from(&self.base_dir)
                .join(theme)
                .to_string_lossy()
                .to_string()
        }
    }

    /// Resolve an alias dir (expands ~).
    fn resolve_alias_dir(dir: &str) -> std::path::PathBuf {
        if let Some(rest) = dir.strip_prefix("~/") {
            let home = std::env::var("HOME").unwrap_or_default();
            std::path::PathBuf::from(home).join(rest)
        } else {
            std::path::PathBuf::from(dir)
        }
    }
}

fn run_setup_rigs(force: bool) -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Setting up wrapper .app bundles ===");

    // Load config
    let rigs_config_path = default_rigs_config();
    let config_path = std::path::Path::new(&rigs_config_path);
    if !config_path.exists() {
        return Err(format!(
            "Rigs config not found: {}\nCreate it or run from the Reaper directory.",
            rigs_config_path
        )
        .into());
    }
    let config_str = std::fs::read_to_string(config_path)?;
    let config: RigsConfig = serde_json::from_str(&config_str)
        .map_err(|e| format!("Failed to parse {}: {e}", rigs_config_path))?;

    println!("  Config: {}", rigs_config_path);

    let base_dir = &config.base_dir;
    let base_icon = std::path::Path::new(&config.base_icon);
    if !base_icon.exists() {
        return Err(format!("Base icon not found: {}", config.base_icon).into());
    }

    let reaper_exe = config.reaper_executable();
    if !reaper_exe.exists() {
        return Err(format!("REAPER binary not found: {}", reaper_exe.display()).into());
    }

    // Build the reaper-launcher binary (release for small size)
    print!("  Building reaper-launcher...");
    let build_status = std::process::Command::new("cargo")
        .args(["build", "-p", "reaper-launcher", "--release", "--bin", "reaper-launcher"])
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::piped())
        .status()?;
    if !build_status.success() {
        return Err("Failed to build reaper-launcher".into());
    }
    println!(" OK");

    // Find the built binary
    let workspace_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let launcher_bin = workspace_root.join("target/release/reaper-launcher");
    if !launcher_bin.exists() {
        return Err(format!("Launcher binary not found: {}", launcher_bin.display()).into());
    }

    // Timestamp-based version busts macOS icon cache on each run
    let version = format!(
        "1.0.{}",
        std::time::SystemTime::now()
            .duration_since(std::time::SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_secs()
    );

    let resources_dir_live = config.resources_dir();
    let ini_path = config.ini_path();

    for wrapper in &config.wrappers {
        let app_name = &wrapper.app_name;
        let bundle_dir =
            std::path::PathBuf::from(base_dir).join(format!("{app_name}.app"));
        let contents_dir = bundle_dir.join("Contents");
        let macos_dir = contents_dir.join("MacOS");
        let resources_dir = contents_dir.join("Resources");
        let wrapper_exe = macos_dir.join("REAPER");
        let plist_path = contents_dir.join("Info.plist");

        // Skip if already set up (unless --force)
        if !force && wrapper_exe.exists() && plist_path.exists() {
            println!("  SKIP {}.app (already exists, use --force to recreate)", app_name);
            continue;
        }

        print!("  {}.app ...", app_name);

        // Clean up existing bundle if forcing
        if force && bundle_dir.exists() {
            std::fs::remove_dir_all(&bundle_dir)?;
        }

        // Create directory structure
        std::fs::create_dir_all(&macos_dir)?;
        std::fs::create_dir_all(&resources_dir)?;

        // Write launch.json — fully editable without recompiling
        let theme_full_path = config.resolve_theme(&wrapper.theme);
        let is_live_instance = wrapper.role != "testing";
        let launch_config = reaper_launcher::LaunchConfig {
            role: wrapper.role.clone(),
            rig_type: wrapper.rig_type.clone(),
            reaper_executable: reaper_exe.to_string_lossy().to_string(),
            resources_dir: resources_dir_live.clone(),
            ini_path: ini_path.clone(),
            ini_overrides: reaper_launcher::ReaperIniConfig {
                undo_max_mem: if is_live_instance { Some(0) } else { None },
                theme: Some(theme_full_path),
            },
            restore_ini_after_launch: is_live_instance,
        };
        launch_config
            .save(&contents_dir.join("launch.json"))
            .map_err(|e| format!("Failed to write launch.json: {e}"))?;

        // Copy the pre-built reaper-launcher binary as the bundle executable
        std::fs::copy(&launcher_bin, &wrapper_exe).map_err(|e| {
            format!("Failed to copy launcher binary into {}.app: {e}", app_name)
        })?;

        // Write Info.plist
        let bundle_id_suffix = wrapper.rig_type.as_deref()
            .unwrap_or(&wrapper.role)
            .replace('-', "");
        let bundle_id = format!("com.fasttrackstudio.{}", bundle_id_suffix);
        let plist_content = format!(
            r#"<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleName</key>
    <string>{app_name}</string>
    <key>CFBundleDisplayName</key>
    <string>{app_name}</string>
    <key>CFBundleIdentifier</key>
    <string>{bundle_id}</string>
    <key>CFBundleExecutable</key>
    <string>REAPER</string>
    <key>CFBundlePackageType</key>
    <string>APPL</string>
    <key>CFBundleVersion</key>
    <string>{version}</string>
    <key>CFBundleShortVersionString</key>
    <string>{version}</string>
    <key>LSUIElement</key>
    <false/>
    <key>CFBundleIconFile</key>
    <string>main-mac</string>
    <key>NSHighResolutionCapable</key>
    <true/>
</dict>
</plist>"#
        );
        std::fs::write(&plist_path, plist_content)?;

        // Generate tinted + badged icon
        if wrapper.icon {
            let icon_key = wrapper.rig_type.as_deref().unwrap_or(&wrapper.role);
            let icon_path = resources_dir.join("main-mac.icns");
            match icon_gen::generate_rig_icon(base_icon, &icon_path, icon_key) {
                Ok(()) => {}
                Err(e) => {
                    println!(" icon failed: {e}");
                }
            }
        }

        // Ad-hoc sign the launcher binary so macOS doesn't block it
        let _ = std::process::Command::new("codesign")
            .args(["--force", "--sign", "-"])
            .arg(&bundle_dir)
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .status();

        // Force LaunchServices to re-register this bundle so the new icon
        // and bundle identity take effect without a reboot.
        let _ = std::process::Command::new(
            "/System/Library/Frameworks/CoreServices.framework/\
             Frameworks/LaunchServices.framework/Support/lsregister",
        )
        .args(["-f"])
        .arg(&bundle_dir)
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status();

        println!(" OK");
    }

    // Create macOS Finder aliases in configured directories
    let alias_dirs: Vec<std::path::PathBuf> = config
        .alias_dirs
        .iter()
        .map(|d| RigsConfig::resolve_alias_dir(d))
        .collect();
    for dir in &alias_dirs {
        std::fs::create_dir_all(dir)?;
    }

    // Clean alias dirs of any previous FTS aliases before recreating
    for dir in &alias_dirs {
        if let Ok(entries) = std::fs::read_dir(dir) {
            for entry in entries.flatten() {
                let name = entry.file_name().to_string_lossy().to_string();
                if name.starts_with("FTS-") {
                    let _ = std::fs::remove_file(entry.path());
                    let _ = std::fs::remove_dir_all(entry.path());
                }
            }
        }
    }

    print!("  Creating aliases...");
    for wrapper in &config.wrappers {
        let app_name = &wrapper.app_name;
        let bundle_dir =
            std::path::PathBuf::from(base_dir).join(format!("{app_name}.app"));

        for dir in &alias_dirs {
            // Use osascript to create a Finder alias
            let _ = std::process::Command::new("osascript")
                .args([
                    "-e",
                    &format!(
                        r#"tell application "Finder" to make alias file to POSIX file "{}" at POSIX file "{}""#,
                        bundle_dir.display(),
                        dir.display(),
                    ),
                ])
                .stdout(std::process::Stdio::null())
                .stderr(std::process::Stdio::null())
                .status();
        }
    }
    println!(" OK");

    // Generate and apply a custom icon to each alias folder
    for dir in &alias_dirs {
        print!("  Setting folder icon for {}...", dir.display());
        let folder_icns = dir.join(".fts-folder-icon.icns");
        match icon_gen::generate_rig_icon(base_icon, &folder_icns, "fts-folder") {
            Ok(()) => {
                let _ = std::process::Command::new("osascript")
                    .args([
                        "-e",
                        &format!(
                            r#"use framework "AppKit"
set iconImage to current application's NSImage's alloc()'s initWithContentsOfFile:"{}"
current application's NSWorkspace's sharedWorkspace()'s setIcon:iconImage forFile:"{}" options:0"#,
                            folder_icns.display(),
                            dir.display(),
                        ),
                    ])
                    .stdout(std::process::Stdio::null())
                    .stderr(std::process::Stdio::null())
                    .status();
                println!(" OK");
            }
            Err(e) => println!(" failed: {e}"),
        }
    }

    // Restart Dock and Finder so updated icons appear immediately
    let _ = std::process::Command::new("killall")
        .arg("Dock")
        .status();
    let _ = std::process::Command::new("killall")
        .arg("Finder")
        .status();

    println!("\n=== Setup complete ===");
    println!("Wrapper bundles: {}", base_dir);
    for dir in &alias_dirs {
        println!("Aliases: {}", dir.display());
    }

    Ok(())
}

fn run_catalog(output: Option<String>) -> Result<(), Box<dyn std::error::Error>> {
    let library_dir = match output {
        Some(dir) => std::path::PathBuf::from(dir),
        None => utils::paths::library_dir(),
    };

    println!("=== Neural DSP Block Catalogue ===");
    println!("Output: {}", library_dir.display());

    // Layout: Library/blocks/plugin/<manufacturer>/<plugin-slug>/snapshots/<folder>/
    let blocks_dir = library_dir.join("blocks/plugin/neural-dsp");
    std::fs::create_dir_all(&blocks_dir)?;

    let mut catalog_plugins = Vec::new();
    let mut total_snapshots = 0usize;
    let mut total_blocks = 0usize;

    for plugin in catalog::NDSP_PLUGINS {
        let lib_path = plugin.disk_library_path();
        if !lib_path.exists() {
            println!("  SKIP {} (not installed)", plugin.name);
            continue;
        }

        // Scan disk preset library
        let presets = catalog::scan_preset_library(&lib_path);
        if presets.is_empty() {
            println!("  SKIP {} (0 presets found)", plugin.name);
            continue;
        }

        total_blocks += 1;
        total_snapshots += presets.len();

        // Create block directory: blocks/plugin/neural-dsp/<plugin-slug>/snapshots/
        let block_dir = blocks_dir.join(plugin.slug);
        let snapshots_dir = block_dir.join("snapshots");
        std::fs::create_dir_all(&snapshots_dir)?;

        // Collect folders (category hierarchy)
        let mut folders: Vec<String> = presets
            .iter()
            .map(|p| p.category.clone())
            .collect::<std::collections::BTreeSet<_>>()
            .into_iter()
            .collect();
        folders.sort();

        // Write snapshot files
        for preset in &presets {
            let folder_dir = if preset.category.is_empty() {
                snapshots_dir.clone()
            } else {
                snapshots_dir.join(&preset.category)
            };
            std::fs::create_dir_all(&folder_dir)?;

            // Use the original filename (sans .xml extension) to preserve
            // spaces, apostrophes, etc.
            let original_stem = preset
                .source_path
                .file_stem()
                .map(|s| s.to_string_lossy().to_string())
                .unwrap_or_else(|| preset.name.clone());
            if original_stem.is_empty() {
                continue;
            }

            // Write binary state file (copy of the original)
            let bin_path = folder_dir.join(format!("{original_stem}.bin"));
            std::fs::copy(&preset.source_path, &bin_path)?;

            // Write snapshot JSON metadata
            // Check if a REAPER chunk file already exists (from a previous harvest)
            let chunk_filename = format!("{original_stem}.chunk");
            let reaper_chunk_file = if folder_dir.join(&chunk_filename).exists() {
                Some(chunk_filename)
            } else {
                None
            };

            let meta = catalog::SnapshotMetadata {
                name: preset.name.clone(),
                id: catalog::slugify(&preset.name),
                block: plugin.slug.to_string(),
                folder: preset.category.clone(),
                tags: preset.tags.clone(),
                preset_uid: None,
                midi_cycle_index: None,
                state_file: format!("{original_stem}.bin"),
                reaper_chunk_file,
                fingerprint: preset.fingerprint.clone(),
            };
            let json_path = folder_dir.join(format!("{original_stem}.json"));
            let json = serde_json::to_string_pretty(&meta)?;
            std::fs::write(&json_path, json)?;
        }

        // Write block.json
        let block_meta = catalog::BlockMetadata {
            name: plugin.name.to_string(),
            manufacturer: "Neural DSP".to_string(),
            slug: plugin.slug.to_string(),
            binary_id: plugin.binary_id.to_string(),
            format: "ndsp-juce-binary".to_string(),
            disk_library_path: lib_path.to_string_lossy().to_string(),
            total_snapshots: presets.len(),
            folders: folders.clone(),
        };
        let block_json = serde_json::to_string_pretty(&block_meta)?;
        std::fs::write(block_dir.join("block.json"), block_json)?;

        catalog_plugins.push(catalog::CatalogPlugin {
            name: plugin.name.to_string(),
            manufacturer: "Neural DSP".to_string(),
            slug: plugin.slug.to_string(),
            binary_id: plugin.binary_id.to_string(),
            disk_library_path: lib_path.to_string_lossy().to_string(),
            total_snapshots: presets.len(),
            folders,
        });

        println!("  OK {} — {} snapshots", plugin.name, presets.len());
    }

    // Write top-level catalog.json
    let cat = catalog::Catalog {
        version: 1,
        generated: chrono_now(),
        plugins: catalog_plugins,
    };
    let catalog_json = serde_json::to_string_pretty(&cat)?;
    std::fs::write(library_dir.join("catalog.json"), catalog_json)?;

    println!("\n=== Catalogue complete ===");
    println!("  Blocks:    {}", total_blocks);
    println!("  Snapshots: {}", total_snapshots);
    println!("  Output:    {}", library_dir.display());

    Ok(())
}

/// Simple ISO-8601 timestamp without pulling in chrono.
/// Recursively copy a directory tree (for .clap bundles).
fn copy_dir_recursive(src: &std::path::Path, dst: &std::path::Path) -> std::io::Result<()> {
    std::fs::create_dir_all(dst)?;
    for entry in std::fs::read_dir(src)? {
        let entry = entry?;
        let ty = entry.file_type()?;
        let src_path = entry.path();
        let dst_path = dst.join(entry.file_name());
        if ty.is_dir() {
            copy_dir_recursive(&src_path, &dst_path)?;
        } else {
            std::fs::copy(&src_path, &dst_path)?;
        }
    }
    Ok(())
}

fn chrono_now() -> String {
    use std::time::SystemTime;
    let now = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_secs();
    // Good enough for a generated timestamp
    format!("unix:{}", now)
}

/// Find integration test binary names that match the given filter string.
///
/// First checks filenames, then searches file contents for the filter as a
/// function name. Returns the file stems (binary names) so we can pass
/// `--test <name>` to cargo and avoid spawning all 25+ test binaries.
fn find_matching_test_binaries(filter: &str) -> Vec<String> {
    let tests_dir = std::path::Path::new("crates/signal/signal/tests");
    let Ok(entries) = std::fs::read_dir(tests_dir) else {
        return Vec::new();
    };

    let mut matches = Vec::new();
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().map(|e| e == "rs").unwrap_or(false) {
            let Some(stem) = path.file_stem().and_then(|s| s.to_str()) else {
                continue;
            };
            // Match against filename first (fast path)
            if stem.contains(filter) {
                matches.push(stem.to_string());
                continue;
            }
            // Then search file contents for `fn <filter>` as a function
            // definition (handles cases where the function name differs
            // from the file name). We check for `fn {filter}` to avoid
            // false positives from imports like `use reaper_test::...`.
            if let Ok(contents) = std::fs::read_to_string(&path) {
                let pattern = format!("fn {filter}");
                if contents.contains(&pattern) {
                    matches.push(stem.to_string());
                }
            }
        }
    }
    matches
}

// ============================================================================
// migrate: Export signal.db to REAPER-native FXChains/TrackTemplates
// ============================================================================

fn run_migrate(dry_run: bool) -> Result<(), Box<dyn std::error::Error>> {
    use signal::{BlockRepo, ModuleRepo};
    use signal_storage::sidecar::{self, PresetKind, SidecarParam, SignalSidecar};

    let library_path = utils::paths::library_dir();
    let reaper = utils::paths::reaper_dir();
    let fxchains_root = utils::paths::reaper_fxchains().join("FTS-Signal");
    let blocks_dir = fxchains_root.join("01-Blocks");
    let modules_dir = fxchains_root.join("02-Modules");

    let db_path = library_path.join("signal.db");
    if !db_path.exists() {
        return Err(format!("signal.db not found at {}", db_path.display()).into());
    }

    println!("=== Signal → REAPER-Native Migration ===");
    println!("  Source DB:    {}", db_path.display());
    println!("  FXChains:    {}", fxchains_root.display());
    if dry_run {
        println!("  Mode:        DRY RUN (no files will be written)");
    }
    println!();

    // Connect to the DB and read all presets
    let runtime = tokio::runtime::Runtime::new()?;
    let db_url = format!("sqlite:{}?mode=ro", db_path.display());
    let db = runtime.block_on(signal_storage::Database::connect(&db_url))?;

    // ── Phase 1: Block presets → FXChains/FTS-Signal/01-Blocks/ ──

    println!("--- Block Presets ---");
    let block_repo = signal_storage::BlockRepoLive::new(db.clone());
    runtime.block_on(block_repo.init_schema())?;

    let mut block_count = 0usize;
    let mut snapshot_count = 0usize;

    for &block_type in signal::ALL_BLOCK_TYPES {
        let collections = runtime.block_on(block_repo.list_block_collections(block_type))?;
        if collections.is_empty() {
            continue;
        }

        // Map block type to folder name
        let category_folder = block_type_to_folder(block_type);
        let category_dir = blocks_dir.join(&category_folder);

        for collection in &collections {
            let metadata = collection.metadata();

            // Determine vendor subfolder from tags
            let vendor = extract_vendor(metadata);
            let collection_name = sanitize_filename(collection.name());

            // Build path: category/vendor/collection/ to avoid name collisions
            let collection_dir = if let Some(vendor) = &vendor {
                category_dir.join(vendor).join(&collection_name)
            } else {
                category_dir.join(&collection_name)
            };

            for snapshot in collection.snapshots() {
                let file_name = sanitize_filename(snapshot.name());
                let rfx_path = collection_dir.join(format!("{file_name}.RfxChain"));

                // Check if we have state_data (raw REAPER chunk)
                let has_state = snapshot.state_data().is_some();

                // Also check if there's an existing .RfxChain on disk in the old library
                let existing_rfx = find_existing_rfxchain(
                    &library_path,
                    collection.name(),
                    snapshot.name(),
                );

                if dry_run {
                    let source = if has_state {
                        "db:state_data"
                    } else if existing_rfx.is_some() {
                        "file:copy"
                    } else {
                        "skip:no-data"
                    };
                    let vendor_part = vendor.as_deref().map(|v| format!("{v}/")).unwrap_or_default();
                    println!(
                        "  [{source}] {category_folder}/{vendor_part}{collection_name}/{file_name}.RfxChain",
                    );
                } else {
                    std::fs::create_dir_all(&collection_dir)?;

                    // Write the .RfxChain file
                    let wrote = if let Some(data) = snapshot.state_data() {
                        std::fs::write(&rfx_path, data)?;
                        true
                    } else if let Some(existing) = &existing_rfx {
                        std::fs::copy(existing, &rfx_path)?;
                        true
                    } else {
                        false
                    };

                    if wrote {
                        // Write .signal.styx sidecar
                        let sc = SignalSidecar {
                            version: 1,
                            id: snapshot.id().to_string(),
                            kind: PresetKind::Block {
                                block_type: block_type.as_str().to_string(),
                            },
                            tags: metadata.tags.as_slice().to_vec(),
                            description: metadata.description.clone(),
                            parameters: snapshot
                                .block()
                                .parameters()
                                .iter()
                                .map(|p| SidecarParam {
                                    id: p.id().to_string(),
                                    name: p.name().to_string(),
                                    value: p.value().get() as f64,
                                })
                                .collect(),
                        };
                        sidecar::write_sidecar(&rfx_path, &sc)?;
                        snapshot_count += 1;
                    }
                }
            }
            block_count += 1;
        }
    }

    println!("  Exported: {block_count} collections, {snapshot_count} snapshots\n");

    // ── Phase 2: Module presets → FXChains/FTS-Signal/02-Modules/ ──

    println!("--- Module Presets ---");
    let module_repo = signal_storage::ModuleRepoLive::new(db.clone());
    runtime.block_on(module_repo.init_schema())?;

    let module_collections = runtime.block_on(module_repo.list_module_collections())?;
    let mut module_count = 0usize;

    for collection in &module_collections {
        let file_name = sanitize_filename(collection.name());
        let rfx_path = modules_dir.join(format!("{file_name}.RfxChain"));

        if dry_run {
            println!("  02-Modules/{file_name}.RfxChain  (sidecar only — no rfxchain data in DB)");
        } else {
            std::fs::create_dir_all(&modules_dir)?;

            // Modules in the DB don't have rfxchain state data, but we write the sidecar
            // so the scanner knows about them when rfxchain files are added later.
            let metadata = collection.metadata();
            let sc = SignalSidecar {
                version: 1,
                id: collection.id().to_string(),
                kind: PresetKind::Module,
                tags: metadata.tags.as_slice().to_vec(),
                description: metadata.description.clone(),
                parameters: vec![],
            };

            // Only write sidecar (no rfxchain file — modules need to be re-captured from REAPER)
            let sidecar_path = sidecar::sidecar_path(&rfx_path);
            let content = format!(
                "// Module preset: {} — re-capture from REAPER to create .RfxChain\n{}",
                collection.name(),
                render_sidecar_inline(&sc),
            );
            std::fs::write(&sidecar_path, content)?;
        }
        module_count += 1;
    }

    println!("  Exported: {module_count} module sidecars\n");

    // ── Phase 3: Copy existing .RfxChain files from Library/presets/ ──

    println!("--- Existing RfxChain Presets ---");
    let presets_dir = library_path.join("presets");
    let mut preset_rfx_count = 0usize;

    if presets_dir.is_dir() {
        for entry in std::fs::read_dir(&presets_dir)?.flatten() {
            let path = entry.path();
            if !path.is_dir() {
                continue;
            }
            let preset_name = path
                .file_name()
                .unwrap_or_default()
                .to_string_lossy()
                .to_string();
            let target_dir = blocks_dir.join("Custom").join(&preset_name);

            for rfx_entry in std::fs::read_dir(&path)?.flatten() {
                let rfx_path = rfx_entry.path();
                if rfx_path
                    .extension()
                    .map_or(true, |e| !e.eq_ignore_ascii_case("rfxchain"))
                {
                    continue;
                }

                let file_name = rfx_path.file_name().unwrap().to_string_lossy().to_string();
                let target_path = target_dir.join(&file_name);

                if dry_run {
                    println!("  [copy] Custom/{preset_name}/{file_name}");
                } else {
                    std::fs::create_dir_all(&target_dir)?;
                    std::fs::copy(&rfx_path, &target_path)?;

                    // Write a basic sidecar
                    let stem = rfx_path
                        .file_stem()
                        .unwrap_or_default()
                        .to_string_lossy()
                        .to_string();
                    let sc = SignalSidecar {
                        version: 1,
                        id: signal::seed_id(&format!("preset-{}-{}", preset_name, stem))
                            .to_string(),
                        kind: PresetKind::Block {
                            block_type: "custom".to_string(),
                        },
                        tags: vec![preset_name.clone()],
                        description: None,
                        parameters: vec![],
                    };
                    sidecar::write_sidecar(&target_path, &sc)?;
                }
                preset_rfx_count += 1;
            }
        }
    }

    println!("  Exported: {preset_rfx_count} preset rfxchain files\n");

    // ── Summary ──

    println!("=== Migration {} ===",
        if dry_run { "dry run complete" } else { "complete" }
    );
    println!("  FXChains root: {}", fxchains_root.display());
    println!("  Block presets: {block_count} ({snapshot_count} snapshots)");
    println!("  Module presets: {module_count}");
    println!("  Preset rfxchains: {preset_rfx_count}");

    if !dry_run {
        // Verify the scanner can read what we wrote
        println!("\n--- Verifying scanner ---");
        let scanned_blocks = signal_storage::seed_data::fxchains_scan::scan_blocks(&fxchains_root);
        let scanned_modules = signal_storage::seed_data::fxchains_scan::scan_modules(&fxchains_root);
        println!("  Scanner found: {} block presets, {} module presets",
            scanned_blocks.len(), scanned_modules.len());
    }

    Ok(())
}

/// Map BlockType to a human-readable folder name for the 01-Blocks/ tree.
fn block_type_to_folder(bt: signal::block::BlockType) -> String {
    match bt {
        signal::block::BlockType::Amp => "Amps".to_string(),
        signal::block::BlockType::Cabinet => "Cabinets".to_string(),
        signal::block::BlockType::Drive => "Drives".to_string(),
        signal::block::BlockType::Saturator => "Drives".to_string(),
        signal::block::BlockType::Boost => "Drives".to_string(),
        signal::block::BlockType::Reverb => "Reverbs".to_string(),
        signal::block::BlockType::Delay => "Delays".to_string(),
        signal::block::BlockType::Eq => "EQ".to_string(),
        signal::block::BlockType::Compressor => "Compression".to_string(),
        signal::block::BlockType::Gate => "Compression".to_string(),
        signal::block::BlockType::Limiter => "Compression".to_string(),
        signal::block::BlockType::Modulation => "Modulation".to_string(),
        signal::block::BlockType::Chorus => "Modulation".to_string(),
        signal::block::BlockType::Flanger => "Modulation".to_string(),
        signal::block::BlockType::Phaser => "Modulation".to_string(),
        signal::block::BlockType::Trem => "Motion".to_string(),
        signal::block::BlockType::Panner => "Motion".to_string(),
        signal::block::BlockType::Vibrato => "Motion".to_string(),
        signal::block::BlockType::Rotary => "Motion".to_string(),
        signal::block::BlockType::Filter => "Filters".to_string(),
        signal::block::BlockType::Wah => "Filters".to_string(),
        _ => "Custom".to_string(),
    }
}

/// Extract a vendor name from metadata tags (e.g., "Neural DSP", "FabFilter").
fn extract_vendor(metadata: &signal::metadata::Metadata) -> Option<String> {
    for tag in metadata.tags.as_slice() {
        if tag == "Neural DSP" {
            return Some("Neural DSP".to_string());
        }
        if tag.starts_with("vendor:") {
            let vendor = &tag["vendor:".len()..];
            // Capitalize first letter
            let mut chars = vendor.chars();
            return Some(match chars.next() {
                Some(c) => c.to_uppercase().collect::<String>() + chars.as_str(),
                None => vendor.to_string(),
            });
        }
    }
    None
}

/// Try to find an existing .RfxChain file in the old library structure.
fn find_existing_rfxchain(
    library_path: &std::path::Path,
    collection_name: &str,
    snapshot_name: &str,
) -> Option<std::path::PathBuf> {
    // Check blocks/plugin/neural-dsp/<collection>/<snapshot>.RfxChain
    let ndsp_path = library_path
        .join("blocks/plugin/neural-dsp")
        .join(collection_name)
        .join(format!("{snapshot_name}.RfxChain"));
    if ndsp_path.exists() {
        return Some(ndsp_path);
    }

    // Check presets/<collection>/<snapshot>.RfxChain
    let preset_path = library_path
        .join("presets")
        .join(collection_name)
        .join(format!("{snapshot_name}.RfxChain"));
    if preset_path.exists() {
        return Some(preset_path);
    }

    None
}

/// Sanitize a name for use as a filename (replace invalid chars).
fn sanitize_filename(name: &str) -> String {
    name.replace(['/', '\\', ':', '*', '?', '"', '<', '>', '|'], "_")
}

/// Simple inline sidecar renderer (same as sidecar::render_sidecar_styx but accessible here).
fn render_sidecar_inline(s: &signal_storage::sidecar::SignalSidecar) -> String {
    use signal_storage::sidecar::PresetKind;

    let mut out = String::new();
    out.push_str(&format!("version {}\n", s.version));
    out.push_str(&format!("id \"{}\"\n", s.id));

    match &s.kind {
        PresetKind::Block { block_type } => {
            out.push_str(&format!("kind @Block{{block_type {block_type}}}\n"));
        }
        PresetKind::Module => out.push_str("kind @Module@\n"),
        PresetKind::Layer => out.push_str("kind @Layer@\n"),
        PresetKind::Engine => out.push_str("kind @Engine@\n"),
        PresetKind::Rig => out.push_str("kind @Rig@\n"),
        PresetKind::Profile => out.push_str("kind @Profile@\n"),
        PresetKind::Song => out.push_str("kind @Song@\n"),
        PresetKind::Rack => out.push_str("kind @Rack@\n"),
    }

    if !s.tags.is_empty() {
        let tags: Vec<_> = s.tags.iter().map(|t| {
            if t.contains(char::is_whitespace) || t.is_empty() {
                format!("\"{}\"", t.replace('"', "\\\""))
            } else {
                t.clone()
            }
        }).collect();
        out.push_str(&format!("tags ({})\n", tags.join(" ")));
    }

    if let Some(desc) = &s.description {
        out.push_str(&format!("description \"{}\"\n", desc.replace('"', "\\\"")));
    }

    out
}
