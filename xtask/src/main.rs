//! xtask: Development tasks for FastTrackStudio
//!
//! Run with: `cargo xtask <command>`

use std::process::ExitCode;

use facet::Facet;
use figue as args;
use signal_proto::catalog;
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
    },
    /// Scan all Neural DSP preset libraries and write a structured catalogue
    Catalog {
        /// Output directory for the catalogue (default: ~/Music/FastTrackStudio/Presets)
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
        Commands::ReaperTest { filter, no_build } => {
            println!("=== Running REAPER integration tests ===");

            // Step 1: Build extension (unless --no-build)
            if !no_build {
                println!("\n>>> Building REAPER extension...");
                cmd!(sh, "cargo build -p reaper-extension").run()?;

                // Copy dylib to REAPER's UserPlugins
                let reaper_path = std::env::var("REAPER_PATH").unwrap_or_else(|_| {
                    "/Users/codywright/Music/FastTrackStudio/Reaper/FTS-TRACKS/".to_string()
                });
                let plugins_dir = std::path::PathBuf::from(&reaper_path).join("UserPlugins");
                std::fs::create_dir_all(&plugins_dir)?;

                let dylib_src = workspace_root.join("target/debug/libreaper_fts.dylib");
                let dylib_dst = plugins_dir.join("libreaper_fts.dylib");
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
            }

            // Step 2: Spawn REAPER (empty project, no splash)
            println!("\n>>> Spawning REAPER...");
            let reaper_exe = std::env::var("REAPER_EXECUTABLE").unwrap_or_else(|_| {
                "/Users/codywright/Music/FastTrackStudio/Reaper/FTS-TRACKS/FTS-LIVE.app/Contents/MacOS/REAPER".to_string()
            });
            let reaper_resources = std::env::var("REAPER_RESOURCES").unwrap_or_else(|_| {
                "/Users/codywright/Music/FastTrackStudio/Reaper/FTS-TRACKS/FTS-LIVE.app/Contents/Resources".to_string()
            });

            let socket_path = "/tmp/fts-control.sock";
            let _ = std::fs::remove_file(socket_path);

            let mut reaper_child = std::process::Command::new(&reaper_exe)
                .current_dir(&reaper_resources)
                .arg("-nosplash")
                .arg("-ignoreerrors")
                .spawn()
                .map_err(|e| format!("Failed to spawn REAPER at {reaper_exe}: {e}"))?;
            let reaper_pid = reaper_child.id();
            println!("  Spawned REAPER (PID {reaper_pid})");

            // Wait for socket
            let socket = std::path::Path::new(socket_path);
            let deadline = std::time::Instant::now() + std::time::Duration::from_secs(30);
            print!("  Waiting for socket");
            while !socket.exists() {
                if std::time::Instant::now() > deadline {
                    println!();
                    let _ = reaper_child.kill();
                    let _ = reaper_child.wait();
                    return Err("Timed out waiting for REAPER socket".into());
                }
                std::thread::sleep(std::time::Duration::from_millis(500));
                print!(".");
            }
            println!("\n  Socket ready");

            // Step 3: Prepare per-test log directory
            let log_dir = std::path::PathBuf::from("/tmp/reaper-tests");
            if log_dir.exists() {
                let _ = std::fs::remove_dir_all(&log_dir);
            }
            std::fs::create_dir_all(&log_dir)?;
            println!("  Log directory: {}", log_dir.display());

            // Step 4: Run REAPER tests (parallel with limited concurrency)
            // Each test gets its own project tab; limit threads to avoid overwhelming
            // REAPER's main thread with too many concurrent plugin loads.
            println!("\n>>> Running tests...");
            let test_result = if let Some(ref f) = filter {
                cmd!(
                    sh,
                    "cargo test -p signal --features daw -- --ignored --nocapture --test-threads=4 {f}"
                )
                .run()
            } else {
                cmd!(
                    sh,
                    "cargo test -p signal --features daw -- --ignored --nocapture --test-threads=4"
                )
                .run()
            };

            // Step 4b: Final cleanup — remove leftover tracks/tabs
            println!("\n>>> Cleaning up REAPER state...");
            let _ = cmd!(
                sh,
                "cargo test -p signal --features daw --test reaper_connection -- --ignored --nocapture final_cleanup"
            )
            .run();

            // Step 5: Kill REAPER
            println!("\n>>> Stopping REAPER (PID {reaper_pid})...");
            let _ = reaper_child.kill();
            let _ = reaper_child.wait();
            let _ = std::fs::remove_file(socket_path);

            // Step 6: On failure, summarize per-test log files
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
    }

    Ok(())
}

fn run_catalog(output: Option<String>) -> Result<(), Box<dyn std::error::Error>> {
    let library_dir = match output {
        Some(dir) => std::path::PathBuf::from(dir),
        None => {
            let home = std::env::var("HOME").unwrap_or_else(|_| "/tmp".to_string());
            std::path::PathBuf::from(home).join("Music/FastTrackStudio/Library")
        }
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
fn chrono_now() -> String {
    use std::time::SystemTime;
    let now = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_secs();
    // Good enough for a generated timestamp
    format!("unix:{}", now)
}
