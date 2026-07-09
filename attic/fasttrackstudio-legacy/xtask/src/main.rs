//! xtask: Development tasks for FastTrackStudio
//!
//! Run with: `cargo xtask <command>`

use std::process::ExitCode;

use facet::Facet;
use figue as args;
use xshell::{Shell, cmd};

mod icon_gen;
mod icon_gen_rs;

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
    /// Create wrapper .app bundles for each rig type (dock names + icons)
    SetupRigs {
        /// Force re-creation of all bundles even if they already exist
        #[facet(args::named, default)]
        force: bool,
    },
    /// Package the FTS library into a .tar.gz for the installer
    PackageLibrary {
        /// Output file path (default: fts-library.tar.gz)
        #[facet(args::positional, default)]
        output: Option<String>,
    },
    /// Generate rig type icons as PNGs (pure Rust, no Swift)
    GenIcons {
        /// Output directory (default: apps/installer/assets/icons)
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
            cmd!(sh, "cargo build -p session-extension").run()?;
            cmd!(sh, "cargo build -p sync-extension").run()?;
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
        Commands::ReaperTest {
            filter,
            no_build,
            keep_open,
        } => {
            use daw::test::runner::{self, TestPackage, TestRunner};

            println!("=== Running REAPER integration tests ===");

            let ci = std::env::var("CI").is_ok();
            let timeout_secs: u64 = std::env::var("REAPER_TEST_TIMEOUT_SECS")
                .ok()
                .and_then(|v| v.parse().ok())
                .unwrap_or(60);
            let resources_dir = runner::fts_reaper_resources();

            let test_runner = TestRunner {
                resources_dir: resources_dir.clone(),
                extension_log: std::path::PathBuf::from("/tmp/daw-bridge.log"),
                timeout_secs,
                keep_open,
                headless: !keep_open,
                ci,
                extension_whitelist: vec![],
            };

            // Step 1: Build daw-bridge host + CLAP plugin + guest extensions (unless --no-build)
            if !no_build {
                // Build daw-bridge from the daw repo (sibling directory)
                let daw_dir = workspace_root
                    .parent()
                    .map(|p| p.join("daw"))
                    .unwrap_or_else(|| workspace_root.join("../daw"));

                runner::section(ci, "reaper-test: build daw-bridge");
                if !daw_dir.exists() {
                    return Err(format!(
                        "daw repo not found at {} — clone it as a sibling",
                        daw_dir.display()
                    )
                    .into());
                }
                let sh_daw = Shell::new()?;
                sh_daw.change_dir(&daw_dir);
                cmd!(sh_daw, "cargo build -p daw-bridge").run()?;

                let user_plugins_dir = resources_dir.join("UserPlugins");
                std::fs::create_dir_all(&user_plugins_dir)?;

                // Remove old reaper_fts extension if present (replaced by daw-bridge)
                for old in &["libreaper_fts.so", "libreaper_fts.dylib"] {
                    let old_path = user_plugins_dir.join(old);
                    if old_path.exists() {
                        std::fs::remove_file(&old_path)?;
                        println!("  Removed old {old} from UserPlugins");
                    }
                }

                // Install daw-bridge as a REAPER plugin
                let (so_src_name, so_dst_name) = if cfg!(target_os = "macos") {
                    ("libreaper_daw_bridge.dylib", "reaper_daw_bridge.dylib")
                } else {
                    ("libreaper_daw_bridge.so", "reaper_daw_bridge.so")
                };
                let so_src = daw_dir.join(format!("target/debug/{so_src_name}"));
                if so_src.exists() {
                    runner::install_plugin(&so_src, so_dst_name, &user_plugins_dir)?;
                } else {
                    return Err(
                        format!("daw-bridge library not found at {}", so_src.display()).into(),
                    );
                }
                runner::end_section(ci);

                // Build and install fts-macros CLAP plugin
                let fts_plugins_dir = workspace_root
                    .parent()
                    .map(|p| p.join("fts-plugins"))
                    .unwrap_or_else(|| workspace_root.join("../fts-plugins"));
                if fts_plugins_dir.exists() {
                    runner::section(ci, "reaper-test: build fts-macros");
                    let sh_plugins = Shell::new()?;
                    sh_plugins.change_dir(&fts_plugins_dir);
                    cmd!(
                        sh_plugins,
                        "cargo run --package xtask -- bundle fts-macros --release"
                    )
                    .run()?;

                    let fx_dir = user_plugins_dir.join("FX");
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
                    runner::end_section(ci);
                }

                // Build and install SHM guest extensions
                // Each entry is (package_name, binary_name)
                runner::section(ci, "reaper-test: build guest extensions");
                let guest_extensions: &[(&str, &str)] = &[
                    ("session-extension", "session"),
                    ("sync-extension", "sync"),
                    ("dynamic-template-extension", "dynamic-template"),
                ];
                for (pkg, _bin) in guest_extensions {
                    cmd!(sh, "cargo build -p {pkg}").run()?;
                }

                let fts_ext_dir = user_plugins_dir.join("fts-extensions");
                std::fs::create_dir_all(&fts_ext_dir)?;
                for (_pkg, bin) in guest_extensions {
                    let src = workspace_root.join(format!("target/debug/{bin}"));
                    if src.exists() {
                        runner::install_plugin(&src, bin, &fts_ext_dir)?;
                    } else {
                        println!("  Warning: {bin} binary not found at {}", src.display());
                    }
                }
                runner::end_section(ci);
            }

            // Step 2: Pre-build test binaries for each package
            runner::section(ci, "reaper-test: build test binaries");
            struct TestCrate<'a> {
                package: &'a str,
                features: &'a [&'a str],
                test_dir: &'a std::path::Path,
            }
            let test_crates = [
                TestCrate {
                    package: "session",
                    features: &[],
                    test_dir: std::path::Path::new("crates/session/session/tests"),
                },
                TestCrate {
                    package: "sync",
                    features: &[],
                    test_dir: std::path::Path::new("crates/sync/sync/tests"),
                },
            ];
            for tc in &test_crates {
                let matched_bins = filter
                    .as_ref()
                    .map(|f| runner::find_matching_test_binaries(f, &[tc.test_dir]));
                // Skip this package if filter matched nothing in it
                if let Some(ref bins) = matched_bins {
                    if bins.is_empty() {
                        continue;
                    }
                }
                let pkg = tc.package;
                let features = tc.features.join(",");
                let mut c = if features.is_empty() {
                    cmd!(sh, "cargo test -p {pkg}")
                } else {
                    cmd!(sh, "cargo test -p {pkg} --features {features}")
                };
                if let Some(ref bins) = matched_bins {
                    for bin in bins {
                        c = c.arg("--test").arg(bin);
                    }
                }
                c.arg("--no-run").run()?;
            }
            runner::end_section(ci);

            // Step 3: Clean, pre-warm, patch INI, spawn REAPER
            test_runner.clean_stale_sockets();
            test_runner.prewarm_reaper();
            test_runner.patch_ini();

            let mut reaper = test_runner.spawn_reaper()?;
            reaper.wait_for_socket(&test_runner)?;

            // Step 4: Run tests — only include packages that have matching tests
            let all_packages: Vec<(TestPackage, &std::path::Path)> = vec![
                (
                    TestPackage {
                        package: "session".into(),
                        features: vec![],
                        test_threads: 4,
                        default_skips: vec![],
                        test_binary: None,
                    },
                    std::path::Path::new("crates/session/session/tests"),
                ),
                (
                    TestPackage {
                        package: "sync".into(),
                        features: vec![],
                        test_threads: 4,
                        default_skips: vec![],
                        test_binary: None,
                    },
                    std::path::Path::new("crates/sync/sync/tests"),
                ),
            ];
            let packages: Vec<TestPackage> = all_packages
                .into_iter()
                .filter_map(|(pkg, test_dir)| {
                    if let Some(ref f) = filter {
                        let bins = runner::find_matching_test_binaries(f, &[test_dir]);
                        if bins.is_empty() {
                            println!("  Skipping {} (no tests match filter)", pkg.package);
                            return None;
                        }
                    }
                    Some(pkg)
                })
                .collect();

            let tests_passed = test_runner.run_tests(&mut reaper, &packages, filter.as_deref())?;

            // Step 5: Cleanup and report
            if !tests_passed {
                reaper.report_failure(&test_runner);
                reaper.stop(&test_runner);
                return Err("Some tests failed".into());
            }

            reaper.stop(&test_runner);
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
        Commands::SetupRigs { force } => {
            run_setup_rigs(force)?;
        }
        Commands::PackageLibrary { output } => {
            run_package_library(output)?;
        }
        Commands::GenIcons { output } => {
            let output_dir = output.unwrap_or_else(|| "apps/installer/assets/icons".to_string());
            println!("=== Generating rig type icons ===");
            println!("  Output: {output_dir}");
            icon_gen_rs::generate_all_icons(std::path::Path::new(&output_dir), &[128])?;
            println!("=== Done ===");
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

    let status = std::process::Command::new("tar").args(&args).status()?;

    if !status.success() {
        return Err(format!("tar failed with status {status}").into());
    }

    // Show size
    let meta = std::fs::metadata(&output_path)?;
    let size_mb = meta.len() as f64 / 1_048_576.0;
    println!("\nCreated {output_path} ({size_mb:.1} MB, {included} entries)");
    println!(
        "Upload with: gh release create library-v1 {output_path} --repo FastTrackStudios/fts-library"
    );

    Ok(())
}

// ============================================================================
// setup-rigs: Create wrapper .app bundles for each rig type
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
        .args([
            "build",
            "-p",
            "reaper-launcher",
            "--release",
            "--bin",
            "reaper-launcher",
        ])
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::piped())
        .status()?;
    if !build_status.success() {
        return Err("Failed to build reaper-launcher".into());
    }
    println!(" OK");

    // Find the built binary
    let workspace_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap();
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
        let bundle_dir = std::path::PathBuf::from(base_dir).join(format!("{app_name}.app"));
        let contents_dir = bundle_dir.join("Contents");
        let macos_dir = contents_dir.join("MacOS");
        let resources_dir = contents_dir.join("Resources");
        let wrapper_exe = macos_dir.join("REAPER");
        let plist_path = contents_dir.join("Info.plist");

        // Skip if already set up (unless --force)
        if !force && wrapper_exe.exists() && plist_path.exists() {
            println!(
                "  SKIP {}.app (already exists, use --force to recreate)",
                app_name
            );
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
            reaper_args: reaper_launcher::LaunchConfig::standard_reaper_args(),
        };
        launch_config
            .save(&contents_dir.join("launch.json"))
            .map_err(|e| format!("Failed to write launch.json: {e}"))?;

        // Copy the pre-built reaper-launcher binary as the bundle executable
        std::fs::copy(&launcher_bin, &wrapper_exe)
            .map_err(|e| format!("Failed to copy launcher binary into {}.app: {e}", app_name))?;

        // Write Info.plist
        let bundle_id_suffix = wrapper
            .rig_type
            .as_deref()
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
        let bundle_dir = std::path::PathBuf::from(base_dir).join(format!("{app_name}.app"));

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
    let _ = std::process::Command::new("killall").arg("Dock").status();
    let _ = std::process::Command::new("killall").arg("Finder").status();

    println!("\n=== Setup complete ===");
    println!("Wrapper bundles: {}", base_dir);
    for dir in &alias_dirs {
        println!("Aliases: {}", dir.display());
    }

    Ok(())
}

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

