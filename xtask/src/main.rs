//! FastTrackStudio Workspace Automation Tasks
//!
//! This xtask runner provides automation for the FastTrackStudio workspace,
//! including TypeScript type generation, testing, and other development tasks.

use std::{
    env,
    path::{Path, PathBuf},
    process::Command,
};

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};

type DynError = Box<dyn std::error::Error>;

#[derive(Parser)]
#[command(name = "xtask")]
#[command(about = "FastTrackStudio workspace automation tasks")]
struct Cli {
    #[command(subcommand)]
    command: Task,
}

#[derive(Subcommand)]
enum Task {
    /// Generate TypeScript types from Rust types
    GenerateTypes {
        /// Output directory for generated types
        #[arg(short, long)]
        output: Option<String>,
        /// Skip creating index.ts file
        #[arg(long)]
        no_index: bool,
    },
    /// Run all tests in the workspace
    Test {
        /// Run tests in release mode
        #[arg(long)]
        release: bool,
        /// Run only tests for a specific package
        #[arg(short, long)]
        package: Option<String>,
    },
    /// Format all code in the workspace
    Format {
        /// Check formatting without applying changes
        #[arg(long)]
        check: bool,
    },
    /// Run clippy on all packages
    Clippy {
        /// Treat warnings as errors
        #[arg(long)]
        deny_warnings: bool,
    },
    /// Clean all build artifacts
    Clean,
    /// Run the complete CI pipeline locally
    Ci,
    /// Start the Tauri development server
    Dev {
        /// Skip type generation
        #[arg(long)]
        skip_types: bool,
    },
    /// Build the desktop application for distribution
    Build {
        /// Build in release mode
        #[arg(long)]
        release: bool,
    },
    /// Bundle an audio plugin (CLAP/VST3)
    Bundle {
        /// The plugin package name(s) to bundle
        #[arg(required = true)]
        packages: Vec<String>,
        /// Build in release mode
        #[arg(long)]
        release: bool,
    },
}

fn main() {
    if let Err(e) = try_main() {
        eprintln!("❌ Error: {}", e);
        std::process::exit(1);
    }
}

fn try_main() -> Result<(), DynError> {
    let cli = Cli::parse();

    match cli.command {
        Task::GenerateTypes { output, no_index } => generate_types(output, no_index)?,
        Task::Test { release, package } => run_tests(release, package)?,
        Task::Format { check } => format_code(check)?,
        Task::Clippy { deny_warnings } => run_clippy(deny_warnings)?,
        Task::Clean => clean_workspace()?,
        Task::Ci => run_ci_pipeline()?,
        Task::Dev { skip_types } => start_dev_server(skip_types)?,
        Task::Build { release } => build_desktop_app(release)?,
        Task::Bundle { packages, release } => bundle_plugin(packages, release)?,
    }

    Ok(())
}

fn generate_types(output: Option<String>, no_index: bool) -> Result<(), DynError> {
    println!("🎵 Generating TypeScript types...");

    // Set output directory - use provided path or default to desktop bindings
    let output_dir = output.unwrap_or_else(|| {
        project_root()
            .join("apps/desktop/src/bindings")
            .to_string_lossy()
            .to_string()
    });

    // Clean up any existing bindings directories in modules first
    clean_module_bindings()?;

    let mut cmd = Command::new(cargo());
    cmd.current_dir(project_root())
        .args(["run", "--bin", "generate-types"]);

    // Force all exports to go to the desktop bindings directory
    cmd.env("TS_RS_EXPORT_DIR", &output_dir);

    // Set no-index flag if requested
    if no_index {
        cmd.env("NO_INDEX_FILE", "true");
    }

    let status = cmd.status().context("Failed to run generate-types tool")?;

    if !status.success() {
        return Err("TypeScript type generation failed".into());
    }

    // Clean up any stray bindings directories that might have been created
    clean_module_bindings()?;

    println!("✅ TypeScript types generated successfully!");
    println!("📍 Types generated at: {}", output_dir);
    Ok(())
}

fn clean_module_bindings() -> Result<(), DynError> {
    let modules_to_clean = ["modules/daw/bindings", "modules/fts/bindings"];

    for module_bindings in &modules_to_clean {
        let bindings_path = project_root().join(module_bindings);
        if bindings_path.exists() {
            println!("🧹 Cleaning up {}", module_bindings);
            std::fs::remove_dir_all(&bindings_path)
                .with_context(|| format!("Failed to remove {}", module_bindings))?;
        }
    }
    Ok(())
}

fn run_tests(release: bool, package: Option<String>) -> Result<(), DynError> {
    println!("🧪 Running tests...");

    let mut cmd = Command::new(cargo());
    cmd.current_dir(project_root()).arg("test");

    if release {
        cmd.arg("--release");
    }

    if let Some(pkg) = package {
        cmd.args(["--package", &pkg]);
    }

    let status = cmd.status().context("Failed to run tests")?;

    if !status.success() {
        return Err("Tests failed".into());
    }

    println!("✅ All tests passed!");
    Ok(())
}

fn format_code(check: bool) -> Result<(), DynError> {
    if check {
        println!("📝 Checking code formatting...");
    } else {
        println!("📝 Formatting code...");
    }

    let mut cmd = Command::new(cargo());
    cmd.current_dir(project_root()).args(["fmt", "--all"]);

    if check {
        cmd.arg("--check");
    }

    let status = cmd.status().context("Failed to run rustfmt")?;

    if !status.success() {
        return Err("Code formatting check failed".into());
    }

    if check {
        println!("✅ Code formatting is correct!");
    } else {
        println!("✅ Code formatted successfully!");
    }
    Ok(())
}

fn run_clippy(deny_warnings: bool) -> Result<(), DynError> {
    println!("📎 Running clippy...");

    let mut cmd = Command::new(cargo());
    cmd.current_dir(project_root())
        .args(["clippy", "--all-targets", "--all-features"]);

    if deny_warnings {
        cmd.args(["--", "-D", "warnings"]);
    }

    let status = cmd.status().context("Failed to run clippy")?;

    if !status.success() {
        return Err("Clippy found issues".into());
    }

    println!("✅ Clippy checks passed!");
    Ok(())
}

fn clean_workspace() -> Result<(), DynError> {
    println!("🧹 Cleaning workspace...");

    let status = Command::new(cargo())
        .current_dir(project_root())
        .args(["clean"])
        .status()
        .context("Failed to run cargo clean")?;

    if !status.success() {
        return Err("Workspace clean failed".into());
    }

    println!("✅ Workspace cleaned successfully!");
    Ok(())
}

fn run_ci_pipeline() -> Result<(), DynError> {
    println!("🚀 Running CI pipeline...");

    // 1. Format check
    println!("\n1️⃣ Checking formatting...");
    format_code(true)?;

    // 2. Clippy
    println!("\n2️⃣ Running clippy...");
    run_clippy(true)?;

    // 3. Generate types
    println!("\n3️⃣ Generating TypeScript types...");
    generate_types(None, false)?;

    // 4. Run tests
    println!("\n4️⃣ Running tests...");
    run_tests(false, None)?;

    // 5. Build check
    println!("\n5️⃣ Checking build...");
    let status = Command::new(cargo())
        .current_dir(project_root())
        .args(["build", "--all"])
        .status()
        .context("Failed to build workspace")?;

    if !status.success() {
        return Err("Build check failed".into());
    }

    println!("\n🎉 CI pipeline completed successfully!");
    Ok(())
}

fn start_dev_server(skip_types: bool) -> Result<(), DynError> {
    if !skip_types {
        println!("🎵 Generating TypeScript types...");
        generate_types(None, false)?;
    }

    println!("🚀 Starting Tauri development server...");

    let status = Command::new(cargo())
        .current_dir(project_root().join("apps/desktop/src-tauri"))
        .args(["tauri", "dev"])
        .status()
        .context("Failed to start Tauri dev server")?;

    if !status.success() {
        return Err("Failed to start development server".into());
    }

    Ok(())
}

fn build_desktop_app(release: bool) -> Result<(), DynError> {
    println!("🏗️ Building desktop application...");

    // Generate types first
    generate_types(None, false)?;

    let mut cmd = Command::new(cargo());
    cmd.current_dir(project_root().join("apps/desktop/src-tauri"))
        .args(["tauri", "build"]);

    if !release {
        cmd.arg("--debug");
    }

    let status = cmd
        .status()
        .context("Failed to build desktop application")?;

    if !status.success() {
        return Err("Desktop application build failed".into());
    }

    println!("✅ Desktop application built successfully!");
    Ok(())
}

fn bundle_plugin(packages: Vec<String>, release: bool) -> Result<(), DynError> {
    println!("📦 Bundling audio plugin(s)...");

    // Build the arguments for nih_plug_xtask
    let mut args: Vec<String> = vec!["bundle".to_string()];

    for pkg in &packages {
        args.push("-p".to_string());
        args.push(pkg.clone());
    }

    if release {
        args.push("--release".to_string());
    }

    // Use nih_plug_xtask's main function directly
    // We need to set up the environment as if called from cargo xtask
    std::env::set_current_dir(project_root())?;

    // Call nih_plug_xtask's main with our arguments
    match nih_plug_xtask::main_with_args("xtask", args) {
        Ok(()) => {
            println!("✅ Plugin(s) bundled successfully!");
            println!("📍 Output: {}/target/bundled/", project_root().display());
            Ok(())
        }
        Err(e) => Err(format!("Plugin bundling failed: {}", e).into()),
    }
}

fn cargo() -> String {
    env::var("CARGO").unwrap_or_else(|_| "cargo".to_string())
}

fn project_root() -> PathBuf {
    Path::new(&env!("CARGO_MANIFEST_DIR"))
        .ancestors()
        .nth(1)
        .unwrap()
        .to_path_buf()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_project_root_detection() {
        let root = project_root();
        assert!(root.join("Cargo.toml").exists());
        assert!(root.join("xtask").exists());
    }
}
