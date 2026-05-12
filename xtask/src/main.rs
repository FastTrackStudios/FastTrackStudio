//! `cargo xtask <command>` — build / test / docs / wiki orchestration.
//!
//! Mirrors the bearcove convention: xtask is a Rust binary that owns
//! repo-wide tasks too lumpy for cargo aliases. Argument parsing goes
//! through `figue` so the surface matches the rest of the project.

use std::env;
use std::path::PathBuf;
use std::process::{Command, ExitCode};

use facet::Facet;
use figue as args;
use owo_colors::OwoColorize;

#[derive(Facet, Debug)]
struct BuildArgs {
    /// Build in release mode.
    #[facet(args::named, args::short = 'r')]
    release: bool,
}

#[derive(Facet, Debug)]
struct TestArgs {
    /// nextest profile to run (`default`, `ci`, `patient`).
    #[facet(args::named)]
    profile: Option<String>,
}

#[derive(Facet, Debug)]
struct E2eArgs {
    /// Run the e2e against the in-memory backend instead of sqlite.
    #[facet(args::named)]
    memory: bool,
}

#[derive(Facet, Debug)]
struct WikiSyncArgs {
    /// Show what would change without pushing.
    #[facet(args::named)]
    dry_run: bool,
}

fn print_usage() {
    eprintln!(
        "{}

  cargo xtask check                  cargo check --workspace
  cargo xtask build [--release]      cargo build --workspace
  cargo xtask test [--profile=ci]    cargo nextest run --workspace
  cargo xtask e2e [--memory]         spawn server, run browser tests
  cargo xtask docs serve             ddc serve
  cargo xtask docs build             ddc build
  cargo xtask wiki sync [--dry-run]  push docs/content/ to Forgejo wiki
  cargo xtask ci                     fmt + clippy + check + test + tracey
  cargo xtask tracey-validate        spec ↔ impl ↔ verify coverage check
",
        "USAGE".bold()
    );
}

fn main() -> ExitCode {
    let argv: Vec<String> = env::args().skip(1).collect();
    let argv_strs: Vec<&str> = argv.iter().map(String::as_str).collect();

    let result: eyre::Result<()> = match argv_strs.split_first() {
        None => {
            print_usage();
            return ExitCode::from(2);
        }
        Some((&"check", _)) => run_check(),
        Some((&"build", rest)) => {
            let a: BuildArgs = figue::from_slice(rest).unwrap();
            run_build(a)
        }
        Some((&"test", rest)) => {
            let a: TestArgs = figue::from_slice(rest).unwrap();
            run_test(a)
        }
        Some((&"e2e", rest)) => {
            let a: E2eArgs = figue::from_slice(rest).unwrap();
            run_e2e(a)
        }
        Some((&"docs", rest)) => match rest.split_first() {
            Some((&"serve", _)) => run_in("docs", &["ddc", "serve"]),
            Some((&"build", _)) => run_in("docs", &["ddc", "build"]),
            _ => {
                eprintln!("{}: cargo xtask docs <serve|build>", "ERROR".red().bold());
                return ExitCode::from(2);
            }
        },
        Some((&"wiki", rest)) => match rest.split_first() {
            Some((&"sync", inner)) => {
                let a: WikiSyncArgs = figue::from_slice(inner).unwrap();
                run_wiki_sync(a)
            }
            _ => {
                eprintln!("{}: cargo xtask wiki <sync>", "ERROR".red().bold());
                return ExitCode::from(2);
            }
        },
        Some((&"ci", _)) => run_ci(),
        Some((&"tracey-validate", _)) => run_tracey_validate(),
        Some((&"--help" | &"-h" | &"help", _)) => {
            print_usage();
            return ExitCode::SUCCESS;
        }
        Some((cmd, _)) => {
            eprintln!("{}: unknown command `{cmd}`", "ERROR".red().bold());
            print_usage();
            return ExitCode::from(2);
        }
    };

    match result {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("{}: {e:#}", "ERROR".red().bold());
            ExitCode::FAILURE
        }
    }
}

// ── Commands ──────────────────────────────────────────────────────────

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .to_path_buf()
}

fn run_check() -> eyre::Result<()> {
    section("check");
    cargo(&["check", "--workspace", "--all-targets"])
}

fn run_build(args: BuildArgs) -> eyre::Result<()> {
    section("build");
    if args.release {
        cargo(&["build", "--workspace", "--release"])
    } else {
        cargo(&["build", "--workspace"])
    }
}

fn run_test(args: TestArgs) -> eyre::Result<()> {
    section("test (nextest)");
    let profile = args.profile.as_deref().unwrap_or("default");
    cargo(&["nextest", "run", "--workspace", "--profile", profile])
}

fn run_e2e(args: E2eArgs) -> eyre::Result<()> {
    section("e2e (browser)");
    let recipe = if args.memory {
        "test-e2e-memory"
    } else {
        "test-e2e"
    };
    run_at_root(&["just", recipe])
}

fn run_wiki_sync(args: WikiSyncArgs) -> eyre::Result<()> {
    section("wiki sync");
    if args.dry_run {
        run_at_root(&["scripts/sync-wiki.sh", "--dry-run"])
    } else {
        run_at_root(&["scripts/sync-wiki.sh"])
    }
}

fn run_ci() -> eyre::Result<()> {
    section("ci");
    cargo(&["fmt", "--all", "--check"])?;
    cargo(&[
        "clippy",
        "--workspace",
        "--all-targets",
        "--",
        "-D",
        "warnings",
    ])?;
    cargo(&["check", "--workspace", "--all-targets"])?;
    cargo(&["nextest", "run", "--workspace", "--profile", "ci"])?;
    // Doctests run separately — nextest doesn't pick them up.
    cargo(&["test", "--doc", "--workspace"])?;
    run_tracey_validate()?;
    Ok(())
}

fn run_tracey_validate() -> eyre::Result<()> {
    section("tracey validate");
    // Exits non-zero on broken refs / unknown prefixes / stale rules /
    // naming violations. Warnings are non-fatal in tracey 1.3.0.
    run_at_root(&["tracey", "query", "validate"])
}

// ── Helpers ───────────────────────────────────────────────────────────

fn section(name: &str) {
    eprintln!("\n{} {}", "❯❯".cyan().bold(), name.bold());
}

fn cargo(args: &[&str]) -> eyre::Result<()> {
    let mut full = vec!["cargo"];
    full.extend_from_slice(args);
    run_at_root(&full)
}

fn run_at_root(argv: &[&str]) -> eyre::Result<()> {
    let status = Command::new(argv[0])
        .args(&argv[1..])
        .current_dir(repo_root())
        .status()?;
    if !status.success() {
        eyre::bail!("`{}` exited with {}", argv.join(" "), status);
    }
    Ok(())
}

fn run_in(subdir: &str, argv: &[&str]) -> eyre::Result<()> {
    let cwd = repo_root().join(subdir);
    let status = Command::new(argv[0])
        .args(&argv[1..])
        .current_dir(&cwd)
        .status()?;
    if !status.success() {
        eyre::bail!("`{}` exited with {}", argv.join(" "), status);
    }
    Ok(())
}
