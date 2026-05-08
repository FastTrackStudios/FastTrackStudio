use std::path::{Path, PathBuf};
use std::process::Command;

use reaper_test::runner::{self, ExtensionPackage, TestPackage, TestRunner};

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    match args.first().map(|s| s.as_str()) {
        Some("install") => install(),
        Some("uninstall") => uninstall(),
        Some("status") => fts_devtools::status(),
        Some("reaper-test") => {
            let filter = args.get(1).cloned();
            let keep_open = args.iter().any(|a| a == "--keep-open");
            if let Err(e) = reaper_test(filter, keep_open) {
                eprintln!("reaper-test failed: {e}");
                std::process::exit(1);
            }
        }
        _ => {
            eprintln!("usage: cargo xtask <command>");
            eprintln!();
            eprintln!("commands:");
            eprintln!("  install       Build and symlink the session test extension into REAPER");
            eprintln!("  uninstall     Remove the session test extension symlink from REAPER");
            eprintln!("  status        Show installed extensions and plugins");
            eprintln!("  reaper-test   Run REAPER integration tests (session-extension only)");
            std::process::exit(1);
        }
    }
}

fn install() {
    // Build the extension
    let status = Command::new("cargo")
        .args(["build", "-p", "session-extension"])
        .status()
        .expect("failed to run cargo build");

    if !status.success() {
        eprintln!("cargo build failed");
        std::process::exit(1);
    }

    let lib = target_dir().join("libreaper_session_extension.so");
    let plugins = runner::fts_reaper_resources().join("UserPlugins");
    runner::install_plugin(&lib, "reaper_session_extension.so", &plugins)
        .expect("failed to install session test extension");
}

fn uninstall() {
    let plugin = runner::fts_reaper_resources()
        .join("UserPlugins")
        .join("reaper_session_extension.so");
    let _ = std::fs::remove_file(&plugin);
    println!("Removed {}", plugin.display());
}

fn target_dir() -> PathBuf {
    // Walk up from xtask dir to workspace root, then into target/debug
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    manifest_dir.parent().unwrap().join("target").join("debug")
}

fn reaper_test(filter: Option<String>, keep_open: bool) -> Result<(), Box<dyn std::error::Error>> {
    let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let resources_dir = runner::fts_reaper_resources();
    let daw_root = workspace_root.join("../daw").canonicalize()?;

    let mut runner = TestRunner::new(resources_dir)
        .with_extension_log(PathBuf::from("/tmp/session-extension.log"));
    runner.keep_open = keep_open;

    let packages = vec![TestPackage {
        package: "session".into(),
        features: vec![],
        test_threads: 1,
        default_skips: vec![],
        test_binary: None,
    }];

    runner.install_test_extensions(
        &daw_root,
        workspace_root,
        &[ExtensionPackage {
            package: "session-extension".into(),
            lib_stem: "reaper_session_extension".into(),
            plugin_name: "reaper_session_extension.so".into(),
            release: false,
        }],
    )?;
    runner.build_test_packages(workspace_root, &packages)?;
    let tests_passed = runner.run_reaper_tests(&packages, filter.as_deref())?;

    if !tests_passed {
        return Err("Some tests failed".into());
    }

    println!("\nAll session tests passed!");
    Ok(())
}
