//! Development tasks for the Task app.
//!
//! Run with: `cargo xtask <command>`
//!
//! Commands:
//!   build             — Build WASM + JS bundle for the Obsidian plugin
//!   codegen           — Generate TypeScript and Swift bindings from VaultService
//!   codegen --ts      — TypeScript only
//!   codegen --swift   — Swift only

use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let command = args.first().map(|s| s.as_str()).unwrap_or("help");

    match command {
        "build" => {
            let workspace_root = workspace_root();
            build_obsidian_plugin(&workspace_root)?;
        }
        "codegen" => {
            let ts_only = args.iter().any(|a| a == "--ts" || a == "--typescript");
            let swift_only = args.iter().any(|a| a == "--swift");

            let workspace_root = workspace_root();

            if !swift_only {
                codegen_typescript(&workspace_root)?;
            }
            if !ts_only {
                codegen_swift(&workspace_root)?;
            }
        }
        "help" | "--help" | "-h" => {
            println!("cargo xtask <command>");
            println!();
            println!("Commands:");
            println!("  build             Build WASM + JS bundle for the Obsidian plugin");
            println!("  codegen           Generate TypeScript and Swift bindings");
            println!("  codegen --ts      TypeScript only");
            println!("  codegen --swift   Swift only");
        }
        other => {
            eprintln!("Unknown command: {other}");
            eprintln!("Run `cargo xtask help` for usage.");
            std::process::exit(1);
        }
    }

    Ok(())
}

fn workspace_root() -> std::path::PathBuf {
    // The xtask binary is run from the workspace root by cargo.
    // CARGO_MANIFEST_DIR points to the xtask/ crate, so go up one level.
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR")
        .expect("CARGO_MANIFEST_DIR must be set (run via `cargo xtask`)");
    Path::new(&manifest_dir)
        .parent()
        .expect("xtask must be inside the workspace")
        .to_path_buf()
}

fn codegen_typescript(workspace_root: &Path) -> Result<(), Box<dyn std::error::Error>> {
    let out_dir = workspace_root.join("obsidian-plugin").join("generated");
    std::fs::create_dir_all(&out_dir)?;

    let service = task_core::vault_service_service_descriptor();
    let ts = vox_codegen::targets::typescript::generate_service(service);

    let filename = format!(
        "{}.generated.ts",
        service.service_name.to_lowercase().replace(' ', "-")
    );
    let out_path = out_dir.join(&filename);
    write_if_changed(&out_path, ts)?;

    println!("Generated TypeScript: {}", out_path.display());
    Ok(())
}

fn codegen_swift(workspace_root: &Path) -> Result<(), Box<dyn std::error::Error>> {
    // Swift client — used by the iOS app and widgets
    let out_dir = workspace_root
        .join("ios-widgets")
        .join("Sources")
        .join("TaskWidgets")
        .join("generated");
    std::fs::create_dir_all(&out_dir)?;

    let service = task_core::vault_service_service_descriptor();

    // Client only (iOS connects to the desktop companion as a client)
    let swift_client = vox_codegen::targets::swift::generate_service_with_bindings(
        service,
        vox_codegen::targets::swift::SwiftBindings::Client,
    );
    let client_path = out_dir.join("VaultServiceClient.swift");
    write_if_changed(&client_path, swift_client)?;

    // Server (desktop app implements the service)
    let desktop_out = workspace_root.join("apps").join("desktop").join("generated");
    std::fs::create_dir_all(&desktop_out)?;
    let swift_server = vox_codegen::targets::swift::generate_service_with_bindings(
        service,
        vox_codegen::targets::swift::SwiftBindings::Server,
    );
    let server_path = desktop_out.join("VaultServiceServer.swift");
    write_if_changed(&server_path, swift_server)?;

    println!("Generated Swift client: {}", client_path.display());
    println!("Generated Swift server: {}", server_path.display());
    Ok(())
}

fn build_obsidian_plugin(workspace_root: &Path) -> Result<(), Box<dyn std::error::Error>> {
    use xshell::{Shell, cmd};

    let plugin_dir = workspace_root.join("obsidian-plugin");
    let sh = Shell::new()?;
    sh.change_dir(&plugin_dir);

    // Step 1: compile Rust → WASM and generate JS bindings via wasm-pack
    eprintln!("==> wasm-pack build --target web");
    cmd!(sh, "wasm-pack build --target web --out-dir pkg --out-name task_task_core").run()?;

    // Step 2: install npm deps if not already present
    if !plugin_dir.join("node_modules").exists() {
        eprintln!("==> npm install");
        cmd!(sh, "npm install").run()?;
    }

    // Step 3: bundle TypeScript + inlined WASM → main.js
    eprintln!("==> npm run build");
    cmd!(sh, "npm run build").run()?;

    println!("Built: {}", plugin_dir.join("main.js").display());
    Ok(())
}

fn write_if_changed(path: &Path, content: String) -> std::io::Result<()> {
    if path.exists() {
        let existing = std::fs::read_to_string(path)?;
        if existing == content {
            return Ok(());
        }
    }
    std::fs::write(path, content)
}
