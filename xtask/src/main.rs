//! Development tasks for the Task app.
//!
//! Run with: `cargo xtask <command>`
//!
//! Commands:
//!   build             — Build WASM + JS bundle for the Obsidian plugin
//!   codegen           — Generate TypeScript bindings from Vox services

use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let command = args.first().map_or("help", std::string::String::as_str);

    match command {
        "build" => {
            let workspace_root = workspace_root();
            build_obsidian_plugin(&workspace_root)?;
        }
        "codegen" => {
            let workspace_root = workspace_root();
            codegen_typescript(&workspace_root)?;
        }
        "help" | "--help" | "-h" => {
            println!("cargo xtask <command>");
            println!();
            println!("Commands:");
            println!("  build             Build WASM + JS bundle for the Obsidian plugin");
            println!("  codegen           Generate TypeScript bindings");
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
    let out_dir = workspace_root
        .join("integrations")
        .join("obsidian")
        .join("plugin")
        .join("generated");
    std::fs::create_dir_all(&out_dir)?;

    for service in service_descriptors() {
        let ts = vox_codegen::targets::typescript::generate_service(service);
        let filename = format!(
            "{}.generated.ts",
            service.service_name.to_lowercase().replace(' ', "-")
        );
        let out_path = out_dir.join(&filename);
        write_if_changed(&out_path, ts)?;
        println!("Generated TypeScript: {}", out_path.display());
    }
    Ok(())
}

fn service_descriptors() -> Vec<&'static vox_types::ServiceDescriptor> {
    // Vertical-slice reset: per-feature service descriptors will be
    // pulled from the new features/* trios as they come online.
    Vec::new()
}

fn build_obsidian_plugin(workspace_root: &Path) -> Result<(), Box<dyn std::error::Error>> {
    use xshell::{Shell, cmd};

    let plugin_dir = workspace_root
        .join("integrations")
        .join("obsidian")
        .join("plugin");
    let sh = Shell::new()?;
    sh.change_dir(&plugin_dir);

    // Step 1: compile Rust → WASM and generate JS bindings via wasm-pack
    eprintln!("==> wasm-pack build --target web");
    cmd!(
        sh,
        "wasm-pack build --target web --out-dir pkg --out-name task_task_core"
    )
    .run()?;

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
