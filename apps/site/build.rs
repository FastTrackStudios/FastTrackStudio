//! Embed the guides vault (docs/guides/**/*.md) into the site.
//!
//! The keybind profiles used to be embedded here too, by reaching into
//! features/reaper/reaper-input. They live in the `daw` repo now and are
//! exported as `reaper_input_config::PROFILES` -- a build script cannot
//! reach across a repo boundary.
//!
//! Runs on the host even for wasm builds, so plain std::fs is fine.

use std::fmt::Write as _;
use std::path::Path;

fn main() {
    let manifest = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let mut out = String::new();

    // The guides vault: docs/guides/**/*.md, embedded for release/wasm
    // builds. Ids are vault-relative paths with forward slashes
    // ("reaper/transport.md"). Dev builds on wasm fetch the same files
    // live from the dx dev server instead (see apps/site/src/vault.rs) —
    // the embedded copy is the file LIST either way.
    let vault_root = Path::new(&manifest).join("../../docs/guides");
    let vault_root = vault_root.canonicalize().expect("docs/guides vault dir");
    println!("cargo::rerun-if-changed={}", vault_root.display());
    let mut notes: Vec<std::path::PathBuf> = Vec::new();
    collect_md(&vault_root, &mut notes);
    notes.sort();

    // The vault's ONE source of truth is docs/guides (portable, no
    // symlink). manganis `asset!` (the dev live-reload path below) needs
    // the files physically inside the crate, so mirror them into a
    // build-generated, gitignored apps/site/assets/guides. Rewritten
    // every build; `rerun-if-changed` on docs/guides above keeps it
    // fresh when a note is edited.
    let assets_guides = Path::new(&manifest).join("assets/guides");
    let _ = std::fs::remove_dir_all(&assets_guides);
    for file in &notes {
        let rel = file.strip_prefix(&vault_root).unwrap();
        let dst = assets_guides.join(rel);
        if let Some(parent) = dst.parent() {
            std::fs::create_dir_all(parent).expect("create assets/guides dir");
        }
        std::fs::copy(file, &dst).expect("mirror vault note into assets");
    }
    out.push_str("pub static VAULT: &[EmbeddedNamed] = &[\n");
    for file in &notes {
        let rel = file
            .strip_prefix(&vault_root)
            .unwrap()
            .to_str()
            .unwrap()
            .replace('\\', "/");
        writeln!(
            out,
            "    EmbeddedNamed {{ id: {rel:?}, styx: include_str!({:?}) }},",
            file.display().to_string(),
        )
        .unwrap();
    }
    out.push_str("];\n");

    // Dev-on-wasm live reload: each note as a SINGLE-FILE `asset!` —
    // dx hot-reloads individual assets in place (same served URL, no
    // rebuild), which folder assets don't get (a folder change causes
    // a full rebuild under a new hash). Reads from the build-mirrored
    // apps/site/assets/guides (copied from docs/guides above) so
    // manganis accepts the in-crate paths.
    out.push_str(
        "#[cfg(all(target_arch = \"wasm32\", debug_assertions))]\n\
         mod vault_assets {\n\
             use dioxus::prelude::*;\n\
             pub static VAULT_ASSETS: &[(&str, Asset)] = &[\n",
    );
    for file in &notes {
        let rel = file
            .strip_prefix(&vault_root)
            .unwrap()
            .to_str()
            .unwrap()
            .replace('\\', "/");
        writeln!(
            out,
            "        ({rel:?}, asset!({:?})),",
            format!("/assets/guides/{rel}"),
        )
        .unwrap();
    }
    out.push_str(
        "    ];\n\
         }\n\
         #[cfg(all(target_arch = \"wasm32\", debug_assertions))]\n\
         pub use vault_assets::VAULT_ASSETS;\n",
    );

    // Guide screencast media (apps/site/assets/guides-media/*). Each file
    // becomes a manganis `asset!` keyed by its file STEM, so a `gif` fence
    // in a note (`transport-play`) resolves to the hashed, production-served
    // URL. Any missing name falls back to `_placeholder` at render time
    // (see vault.rs). Dropping a real `transport-play.gif` (or .webp/.mp4)
    // into this dir lights up that slot on the next build — no note edits.
    let media_dir = Path::new(&manifest).join("assets/guides-media");
    println!("cargo::rerun-if-changed={}", media_dir.display());
    let mut media: Vec<std::path::PathBuf> = std::fs::read_dir(&media_dir)
        .expect("assets/guides-media dir")
        .flatten()
        .map(|e| e.path())
        .filter(|p| {
            p.is_file()
                && !p
                    .file_name()
                    .is_some_and(|n| n.to_string_lossy().starts_with('.'))
        })
        .collect();
    media.sort();
    out.push_str(
        "mod media_assets {\n\
             use dioxus::prelude::*;\n\
             /// (file stem, hashed asset) for every guide screencast.\n\
             pub static MEDIA_ASSETS: &[(&str, Asset)] = &[\n",
    );
    for file in &media {
        let stem = file.file_stem().unwrap().to_str().unwrap();
        let name = file.file_name().unwrap().to_str().unwrap();
        writeln!(
            out,
            "        ({stem:?}, asset!({:?})),",
            format!("/assets/guides-media/{name}"),
        )
        .unwrap();
    }
    out.push_str(
        "    ];\n\
         }\n\
         pub use media_assets::MEDIA_ASSETS;\n",
    );

    let dest = Path::new(&std::env::var("OUT_DIR").unwrap()).join("input_profiles.rs");
    std::fs::write(dest, out).expect("write generated profiles");
}

/// Recursively collect every `.md` under `dir`.
fn collect_md(dir: &Path, out: &mut Vec<std::path::PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_md(&path, out);
        } else if path.extension().is_some_and(|x| x == "md") {
            out.push(path);
        }
    }
}
