//! Embed the canonical keybind profiles (features/reaper/reaper-input/
//! config/config/*) into the site so the /input tutorial renders the real
//! configurations. Generates $OUT_DIR/input_profiles.rs:
//!
//! ```ignore
//! pub static PROFILES: &[EmbeddedProfile] = &[ ... ];
//! ```
//!
//! Runs on the host even for wasm builds, so plain std::fs is fine.

use std::fmt::Write as _;
use std::path::Path;

fn main() {
    let manifest = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let config_root = Path::new(&manifest).join("../../features/reaper/reaper-input/config/config");
    let config_root = config_root.canonicalize().expect("keybind config dir");
    println!("cargo::rerun-if-changed={}", config_root.display());

    let mut out = String::from(
        "/// One embedded section file (category) of a profile.\n\
         pub struct EmbeddedSection {\n\
             pub id: &'static str,\n\
             pub styx: &'static str,\n\
         }\n\
         /// One embedded keybind profile.\n\
         pub struct EmbeddedProfile {\n\
             pub id: &'static str,\n\
             pub profile_styx: &'static str,\n\
             pub sections: &'static [EmbeddedSection],\n\
         }\n\
         pub static PROFILES: &[EmbeddedProfile] = &[\n",
    );

    let mut profiles: Vec<_> = std::fs::read_dir(&config_root)
        .expect("read config root")
        .flatten()
        .filter(|e| e.path().join("profile.styx").is_file())
        .map(|e| e.path())
        .collect();
    profiles.sort();

    for dir in profiles {
        let id = dir.file_name().unwrap().to_str().unwrap().to_string();
        let profile_styx = dir.join("profile.styx");
        writeln!(
            out,
            "    EmbeddedProfile {{\n        id: {id:?},\n        profile_styx: include_str!({:?}),\n        sections: &[",
            profile_styx.display().to_string(),
        )
        .unwrap();

        let mut sections: Vec<_> = std::fs::read_dir(&dir)
            .expect("read profile dir")
            .flatten()
            .map(|e| e.path())
            .filter(|p| {
                p.extension().is_some_and(|x| x == "styx")
                    && p.file_stem().is_some_and(|s| {
                        s != "profile" && s != "mouse-profile"
                    })
            })
            .collect();
        sections.sort();

        for section in sections {
            let sid = section.file_stem().unwrap().to_str().unwrap().to_string();
            writeln!(
                out,
                "            EmbeddedSection {{ id: {sid:?}, styx: include_str!({:?}) }},",
                section.display().to_string(),
            )
            .unwrap();
        }
        out.push_str("        ],\n    },\n");
    }
    out.push_str("];\n");

    // Workflows ("modes") and overlays are SHARED across profiles — they
    // live in config/workflows/*.styx and config/overlays/*.styx and layer
    // on whatever profile is active.
    out.push_str(
        "/// One embedded shared config file (workflow or overlay).\n\
         pub struct EmbeddedNamed {\n\
             pub id: &'static str,\n\
             pub styx: &'static str,\n\
         }\n",
    );
    for (dir_name, static_name) in [("workflows", "WORKFLOWS"), ("overlays", "OVERLAYS")] {
        writeln!(out, "pub static {static_name}: &[EmbeddedNamed] = &[").unwrap();
        let mut files: Vec<_> = std::fs::read_dir(config_root.join(dir_name))
            .expect("read shared config dir")
            .flatten()
            .map(|e| e.path())
            .filter(|p| p.extension().is_some_and(|x| x == "styx"))
            .collect();
        files.sort();
        for file in files {
            let id = file.file_stem().unwrap().to_str().unwrap().to_string();
            writeln!(
                out,
                "    EmbeddedNamed {{ id: {id:?}, styx: include_str!({:?}) }},",
                file.display().to_string(),
            )
            .unwrap();
        }
        out.push_str("];\n");
    }

    let dest = Path::new(&std::env::var("OUT_DIR").unwrap()).join("input_profiles.rs");
    std::fs::write(dest, out).expect("write generated profiles");
}
