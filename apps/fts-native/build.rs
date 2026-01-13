// Build script for WESL shader compilation
// Compiles .wesl shaders to WGSL at build time

fn main() {
    // Compile interactive canvas shader
    wesl::Wesl::new("src/shaders")
        .build_artifact(&"package::interactive".parse().unwrap(), "interactive_shader");

    // Compile bunnymark shader
    wesl::Wesl::new("src/shaders")
        .build_artifact(&"package::bunnymark".parse().unwrap(), "bunnymark_shader");

    // Compile glow post-processor shader (legacy simple version)
    wesl::Wesl::new("src/shaders")
        .build_artifact(&"package::glow".parse().unwrap(), "glow_shader");

    // Compile multi-pass bloom shader
    wesl::Wesl::new("src/shaders")
        .build_artifact(&"package::bloom".parse().unwrap(), "bloom_shader");

    // Re-run if any shader files change
    println!("cargo::rerun-if-changed=src/shaders/");
}
