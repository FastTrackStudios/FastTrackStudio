//! Generate FFI bindings for libwpe + wpebackend-fdo + wpe-webkit-2.0.
//!
//! Discovers the C libraries via `pkg-config` (Nix dev shell exports
//! `PKG_CONFIG_PATH`) and runs bindgen with narrow allowlists per
//! library so the generated `*_sys.rs` files stay small and the lib
//! crate can pick exactly what it re-exports.
//!
//! Linux-only — on macOS / Windows / non-Linux Unix this build script
//! is a no-op and the crate exposes no API. Phase 2 / #19.

use std::env;
use std::path::PathBuf;

fn main() {
    let target_os = env::var("CARGO_CFG_TARGET_OS").unwrap_or_default();
    if target_os != "linux" {
        // wpe-webkit upstream doesn't ship for non-Linux — skip.
        return;
    }

    let out_dir = PathBuf::from(env::var("OUT_DIR").expect("OUT_DIR set by cargo"));

    generate_libwpe(&out_dir);
    generate_wpebackend_fdo(&out_dir);
    generate_wpe_webkit(&out_dir);

    // Re-run if any of the C library pkg-config files change.
    println!("cargo:rerun-if-env-changed=PKG_CONFIG_PATH");
    println!("cargo:rerun-if-changed=build.rs");
}

fn generate_libwpe(out_dir: &PathBuf) {
    let lib = match pkg_config::Config::new()
        .atleast_version("1.14")
        .probe("wpe-1.0")
    {
        Ok(lib) => lib,
        Err(e) => {
            println!("cargo:warning=wpe-1.0 not found: {e}");
            return;
        }
    };

    let mut builder = bindgen::Builder::default()
        .header_contents("libwpe_wrapper.h", "#include <wpe/wpe.h>\n")
        .allowlist_function("wpe_.*")
        .allowlist_type("wpe_.*")
        .allowlist_var("WPE_.*")
        .generate_comments(true)
        .derive_debug(true);
    for path in &lib.include_paths {
        builder = builder.clang_arg(format!("-I{}", path.display()));
    }
    builder
        .generate()
        .expect("bindgen libwpe")
        .write_to_file(out_dir.join("libwpe_sys.rs"))
        .expect("write libwpe_sys.rs");
}

fn generate_wpebackend_fdo(out_dir: &PathBuf) {
    let lib = match pkg_config::Config::new()
        .atleast_version("1.14")
        .probe("wpebackend-fdo-1.0")
    {
        Ok(lib) => lib,
        Err(e) => {
            println!("cargo:warning=wpebackend-fdo-1.0 not found: {e}");
            return;
        }
    };

    let mut builder = bindgen::Builder::default()
        .header_contents(
            "wpebackend_fdo_wrapper.h",
            "#include <wpe/fdo.h>\n#include <wpe/extensions/video-plane-display-dmabuf.h>\n#include <wpe/fdo-egl.h>\n",
        )
        .allowlist_function("wpe_fdo_.*")
        .allowlist_function("wpe_view_backend_exportable_.*")
        .allowlist_function("wpe_video_plane_display_dmabuf_.*")
        .allowlist_type("wpe_fdo_.*")
        .allowlist_type("wpe_video_plane_display_dmabuf_.*")
        .allowlist_var("WPE_FDO_.*")
        .opaque_type("_EGLDisplay")
        .opaque_type("EGLDisplay")
        .generate_comments(true)
        .derive_debug(true);
    for path in &lib.include_paths {
        builder = builder.clang_arg(format!("-I{}", path.display()));
    }
    builder
        .generate()
        .expect("bindgen wpebackend-fdo")
        .write_to_file(out_dir.join("wpebackend_fdo_sys.rs"))
        .expect("write wpebackend_fdo_sys.rs");
}

fn generate_wpe_webkit(out_dir: &PathBuf) {
    let lib = match pkg_config::Config::new()
        .atleast_version("2.40")
        .probe("wpe-webkit-2.0")
    {
        Ok(lib) => lib,
        Err(e) => {
            println!("cargo:warning=wpe-webkit-2.0 not found: {e}");
            return;
        }
    };

    let mut builder = bindgen::Builder::default()
        .header_contents("wpe_webkit_wrapper.h", "#include <wpe/webkit.h>\n")
        // Pick a narrow surface for the spike — enough to create a
        // WebView, load a URI, and shut it down. Phase 2 will widen as
        // the integration grows.
        .allowlist_function("webkit_web_context_.*")
        .allowlist_function("webkit_web_view_.*")
        .allowlist_function("webkit_settings_.*")
        .allowlist_function("webkit_user_content_manager_.*")
        .allowlist_function("webkit_web_view_backend_.*")
        .allowlist_type("WebKit.*")
        .allowlist_var("WEBKIT_.*")
        // GObject basics needed for ref/unref + signal connect.
        .allowlist_function("g_object_unref")
        .allowlist_function("g_object_ref")
        .allowlist_function("g_signal_connect_data")
        .allowlist_function("g_main_context_iteration")
        .allowlist_function("g_main_context_default")
        .allowlist_function("g_main_context_pending")
        .allowlist_type("GObject")
        .allowlist_type("GType")
        .allowlist_type("GMainContext")
        // Treat heavy GLib / Cairo / JSC types as opaque pointers.
        .opaque_type("_GValue")
        .opaque_type("_GTypeClass")
        .opaque_type("_GTypeInstance")
        .opaque_type("_GData")
        .opaque_type("_GList")
        .opaque_type("_GSList")
        .opaque_type("_GError")
        .opaque_type("_GBytes")
        .opaque_type("_GVariant")
        .opaque_type("_GCancellable")
        .opaque_type("_GInputStream")
        .opaque_type("_GOutputStream")
        .opaque_type("_GAsyncResult")
        .opaque_type("_GTlsCertificate")
        .opaque_type("_GUri")
        .opaque_type("_cairo.*")
        .opaque_type("_SoupMessage.*")
        .opaque_type("_JSC.*")
        .generate_comments(true)
        .derive_debug(true);
    for path in &lib.include_paths {
        builder = builder.clang_arg(format!("-I{}", path.display()));
    }
    // Walk wpe-webkit's transitive include paths. pkg-config's default
    // probe doesn't follow `Requires.private` so we top up explicitly
    // with the headers wpe-webkit's internal `wpe-platform/wpe/*`
    // headers transitively include (xkbcommon, glib, cairo, etc.).
    for extra in &[
        "xkbcommon",
        "glib-2.0",
        "gobject-2.0",
        "gio-2.0",
        "cairo",
        "harfbuzz",
        "libdrm",
        "gbm",
        "egl",
    ] {
        if let Ok(ext) = pkg_config::Config::new().probe(extra) {
            for path in &ext.include_paths {
                builder = builder.clang_arg(format!("-I{}", path.display()));
            }
        }
    }
    builder
        .generate()
        .expect("bindgen wpe-webkit")
        .write_to_file(out_dir.join("wpe_webkit_sys.rs"))
        .expect("write wpe_webkit_sys.rs");
}
