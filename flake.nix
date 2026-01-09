{
  description = "FastTrackStudio development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };

        # Rust toolchain - using latest stable
        rustToolchain = pkgs.rust-bin.stable.latest.default.override {
          extensions = [ "rust-src" "rust-analyzer" "clippy" "rustfmt" ];
          targets = [ "wasm32-unknown-unknown" ];
        };

        # Common build inputs
        buildInputs = with pkgs; [
          # Rust toolchain
          rustToolchain
          cargo-watch
          cargo-expand

          # Build tools
          pkg-config
          cmake
          gnumake
          clang
          llvmPackages.bintools

          # CSS tooling
          tailwindcss_4

          # Font/text rendering (for yeslogic-fontconfig-sys)
          fontconfig
          freetype

          # Audio development
          alsa-lib
          jack2
          pipewire

          # X11/GUI development
          xorg.libX11
          xorg.libXcursor
          xorg.libXrandr
          xorg.libXi
          xorg.libxcb
          libxkbcommon
          xdotool

          # Wayland
          wayland
          wayland-protocols

          # OpenGL/Vulkan
          libGL
          vulkan-loader
          vulkan-headers

          # GTK (for file dialogs via rfd)
          gtk3
          glib
          cairo
          pango
          gdk-pixbuf
          atk

          # WebKitGTK (for dioxus-desktop webview)
          webkitgtk_4_1
          libsoup_3

          # Networking
          openssl

          # Python (for stylo/servo CSS build)
          python3

          # Other utilities
          just
          watchexec
        ];

        # Libraries to link against
        nativeBuildInputs = with pkgs; [
          pkg-config
          wrapGAppsHook3
        ];

        # Runtime library paths
        libPath = pkgs.lib.makeLibraryPath (with pkgs; [
          fontconfig
          freetype
          alsa-lib
          jack2
          pipewire
          xorg.libX11
          xorg.libXcursor
          xorg.libXrandr
          xorg.libXi
          xorg.libxcb
          libxkbcommon
          xdotool
          wayland
          libGL
          vulkan-loader
          gtk3
          glib
          cairo
          pango
          gdk-pixbuf
          atk
          webkitgtk_4_1
          libsoup_3
          openssl
        ]);

      in
      {
        devShells.default = pkgs.mkShell {
          inherit buildInputs nativeBuildInputs;

          shellHook = ''
            export LD_LIBRARY_PATH="${libPath}:$LD_LIBRARY_PATH"
            export PKG_CONFIG_PATH="${pkgs.fontconfig.dev}/lib/pkgconfig:${pkgs.freetype.dev}/lib/pkgconfig:${pkgs.alsa-lib.dev}/lib/pkgconfig:${pkgs.openssl.dev}/lib/pkgconfig:${pkgs.xorg.libX11.dev}/lib/pkgconfig:${pkgs.xorg.libXcursor.dev}/lib/pkgconfig:${pkgs.xorg.libXrandr.dev}/lib/pkgconfig:${pkgs.xorg.libXi.dev}/lib/pkgconfig:${pkgs.xorg.libxcb.dev}/lib/pkgconfig:${pkgs.libxkbcommon.dev}/lib/pkgconfig:${pkgs.wayland.dev}/lib/pkgconfig:${pkgs.libGL.dev}/lib/pkgconfig:$PKG_CONFIG_PATH"
            export LIBRARY_PATH="${pkgs.xdotool}/lib:$LIBRARY_PATH"

            # For Wayland
            export XDG_DATA_DIRS="${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}:${pkgs.gtk3}/share/gsettings-schemas/${pkgs.gtk3.name}:$XDG_DATA_DIRS"

            # Rust flags for linking
            export RUSTFLAGS="-C link-arg=-Wl,-rpath,${libPath}"

            echo "FastTrackStudio development environment loaded"
            echo "Rust version: $(rustc --version)"
            echo ""
            echo "Available commands:"
            echo "  just help          - Show all justfile commands"
            echo "  just build         - Build the workspace"
            echo "  just dev <plugin>  - Build and test a plugin with REAPER"
          '';

          # Environment variables
          LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
          BINDGEN_EXTRA_CLANG_ARGS = "-isystem ${pkgs.llvmPackages.libclang.lib}/lib/clang/${pkgs.llvmPackages.libclang.version}/include";
        };
      }
    );
}
