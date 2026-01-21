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

        isDarwin = pkgs.stdenv.isDarwin;
        isLinux = pkgs.stdenv.isLinux;

        # Rust toolchain - using latest stable
        rustToolchain = pkgs.rust-bin.stable.latest.default.override {
          extensions = [ "rust-src" "rust-analyzer" "clippy" "rustfmt" ];
          targets = [ "wasm32-unknown-unknown" ];
        };

        # Common build inputs for all platforms
        commonBuildInputs = with pkgs; [
          # Rust toolchain
          rustToolchain
          cargo-watch
          cargo-expand
          # Note: dioxus-cli has linking issues on Darwin, install via: cargo install dioxus-cli

          # Build tools
          pkg-config
          cmake
          gnumake
          clang
          llvmPackages.bintools
          lld  # LLVM linker with WASM support

          # CSS tooling
          tailwindcss_4

          # Font/text rendering (for yeslogic-fontconfig-sys)
          fontconfig
          freetype

          # Networking
          openssl

          # Python (for stylo/servo CSS build)
          python3

          # Other utilities
          just
          watchexec
        ];

        # Linux-specific packages
        linuxBuildInputs = with pkgs; [
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
        ];

        # Darwin-specific packages
        darwinBuildInputs = with pkgs; [
          # macOS SDK and frameworks (new unified approach)
          apple-sdk_15
          libiconv
        ];

        buildInputs = commonBuildInputs
          ++ (if isLinux then linuxBuildInputs else [])
          ++ (if isDarwin then darwinBuildInputs else []);

        # Libraries to link against
        nativeBuildInputs = with pkgs; [ pkg-config ]
          ++ (if isLinux then [ wrapGAppsHook3 ] else []);

        # Linux-specific runtime library paths
        linuxLibPath = pkgs.lib.makeLibraryPath (with pkgs; [
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

        # Darwin-specific runtime library paths
        darwinLibPath = pkgs.lib.makeLibraryPath (with pkgs; [
          fontconfig
          freetype
          openssl
          libiconv
        ]);

        libPath = if isLinux then linuxLibPath else darwinLibPath;

      in
      {
        devShells.default = pkgs.mkShell {
          inherit buildInputs nativeBuildInputs;

          shellHook = ''
            ${if isLinux then ''
              export LD_LIBRARY_PATH="${libPath}:$LD_LIBRARY_PATH"
              export PKG_CONFIG_PATH="${pkgs.fontconfig.dev}/lib/pkgconfig:${pkgs.freetype.dev}/lib/pkgconfig:${pkgs.alsa-lib.dev}/lib/pkgconfig:${pkgs.openssl.dev}/lib/pkgconfig:${pkgs.xorg.libX11.dev}/lib/pkgconfig:${pkgs.xorg.libXcursor.dev}/lib/pkgconfig:${pkgs.xorg.libXrandr.dev}/lib/pkgconfig:${pkgs.xorg.libXi.dev}/lib/pkgconfig:${pkgs.xorg.libxcb.dev}/lib/pkgconfig:${pkgs.libxkbcommon.dev}/lib/pkgconfig:${pkgs.wayland.dev}/lib/pkgconfig:${pkgs.libGL.dev}/lib/pkgconfig:$PKG_CONFIG_PATH"
              export LIBRARY_PATH="${pkgs.xdotool}/lib:$LIBRARY_PATH"

              # For Wayland
              export XDG_DATA_DIRS="${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}:${pkgs.gtk3}/share/gsettings-schemas/${pkgs.gtk3.name}:$XDG_DATA_DIRS"

              # Rust flags for linking
              export RUSTFLAGS="-C link-arg=-Wl,-rpath,${libPath}"
            '' else ''
              export DYLD_LIBRARY_PATH="${libPath}:$DYLD_LIBRARY_PATH"
              export PKG_CONFIG_PATH="${pkgs.fontconfig.dev}/lib/pkgconfig:${pkgs.freetype.dev}/lib/pkgconfig:${pkgs.openssl.dev}/lib/pkgconfig:$PKG_CONFIG_PATH"
            ''}

            # WASM build support: Use LLVM clang for ring/rustls C compilation
            export CC_wasm32_unknown_unknown="${pkgs.llvmPackages_18.clang}/bin/clang"
            export AR_wasm32_unknown_unknown="${pkgs.llvmPackages_18.bintools}/bin/llvm-ar"

            echo "FastTrackStudio development environment loaded (${if isDarwin then "Darwin" else "Linux"})"
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
