{
  description = "Native testing development environment";

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

        rustToolchain = pkgs.rust-bin.stable.latest.default.override {
          extensions = [ "rust-src" "rust-analyzer" "clippy" "rustfmt" ];
        };

        # Common build inputs for all platforms
        commonBuildInputs = with pkgs; [
          rustToolchain
          pkg-config
          cmake
          clang
          llvmPackages.bintools

          # Font/text rendering
          fontconfig
          freetype

          # Networking
          openssl

          # Python for stylo build
          python3
        ];

        # Linux-specific packages
        linuxBuildInputs = with pkgs; [
          # X11/GUI
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

          # GTK
          gtk3
          glib
          cairo
          pango
          gdk-pixbuf
          atk
        ];

        # Darwin-specific packages
        darwinBuildInputs = with pkgs; [
          # macOS frameworks are linked automatically
          darwin.apple_sdk.frameworks.Cocoa
          darwin.apple_sdk.frameworks.CoreFoundation
          darwin.apple_sdk.frameworks.CoreGraphics
          darwin.apple_sdk.frameworks.CoreText
          darwin.apple_sdk.frameworks.IOKit
          darwin.apple_sdk.frameworks.Metal
          darwin.apple_sdk.frameworks.MetalKit
          darwin.apple_sdk.frameworks.QuartzCore
          darwin.apple_sdk.frameworks.Security
          darwin.apple_sdk.frameworks.AppKit
          darwin.libiconv
        ];

        buildInputs = commonBuildInputs
          ++ (if isLinux then linuxBuildInputs else [])
          ++ (if isDarwin then darwinBuildInputs else []);

        # Linux-specific library path
        linuxLibPath = pkgs.lib.makeLibraryPath (with pkgs; [
          fontconfig
          freetype
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
          openssl
        ]);

        # Darwin-specific library path
        darwinLibPath = pkgs.lib.makeLibraryPath (with pkgs; [
          fontconfig
          freetype
          openssl
          darwin.libiconv
        ]);

        libPath = if isLinux then linuxLibPath else darwinLibPath;

      in
      {
        devShells.default = pkgs.mkShell {
          inherit buildInputs;
          nativeBuildInputs = with pkgs; [ pkg-config ]
            ++ (if isLinux then [ wrapGAppsHook3 ] else []);

          shellHook = ''
            ${if isLinux then ''
              export LD_LIBRARY_PATH="${libPath}:$LD_LIBRARY_PATH"
              export PKG_CONFIG_PATH="${pkgs.fontconfig.dev}/lib/pkgconfig:${pkgs.freetype.dev}/lib/pkgconfig:${pkgs.openssl.dev}/lib/pkgconfig:${pkgs.xorg.libX11.dev}/lib/pkgconfig:${pkgs.xorg.libXcursor.dev}/lib/pkgconfig:${pkgs.xorg.libXrandr.dev}/lib/pkgconfig:${pkgs.xorg.libXi.dev}/lib/pkgconfig:${pkgs.xorg.libxcb.dev}/lib/pkgconfig:${pkgs.libxkbcommon.dev}/lib/pkgconfig:${pkgs.wayland.dev}/lib/pkgconfig:${pkgs.libGL.dev}/lib/pkgconfig:$PKG_CONFIG_PATH"
              export LIBRARY_PATH="${pkgs.xdotool}/lib:$LIBRARY_PATH"
            '' else ''
              export DYLD_LIBRARY_PATH="${libPath}:$DYLD_LIBRARY_PATH"
              export PKG_CONFIG_PATH="${pkgs.fontconfig.dev}/lib/pkgconfig:${pkgs.freetype.dev}/lib/pkgconfig:${pkgs.openssl.dev}/lib/pkgconfig:$PKG_CONFIG_PATH"
            ''}
            echo "Native testing dev environment loaded (${if isDarwin then "Darwin" else "Linux"})"
            echo "Rust version: $(rustc --version)"
          '';

          LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
        };
      }
    );
}
