{
  description = "FastTrackStudio - Music production toolset for REAPER DAW";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    crane.url = "github:ipetkov/crane";
  };

  outputs = { self, flake-parts, crane, ... } @inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" "aarch64-linux" ];

      perSystem = { self', config, pkgs, lib, system, ... }:
        let
          # Rust toolchain with WASM support
          rustToolchain = pkgs.rust-bin.stable.latest.default.override {
            extensions = [ "rust-src" "rust-analyzer" "clippy" "rustfmt" ];
            targets = [ "wasm32-unknown-unknown" ];
          };

          # Crane lib configured with our toolchain
          craneLib = (crane.mkLib pkgs).overrideToolchain rustToolchain;

          # Version info
          rev = toString (self.shortRev or self.dirtyShortRev or self.lastModified or "unknown");

          # Source filtering
          src = craneLib.cleanCargoSource ./.;

          # Build dependencies
          buildInputs = (with pkgs; [
            openssl openssl.dev libiconv pkg-config fontconfig freetype cmake python3
          ])
          ++ lib.optionals pkgs.stdenv.isLinux (with pkgs; [
            alsa-lib alsa-lib.dev jack2
            glib gtk3 libsoup_3 webkitgtk_4_1 xdotool
            xorg.libX11 xorg.libXcursor xorg.libXrandr xorg.libXi xorg.libxcb
            libxkbcommon wayland libGL vulkan-loader
          ])
          ++ lib.optionals pkgs.stdenv.isDarwin (with pkgs; [
            # Use the unified Apple SDK (frameworks included automatically)
            apple-sdk_15
            libiconv
          ]);

          nativeBuildInputs = with pkgs; [
            pkg-config
            rustPlatform.bindgenHook
            dioxus-cli
            wasm-bindgen-cli
            just
            tailwindcss_4
          ];

          # Common args for all crane builds
          commonArgs = {
            inherit src buildInputs nativeBuildInputs;
            strictDeps = true;
            LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
            OPENSSL_DIR = "${pkgs.openssl.dev}";
            OPENSSL_LIB_DIR = "${pkgs.openssl.out}/lib";
            CC_wasm32_unknown_unknown = "${pkgs.llvmPackages_18.clang}/bin/clang";
            AR_wasm32_unknown_unknown = "${pkgs.llvmPackages_18.bintools}/bin/llvm-ar";
          };

          # Build workspace dependencies (cached)
          cargoArtifacts = craneLib.buildDepsOnly commonArgs;

          # Library path for runtime
          libPath = lib.makeLibraryPath (with pkgs;
            [ fontconfig freetype openssl ]
            ++ lib.optionals pkgs.stdenv.isLinux [
              alsa-lib jack2 libGL vulkan-loader gtk3 glib
              xorg.libX11 xorg.libxcb libxkbcommon wayland
              webkitgtk_4_1 libsoup_3
            ]
          );

        in {
          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [ inputs.rust-overlay.overlays.default ];
          };

          formatter = pkgs.nixfmt-rfc-style;

          # ============================================================
          # Packages
          # ============================================================
          packages = {
            deps = cargoArtifacts;

            web = craneLib.buildPackage (commonArgs // {
              pname = "fts-web";
              version = rev;
              inherit cargoArtifacts;
              buildPhaseCargoCommand = "dx build --release --platform web";
              installPhaseCommand = ''
                mkdir -p $out/www
                cp -r target/dx/web/release/web/* $out/www/
              '';
              doCheck = false;
            });

            desktop = craneLib.buildPackage (commonArgs // {
              pname = "fts-desktop";
              version = rev;
              inherit cargoArtifacts;
              buildPhaseCargoCommand = "dx build --release --platform desktop";
              installPhaseCommand = ''
                mkdir -p $out/bin
                cp target/dx/desktop/release/desktop $out/bin/fts-desktop
              '';
              doCheck = false;
            });

            reaper = craneLib.buildPackage (commonArgs // {
              pname = "fts-reaper-extension";
              version = rev;
              inherit cargoArtifacts;
              cargoExtraArgs = "-p reaper_extension";
              installPhaseCommand = ''
                mkdir -p $out/lib
                ${if pkgs.stdenv.isDarwin then ''
                  cp target/release/libreaper_extension.dylib $out/lib/reaper_fts.dylib
                '' else ''
                  cp target/release/libreaper_extension.so $out/lib/reaper_fts.so
                ''}
              '';
              doCheck = false;
            });

            gain = craneLib.buildPackage (commonArgs // {
              pname = "fts-gain";
              version = rev;
              inherit cargoArtifacts;
              buildPhaseCargoCommand = "cargo xtask bundle gain --release";
              installPhaseCommand = ''
                mkdir -p $out/lib/clap $out/lib/vst3
                cp -r target/bundled/gain.clap $out/lib/clap/ 2>/dev/null || true
                cp -r target/bundled/gain.vst3 $out/lib/vst3/ 2>/dev/null || true
              '';
              doCheck = false;
            });

            guide = craneLib.buildPackage (commonArgs // {
              pname = "fts-guide";
              version = rev;
              inherit cargoArtifacts;
              buildPhaseCargoCommand = "cargo xtask bundle guide --release";
              installPhaseCommand = ''
                mkdir -p $out/lib/clap $out/lib/vst3
                cp -r target/bundled/guide.clap $out/lib/clap/ 2>/dev/null || true
                cp -r target/bundled/guide.vst3 $out/lib/vst3/ 2>/dev/null || true
              '';
              doCheck = false;
            });

            eq = craneLib.buildPackage (commonArgs // {
              pname = "fts-eq";
              version = rev;
              inherit cargoArtifacts;
              buildPhaseCargoCommand = "cargo xtask bundle eq --release";
              installPhaseCommand = ''
                mkdir -p $out/lib/clap $out/lib/vst3
                cp -r target/bundled/eq.clap $out/lib/clap/ 2>/dev/null || true
                cp -r target/bundled/eq.vst3 $out/lib/vst3/ 2>/dev/null || true
              '';
              doCheck = false;
            });

            spectrum-analyzer = craneLib.buildPackage (commonArgs // {
              pname = "fts-spectrum-analyzer";
              version = rev;
              inherit cargoArtifacts;
              buildPhaseCargoCommand = "cargo xtask bundle spectrum_analyzer --release";
              installPhaseCommand = ''
                mkdir -p $out/lib/clap $out/lib/vst3
                cp -r target/bundled/spectrum_analyzer.clap $out/lib/clap/ 2>/dev/null || true
                cp -r target/bundled/spectrum_analyzer.vst3 $out/lib/vst3/ 2>/dev/null || true
              '';
              doCheck = false;
            });

            default = self'.packages.web;
          };

          # ============================================================
          # Checks
          # ============================================================
          checks = {
            clippy = craneLib.cargoClippy (commonArgs // {
              inherit cargoArtifacts;
              cargoClippyExtraArgs = "--all-targets -- --deny warnings";
            });

            fmt = craneLib.cargoFmt { inherit src; };

            tests = craneLib.cargoNextest (commonArgs // {
              inherit cargoArtifacts;
              partitions = 1;
              partitionType = "count";
            });
          };

          # ============================================================
          # Dev Shell
          # ============================================================
          devShells.default = pkgs.mkShell {
            name = "fts-dev";
            inherit buildInputs nativeBuildInputs;

            packages = with pkgs; [
              rustToolchain
              dioxus-cli
              wasm-bindgen-cli
              tailwindcss_4
              just
              cargo-watch
              cargo-nextest
              bacon
            ];

            shellHook = ''
              ${if pkgs.stdenv.isLinux then ''
                export LD_LIBRARY_PATH="${libPath}:$LD_LIBRARY_PATH"
                export XDG_DATA_DIRS="${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}:${pkgs.gtk3}/share/gsettings-schemas/${pkgs.gtk3.name}:$XDG_DATA_DIRS"
              '' else ''
                export DYLD_LIBRARY_PATH="${libPath}:$DYLD_LIBRARY_PATH"
              ''}
              export CC_wasm32_unknown_unknown="${pkgs.llvmPackages_18.clang}/bin/clang"
              export AR_wasm32_unknown_unknown="${pkgs.llvmPackages_18.bintools}/bin/llvm-ar"
              export RUST_SRC_PATH="${rustToolchain}/lib/rustlib/src/rust/library"
              [ -f .env ] && { set -a; source .env; set +a; }
              echo "FastTrackStudio dev environment (${system})"
            '';

            LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
            OPENSSL_DIR = "${pkgs.openssl.dev}";
            OPENSSL_LIB_DIR = "${pkgs.openssl.out}/lib";
          };
        };
    };
}
