reaper_plugins := "$HOME/.config/REAPER/UserPlugins"

build:
    cargo build -p fts-extensions

build-release:
    cargo build -p fts-extensions --release

install: build
    mkdir -p {{reaper_plugins}}
    ln -sf "$(pwd)/target/debug/libreaper_fts_extensions.so" "{{reaper_plugins}}/reaper_fts_extensions.so"
    @echo "Symlinked → {{reaper_plugins}}/reaper_fts_extensions.so"

install-release: build-release
    mkdir -p {{reaper_plugins}}
    ln -sf "$(pwd)/target/release/libreaper_fts_extensions.so" "{{reaper_plugins}}/reaper_fts_extensions.so"
    @echo "Symlinked → {{reaper_plugins}}/reaper_fts_extensions.so"

uninstall:
    rm -f "{{reaper_plugins}}/reaper_fts_extensions.so"
    @echo "Removed {{reaper_plugins}}/reaper_fts_extensions.so"
