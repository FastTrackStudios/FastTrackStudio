# FastTrackStudio REAPER Extension

A minimal REAPER extension built using [reaper-rs](https://github.com/helgoboss/reaper-rs). This extension will eventually integrate with the FastTrackStudio message router to broadcast transport state changes.

## Build

1. Ensure you have the latest stable Rust installed: https://www.rust-lang.org/
2. Build the extension:
   ```bash
   cargo build
   ```
3. Copy the built library to REAPER's UserPlugins directory:
   - **macOS**: Copy `target/debug/libreaper_extension.dylib` to `$REAPER_RESOURCE_DIR/UserPlugins/reaper_extension.dylib`
     - Make sure to remove the `lib` prefix! REAPER won't load extensions with the `lib` prefix.
   - **Windows**: Copy `target/debug/reaper_extension.dll` to `%APPDATA%\REAPER\UserPlugins\reaper_extension.dll`
   - **Linux**: Copy `target/debug/libreaper_extension.so` to `~/.config/REAPER/UserPlugins/reaper_extension.so`

## Development Tips

- Create a symbolic link instead of copying, so you don't have to copy after each build:
  ```bash
  ln -s $(pwd)/target/debug/libreaper_extension.dylib ~/Library/Application\ Support/REAPER/UserPlugins/reaper_extension.dylib
  ```
- Check REAPER's console (View → Show Console) to see extension messages
- The extension uses `tracing` for logging - check logs for detailed information

## Future Integration

This extension will be enhanced to:
- Monitor REAPER's transport state (play, pause, stop, position, tempo)
- Broadcast state changes via the FastTrackStudio message router (OSC/WebSocket/UDP)
- Receive commands from the message router and apply them to REAPER
- Integrate with the FastTrackStudio state server architecture

