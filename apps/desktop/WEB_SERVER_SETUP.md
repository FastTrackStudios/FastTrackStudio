# Web Server Setup for Vite Application

## Overview

The Tauri desktop application now includes an embedded Axum web server that serves the built Vite application, allowing network access from any device on the same network.

## How It Works

1. **Build the Vite App**: The Vite app is built to `apps/desktop/dist/`
2. **Start Tauri App**: When the Tauri app starts, it launches an Axum web server
3. **Serve Static Files**: The web server serves files from the `dist/` directory
4. **Network Access**: The server binds to `0.0.0.0:8080`, making it accessible from any device on your network

## Features

- ✅ **Static File Serving**: Serves all assets (JS, CSS, images) from the Vite build
- ✅ **SPA Routing**: All routes serve `index.html` for client-side routing
- ✅ **CORS Enabled**: Allows cross-origin requests from any device
- ✅ **Network Access**: Binds to `0.0.0.0` so it's accessible from other devices
- ✅ **Automatic Port**: Uses port 8080 by default (configurable)

## Usage

### Development

1. Build the Vite app:
   ```bash
   cd apps/desktop
   pnpm build
   ```

2. Run the Tauri app:
   ```bash
   cd apps/desktop
   pnpm tauri dev
   ```

3. Access from any device:
   - Find your computer's IP address (e.g., `192.168.1.100`)
   - Open `http://192.168.1.100:8080` in any browser on your network

### Production

The web server automatically starts when the Tauri app launches. Users can access the UI from any device on their network.

## Configuration

You can modify the web server configuration in `src-tauri/src/main.rs`:

```rust
let web_config = web_server::WebServerConfig {
    port: 8080,                    // Change port if needed
    dist_path: dist_path,          // Path to dist directory
    enable_cors: true,             // Enable/disable CORS
    allowed_origins: Vec::new(),   // Empty = allow all origins
};
```

## Architecture

```
┌─────────────────────────────────────┐
│      Tauri Desktop App              │
│  ┌───────────────────────────────┐  │
│  │   Axum Web Server (Port 8080) │  │
│  │   - Serves Vite dist/          │  │
│  │   - CORS enabled               │  │
│  │   - SPA routing                │  │
│  └───────────────────────────────┘  │
└─────────────────────────────────────┘
           │
           │ HTTP (0.0.0.0:8080)
           │
    ┌──────┼──────┬──────┬──────┐
    │      │      │      │      │
  Phone  Tablet  Laptop  Other  ...
```

## Next Steps

1. **Add API Routes**: Add your message router API endpoints to `/api/*`
2. **WebSocket Support**: Add WebSocket endpoint for real-time updates
3. **Authentication**: Add authentication if needed for network access
4. **HTTPS**: Consider adding HTTPS support for production

## Files

- `src-tauri/src/web_server.rs` - Web server implementation
- `src-tauri/src/main.rs` - Tauri app entry point (starts web server)
- `apps/desktop/dist/` - Built Vite application (served by web server)

