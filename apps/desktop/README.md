# FastTrackStudio Desktop - Ableset Performance View

A full-screen desktop application featuring the Ableset performance theme for digital audio workstation control. Built with Tauri, React, TypeScript, and TanStack Router.

## Features

- **Full-Screen Performance View**: Immersive Ableset theme optimized for live performances
- **Transport Controls**: Play/pause/stop/record with visual feedback
- **Real-Time WebSocket Integration**: Connects to DAW servers for live control
- **Dark Theme**: Studio-optimized dark color scheme
- **Responsive Design**: Scales beautifully across different screen sizes

## Technology Stack

- **Frontend**: React 19, TypeScript, Tailwind CSS v4
- **Desktop Framework**: Tauri 2
- **Routing**: TanStack Router with file-based routing
- **UI Components**: Radix UI with shadcn/ui
- **Styling**: Custom Ableset theme with CSS variables
- **WebSocket**: Real-time DAW communication

## Ableset Theme

The Ableset theme provides a professional performance interface with:

- **Deep Background**: `#080909` for minimal eye strain
- **Light Surfaces**: `#182024` for UI elements
- **Primary Green**: `#48bb78` for active states and highlights
- **Typography**: High contrast white text with proper hierarchy
- **Transport Bar**: Fixed bottom transport controls with 4-button layout

## Architecture

```
src/
├── components/
│   ├── daw/
│   │   └── performance/
│   │       └── themes/
│   │           └── ableset/          # Ableset theme components
│   │               ├── Ableset.tsx   # Main performance view
│   │               ├── StatusBar.tsx # Bottom transport controls
│   │               ├── SongTitle.tsx # Large song title display
│   │               └── ...           # Other theme components
│   ├── ui/                           # Reusable UI components
│   └── theme-provider.tsx            # Theme management
├── contexts/
│   └── WebSocketContext.tsx          # WebSocket state management
└── routes/
    ├── __root.tsx                    # Root layout with Ableset
    └── index.tsx                     # Empty index route
```

## WebSocket Integration

The app connects to DAW servers via WebSocket for real-time control:

- **Connection**: Automatically connects to `ws://localhost:3001/ws/all`
- **Commands**: OSC-style commands for transport control
- **State Management**: Real-time transport and setlist state updates
- **Reconnection**: Automatic reconnection with exponential backoff

### Supported Commands

```typescript
// Transport controls
commands.play()
commands.pause()
commands.stop()
commands.playPause()
commands.toggleRecording()
commands.toggleLoop()

// Navigation
commands.jumpToTime(seconds)
commands.jumpToSong(identifier)
commands.jumpToSection(identifier)
```

## Development

### Prerequisites

- Node.js 18+
- Rust 1.70+
- Tauri CLI

### Setup

```bash
# Install dependencies
npm install

# Start development server
npm run dev

# Build for production
npm run build

# Run Tauri app
npm run tauri dev
```

### WebSocket Server

To test the WebSocket integration, run a compatible DAW server or mock server on `ws://localhost:3001/ws/all`.

## CSS Variables

The Ableset theme uses CSS variables for consistent theming:

```css
:root {
  /* Backgrounds */
  --ableset-color-background-deep: #080909;
  --ableset-color-background-light: #182024;
  
  /* Colors */
  --ableset-color-text: #fff;
  --ableset-color-border: #222b30;
  --ableset-color-hover: #1a2328;
  
  /* Green Scale */
  --ableset-color-default-500: #48bb78;
  --ableset-color-default-300: #9ae6b4;
  --ableset-color-default-700: #276749;
}
```

## Transport Controls

The bottom transport bar provides:

1. **Restart Song**: Jump to beginning of current song
2. **Play/Pause**: Toggle playback with visual state
3. **Loop**: Toggle section/song looping
4. **Next**: Jump to next section or song

Each button provides visual feedback and hover states for better UX.

## Performance Optimizations

- **Debounced Updates**: WebSocket track updates are debounced to prevent excessive re-renders
- **Efficient State Management**: React context with selective subscriptions
- **CSS Variables**: Runtime theming without style recalculation
- **Minimal Dependencies**: Only essential packages included

## Deployment

Build the application for production:

```bash
npm run tauri build
```

This creates platform-specific installers in `src-tauri/target/release/bundle/`.

## License

MIT License - see LICENSE file for details.