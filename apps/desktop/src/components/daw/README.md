# DAW Components Architecture

This directory contains the DAW (Digital Audio Workstation) components for FastTrackStudio, organized using a **Container/Presentational Component** pattern for better maintainability, testability, and reusability.

## Architecture Overview

The components are organized into two main categories:

### 📦 Containers (`/containers/`)
Smart components that manage state and data fetching using the `useAppState` hook. These components:
- Handle state management via hooks
- Manage async operations and error handling
- Pass data and callbacks as props to presentational components
- Act as the bridge between the app state and UI components

### 🎨 Presentational Components
Pure components that only receive props and render UI. These components:
- Are completely unaware of app state management
- Receive all data via props
- Are easily testable with mock data
- Can be reused in different contexts
- Support React.memo optimization

## State Management Flow

```
useAppState Hook (Single Source of Truth)
    ↓
Container Components (Smart)
    ↓ (props)
Presentational Components (Dumb)
```

## Container Components

### `TransportContainer`
Manages transport controls (play, pause, stop) and passes transport state to `TransportDisplay` and `TransportBar` components.

**Usage:**
```tsx
<TransportContainer className="my-transport" />
```

### `SetlistContainer`
Handles setlist data and provides different views:
- `display` - Basic setlist information
- `songView` - Detailed current song view with progress
- `sections` - Song sections breakdown

**Usage:**
```tsx
<SetlistContainer view="songView" className="my-setlist" />
```

### `MarkerRegionContainer`
Manages markers and regions data from the active project.

**Usage:**
```tsx
<MarkerRegionContainer className="my-markers" />
```

### `PerformanceContainer`
Handles Ableset theme components for live performance mode:
- `songTitle` - Current song title display
- `songDescription` - Song description/metadata
- `detailBadges` - Performance info badges (tempo, time sig, etc.)
- `songProgressBar` - Visual progress through song sections
- `transportBar` - Performance transport controls

**Usage:**
```tsx
<PerformanceContainer 
  component="songTitle" 
  className="my-performance"
  onSectionClick={(index) => console.log('Jump to section:', index)}
/>
```

## Presentational Components

### Transport Components
- `TransportDisplay` - Detailed transport info card
- `TransportBar` - Floating transport controls

### Setlist Components  
- `SetlistDisplay` - Basic setlist overview
- `SetlistSongView` - Current song with progress
- `SongSectionsDisplay` - Song sections breakdown

### Markers & Regions
- `MarkerRegionDisplay` - Shows project markers and regions

### Performance Theme Components (`/performance/themes/ableset/`)
- `SongTitle` - Large song title display
- `SongDescription` - Song metadata/description
- `DetailBadges` - Info badges (tempo, time signature, etc.)
- `SongProgressBar` - Visual progress bar with section indicators
- `TransportBar` - Full-width performance transport controls

## Props Interface Examples

### Transport Components
```tsx
interface TransportProps {
  transport: TransportState | null;
  connected: boolean;
  loading: boolean;
  error: string | null;
  onPlayPause: () => void;
  onStop: () => void;
  onGoToStart: () => void;
}
```

### Setlist Components
```tsx
interface SetlistProps {
  setlistState: SetlistState | null;
  transport?: TransportState | null; // For progress calculations
  connected: boolean;
  loading: boolean;
  error: string | null;
}
```

## Benefits of This Architecture

### 🧪 **Testability**
Presentational components can be tested in isolation with mock props, making unit tests simple and reliable.

```tsx
// Easy to test
render(
  <SongTitle 
    setlistState={{ current_song: { name: "Test Song" } }}
    connected={true}
    loading={false}
    error={null}
  />
);
```

### ♻️ **Reusability**
Components can be used in different contexts without state coupling.

```tsx
// Can be used anywhere with different data sources
<SongTitle {...mockData} />
<SongTitle {...liveData} />
<SongTitle {...cachedData} />
```

### 🚀 **Performance** 
Presentational components can be optimized with `React.memo` since they only depend on props.

```tsx
export const SongTitle = React.memo<SongTitleProps>(({ ... }) => {
  // Will only re-render when props change
});
```

### 🎯 **Separation of Concerns**
Clear division between state management (containers) and UI rendering (components).

### 🔧 **Maintainability**
Changes to state management don't affect UI components and vice versa.

## Migration from WebSocket Context

Previously, components used `useWebSocket()` directly:
```tsx
// Old pattern ❌
const { transportState, connected } = useWebSocket();
```

Now they receive data via props:
```tsx
// New pattern ✅
interface ComponentProps {
  transport: TransportState | null;
  connected: boolean;
}
```

The container components handle the state management:
```tsx
// Container handles state
const { appState } = useAppState();
const transport = useTransportState(appState);

return <Component transport={transport} connected={connected} />;
```

## Migration Status

### ✅ Completed Components
The following components have been successfully migrated to the container/presentational pattern:

**Transport Components:**
- `TransportDisplay` → `TransportContainer`
- `TransportBar` → `TransportContainer`

**Setlist Components:**
- `SetlistDisplay` → `SetlistContainer`
- `SetlistSongView` → `SetlistContainer`
- `SongSectionsDisplay` → `SetlistContainer`

**Markers & Regions:**
- `MarkerRegionDisplay` → `MarkerRegionContainer`

**Performance Components (Ableset Theme):**
- `SongTitle` → `PerformanceContainer`
- `SongDescription` → `PerformanceContainer`
- `DetailBadges` → `PerformanceContainer`
- `SongProgressBar` → `PerformanceContainer`
- `TransportBar` (Ableset) → `PerformanceContainer`

### 📝 Usage Examples

**Before (Old Pattern):**
```tsx
// ❌ Component managed its own state
function MyComponent() {
  const { transportState, connected } = useWebSocket();
  return <div>{transportState?.position}</div>;
}
```

**After (New Pattern):**
```tsx
// ✅ Container manages state, component receives props
function MyPage() {
  return (
    <div>
      <TransportContainer className="my-transport" />
      <SetlistContainer view="songView" />
      <PerformanceContainer component="songTitle" />
    </div>
  );
}
```

### 🚧 Current Limitations

- **Missing Backend Types**: Setlist and MarkerRegion types use placeholder interfaces
- **Limited Transport Controls**: Some advanced transport features not yet implemented
- **Static Data**: Components currently receive null data until backend integration is complete

### 🔄 Migration from WebSocket Context

Components have been migrated from `useWebSocket()` to `useAppState()`:

```tsx
// Old: Direct WebSocket usage
const { transportState, setlistState, connected } = useWebSocket();

// New: Container pattern with useAppState
const { appState, loading, error, connected } = useAppState();
const transport = useTransportState(appState);
```

## Future Enhancements

- **Backend Integration**: Replace placeholder types with actual Rust-generated types
- **React.memo Optimization**: Add memoization to presentational components
- **Error Boundaries**: Wrap container components with error handling
- **Storybook Documentation**: Create component stories for design system
- **Advanced Transport**: Implement loop controls, section navigation
- **Real-time Updates**: Optimize polling intervals for better performance
- **TypeScript Strict Mode**: Enable strict type checking
- **Custom Hooks**: Create specialized hooks for common prop transformations