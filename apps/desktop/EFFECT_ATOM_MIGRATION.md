# Effect Atom Migration Status

## Overview
Migrating WebSocket state management from React hooks to Effect Atom for better state management and Effect integration.

## Current Status
- ✅ Dependencies installed (`@effect-atom/atom-react`, `effect`, `@effect/platform`)
- ✅ Atom structure created (`connectionStateAtom`, `setlistAtom`, `activeSongIndexAtom`, `transportStatesAtom`)
- ✅ Derived atoms created (`currentSongIndexAtom`, `currentSectionIndexAtom`, `activeProjectNameAtom`)
- ✅ React hook wrapper created (`useWebSocketAtoms`)
- ⚠️ WebSocket connection atom has Registry API issues

## Issues

### Registry API
The main challenge is updating atoms from WebSocket callbacks (which are outside Effect context). The Registry API usage needs clarification:

1. `registry.set()` - Method doesn't exist on `Registry.AtomRegistry` type
2. `registry.update()` - Method doesn't exist on `Registry.AtomRegistry` type
3. Need to understand correct way to update atoms from non-Effect contexts

## Next Steps

1. **Research Effect Atom Registry API**
   - Check documentation for correct way to update atoms
   - May need to use `Atom.writable` with custom setters
   - Or use a message queue pattern with Effect processing

2. **Alternative Approaches**
   - Use `Atom.writable` for atoms that need external updates
   - Create a bridge between WebSocket callbacks and Effect context
   - Use Effect's async/streaming capabilities for WebSocket

3. **Complete Migration**
   - Fix WebSocket connection atom
   - Update main page component to use new hook
   - Test all functionality

## Files Created

- `apps/desktop/src/atoms/runtime.ts` - Effect Atom runtime setup
- `apps/desktop/src/atoms/types.ts` - Type exports
- `apps/desktop/src/atoms/websocket.ts` - WebSocket atoms (needs Registry API fixes)
- `apps/desktop/src/atoms/derived.ts` - Derived/computed atoms
- `apps/desktop/src/hooks/use-websocket-atoms.ts` - React hook wrapper

## Files to Update

- `apps/desktop/src/routes/index.tsx` - Replace `useWebSocket` with `useWebSocketAtoms`
- Components using WebSocket state - Update to use new hook

