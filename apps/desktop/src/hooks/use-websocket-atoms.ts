import { useAtomValue, useAtomSet } from "@effect-atom/atom-react";
import {
  connectionStateAtom,
  setlistAtom,
  activeSongIndexAtom,
  transportStatesAtom,
  switchToProjectAtom,
  seekToSectionAtom,
  wsConnectionAtom,
} from "../atoms/websocket";
import type { Setlist, TransportStates } from "../atoms/types";

/**
 * Hook to use WebSocket state managed by Effect Atom
 * The WebSocket connection atom auto-starts when accessed (via keepAlive)
 */
export function useWebSocketAtoms() {
  // Access the connection atom to ensure it's initialized (it will auto-start)
  // This is a no-op read, but ensures the atom is created and kept alive
  useAtomValue(wsConnectionAtom);
  
  // Get state values
  const connectionState = useAtomValue(connectionStateAtom);
  const setlist = useAtomValue(setlistAtom);
  const activeSongIndex = useAtomValue(activeSongIndexAtom);
  const transportStates = useAtomValue(transportStatesAtom);
  
  // Get action functions using useAtomSet for Atom.fn
  const switchToProject = useAtomSet(switchToProjectAtom);
  const seekToSection = useAtomSet(seekToSectionAtom);
  
  return {
    connectionState,
    setlist,
    activeSongIndex,
    transportStates,
    switchToProject: (projectName: string) => switchToProject(projectName),
    seekToSection: (projectName: string, songName: string, sectionName: string) =>
      seekToSection({ projectName, songName, sectionName }),
  };
}

