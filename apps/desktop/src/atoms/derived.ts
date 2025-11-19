import { Atom } from "@effect-atom/atom-react";
import { setlistAtom, transportStatesAtom, activeSongIndexAtom } from "./websocket";
import type { Song, TransportState } from "./types";

// Helper to get project name from song metadata
const getProjectName = (song: Song): string | null => {
  return song.metadata?.project_name || song.metadata?.Project || song.metadata?.project || null;
};

// Helper to find song by position in a project
const findSongByPosition = (
  setlist: { songs: Song[] } | null,
  projectName: string,
  position: number
): number | null => {
  if (!setlist) return null;
  
  for (let i = 0; i < setlist.songs.length; i++) {
    const song = setlist.songs[i];
    const songProjectName = getProjectName(song);
    
    // Only check songs from this project
    if (songProjectName !== projectName) continue;

    // Try song_region markers first, then fall back to start_marker/song_end_marker
    const songStart = song.song_region_start_marker?.position.time.seconds ?? 
                      song.start_marker?.position.time.seconds ?? 
                      (song.sections.length > 0 ? song.sections[0].start_position.time.seconds : 0);
    const songEnd = song.song_region_end_marker?.position.time.seconds ?? 
                    song.song_end_marker?.position.time.seconds ?? 
                    (song.sections.length > 0 ? song.sections[song.sections.length - 1].end_position.time.seconds : songStart);

    if (position >= songStart && position <= songEnd) {
      return i;
    }
  }
  
  return null;
};

// Helper to find first song in a project
const findFirstSongInProject = (
  setlist: { songs: Song[] } | null,
  projectName: string
): number | null => {
  if (!setlist) return null;
  
  for (let i = 0; i < setlist.songs.length; i++) {
    const song = setlist.songs[i];
    const songProjectName = getProjectName(song);
    if (songProjectName === projectName) {
      return i;
    }
  }
  
  return null;
};

// Active project name atom (derived from transport states)
export const activeProjectNameAtom = Atom.make((get) => {
  const transportStates = get(transportStatesAtom);
  for (const [projectName, transport] of transportStates.entries()) {
    if (transport.is_active) {
      return projectName;
    }
  }
  return null as string | null;
});

// Current song index atom (derived from setlist, transport states, and active song index)
export const currentSongIndexAtom = Atom.make((get) => {
  const setlist = get(setlistAtom);
  const transportStates = get(transportStatesAtom);
  const backendActiveSongIndex = get(activeSongIndexAtom);
  const activeProjectName = get(activeProjectNameAtom);
  
  if (!setlist || setlist.songs.length === 0) return undefined;
  
  // Use backend-provided active song index if available and valid
  if (backendActiveSongIndex !== null && backendActiveSongIndex !== undefined) {
    if (backendActiveSongIndex >= 0 && backendActiveSongIndex < setlist.songs.length) {
      return backendActiveSongIndex;
    }
  }
  
  if (transportStates.size === 0) {
    // If no transport states, return first song as fallback
    return 0;
  }

  // Find all playing projects with their start timestamps
  const playingProjects: Array<{ projectName: string; startedAt: number; transport: TransportState }> = [];
  for (const [projectName, transport] of transportStates.entries()) {
    if (transport.playing && transport.started_playing_at) {
      playingProjects.push({
        projectName,
        startedAt: transport.started_playing_at,
        transport,
      });
    }
  }

  // If something is playing, use the most recently started one
  if (playingProjects.length > 0) {
    // Sort by started_playing_at descending (most recent first)
    playingProjects.sort((a, b) => b.startedAt - a.startedAt);
    const mostRecent = playingProjects[0];
    
    // Try to find song by position
    const songIndex = findSongByPosition(setlist, mostRecent.projectName, mostRecent.transport.position);
    if (songIndex !== null) return songIndex;
    
    // Fallback to first song in project
    const firstSong = findFirstSongInProject(setlist, mostRecent.projectName);
    if (firstSong !== null) return firstSong;
  }

  // If nothing is playing, use the active project
  if (activeProjectName) {
    const activeTransport = transportStates.get(activeProjectName);
    if (activeTransport) {
      // Try to find song by position
      const songIndex = findSongByPosition(setlist, activeProjectName, activeTransport.position);
      if (songIndex !== null) return songIndex;
      
      // Fallback to first song in active project
      const firstSong = findFirstSongInProject(setlist, activeProjectName);
      if (firstSong !== null) return firstSong;
    }
  }

  // Final fallback: return first song
  return 0;
});

// Current section index atom (derived from current song and transport position)
export const currentSectionIndexAtom = Atom.make((get) => {
  const setlist = get(setlistAtom);
  const transportStates = get(transportStatesAtom);
  const currentSongIndex = get(currentSongIndexAtom);
  
  if (!setlist || transportStates.size === 0 || currentSongIndex === undefined) return null;

  const song = setlist.songs[currentSongIndex];
  const songProjectName = getProjectName(song);
  if (!songProjectName) return null;

  const transport = transportStates.get(songProjectName);
  if (!transport) return null;

  // Use active project even if not playing
  const currentPos = transport.position;

  for (let i = 0; i < song.sections.length; i++) {
    const section = song.sections[i];
    const start = section.start_position.time.seconds;
    const end = section.end_position.time.seconds;

    if (currentPos >= start && currentPos <= end) {
      return i;
    }
  }

  return null;
});

