/**
 * Adapter hook to provide a WebSocketContext-like API for performance theme components
 * This maps Effect Atom state to the legacy structure expected by Ableset theme components
 */
import { useAtomValue } from '@effect-atom/atom-react';
import {
  connectionStateAtom,
  setlistAtom,
  activeSongIndexAtom,
  transportStatesAtom,
} from '../atoms/websocket';
import { currentSongIndexAtom, currentSectionIndexAtom, activeProjectNameAtom } from '../atoms/derived';
import type { Setlist, Song, TransportState, TransportStates } from '../atoms/types';

// Helper to get project name from song metadata
const getProjectName = (song: Song): string | null => {
  return song.metadata?.project_name || song.metadata?.Project || song.metadata?.project || null;
};

// Helper to find song by index
const findSongByIndex = (setlist: Setlist | null, index: number): Song | null => {
  if (!setlist || index < 0 || index >= setlist.songs.length) return null;
  return setlist.songs[index];
};

// Helper to find section by index in a song
const findSectionByIndex = (song: Song | null, index: number) => {
  if (!song || index < 0 || index >= song.sections.length) return null;
  return song.sections[index];
};

// Helper to get transport state for active project
const getActiveTransport = (transportStates: TransportStates, activeProjectName: string | null): TransportState | null => {
  if (!activeProjectName) return null;
  return transportStates.get(activeProjectName) || null;
};

// Legacy SetlistState interface expected by performance components
interface LegacySetlistState {
  songs: Array<{
    index: number;
    name: string;
    start_position_seconds: number;
    end_position_seconds: number;
    start_position_beats: number;
    end_position_beats: number;
    length_measures: number;
    length_time: number;
    sections: Array<{
      index: number;
      name: string;
      start_position_beats: number;
      end_position_beats: number;
      length_measures: number;
      length_time: number;
      color?: number;
    }>;
    color?: number;
    tempo_changes: any[];
  }>;
  current_song: {
    index: number;
    name: string;
    start_position_seconds: number;
    end_position_seconds: number;
    start_position_beats: number;
    end_position_beats: number;
    length_measures: number;
    length_time: number;
    sections: any[];
    color?: number;
  } | null;
  current_section: {
    index: number;
    name: string;
    start_position_beats: number;
    end_position_beats: number;
    length_measures: number;
    length_time: number;
    color?: number;
  } | null;
  next_section: {
    index: number;
    name: string;
    start_position_beats: number;
    end_position_beats: number;
    length_measures: number;
    length_time: number;
    color?: number;
  } | null;
  queued_name: [string, string];
  queued_index: [number, number];
  timestamp: number;
}

// Legacy TransportState interface expected by performance components
interface LegacyTransportState {
  position_seconds: number;
  tempo_bpm: number;
  is_playing: boolean;
  is_recording: boolean;
  is_paused: boolean;
  repeat_enabled: boolean;
  time_signature_numerator: number;
  time_signature_denominator: number;
  timestamp: number;
}

// Legacy commands interface
interface LegacyCommands {
  playPause: () => void;
  toggleRecording: () => void;
  toggleLoop: () => void;
  jumpToSong: (index: number) => void;
  jumpToTime: (seconds: number) => void;
  jumpBySections: (delta: number) => void;
  jumpBySongs: (delta: number) => void;
}

/**
 * Adapter hook that provides legacy WebSocketContext API
 * Maps Effect Atom state to the structure expected by performance theme components
 */
export function useWebSocketAdapter() {
  const connectionState = useAtomValue(connectionStateAtom);
  const setlist = useAtomValue(setlistAtom);
  const activeSongIndex = useAtomValue(activeSongIndexAtom);
  const currentSongIndex = useAtomValue(currentSongIndexAtom);
  const currentSectionIndex = useAtomValue(currentSectionIndexAtom);
  const transportStates = useAtomValue(transportStatesAtom);
  const activeProjectName = useAtomValue(activeProjectNameAtom);

  // Determine which song index to use (prefer backend active, fallback to derived)
  const songIndex = activeSongIndex !== null && activeSongIndex !== undefined ? activeSongIndex : (currentSongIndex ?? null);
  
  const currentSong = songIndex !== null && songIndex !== undefined ? findSongByIndex(setlist, songIndex) : null;
  const activeTransport = getActiveTransport(transportStates, activeProjectName);

  // Convert Song to legacy format
  const convertSongToLegacy = (song: Song, index: number) => {
    const songStart = song.song_region_start_marker?.position.time.seconds ?? 
                      song.start_marker?.position.time.seconds ?? 
                      (song.sections.length > 0 ? song.sections[0].start_position.time.seconds : 0);
    const songEnd = song.song_region_end_marker?.position.time.seconds ?? 
                    song.song_end_marker?.position.time.seconds ?? 
                    (song.sections.length > 0 ? song.sections[song.sections.length - 1].end_position.time.seconds : songStart);
    const songLength = songEnd - songStart;

    return {
      index,
      name: song.name,
      start_position_seconds: songStart,
      end_position_seconds: songEnd,
      start_position_beats: 0, // TODO: Calculate from musical position if available
      end_position_beats: 0,
      length_measures: 0, // TODO: Calculate from sections
      length_time: songLength,
      sections: song.sections.map((section, sectionIdx) => ({
        index: sectionIdx,
        name: section.name || section.section_type,
        start_position_beats: 0, // TODO: Calculate from musical position
        end_position_beats: 0,
        length_measures: 0, // TODO: Calculate from musical position
        length_time: (section.end_position?.time?.seconds ?? 0) - (section.start_position?.time?.seconds ?? 0),
        color: undefined,
      })),
      color: undefined,
      tempo_changes: [],
    };
  };

  // Build legacy setlistState
  const setlistState: LegacySetlistState = {
    songs: (setlist?.songs ?? []).map((song, idx) => convertSongToLegacy(song, idx)),
    current_song: currentSong ? convertSongToLegacy(currentSong, songIndex ?? 0) : null,
    current_section: currentSong && currentSectionIndex !== null ? {
      index: currentSectionIndex,
      name: currentSong.sections[currentSectionIndex]?.name || currentSong.sections[currentSectionIndex]?.section_type || '',
      start_position_beats: 0,
      end_position_beats: 0,
      length_measures: 0,
      length_time: (currentSong.sections[currentSectionIndex]?.end_position?.time?.seconds ?? 0) - 
                   (currentSong.sections[currentSectionIndex]?.start_position?.time?.seconds ?? 0),
      color: undefined,
    } : null,
    next_section: currentSong && currentSectionIndex !== null && currentSectionIndex < currentSong.sections.length - 1 ? {
      index: currentSectionIndex + 1,
      name: currentSong.sections[currentSectionIndex + 1]?.name || currentSong.sections[currentSectionIndex + 1]?.section_type || '',
      start_position_beats: 0,
      end_position_beats: 0,
      length_measures: 0,
      length_time: (currentSong.sections[currentSectionIndex + 1]?.end_position?.time?.seconds ?? 0) - 
                   (currentSong.sections[currentSectionIndex + 1]?.start_position?.time?.seconds ?? 0),
      color: undefined,
    } : null,
    queued_name: ['', ''],
    queued_index: [0, 0],
    timestamp: Date.now(),
  };

  // Build legacy transportState
  const transportState: LegacyTransportState | null = activeTransport ? {
    position_seconds: activeTransport.position,
    tempo_bpm: activeTransport.tempo,
    is_playing: activeTransport.playing,
    is_recording: false, // TODO: Add recording state to TransportState
    is_paused: !activeTransport.playing,
    repeat_enabled: false, // TODO: Add repeat state to TransportState
    time_signature_numerator: 4, // TODO: Add time signature to TransportState
    time_signature_denominator: 4,
    timestamp: Date.now(),
  } : null;

  // Build legacy commands (stubs for now - TODO: implement actual commands)
  const commands: LegacyCommands = {
    playPause: () => {
      console.warn('playPause command not implemented');
    },
    toggleRecording: () => {
      console.warn('toggleRecording command not implemented');
    },
    toggleLoop: () => {
      console.warn('toggleLoop command not implemented');
    },
    jumpToSong: (index: number) => {
      console.warn('jumpToSong command not implemented', index);
    },
    jumpToTime: (seconds: number) => {
      console.warn('jumpToTime command not implemented', seconds);
    },
    jumpBySections: (delta: number) => {
      console.warn('jumpBySections command not implemented', delta);
    },
    jumpBySongs: (delta: number) => {
      console.warn('jumpBySongs command not implemented', delta);
    },
  };

  const connected = connectionState === 'connected';
  const connectionStatus = connectionState;

  return {
    setlistState,
    transportState,
    connected,
    connectionStatus,
    commands,
  };
}

