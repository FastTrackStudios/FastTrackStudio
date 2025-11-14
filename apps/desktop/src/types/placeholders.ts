// Placeholder types for components that reference types not yet generated
// TODO: Remove these when the corresponding Rust types are implemented and exported

export interface SectionInfo {
  index: number;
  name: string;
  start_position_beats: number;
  end_position_beats: number;
  start_position_seconds: number;
  end_position_seconds: number;
  length_measures: number;
  length_time: number;
  color?: number;
}

export interface SongInfo {
  index: number;
  name: string;
  start_position_seconds: number;
  end_position_seconds: number;
  start_position_beats: number;
  end_position_beats: number;
  length_measures: number;
  length_time: number;
  sections: SectionInfo[];
  color?: number;
  tempo_changes?: TempoChangeInfo[];
}

export interface TempoChangeInfo {
  position_seconds: number;
  position_beats: number;
  tempo: number;
  time_signature_numerator: number;
  time_signature_denominator: number;
  is_tempo_marker: boolean;
  is_time_signature_marker: boolean;
}

export interface SetlistState {
  songs: SongInfo[];
  current_song: SongInfo | null;
  current_section: SectionInfo | null;
  next_section: SectionInfo | null;
  previous_song: SongInfo | null;
  next_song: SongInfo | null;
  queued_name: [string, string]; // [song_name, section_name]
  queued_index: [number, number]; // [song_index, section_index]
  length_time: number;
  timestamp: number;
}

export interface MarkerInfo {
  id: number;
  name: string;
  position_seconds: number;
  color?: number;
}

export interface RegionInfo {
  id: number;
  name: string;
  position_seconds: number;
  end_position_seconds: number;
  color?: number;
}

export interface MarkerRegionState {
  markers: MarkerInfo[];
  regions: RegionInfo[];
  current_marker: MarkerInfo | null;
  current_region: RegionInfo | null;
  timestamp: number;
}
