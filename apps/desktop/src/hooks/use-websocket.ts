import { useEffect, useState, useRef } from 'react';

export type SetlistMessage = 
  | { type: 'SetlistUpdate'; setlist: Setlist; active_song_index?: number | null }
  | { type: 'TransportUpdate'; project_name: string; is_active: boolean; playing: boolean; position: number; tempo: number; song_progress: Record<string, number>; section_progress: Record<string, number> }
  | { type: 'RequestSetlist' }
  | { type: 'SwitchToProject'; project_name: string }
  | { type: 'SeekToSection'; project_name: string; song_name: string; section_name: string }
  | { type: 'Ping' }
  | { type: 'Pong' };

export interface Setlist {
  id: string | null;
  name: string;
  songs: Song[];
  metadata: Record<string, string>;
}

export interface Song {
  id: string | null;
  name: string;
  sections: Section[];
  metadata: Record<string, string>;
  count_in_marker: Marker | null;
  start_marker: Marker | null;
  song_end_marker: Marker | null;
  render_start_marker: Marker | null;
  render_end_marker: Marker | null;
  end_marker: Marker | null;
  song_region_start_marker: Marker | null;
  song_region_end_marker: Marker | null;
  tempo_time_sig_changes?: Array<unknown>;
}

export interface Marker {
  name: string;
  position: Position;
  color?: number | null;
}

export interface Section {
  name: string;
  section_type: string;
  start_position: Position;
  end_position: Position;
  color?: number | null;
  metadata?: Record<string, string>;
}

export interface Marker {
  name: string;
  position: Position;
}

export interface Position {
  time: { seconds: number };
  musical_position: MusicalPosition;
}

export interface MusicalPosition {
  measure: number;
  beat: number;
  tick: number;
}

export interface TransportState {
  is_active: boolean;
  playing: boolean;
  position: number;
  tempo: number;
  /// Progress for each song (0-1), keyed by song name
  song_progress: Record<string, number>;
  /// Progress for each section (0-1), keyed by "song_name:section_name"
  section_progress: Record<string, number>;
  /// Timestamp when this project started playing (milliseconds since epoch)
  /// Only set when playing transitions from false to true
  started_playing_at?: number;
}

// Map of project name to transport state
export type TransportStates = Map<string, TransportState>;

export function useWebSocket(url: string) {
  const [connectionState, setConnectionState] = useState<'connecting' | 'connected' | 'disconnected' | 'error'>('disconnected');
  const [setlist, setSetlist] = useState<Setlist | null>(null);
  const [activeSongIndex, setActiveSongIndex] = useState<number | null>(null);
  const [transportStates, setTransportStates] = useState<TransportStates>(new Map());
  const wsRef = useRef<WebSocket | null>(null);
  const reconnectTimeoutRef = useRef<number | null>(null);

  useEffect(() => {
    let mounted = true;

    const connect = () => {
      if (!mounted) return;

      try {
        setConnectionState('connecting');
        const ws = new WebSocket(url);
        wsRef.current = ws;

        ws.onopen = () => {
          if (!mounted) return;
          console.log('WebSocket connected');
          setConnectionState('connected');
          if (reconnectTimeoutRef.current) {
            clearTimeout(reconnectTimeoutRef.current);
            reconnectTimeoutRef.current = null;
          }
          
          // Request setlist immediately on connection
          try {
            const request = JSON.stringify({ type: 'RequestSetlist' });
            ws.send(request);
            console.log('Requested setlist update from server');
          } catch (error) {
            console.error('Failed to send setlist request:', error);
          }
        };

        ws.onmessage = (event) => {
          if (!mounted) return;
          try {
            const rawData = event.data;
            console.log('WebSocket message received:', rawData);
            const message: SetlistMessage = JSON.parse(rawData);
            console.log('Parsed message:', message);
            
            switch (message.type) {
              case 'SetlistUpdate':
                console.log('SetlistUpdate received:', message.setlist);
                console.log('Songs count:', message.setlist.songs?.length || 0);
                console.log('Active song index:', message.active_song_index);
                setSetlist(message.setlist);
                // Store active song index from backend if provided
                if (message.active_song_index !== undefined && message.active_song_index !== null) {
                  setActiveSongIndex(message.active_song_index);
                }
                break;
              case 'TransportUpdate':
                setTransportStates((prev) => {
                  const next = new Map(prev);
                  const oldState = prev.get(message.project_name);
                  
                  // Only log when is_active changes to help debug
                  if (oldState && oldState.is_active !== message.is_active) {
                    console.log(`Active project changed: ${message.project_name} is_active=${message.is_active}`);
                  }
                  
                  // Track when a project starts playing
                  let started_playing_at = oldState?.started_playing_at;
                  if (message.playing && !oldState?.playing) {
                    // Transitioned from not playing to playing - record timestamp
                    started_playing_at = Date.now();
                  } else if (!message.playing) {
                    // Not playing anymore - clear timestamp
                    started_playing_at = undefined;
                  }
                  
                  next.set(message.project_name, {
                    is_active: message.is_active,
                  playing: message.playing,
                  position: message.position,
                  tempo: message.tempo,
                    song_progress: message.song_progress,
                    section_progress: message.section_progress,
                    started_playing_at,
                  });
                  return next;
                });
                break;
              case 'Ping':
                // Respond to ping with pong
                if (wsRef.current?.readyState === WebSocket.OPEN) {
                  wsRef.current.send(JSON.stringify({ type: 'Pong' }));
                }
                break;
              case 'Pong':
                // Pong received, connection is alive
                break;
            }
          } catch (error) {
            console.error('Failed to parse WebSocket message:', error);
          }
        };

        ws.onerror = (error) => {
          if (!mounted) return;
          console.error('WebSocket error:', error);
          setConnectionState('error');
        };

        ws.onclose = () => {
          if (!mounted) return;
          console.log('WebSocket disconnected');
          setConnectionState('disconnected');
          wsRef.current = null;

          // Attempt to reconnect after 5 seconds
          if (mounted) {
            reconnectTimeoutRef.current = window.setTimeout(() => {
              connect();
            }, 5000);
          }
        };
      } catch (error) {
        console.error('Failed to create WebSocket:', error);
        setConnectionState('error');
      }
    };

    connect();

    return () => {
      mounted = false;
      if (reconnectTimeoutRef.current) {
        clearTimeout(reconnectTimeoutRef.current);
      }
      if (wsRef.current) {
        wsRef.current.close();
        wsRef.current = null;
      }
    };
  }, [url]);

  const switchToProject = (projectName: string) => {
    if (wsRef.current?.readyState === WebSocket.OPEN) {
      const message: SetlistMessage = { type: 'SwitchToProject', project_name: projectName };
      wsRef.current.send(JSON.stringify(message));
      console.log(`Requested switch to project: ${projectName}`);
    } else {
      console.warn('WebSocket not connected, cannot switch project');
    }
  };

  const seekToSection = (projectName: string, songName: string, sectionName: string) => {
    if (wsRef.current?.readyState === WebSocket.OPEN) {
      const message: SetlistMessage = { 
        type: 'SeekToSection', 
        project_name: projectName,
        song_name: songName,
        section_name: sectionName
      };
      wsRef.current.send(JSON.stringify(message));
      console.log(`Requested seek to section: ${projectName}/${songName}/${sectionName}`);
    } else {
      console.warn('WebSocket not connected, cannot seek to section');
    }
  };

  return { connectionState, setlist, activeSongIndex, transportStates, switchToProject, seekToSection };
}

