import React, {
  createContext,
  useContext,
  useState,
  useEffect,
  useRef,
  ReactNode,
  useCallback,
} from "react";

// Types matching the backend domain structures
interface SectionInfo {
  index: number;
  name: string;
  start_position_beats: number;
  end_position_beats: number;
  length_measures: number;
  length_time: number;
  color?: number;
}

interface TempoChangeInfo {
  position_seconds: number;
  position_beats: number;
  tempo: number;
  time_signature_numerator: number;
  time_signature_denominator: number;
  is_tempo_marker: boolean;
  is_time_signature_marker: boolean;
}

interface SongInfo {
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
  tempo_changes: TempoChangeInfo[];
}

interface SetlistState {
  songs: SongInfo[];
  breaks: BreakInfo[];
  current_song: SongInfo | null;
  current_section: SectionInfo | null;
  next_song: SongInfo | null;
  previous_song: SongInfo | null;
  next_section: SectionInfo | null;
  previous_section: SectionInfo | null;
  queued_name: [string, string]; // [song_name, section_name]
  queued_index: [number, number]; // [song_index, section_index]
  length_measures: number;
  length_time: number;
  timestamp: number;
}

interface BreakInfo {
  global_index: number;
  index: number;
  has_delay: boolean;
  delay: number;
}

interface TransportState {
  position_seconds: number;
  position_beats: number;
  timestamp: number;
  is_playing: boolean;
  is_paused: boolean;
  is_recording: boolean;
  repeat_enabled: boolean;
  tempo_bpm: number;
  time_signature_numerator: number;
  time_signature_denominator: number;
  edit_position_seconds: number;
  edit_position_beats: number;
  measure: number;
  beat: number;
  subdivision: number;
  edit_measure: number;
  edit_beat: number;
  edit_subdivision: number;
}

interface MarkerInfo {
  id: number;
  name: string;
  position_seconds: number;
  color?: number;
}

interface RegionInfo {
  id: number;
  name: string;
  position_seconds: number;
  end_position_seconds: number;
  color?: number;
}

interface MarkerRegionState {
  markers: MarkerInfo[];
  regions: RegionInfo[];
  current_marker: MarkerInfo | null;
  current_region: RegionInfo | null;
  timestamp: number;
}

// Track interfaces
interface TrackInfo {
  index: number;
  guid: string;
  name: string;
  volume: number;
  volume_db: number;
  pan: number;
  width: number;
  is_muted: boolean;
  is_solo: boolean;
  is_armed: boolean;
  is_selected: boolean;
  color?: string;
  color_raw?: number;
  is_folder: boolean;
  folder_depth: number;
  folder_state_tcp: string;
  folder_state_mcp: string;
  show_in_tcp: boolean;
  show_in_mcp: boolean;
  send_count: number;
  receive_count: number;
  fx_count: number;
  has_fx_enabled: boolean;
  input_monitoring: string;
  recording_mode: string;
  peak_left: number;
  peak_right: number;
  peak_hold_left: number;
  peak_hold_right: number;
  channel_count: number;
  vu_mode: number;
}

interface TracksState {
  tracks: TrackInfo[];
  master_track?: TrackInfo;
  timestamp: number;
}

// Command interface matching OSC-style commands
interface WebSocketCommands {
  // Global transport commands
  play: () => void;
  pause: () => void;
  stop: () => void;
  playPause: () => void;
  playStop: () => void;
  startPlaying: () => void;
  continuePlaying: () => void;

  // Recording commands
  startRecording: () => void;
  stopRecording: () => void;
  toggleRecording: () => void;

  // Loop commands
  enableLoop: () => void;
  disableLoop: () => void;
  toggleLoop: () => void;

  // Setlist navigation commands
  jumpToTime: (time: number) => void;
  jumpToSong: (identifier: number | string) => void;
  jumpBySongs: (steps: number, force?: boolean) => void;
  jumpToSection: (identifier: number | string, fuzzy?: boolean) => void;
  jumpBySections: (steps: number, force?: boolean) => void;
  jumpToQueued: (instant?: boolean) => void;
  playCuedSong: () => void;

  // Click track commands
  muteClick: (mute?: boolean) => void;
  soloClick: (solo?: boolean) => void;

  // Settings commands
  setAutoplay: (enabled: boolean) => void;
  setAutoJumpToNextSong: (enabled: boolean) => void;
  setAutoLoopCurrentSection: (enabled: boolean) => void;
  setJumpMode: (
    mode: "quantized" | "end-of-section" | "end-of-song" | "manual"
  ) => void;

  // Track control commands
  setTrackMute: (trackGuid: string, muted: boolean) => void;
  setTrackSolo: (trackGuid: string, solo: boolean) => void;
  setTrackArm: (trackGuid: string, armed: boolean) => void;
  setTrackSelect: (trackGuid: string, selected: boolean) => void;
  setTrackVolume: (trackGuid: string, volume: number) => void;
  setTrackPan: (trackGuid: string, pan: number) => void;
}

// WebSocket message types matching backend
type WebSocketMessage =
  | { type: "Transport"; data: TransportState }
  | { type: "MarkerRegions"; data: MarkerRegionState }
  | { type: "Setlist"; data: SetlistState }
  | { type: "Tracks"; data: TracksState }
  | { type: "Error"; data: { message: string } };

// Command message type for sending commands to backend
interface CommandMessage {
  type: "Command";
  command: string;
  params?: Record<string, any>;
}

interface WebSocketContextType {
  connected: boolean;
  connectionStatus: "connecting" | "connected" | "disconnected" | "error";
  setlistState: SetlistState | null;
  transportState: TransportState | null;
  markerRegionState: MarkerRegionState | null;
  tracksState: TracksState | null;
  commands: WebSocketCommands;
}

const WebSocketContext = createContext<WebSocketContextType | null>(null);

interface WebSocketProviderProps {
  children: ReactNode;
}

export const WebSocketProvider: React.FC<WebSocketProviderProps> = ({
  children,
}) => {
  const [connected, setConnected] = useState(false);
  const [connectionStatus, setConnectionStatus] = useState<
    "connecting" | "connected" | "disconnected" | "error"
  >("disconnected");
  const [setlistState, setSetlistState] = useState<SetlistState | null>(null);
  const [transportState, setTransportState] = useState<TransportState | null>(
    null
  );
  const [markerRegionState, setMarkerRegionState] =
    useState<MarkerRegionState | null>(null);
  const [tracksState, setTracksState] = useState<TracksState | null>(null);

  const wsRef = useRef<WebSocket | null>(null);
  const reconnectTimeoutRef = useRef<NodeJS.Timeout | null>(null);
  const reconnectAttempts = useRef(0);
  const maxReconnectAttempts = 5;

  // Debouncing for tracks updates
  const tracksUpdateTimeoutRef = useRef<NodeJS.Timeout | null>(null);
  const lastTracksUpdateRef = useRef<number>(0);
  const TRACKS_UPDATE_DEBOUNCE_MS = 100; // 100ms debounce

  // Debounced tracks state setter
  const setTracksStateDebounced = useCallback((newTracksState: TracksState) => {
    const now = Date.now();

    // Clear existing timeout
    if (tracksUpdateTimeoutRef.current) {
      clearTimeout(tracksUpdateTimeoutRef.current);
    }

    // If enough time has passed since last update, update immediately
    if (now - lastTracksUpdateRef.current > TRACKS_UPDATE_DEBOUNCE_MS * 2) {
      setTracksState(newTracksState);
      lastTracksUpdateRef.current = now;
    } else {
      // Otherwise, debounce the update
      tracksUpdateTimeoutRef.current = setTimeout(() => {
        setTracksState(newTracksState);
        lastTracksUpdateRef.current = Date.now();
      }, TRACKS_UPDATE_DEBOUNCE_MS);
    }
  }, []);

  // Command sending function
  const sendCommand = (command: string, params?: Record<string, any>) => {
    if (wsRef.current && wsRef.current.readyState === WebSocket.OPEN) {
      const message: CommandMessage = {
        type: "Command",
        command,
        params: params || {},
      };
      wsRef.current.send(JSON.stringify(message));
      console.log("Sent command:", message);
    } else {
      console.warn("WebSocket not connected, cannot send command:", command);
    }
  };

  // Create commands object with OSC-style command paths
  const commands: WebSocketCommands = {
    // Global transport commands
    play: () => sendCommand("/global/play"),
    pause: () => sendCommand("/global/pause"),
    stop: () => sendCommand("/global/stop"),
    playPause: () => sendCommand("/global/playPause"),
    playStop: () => sendCommand("/global/playStop"),
    startPlaying: () => sendCommand("/global/startPlaying"),
    continuePlaying: () => sendCommand("/global/continuePlaying"),

    // Recording commands
    startRecording: () => sendCommand("/global/startRecording"),
    stopRecording: () => sendCommand("/global/stopRecording"),
    toggleRecording: () => sendCommand("/global/toggleRecording"),

    // Loop commands
    enableLoop: () => sendCommand("/loop/enable"),
    disableLoop: () => sendCommand("/loop/escape"),
    toggleLoop: () => sendCommand("/loop/toggle"),

    // Setlist navigation commands
    jumpToTime: (time: number) => sendCommand("/setlist/jumpToTime", { time }),
    jumpToSong: (identifier: number | string) =>
      sendCommand("/setlist/jumpToSong", { identifier }),
    jumpBySongs: (steps: number, force?: boolean) =>
      sendCommand("/setlist/jumpBySongs", { steps, force }),
    jumpToSection: (identifier: number | string, fuzzy?: boolean) =>
      sendCommand("/setlist/jumpToSection", { identifier, fuzzy }),
    jumpBySections: (steps: number, force?: boolean) =>
      sendCommand("/setlist/jumpBySections", { steps, force }),
    jumpToQueued: (instant?: boolean) =>
      sendCommand("/setlist/jumpToQueued", { instant }),
    playCuedSong: () => sendCommand("/setlist/playCuedSong"),

    // Click track commands
    muteClick: (mute?: boolean) => sendCommand("/click/mute", { mute }),
    soloClick: (solo?: boolean) => sendCommand("/click/solo", { solo }),

    // Settings commands
    setAutoplay: (enabled: boolean) =>
      sendCommand("/settings/autoplay", { enabled }),
    setAutoJumpToNextSong: (enabled: boolean) =>
      sendCommand("/settings/autoJumpToNextSong", { enabled }),
    setAutoLoopCurrentSection: (enabled: boolean) =>
      sendCommand("/settings/autoLoopCurrentSection", { enabled }),
    setJumpMode: (
      mode: "quantized" | "end-of-section" | "end-of-song" | "manual"
    ) => sendCommand("/settings/jumpMode", { mode }),

    // Track control commands
    setTrackMute: (trackGuid: string, muted: boolean) =>
      sendCommand("/track/mute", { guid: trackGuid, muted }),
    setTrackSolo: (trackGuid: string, solo: boolean) =>
      sendCommand("/track/solo", { guid: trackGuid, solo }),
    setTrackArm: (trackGuid: string, armed: boolean) =>
      sendCommand("/track/arm", { guid: trackGuid, armed }),
    setTrackSelect: (trackGuid: string, selected: boolean) =>
      sendCommand("/track/select", { guid: trackGuid, selected }),
    setTrackVolume: (trackGuid: string, volume: number) =>
      sendCommand("/track/volume", { guid: trackGuid, volume }),
    setTrackPan: (trackGuid: string, pan: number) =>
      sendCommand("/track/pan", { guid: trackGuid, pan }),
  };

  const connectWebSocket = () => {
    if (wsRef.current?.readyState === WebSocket.OPEN) {
      return;
    }

    setConnectionStatus("connecting");

    try {
      // Try multiple endpoints
      const endpoints = [
        "ws://127.0.0.1:3001/ws/all",
        "ws://localhost:3001/ws/all",
      ];

      const wsUrl = endpoints[0]; // Start with first endpoint
      wsRef.current = new WebSocket(wsUrl);

      wsRef.current.onopen = () => {
        console.log("WebSocket connected to:", wsUrl);
        setConnected(true);
        setConnectionStatus("connected");
        reconnectAttempts.current = 0;
      };

      wsRef.current.onmessage = (event) => {
        try {
          const message: WebSocketMessage = JSON.parse(event.data);

          switch (message.type) {
            case "Setlist":
              setSetlistState(message.data);
              break;
            case "Transport":
              console.log("Transport state received:", message.data);
              setTransportState(message.data);
              break;
            case "MarkerRegions":
              setMarkerRegionState(message.data);
              break;
            case "Tracks":
              console.log("Tracks state received:", message.data);
              setTracksStateDebounced(message.data);
              break;
            case "Error":
              console.error("WebSocket error:", message.data.message);
              break;
            default:
              console.log("Unknown message type:", message);
          }
        } catch (error) {
          console.error("Error parsing WebSocket message:", error);
        }
      };

      wsRef.current.onclose = () => {
        console.log("WebSocket disconnected");
        setConnected(false);
        setConnectionStatus("disconnected");

        // Attempt to reconnect
        if (reconnectAttempts.current < maxReconnectAttempts) {
          reconnectAttempts.current++;
          const delay = Math.min(
            1000 * Math.pow(2, reconnectAttempts.current),
            30000
          );
          console.log(
            `Attempting to reconnect in ${delay}ms (attempt ${reconnectAttempts.current}/${maxReconnectAttempts})`
          );

          reconnectTimeoutRef.current = setTimeout(() => {
            connectWebSocket();
          }, delay);
        } else {
          setConnectionStatus("error");
          console.error("Max reconnection attempts reached");
        }
      };

      wsRef.current.onerror = (error) => {
        console.error("WebSocket error:", error);
        setConnectionStatus("error");
      };
    } catch (error) {
      console.error("Failed to create WebSocket connection:", error);
      setConnectionStatus("error");
    }
  };

  useEffect(() => {
    connectWebSocket();

    return () => {
      if (reconnectTimeoutRef.current) {
        clearTimeout(reconnectTimeoutRef.current);
      }
      if (tracksUpdateTimeoutRef.current) {
        clearTimeout(tracksUpdateTimeoutRef.current);
      }
      if (wsRef.current) {
        wsRef.current.close();
      }
    };
  }, []);

  const contextValue: WebSocketContextType = {
    connected,
    connectionStatus,
    setlistState,
    transportState,
    markerRegionState,
    tracksState,
    commands,
  };

  return (
    <WebSocketContext.Provider value={contextValue}>
      {children}
    </WebSocketContext.Provider>
  );
};

export const useWebSocket = () => {
  const context = useContext(WebSocketContext);
  if (!context) {
    throw new Error("useWebSocket must be used within a WebSocketProvider");
  }
  return context;
};
