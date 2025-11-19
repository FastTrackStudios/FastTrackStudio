import React from "react";
import { Music } from "lucide-react";
import { useWebSocketAdapter } from "../../../../hooks/use-websocket-adapter";

interface SongInfo {
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
}

interface SetlistState {
  songs: SongInfo[];
  current_song: SongInfo | null;
  timestamp: number;
}

interface WebSocketMessage {
  type: "Setlist" | "Transport" | "MarkerRegions" | "Error";
  data: SetlistState | any;
}

interface SongTitleProps {
  className?: string;
}

export const SongTitle: React.FC<SongTitleProps> = ({ className = "" }) => {
  const { setlistState, connected, connectionStatus } = useWebSocketAdapter();

  // Get current song title
  const getCurrentSongTitle = (): string => {
    if (connectionStatus === "connecting") return "Connecting...";
    if (connectionStatus === "error") return "Connection Error";
    if (!connected) return "Disconnected from REAPER";
    if (!setlistState?.current_song) return "No Song Playing";
    return setlistState.current_song.name;
  };

  // Get song index for display
  const getCurrentSongIndex = (): string => {
    if (!setlistState?.current_song) return "";
    return `${setlistState.current_song.index + 1}`;
  };

  return (
    <div
      className={`flex flex-col items-center w-full ${className}`}
      style={{
        background: "var(--ableset-color-background-deep)",
        color: "var(--ableset-color-text)",
      }}
    >
      <div className="text-center w-full max-w-5xl">
        {/* Main Song Title */}
        <h1
          className="font-bold tracking-tight leading-none"
          style={{
            color: "var(--ableset-color-default-500)",
            fontSize: "calc(var(--ui-size, 1) * 14vmin)",
          }}
        >
          {getCurrentSongTitle()}
        </h1>
      </div>
    </div>
  );
};
