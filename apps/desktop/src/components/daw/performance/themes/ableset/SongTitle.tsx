import React from "react";
import { Music } from "lucide-react";
import type { SetlistState } from "../../../../../types/placeholders";

interface SongTitleProps {
  className?: string;
  setlistState: SetlistState | null;
  connected: boolean;
  loading: boolean;
  error: string | null;
}

export const SongTitle: React.FC<SongTitleProps> = ({
  className = "",
  setlistState,
  connected,
  loading,
  error
}) => {

  // Get current song title
  const getCurrentSongTitle = (): string => {
    if (loading) return "Loading...";
    if (error) return "Connection Error";
    if (!connected) return "Disconnected from App State";
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
