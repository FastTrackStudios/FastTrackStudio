import React from "react";
import { cn } from "@/lib/utils";
import { useWebSocket } from "../../../../../contexts/WebSocketContext";

interface SongDescriptionProps {
  className?: string;
}

export const SongDescription: React.FC<SongDescriptionProps> = ({
  className = "",
}) => {
  const { setlistState, connected } = useWebSocket();

  // Get song description - this would come from song metadata
  const getSongDescription = (): string => {
    if (!connected) return "Disconnected from REAPER";
    if (!setlistState?.current_song) return "No song playing";

    // TODO: Get actual description from song metadata
    // For now, return a placeholder based on the song name
    const songName = setlistState.current_song.name;

    // Generate a simple description based on song name
    if (songName.toLowerCase().includes("intro")) {
      return "Opening track with atmospheric build-up";
    } else if (songName.toLowerCase().includes("verse")) {
      return "Main verse section with vocals";
    } else if (songName.toLowerCase().includes("chorus")) {
      return "High-energy chorus section";
    } else if (songName.toLowerCase().includes("bridge")) {
      return "Transitional bridge with dynamic changes";
    } else if (songName.toLowerCase().includes("outro")) {
      return "Closing section with fade-out";
    } else {
      return "Description";
    }
  };

  const description = getSongDescription();

  return (
    <div className={cn("w-full text-center", className)}>
      <p
        className="text-lg font-normal opacity-80"
        style={{
          color: "var(--ableset-color-text)",
          lineHeight: "1.4",
        }}
      >
        {description}
      </p>
    </div>
  );
};
