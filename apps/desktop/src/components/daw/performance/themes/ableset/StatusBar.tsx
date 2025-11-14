import React from "react";
import {
  ChevronRight,
  Pause,
  Play,
  RotateCcw,
  SkipBack,
  Square,
  CircleDot,
} from "lucide-react";
import { cn } from "@/lib/utils";
import { useWebSocket } from "../../../../../contexts/WebSocketContext";

interface StatusBarProps {
  className?: string;
}

interface TransportButtonProps {
  children: React.ReactNode;
  className?: string;
  onClick?: () => void;
  isActive?: boolean;
  activeBackgroundColor?: string;
}

const TransportButton: React.FC<TransportButtonProps> = ({
  children,
  className,
  onClick,
  isActive = false,
  activeBackgroundColor = "var(--ableset-color-default-500)",
}) => {
  return (
    <button
      className={cn(
        "flex items-center justify-center h-full relative",
        "transition-colors duration-200",
        className
      )}
      style={{
        backgroundColor: isActive
          ? activeBackgroundColor
          : "var(--ableset-color-background-light)",
      }}
      onMouseEnter={(e) => {
        e.currentTarget.style.boxShadow =
          "inset 0 0 0 1000px rgba(255, 255, 255, 0.05)";
      }}
      onMouseLeave={(e) => {
        e.currentTarget.style.boxShadow = "";
      }}
      onClick={onClick}
    >
      {children}
    </button>
  );
};

export const StatusBar: React.FC<StatusBarProps> = ({ className = "" }) => {
  const { transportState, setlistState, connected, commands } = useWebSocket();

  const handleGoToStart = () => {
    // Jump to the beginning of the current song
    if (setlistState?.current_song) {
      commands.jumpToSong(setlistState.current_song.index);
    } else {
      // Fallback: jump to time 0
      commands.jumpToTime(0);
    }
  };

  const handleTogglePlayPause = () => {
    // If recording, toggle recording instead of play/pause
    if (transportState?.is_recording) {
      commands.toggleRecording();
    } else {
      commands.playPause();
    }
  };

  const handleToggleLoop = () => {
    commands.toggleLoop();
  };

  const handleNext = () => {
    // Jump to next section, or next song if at the end of current song
    if (setlistState?.next_section) {
      commands.jumpBySections(1);
    } else if (setlistState?.next_song) {
      commands.jumpBySongs(1);
    }
  };

  const isPlaying = transportState?.is_playing || false;
  const isRecording = transportState?.is_recording || false;
  const isLooping = transportState?.repeat_enabled || false;

  return (
    <div className={`fixed bottom-0 left-0 right-0 ${className}`}>
      {/* Transport Control Buttons */}
      <div
        className={cn(
          "h-36 grid grid-cols-4",
          "divide-x-2 divide-[var(--ableset-color-border)]",
          "border-t-2 border-[var(--ableset-color-border)]"
        )}
      >
        {/* Go to Song Start */}
        <TransportButton onClick={handleGoToStart}>
          <div className="flex items-center justify-center">
            <SkipBack
              className="mr-2 h-5 w-5 text-[var(--ableset-color-text)]"
              strokeWidth={1.5}
            />
            <span className="text-base font-medium text-[var(--ableset-color-text)]">
              Restart Song
            </span>
          </div>
        </TransportButton>

        {/* Play/Pause/Record Button */}
        <TransportButton
          onClick={handleTogglePlayPause}
          isActive={isPlaying || isRecording}
          activeBackgroundColor={
            isRecording
              ? "#dc2626" // Red background for recording
              : "var(--ableset-color-default-500)" // Default green for playing
          }
        >
          <div className="relative flex items-center justify-center">
            {isRecording ? (
              <CircleDot
                className="h-10 w-10 text-white"
                strokeWidth={1.5}
                fill="white"
              />
            ) : isPlaying ? (
              <Square
                className="h-10 w-10 text-white"
                strokeWidth={1.5}
                fill="white"
              />
            ) : (
              <Play
                className="h-10 w-10 text-white"
                strokeWidth={1.5}
                fill="white"
              />
            )}
          </div>
        </TransportButton>

        {/* Loop Button */}
        <TransportButton
          onClick={handleToggleLoop}
          isActive={isLooping}
          activeBackgroundColor="#1d3a31"
        >
          <div className="flex items-center justify-center">
            <RotateCcw
              className="mr-2 h-5 w-5"
              style={{
                color: isLooping
                  ? "var(--ableset-color-default-500)"
                  : "var(--ableset-color-text)",
              }}
              strokeWidth={1.5}
            />
            <span
              className="text-base font-medium"
              style={{
                color: isLooping
                  ? "var(--ableset-color-default-500)"
                  : "var(--ableset-color-text)",
              }}
            >
              Loop
            </span>
          </div>
        </TransportButton>

        {/* Next Button */}
        <TransportButton onClick={handleNext}>
          <div className="flex items-center justify-center">
            <span className="text-base font-medium text-[var(--ableset-color-text)] mr-2">
              Next
            </span>
            <ChevronRight
              className="h-5 w-5 text-[var(--ableset-color-text)]"
              strokeWidth={1.5}
            />
          </div>
        </TransportButton>
      </div>
    </div>
  );
};
