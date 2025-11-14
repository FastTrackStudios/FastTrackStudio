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
  // This StatusBar is deprecated - use EnhancedStatusBar instead
  // Keeping minimal implementation to avoid breaking existing code

  const handleGoToStart = () => {
    console.log('StatusBar: GoToStart clicked - use EnhancedStatusBar instead');
  };

  const handleTogglePlayPause = () => {
    console.log('StatusBar: PlayPause clicked - use EnhancedStatusBar instead');
  };

  const handleToggleLoop = () => {
    console.log('StatusBar: Loop clicked - use EnhancedStatusBar instead');
  };

  const handleNext = () => {
    console.log('StatusBar: Next clicked - use EnhancedStatusBar instead');
  };

  // Deprecated component - use EnhancedStatusBar instead
  const isPlaying = false;
  const isRecording = false;
  const isLooping = false;

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
