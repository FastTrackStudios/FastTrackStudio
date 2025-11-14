import React from "react";
import { ChevronRight, Pause, Play, RotateCcw, SkipBack } from "lucide-react";
import { cn } from "@/lib/utils";
import type { SetlistState } from "../../../../../types/placeholders";
import type { Transport } from "../../../../../bindings";

interface TransportBarProps {
  className?: string;
  setlistState: SetlistState | null;
  transport: Transport | null;
  connected: boolean;
  loading: boolean;
  error: string | null;
  onPlayPause: () => void;
  onStop: () => void;
  onGoToStart: () => void;
  onToggleLoop: () => void;
  onNext: () => void;
}

interface TransportButtonProps {
  children: React.ReactNode;
  className?: string;
  onClick?: () => void;
  isActive?: boolean;
}

const TransportButton: React.FC<TransportButtonProps> = ({
  children,
  className,
  onClick,
  isActive = false,
}) => {
  return (
    <button
      className={cn(
        "flex items-center justify-center h-full relative",
        "transition-colors duration-200",
        isActive
          ? "bg-[var(--ableset-color-default-active)]"
          : "hover:bg-[var(--ableset-color-hover)]",
        className
      )}
      onClick={onClick}
    >
      {children}
    </button>
  );
};

export const TransportBar: React.FC<TransportBarProps> = ({
  className,
  setlistState,
  transport,
  connected,
  loading,
  error,
  onPlayPause,
  onStop,
  onGoToStart,
  onToggleLoop,
  onNext
}) => {

  return (
    <div
      className={cn(
        "fixed bottom-0 left-0 right-0 h-20",
        "grid grid-cols-4",
        "divide-x-2 divide-[var(--ableset-color-border)]",
        "border-t-2 border-[var(--ableset-color-border)]",
        "bg-[var(--ableset-color-background-light)]",
        className
      )}
    >
      {/* Go to Song Start */}
      <TransportButton onClick={onGoToStart}>
        <div className="flex items-center justify-center">
          <SkipBack
            className="mr-2 h-6 w-6 text-[var(--ableset-color-text)]"
            strokeWidth={1.5}
          />
          <span className="text-lg font-medium text-[var(--ableset-color-text)]">
            Go to Song Start
          </span>
        </div>
      </TransportButton>

      {/* Play/Pause Button */}
      <TransportButton
        onClick={onPlayPause}
        isActive={transport?.play_state === 'Playing' || false}
      >
        <div className="relative flex items-center justify-center">
          {transport?.play_state === 'Playing' ? (
            <Pause
              className="h-12 w-12 text-[var(--ableset-color-text)]"
              strokeWidth={1.5}
            />
          ) : (
            <Play
              className="h-12 w-12 text-[var(--ableset-color-text)]"
              strokeWidth={1.5}
            />
          )}

          {/* Connection status indicator */}
          <div
            className={cn(
              "absolute bottom-1 right-1 h-3 w-3 rounded-full",
              connected ? "bg-green-500" : "bg-red-500"
            )}
            title={
              connected ? "Connected to App State" : "Not connected to App State"
            }
          />
        </div>
      </TransportButton>

      {/* Loop Button */}
      <TransportButton
        onClick={onToggleLoop}
        isActive={transport?.looping || false}
      >
        <div className="flex items-center justify-center">
          <RotateCcw
            className="mr-2 h-6 w-6 text-[var(--ableset-color-text)]"
            strokeWidth={1.5}
          />
          <span className="text-lg font-medium text-[var(--ableset-color-text)]">
            Loop
          </span>
        </div>
      </TransportButton>

      {/* Next Button */}
      <TransportButton onClick={onNext}>
        <div className="flex items-center justify-center">
          <span className="text-lg font-medium text-[var(--ableset-color-text)] mr-2">
            Next
          </span>
          <ChevronRight
            className="h-6 w-6 text-[var(--ableset-color-text)]"
            strokeWidth={1.5}
          />
        </div>
      </TransportButton>
    </div>
  );
};
