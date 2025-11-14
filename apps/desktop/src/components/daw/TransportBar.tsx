import React, { useState } from "react";
import { Play, Pause, Square, SkipBack, SkipForward } from "lucide-react";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";
import type { Transport } from "../../bindings";

interface TransportBarProps {
  className?: string;
  transport: Transport | null;
  connected: boolean;
  loading: boolean;
  error: string | null;
  onPlayPause: () => void;
  onStop: () => void;
  onGoToStart: () => void;
}

export const TransportBar: React.FC<TransportBarProps> = ({
  className = "",
  transport,
  connected,
  loading,
  error,
  onPlayPause,
  onStop,
  onGoToStart,
}) => {
  const [isExpanded, setIsExpanded] = useState(false);
  const [isVisible, setIsVisible] = useState(true);

  if (!isVisible) {
    return (
      <div className="fixed bottom-4 right-4">
        <Button
          variant="outline"
          size="sm"
          onClick={() => setIsVisible(true)}
          className="opacity-50 hover:opacity-100"
        >
          Show Transport
        </Button>
      </div>
    );
  }

  return (
    <div
      className={`fixed bottom-4 right-4 bg-background border rounded-lg shadow-lg transition-all duration-200 ${
        isExpanded ? "p-4" : "p-2"
      } ${className}`}
    >
      <div className="flex items-center gap-2">
        {/* Connection Status */}
        <Badge
          variant={connected ? "default" : "destructive"}
          className="text-xs"
        >
          {loading && !appState
            ? "Loading..."
            : connected
            ? "Connected"
            : error
            ? "Error"
            : "Disconnected"}
        </Badge>

        {/* Transport Controls */}
        <div className="flex items-center gap-1">
          <Button
            variant="ghost"
            size="sm"
            onClick={onGoToStart}
            disabled={loading}
          >
            <SkipBack className="h-4 w-4" />
          </Button>
          <Button
            variant="ghost"
            size="sm"
            onClick={onPlayPause}
            disabled={loading}
          >
            {transport?.play_state === 'Playing' ? (
              <Pause className="h-4 w-4" />
            ) : (
              <Play className="h-4 w-4" />
            )}
          </Button>
          <Button
            variant="ghost"
            size="sm"
            onClick={onStop}
            disabled={loading}
          >
            <Square className="h-4 w-4" />
          </Button>
          <Button variant="ghost" size="sm" disabled>
            <SkipForward className="h-4 w-4" />
          </Button>
        </div>

        {/* Expand/Collapse Button */}
        <Button
          variant="ghost"
          size="sm"
          onClick={() => setIsExpanded(!isExpanded)}
        >
          {isExpanded ? "−" : "+"}
        </Button>

        {/* Hide Button */}
        <Button
          variant="ghost"
          size="sm"
          onClick={() => setIsVisible(false)}
          className="text-muted-foreground hover:text-foreground"
        >
          ×
        </Button>
      </div>

      {/* Expanded Content */}
      {isExpanded && transport && (
        <div className="mt-3 pt-3 border-t space-y-2 text-sm">
          <div className="grid grid-cols-2 gap-2">
            <div>
              <span className="text-muted-foreground">Position:</span>
              <div className="font-mono">
                {Math.floor((transport.playhead_position?.time?.seconds || 0) / 60)}:
                {Math.floor((transport.playhead_position?.time?.seconds || 0) % 60)
                  .toString()
                  .padStart(2, "0")}
              </div>
            </div>
            <div>
              <span className="text-muted-foreground">BPM:</span>
              <div className="font-mono">
                {(transport.tempo?.bpm || 120).toFixed(1)}
              </div>
            </div>
            <div>
              <span className="text-muted-foreground">Playing:</span>
              <div>{transport.play_state === 'Playing' ? "Yes" : "No"}</div>
            </div>
            <div>
              <span className="text-muted-foreground">Recording:</span>
              <div>{transport.play_state === 'Recording' ? "Yes" : "No"}</div>
            </div>
          </div>
        </div>
      )}
    </div>
  );
};
