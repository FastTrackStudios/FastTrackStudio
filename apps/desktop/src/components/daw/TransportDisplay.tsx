import React from "react";
import {
  Play,
  Pause,
  Square,
  SkipBack,
  SkipForward,
  Circle,
} from "lucide-react";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { useWebSocket } from "../contexts/WebSocketContext";

interface TransportDisplayProps {
  className?: string;
}

export const TransportDisplay: React.FC<TransportDisplayProps> = ({
  className = "",
}) => {
  const { transportState, connected, connectionStatus } = useWebSocket();

  const formatTime = (seconds: number): string => {
    const minutes = Math.floor(seconds / 60);
    const secs = Math.floor(seconds % 60);
    const ms = Math.floor((seconds % 1) * 100);
    return `${minutes}:${secs.toString().padStart(2, "0")}.${ms
      .toString()
      .padStart(2, "0")}`;
  };

  return (
    <Card className={className}>
      <CardHeader>
        <CardTitle className="flex items-center justify-between">
          <span>Transport</span>
          <Badge variant={connected ? "default" : "destructive"}>
            {connectionStatus === "connecting"
              ? "Connecting..."
              : connectionStatus === "connected"
              ? "Connected"
              : connectionStatus === "error"
              ? "Error"
              : "Disconnected"}
          </Badge>
        </CardTitle>
      </CardHeader>
      <CardContent className="space-y-4">
        {/* Transport Controls */}
        <div className="flex items-center justify-center gap-2">
          <Button variant="outline" size="sm">
            <SkipBack className="h-4 w-4" />
          </Button>
          <Button variant="outline" size="sm">
            {transportState?.is_playing ? (
              <Pause className="h-4 w-4" />
            ) : (
              <Play className="h-4 w-4" />
            )}
          </Button>
          <Button variant="outline" size="sm">
            <Square className="h-4 w-4" />
          </Button>
          <Button variant="outline" size="sm">
            <SkipForward className="h-4 w-4" />
          </Button>
          {transportState?.is_recording && (
            <Button variant="destructive" size="sm">
              <Circle className="h-4 w-4 fill-current" />
            </Button>
          )}
        </div>

        {/* Transport Information */}
        {transportState && (
          <div className="grid grid-cols-2 gap-4 text-sm">
            <div>
              <span className="text-muted-foreground">Position:</span>
              <div className="font-mono text-lg">
                {formatTime(transportState.position_seconds)}
              </div>
            </div>
            <div>
              <span className="text-muted-foreground">Tempo:</span>
              <div className="font-mono text-lg">
                {transportState.tempo_bpm.toFixed(1)} BPM
              </div>
            </div>
            <div>
              <span className="text-muted-foreground">Time Signature:</span>
              <div className="font-mono">
                {transportState.time_signature_numerator}/
                {transportState.time_signature_denominator}
              </div>
            </div>
            <div>
              <span className="text-muted-foreground">Status:</span>
              <div className="flex items-center gap-2">
                {transportState.is_playing && (
                  <Badge variant="default" className="text-xs">
                    Playing
                  </Badge>
                )}
                {transportState.is_paused && (
                  <Badge variant="secondary" className="text-xs">
                    Paused
                  </Badge>
                )}
                {transportState.is_recording && (
                  <Badge variant="destructive" className="text-xs">
                    Recording
                  </Badge>
                )}
                {transportState.repeat_enabled && (
                  <Badge variant="outline" className="text-xs">
                    Loop
                  </Badge>
                )}
              </div>
            </div>
          </div>
        )}

        {!connected && (
          <div className="text-center text-muted-foreground">
            <p>Not connected to REAPER</p>
            <p className="text-xs">
              Make sure REAPER is running with the FTS Extensions plugin loaded.
            </p>
          </div>
        )}
      </CardContent>
    </Card>
  );
};
