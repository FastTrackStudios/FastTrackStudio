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
import type { Transport } from "../../bindings";

interface TransportDisplayProps {
  className?: string;
  transport: Transport | null;
  connected: boolean;
  loading: boolean;
  error: string | null;
  onPlayPause: () => void;
  onStop: () => void;
  onGoToStart: () => void;
}

export const TransportDisplay: React.FC<TransportDisplayProps> = ({
  className = "",
  transport,
  connected,
  loading,
  error,
  onPlayPause,
  onStop,
  onGoToStart,
}) => {

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
            {loading
              ? "Loading..."
              : connected
              ? "Connected"
              : error
              ? "Error"
              : "Disconnected"}
          </Badge>
        </CardTitle>
      </CardHeader>
      <CardContent className="space-y-4">
        {/* Transport Controls */}
        <div className="flex items-center justify-center gap-2">
          <Button
            variant="outline"
            size="sm"
            onClick={onGoToStart}
            disabled={loading}
          >
            <SkipBack className="h-4 w-4" />
          </Button>
          <Button
            variant="outline"
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
            variant="outline"
            size="sm"
            onClick={onStop}
            disabled={loading}
          >
            <Square className="h-4 w-4" />
          </Button>
          <Button variant="outline" size="sm" disabled>
            <SkipForward className="h-4 w-4" />
          </Button>
          {transport?.play_state === 'Recording' && (
            <Button variant="destructive" size="sm">
              <Circle className="h-4 w-4 fill-current" />
            </Button>
          )}
        </div>

        {/* Transport Information */}
        {transport && (
          <div className="grid grid-cols-2 gap-4 text-sm">
            <div>
              <span className="text-muted-foreground">Position:</span>
              <div className="font-mono text-lg">
                {formatTime(transport.playhead_position?.time?.seconds || 0)}
              </div>
            </div>
            <div>
              <span className="text-muted-foreground">Tempo:</span>
              <div className="font-mono text-lg">
                {(transport.tempo?.bpm || 120).toFixed(1)} BPM
              </div>
            </div>
            <div>
              <span className="text-muted-foreground">Time Signature:</span>
              <div className="font-mono">
                {transport.time_signature?.numerator || 4}/
                {transport.time_signature?.denominator || 4}
              </div>
            </div>
            <div>
              <span className="text-muted-foreground">Status:</span>
              <div className="flex items-center gap-2">
                {transport.play_state === 'Playing' && (
                  <Badge variant="default" className="text-xs">
                    Playing
                  </Badge>
                )}
                {transport.play_state === 'Paused' && (
                  <Badge variant="secondary" className="text-xs">
                    Paused
                  </Badge>
                )}
                {transport.play_state === 'Recording' && (
                  <Badge variant="destructive" className="text-xs">
                    Recording
                  </Badge>
                )}
                {transport.looping && (
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
            <p>Not connected to app state</p>
            <p className="text-xs">
              {error ? `Error: ${error}` : "Loading app state..."}
            </p>
          </div>
        )}
      </CardContent>
    </Card>
  );
};
