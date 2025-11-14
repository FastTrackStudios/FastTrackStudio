import React from "react";
import { Music, Hash, Clock, Play, SkipForward } from "lucide-react";
import { Badge } from "@/components/ui/badge";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { Progress } from "@/components/ui/progress";
import { useWebSocket } from "../contexts/WebSocketContext";

interface SetlistSongViewProps {
  className?: string;
}

export const SetlistSongView: React.FC<SetlistSongViewProps> = ({
  className = "",
}) => {
  const { setlistState, transportState, connected, connectionStatus } =
    useWebSocket();

  const formatTime = (seconds: number): string => {
    const minutes = Math.floor(seconds / 60);
    const secs = Math.floor(seconds % 60);
    return `${minutes}:${secs.toString().padStart(2, "0")}`;
  };

  const calculateSongProgress = (): number => {
    if (!transportState || !setlistState?.current_song) return 0;

    const currentPosition = transportState.position_seconds;
    const songStart = setlistState.current_song.start_position_seconds;
    const songEnd = setlistState.current_song.end_position_seconds;

    if (currentPosition < songStart) return 0;
    if (currentPosition > songEnd) return 100;

    const progress =
      ((currentPosition - songStart) / (songEnd - songStart)) * 100;
    return Math.max(0, Math.min(100, progress));
  };

  return (
    <Card className={className}>
      <CardHeader>
        <CardTitle className="flex items-center justify-between">
          <span>Song Overview</span>
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
        {setlistState && (
          <>
            {/* Current Song */}
            {setlistState.current_song && (
              <div className="space-y-3">
                <div className="flex items-center gap-2">
                  <Play className="h-5 w-5 text-primary" />
                  <div className="flex-1">
                    <h3 className="font-semibold text-lg">
                      {setlistState.current_song.name}
                    </h3>
                    <div className="flex items-center gap-4 text-sm text-muted-foreground">
                      <span>Song #{setlistState.current_song.index + 1}</span>
                      <span className="flex items-center gap-1">
                        <Clock className="h-3 w-3" />
                        {formatTime(setlistState.current_song.length_time)}
                      </span>
                      <span className="flex items-center gap-1">
                        <Hash className="h-3 w-3" />
                        {setlistState.current_song.sections.length} sections
                      </span>
                    </div>
                  </div>
                </div>

                {/* Song Progress */}
                <div className="space-y-2">
                  <div className="flex justify-between text-sm">
                    <span>Song Progress</span>
                    <span>{calculateSongProgress().toFixed(1)}%</span>
                  </div>
                  <Progress value={calculateSongProgress()} className="h-3" />
                  <div className="flex justify-between text-xs text-muted-foreground">
                    <span>0:00</span>
                    <span>
                      {formatTime(setlistState.current_song.length_time)}
                    </span>
                  </div>
                </div>

                {/* Current Section */}
                {setlistState.current_section && (
                  <div className="p-3 bg-primary/10 border border-primary/20 rounded-lg">
                    <div className="flex items-center gap-2 mb-1">
                      <Hash className="h-4 w-4 text-primary" />
                      <span className="font-medium">
                        Current: {setlistState.current_section.name}
                      </span>
                    </div>
                    <div className="text-sm text-muted-foreground">
                      Section {setlistState.current_section.index + 1} •{" "}
                      {formatTime(setlistState.current_section.length_time)}
                    </div>
                  </div>
                )}
              </div>
            )}

            {/* Navigation */}
            <div className="grid grid-cols-1 gap-3">
              {/* Previous Song */}
              {setlistState.previous_song && (
                <div className="p-2 bg-muted/30 rounded">
                  <div className="flex items-center gap-2 text-sm">
                    <Music className="h-3 w-3 text-muted-foreground" />
                    <span className="text-muted-foreground">Previous:</span>
                    <span className="font-medium">
                      {setlistState.previous_song.name}
                    </span>
                  </div>
                </div>
              )}

              {/* Next Song */}
              {setlistState.next_song && (
                <div className="p-2 bg-muted/50 rounded">
                  <div className="flex items-center gap-2 text-sm">
                    <SkipForward className="h-3 w-3 text-muted-foreground" />
                    <span className="text-muted-foreground">Next:</span>
                    <span className="font-medium">
                      {setlistState.next_song.name}
                    </span>
                    <Badge variant="outline" className="text-xs">
                      #{setlistState.next_song.index + 1}
                    </Badge>
                  </div>
                </div>
              )}
            </div>

            {/* Setlist Summary */}
            <div className="grid grid-cols-3 gap-4 pt-2 border-t">
              <div className="text-center">
                <div className="text-lg font-semibold">
                  {setlistState.songs.length}
                </div>
                <div className="text-xs text-muted-foreground">Total Songs</div>
              </div>
              <div className="text-center">
                <div className="text-lg font-semibold">
                  {formatTime(setlistState.length_time)}
                </div>
                <div className="text-xs text-muted-foreground">Total Time</div>
              </div>
              <div className="text-center">
                <div className="text-lg font-semibold">
                  {setlistState.current_song
                    ? setlistState.current_song.index + 1
                    : 0}
                </div>
                <div className="text-xs text-muted-foreground">
                  Current Song
                </div>
              </div>
            </div>

            {/* Quick Song List */}
            {setlistState.songs.length > 0 && (
              <div className="space-y-2">
                <h4 className="text-sm font-medium">Quick Navigation</h4>
                <div className="max-h-32 overflow-y-auto space-y-1">
                  {setlistState.songs.slice(0, 3).map((song) => (
                    <div
                      key={song.index}
                      className={`flex items-center gap-2 p-1 text-xs rounded ${
                        setlistState.current_song?.index === song.index
                          ? "bg-primary/10 text-primary font-medium"
                          : "text-muted-foreground"
                      }`}
                    >
                      <span className="w-6 text-center">#{song.index + 1}</span>
                      <span className="flex-1 truncate">{song.name}</span>
                      <span>{formatTime(song.length_time)}</span>
                    </div>
                  ))}
                  {setlistState.songs.length > 3 && (
                    <div className="text-xs text-muted-foreground text-center py-1">
                      +{setlistState.songs.length - 3} more songs
                    </div>
                  )}
                </div>
              </div>
            )}
          </>
        )}

        {!connected && (
          <div className="text-center text-muted-foreground">
            <p>Not connected to REAPER</p>
            <p className="text-xs">
              Make sure REAPER is running with the FTS Extensions plugin loaded.
            </p>
          </div>
        )}

        {connected && !setlistState && (
          <div className="text-center text-muted-foreground">
            <p>No setlist data available</p>
          </div>
        )}
      </CardContent>
    </Card>
  );
};
