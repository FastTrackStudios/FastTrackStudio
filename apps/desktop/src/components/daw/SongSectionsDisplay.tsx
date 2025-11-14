import React from "react";
import { Hash, Play, Clock } from "lucide-react";
import { Badge } from "@/components/ui/badge";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { Progress } from "@/components/ui/progress";
import { useWebSocket } from "../contexts/WebSocketContext";

interface SongSectionsDisplayProps {
  className?: string;
}

export const SongSectionsDisplay: React.FC<SongSectionsDisplayProps> = ({
  className = "",
}) => {
  const { setlistState, transportState, connected, connectionStatus } =
    useWebSocket();

  const formatTime = (seconds: number): string => {
    const minutes = Math.floor(seconds / 60);
    const secs = Math.floor(seconds % 60);
    return `${minutes}:${secs.toString().padStart(2, "0")}`;
  };

  const calculateProgress = (): number => {
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
          <span>Song Sections</span>
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
        {setlistState?.current_song && (
          <>
            {/* Current Song Info */}
            <div className="space-y-2">
              <div className="flex items-center gap-2">
                <Play className="h-4 w-4 text-primary" />
                <span className="font-medium">
                  {setlistState.current_song.name}
                </span>
                <Badge variant="outline" className="text-xs">
                  #{setlistState.current_song.index + 1}
                </Badge>
              </div>

              {/* Song Progress */}
              <div className="space-y-1">
                <div className="flex justify-between text-xs text-muted-foreground">
                  <span>Progress</span>
                  <span>{calculateProgress().toFixed(1)}%</span>
                </div>
                <Progress value={calculateProgress()} className="h-2" />
              </div>
            </div>

            {/* Current Section */}
            {setlistState.current_section && (
              <div className="space-y-2">
                <h4 className="text-sm font-medium">Current Section</h4>
                <div className="p-3 bg-primary/10 border border-primary/20 rounded-lg">
                  <div className="flex items-center gap-2 mb-2">
                    <Hash className="h-4 w-4 text-primary" />
                    <span className="font-medium">
                      {setlistState.current_section.name}
                    </span>
                    <Badge variant="outline" className="text-xs">
                      Section {setlistState.current_section.index + 1}
                    </Badge>
                  </div>
                  <div className="grid grid-cols-2 gap-2 text-xs text-muted-foreground">
                    <div className="flex items-center gap-1">
                      <Clock className="h-3 w-3" />
                      {formatTime(setlistState.current_section.length_time)}
                    </div>
                    <div>
                      {setlistState.current_section.length_measures.toFixed(1)}{" "}
                      measures
                    </div>
                  </div>
                </div>
              </div>
            )}

            {/* Sections List */}
            {setlistState.current_song.sections.length > 0 && (
              <div className="space-y-2">
                <h4 className="text-sm font-medium">
                  All Sections ({setlistState.current_song.sections.length})
                </h4>
                <div className="max-h-48 overflow-y-auto space-y-1">
                  {setlistState.current_song.sections.map((section) => (
                    <div
                      key={section.index}
                      className={`flex items-center gap-2 p-2 text-xs rounded ${
                        setlistState.current_section?.index === section.index
                          ? "bg-primary/10 border border-primary/20 text-primary"
                          : "bg-muted/30 text-muted-foreground"
                      }`}
                    >
                      <Hash className="h-3 w-3" />
                      <span className="flex-1 font-medium">{section.name}</span>
                      <span>{formatTime(section.length_time)}</span>
                      <span>{section.length_measures.toFixed(1)}m</span>
                    </div>
                  ))}
                </div>
              </div>
            )}

            {/* Next Section */}
            {setlistState.next_section && (
              <div className="space-y-2">
                <h4 className="text-sm font-medium">Next Section</h4>
                <div className="p-2 bg-muted/50 rounded">
                  <div className="flex items-center gap-2">
                    <Hash className="h-3 w-3 text-muted-foreground" />
                    <span className="text-sm">
                      {setlistState.next_section.name}
                    </span>
                    <Badge variant="outline" className="text-xs">
                      Section {setlistState.next_section.index + 1}
                    </Badge>
                  </div>
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

        {connected && !setlistState?.current_song && (
          <div className="text-center text-muted-foreground">
            <p>No song currently playing</p>
            <p className="text-xs">Start playing a song to see sections</p>
          </div>
        )}
      </CardContent>
    </Card>
  );
};
