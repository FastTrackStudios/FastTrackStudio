import React from "react";
import { Music, Clock, Hash } from "lucide-react";
import { Badge } from "@/components/ui/badge";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import type { SetlistState } from "../../types/placeholders";

interface SetlistDisplayProps {
  className?: string;
  setlistState: SetlistState | null;
  connected: boolean;
  loading: boolean;
  error: string | null;
}

export const SetlistDisplay: React.FC<SetlistDisplayProps> = ({
  className = "",
  setlistState,
  connected,
  loading,
  error,
}) => {

  const formatTime = (seconds: number): string => {
    const minutes = Math.floor(seconds / 60);
    const secs = Math.floor(seconds % 60);
    return `${minutes}:${secs.toString().padStart(2, "0")}`;
  };

  return (
    <Card className={className}>
      <CardHeader>
        <CardTitle className="flex items-center justify-between">
          <span>Setlist</span>
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
        {setlistState && (
          <>
            {/* Current Song */}
            {setlistState.current_song && (
              <div className="space-y-2">
                <h4 className="text-sm font-medium">Current Song</h4>
                <div className="p-3 bg-primary/10 border border-primary/20 rounded-lg">
                  <div className="flex items-center gap-2 mb-2">
                    <Music className="h-4 w-4 text-primary" />
                    <span className="font-medium">
                      {setlistState.current_song.name}
                    </span>
                    <Badge variant="outline" className="text-xs">
                      #{setlistState.current_song.index + 1}
                    </Badge>
                  </div>
                  <div className="grid grid-cols-2 gap-2 text-xs text-muted-foreground">
                    <div className="flex items-center gap-1">
                      <Clock className="h-3 w-3" />
                      {formatTime(setlistState.current_song.length_time)}
                    </div>
                    <div className="flex items-center gap-1">
                      <Hash className="h-3 w-3" />
                      {setlistState.current_song.sections.length} sections
                    </div>
                  </div>
                </div>
              </div>
            )}

            {/* Current Section */}
            {setlistState.current_section && (
              <div className="space-y-2">
                <h4 className="text-sm font-medium">Current Section</h4>
                <div className="p-2 bg-muted rounded">
                  <div className="font-medium text-sm">
                    {setlistState.current_section.name}
                  </div>
                  <div className="text-xs text-muted-foreground">
                    Section {setlistState.current_section.index + 1} •{" "}
                    {formatTime(setlistState.current_section.length_time)}
                  </div>
                </div>
              </div>
            )}

            {/* Next Song */}
            {setlistState.next_song && (
              <div className="space-y-2">
                <h4 className="text-sm font-medium">Next Song</h4>
                <div className="p-2 bg-muted/50 rounded">
                  <div className="flex items-center gap-2">
                    <Music className="h-3 w-3 text-muted-foreground" />
                    <span className="text-sm">
                      {setlistState.next_song.name}
                    </span>
                    <Badge variant="outline" className="text-xs">
                      #{setlistState.next_song.index + 1}
                    </Badge>
                  </div>
                </div>
              </div>
            )}

            {/* Setlist Summary */}
            <div className="space-y-2">
              <h4 className="text-sm font-medium">Setlist Summary</h4>
              <div className="grid grid-cols-2 gap-2 text-sm">
                <div>
                  <span className="text-muted-foreground">Total Songs:</span>
                  <div className="font-mono">{setlistState.songs.length}</div>
                </div>
                <div>
                  <span className="text-muted-foreground">Total Duration:</span>
                  <div className="font-mono">
                    {formatTime(setlistState.length_time)}
                  </div>
                </div>
              </div>
            </div>

            {/* Song List Preview */}
            {setlistState.songs.length > 0 && (
              <div className="space-y-2">
                <h4 className="text-sm font-medium">Songs</h4>
                <div className="max-h-32 overflow-y-auto space-y-1">
                  {setlistState.songs.slice(0, 5).map((song) => (
                    <div
                      key={song.index}
                      className={`flex items-center gap-2 p-1 text-xs rounded ${
                        setlistState.current_song?.index === song.index
                          ? "bg-primary/10 text-primary"
                          : "text-muted-foreground"
                      }`}
                    >
                      <span className="w-6 text-center">#{song.index + 1}</span>
                      <span className="flex-1 truncate">{song.name}</span>
                      <span>{formatTime(song.length_time)}</span>
                    </div>
                  ))}
                  {setlistState.songs.length > 5 && (
                    <div className="text-xs text-muted-foreground text-center">
                      +{setlistState.songs.length - 5} more songs
                    </div>
                  )}
                </div>
              </div>
            )}
          </>
        )}

        {!connected && (
          <div className="text-center text-muted-foreground">
            <p>Not connected to app state</p>
            <p className="text-xs">
              {error ? `Error: ${error}` : "Loading app state..."}
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
