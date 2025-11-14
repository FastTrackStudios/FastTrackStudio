import React from "react";
import { MapPin, Square } from "lucide-react";
import { Badge } from "@/components/ui/badge";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { useWebSocket } from "../contexts/WebSocketContext";

interface MarkerRegionDisplayProps {
  className?: string;
}

export const MarkerRegionDisplay: React.FC<MarkerRegionDisplayProps> = ({
  className = "",
}) => {
  const { markerRegionState, connected, connectionStatus } = useWebSocket();

  const formatTime = (seconds: number): string => {
    const minutes = Math.floor(seconds / 60);
    const secs = Math.floor(seconds % 60);
    return `${minutes}:${secs.toString().padStart(2, "0")}`;
  };

  return (
    <Card className={className}>
      <CardHeader>
        <CardTitle className="flex items-center justify-between">
          <span>Markers & Regions</span>
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
        {markerRegionState && (
          <>
            {/* Current Marker/Region */}
            {(markerRegionState.current_marker ||
              markerRegionState.current_region) && (
              <div className="space-y-2">
                <h4 className="text-sm font-medium">Current</h4>
                {markerRegionState.current_marker && (
                  <div className="flex items-center gap-2 p-2 bg-muted rounded">
                    <MapPin className="h-4 w-4 text-blue-500" />
                    <div className="flex-1">
                      <div className="font-medium text-sm">
                        {markerRegionState.current_marker.name}
                      </div>
                      <div className="text-xs text-muted-foreground">
                        Marker at{" "}
                        {formatTime(
                          markerRegionState.current_marker.position_seconds
                        )}
                      </div>
                    </div>
                  </div>
                )}
                {markerRegionState.current_region && (
                  <div className="flex items-center gap-2 p-2 bg-muted rounded">
                    <Square className="h-4 w-4 text-green-500" />
                    <div className="flex-1">
                      <div className="font-medium text-sm">
                        {markerRegionState.current_region.name}
                      </div>
                      <div className="text-xs text-muted-foreground">
                        Region{" "}
                        {formatTime(
                          markerRegionState.current_region.position_seconds
                        )}{" "}
                        -{" "}
                        {formatTime(
                          markerRegionState.current_region.end_position_seconds
                        )}
                      </div>
                    </div>
                  </div>
                )}
              </div>
            )}

            {/* Markers List */}
            {markerRegionState.markers.length > 0 && (
              <div className="space-y-2">
                <h4 className="text-sm font-medium">
                  Markers ({markerRegionState.markers.length})
                </h4>
                <div className="max-h-32 overflow-y-auto space-y-1">
                  {markerRegionState.markers.slice(0, 5).map((marker) => (
                    <div
                      key={marker.id}
                      className="flex items-center gap-2 p-1 text-xs"
                    >
                      <MapPin className="h-3 w-3 text-blue-500" />
                      <span className="flex-1 truncate">{marker.name}</span>
                      <span className="text-muted-foreground">
                        {formatTime(marker.position_seconds)}
                      </span>
                    </div>
                  ))}
                  {markerRegionState.markers.length > 5 && (
                    <div className="text-xs text-muted-foreground text-center">
                      +{markerRegionState.markers.length - 5} more
                    </div>
                  )}
                </div>
              </div>
            )}

            {/* Regions List */}
            {markerRegionState.regions.length > 0 && (
              <div className="space-y-2">
                <h4 className="text-sm font-medium">
                  Regions ({markerRegionState.regions.length})
                </h4>
                <div className="max-h-32 overflow-y-auto space-y-1">
                  {markerRegionState.regions.slice(0, 5).map((region) => (
                    <div
                      key={region.id}
                      className="flex items-center gap-2 p-1 text-xs"
                    >
                      <Square className="h-3 w-3 text-green-500" />
                      <span className="flex-1 truncate">{region.name}</span>
                      <span className="text-muted-foreground">
                        {formatTime(region.position_seconds)}
                      </span>
                    </div>
                  ))}
                  {markerRegionState.regions.length > 5 && (
                    <div className="text-xs text-muted-foreground text-center">
                      +{markerRegionState.regions.length - 5} more
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

        {connected && !markerRegionState && (
          <div className="text-center text-muted-foreground">
            <p>No marker/region data available</p>
          </div>
        )}
      </CardContent>
    </Card>
  );
};
