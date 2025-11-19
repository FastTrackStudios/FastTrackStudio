import React from 'react';
import { Song, TransportState } from '@/atoms/types';

interface StatusCardsProps {
  song: Song | null;
  transport: TransportState | null;
}

export const StatusCards: React.FC<StatusCardsProps> = ({ song, transport }) => {
  if (!song || !transport) return null;

  // Get project information from metadata
  const projectName = song.metadata?.project_name || song.metadata?.Project || song.metadata?.project || null;
  const projectPath = song.metadata?.project_path || song.metadata?.ProjectPath || null;
  const artist = song.metadata?.artist || song.metadata?.Artist || null;
  const key = song.metadata?.key || song.metadata?.Key || null;

  // Calculate musical position
  const beatsPerMeasure = 4; // Default 4/4 time
  const beatsPerSecond = transport.tempo / 60;
  
  // Get song boundaries for display position
  const sortedSections = [...song.sections].sort(
    (a, b) => a.start_position.time.seconds - b.start_position.time.seconds
  );
  const songStart = song.song_region_start_marker?.position.time.seconds ?? 
                    song.start_marker?.position.time.seconds ?? 
                    (sortedSections.length > 0 ? sortedSections[0].start_position.time.seconds : 0);
  const currentPos = transport.position;
  const displayPosition = Math.max(0, currentPos - songStart);
  
  const totalBeats = displayPosition * beatsPerSecond;
  const measure = Math.floor(totalBeats / beatsPerMeasure) + 1;
  const beat = Math.floor(totalBeats % beatsPerMeasure) + 1;

  return (
    <div className="flex flex-row gap-2 flex-wrap">
      {/* Project Name */}
      {projectName && (
        <div className="bg-muted/50 rounded-md px-3 py-2 border text-xs">
          <div className="text-[10px] text-muted-foreground mb-0.5">Project</div>
          <div className="text-sm font-semibold truncate max-w-[200px]">{projectName}</div>
        </div>
      )}

      {/* Tempo */}
      <div className="bg-muted/50 rounded-md px-3 py-2 border text-xs">
        <div className="text-[10px] text-muted-foreground mb-0.5">Tempo</div>
        <div className="text-sm font-mono font-semibold">{transport.tempo.toFixed(1)} BPM</div>
      </div>

      {/* Musical Position */}
      <div className="bg-muted/50 rounded-md px-3 py-2 border text-xs">
        <div className="text-[10px] text-muted-foreground mb-0.5">Position</div>
        <div className="text-sm font-mono font-semibold">
          {String(measure).padStart(3, '0')}.{String(beat).padStart(2, '0')}
        </div>
      </div>

      {/* Time Position */}
      <div className="bg-muted/50 rounded-md px-3 py-2 border text-xs">
        <div className="text-[10px] text-muted-foreground mb-0.5">Time</div>
        <div className="text-sm font-mono font-semibold">{displayPosition.toFixed(2)}s</div>
      </div>

      {/* Key */}
      {key && (
        <div className="bg-muted/50 rounded-md px-3 py-2 border text-xs">
          <div className="text-[10px] text-muted-foreground mb-0.5">Key</div>
          <div className="text-sm font-semibold">{key}</div>
        </div>
      )}

      {/* Status */}
      <div className="bg-muted/50 rounded-md px-3 py-2 border text-xs">
        <div className="text-[10px] text-muted-foreground mb-0.5">Status</div>
        <div className="text-sm font-semibold">
          {transport.playing ? (
            <span className="text-green-500">Playing</span>
          ) : (
            <span className="text-muted-foreground">Paused</span>
          )}
        </div>
      </div>
    </div>
  );
};

