import React, { useState } from 'react';
import { Bug } from 'lucide-react';
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
  DialogTrigger,
} from '@/components/ui/dialog';
import { Button } from '@/components/ui/button';
import { ScrollArea } from '@/components/ui/scroll-area';
import { Badge } from '@/components/ui/badge';
import { Setlist, Song } from '@/atoms/types';
import { cn } from '@/lib/utils';

interface SetlistDebugDialogProps {
  setlist: Setlist | null;
}

export function SetlistDebugDialog({ setlist }: SetlistDebugDialogProps) {
  const [open, setOpen] = useState(false);

  // Helper to get project name from song metadata
  const getProjectName = (song: Song): string => {
    return song.metadata?.project_name || song.metadata?.Project || song.metadata?.project || 'Unknown Project';
  };

  // Group songs by project - must be called before early return to follow Rules of Hooks
  const songsByProject = React.useMemo(() => {
    if (!setlist) {
      return new Map<string, Array<{ song: Song; index: number }>>();
    }
    
    const grouped = new Map<string, Array<{ song: Song; index: number }>>();
    
    setlist.songs.forEach((song, index) => {
      const projectName = getProjectName(song);
      if (!grouped.has(projectName)) {
        grouped.set(projectName, []);
      }
      grouped.get(projectName)!.push({ song, index });
    });
    
    return grouped;
  }, [setlist]);

  if (!setlist) {
    return null;
  }

  return (
    <Dialog open={open} onOpenChange={setOpen}>
      <DialogTrigger asChild>
        <Button variant="outline" size="sm" className="fixed bottom-4 left-4 z-50">
          <Bug className="h-4 w-4 mr-2" />
          Debug Setlist
        </Button>
      </DialogTrigger>
      <DialogContent className="max-w-4xl max-h-[80vh]">
        <DialogHeader>
          <DialogTitle>Setlist Debug Information</DialogTitle>
          <DialogDescription>
            Shows the order of songs and their sections as received from the backend
          </DialogDescription>
        </DialogHeader>
        
        <ScrollArea className="max-h-[60vh] pr-4">
          <div className="space-y-6">
            {/* Summary */}
            <div className="space-y-2">
              <h3 className="text-lg font-semibold">Summary</h3>
              <div className="grid grid-cols-3 gap-4">
                <div className="p-3 bg-muted rounded-lg">
                  <div className="text-sm text-muted-foreground">Total Songs</div>
                  <div className="text-2xl font-bold">{setlist.songs.length}</div>
                </div>
                <div className="p-3 bg-muted rounded-lg">
                  <div className="text-sm text-muted-foreground">Projects</div>
                  <div className="text-2xl font-bold">{songsByProject.size}</div>
                </div>
                <div className="p-3 bg-muted rounded-lg">
                  <div className="text-sm text-muted-foreground">Total Sections</div>
                  <div className="text-2xl font-bold">
                    {setlist.songs.reduce((sum, song) => sum + song.sections.length, 0)}
                  </div>
                </div>
              </div>
            </div>

            {/* Songs by Project */}
            <div className="space-y-4">
              <h3 className="text-lg font-semibold">Songs by Project</h3>
              {Array.from(songsByProject.entries()).map(([projectName, songs]) => (
                <div key={projectName} className="border rounded-lg p-4 space-y-3">
                  <div className="flex items-center justify-between">
                    <h4 className="font-semibold text-base">{projectName}</h4>
                    <Badge variant="secondary">{songs.length} song{songs.length !== 1 ? 's' : ''}</Badge>
                  </div>
                  
                  <div className="space-y-2 ml-4">
                    {songs.map(({ song, index }) => (
                      <div key={index} className="border-l-2 border-primary/30 pl-3 space-y-2">
                        <div className="flex items-center gap-2">
                          <Badge variant="outline" className="font-mono">
                            #{index}
                          </Badge>
                          <span className="font-medium">{song.name}</span>
                        </div>
                        
                        {/* Sections */}
                        {song.sections.length > 0 && (
                          <div className="ml-6 space-y-1">
                            <div className="text-xs text-muted-foreground mb-1">Sections ({song.sections.length}):</div>
                            {song.sections.map((section, sectionIdx) => {
                              const startSec = section.start_position?.time?.seconds ?? 0;
                              const endSec = section.end_position?.time?.seconds ?? 0;
                              const duration = endSec - startSec;
                              
                              return (
                                <div
                                  key={sectionIdx}
                                  className="text-xs flex items-center gap-2 py-1"
                                >
                                  <Badge variant="outline" className="text-xs font-mono w-8">
                                    {sectionIdx}
                                  </Badge>
                                  <span className="font-medium min-w-[120px]">
                                    {section.name || section.section_type}
                                  </span>
                                  <span className="text-muted-foreground font-mono text-[10px]">
                                    {startSec.toFixed(2)}s - {endSec.toFixed(2)}s
                                  </span>
                                  <span className="text-muted-foreground text-[10px]">
                                    ({duration.toFixed(2)}s)
                                  </span>
                                </div>
                              );
                            })}
                          </div>
                        )}
                        
                        {/* Markers */}
                        <div className="ml-6 space-y-1 text-xs text-muted-foreground">
                          {song.count_in_marker && (
                            <div>Count-In: {song.count_in_marker.position.time.seconds.toFixed(2)}s</div>
                          )}
                          {song.start_marker && (
                            <div>Start: {song.start_marker.position.time.seconds.toFixed(2)}s</div>
                          )}
                          {song.song_end_marker && (
                            <div>Song End: {song.song_end_marker.position.time.seconds.toFixed(2)}s</div>
                          )}
                          {song.end_marker && (
                            <div>End: {song.end_marker.position.time.seconds.toFixed(2)}s</div>
                          )}
                        </div>
                      </div>
                    ))}
                  </div>
                </div>
              ))}
            </div>

            {/* Full Order List */}
            <div className="space-y-2">
              <h3 className="text-lg font-semibold">Full Setlist Order</h3>
              <div className="space-y-1">
                {setlist.songs.map((song, index) => {
                  const projectName = getProjectName(song);
                  return (
                    <div
                      key={index}
                      className="flex items-center gap-2 p-2 rounded hover:bg-muted/50 text-sm"
                    >
                      <Badge variant="outline" className="font-mono w-10">
                        {index}
                      </Badge>
                      <span className="font-medium flex-1">{song.name}</span>
                      <Badge variant="secondary" className="text-xs">
                        {projectName}
                      </Badge>
                      <Badge variant="outline" className="text-xs">
                        {song.sections.length} sections
                      </Badge>
                    </div>
                  );
                })}
              </div>
            </div>
          </div>
        </ScrollArea>
      </DialogContent>
    </Dialog>
  );
}

