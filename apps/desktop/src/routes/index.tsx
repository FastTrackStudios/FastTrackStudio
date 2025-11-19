import { createFileRoute } from '@tanstack/react-router'
import { Badge } from '@/components/ui/badge';
import { useWebSocketAtoms } from '@/hooks/use-websocket-atoms';
import { useAtomValue } from '@effect-atom/atom-react';
import { currentSongIndexAtom, currentSectionIndexAtom } from '@/atoms/derived';
import { Alert, AlertDescription, AlertTitle } from '@/components/ui/alert';
import { CheckCircle2, XCircle, Loader2, AlertCircle } from 'lucide-react';
import { ResizablePanelGroup, ResizablePanel, ResizableHandle } from '@/components/ui/resizable';
import { Navigator } from '@/components/Navigator';
import { ProgressBar } from '@/components/ProgressBar';
import { TransportControls } from '@/components/TransportControls';
import { SetlistDebugDialog } from '@/components/SetlistDebugDialog';

export const Route = createFileRoute('/')({
  component: HomePage,
})

function HomePage() {
  // Use Effect Atom-based WebSocket hook
  const { connectionState, setlist, transportStates, switchToProject, seekToSection } = useWebSocketAtoms();
  
  // Get derived atoms for current song and section
  const currentSongIndex = useAtomValue(currentSongIndexAtom);
  const currentSectionIndex = useAtomValue(currentSectionIndexAtom);

  const getConnectionStatusBadge = () => {
    switch (connectionState) {
      case 'connected':
        return (
          <Badge variant="default" className="bg-green-500">
            <CheckCircle2 className="w-3 h-3 mr-1" />
            Connected
          </Badge>
        );
      case 'connecting':
        return (
          <Badge variant="secondary">
            <Loader2 className="w-3 h-3 mr-1 animate-spin" />
            Connecting...
          </Badge>
        );
      case 'disconnected':
        return (
          <Badge variant="destructive">
            <XCircle className="w-3 h-3 mr-1" />
            Disconnected
          </Badge>
        );
      case 'error':
        return (
          <Badge variant="destructive">
            <AlertCircle className="w-3 h-3 mr-1" />
            Error
          </Badge>
        );
    }
  };

  // Show connection status if not connected
  if (connectionState !== 'connected') {
  return (
      <div className="h-screen flex items-center justify-center p-8">
        <div className="max-w-2xl w-full space-y-4">
          <div className="text-center space-y-2">
          <h1 className="text-4xl font-bold tracking-tight">
            FastTrackStudio
          </h1>
          <p className="text-xl text-muted-foreground">
            Web UI - Network Broadcast
          </p>
        </div>
              <Alert>
                <AlertCircle className="h-4 w-4" />
            <AlertTitle>
              {connectionState === 'connecting' ? 'Connecting...' : 'Not Connected'}
            </AlertTitle>
                <AlertDescription>
              {connectionState === 'connecting' ? (
                'Attempting to connect to REAPER extension...'
              ) : (
                'Waiting for REAPER extension to connect. Make sure the REAPER extension is loaded and the desktop app is running.'
              )}
                </AlertDescription>
              </Alert>
          <div className="flex justify-center">
            {getConnectionStatusBadge()}
                </div>
                </div>
              </div>
    );
  }

  // Main layout with Resizable panels
                    return (
    <div className="h-screen w-screen overflow-hidden flex flex-col">
      {/* Top bar with connection status */}
      <div className="h-12 flex-shrink-0 border-b bg-background flex items-center justify-between px-4">
        <h1 className="text-lg font-semibold">FastTrackStudio</h1>
        {getConnectionStatusBadge()}
      </div>

      {/* Resizable panels */}
      <ResizablePanelGroup direction="horizontal" className="flex-1 min-h-0">
        {/* Left panel: Navigator (1/3 width) */}
        <ResizablePanel defaultSize={33} minSize={20} maxSize={50}>
          <Navigator
            songs={setlist?.songs ?? []}
            transportStates={transportStates}
            selectedSongIndex={currentSongIndex}
            selectedSectionIndex={currentSectionIndex}
            onSongClick={(_songIndex, projectName) => {
              // Switch to the project that contains this song
              switchToProject(projectName);
            }}
            onSectionClick={(_songIndex, _sectionIndex, projectName, songName, sectionName) => {
              // Switch to project and seek to section start
              switchToProject(projectName);
              seekToSection(projectName, songName, sectionName);
            }}
          />
        </ResizablePanel>

        <ResizableHandle withHandle />

        {/* Right panel: Progress Bar and Transport Controls */}
        <ResizablePanel defaultSize={67} minSize={30}>
          <div className="h-full flex flex-col overflow-hidden">
            {/* Progress Bar */}
            <div className="flex-1 min-h-0 overflow-y-auto">
              <ProgressBar
                song={currentSongIndex !== undefined && setlist?.songs[currentSongIndex] ? setlist.songs[currentSongIndex] : null}
                songIndex={currentSongIndex ?? 0}
                transportStates={transportStates}
                onSectionClick={(projectName, songName, sectionName) => {
                  // Switch to project and seek to section start
                  switchToProject(projectName);
                  seekToSection(projectName, songName, sectionName);
                }}
              />
            </div>

            {/* Transport Controls at bottom - fixed height, always visible */}
            <div className="flex-shrink-0 border-t bg-background" style={{ padding: '16px', minHeight: '112px' }}>
              <TransportControls
                song={currentSongIndex !== undefined && setlist?.songs[currentSongIndex] ? setlist.songs[currentSongIndex] : null}
                songIndex={currentSongIndex ?? 0}
                currentSectionIndex={currentSectionIndex ?? null}
                isPlaying={
                  currentSongIndex !== undefined && setlist?.songs[currentSongIndex] ? (
                    (() => {
                      const song = setlist.songs[currentSongIndex];
                      const projectName = song.metadata?.project_name || song.metadata?.Project || song.metadata?.project;
                      if (projectName) {
                        const transport = transportStates.get(projectName);
                        return transport?.playing ?? false;
                      }
                      return false;
                    })()
                  ) : false
                }
                isLooping={false} // TODO: Get loop state from transport
                onBack={() => {
                  // Go to start of song (first section)
                  const song = currentSongIndex !== undefined && setlist?.songs[currentSongIndex] ? setlist.songs[currentSongIndex] : null;
                  if (song && song.sections.length > 0) {
                    const projectName = song.metadata?.project_name || song.metadata?.Project || song.metadata?.project;
                    const firstSection = song.sections[0];
                    const sectionName = firstSection.name || firstSection.section_type;
                    if (projectName) {
                      switchToProject(projectName);
                      seekToSection(projectName, song.name, sectionName);
                    }
                  }
                }}
                onPlayPause={() => {
                  // TODO: Implement play/pause command
                  console.warn('Play/Pause not implemented yet');
                }}
                onLoop={() => {
                  // TODO: Implement loop toggle command
                  console.warn('Loop toggle not implemented yet');
                }}
                onAdvance={() => {
                  // Go to next section
                  const song = currentSongIndex !== undefined && setlist?.songs[currentSongIndex] ? setlist.songs[currentSongIndex] : null;
                  if (song && currentSectionIndex !== null && currentSectionIndex < song.sections.length - 1) {
                    const projectName = song.metadata?.project_name || song.metadata?.Project || song.metadata?.project;
                    const nextSection = song.sections[currentSectionIndex + 1];
                    const sectionName = nextSection.name || nextSection.section_type;
                    if (projectName) {
                      switchToProject(projectName);
                      seekToSection(projectName, song.name, sectionName);
                    }
                  } else if (song && currentSectionIndex === null && song.sections.length > 0) {
                    // If no section selected, go to first section
                    const projectName = song.metadata?.project_name || song.metadata?.Project || song.metadata?.project;
                    const firstSection = song.sections[0];
                    const sectionName = firstSection.name || firstSection.section_type;
                    if (projectName) {
                      switchToProject(projectName);
                      seekToSection(projectName, song.name, sectionName);
                    }
                  }
                }}
              />
            </div>
          </div>
        </ResizablePanel>
      </ResizablePanelGroup>
      
      {/* Debug Dialog */}
      <SetlistDebugDialog setlist={setlist} />
    </div>
  )
}
