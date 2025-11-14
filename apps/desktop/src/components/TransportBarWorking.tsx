import React, { useState, useEffect } from 'react';
import { invoke } from '@tauri-apps/api/core';
import { Play, Pause, Square, SkipBack, SkipForward } from 'lucide-react';

interface Transport {
  play_state: 'Playing' | 'Paused' | 'Stopped' | 'Recording';
  playhead_position: {
    time: { seconds: number };
    musical: { measure: number; beat: number; subdivision: number };
  };
  tempo: { bpm: number };
  time_signature: { numerator: number; denominator: number };
  looping: boolean;
  record_mode: string;
}

interface AppStateSnapshot {
  active_project: string | null;
  projects: Record<string, {
    name: string;
    transport: Transport;
    is_active: boolean;
  }>;
}

interface TransportBarWorkingProps {
  className?: string;
}

export const TransportBarWorking: React.FC<TransportBarWorkingProps> = ({
  className = "",
}) => {
  const [appState, setAppState] = useState<AppStateSnapshot | null>(null);
  const [isExpanded, setIsExpanded] = useState(false);
  const [isVisible, setIsVisible] = useState(true);
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [connected, setConnected] = useState(false);

  // Load app state and set up polling
  useEffect(() => {
    loadAppState();
    const interval = setInterval(loadAppState, 250); // Poll every 250ms for smooth updates
    return () => clearInterval(interval);
  }, []);

  const loadAppState = async () => {
    try {
      const state = await invoke<AppStateSnapshot>('get_app_state');
      setAppState(state);
      setConnected(true);
      setError(null);
    } catch (err) {
      console.error('Failed to load app state:', err);
      setError(String(err));
      setConnected(false);
    }
  };

  const handleTransportCommand = async (command: string, params?: Record<string, any>) => {
    if (!appState?.active_project) {
      setError('No active project');
      return;
    }

    setIsLoading(true);
    try {
      console.log(`Executing transport command: ${command}`, params);

      if (params) {
        await invoke(command, params);
      } else {
        await invoke(command);
      }

      setError(null);
      console.log(`Transport command ${command} executed successfully`);

      // Refresh state immediately after command
      setTimeout(loadAppState, 50);

    } catch (err) {
      console.error(`Transport command ${command} failed:`, err);
      setError(String(err));
    } finally {
      setIsLoading(false);
    }
  };

  const handlePlayPause = () => {
    handleTransportCommand('transport_play_pause');
  };

  const handleStop = () => {
    handleTransportCommand('transport_stop');
  };

  const handleRewind = () => {
    handleTransportCommand('transport_set_position', { seconds: 0 });
  };

  const handleFastForward = () => {
    // Skip forward 10 seconds
    const currentTime = activeProject?.transport.playhead_position.time.seconds || 0;
    handleTransportCommand('transport_set_position', { seconds: currentTime + 10 });
  };

  const activeProject = appState?.active_project
    ? appState.projects[appState.active_project]
    : null;

  const transportState = activeProject?.transport;

  if (!isVisible) {
    return (
      <div className="fixed bottom-4 right-4">
        <button
          onClick={() => setIsVisible(true)}
          className="px-3 py-2 bg-gray-100 hover:bg-gray-200 border rounded-md shadow-sm text-sm opacity-50 hover:opacity-100 transition-opacity"
        >
          Show Transport
        </button>
      </div>
    );
  }

  const getConnectionStatus = () => {
    if (isLoading) return "Loading...";
    if (error) return "Error";
    if (connected && appState?.active_project) return "Connected";
    return "Disconnected";
  };

  const getConnectionColor = () => {
    if (isLoading) return "bg-yellow-500";
    if (error) return "bg-red-500";
    if (connected && appState?.active_project) return "bg-green-500";
    return "bg-gray-500";
  };

  const formatTime = (seconds: number) => {
    const minutes = Math.floor(seconds / 60);
    const secs = Math.floor(seconds % 60);
    return `${minutes}:${secs.toString().padStart(2, '0')}`;
  };

  return (
    <div
      className={`fixed bottom-4 right-4 bg-white border rounded-lg shadow-lg transition-all duration-200 ${
        isExpanded ? "p-4" : "p-3"
      } ${className}`}
    >
      <div className="flex items-center gap-3">
        {/* Connection Status */}
        <div className="flex items-center gap-2">
          <div
            className={`w-2 h-2 rounded-full ${getConnectionColor()}`}
            title={getConnectionStatus()}
          />
          <span className="text-xs text-gray-600">
            {getConnectionStatus()}
          </span>
        </div>

        {/* Error Display */}
        {error && (
          <div className="px-2 py-1 bg-red-100 text-red-700 rounded text-xs max-w-32 truncate">
            {error}
          </div>
        )}

        {/* Transport Controls */}
        <div className="flex items-center gap-1">
          <button
            onClick={handleRewind}
            disabled={isLoading || !connected}
            className="p-1.5 hover:bg-gray-100 rounded disabled:opacity-50 disabled:cursor-not-allowed transition-colors"
            title="Rewind to Start"
          >
            <SkipBack className="h-4 w-4" />
          </button>

          <button
            onClick={handlePlayPause}
            disabled={isLoading || !connected}
            className="p-2 hover:bg-gray-100 rounded disabled:opacity-50 disabled:cursor-not-allowed transition-colors"
            title={transportState?.play_state === 'Playing' ? 'Pause' : 'Play'}
          >
            {transportState?.play_state === 'Playing' ? (
              <Pause className="h-5 w-5" />
            ) : (
              <Play className="h-5 w-5" />
            )}
          </button>

          <button
            onClick={handleStop}
            disabled={isLoading || !connected}
            className="p-1.5 hover:bg-gray-100 rounded disabled:opacity-50 disabled:cursor-not-allowed transition-colors"
            title="Stop"
          >
            <Square className="h-4 w-4" />
          </button>

          <button
            onClick={handleFastForward}
            disabled={isLoading || !connected}
            className="p-1.5 hover:bg-gray-100 rounded disabled:opacity-50 disabled:cursor-not-allowed transition-colors"
            title="Fast Forward"
          >
            <SkipForward className="h-4 w-4" />
          </button>
        </div>

        {/* State Display */}
        {transportState && (
          <div className="flex items-center gap-2 text-xs text-gray-600">
            <span className={`font-medium ${
              transportState.play_state === 'Playing' ? 'text-green-600' :
              transportState.play_state === 'Recording' ? 'text-red-600' :
              'text-gray-600'
            }`}>
              {transportState.play_state}
            </span>
            <span className="font-mono">
              {formatTime(transportState.playhead_position.time.seconds)}
            </span>
            <span>
              {transportState.tempo.bpm.toFixed(0)} BPM
            </span>
          </div>
        )}

        {/* Expand/Collapse Button */}
        <button
          onClick={() => setIsExpanded(!isExpanded)}
          className="p-1 hover:bg-gray-100 rounded transition-colors"
          title={isExpanded ? "Collapse" : "Expand"}
        >
          <span className="text-sm font-bold">
            {isExpanded ? "−" : "+"}
          </span>
        </button>

        {/* Hide Button */}
        <button
          onClick={() => setIsVisible(false)}
          className="p-1 hover:bg-gray-100 rounded transition-colors text-gray-400 hover:text-gray-600"
          title="Hide Transport"
        >
          <span className="text-sm font-bold">×</span>
        </button>
      </div>

      {/* Expanded Content */}
      {isExpanded && transportState && (
        <div className="mt-4 pt-4 border-t space-y-3">
          {/* Detailed Transport Info */}
          <div className="grid grid-cols-2 gap-3 text-sm">
            <div className="space-y-1">
              <div>
                <span className="text-gray-500">Position:</span>
                <div className="font-mono font-medium">
                  {formatTime(transportState.playhead_position.time.seconds)}
                </div>
              </div>
              <div>
                <span className="text-gray-500">Measure:</span>
                <div className="font-mono font-medium">
                  {transportState.playhead_position.musical.measure}.
                  {transportState.playhead_position.musical.beat}.
                  {transportState.playhead_position.musical.subdivision}
                </div>
              </div>
            </div>
            <div className="space-y-1">
              <div>
                <span className="text-gray-500">Tempo:</span>
                <div className="font-mono font-medium">
                  {transportState.tempo.bpm.toFixed(1)} BPM
                </div>
              </div>
              <div>
                <span className="text-gray-500">Time Sig:</span>
                <div className="font-mono font-medium">
                  {transportState.time_signature.numerator}/{transportState.time_signature.denominator}
                </div>
              </div>
            </div>
          </div>

          {/* Status Indicators */}
          <div className="flex items-center gap-4 text-sm">
            <div className={`flex items-center gap-1 ${
              transportState.play_state === 'Playing' ? 'text-green-600' : 'text-gray-500'
            }`}>
              <div className={`w-2 h-2 rounded-full ${
                transportState.play_state === 'Playing' ? 'bg-green-500' : 'bg-gray-400'
              }`} />
              <span>Playing</span>
            </div>
            <div className={`flex items-center gap-1 ${
              transportState.play_state === 'Recording' ? 'text-red-600' : 'text-gray-500'
            }`}>
              <div className={`w-2 h-2 rounded-full ${
                transportState.play_state === 'Recording' ? 'bg-red-500' : 'bg-gray-400'
              }`} />
              <span>Recording</span>
            </div>
            <div className={`flex items-center gap-1 ${
              transportState.looping ? 'text-blue-600' : 'text-gray-500'
            }`}>
              <div className={`w-2 h-2 rounded-full ${
                transportState.looping ? 'bg-blue-500' : 'bg-gray-400'
              }`} />
              <span>Loop</span>
            </div>
          </div>

          {/* Quick Actions */}
          <div className="flex gap-2">
            <button
              onClick={() => handleTransportCommand('transport_set_tempo', { bpm: 120 })}
              disabled={isLoading}
              className="px-2 py-1 text-xs bg-gray-100 hover:bg-gray-200 rounded disabled:opacity-50"
            >
              120 BPM
            </button>
            <button
              onClick={() => handleTransportCommand('transport_set_tempo', { bpm: 140 })}
              disabled={isLoading}
              className="px-2 py-1 text-xs bg-gray-100 hover:bg-gray-200 rounded disabled:opacity-50"
            >
              140 BPM
            </button>
            <button
              onClick={() => handleTransportCommand('transport_set_time_signature', { numerator: 4, denominator: 4 })}
              disabled={isLoading}
              className="px-2 py-1 text-xs bg-gray-100 hover:bg-gray-200 rounded disabled:opacity-50"
            >
              4/4
            </button>
            <button
              onClick={loadAppState}
              disabled={isLoading}
              className="px-2 py-1 text-xs bg-blue-100 hover:bg-blue-200 rounded disabled:opacity-50"
            >
              Refresh
            </button>
          </div>

          {/* Project Info */}
          {activeProject && (
            <div className="pt-2 border-t">
              <div className="text-xs text-gray-500">
                Project: <span className="font-medium text-gray-700">{activeProject.name}</span>
              </div>
            </div>
          )}
        </div>
      )}
    </div>
  );
};

export default TransportBarWorking;
