import React, { useState, useEffect } from 'react';
import { invoke } from '@tauri-apps/api/core';

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

export const TransportTest: React.FC = () => {
  const [appState, setAppState] = useState<AppStateSnapshot | null>(null);
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [lastCommand, setLastCommand] = useState<string>('');

  useEffect(() => {
    loadAppState();
    const interval = setInterval(loadAppState, 500); // Poll every 500ms
    return () => clearInterval(interval);
  }, []);

  const loadAppState = async () => {
    try {
      const state = await invoke<AppStateSnapshot>('get_app_state');
      setAppState(state);
      setError(null);
    } catch (err) {
      console.error('Failed to load app state:', err);
      setError(String(err));
    }
  };

  const executeCommand = async (command: string, params?: Record<string, any>) => {
    setIsLoading(true);
    setLastCommand(command);

    try {
      console.log(`Executing command: ${command}`, params);

      if (params) {
        await invoke(command, params);
      } else {
        await invoke(command);
      }

      setError(null);
      console.log(`Command ${command} executed successfully`);

      // Refresh state after command
      setTimeout(loadAppState, 100);

    } catch (err) {
      console.error(`Command ${command} failed:`, err);
      setError(String(err));
    } finally {
      setIsLoading(false);
    }
  };

  const activeProject = appState?.active_project
    ? appState.projects[appState.active_project]
    : null;

  const transport = activeProject?.transport;

  return (
    <div className="p-6 max-w-2xl mx-auto bg-white border rounded-lg shadow-lg">
      <h2 className="text-xl font-bold mb-4">Transport Control Test</h2>

      {/* Status */}
      <div className="mb-4 p-3 bg-gray-50 rounded">
        <div className="grid grid-cols-2 gap-2 text-sm">
          <div>
            <strong>Active Project:</strong> {appState?.active_project || 'None'}
          </div>
          <div>
            <strong>Connection:</strong> {appState ? '✅ Connected' : '❌ Disconnected'}
          </div>
          <div>
            <strong>Last Command:</strong> {lastCommand || 'None'}
          </div>
          <div>
            <strong>Loading:</strong> {isLoading ? '🔄 Yes' : '✅ No'}
          </div>
        </div>
        {error && (
          <div className="mt-2 p-2 bg-red-100 text-red-700 rounded text-sm">
            <strong>Error:</strong> {error}
          </div>
        )}
      </div>

      {/* Transport State */}
      {transport && (
        <div className="mb-4 p-3 bg-blue-50 rounded">
          <h3 className="font-semibold mb-2">Transport State</h3>
          <div className="grid grid-cols-2 gap-2 text-sm">
            <div>
              <strong>State:</strong> {transport.play_state}
            </div>
            <div>
              <strong>Position:</strong> {transport.playhead_position.time.seconds.toFixed(2)}s
            </div>
            <div>
              <strong>Tempo:</strong> {transport.tempo.bpm} BPM
            </div>
            <div>
              <strong>Time Sig:</strong> {transport.time_signature.numerator}/{transport.time_signature.denominator}
            </div>
            <div>
              <strong>Looping:</strong> {transport.looping ? 'Yes' : 'No'}
            </div>
            <div>
              <strong>Record Mode:</strong> {transport.record_mode}
            </div>
          </div>
        </div>
      )}

      {/* Transport Controls */}
      <div className="mb-4">
        <h3 className="font-semibold mb-2">Basic Controls</h3>
        <div className="flex flex-wrap gap-2">
          <button
            onClick={() => executeCommand('transport_play')}
            disabled={isLoading}
            className="px-3 py-1 bg-green-500 text-white rounded hover:bg-green-600 disabled:opacity-50"
          >
            ▶️ Play
          </button>
          <button
            onClick={() => executeCommand('transport_pause')}
            disabled={isLoading}
            className="px-3 py-1 bg-yellow-500 text-white rounded hover:bg-yellow-600 disabled:opacity-50"
          >
            ⏸️ Pause
          </button>
          <button
            onClick={() => executeCommand('transport_stop')}
            disabled={isLoading}
            className="px-3 py-1 bg-red-500 text-white rounded hover:bg-red-600 disabled:opacity-50"
          >
            ⏹️ Stop
          </button>
          <button
            onClick={() => executeCommand('transport_play_pause')}
            disabled={isLoading}
            className="px-3 py-1 bg-blue-500 text-white rounded hover:bg-blue-600 disabled:opacity-50"
          >
            ⏯️ Play/Pause
          </button>
        </div>
      </div>

      {/* Recording Controls */}
      <div className="mb-4">
        <h3 className="font-semibold mb-2">Recording</h3>
        <div className="flex flex-wrap gap-2">
          <button
            onClick={() => executeCommand('transport_start_recording')}
            disabled={isLoading}
            className="px-3 py-1 bg-red-600 text-white rounded hover:bg-red-700 disabled:opacity-50"
          >
            🔴 Start Recording
          </button>
          <button
            onClick={() => executeCommand('transport_stop_recording')}
            disabled={isLoading}
            className="px-3 py-1 bg-gray-500 text-white rounded hover:bg-gray-600 disabled:opacity-50"
          >
            ⏹️ Stop Recording
          </button>
        </div>
      </div>

      {/* Position Controls */}
      <div className="mb-4">
        <h3 className="font-semibold mb-2">Position</h3>
        <div className="flex flex-wrap gap-2">
          <button
            onClick={() => executeCommand('transport_set_position', { seconds: 0 })}
            disabled={isLoading}
            className="px-3 py-1 bg-purple-500 text-white rounded hover:bg-purple-600 disabled:opacity-50"
          >
            ⏮️ Go to Start
          </button>
          <button
            onClick={() => executeCommand('transport_set_position', { seconds: 30 })}
            disabled={isLoading}
            className="px-3 py-1 bg-purple-500 text-white rounded hover:bg-purple-600 disabled:opacity-50"
          >
            ⏭️ Go to 30s
          </button>
          <button
            onClick={() => {
              const currentTime = transport?.playhead_position.time.seconds || 0;
              executeCommand('transport_set_position', { seconds: currentTime + 10 });
            }}
            disabled={isLoading || !transport}
            className="px-3 py-1 bg-purple-500 text-white rounded hover:bg-purple-600 disabled:opacity-50"
          >
            ⏩ +10s
          </button>
        </div>
      </div>

      {/* Tempo Controls */}
      <div className="mb-4">
        <h3 className="font-semibold mb-2">Tempo</h3>
        <div className="flex flex-wrap gap-2">
          <button
            onClick={() => executeCommand('transport_set_tempo', { bpm: 120 })}
            disabled={isLoading}
            className="px-3 py-1 bg-indigo-500 text-white rounded hover:bg-indigo-600 disabled:opacity-50"
          >
            120 BPM
          </button>
          <button
            onClick={() => executeCommand('transport_set_tempo', { bpm: 140 })}
            disabled={isLoading}
            className="px-3 py-1 bg-indigo-500 text-white rounded hover:bg-indigo-600 disabled:opacity-50"
          >
            140 BPM
          </button>
          <button
            onClick={() => {
              const currentTempo = transport?.tempo.bpm || 120;
              executeCommand('transport_set_tempo', { bpm: currentTempo + 10 });
            }}
            disabled={isLoading || !transport}
            className="px-3 py-1 bg-indigo-500 text-white rounded hover:bg-indigo-600 disabled:opacity-50"
          >
            +10 BPM
          </button>
        </div>
      </div>

      {/* Time Signature */}
      <div className="mb-4">
        <h3 className="font-semibold mb-2">Time Signature</h3>
        <div className="flex flex-wrap gap-2">
          <button
            onClick={() => executeCommand('transport_set_time_signature', { numerator: 4, denominator: 4 })}
            disabled={isLoading}
            className="px-3 py-1 bg-teal-500 text-white rounded hover:bg-teal-600 disabled:opacity-50"
          >
            4/4
          </button>
          <button
            onClick={() => executeCommand('transport_set_time_signature', { numerator: 3, denominator: 4 })}
            disabled={isLoading}
            className="px-3 py-1 bg-teal-500 text-white rounded hover:bg-teal-600 disabled:opacity-50"
          >
            3/4
          </button>
          <button
            onClick={() => executeCommand('transport_set_time_signature', { numerator: 6, denominator: 8 })}
            disabled={isLoading}
            className="px-3 py-1 bg-teal-500 text-white rounded hover:bg-teal-600 disabled:opacity-50"
          >
            6/8
          </button>
        </div>
      </div>

      {/* Refresh */}
      <div>
        <button
          onClick={loadAppState}
          disabled={isLoading}
          className="w-full px-3 py-2 bg-gray-500 text-white rounded hover:bg-gray-600 disabled:opacity-50"
        >
          🔄 Refresh State
        </button>
      </div>
    </div>
  );
};

export default TransportTest;
