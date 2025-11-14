import React, { useState, useEffect } from 'react';

/**
 * Simple App State component that doesn't rely on generated bindings
 * This is a fallback version to demonstrate the tab functionality
 */
export const SimpleAppState: React.FC = () => {
  const [appInfo] = useState({
    version: '0.1.0',
    debug_mode: true,
    active_project: 'Default Project',
    project_count: 1
  });

  const [transportState, setTransportState] = useState({
    is_playing: false,
    is_recording: false,
    tempo: 120,
    position: 0.0,
    time_signature: { numerator: 4, denominator: 4 }
  });

  // Simulate some state updates
  useEffect(() => {
    const interval = setInterval(() => {
      if (transportState.is_playing) {
        setTransportState(prev => ({
          ...prev,
          position: prev.position + 0.1
        }));
      }
    }, 100);

    return () => clearInterval(interval);
  }, [transportState.is_playing]);

  const togglePlayback = () => {
    setTransportState(prev => ({
      ...prev,
      is_playing: !prev.is_playing
    }));
  };

  const toggleRecording = () => {
    setTransportState(prev => ({
      ...prev,
      is_recording: !prev.is_recording
    }));
  };

  const resetPosition = () => {
    setTransportState(prev => ({
      ...prev,
      position: 0.0,
      is_playing: false
    }));
  };

  return (
    <div className="p-6 max-w-4xl mx-auto space-y-6">
      <h1 className="text-3xl font-bold text-foreground">FastTrackStudio App State</h1>

      {/* Application Info */}
      <section className="p-6 bg-card border rounded-lg">
        <h2 className="text-xl font-semibold mb-4 flex items-center gap-2">
          <span className="w-2 h-2 bg-green-500 rounded-full"></span>
          Application Info
        </h2>
        <div className="grid grid-cols-2 gap-4 text-sm">
          <div className="flex justify-between">
            <strong>Version:</strong>
            <span>{appInfo.version}</span>
          </div>
          <div className="flex justify-between">
            <strong>Debug Mode:</strong>
            <span className={appInfo.debug_mode ? 'text-orange-500' : 'text-green-500'}>
              {appInfo.debug_mode ? 'Enabled' : 'Disabled'}
            </span>
          </div>
          <div className="flex justify-between">
            <strong>Active Project:</strong>
            <span>{appInfo.active_project}</span>
          </div>
          <div className="flex justify-between">
            <strong>Total Projects:</strong>
            <span>{appInfo.project_count}</span>
          </div>
        </div>
      </section>

      {/* Transport Controls */}
      <section className="p-6 bg-card border rounded-lg">
        <h2 className="text-xl font-semibold mb-4 flex items-center gap-2">
          🎵 Transport State
        </h2>

        <div className="grid grid-cols-3 gap-4 mb-6 text-sm">
          <div className="text-center p-3 bg-muted rounded">
            <strong className="block">Status</strong>
            <span className={`text-lg ${transportState.is_playing ? 'text-green-500' : 'text-gray-500'}`}>
              {transportState.is_playing ? '▶️ Playing' : '⏹️ Stopped'}
            </span>
          </div>
          <div className="text-center p-3 bg-muted rounded">
            <strong className="block">Tempo</strong>
            <span className="text-lg">{transportState.tempo} BPM</span>
          </div>
          <div className="text-center p-3 bg-muted rounded">
            <strong className="block">Time Signature</strong>
            <span className="text-lg">
              {transportState.time_signature.numerator}/{transportState.time_signature.denominator}
            </span>
          </div>
        </div>

        <div className="mb-6">
          <div className="flex justify-between items-center mb-2">
            <strong>Position:</strong>
            <span className="font-mono">{transportState.position.toFixed(2)}s</span>
          </div>
          <div className="w-full bg-muted rounded-full h-2">
            <div
              className="bg-primary h-2 rounded-full transition-all duration-100"
              style={{ width: `${Math.min((transportState.position / 60) * 100, 100)}%` }}
            ></div>
          </div>
        </div>

        {/* Transport Buttons */}
        <div className="flex gap-3">
          <button
            onClick={togglePlayback}
            className={`px-6 py-2 rounded font-medium transition-colors ${
              transportState.is_playing
                ? 'bg-orange-500 hover:bg-orange-600 text-white'
                : 'bg-green-500 hover:bg-green-600 text-white'
            }`}
          >
            {transportState.is_playing ? '⏸️ Pause' : '▶️ Play'}
          </button>

          <button
            onClick={toggleRecording}
            className={`px-6 py-2 rounded font-medium transition-colors ${
              transportState.is_recording
                ? 'bg-red-600 hover:bg-red-700 text-white'
                : 'bg-red-500 hover:bg-red-600 text-white border-2 border-red-300'
            }`}
          >
            {transportState.is_recording ? '⏹️ Stop Rec' : '🔴 Record'}
          </button>

          <button
            onClick={resetPosition}
            className="px-6 py-2 rounded font-medium bg-secondary hover:bg-secondary/80 text-secondary-foreground"
          >
            ⏮️ Reset
          </button>
        </div>
      </section>

      {/* Project Info */}
      <section className="p-6 bg-card border rounded-lg">
        <h2 className="text-xl font-semibold mb-4 flex items-center gap-2">
          📁 Project Details
        </h2>

        <div className="space-y-4">
          <div className="p-4 bg-muted rounded">
            <h3 className="font-medium mb-2">{appInfo.active_project}</h3>
            <div className="grid grid-cols-2 gap-4 text-sm text-muted-foreground">
              <div>Sample Rate: 44100 Hz</div>
              <div>Bit Depth: 24 bit</div>
              <div>Created: Today</div>
              <div>Status: Active</div>
            </div>
          </div>
        </div>
      </section>

      {/* Generated Types Info */}
      <section className="p-6 bg-card border rounded-lg">
        <h2 className="text-xl font-semibold mb-4 flex items-center gap-2">
          🔧 TypeScript Integration
        </h2>

        <div className="space-y-3 text-sm">
          <p className="text-muted-foreground">
            This is a simplified demo component. The full version uses auto-generated TypeScript types from Rust.
          </p>

          <div className="p-3 bg-muted rounded font-mono text-xs">
            <div className="text-green-600 mb-1"># Generate types:</div>
            <div>cargo xtask generate-types</div>
          </div>

          <div className="p-3 bg-muted rounded">
            <strong className="text-xs block mb-2">Generated files location:</strong>
            <code className="text-xs">apps/desktop/src/bindings/</code>
          </div>

          <div className="flex flex-wrap gap-2">
            {[
              'AppState.ts', 'Transport.ts', 'ProjectState.ts',
              'PlayState.ts', 'RecordMode.ts', 'UIState.ts'
            ].map(file => (
              <span key={file} className="px-2 py-1 bg-primary/10 text-primary rounded text-xs font-mono">
                {file}
              </span>
            ))}
          </div>
        </div>
      </section>

      {/* Footer */}
      <div className="text-center text-sm text-muted-foreground pt-6 border-t">
        <p>FastTrackStudio Desktop • Comprehensive App State Management</p>
        <p className="mt-1">Switch to Performance tab to see the live DAW interface</p>
      </div>
    </div>
  );
};

export default SimpleAppState;
