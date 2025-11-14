import React, { useState, useEffect } from 'react';
import { invoke } from '@tauri-apps/api/core';
import type {
  AppStateSnapshot,
  PlayState,
  AppPreferences
} from '../bindings';

/**
 * Example component demonstrating comprehensive app state usage
 * This shows how to use the generated Rust types in React
 */
export const AppStateExample: React.FC = () => {
  const [appState, setAppState] = useState<AppStateSnapshot | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  // Load initial app state
  useEffect(() => {
    console.log('🚀 AppStateExample component mounted, loading initial state...');
    loadAppState();
  }, []);

  const loadAppState = async () => {
    try {
      setLoading(true);
      console.log('📡 Loading app state...');
      const state = await invoke<AppStateSnapshot>('get_app_state');
      console.log('📊 App state loaded:', state);
      console.log('📋 Active project:', state.active_project);
      console.log('📁 Available projects:', Object.keys(state.projects));
      setAppState(state);
      setError(null);
    } catch (err) {
      console.error('❌ Failed to load app state:', err);
      console.error('❌ Error details:', err);
      setError(String(err));
    } finally {
      setLoading(false);
    }
  };

  const updatePreferences = async (newTheme: 'Light' | 'Dark' | 'System') => {
    if (!appState) return;

    const updatedPreferences: AppPreferences = {
      ...appState.preferences,
      theme: newTheme
    };

    try {
      await invoke('update_app_preferences', { preferences: updatedPreferences });
      await loadAppState(); // Refresh state
    } catch (err) {
      setError(err as string);
    }
  };

  const toggleTransport = async () => {
    console.log('🎵 Transport button clicked!');
    console.log('AppState:', appState);
    console.log('Active project:', appState?.active_project);

    if (!appState?.active_project) {
      console.error('❌ No active project found!');
      setError('No active project available');
      return;
    }

    try {
      console.log('🔄 Invoking transport_play_pause command...');
      const result = await invoke('transport_play_pause');
      console.log('✅ Transport command result:', result);
      await loadAppState(); // Refresh state
      console.log('🔄 State refreshed');
    } catch (err) {
      console.error('❌ Transport command failed:', err);
      setError(err as string);
    }
  };

  const createDefaultProject = async () => {
    console.log('🏗️ Creating default project...');
    try {
      const result = await invoke('create_project', { name: 'Default Project' });
      console.log('✅ Project created:', result);
      await loadAppState(); // Refresh state
    } catch (err) {
      console.error('❌ Failed to create project:', err);
      setError(err as string);
    }
  };

  if (loading) {
    return (
      <div className="p-4">
        <div className="text-lg">Loading app state...</div>
        <div className="text-sm text-gray-500 mt-2">
          Check browser console (F12) for debug logs
        </div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="p-4 text-red-600">
        <h3>Error loading app state:</h3>
        <p className="bg-red-50 p-3 rounded border">{error}</p>
        <div className="mt-4 space-y-2">
          <button
            onClick={loadAppState}
            className="px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600"
          >
            Retry Load State
          </button>
          <button
            onClick={createDefaultProject}
            className="ml-2 px-4 py-2 bg-green-500 text-white rounded hover:bg-green-600"
          >
            Create Default Project
          </button>
        </div>
        <div className="mt-4 text-sm text-gray-600">
          <strong>Debug Info:</strong>
          <div className="bg-gray-100 p-2 rounded mt-1">
            Check browser console (F12) for detailed error logs
          </div>
        </div>
      </div>
    );
  }

  if (!appState) {
    return <div className="p-4">No app state available</div>;
  }

  const activeProject = appState.active_project
    ? appState.projects[appState.active_project]
    : null;

  const getPlayStateDisplay = (playState: PlayState): string => {
    const stateMap = {
      'Stopped': '⏹️ Stopped',
      'Playing': '▶️ Playing',
      'Paused': '⏸️ Paused',
      'Recording': '🔴 Recording'
    };
    return stateMap[playState] || playState;
  };

  return (
    <div className="p-6 max-w-4xl mx-auto">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-2xl font-bold">FastTrackStudio App State</h1>
        <button
          onClick={() => console.log('📊 Current AppState:', { appState, loading, error })}
          className="px-3 py-1 text-sm bg-gray-200 rounded hover:bg-gray-300"
        >
          Show Debug Info
        </button>
      </div>

      {/* App Info */}
      <section className="mb-6 p-4 bg-gray-100 rounded-lg">
        <h2 className="text-lg font-semibold mb-2">Application Info</h2>
        <div className="grid grid-cols-2 gap-4 text-sm">
          <div>
            <strong>Version:</strong> {appState.app_version}
          </div>
          <div>
            <strong>Debug Mode:</strong> {appState.debug_mode ? 'Yes' : 'No'}
          </div>
          <div>
            <strong>Theme:</strong> {appState.preferences.theme}
          </div>
          <div>
            <strong>Projects:</strong> {Object.keys(appState.projects).length}
          </div>
        </div>
        <div className="mt-3">
          <strong>Theme Controls:</strong>
          <div className="flex gap-2 mt-1">
            {(['Light', 'Dark', 'System'] as const).map(theme => (
              <button
                key={theme}
                onClick={() => updatePreferences(theme)}
                className={`px-3 py-1 rounded text-sm ${
                  appState.preferences.theme === theme
                    ? 'bg-blue-500 text-white'
                    : 'bg-gray-200 hover:bg-gray-300'
                }`}
              >
                {theme}
              </button>
            ))}
          </div>
        </div>
      </section>

      {/* Active Project */}
      <section className="mb-6 p-4 bg-blue-50 rounded-lg">
        <h2 className="text-lg font-semibold mb-2">Active Project</h2>
        {activeProject ? (
          <div>
            <div className="grid grid-cols-2 gap-4 mb-4">
              <div>
                <strong>Name:</strong> {activeProject.name}
              </div>
              <div>
                <strong>Created:</strong> {new Date(activeProject.metadata.created_at).toLocaleDateString()}
              </div>
              <div>
                <strong>Sample Rate:</strong> {activeProject.metadata.sample_rate} Hz
              </div>
              <div>
                <strong>Bit Depth:</strong> {activeProject.metadata.bit_depth} bit
              </div>
            </div>

            {/* Transport Controls */}
            <div className="p-3 bg-white rounded border">
              <h3 className="font-semibold mb-2">Transport</h3>
              <div className="grid grid-cols-3 gap-4 mb-3 text-sm">
                <div>
                  <strong>State:</strong> {getPlayStateDisplay(activeProject.transport.play_state)}
                </div>
                <div>
                  <strong>Tempo:</strong> {activeProject.transport.tempo.bpm} BPM
                </div>
                <div>
                  <strong>Time Sig:</strong> {activeProject.transport.time_signature.numerator}/{activeProject.transport.time_signature.denominator}
                </div>
                <div>
                  <strong>Position:</strong> {activeProject.transport.playhead_position.time.seconds.toFixed(2)}s
                </div>
                <div>
                  <strong>Looping:</strong> {activeProject.transport.looping ? 'Yes' : 'No'}
                </div>
                <div>
                  <strong>Recording Mode:</strong> {activeProject.transport.record_mode}
                </div>
              </div>
              <div className="flex gap-2">
                <button
                  onClick={toggleTransport}
                  className="px-4 py-2 bg-green-500 text-white rounded hover:bg-green-600"
                  title="Click to toggle transport (check console for debug logs)"
                >
                  🎵 {activeProject.transport.play_state === 'Playing' ? 'Pause' : 'Play'}
                </button>
                {!appState?.active_project && (
                  <button
                    onClick={createDefaultProject}
                    className="px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600"
                  >
                    Create Project
                  </button>
                )}
              </div>
            </div>
          </div>
        ) : (
          <div className="space-y-4">
            <p className="text-red-600 font-medium">❌ No active project found</p>
            <div className="bg-yellow-50 p-4 rounded border border-yellow-200">
              <p className="text-sm text-yellow-800">
                <strong>Debug Info:</strong><br/>
                • Total projects: {Object.keys(appState?.projects || {}).length}<br/>
                • Active project name: {appState?.active_project || 'null'}<br/>
                • Available projects: {Object.keys(appState?.projects || {}).join(', ') || 'none'}
              </p>
              <button
                onClick={createDefaultProject}
                className="mt-3 px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600"
              >
                Create Default Project
              </button>
            </div>
          </div>
        )}
      </section>

      {/* UI State */}
      <section className="mb-6 p-4 bg-purple-50 rounded-lg">
        <h2 className="text-lg font-semibold mb-2">UI State</h2>
        <div className="grid grid-cols-2 gap-4 text-sm">
          <div>
            <strong>Selected Tool:</strong> {appState.ui.selected_tool}
          </div>
          <div>
            <strong>Timeline Zoom:</strong> {appState.ui.timeline_zoom}x
          </div>
          <div>
            <strong>Window Size:</strong> {appState.ui.window_size.width}x{appState.ui.window_size.height}
          </div>
          <div>
            <strong>Transport Panel:</strong> {appState.ui.transport_panel_visible ? 'Visible' : 'Hidden'}
          </div>
        </div>
      </section>

      {/* Recent Projects */}
      <section className="mb-6 p-4 bg-green-50 rounded-lg">
        <h2 className="text-lg font-semibold mb-2">Recent Projects</h2>
        {appState.recent_projects.projects.length > 0 ? (
          <ul className="space-y-2">
            {appState.recent_projects.projects.slice(0, 5).map((project, index) => (
              <li key={index} className="text-sm">
                <strong>{project.name}</strong> - {new Date(project.last_opened).toLocaleString()}
              </li>
            ))}
          </ul>
        ) : (
          <p className="text-sm text-gray-600">No recent projects</p>
        )}
      </section>

      {/* All Projects */}
      <section className="mb-6 p-4 bg-yellow-50 rounded-lg">
        <h2 className="text-lg font-semibold mb-2">All Projects</h2>
        <div className="space-y-2">
          {Object.values(appState.projects).filter(Boolean).map((project) => (
            <div
              key={project?.name || 'unknown'}
              className={`p-2 rounded text-sm ${
                project?.is_active
                  ? 'bg-blue-200 border-2 border-blue-400'
                  : 'bg-white border'
              }`}
            >
              <div className="flex justify-between items-center">
                <span className="font-medium">{project?.name}</span>
                <span className="text-xs">
                  {project?.transport ? getPlayStateDisplay(project.transport.play_state) : 'Unknown'}
                </span>
              </div>
            </div>
          ))}
        </div>
      </section>

      {/* Debug Info */}
      <section className="p-4 bg-gray-50 rounded-lg">
        <h2 className="text-lg font-semibold mb-2">Debug Info</h2>
        <div className="text-xs space-y-2">
          <p><strong>Snapshot Timestamp:</strong> {appState.snapshot_timestamp}</p>
          <p><strong>Loading:</strong> {loading ? 'Yes' : 'No'}</p>
          <p><strong>Error:</strong> {error || 'None'}</p>
          <p><strong>Active Project:</strong> {appState.active_project || 'None'}</p>
          <div className="flex gap-2 mt-3">
            <button
              onClick={loadAppState}
              className="px-3 py-1 bg-gray-200 text-gray-700 rounded text-sm hover:bg-gray-300"
              disabled={loading}
            >
              {loading ? 'Refreshing...' : 'Refresh State'}
            </button>
            <button
              onClick={() => {
                console.log('🔍 FULL DEBUG DUMP:');
                console.log('AppState:', appState);
                console.log('Loading:', loading);
                console.log('Error:', error);
                console.log('Available Tauri Commands: get_app_state, transport_play_pause, create_project');
              }}
              className="px-3 py-1 bg-blue-200 text-blue-700 rounded text-sm hover:bg-blue-300"
            >
              Log Debug Info
            </button>
          </div>
        </div>
      </section>
    </div>
  );
};

export default AppStateExample;
