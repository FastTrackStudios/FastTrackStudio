import { useState, useEffect, useCallback } from 'react';
import { invoke } from '@tauri-apps/api/core';
import type { AppStateSnapshot } from '../bindings';

interface UseAppStateOptions {
  pollInterval?: number;
  autoStart?: boolean;
}

interface UseAppStateReturn {
  appState: AppStateSnapshot | null;
  loading: boolean;
  error: string | null;
  connected: boolean;
  refreshState: () => Promise<void>;
  updatePreferences: (preferences: import('../bindings').AppPreferences) => Promise<void>;
  updateUIState: (uiState: import('../bindings').UIState) => Promise<void>;
}

/**
 * Custom hook for comprehensive app state management
 * Integrates with the auto-generated Rust types and Tauri commands
 */
export const useAppState = (options: UseAppStateOptions = {}): UseAppStateReturn => {
  const { pollInterval = 1000, autoStart = true } = options;

  const [appState, setAppState] = useState<AppStateSnapshot | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [connected, setConnected] = useState(false);

  // Load app state from Tauri backend
  const loadAppState = useCallback(async () => {
    try {
      console.log('📡 useAppState: Loading app state...');
      const state = await invoke<AppStateSnapshot>('get_app_state');
      console.log('📊 useAppState: App state loaded:', {
        active_project: state.active_project,
        projects: Object.keys(state.projects),
        total_projects: Object.keys(state.projects).length
      });
      setAppState(state);
      setConnected(true);
      setError(null);
      return state;
    } catch (err) {
      const errorMessage = err instanceof Error ? err.message : String(err);
      console.error('❌ useAppState: Failed to load app state:', err);
      setError(errorMessage);
      setConnected(false);
      throw err;
    }
  }, []);

  // Refresh state manually
  const refreshState = useCallback(async () => {
    setLoading(true);
    try {
      await loadAppState();
    } finally {
      setLoading(false);
    }
  }, [loadAppState]);

  // Update app preferences
  const updatePreferences = useCallback(async (preferences: import('../bindings').AppPreferences) => {
    try {
      await invoke('update_app_preferences', { preferences });
      await loadAppState(); // Refresh state after update
    } catch (err) {
      const errorMessage = err instanceof Error ? err.message : String(err);
      setError(errorMessage);
      throw err;
    }
  }, [loadAppState]);

  // Update UI state
  const updateUIState = useCallback(async (uiState: import('../bindings').UIState) => {
    try {
      await invoke('update_ui_state', { uiState });
      await loadAppState(); // Refresh state after update
    } catch (err) {
      const errorMessage = err instanceof Error ? err.message : String(err);
      setError(errorMessage);
      throw err;
    }
  }, [loadAppState]);

  // Initial load and polling setup
  useEffect(() => {
    let mounted = true;
    let pollTimer: NodeJS.Timeout | null = null;

    const startPolling = async () => {
      if (!autoStart) return;

      // Initial load
      try {
        await loadAppState();
      } catch (err) {
        // Error already handled in loadAppState
      } finally {
        if (mounted) {
          setLoading(false);
        }
      }

      // Set up polling if interval is provided
      if (pollInterval > 0 && mounted) {
        pollTimer = setInterval(async () => {
          if (!mounted) return;

          try {
            await loadAppState();
          } catch (err) {
            // Error already handled in loadAppState
          }
        }, pollInterval);
      }
    };

    startPolling();

    return () => {
      mounted = false;
      if (pollTimer) {
        clearInterval(pollTimer);
      }
    };
  }, [loadAppState, pollInterval, autoStart]);

  return {
    appState,
    loading,
    error,
    connected,
    refreshState,
    updatePreferences,
    updateUIState
  };
};

/**
 * Hook to get the active project from app state
 */
export const useActiveProject = (appState: AppStateSnapshot | null) => {
  if (!appState || !appState.active_project) {
    return null;
  }

  return appState.projects[appState.active_project] || null;
};

/**
 * Hook to get transport state from the active project
 */
export const useTransportState = (appState: AppStateSnapshot | null) => {
  const activeProject = useActiveProject(appState);
  const transport = activeProject?.transport || null;

  console.log('🚛 useTransportState: Transport state extracted:', {
    hasAppState: !!appState,
    activeProjectName: appState?.active_project,
    hasActiveProject: !!activeProject,
    hasTransport: !!transport,
    playState: transport?.play_state,
    isPlaying: transport?.play_state === 'Playing'
  });

  return transport;
};

/**
 * Hook for transport controls with type safety
 */
export const useTransportControls = () => {
  const playPause = useCallback(async () => {
    try {
      console.log('🎵 useTransportControls: Invoking transport_play_pause...');
      const result = await invoke<string>('transport_play_pause');
      console.log('✅ useTransportControls: transport_play_pause result:', result);
      return result;
    } catch (err) {
      console.error('❌ useTransportControls: Failed to toggle play/pause:', err);
      throw err;
    }
  }, []);

  const play = useCallback(async () => {
    try {
      return await invoke<string>('transport_play');
    } catch (err) {
      console.error('Failed to play:', err);
      throw err;
    }
  }, []);

  const pause = useCallback(async () => {
    try {
      return await invoke<string>('transport_pause');
    } catch (err) {
      console.error('Failed to pause:', err);
      throw err;
    }
  }, []);

  const stop = useCallback(async () => {
    try {
      return await invoke<string>('transport_stop');
    } catch (err) {
      console.error('Failed to stop:', err);
      throw err;
    }
  }, []);

  const startRecording = useCallback(async () => {
    try {
      return await invoke<string>('transport_start_recording');
    } catch (err) {
      console.error('Failed to start recording:', err);
      throw err;
    }
  }, []);

  const stopRecording = useCallback(async () => {
    try {
      return await invoke<string>('transport_stop_recording');
    } catch (err) {
      console.error('Failed to stop recording:', err);
      throw err;
    }
  }, []);

  const setTempo = useCallback(async (bpm: number) => {
    try {
      return await invoke<string>('transport_set_tempo', { bpm });
    } catch (err) {
      console.error('Failed to set tempo:', err);
      throw err;
    }
  }, []);

  const setPosition = useCallback(async (seconds: number) => {
    try {
      return await invoke<string>('transport_set_position', { seconds });
    } catch (err) {
      console.error('Failed to set position:', err);
      throw err;
    }
  }, []);

  return {
    playPause,
    play,
    pause,
    stop,
    startRecording,
    stopRecording,
    setTempo,
    setPosition
  };
};

export default useAppState;
