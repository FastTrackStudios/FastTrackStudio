import React, { useState, useEffect, useCallback, useRef } from 'react';
import { api } from '@/lib/tauri-api';
import type { ProjectInfo, TransportStateResponse, Transport } from '@/lib/types';

interface UseProjectManagerOptions {
  autoLoadProjects?: boolean;
  pollInterval?: number;
}

interface UseProjectManagerReturn {
  // Project state
  projects: string[];
  activeProject: string | null;
  projectsLoading: boolean;
  error: string | null;

  // Transport state for active project
  transportState: Transport | null;
  transportLoading: boolean;

  // Project actions
  createProject: (name: string) => Promise<void>;
  setActiveProject: (name: string) => Promise<void>;
  refreshProjects: () => Promise<void>;
  getProjectInfo: (name: string) => Promise<ProjectInfo>;

  // Transport actions for active project
  play: () => Promise<void>;
  pause: () => Promise<void>;
  stop: () => Promise<void>;
  playPause: () => Promise<void>;
  startRecording: () => Promise<void>;
  stopRecording: () => Promise<void>;
  setTempo: (bpm: number) => Promise<void>;
  setPosition: (seconds: number) => Promise<void>;
  setTimeSignature: (numerator: number, denominator: number) => Promise<void>;
  refreshTransportState: () => Promise<void>;

  // Transport actions for specific project
  playProject: (projectName: string) => Promise<void>;
  pauseProject: (projectName: string) => Promise<void>;
  stopProject: (projectName: string) => Promise<void>;

  // Derived state
  isPlaying: boolean;
  isRecording: boolean;
  currentTempo: number;
  currentPosition: number;
  timeSignature: { numerator: number; denominator: number };
}

export function useProjectManager(options: UseProjectManagerOptions = {}): UseProjectManagerReturn {
  const {
    autoLoadProjects = true,
    pollInterval = 1000, // Poll transport state every second
  } = options;

  // State
  const [projects, setProjects] = useState<string[]>([]);
  const [activeProject, setActiveProjectState] = useState<string | null>(null);
  const [projectsLoading, setProjectsLoading] = useState(false);
  const [transportState, setTransportState] = useState<Transport | null>(null);
  const [transportLoading, setTransportLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  // Refs for cleanup
  const mountedRef = useRef(true);
  const pollTimerRef = useRef<NodeJS.Timeout | null>(null);

  // Helper to handle errors
  const handleError = useCallback((err: any, operation: string) => {
    const message = err instanceof Error ? err.message : String(err);
    console.error(`Error in ${operation}:`, message);
    setError(`${operation}: ${message}`);
  }, []);

  // Load projects list
  const refreshProjects = useCallback(async () => {
    if (!mountedRef.current) return;

    try {
      setProjectsLoading(true);
      setError(null);

      const [projectsList, currentActive] = await Promise.all([
        api.projects.list_projects(),
        api.projects.get_active_project(),
      ]);

      if (mountedRef.current) {
        setProjects(projectsList);
        setActiveProjectState(currentActive);
      }
    } catch (err) {
      if (mountedRef.current) {
        handleError(err, 'Loading projects');
      }
    } finally {
      if (mountedRef.current) {
        setProjectsLoading(false);
      }
    }
  }, [handleError]);

  // Load transport state for active project
  const refreshTransportState = useCallback(async () => {
    if (!mountedRef.current || !activeProject) return;

    try {
      setTransportLoading(true);
      const state = await api.transport.get_state();

      if (mountedRef.current) {
        setTransportState(state);
      }
    } catch (err) {
      if (mountedRef.current) {
        handleError(err, 'Loading transport state');
      }
    } finally {
      if (mountedRef.current) {
        setTransportLoading(false);
      }
    }
  }, [activeProject, handleError]);

  // Project actions
  const createProject = useCallback(async (name: string) => {
    try {
      setError(null);
      await api.projects.create_project(name);
      await refreshProjects();
    } catch (err) {
      handleError(err, 'Creating project');
      throw err;
    }
  }, [refreshProjects, handleError]);

  const setActiveProject = useCallback(async (name: string) => {
    try {
      setError(null);
      await api.projects.set_active_project(name);
      setActiveProjectState(name);
      await refreshTransportState();
    } catch (err) {
      handleError(err, 'Setting active project');
      throw err;
    }
  }, [refreshTransportState, handleError]);

  const getProjectInfo = useCallback(async (name: string): Promise<ProjectInfo> => {
    try {
      return await api.projects.get_project_info(name);
    } catch (err) {
      handleError(err, 'Getting project info');
      throw err;
    }
  }, [handleError]);

  // Transport actions for active project
  const play = useCallback(async () => {
    try {
      await api.transport.play();
      await refreshTransportState();
    } catch (err) {
      handleError(err, 'Playing transport');
      throw err;
    }
  }, [refreshTransportState, handleError]);

  const pause = useCallback(async () => {
    try {
      await api.transport.pause();
      await refreshTransportState();
    } catch (err) {
      handleError(err, 'Pausing transport');
      throw err;
    }
  }, [refreshTransportState, handleError]);

  const stop = useCallback(async () => {
    try {
      await api.transport.stop();
      await refreshTransportState();
    } catch (err) {
      handleError(err, 'Stopping transport');
      throw err;
    }
  }, [refreshTransportState, handleError]);

  const playPause = useCallback(async () => {
    try {
      await api.transport.play_pause();
      await refreshTransportState();
    } catch (err) {
      handleError(err, 'Play/pause transport');
      throw err;
    }
  }, [refreshTransportState, handleError]);

  const startRecording = useCallback(async () => {
    try {
      await api.transport.start_recording();
      await refreshTransportState();
    } catch (err) {
      handleError(err, 'Starting recording');
      throw err;
    }
  }, [refreshTransportState, handleError]);

  const stopRecording = useCallback(async () => {
    try {
      await api.transport.stop_recording();
      await refreshTransportState();
    } catch (err) {
      handleError(err, 'Stopping recording');
      throw err;
    }
  }, [refreshTransportState, handleError]);

  const setTempo = useCallback(async (bpm: number) => {
    try {
      await api.transport.set_tempo(bpm);
      await refreshTransportState();
    } catch (err) {
      handleError(err, 'Setting tempo');
      throw err;
    }
  }, [refreshTransportState, handleError]);

  const setPosition = useCallback(async (seconds: number) => {
    try {
      await api.transport.set_position(seconds);
      await refreshTransportState();
    } catch (err) {
      handleError(err, 'Setting position');
      throw err;
    }
  }, [refreshTransportState, handleError]);

  const setTimeSignature = useCallback(async (numerator: number, denominator: number) => {
    try {
      await api.transport.set_time_signature(numerator, denominator);
      await refreshTransportState();
    } catch (err) {
      handleError(err, 'Setting time signature');
      throw err;
    }
  }, [refreshTransportState, handleError]);

  // Transport actions for specific project
  const playProject = useCallback(async (projectName: string) => {
    try {
      await api.projectTransport.play(projectName);
      if (projectName === activeProject) {
        await refreshTransportState();
      }
    } catch (err) {
      handleError(err, `Playing project ${projectName}`);
      throw err;
    }
  }, [activeProject, refreshTransportState, handleError]);

  const pauseProject = useCallback(async (projectName: string) => {
    try {
      await api.projectTransport.pause(projectName);
      if (projectName === activeProject) {
        await refreshTransportState();
      }
    } catch (err) {
      handleError(err, `Pausing project ${projectName}`);
      throw err;
    }
  }, [activeProject, refreshTransportState, handleError]);

  const stopProject = useCallback(async (projectName: string) => {
    try {
      await api.projectTransport.stop(projectName);
      if (projectName === activeProject) {
        await refreshTransportState();
      }
    } catch (err) {
      handleError(err, `Stopping project ${projectName}`);
      throw err;
    }
  }, [activeProject, refreshTransportState, handleError]);

  // Auto-load projects on mount
  useEffect(() => {
    if (autoLoadProjects) {
      refreshProjects();
    }

    return () => {
      mountedRef.current = false;
    };
  }, [autoLoadProjects, refreshProjects]);

  // Load transport state when active project changes
  useEffect(() => {
    if (activeProject) {
      refreshTransportState();
    } else {
      setTransportState(null);
    }
  }, [activeProject, refreshTransportState]);

  // Polling for transport state updates
  useEffect(() => {
    if (pollInterval > 0 && activeProject && transportState?.play_state === 'Playing') {
      pollTimerRef.current = setInterval(() => {
        refreshTransportState();
      }, pollInterval);

      return () => {
        if (pollTimerRef.current) {
          clearInterval(pollTimerRef.current);
        }
      };
    }
  }, [pollInterval, activeProject, transportState?.play_state, refreshTransportState]);

  // Derived state
  const isPlaying = transportState?.play_state === 'Playing' || false;
  const isRecording = transportState?.play_state === 'Recording' || false;
  const currentTempo = transportState?.tempo.bpm || 120;
  const currentPosition = transportState?.playhead_position.time.seconds || 0;
  const timeSignature = transportState?.time_signature || { numerator: 4, denominator: 4 };

  return {
    // Project state
    projects,
    activeProject,
    projectsLoading,
    error,

    // Transport state
    transportState,
    transportLoading,

    // Project actions
    createProject,
    setActiveProject,
    refreshProjects,
    getProjectInfo,

    // Transport actions for active project
    play,
    pause,
    stop,
    playPause,
    startRecording,
    stopRecording,
    setTempo,
    setPosition,
    setTimeSignature,
    refreshTransportState,

    // Transport actions for specific project
    playProject,
    pauseProject,
    stopProject,

    // Derived state
    isPlaying,
    isRecording,
    currentTempo,
    currentPosition,
    timeSignature,
  };
}

// Higher-order component for providing project manager context
export function withProjectManager<P extends object>(
  Component: React.ComponentType<P & { projectManager: UseProjectManagerReturn }>
) {
  return function WithProjectManagerComponent(props: P) {
    const projectManager = useProjectManager();
    return React.createElement(Component, { ...props, projectManager });
  };
}
