import { invoke } from '@tauri-apps/api/core';
import type {
  ProjectInfo,
  Transport,
  AppStateSnapshot,
  ProjectManager,
  TransportController,
  ProjectTransportController
} from './types';

/**
 * Tauri API wrapper for FastTrack Studio Desktop
 * Provides type-safe access to Rust backend functionality
 */
class TauriAPI {
  // Project Management
  readonly projects: ProjectManager = {
    list_projects: () => invoke<string[]>('list_projects'),

    get_active_project: () => invoke<string | null>('get_active_project'),

    create_project: (name: string) => invoke<string>('create_project', { name }),

    set_active_project: (name: string) => invoke<string>('set_active_project', { name }),

    get_project_info: (name: string) => invoke<ProjectInfo>('get_project_info', { name }),
  };

  // Transport for Active Project
  readonly transport: TransportController = {
    play: () => invoke<string>('transport_play'),

    pause: () => invoke<string>('transport_pause'),

    stop: () => invoke<string>('transport_stop'),

    play_pause: () => invoke<string>('transport_play_pause'),

    start_recording: () => invoke<string>('transport_start_recording'),

    stop_recording: () => invoke<string>('transport_stop_recording'),

    set_tempo: (bpm: number) => invoke<string>('transport_set_tempo', { bpm }),

    set_position: (seconds: number) => invoke<string>('transport_set_position', { seconds }),

    set_time_signature: (numerator: number, denominator: number) =>
      invoke<string>('transport_set_time_signature', { numerator, denominator }),

    get_state: async (): Promise<Transport | null> => {
      const appState = await invoke<AppStateSnapshot>('get_app_state');
      if (!appState.active_project) return null;
      const project = appState.projects[appState.active_project];
      return project?.transport || null;
    },
  };

  // Transport for Specific Project
  readonly projectTransport: ProjectTransportController = {
    play: (project_name: string) => invoke<string>('project_transport_play', { project_name }),

    pause: (project_name: string) => invoke<string>('project_transport_pause', { project_name }),

    stop: (project_name: string) => invoke<string>('project_transport_stop', { project_name }),
  };

  // Legacy greet function (can be removed later)
  greet(name: string): Promise<string> {
    return invoke<string>('greet', { name });
  }
}

// Export singleton instance
export const api = new TauriAPI();

// Also export the class for advanced usage
export { TauriAPI };

// Export convenience functions for common operations
export const projectManager = api.projects;
export const transport = api.transport;
export const projectTransport = api.projectTransport;
