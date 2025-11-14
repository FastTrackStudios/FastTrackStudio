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
    list_projects: async () => {
      console.log('🔄 API: Calling list_projects...');
      try {
        const result = await invoke<string[]>('list_projects');
        console.log('✅ API: list_projects result:', result);
        return result;
      } catch (error) {
        console.error('❌ API: list_projects error:', error);
        throw error;
      }
    },

    get_active_project: async () => {
      console.log('🔄 API: Calling get_active_project...');
      try {
        const result = await invoke<string | null>('get_active_project');
        console.log('✅ API: get_active_project result:', result);
        return result;
      } catch (error) {
        console.error('❌ API: get_active_project error:', error);
        throw error;
      }
    },

    create_project: async (name: string) => {
      console.log('🔄 API: Calling create_project with name:', name);
      try {
        const result = await invoke<string>('create_project', { name });
        console.log('✅ API: create_project result:', result);
        return result;
      } catch (error) {
        console.error('❌ API: create_project error:', error);
        throw error;
      }
    },

    set_active_project: async (name: string) => {
      console.log('🔄 API: Calling set_active_project with name:', name);
      try {
        const result = await invoke<string>('set_active_project', { name });
        console.log('✅ API: set_active_project result:', result);
        return result;
      } catch (error) {
        console.error('❌ API: set_active_project error:', error);
        throw error;
      }
    },

    get_project_info: async (name: string) => {
      console.log('🔄 API: Calling get_project_info with name:', name);
      try {
        const result = await invoke<ProjectInfo>('get_project_info', { name });
        console.log('✅ API: get_project_info result:', result);
        return result;
      } catch (error) {
        console.error('❌ API: get_project_info error:', error);
        throw error;
      }
    },
  };

  // Transport for Active Project
  readonly transport: TransportController = {
    play: async () => {
      console.log('🔄 API: Calling transport_play...');
      try {
        const result = await invoke<string>('transport_play');
        console.log('✅ API: transport_play result:', result);
        return result;
      } catch (error) {
        console.error('❌ API: transport_play error:', error);
        throw error;
      }
    },

    pause: async () => {
      console.log('🔄 API: Calling transport_pause...');
      try {
        const result = await invoke<string>('transport_pause');
        console.log('✅ API: transport_pause result:', result);
        return result;
      } catch (error) {
        console.error('❌ API: transport_pause error:', error);
        throw error;
      }
    },

    stop: async () => {
      console.log('🔄 API: Calling transport_stop...');
      try {
        const result = await invoke<string>('transport_stop');
        console.log('✅ API: transport_stop result:', result);
        return result;
      } catch (error) {
        console.error('❌ API: transport_stop error:', error);
        throw error;
      }
    },

    play_pause: async () => {
      console.log('🔄 API: Calling transport_play_pause...');
      try {
        const result = await invoke<string>('transport_play_pause');
        console.log('✅ API: transport_play_pause result:', result);
        return result;
      } catch (error) {
        console.error('❌ API: transport_play_pause error:', error);
        throw error;
      }
    },

    start_recording: async () => {
      console.log('🔄 API: Calling transport_start_recording...');
      try {
        const result = await invoke<string>('transport_start_recording');
        console.log('✅ API: transport_start_recording result:', result);
        return result;
      } catch (error) {
        console.error('❌ API: transport_start_recording error:', error);
        throw error;
      }
    },

    stop_recording: async () => {
      console.log('🔄 API: Calling transport_stop_recording...');
      try {
        const result = await invoke<string>('transport_stop_recording');
        console.log('✅ API: transport_stop_recording result:', result);
        return result;
      } catch (error) {
        console.error('❌ API: transport_stop_recording error:', error);
        throw error;
      }
    },

    set_tempo: async (bpm: number) => {
      console.log('🔄 API: Calling transport_set_tempo with bpm:', bpm);
      try {
        const result = await invoke<string>('transport_set_tempo', { bpm });
        console.log('✅ API: transport_set_tempo result:', result);
        return result;
      } catch (error) {
        console.error('❌ API: transport_set_tempo error:', error);
        throw error;
      }
    },

    set_position: async (seconds: number) => {
      console.log('🔄 API: Calling transport_set_position with seconds:', seconds);
      try {
        const result = await invoke<string>('transport_set_position', { seconds });
        console.log('✅ API: transport_set_position result:', result);
        return result;
      } catch (error) {
        console.error('❌ API: transport_set_position error:', error);
        throw error;
      }
    },

    set_time_signature: async (numerator: number, denominator: number) => {
      console.log('🔄 API: Calling transport_set_time_signature with sig:', numerator, '/', denominator);
      try {
        const result = await invoke<string>('transport_set_time_signature', { numerator, denominator });
        console.log('✅ API: transport_set_time_signature result:', result);
        return result;
      } catch (error) {
        console.error('❌ API: transport_set_time_signature error:', error);
        throw error;
      }
    },

    get_state: async () => {
      console.log('🔄 API: Calling transport_get_state...');
      try {
        const result = await invoke<Transport>('transport_get_state');
        console.log('✅ API: transport_get_state result:', result);
        return result;
      } catch (error) {
        console.error('❌ API: transport_get_state error:', error);
        throw error;
      }
    },

  };

  // Transport for Specific Project
  readonly projectTransport: ProjectTransportController = {
    play: (project_name: string) => invoke<string>('project_transport_play', { project_name }),

    pause: (project_name: string) => invoke<string>('project_transport_pause', { project_name }),

    stop: (project_name: string) => invoke<string>('project_transport_stop', { project_name }),
  };

  // Legacy greet function (can be removed later)
  async greet(name: string): Promise<string> {
    console.log('🔄 API: Calling greet with name:', name);
    try {
      const result = await invoke<string>('greet', { name });
      console.log('✅ API: greet result:', result);
      return result;
    } catch (error) {
      console.error('❌ API: greet error:', error);
      throw error;
    }
  }

  // Test function to verify transport object serialization
  async test_transport_object(): Promise<Transport> {
    console.log('🔄 API: Calling test_transport_object...');
    try {
      const result = await invoke<Transport>('test_transport_object');
      console.log('✅ API: test_transport_object result:', result);
      return result;
    } catch (error) {
      console.error('❌ API: test_transport_object error:', error);
      throw error;
    }
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
