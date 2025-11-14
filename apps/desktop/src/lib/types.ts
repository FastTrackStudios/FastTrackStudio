// TypeScript types for FastTrack Studio Desktop App
// This file now imports from auto-generated Rust bindings for type safety

// === PRIMARY IMPORTS ===
// Import the complete application state - this is your main interface
export type {
  AppState,
  AppStateSnapshot,
  ProjectState,
  Transport,
  PlayState,
  RecordMode,
  Tempo,
  Position,
  TimePosition,
  MusicalPosition,
  TimeRange,
  TimeSignature,
  ProjectError,
  ProjectMetadata,
  AppPreferences,
  Theme,
  UIState,
  Tool,
  RecentProjects
} from '../bindings';

// Re-export everything for convenience
export * from '../bindings';

// === TAURI-SPECIFIC INTERFACES ===
// These extend the Rust types with Tauri command interfaces

/**
 * Tauri command interface for project management
 * Uses the generated ProjectState type internally
 */
export interface ProjectManager {
  list_projects: () => Promise<string[]>;
  get_active_project: () => Promise<string | null>;
  create_project: (name: string) => Promise<string>;
  set_active_project: (name: string) => Promise<string>;
  get_project_info: (name: string) => Promise<ProjectInfo>;
}

/**
 * Simplified project info for quick display
 * Derived from the comprehensive ProjectState type
 */
export interface ProjectInfo {
  name: string;
  is_playing: boolean;
  is_recording: boolean;
  tempo: number;
  position: number;
}

/**
 * Tauri command interface for transport control
 * Commands that work on the currently active project
 */
export interface TransportController {
  play: () => Promise<string>;
  pause: () => Promise<string>;
  stop: () => Promise<string>;
  play_pause: () => Promise<string>;
  start_recording: () => Promise<string>;
  stop_recording: () => Promise<string>;
  set_tempo: (bpm: number) => Promise<string>;
  set_position: (seconds: number) => Promise<string>;
  set_time_signature: (numerator: number, denominator: number) => Promise<string>;
  get_state: () => Promise<Transport | null>;
}

/**
 * Response from transport state commands
 * This extends the generated Transport type with project context
 */
export interface TransportStateResponse {
  project_name: string;
  is_playing: boolean;
  is_recording: boolean;
  tempo: number;
  position: number;
  time_signature: import('../bindings').TimeSignature;
}

/**
 * Tauri command interface for project-specific transport control
 */
export interface ProjectTransportController {
  play: (project_name: string) => Promise<string>;
  pause: (project_name: string) => Promise<string>;
  stop: (project_name: string) => Promise<string>;
}

// === HELPER TYPES ===

/**
 * Union type for all possible app states
 * Useful for state machine implementations
 */
export type AppStatus = 'loading' | 'ready' | 'error' | 'saving';

/**
 * Event types for real-time updates
 */
export type AppEvent =
  | { type: 'transport_state_changed'; payload: import('../bindings').Transport }
  | { type: 'project_switched'; payload: { from: string | null; to: string } }
  | { type: 'project_created'; payload: import('../bindings').ProjectState }
  | { type: 'preferences_updated'; payload: import('../bindings').AppPreferences }
  | { type: 'ui_state_changed'; payload: import('../bindings').UIState };

/**
 * Tauri command result wrapper
 */
export interface CommandResult<T> {
  success: boolean;
  data?: T;
  error?: string;
}

// === USAGE EXAMPLES ===
/*

// 1. Complete application state management:
import { AppStateSnapshot } from './types';

const getAppState = async (): Promise<AppStateSnapshot> => {
  return await invoke('get_app_state');
};

// 2. Type-safe transport control:
import { Transport, PlayState } from './types';

const isPlaying = (transport: Transport): boolean => {
  return transport.play_state === 'Playing';
};

// 3. Project state management:
import { ProjectState } from './types';

const getActiveProjectTransport = (state: AppStateSnapshot): Transport | null => {
  if (!state.active_project) return null;
  const project = state.projects[state.active_project];
  return project?.transport || null;
};

// 4. UI state synchronization:
import { UIState, Tool } from './types';

const updateTool = (ui: UIState, tool: Tool): UIState => {
  return { ...ui, selected_tool: tool };
};

*/
