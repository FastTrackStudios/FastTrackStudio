import React, { useState } from 'react';
import { useAppState, useActiveProject, useTransportState, useTransportControls } from '../../hooks/useAppState';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { invoke } from '@tauri-apps/api/core';

export const AppStateDebug: React.FC = () => {
  const { appState, loading, error, connected, refreshState } = useAppState({
    pollInterval: 1000,
    autoStart: true
  });

  const activeProject = useActiveProject(appState);
  const transport = useTransportState(appState);
  const transportControls = useTransportControls();

  const [newProjectName, setNewProjectName] = useState('Test Project');

  const handlePlay = async () => {
    console.log('🎵 Debug Panel: Play button clicked');
    console.log('🔍 Current state:', { appState: !!appState, activeProject, transport });
    try {
      console.log('📤 Invoking transport_play command...');
      const result = await invoke<string>('transport_play');
      console.log('✅ Direct transport_play result:', result);
      await refreshState();
    } catch (err) {
      console.error('❌ Direct transport_play failed:', err);
    }
  };

  const handlePause = async () => {
    console.log('⏸️ Debug Panel: Pause button clicked');
    try {
      console.log('📤 Invoking transport_pause command...');
      const result = await invoke<string>('transport_pause');
      console.log('✅ Direct transport_pause result:', result);
      await refreshState();
    } catch (err) {
      console.error('❌ Direct transport_pause failed:', err);
    }
  };

  const handleStop = async () => {
    console.log('⏹️ Debug Panel: Stop button clicked');
    try {
      console.log('📤 Invoking transport_stop command...');
      const result = await invoke<string>('transport_stop');
      console.log('✅ Direct transport_stop result:', result);
      await refreshState();
    } catch (err) {
      console.error('❌ Direct transport_stop failed:', err);
    }
  };

  const handlePlayPause = async () => {
    console.log('⏯️ Debug Panel: PlayPause button clicked');
    console.log('🔍 Pre-command state:', {
      hasActiveProject: !!activeProject,
      transportPlayState: transport?.play_state,
      isPlaying: transport?.play_state === 'Playing'
    });
    try {
      console.log('📤 Invoking transport_play_pause command directly...');
      const result = await invoke<string>('transport_play_pause');
      console.log('✅ Direct transport_play_pause result:', result);
      console.log('🔄 Refreshing state...');
      await refreshState();
      console.log('🔄 State refreshed');
    } catch (err) {
      console.error('❌ Direct transport_play_pause failed:', err);
      console.error('❌ Error details:', {
        error: err,
        errorType: typeof err,
        errorMessage: String(err)
      });
    }
  };

  const testAllTransportCommands = async () => {
    console.log('🧪 Testing all transport commands...');

    const commands = [
      'transport_play',
      'transport_pause',
      'transport_stop',
      'transport_play_pause',
      'transport_start_recording',
      'transport_stop_recording'
    ];

    for (const command of commands) {
      try {
        console.log(`📤 Testing ${command}...`);
        const result = await invoke<string>(command);
        console.log(`✅ ${command} result:`, result);
      } catch (err) {
        console.error(`❌ ${command} failed:`, err);
      }
    }

    await refreshState();
  };

  const handleRefresh = async () => {
    console.log('🔄 Debug Panel: Refresh button clicked');
    try {
      await refreshState();
      console.log('✅ State refreshed successfully');
    } catch (err) {
      console.error('❌ Refresh failed:', err);
    }
  };

  const handleCreateProject = async () => {
    console.log('🆕 Debug Panel: Creating project:', newProjectName);
    try {
      console.log('📤 Invoking create_project command...');
      const result = await invoke<string>('create_project', { name: newProjectName });
      console.log('✅ Create project result:', result);

      console.log('📤 Setting as active project...');
      const activeResult = await invoke<string>('set_active_project', { name: newProjectName });
      console.log('✅ Set active project result:', activeResult);

      await handleRefresh();
    } catch (err) {
      console.error('❌ Create project failed:', err);
      console.error('❌ Error details:', {
        error: err,
        errorType: typeof err,
        errorMessage: String(err)
      });
    }
  };

  const testAppState = async () => {
    console.log('🧪 Testing app state command...');
    try {
      console.log('📤 Invoking get_app_state command...');
      const result = await invoke('get_app_state');
      console.log('✅ Direct get_app_state result:', result);
    } catch (err) {
      console.error('❌ get_app_state failed:', err);
    }
  };

  const testBasicConnectivity = async () => {
    console.log('🔌 Testing basic Tauri connectivity...');
    try {
      console.log('📤 Invoking greet command...');
      const result = await invoke<string>('greet', { name: 'Debug Panel' });
      console.log('✅ Basic connectivity test passed:', result);
    } catch (err) {
      console.error('❌ Basic connectivity test failed:', err);
      console.error('❌ This indicates Tauri backend is not responding');
    }
  };

  const handleSetActiveProject = async (projectName: string) => {
    console.log('🎯 Setting active project:', projectName);
    try {
      const result = await invoke<string>('set_active_project', { name: projectName });
      console.log('✅ Set active project result:', result);
      await handleRefresh();
    } catch (err) {
      console.error('❌ Set active project failed:', err);
    }
  };

  return (
    <div className="space-y-4 p-4">
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center justify-between">
            App State Debug
            <div className="flex gap-2">
              <Badge variant={connected ? "default" : "destructive"}>
                {loading ? "Loading..." : connected ? "Connected" : "Disconnected"}
              </Badge>
              <Button size="sm" onClick={handleRefresh}>
                Refresh
              </Button>
            </div>
          </CardTitle>
        </CardHeader>
        <CardContent className="space-y-4">
          {error && (
            <div className="p-3 bg-red-100 border border-red-300 rounded text-red-800">
              <strong>Error:</strong> {error}
            </div>
          )}

          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            {/* App State Info */}
            <div className="space-y-2">
              <h3 className="font-semibold">App State</h3>
              <div className="text-sm space-y-1">
                <div><strong>Active Project:</strong> {appState?.active_project || 'None'}</div>
                <div><strong>Projects Count:</strong> {Object.keys(appState?.projects || {}).length}</div>
                <div><strong>App Version:</strong> {appState?.app_version || 'Unknown'}</div>
                <div><strong>Debug Mode:</strong> {appState?.debug_mode ? 'Yes' : 'No'}</div>
                <div><strong>Snapshot Time:</strong> {appState?.snapshot_timestamp || 'Unknown'}</div>
              </div>
            </div>

            {/* Active Project Info */}
            <div className="space-y-2">
              <h3 className="font-semibold">Active Project</h3>
              <div className="text-sm space-y-1">
                <div><strong>Name:</strong> {activeProject?.name || 'None'}</div>
                <div><strong>Is Active:</strong> {activeProject?.is_active ? 'Yes' : 'No'}</div>
                <div><strong>Has Transport:</strong> {activeProject?.transport ? 'Yes' : 'No'}</div>
              </div>
            </div>
          </div>

          {/* Transport State */}
          {transport && (
            <div className="space-y-2">
              <h3 className="font-semibold">Transport State</h3>
              <div className="grid grid-cols-2 md:grid-cols-3 gap-4 text-sm">
                <div><strong>Play State:</strong> {transport.play_state}</div>
                <div><strong>Record Mode:</strong> {transport.record_mode}</div>
                <div><strong>Looping:</strong> {transport.looping ? 'Yes' : 'No'}</div>
                <div><strong>Tempo:</strong> {transport.tempo?.bpm || 'Unknown'} BPM</div>
                <div><strong>Play Rate:</strong> {transport.playrate || 1.0}x</div>
                <div><strong>Time Sig:</strong> {transport.time_signature?.numerator || 4}/{transport.time_signature?.denominator || 4}</div>
              </div>

              <div className="space-y-2">
                <h4 className="font-medium">Position Info</h4>
                <div className="grid grid-cols-1 md:grid-cols-2 gap-4 text-sm">
                  <div>
                    <strong>Playhead (Time):</strong><br/>
                    {transport.playhead_position?.time?.seconds?.toFixed(2) || '0.00'}s
                  </div>
                  <div>
                    <strong>Playhead (Musical):</strong><br/>
                    {transport.playhead_position?.musical?.measure || 0}.{transport.playhead_position?.musical?.beat || 0}.{transport.playhead_position?.musical?.subdivision || 0}
                  </div>
                </div>
              </div>

              <div className="flex gap-2 flex-wrap">
                <Button size="sm" onClick={handlePlay} disabled={loading}>
                  Play
                </Button>
                <Button size="sm" onClick={handlePause} disabled={loading}>
                  Pause
                </Button>
                <Button size="sm" onClick={handleStop} disabled={loading}>
                  Stop
                </Button>
                <Button size="sm" onClick={handlePlayPause} disabled={loading}>
                  Play/Pause
                </Button>
              </div>
            </div>
          )}

          {/* Transport Controls Available */}
          <div className="space-y-2">
            <h3 className="font-semibold">Transport Controls Available</h3>
            <div className="text-sm">
              <div><strong>Controls Object:</strong> {transportControls ? 'Available' : 'Not Available'}</div>
              {transportControls && (
                <div className="mt-2 space-y-1">
                  <div><strong>play:</strong> {typeof transportControls.play}</div>
                  <div><strong>pause:</strong> {typeof transportControls.pause}</div>
                  <div><strong>stop:</strong> {typeof transportControls.stop}</div>
                  <div><strong>playPause:</strong> {typeof transportControls.playPause}</div>
                  <div><strong>setTempo:</strong> {typeof transportControls.setTempo}</div>
                  <div><strong>setPosition:</strong> {typeof transportControls.setPosition}</div>
                </div>
              )}
            </div>
          </div>

          {/* Project Management */}
          <div className="space-y-2">
            <h3 className="font-semibold">Project Management</h3>
            <div className="flex gap-2">
              <Input
                placeholder="Project name"
                value={newProjectName}
                onChange={(e) => setNewProjectName(e.target.value)}
                className="flex-1"
              />
              <Button size="sm" onClick={handleCreateProject} disabled={loading}>
                Create
              </Button>
            </div>
          </div>

          {/* Transport Command Testing */}
          <div className="space-y-2">
            <h3 className="font-semibold">Direct Transport Command Testing</h3>
            <div className="text-xs text-gray-600 mb-2">
              These buttons call Tauri commands directly to test backend connectivity
            </div>
            <div className="flex gap-2 flex-wrap">
              <Button size="sm" onClick={testBasicConnectivity} disabled={loading} variant="outline">
                🔌 Test Connectivity
              </Button>
              <Button size="sm" onClick={testAllTransportCommands} disabled={loading} variant="outline">
                🧪 Test All Commands
              </Button>
              <Button size="sm" onClick={testAppState} disabled={loading} variant="outline">
                🔍 Test App State
              </Button>
            </div>
            <div className="grid grid-cols-2 gap-2 text-xs">
              <Button size="sm" onClick={handlePlay} disabled={loading}>
                ▶️ Direct Play
              </Button>
              <Button size="sm" onClick={handlePause} disabled={loading}>
                ⏸️ Direct Pause
              </Button>
              <Button size="sm" onClick={handleStop} disabled={loading}>
                ⏹️ Direct Stop
              </Button>
              <Button size="sm" onClick={handlePlayPause} disabled={loading}>
                ⏯️ Direct Play/Pause
              </Button>
            </div>
            <div className="text-xs text-gray-500">
              Check browser console (F12) for detailed logs
            </div>
          </div>

          {/* Projects List */}
          {appState?.projects && Object.keys(appState.projects).length > 0 && (
            <div className="space-y-2">
              <h3 className="font-semibold">Available Projects</h3>
              <div className="text-sm space-y-1">
                {Object.entries(appState.projects).map(([name, project]) => (
                  <div key={name} className="flex justify-between items-center p-2 bg-gray-50 rounded">
                    <span><strong>{name}</strong> {project.is_active && '(Active)'}</span>
                    <div className="flex gap-2 items-center">
                      <Badge variant={project.is_active ? "default" : "outline"}>
                        {project.transport?.play_state || 'Unknown'}
                      </Badge>
                      {!project.is_active && (
                        <Button
                          size="sm"
                          variant="outline"
                          onClick={() => handleSetActiveProject(name)}
                          disabled={loading}
                        >
                          Set Active
                        </Button>
                      )}
                    </div>
                  </div>
                ))}
              </div>
            </div>
          )}

          {/* Raw Data */}
          <div className="space-y-2">
            <h3 className="font-semibold">Raw Data (JSON)</h3>
            <details className="text-xs">
              <summary className="cursor-pointer">Click to expand raw app state</summary>
              <pre className="mt-2 p-2 bg-gray-100 rounded overflow-auto max-h-64">
                {JSON.stringify(appState, null, 2)}
              </pre>
            </details>
          </div>
        </CardContent>
      </Card>
    </div>
  );
};
