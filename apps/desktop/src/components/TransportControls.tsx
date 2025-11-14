import React, { useState } from 'react';
import { Button } from '@/components/ui/button';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Separator } from '@/components/ui/separator';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import {
  Play,
  Pause,
  Square,
  Circle,
  Music,
  Clock,
  FolderOpen,
  Plus
} from 'lucide-react';
import { useProjectManager } from '@/hooks/useProjectManager';
import { api } from '@/lib/tauri-api';

interface TransportControlsProps {
  className?: string;
}

export function TransportControls({ className = '' }: TransportControlsProps) {
  const projectManager = useProjectManager({
    autoLoadProjects: true,
    pollInterval: 1000,
  });

  // Debug logging for TransportControls
  React.useEffect(() => {
    console.log('🎛️ TransportControls - Component mounted/updated');
    console.log('🎛️ TransportControls - projectManager state:', {
      projects: projectManager.projects,
      activeProject: projectManager.activeProject,
      projectsLoading: projectManager.projectsLoading,
      transportLoading: projectManager.transportLoading,
      error: projectManager.error,
      isPlaying: projectManager.isPlaying,
      isRecording: projectManager.isRecording,
      currentTempo: projectManager.currentTempo,
      currentPosition: projectManager.currentPosition,
      transportState: projectManager.transportState
    });
  }, [projectManager]);

  // Debug logging for button state specifically
  React.useEffect(() => {
    console.log('🎛️ TransportControls - Button state debug:', {
      activeProject: projectManager.activeProject,
      activeProjectType: typeof projectManager.activeProject,
      activeProjectTruthy: !!projectManager.activeProject,
      transportLoading: projectManager.transportLoading,
      buttonsShouldBeDisabled: !projectManager.activeProject || projectManager.transportLoading,
      projects: projectManager.projects,
      projectsLength: projectManager.projects?.length,
      error: projectManager.error
    });
  }, [projectManager.activeProject, projectManager.transportLoading, projectManager.projects, projectManager.error]);

  // Direct API test functions
  const [apiTestResults, setApiTestResults] = useState<string[]>([]);

  const addTestResult = (result: string) => {
    setApiTestResults(prev => [...prev.slice(-10), result]); // Keep last 11 results
  };

  const testDirectAPI = async () => {
    addTestResult('🧪 Testing direct API calls...');
    try {
      const projects = await api.projects.list_projects();
      addTestResult(`✅ Projects: ${JSON.stringify(projects)}`);

      const activeProject = await api.projects.get_active_project();
      addTestResult(`✅ Active Project: ${activeProject || 'null'}`);

      if (activeProject) {
        const transportState = await api.transport.get_state();
        addTestResult(`✅ Transport State: play_state=${transportState?.play_state}, tempo=${transportState?.tempo?.bpm}`);
      }
    } catch (error) {
      addTestResult(`❌ API Error: ${error}`);
    }
  };

  const testGreetCommand = async () => {
    addTestResult('🧪 Testing greet command...');
    try {
      const result = await api.greet('Frontend Test');
      addTestResult(`✅ Greet result: ${result}`);
    } catch (error) {
      addTestResultconsole.error('🧪 Greet error:', error);
    }
  };

  const testTransportObject = async () => {
    console.log('🧪 Testing transport object...');
    setApiTestResults(prev => [...prev, '🧪 Testing transport object...']);
    try {
      const result = await api.test_transport_object();
      console.log('🧪 Transport object result:', result);
      setApiTestResults(prev => [...prev, `Transport object: ${JSON.stringify(result, null, 2)}`]);
    } catch (error) {
      console.error('🧪 Transport object error:', error);
      setApiTestResults(prev => [...prev, `Transport object error: ${error}`]);
    }
  };

  const testManualStateUpdate = async () => {
    console.log('🧪 Testing manual state update...');
    setApiTestResults(prev => [...prev, '🧪 Testing manual state update...']);
    try {
      const projects = await api.projects.list_projects();
      const activeProject = await api.projects.get_active_project();
      console.log('🧪 Manual test - projects:', projects);
      console.log('🧪 Manual test - activeProject:', activeProject);
      console.log('🧪 Manual test - Expected activeProject to be:', 'Default Project');
      console.log('🧪 Manual test - Are they equal?', activeProject === 'Default Project');

      setApiTestResults(prev => [...prev,
        `Manual API projects: ${JSON.stringify(projects)}`,
        `Manual API activeProject: ${JSON.stringify(activeProject)}`,
        `Expected: "Default Project"`,
        `Match: ${activeProject === 'Default Project'}`,
        `Current hook activeProject: ${JSON.stringify(projectManager.activeProject)}`,
        `Hook vs API match: ${projectManager.activeProject === activeProject}`
      ]);
    } catch (error) {
      console.error('🧪 Manual state test error:', error);
      setApiTestResults(prev => [...prev, `Manual state test error: ${error}`]);
    }
  };

  const [newProjectName, setNewProjectName] = useState('');
  const [tempoInput, setTempoInput] = useState('');
  const [positionInput, setPositionInput] = useState('');
  const [timeSigNum, setTimeSigNum] = useState('4');
  const [timeSigDen, setTimeSigDen] = useState('4');
  const [showCreateProject, setShowCreateProject] = useState(false);

  // Update input fields when state changes
  React.useEffect(() => {
    if (projectManager.currentTempo) {
      setTempoInput(projectManager.currentTempo.toString());
    }
  }, [projectManager.currentTempo]);

  React.useEffect(() => {
    if (projectManager.currentPosition !== undefined) {
      setPositionInput(projectManager.currentPosition.toFixed(2));
    }
  }, [projectManager.currentPosition]);

  React.useEffect(() => {
    if (projectManager.timeSignature) {
      setTimeSigNum(projectManager.timeSignature.numerator.toString());
      setTimeSigDen(projectManager.timeSignature.denominator.toString());
    }
  }, [projectManager.timeSignature]);

  const handleCreateProject = async () => {
    if (newProjectName.trim()) {
      try {
        await projectManager.createProject(newProjectName.trim());
        setNewProjectName('');
        setShowCreateProject(false);
      } catch (error) {
        console.error('Failed to create project:', error);
      }
    }
  };

  const handleTempoSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    const bpm = parseFloat(tempoInput);
    if (bpm > 0 && bpm <= 300) {
      try {
        await projectManager.setTempo(bpm);
      } catch (error) {
        console.error('Failed to set tempo:', error);
      }
    }
  };

  const handlePositionSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    const seconds = parseFloat(positionInput);
    if (seconds >= 0) {
      try {
        await projectManager.setPosition(seconds);
      } catch (error) {
        console.error('Failed to set position:', error);
      }
    }
  };

  const handleTimeSignatureSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    const num = parseInt(timeSigNum);
    const den = parseInt(timeSigDen);
    if (num > 0 && den > 0) {
      try {
        await projectManager.setTimeSignature(num, den);
      } catch (error) {
        console.error('Failed to set time signature:', error);
      }
    }
  };

  const formatTime = (seconds: number) => {
    const mins = Math.floor(seconds / 60);
    const secs = (seconds % 60).toFixed(2);
    return `${mins}:${secs.padStart(5, '0')}`;
  };

  const getPlayStateDisplay = () => {
    if (projectManager.isRecording) {
      return { text: 'Recording', color: 'bg-red-500' };
    } else if (projectManager.isPlaying) {
      return { text: 'Playing', color: 'bg-green-500' };
    } else {
      return { text: 'Stopped', color: 'bg-gray-500' };
    }
  };

  return (
    <div className={`space-y-6 ${className}`}>
      {/* Project Management */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <FolderOpen className="w-5 h-5" />
            Project Management
          </CardTitle>
        </CardHeader>
        <CardContent>
          <div className="space-y-4">
            <div className="flex items-center gap-4">
              <Label className="text-sm font-medium">Active Project:</Label>
              <Select
                value={projectManager.activeProject || ''}
                onValueChange={projectManager.setActiveProject}
                disabled={projectManager.projectsLoading}
              >
                <SelectTrigger className="w-64">
                  <SelectValue placeholder="Select a project" />
                </SelectTrigger>
                <SelectContent>
                  {projectManager.projects.map((project) => (
                    <SelectItem key={project} value={project}>
                      {project}
                    </SelectItem>
                  ))}
                </SelectContent>
              </Select>
              <Button
                onClick={() => setShowCreateProject(!showCreateProject)}
                variant="outline"
                size="sm"
              >
                <Plus className="w-4 h-4 mr-2" />
                New Project
              </Button>
            </div>

            {showCreateProject && (
              <div className="flex items-center gap-2 p-4 border rounded-lg bg-muted">
                <Input
                  placeholder="Project name"
                  value={newProjectName}
                  onChange={(e) => setNewProjectName(e.target.value)}
                  onKeyDown={(e) => e.key === 'Enter' && handleCreateProject()}
                />
                <Button onClick={handleCreateProject} size="sm">
                  Create
                </Button>
                <Button
                  onClick={() => setShowCreateProject(false)}
                  variant="outline"
                  size="sm"
                >
                  Cancel
                </Button>
              </div>
            )}

            {projectManager.error && (
              <div className="p-3 bg-red-50 border border-red-200 rounded text-sm text-red-700">
                {projectManager.error}
              </div>
            )}
          </div>
        </CardContent>
      </Card>

      {/* Real-time State Display */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            🔍 Real-time State Display
          </CardTitle>
        </CardHeader>
        <CardContent>
          <div className="grid grid-cols-2 gap-4 text-sm">
            <div>
              <strong>Projects Array:</strong>
              <pre className="text-xs bg-gray-100 p-2 rounded mt-1">
                {JSON.stringify(projectManager.projects, null, 2)}
              </pre>
            </div>
            <div>
              <strong>Active Project:</strong>
              <pre className="text-xs bg-gray-100 p-2 rounded mt-1">
                Value: {JSON.stringify(projectManager.activeProject)}
                Type: {typeof projectManager.activeProject}
                Truthy: {String(!!projectManager.activeProject)}
                Length: {projectManager.activeProject?.length || 'N/A'}
              </pre>
            </div>
            <div>
              <strong>Button State:</strong>
              <pre className="text-xs bg-gray-100 p-2 rounded mt-1">
                Should be disabled: {String(!projectManager.activeProject || projectManager.transportLoading)}
                No active project: {String(!projectManager.activeProject)}
                Transport loading: {String(projectManager.transportLoading)}
              </pre>
            </div>
            <div>
              <strong>Loading States:</strong>
              <pre className="text-xs bg-gray-100 p-2 rounded mt-1">
                Projects Loading: {String(projectManager.projectsLoading)}
                Transport Loading: {String(projectManager.transportLoading)}
                Error: {projectManager.error || 'None'}
              </pre>
            </div>
          </div>
        </CardContent>
      </Card>

      {/* API Debug Testing */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            🧪 API Debug Testing
          </CardTitle>
        </CardHeader>
        <CardContent>
          <div className="flex gap-2 flex-wrap">
            <Button onClick={testGreetCommand} variant="outline" size="sm">
              Test Greet Command
            </Button>
            <Button onClick={testDirectAPI} variant="outline" size="sm">
              Test Direct API Calls
            </Button>
            <Button onClick={() => projectManager.refreshProjects()} variant="outline" size="sm">
              Refresh Projects
            </Button>
            <Button onClick={() => projectManager.refreshTransportState()} variant="outline" size="sm">
              Refresh Transport State
            </Button>
            <Button onClick={testTransportObject} variant="outline" size="sm">
              Test Transport Object
            </Button>
            <Button onClick={testManualStateUpdate} variant="outline" size="sm">
              Manual State Test
            </Button>
            <Button onClick={() => setApiTestResults([])} variant="outline" size="sm">
              Clear Results
            </Button>
          </div>
          {apiTestResults.length > 0 && (
            <div className="mt-4">
              <strong>Test Results:</strong>
              <pre className="text-xs bg-gray-100 p-2 rounded mt-1 max-h-32 overflow-auto">
                {apiTestResults.join('\n')}
              </pre>
            </div>
          )}
        </CardContent>
      </Card>

      {/* Transport State Display */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Music className="w-5 h-5" />
            Transport State
            {projectManager.activeProject && (
              <Badge variant="outline">{projectManager.activeProject}</Badge>
            )}
          </CardTitle>
        </CardHeader>
        <CardContent>
          <div className="grid grid-cols-2 gap-4">
            <div>
              <Label className="text-sm text-muted-foreground">Play State</Label>
              <div className="flex items-center gap-2 mt-1">
                <div
                  className={`w-3 h-3 rounded-full ${getPlayStateDisplay().color}`}
                />
                <span className="text-sm font-medium">
                  {getPlayStateDisplay().text}
                </span>
              </div>
            </div>
            <div>
              <Label className="text-sm text-muted-foreground">Position</Label>
              <p className="text-lg font-mono mt-1">
                {formatTime(projectManager.currentPosition)}
              </p>
            </div>
            <div>
              <Label className="text-sm text-muted-foreground">Tempo</Label>
              <p className="text-lg font-mono mt-1">
                {projectManager.currentTempo.toFixed(1)} BPM
              </p>
            </div>
            <div>
              <Label className="text-sm text-muted-foreground">Time Signature</Label>
              <p className="text-lg font-mono mt-1">
                {projectManager.timeSignature.numerator}/{projectManager.timeSignature.denominator}
              </p>
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Main Transport Controls */}
      <Card>
        <CardHeader>
          <CardTitle>Transport Controls</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="flex flex-wrap gap-2 justify-center">
            {/* Playback Controls */}
            <Button
              onClick={() => {
                console.log('🎛️ Stop button clicked, activeProject:', projectManager.activeProject);
                projectManager.stop();
              }}
              disabled={!projectManager.activeProject || projectManager.transportLoading}
              variant="outline"
              size="lg"
              title={`Disabled: ${!projectManager.activeProject ? 'No active project' : projectManager.transportLoading ? 'Transport loading' : 'Ready'}`}
            >
              <Square className="w-4 h-4 mr-2" />
              Stop {!projectManager.activeProject && '(No Project)'}
            </Button>

            <Button
              onClick={() => {
                console.log('🎛️ Play button clicked, activeProject:', projectManager.activeProject);
                projectManager.play();
              }}
              disabled={!projectManager.activeProject || projectManager.isPlaying || projectManager.transportLoading}
              variant={projectManager.isPlaying ? "default" : "outline"}
              size="lg"
              title={`Disabled: ${!projectManager.activeProject ? 'No active project' : projectManager.isPlaying ? 'Already playing' : projectManager.transportLoading ? 'Transport loading' : 'Ready'}`}
            >
              <Play className="w-4 h-4 mr-2" />
              Play {!projectManager.activeProject && '(No Project)'}
            </Button>

            <Button
              onClick={() => {
                console.log('🎛️ Pause button clicked, activeProject:', projectManager.activeProject);
                projectManager.pause();
              }}
              disabled={!projectManager.activeProject || !projectManager.isPlaying || projectManager.transportLoading}
              variant="outline"
              size="lg"
              title={`Disabled: ${!projectManager.activeProject ? 'No active project' : !projectManager.isPlaying ? 'Not playing' : projectManager.transportLoading ? 'Transport loading' : 'Ready'}`}
            >
              <Pause className="w-4 h-4 mr-2" />
              Pause {!projectManager.activeProject && '(No Project)'}
            </Button>

            <Button
              onClick={() => {
                console.log('🎛️ Play/Pause button clicked, activeProject:', projectManager.activeProject);
                projectManager.playPause();
              }}
              disabled={!projectManager.activeProject || projectManager.transportLoading}
              variant="outline"
              size="lg"
              title={`Disabled: ${!projectManager.activeProject ? 'No active project' : projectManager.transportLoading ? 'Transport loading' : 'Ready'}`}
            >
              {projectManager.isPlaying ? (
                <Pause className="w-4 h-4 mr-2" />
              ) : (
                <Play className="w-4 h-4 mr-2" />
              )}
              Play/Pause {!projectManager.activeProject && '(No Project)'}
            </Button>

            <Separator orientation="vertical" className="h-10" />

            {/* Recording Controls */}
            <Button
              onClick={() => {
                console.log('🎛️ Record button clicked, activeProject:', projectManager.activeProject);
                projectManager.startRecording();
              }}
              disabled={!projectManager.activeProject || projectManager.isRecording || projectManager.transportLoading}
              variant={projectManager.isRecording ? "destructive" : "outline"}
              size="lg"
              title={`Disabled: ${!projectManager.activeProject ? 'No active project' : projectManager.isRecording ? 'Already recording' : projectManager.transportLoading ? 'Transport loading' : 'Ready'}`}
            >
              <Circle className={`w-4 h-4 mr-2 ${projectManager.isRecording ? 'fill-current' : ''}`} />
              Record {!projectManager.activeProject && '(No Project)'}
            </Button>

            <Button
              onClick={() => {
                console.log('🎛️ Stop recording button clicked, activeProject:', projectManager.activeProject);
                projectManager.stopRecording();
              }}
              disabled={!projectManager.activeProject || !projectManager.isRecording || projectManager.transportLoading}
              variant="outline"
              size="lg"
              title={`Disabled: ${!projectManager.activeProject ? 'No active project' : !projectManager.isRecording ? 'Not recording' : projectManager.transportLoading ? 'Transport loading' : 'Ready'}`}
            >
              <Square className="w-4 h-4 mr-2" />
              Stop Rec {!projectManager.activeProject && '(No Project)'}
            </Button>
          </div>
        </CardContent>
      </Card>

      {/* Configuration Controls */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Clock className="w-5 h-5" />
            Configuration
          </CardTitle>
        </CardHeader>
        <CardContent>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
            {/* Tempo Control */}
            <form onSubmit={handleTempoSubmit} className="space-y-2">
              <Label htmlFor="tempo">Tempo (BPM)</Label>
              <div className="flex gap-2">
                <Input
                  id="tempo"
                  type="number"
                  value={tempoInput}
                  onChange={(e) => setTempoInput(e.target.value)}
                  placeholder="120.0"
                  min="20"
                  max="300"
                  step="0.1"
                />
                <Button
                  type="submit"
                  size="sm"
                  disabled={!projectManager.activeProject || projectManager.transportLoading}
                >
                  Set
                </Button>
              </div>
            </form>

            {/* Position Control */}
            <form onSubmit={handlePositionSubmit} className="space-y-2">
              <Label htmlFor="position">Position (seconds)</Label>
              <div className="flex gap-2">
                <Input
                  id="position"
                  type="number"
                  value={positionInput}
                  onChange={(e) => setPositionInput(e.target.value)}
                  placeholder="0.00"
                  min="0"
                  step="0.01"
                />
                <Button
                  type="submit"
                  size="sm"
                  disabled={!projectManager.activeProject || projectManager.transportLoading}
                >
                  Seek
                </Button>
              </div>
            </form>

            {/* Time Signature Control */}
            <form onSubmit={handleTimeSignatureSubmit} className="space-y-2">
              <Label>Time Signature</Label>
              <div className="flex gap-2">
                <Input
                  type="number"
                  value={timeSigNum}
                  onChange={(e) => setTimeSigNum(e.target.value)}
                  placeholder="4"
                  min="1"
                  max="32"
                  className="w-16"
                />
                <span className="self-center">/</span>
                <Input
                  type="number"
                  value={timeSigDen}
                  onChange={(e) => setTimeSigDen(e.target.value)}
                  placeholder="4"
                  min="1"
                  max="32"
                  className="w-16"
                />
                <Button
                  type="submit"
                  size="sm"
                  disabled={!projectManager.activeProject || projectManager.transportLoading}
                >
                  Set
                </Button>
              </div>
            </form>
          </div>

          <Separator className="my-4" />

          {/* Action buttons */}
          <div className="flex items-center justify-between">
            <Button
              onClick={projectManager.refreshTransportState}
              variant="outline"
              size="sm"
              disabled={!projectManager.activeProject || projectManager.transportLoading}
            >
              Refresh State
            </Button>
            <Button
              onClick={() => projectManager.setPosition(0)}
              variant="outline"
              size="sm"
              disabled={!projectManager.activeProject || projectManager.transportLoading}
            >
              Go to Start
            </Button>
          </div>
        </CardContent>
      </Card>

      {/* Debug Info (Development) */}
      {process.env.NODE_ENV === 'development' && (
        <Card>
          <CardHeader>
            <CardTitle>Debug Info</CardTitle>
          </CardHeader>
          <CardContent>
            <details className="cursor-pointer">
              <summary className="font-medium mb-2">Transport State</summary>
              <pre className="text-xs bg-gray-50 p-2 rounded overflow-auto">
                {JSON.stringify(projectManager.transportState, null, 2)}
              </pre>
            </details>
          </CardContent>
        </Card>
      )}
    </div>
  );
}
