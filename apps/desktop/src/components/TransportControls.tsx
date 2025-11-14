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

interface TransportControlsProps {
  className?: string;
}

export function TransportControls({ className = '' }: TransportControlsProps) {
  const projectManager = useProjectManager({
    autoLoadProjects: true,
    pollInterval: 1000,
  });

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
              onClick={projectManager.stop}
              disabled={!projectManager.activeProject || projectManager.transportLoading}
              variant="outline"
              size="lg"
            >
              <Square className="w-4 h-4 mr-2" />
              Stop
            </Button>

            <Button
              onClick={projectManager.play}
              disabled={!projectManager.activeProject || projectManager.isPlaying || projectManager.transportLoading}
              variant={projectManager.isPlaying ? "default" : "outline"}
              size="lg"
            >
              <Play className="w-4 h-4 mr-2" />
              Play
            </Button>

            <Button
              onClick={projectManager.pause}
              disabled={!projectManager.activeProject || !projectManager.isPlaying || projectManager.transportLoading}
              variant="outline"
              size="lg"
            >
              <Pause className="w-4 h-4 mr-2" />
              Pause
            </Button>

            <Button
              onClick={projectManager.playPause}
              disabled={!projectManager.activeProject || projectManager.transportLoading}
              variant="outline"
              size="lg"
            >
              {projectManager.isPlaying ? (
                <Pause className="w-4 h-4 mr-2" />
              ) : (
                <Play className="w-4 h-4 mr-2" />
              )}
              Play/Pause
            </Button>

            <Separator orientation="vertical" className="h-10" />

            {/* Recording Controls */}
            <Button
              onClick={projectManager.startRecording}
              disabled={!projectManager.activeProject || projectManager.isRecording || projectManager.transportLoading}
              variant={projectManager.isRecording ? "destructive" : "outline"}
              size="lg"
            >
              <Circle className={`w-4 h-4 mr-2 ${projectManager.isRecording ? 'fill-current' : ''}`} />
              Record
            </Button>

            <Button
              onClick={projectManager.stopRecording}
              disabled={!projectManager.activeProject || !projectManager.isRecording || projectManager.transportLoading}
              variant="outline"
              size="lg"
            >
              <Square className="w-4 h-4 mr-2" />
              Stop Rec
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
