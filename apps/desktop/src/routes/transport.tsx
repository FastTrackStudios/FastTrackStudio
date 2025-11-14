import { createFileRoute } from '@tanstack/react-router';
import React, { useEffect, useState } from 'react';
import { Button } from '@/components/ui/button';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Play, Square, Music, Clock } from 'lucide-react';
import { transport, projectManager } from '@/lib/tauri-api';
import type { Transport } from '@/lib/types';

export const Route = createFileRoute('/transport')({
  component: TransportPage,
});

function TransportPage() {
  const [transportState, setTransportState] = useState<Transport | null>(null);
  const [activeProject, setActiveProject] = useState<string | null>(null);
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [tempoInput, setTempoInput] = useState<string>('');
  const [numeratorInput, setNumeratorInput] = useState<string>('');
  const [denominatorInput, setDenominatorInput] = useState<string>('');

  // Format position for display
  const formatPosition = (timePosition?: { minutes: number, seconds: number, milliseconds: number }): string => {
    if (!timePosition) return '00:00.000';
    const totalSeconds = timePosition.seconds + (timePosition.milliseconds / 1000);
    return `${timePosition.minutes.toString().padStart(2, '0')}:${totalSeconds.toFixed(3).padStart(6, '0')}`;
  };

  // Load initial data and start polling
  useEffect(() => {
    loadInitialData();
    const interval = setInterval(pollTransportState, 100); // Poll every 100ms for smooth updates
    return () => clearInterval(interval);
  }, []);

  const loadInitialData = async () => {
    try {
      setError(null);
      const activeProj = await projectManager.get_active_project();
      setActiveProject(activeProj);

      if (activeProj) {
        await pollTransportState();
      }
    } catch (err) {
      console.error('Failed to load initial data:', err);
      setError('Failed to load project data');
    }
  };

  // Initialize input fields only once when transport state first loads
  React.useEffect(() => {
    if (transportState && tempoInput === '' && numeratorInput === '' && denominatorInput === '') {
      setTempoInput(transportState.tempo.bpm.toString());
      setNumeratorInput(transportState.time_signature.numerator.toString());
      setDenominatorInput(transportState.time_signature.denominator.toString());
    }
  }, [transportState, tempoInput, numeratorInput, denominatorInput]);

  const pollTransportState = async () => {
    try {
      console.log('📊 [FRONTEND] Calling transport.get_state()...');
      const state = await transport.get_state();
      console.log('📊 [FRONTEND] Received transport state:', {
        play_state: state?.play_state,
        position: state?.position?.time,
        tempo: state?.tempo?.bpm,
        time_signature: state?.time_signature
      });
      setTransportState(state);
      setError(null);
    } catch (err) {
      console.error('❌ [FRONTEND] Failed to get transport state:', err);
      // Don't set error for polling failures to avoid UI flicker
    }
  };

  const handleTogglePlay = async () => {
    console.log('🖱️ [FRONTEND] handleTogglePlay called');
    console.log('🖱️ [FRONTEND] Active project:', activeProject);
    console.log('🖱️ [FRONTEND] Current play state:', transportState?.play_state);

    if (!activeProject) {
      console.log('❌ [FRONTEND] No active project - aborting');
      setError('No active project');
      return;
    }

    setIsLoading(true);
    try {
      setError(null);

      if (transportState?.play_state === 'Playing') {
        console.log('⏹️ [FRONTEND] Calling transport.stop()...');
        const result = await transport.stop();
        console.log('⏹️ [FRONTEND] transport.stop() result:', result);
      } else {
        console.log('🎵 [FRONTEND] Calling transport.play()...');
        const result = await transport.play();
        console.log('🎵 [FRONTEND] transport.play() result:', result);
      }

      // Immediately poll for updated state
      console.log('🔄 [FRONTEND] Polling for updated state...');
      await pollTransportState();
    } catch (err) {
      console.error('❌ [FRONTEND] Transport operation failed:', err);
      setError('Transport operation failed');
    } finally {
      setIsLoading(false);
    }
  };

  const handleTempoChange = async () => {
    if (!activeProject) {
      setError('No active project');
      return;
    }

    const bpm = parseFloat(tempoInput);
    if (isNaN(bpm) || bpm < 20 || bpm > 300) {
      setError('Tempo must be between 20 and 300 BPM');
      return;
    }

    setIsLoading(true);
    try {
      setError(null);
      console.log('🎵 [FRONTEND] Calling transport.set_tempo with BPM:', bpm);
      const result = await transport.set_tempo(bpm);
      console.log('🎵 [FRONTEND] set_tempo result:', result);
      await pollTransportState();
    } catch (err) {
      console.error('❌ [FRONTEND] Failed to set tempo:', err);
      setError('Failed to set tempo');
    } finally {
      setIsLoading(false);
    }
  };

  const handleTimeSignatureChange = async () => {
    if (!activeProject) {
      setError('No active project');
      return;
    }

    const numerator = parseInt(numeratorInput);
    const denominator = parseInt(denominatorInput);

    if (isNaN(numerator) || isNaN(denominator) || numerator < 1 || numerator > 32 || ![1, 2, 4, 8, 16, 32].includes(denominator)) {
      setError('Invalid time signature');
      return;
    }

    setIsLoading(true);
    try {
      setError(null);
      console.log('🎵 [FRONTEND] Calling transport.set_time_signature:', numerator, '/', denominator);
      const result = await transport.set_time_signature(numerator, denominator);
      console.log('🎵 [FRONTEND] set_time_signature result:', result);
      await pollTransportState();
    } catch (err) {
      console.error('❌ [FRONTEND] Failed to set time signature:', err);
      setError('Failed to set time signature');
    } finally {
      setIsLoading(false);
    }
  };

  const isPlaying = transportState?.play_state === 'Playing';
  const currentPosition = formatPosition(transportState?.playhead_position?.time);

  return (
    <div className="min-h-screen bg-background flex items-center justify-center p-6">
      <div className="w-full max-w-2xl space-y-6">
        {/* Main Transport Card */}
        <Card>
          <CardHeader className="text-center">
            <CardTitle className="text-2xl">FastTrack Studio Transport</CardTitle>
            <p className="text-muted-foreground">{activeProject || 'No Project'}</p>
          </CardHeader>
          <CardContent className="space-y-6">
            {/* Error Display */}
            {error && (
              <div className="p-3 text-sm text-destructive bg-destructive/10 border border-destructive/20 rounded">
                {error}
              </div>
            )}

            {/* Position Display */}
            <div className="text-center">
              <p className="text-4xl font-mono mb-2">{currentPosition}</p>
              <p className="text-sm text-muted-foreground">
                {isPlaying ? 'Playing' : 'Stopped'}
              </p>
              {transportState && (
                <div className="text-xs text-muted-foreground mt-1">
                  {transportState.tempo.bpm} BPM • {transportState.time_signature.numerator}/{transportState.time_signature.denominator}
                </div>
              )}
            </div>

            {/* Transport Button */}
            <div className="flex justify-center">
              <Button
                size="lg"
                variant={isPlaying ? "destructive" : "default"}
                disabled={isLoading || !activeProject}
                onClick={handleTogglePlay}
                className="w-20 h-20 rounded-full"
              >
                {isPlaying ? (
                  <Square className="h-8 w-8" />
                ) : (
                  <Play className="h-8 w-8" />
                )}
              </Button>
            </div>

            {/* Status */}
            <div className="text-center text-sm text-muted-foreground">
              {!activeProject ? (
                'No active project loaded'
              ) : isLoading ? (
                'Loading...'
              ) : (
                `Ready • ${transportState?.play_state || 'Unknown'}`
              )}
            </div>
          </CardContent>
        </Card>

        {/* Status Display */}
        <Card>
          <CardHeader>
            <CardTitle className="text-lg">Transport Status</CardTitle>
          </CardHeader>
          <CardContent>
            <div className="grid grid-cols-2 md:grid-cols-4 gap-4 text-center">
              <div>
                <p className="text-sm font-medium text-muted-foreground">Tempo</p>
                <p className="text-2xl font-mono">{transportState?.tempo.bpm || '--'}</p>
                <p className="text-xs text-muted-foreground">BPM</p>
              </div>
              <div>
                <p className="text-sm font-medium text-muted-foreground">Time Signature</p>
                <p className="text-2xl font-mono">
                  {transportState?.time_signature.numerator || '--'}/{transportState?.time_signature.denominator || '--'}
                </p>
                <p className="text-xs text-muted-foreground">Beats/Note</p>
              </div>
              <div>
                <p className="text-sm font-medium text-muted-foreground">State</p>
                <p className="text-2xl font-mono">{transportState?.play_state || '--'}</p>
                <p className="text-xs text-muted-foreground">Mode</p>
              </div>
              <div>
                <p className="text-sm font-medium text-muted-foreground">Playrate</p>
                <p className="text-2xl font-mono">{transportState?.playrate || '--'}x</p>
                <p className="text-xs text-muted-foreground">Speed</p>
              </div>
            </div>
          </CardContent>
        </Card>

        {/* Control Cards */}
        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
          {/* Tempo Control */}
          <Card>
            <CardHeader className="flex flex-row items-center space-y-0 pb-2">
              <CardTitle className="text-lg flex items-center gap-2">
                <Clock className="h-5 w-5" />
                Set Tempo
              </CardTitle>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="flex items-end gap-2">
                <div className="flex-1">
                  <Label htmlFor="tempo">BPM (20-300)</Label>
                  <Input
                    id="tempo"
                    type="number"
                    min="20"
                    max="300"
                    value={tempoInput}
                    onChange={(e) => setTempoInput(e.target.value)}
                    onKeyDown={(e) => e.key === 'Enter' && handleTempoChange()}
                    disabled={!activeProject || isLoading}
                    className="text-center font-mono"
                    placeholder="120"
                  />
                </div>
                <Button
                  onClick={handleTempoChange}
                  disabled={!activeProject || isLoading || !tempoInput}
                  size="sm"
                >
                  Set
                </Button>
              </div>
            </CardContent>
          </Card>

          {/* Time Signature Control */}
          <Card>
            <CardHeader className="flex flex-row items-center space-y-0 pb-2">
              <CardTitle className="text-lg flex items-center gap-2">
                <Music className="h-5 w-5" />
                Set Time Signature
              </CardTitle>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="flex items-end gap-2">
                <div className="flex-1">
                  <Label htmlFor="numerator">Beats (1-32)</Label>
                  <Input
                    id="numerator"
                    type="number"
                    min="1"
                    max="32"
                    value={numeratorInput}
                    onChange={(e) => setNumeratorInput(e.target.value)}
                    disabled={!activeProject || isLoading}
                    className="text-center font-mono"
                    placeholder="4"
                  />
                </div>
                <div className="text-2xl font-bold text-muted-foreground">/</div>
                <div className="flex-1">
                  <Label htmlFor="denominator">Note Value</Label>
                  <Input
                    id="denominator"
                    type="number"
                    value={denominatorInput}
                    onChange={(e) => setDenominatorInput(e.target.value)}
                    disabled={!activeProject || isLoading}
                    className="text-center font-mono"
                    placeholder="4"
                  />
                </div>
                <Button
                  onClick={handleTimeSignatureChange}
                  disabled={!activeProject || isLoading || !numeratorInput || !denominatorInput}
                  size="sm"
                >
                  Set
                </Button>
              </div>
            </CardContent>
          </Card>
        </div>
      </div>
    </div>
  );
}
