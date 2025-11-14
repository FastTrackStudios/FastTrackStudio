import React, { useState } from "react";
import {
  ChevronRight,
  Play,
  Pause,
  Square,
  SkipBack,
  RotateCcw,
  CircleDot,
  Settings,
  Activity,
  Clock,
} from "lucide-react";
import { cn } from "@/lib/utils";
import { useAppState, useTransportState, useTransportControls } from "../../../../../hooks/useAppState";
import type { PlayState } from "../../../../../bindings";

interface EnhancedStatusBarProps {
  className?: string;
}

interface TransportButtonProps {
  children: React.ReactNode;
  className?: string;
  onClick?: () => void;
  isActive?: boolean;
  activeBackgroundColor?: string;
  disabled?: boolean;
}

const TransportButton: React.FC<TransportButtonProps> = ({
  children,
  className,
  onClick,
  isActive = false,
  activeBackgroundColor = "var(--ableset-color-default-500)",
  disabled = false,
}) => {
  return (
    <button
      className={cn(
        "flex items-center justify-center h-full relative",
        "transition-colors duration-200",
        disabled ? "opacity-50 cursor-not-allowed" : "cursor-pointer",
        className
      )}
      style={{
        backgroundColor: isActive
          ? activeBackgroundColor
          : "var(--ableset-color-background-light)",
      }}
      onMouseEnter={(e) => {
        if (!disabled) {
          e.currentTarget.style.boxShadow =
            "inset 0 0 0 1000px rgba(255, 255, 255, 0.05)";
        }
      }}
      onMouseLeave={(e) => {
        e.currentTarget.style.boxShadow = "";
      }}
      onClick={disabled ? undefined : onClick}
      disabled={disabled}
    >
      {children}
    </button>
  );
};

// Status indicator component
const StatusIndicator: React.FC<{ connected: boolean }> = ({ connected }) => (
  <div className="flex items-center gap-2 px-3">
    <div
      className={`w-2 h-2 rounded-full ${connected ? 'bg-green-500' : 'bg-red-500'}`}
    />
    <span className="text-xs text-[var(--ableset-color-text)] opacity-75">
      {connected ? 'Connected' : 'Disconnected'}
    </span>
  </div>
);

// Transport info display
const TransportInfo: React.FC<{ transport: any; className?: string }> = ({
  transport,
  className
}) => {
  if (!transport) return null;

  return (
    <div className={cn("flex items-center gap-4 px-4", className)}>
      <div className="flex items-center gap-1">
        <Clock className="w-3 h-3 text-[var(--ableset-color-text)] opacity-75" />
        <span className="text-xs text-[var(--ableset-color-text)] opacity-75">
          {transport.playhead_position?.time?.seconds?.toFixed(1) || '0.0'}s
        </span>
      </div>

      <div className="flex items-center gap-1">
        <Activity className="w-3 h-3 text-[var(--ableset-color-text)] opacity-75" />
        <span className="text-xs text-[var(--ableset-color-text)] opacity-75">
          {transport.tempo?.bpm || 120} BPM
        </span>
      </div>

      <div className="flex items-center gap-1">
        <span className="text-xs text-[var(--ableset-color-text)] opacity-75">
          {transport.time_signature?.numerator || 4}/{transport.time_signature?.denominator || 4}
        </span>
      </div>
    </div>
  );
};

export const EnhancedStatusBar: React.FC<EnhancedStatusBarProps> = ({
  className = ""
}) => {
  const { appState, loading, error, connected } = useAppState({
    pollInterval: 500, // Update every 500ms for responsive UI
    autoStart: true
  });

  const transport = useTransportState(appState);
  const transportControls = useTransportControls();
  const [showAdvanced, setShowAdvanced] = useState(false);

  // Transport state helpers
  const getPlayState = (): PlayState => transport?.play_state || 'Stopped';
  const isPlaying = getPlayState() === 'Playing';
  const isPaused = getPlayState() === 'Paused';
  const isRecording = getPlayState() === 'Recording';
  const isStopped = getPlayState() === 'Stopped';
  const isLooping = transport?.looping || false;

  // Transport action handlers
  const handlePlayPause = async () => {
    console.log('🎵 EnhancedStatusBar: PlayPause clicked', {
      isRecording,
      currentPlayState: getPlayState(),
      transportControls: !!transportControls,
      connected
    });
    try {
      if (isRecording) {
        // If recording, stop recording but continue playback
        console.log('🔴 Stopping recording...');
        const result = await transportControls.stopRecording();
        console.log('✅ Stop recording result:', result);
      } else {
        console.log('⏯️ Toggling play/pause...');
        const result = await transportControls.playPause();
        console.log('✅ PlayPause result:', result);
      }
    } catch (err) {
      console.error('❌ Transport action failed:', err);
    }
  };

  const handleStop = async () => {
    console.log('⏹️ EnhancedStatusBar: Stop clicked', {
      currentPlayState: getPlayState(),
      transportControls: !!transportControls,
      connected
    });
    try {
      const result = await transportControls.stop();
      console.log('✅ Stop result:', result);
    } catch (err) {
      console.error('❌ Stop failed:', err);
    }
  };

  const handleRecord = async () => {
    console.log('🔴 EnhancedStatusBar: Record clicked', {
      isRecording,
      currentPlayState: getPlayState(),
      transportControls: !!transportControls,
      connected
    });
    try {
      if (isRecording) {
        console.log('🛑 Stopping recording...');
        const result = await transportControls.stopRecording();
        console.log('✅ Stop recording result:', result);
      } else {
        console.log('🔴 Starting recording...');
        const result = await transportControls.startRecording();
        console.log('✅ Start recording result:', result);
      }
    } catch (err) {
      console.error('❌ Record toggle failed:', err);
    }
  };

  const handleGoToStart = async () => {
    console.log('⏮️ EnhancedStatusBar: GoToStart clicked', {
      currentPosition: transport?.playhead_position?.time?.seconds || 0,
      transportControls: !!transportControls,
      connected
    });
    try {
      const result = await transportControls.setPosition(0);
      console.log('✅ GoToStart result:', result);
    } catch (err) {
      console.error('❌ Go to start failed:', err);
    }
  };

  // Get appropriate play button icon and color
  const getPlayButtonContent = () => {
    if (isRecording) {
      return {
        icon: <CircleDot className="h-8 w-8 text-white" strokeWidth={1.5} fill="white" />,
        bgColor: "#dc2626",
        label: "Recording"
      };
    } else if (isPlaying) {
      return {
        icon: <Pause className="h-8 w-8 text-white" strokeWidth={1.5} />,
        bgColor: "var(--ableset-color-default-500)",
        label: "Pause"
      };
    } else {
      return {
        icon: <Play className="h-8 w-8 text-white" strokeWidth={1.5} fill="white" />,
        bgColor: "var(--ableset-color-background-light)",
        label: "Play"
      };
    }
  };

  const playButtonContent = getPlayButtonContent();

  if (error && !appState) {
    return (
      <div className={`fixed bottom-0 left-0 right-0 ${className}`}>
        <div className="h-36 flex items-center justify-center bg-red-900 border-t-2 border-red-700">
          <div className="text-center">
            <span className="text-red-200 text-sm">Transport Error</span>
            <p className="text-red-300 text-xs mt-1">{error}</p>
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className={`fixed bottom-0 left-0 right-0 ${className}`}>
      {/* Advanced info bar */}
      {showAdvanced && transport && (
        <div className="bg-black/50 border-t border-[var(--ableset-color-border)] px-4 py-2">
          <div className="flex items-center justify-between">
            <TransportInfo transport={transport} />
            <StatusIndicator connected={connected} />
          </div>
        </div>
      )}

      {/* Main transport controls */}
      <div
        className={cn(
          "h-36 grid grid-cols-5",
          "divide-x-2 divide-[var(--ableset-color-border)]",
          "border-t-2 border-[var(--ableset-color-border)]"
        )}
      >
        {/* Go to Start */}
        <TransportButton
          onClick={handleGoToStart}
          disabled={loading}
        >
          <div className="flex flex-col items-center justify-center gap-1">
            <SkipBack
              className="h-5 w-5 text-[var(--ableset-color-text)]"
              strokeWidth={1.5}
            />
            <span className="text-xs font-medium text-[var(--ableset-color-text)]">
              Start
            </span>
          </div>
        </TransportButton>

        {/* Play/Pause */}
        <TransportButton
          onClick={handlePlayPause}
          isActive={isPlaying || isRecording}
          activeBackgroundColor={playButtonContent.bgColor}
          disabled={loading}
        >
          <div className="flex flex-col items-center justify-center gap-1">
            {playButtonContent.icon}
            <span className="text-xs font-medium text-white">
              {playButtonContent.label}
            </span>
          </div>
        </TransportButton>

        {/* Stop */}
        <TransportButton
          onClick={handleStop}
          isActive={false}
          disabled={loading || isStopped}
        >
          <div className="flex flex-col items-center justify-center gap-1">
            <Square
              className="h-5 w-5 text-[var(--ableset-color-text)]"
              strokeWidth={1.5}
            />
            <span className="text-xs font-medium text-[var(--ableset-color-text)]">
              Stop
            </span>
          </div>
        </TransportButton>

        {/* Record */}
        <TransportButton
          onClick={handleRecord}
          isActive={isRecording}
          activeBackgroundColor="#dc2626"
          disabled={loading}
        >
          <div className="flex flex-col items-center justify-center gap-1">
            <CircleDot
              className={`h-5 w-5 ${isRecording ? 'text-white' : 'text-red-500'}`}
              strokeWidth={1.5}
              fill={isRecording ? 'white' : 'none'}
            />
            <span className={`text-xs font-medium ${isRecording ? 'text-white' : 'text-[var(--ableset-color-text)]'}`}>
              Record
            </span>
          </div>
        </TransportButton>

        {/* Settings/Info Toggle */}
        <TransportButton
          onClick={() => setShowAdvanced(!showAdvanced)}
          isActive={showAdvanced}
          activeBackgroundColor="var(--ableset-color-background-dark)"
        >
          <div className="flex flex-col items-center justify-center gap-1">
            <Settings
              className="h-5 w-5 text-[var(--ableset-color-text)]"
              strokeWidth={1.5}
            />
            <span className="text-xs font-medium text-[var(--ableset-color-text)]">
              Info
            </span>
          </div>
        </TransportButton>
      </div>

      {/* Loading indicator */}
      {loading && (
        <div className="absolute top-0 left-0 right-0 h-1">
          <div className="h-full bg-gradient-to-r from-blue-500 to-green-500 animate-pulse" />
        </div>
      )}
    </div>
  );
};
