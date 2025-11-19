import React from 'react';
import { Song } from '@/atoms/types';
import { getSongColor } from '@/components/ProgressBar';

interface TransportControlsProps {
  song: Song | null;
  songIndex: number;
  currentSectionIndex: number | null;
  isPlaying: boolean;
  isLooping: boolean;
  onBack: () => void;
  onPlayPause: () => void;
  onLoop: () => void;
  onAdvance: () => void;
}

export const TransportControls: React.FC<TransportControlsProps> = ({
  song,
  songIndex,
  currentSectionIndex,
  isPlaying,
  isLooping,
  onBack,
  onPlayPause,
  onLoop,
  onAdvance,
}) => {
  const songColor = song ? getSongColor(songIndex, 'bright') : undefined;

  return (
    <div className="grid grid-cols-4 gap-2" style={{ height: '80px' }}>
      {/* Back Button */}
      <button
        onClick={onBack}
        className="border rounded-lg flex items-center justify-center font-medium transition-all hover:bg-muted/50 active:scale-95 h-full"
        style={{
          borderColor: songColor || 'currentColor',
          color: songColor || 'currentColor',
        }}
      >
        <span className="text-lg">◀</span>
        {song && <span className="ml-1">Back</span>}
      </button>

      {/* Play/Pause Button */}
      <button
        onClick={onPlayPause}
        className="border rounded-lg flex items-center justify-center font-medium transition-all hover:opacity-90 active:scale-95 h-full"
        style={{
          backgroundColor: isPlaying ? songColor : 'transparent',
          borderColor: songColor || 'currentColor',
          color: isPlaying ? '#ffffff' : (songColor || 'currentColor'),
        }}
      >
        <span className="text-lg mr-1">{isPlaying ? '⏸' : '▶'}</span>
        <span>{isPlaying ? 'Pause' : 'Play'}</span>
      </button>

      {/* Loop Button */}
      <button
        onClick={onLoop}
        className="border rounded-lg flex items-center justify-center font-medium transition-all hover:opacity-90 active:scale-95 h-full"
        style={{
          backgroundColor: isLooping ? '#22c55e' : 'rgba(0, 0, 0, 0.1)',
          borderColor: isLooping ? '#22c55e' : 'currentColor',
          color: isLooping ? '#ffffff' : 'currentColor',
        }}
      >
        {isLooping && <span className="text-lg mr-1">🔁</span>}
        <span>Loop</span>
      </button>

      {/* Advance Button */}
      <button
        onClick={onAdvance}
        className="border rounded-lg flex items-center justify-center font-medium transition-all hover:bg-muted/50 active:scale-95 h-full"
        style={{
          borderColor: songColor || 'currentColor',
          color: songColor || 'currentColor',
        }}
      >
        {song && <span className="mr-1">Advance</span>}
        <span className="text-lg">▶</span>
      </button>
    </div>
  );
};

