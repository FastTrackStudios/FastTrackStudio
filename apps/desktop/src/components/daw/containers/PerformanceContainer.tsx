import React from 'react';
import { SongTitle } from '../performance/themes/ableset/SongTitle';
import { SongDescription } from '../performance/themes/ableset/SongDescription';
import { DetailBadges } from '../performance/themes/ableset/DetailBadges';
import { SongProgressBar } from '../performance/themes/ableset/SongProgressBar';
import { TransportBar } from '../performance/themes/ableset/TransportBar';
import { useAppState, useActiveProject, useTransportState, useTransportControls } from '../../../hooks/useAppState';
import type { SetlistState } from '../../../types/placeholders';

interface PerformanceContainerProps {
  className?: string;
  component: 'songTitle' | 'songDescription' | 'detailBadges' | 'songProgressBar' | 'transportBar';
  onSectionClick?: (sectionIndex: number) => void;
}

export const PerformanceContainer: React.FC<PerformanceContainerProps> = ({
  className,
  component,
  onSectionClick
}) => {
  const { appState, loading, error, connected } = useAppState();
  const transport = useTransportState(appState);
  const transportControls = useTransportControls();
  // TODO: Replace with actual setlist data when available from backend
  const setlistState: SetlistState | null = null;

  // Transport control handlers
  const handlePlayPause = async () => {
    console.log('🎵 PerformanceContainer: PlayPause clicked');
    try {
      const result = await transportControls.playPause();
      console.log('✅ PerformanceContainer: PlayPause result:', result);
    } catch (err) {
      console.error('❌ PerformanceContainer: PlayPause failed:', err);
    }
  };

  const handleStop = async () => {
    console.log('⏹️ PerformanceContainer: Stop clicked');
    try {
      const result = await transportControls.stop();
      console.log('✅ PerformanceContainer: Stop result:', result);
    } catch (err) {
      console.error('❌ PerformanceContainer: Stop failed:', err);
    }
  };

  const handleGoToStart = async () => {
    console.log('⏮️ PerformanceContainer: GoToStart clicked');
    try {
      const result = await transportControls.setPosition(0);
      console.log('✅ PerformanceContainer: GoToStart result:', result);
    } catch (err) {
      console.error('❌ PerformanceContainer: GoToStart failed:', err);
    }
  };

  const handleToggleLoop = async () => {
    console.log('🔄 PerformanceContainer: ToggleLoop clicked - not yet implemented');
    // TODO: Implement loop toggle when available in transportControls
  };

  const handleNext = async () => {
    console.log('⏭️ PerformanceContainer: Next clicked - not yet implemented');
    // TODO: Implement next section/song navigation when available
  };

  const handleSectionClick = (sectionIndex: number) => {
    console.log('🎯 PerformanceContainer: SectionClick clicked, index:', sectionIndex);
    if (onSectionClick) {
      onSectionClick(sectionIndex);
    } else {
      // TODO: Implement section jumping when available in transportControls
      console.log('Jump to section not yet implemented:', sectionIndex);
    }
  };

  // Common props for all components
  const commonProps = {
    className,
    setlistState,
    transport,
    connected,
    loading,
    error,
  };

  switch (component) {
    case 'songTitle':
      return (
        <SongTitle
          {...commonProps}
        />
      );

    case 'songDescription':
      return (
        <SongDescription
          {...commonProps}
        />
      );

    case 'detailBadges':
      return (
        <DetailBadges
          {...commonProps}
        />
      );

    case 'songProgressBar':
      return (
        <SongProgressBar
          className={className}
          setlistState={setlistState}
          transport={transport}
          connected={connected}
          onSectionClick={handleSectionClick}
        />
      );

    case 'transportBar':
      return (
        <TransportBar
          {...commonProps}
          onPlayPause={handlePlayPause}
          onStop={handleStop}
          onGoToStart={handleGoToStart}
          onToggleLoop={handleToggleLoop}
          onNext={handleNext}
        />
      );

    default:
      return null;
  }
};
