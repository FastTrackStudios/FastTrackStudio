import React from 'react';
import { TransportDisplay } from '../TransportDisplay';
import { useAppState, useTransportState, useTransportControls } from '../../../hooks/useAppState';

interface TransportContainerProps {
  className?: string;
}

export const TransportContainer: React.FC<TransportContainerProps> = ({
  className
}) => {
  const { appState, loading, error, connected } = useAppState();
  const transport = useTransportState(appState);
  const transportControls = useTransportControls();

  const handlePlayPause = async () => {
    console.log('🎵 TransportContainer: PlayPause clicked');
    try {
      const result = await transportControls.playPause();
      console.log('✅ TransportContainer: PlayPause result:', result);
    } catch (err) {
      console.error('❌ TransportContainer: PlayPause failed:', err);
    }
  };

  const handleStop = async () => {
    console.log('⏹️ TransportContainer: Stop clicked');
    try {
      const result = await transportControls.stop();
      console.log('✅ TransportContainer: Stop result:', result);
    } catch (err) {
      console.error('❌ TransportContainer: Stop failed:', err);
    }
  };

  const handleGoToStart = async () => {
    console.log('⏮️ TransportContainer: GoToStart clicked');
    try {
      const result = await transportControls.setPosition(0);
      console.log('✅ TransportContainer: GoToStart result:', result);
    } catch (err) {
      console.error('❌ TransportContainer: GoToStart failed:', err);
    }
  };

  return (
    <TransportDisplay
      className={className}
      transport={transport}
      connected={connected}
      loading={loading}
      error={error}
      onPlayPause={handlePlayPause}
      onStop={handleStop}
      onGoToStart={handleGoToStart}
    />
  );
};
