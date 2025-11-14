import React from 'react';
import { MarkerRegionDisplay } from '../MarkerRegionDisplay';
import { useAppState, useActiveProject } from '../../../hooks/useAppState';
import type { MarkerRegionState } from '../../../types/placeholders';

interface MarkerRegionContainerProps {
  className?: string;
}

export const MarkerRegionContainer: React.FC<MarkerRegionContainerProps> = ({
  className
}) => {
  const { appState, loading, error, connected } = useAppState();
  const activeProject = useActiveProject(appState);

  // TODO: Replace with actual markers/regions data when available from backend
  const markerRegionState: MarkerRegionState | null = null;

  return (
    <MarkerRegionDisplay
      className={className}
      markerRegionState={markerRegionState}
      connected={connected}
      loading={loading}
      error={error}
    />
  );
};
