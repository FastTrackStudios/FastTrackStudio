import React from 'react';
import { SetlistDisplay } from '../SetlistDisplay';
import { SetlistSongView } from '../SetlistSongView';
import { SongSectionsDisplay } from '../SongSectionsDisplay';
import { useAppState, useActiveProject, useTransportState } from '../../../hooks/useAppState';
import type { SetlistState } from '../../../types/placeholders';

interface SetlistContainerProps {
  className?: string;
  view?: 'display' | 'songView' | 'sections';
}

export const SetlistContainer: React.FC<SetlistContainerProps> = ({
  className,
  view = 'display'
}) => {
  const { appState, loading, error, connected } = useAppState();
  const activeProject = useActiveProject(appState);
  const transport = useTransportState(appState);

  // TODO: Replace with actual setlist data when available from backend
  const setlistState: SetlistState | null = null;

  const commonProps = {
    className,
    setlistState,
    connected,
    loading,
    error,
  };

  switch (view) {
    case 'songView':
      return (
        <SetlistSongView
          {...commonProps}
          transport={transport}
        />
      );

    case 'sections':
      return (
        <SongSectionsDisplay
          {...commonProps}
          transport={transport}
        />
      );

    case 'display':
    default:
      return (
        <SetlistDisplay
          {...commonProps}
        />
      );
  }
};
