import React, { useState } from 'react';
import { Ableset } from '@/components/daw/performance/themes/ableset/Ableset';
import { AppStateDebug } from '@/components/debug/AppStateDebug';
import { Button } from '@/components/ui/button';

export const MainApp: React.FC = () => {
  const [showDebug, setShowDebug] = useState(false);

  return (
    <div className="h-screen w-screen ableset-theme overflow-hidden">
      {/* Debug Toggle Button */}
      <button
        className="fixed top-4 right-4 z-50 px-3 py-1 bg-black/50 text-white rounded text-xs hover:bg-black/70"
        onClick={() => setShowDebug(!showDebug)}
      >
        {showDebug ? 'Hide Debug' : 'Show Debug'}
      </button>

      {/* Debug Panel */}
      {showDebug && (
        <div className="fixed top-12 right-4 z-40 w-80 max-h-96 overflow-auto bg-black/90 rounded p-2">
          <AppStateDebug />
        </div>
      )}

      <Ableset />
    </div>
  );
};

export default MainApp;
