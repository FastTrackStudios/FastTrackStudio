import React from "react";
import { EnhancedStatusBar } from "./EnhancedStatusBar";
import { TopControls } from "./TopControls";
import { PerformanceContainer } from "../../../containers/PerformanceContainer";
import { SettingsContext, SettingsDropdownProvider } from "./SettingsDropdown";

interface AblesetProps {
  className?: string;
  onExit?: () => void;
}

export const Ableset: React.FC<AblesetProps> = ({ className = "", onExit }) => {
  return (
    <SettingsDropdownProvider>
      <AblesetContent className={className} onExit={onExit} />
    </SettingsDropdownProvider>
  );
};

const AblesetContent: React.FC<AblesetProps> = ({ className = "", onExit }) => {
  const { settings } = React.useContext(SettingsContext);

  // Helper function to check if a setting is enabled
  const isSettingEnabled = (id: string): boolean => {
    const setting = settings.find((s) => s.id === id);
    return setting ? setting.enabled : true; // Default to true if setting doesn't exist
  };

  return (
    <div
      className={`flex flex-col w-full h-full ${className}`}
      style={{
        background: "var(--ableset-color-background-deep)",
      }}
    >
      {/* Top Controls */}
      <TopControls onExit={onExit} />

      {/* Song Title Area - takes up most of the space with bottom offset */}
      <div className="flex-1 w-full pb-48 flex flex-col px-24">
        <div className="w-full flex-1 flex flex-col justify-center space-y-8">
          <div className="w-full text-center space-y-4">
            <PerformanceContainer component="songTitle" className="w-full" />
            {isSettingEnabled("songDescriptions") && (
              <PerformanceContainer component="songDescription" className="w-full" />
            )}
          </div>

          {isSettingEnabled("songInformation") && (
            <PerformanceContainer component="detailBadges" className="w-full justify-center" />
          )}

          {isSettingEnabled("songProgress") && (
            <PerformanceContainer component="songProgressBar" />
          )}
        </div>
      </div>

      {/* Status Bar at Bottom */}
      <EnhancedStatusBar />
    </div>
  );
};
