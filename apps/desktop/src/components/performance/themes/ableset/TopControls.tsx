import React from "react";
import { ExitButton } from "./ExitButton";
import { LockControls } from "./LockControls";
import { SettingsDropdown, SettingsContext } from "./SettingsDropdown";
import { Clock } from "./Clock";

interface TopControlsProps {
  className?: string;
  onExit?: () => void;
}

export const TopControls: React.FC<TopControlsProps> = ({
  className = "",
  onExit,
}) => {
  const { settings } = React.useContext(SettingsContext);

  // Helper function to check if a setting is enabled
  const isSettingEnabled = (id: string): boolean => {
    const setting = settings.find((s) => s.id === id);
    return setting ? setting.enabled : true; // Default to true if setting doesn't exist
  };

  return (
    <>
      {/* Lock and Settings in top left */}
      <div
        className={`fixed top-4 left-4 flex items-center gap-4 z-50 ${className}`}
      >
        <LockControls />
        <SettingsDropdown />
      </div>

      {/* Setlist title centered at top */}
      <div
        className={`fixed top-4 left-1/2 transform -translate-x-1/2 flex items-center h-12 z-50 ${className}`}
      >
        <span
          className="text-md font-bold opacity-50"
          style={{ color: "var(--ableset-color-text)" }}
        >
          SETLIST TITLE
        </span>
      </div>

      {/* Clock and Exit button in top right */}
      <div
        className={`fixed top-4 right-4 flex items-center gap-4 z-50 ${className}`}
      >
        {isSettingEnabled("clock") && <Clock />}
        <ExitButton onClick={onExit} />
      </div>
    </>
  );
};
