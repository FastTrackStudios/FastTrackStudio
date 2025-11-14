import React from "react";
import { cn } from "@/lib/utils";
import { useWebSocket } from "../../../../../contexts/WebSocketContext";
import { SettingsContext } from "./SettingsDropdown";

interface DetailBadgesProps {
  className?: string;
}

interface BadgeProps {
  children: React.ReactNode;
  className?: string;
  variant?: "default" | "active" | "muted" | "blue" | "green";
}

const Badge: React.FC<BadgeProps> = ({
  children,
  className = "",
  variant = "default",
}) => {
  const getVariantStyles = () => {
    switch (variant) {
      case "blue":
        return {
          backgroundColor: "var(--ableset-tag-blue)",
          color: "var(--ableset-tag-blue-text)",
        };
      case "green":
        return {
          backgroundColor: "var(--ableset-tag-green)",
          color: "var(--ableset-tag-green-text)",
        };
      case "active":
        return {
          backgroundColor: "var(--ableset-tag-green)",
          color: "var(--ableset-tag-green-text)",
        };
      case "muted":
        return {
          backgroundColor: "var(--ableset-color-background-light)",
          color: "var(--ableset-color-default-300)",
        };
      default:
        return {
          backgroundColor: "var(--ableset-tag-blue)",
          color: "var(--ableset-tag-blue-text)",
        };
    }
  };

  return (
    <div
      className={cn(
        "px-6 py-3 rounded-xl font-semibold text-lg",
        "border border-[var(--ableset-color-border)]",
        "transition-colors duration-200",
        className
      )}
      style={getVariantStyles()}
    >
      {children}
    </div>
  );
};

export const DetailBadges: React.FC<DetailBadgesProps> = ({
  className = "",
}) => {
  const { setlistState, transportState, connected } = useWebSocket();
  const { settings } = React.useContext(SettingsContext);

  // Helper function to check if a setting is enabled
  const isSettingEnabled = (id: string): boolean => {
    const setting = settings.find((s) => s.id === id);
    return setting ? setting.enabled : true; // Default to true if setting doesn't exist
  };

  // Helper functions to get badge data
  const getTempo = (): string => {
    if (!transportState?.tempo_bpm) return "-- BPM";
    return `${Math.round(transportState.tempo_bpm)} BPM`;
  };

  const getTimeSignature = (): string => {
    if (
      !transportState?.time_signature_numerator ||
      !transportState?.time_signature_denominator
    ) {
      return "--/--";
    }
    return `${transportState.time_signature_numerator}/${transportState.time_signature_denominator}`;
  };

  const getCurrentSection = (): string => {
    if (!setlistState?.current_section) return "No Section";
    return setlistState.current_section.name;
  };

  const getNextSection = (): string => {
    if (
      !setlistState?.current_song?.sections ||
      !setlistState?.current_section
    ) {
      return "No Next Section";
    }

    const currentIndex = setlistState.current_section.index;
    const nextSection = setlistState.current_song.sections.find(
      (section) => section.index === currentIndex + 1
    );

    return nextSection ? nextSection.name : "End of Song";
  };

  const getSongDuration = (): string => {
    if (!setlistState?.current_song) return "0:00";

    const totalSeconds = setlistState.current_song.length_time;
    const minutes = Math.floor(totalSeconds / 60);
    const seconds = Math.floor(totalSeconds % 60);

    return `${minutes}:${seconds.toString().padStart(2, "0")}`;
  };

  const getSongTags = (): string => {
    // TODO: Implement song tags from song metadata
    return "Rock, Upbeat";
  };

  if (!connected) {
    return (
      <div className={cn("flex flex-wrap gap-4", className)}>
        <Badge variant="muted">Disconnected</Badge>
      </div>
    );
  }

  // Collect visible badges based on settings
  const visibleBadges = [];

  if (isSettingEnabled("songTags")) {
    visibleBadges.push(
      <Badge key="songTags" variant="muted">
        {getSongTags()}
      </Badge>
    );
  }

  if (isSettingEnabled("currentSection")) {
    visibleBadges.push(
      <Badge key="currentSection" variant="green">
        {getCurrentSection()}
      </Badge>
    );
  }

  if (isSettingEnabled("nextSection")) {
    visibleBadges.push(
      <Badge key="nextSection" variant="default">
        Next: {getNextSection()}
      </Badge>
    );
  }

  if (isSettingEnabled("tempo")) {
    visibleBadges.push(
      <Badge key="tempo" variant="blue">
        {getTempo()}
      </Badge>
    );
  }

  if (isSettingEnabled("timeSignature")) {
    visibleBadges.push(
      <Badge key="timeSignature" variant="blue">
        {getTimeSignature()}
      </Badge>
    );
  }

  if (isSettingEnabled("songDuration")) {
    visibleBadges.push(
      <Badge key="songDuration" variant="default">
        {getSongDuration()}
      </Badge>
    );
  }

  // If no badges are visible, don't render anything
  if (visibleBadges.length === 0) {
    return null;
  }

  return (
    <div className={cn("flex flex-wrap gap-4", className)}>{visibleBadges}</div>
  );
};
