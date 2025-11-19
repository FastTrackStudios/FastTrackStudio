import React, { useState, useEffect } from "react";
import { useWebSocketAdapter } from "../../../../hooks/use-websocket-adapter";

// Add CSS keyframes for flashing animation
const flashKeyframes = `
  @keyframes flash {
    0% {
      box-shadow: inset 0 0 0 1000px rgba(255, 255, 255, 0.1);
    }
    50% {
      box-shadow: inset 0 0 0 1000px rgba(255, 255, 255, 0.25);
    }
    100% {
      box-shadow: inset 0 0 0 1000px rgba(255, 255, 255, 0.1);
    }
  }
`;

// Inject the keyframes into the document head
if (typeof document !== "undefined") {
  const style = document.createElement("style");
  style.textContent = flashKeyframes;
  document.head.appendChild(style);
}

interface TempoChangeInfo {
  position_seconds: number;
  position_beats: number;
  tempo: number;
  time_signature_numerator: number;
  time_signature_denominator: number;
  is_tempo_marker: boolean;
  is_time_signature_marker: boolean;
}

interface SectionInfo {
  index: number;
  name: string;
  start_position_beats: number;
  end_position_beats: number;
  length_measures: number;
  length_time: number;
  color?: number;
}

interface SongInfo {
  index: number;
  name: string;
  start_position_seconds: number;
  end_position_seconds: number;
  start_position_beats: number;
  end_position_beats: number;
  length_measures: number;
  length_time: number;
  sections: SectionInfo[];
  color?: number;
  tempo_changes: TempoChangeInfo[];
}

interface SetlistState {
  songs: SongInfo[];
  current_song: SongInfo | null;
  current_section: SectionInfo | null;
  next_section: SectionInfo | null;
  queued_name: [string, string]; // [song_name, section_name]
  queued_index: [number, number]; // [song_index, section_index]
  timestamp: number;
}

interface TransportState {
  position_seconds: number;
  timestamp: number;
}

interface WebSocketMessage {
  type: "Setlist" | "Transport" | "MarkerRegions" | "Error";
  data: SetlistState | TransportState | any;
}

interface SongProgressBarProps {
  className?: string;
}

export const SongProgressBar: React.FC<SongProgressBarProps> = ({
  className = "",
}) => {
  const { setlistState, transportState, connected, commands } = useWebSocketAdapter();

  // Track previous section to detect changes
  const [previousSectionIndex, setPreviousSectionIndex] = useState<
    number | null
  >(null);
  const [shouldAnimate, setShouldAnimate] = useState(true);

  // Detect section changes and manage animation
  useEffect(() => {
    const currentSectionIndex = setlistState?.current_section?.index ?? null;

    if (
      previousSectionIndex !== null &&
      currentSectionIndex !== previousSectionIndex
    ) {
      // Section changed - disable animation temporarily
      setShouldAnimate(false);

      // Re-enable animation after a brief delay
      const timer = setTimeout(() => {
        setShouldAnimate(true);
      }, 50);

      return () => clearTimeout(timer);
    }

    setPreviousSectionIndex(currentSectionIndex);
  }, [setlistState?.current_section?.index, previousSectionIndex]);

  // Handle section click to jump to that section
  const handleSectionClick = (section: SectionInfo) => {
    // Jump to the section by index
    commands.jumpToSection(section.index);
  };

  // Calculate section widths based on their length in measures
  const getSectionWidths = (): Array<{
    section: any;
    widthPercent: number;
    isActive: boolean;
    isQueued: boolean;
  }> => {
    if (
      !setlistState?.current_song?.sections ||
      setlistState.current_song.sections.length === 0
    ) {
      return [];
    }

    const song = setlistState.current_song;
    // Use total measures for width calculation instead of time
    const totalMeasures = song.sections.reduce(
      (total, section) => total + section.length_measures,
      0
    );

    // Check if there's a manually queued section (not just naturally next)
    const hasQueuedSection =
      setlistState.queued_index &&
      setlistState.queued_index[0] !== -1 &&
      setlistState.queued_index[1] !== -1 &&
      setlistState.queued_name[1] !== ""; // Section name is not empty

    const queuedSectionIndex = hasQueuedSection
      ? setlistState.queued_index[1]
      : null;

    return song.sections.map((section: any) => {
      // Calculate width based purely on measures, not time
      const widthPercent = (section.length_measures / totalMeasures) * 100;
      const isActive = setlistState.current_section?.index === section.index;

      // Only mark as queued if it's manually queued (not just naturally next)
      const isQueued = hasQueuedSection && queuedSectionIndex === section.index;

      return {
        section,
        widthPercent,
        isActive,
        isQueued,
      };
    });
  };

  // Calculate overall song progress based on measures/beats
  const getSongProgress = (): number => {
    if (!transportState || !setlistState?.current_song) return 0;

    const song = setlistState.current_song;

    // Use accurate beat position from transport state
    const currentPositionBeats = transportState.position_beats;
    const songStartBeats = song.start_position_beats;
    const songEndBeats = song.end_position_beats;

    if (
      currentPositionBeats < songStartBeats ||
      currentPositionBeats > songEndBeats
    )
      return 0;

    // Calculate progress based on beats for accurate positioning regardless of tempo changes
    const songDurationBeats = songEndBeats - songStartBeats;
    const beatsProgress =
      (currentPositionBeats - songStartBeats) / songDurationBeats;

    return Math.max(0, Math.min(100, beatsProgress * 100));
  };

  // Helper function to convert color number to CSS color
  const getSectionColor = (
    section: any,
    isActive: boolean,
    isQueued: boolean
  ): string => {
    const currentSectionIndex = setlistState?.current_section?.index ?? -1;
    const hasBeenPlayed = section.index < currentSectionIndex;
    const isUpcoming = section.index > currentSectionIndex;

    if (section.color) {
      const r = (section.color >> 16) & 0xff;
      const g = (section.color >> 8) & 0xff;
      const b = section.color & 0xff;

      if (isActive) {
        // Current section - full brightness
        return `rgb(${r}, ${g}, ${b})`;
      } else if (isQueued) {
        // Queued section - brighter for flashing effect
        return `rgb(${Math.min(255, Math.floor(r * 1.2))}, ${Math.min(
          255,
          Math.floor(g * 1.2)
        )}, ${Math.min(255, Math.floor(b * 1.2))})`;
      } else if (hasBeenPlayed) {
        // Already played - slightly brighter than normal
        return `rgb(${Math.min(255, Math.floor(r * 1.1))}, ${Math.min(
          255,
          Math.floor(g * 1.1)
        )}, ${Math.min(255, Math.floor(b * 1.1))})`;
      } else if (isUpcoming) {
        // Not played yet - muted
        return `rgb(${Math.floor(r * 0.5)}, ${Math.floor(
          g * 0.5
        )}, ${Math.floor(b * 0.5)})`;
      }
    }

    // Fall back to default colors if no section color
    if (isActive) {
      return "var(--ableset-color-default-500)";
    } else if (isQueued) {
      return "var(--ableset-color-default-300)"; // Brighter for queued
    } else if (hasBeenPlayed) {
      return "var(--ableset-color-default-500)"; // Slightly brighter for played
    } else {
      return "var(--ableset-color-default-700)"; // Muted for upcoming
    }
  };

  // Calculate progress within the current section (0-100%)
  const getSectionProgress = (): number => {
    if (
      !transportState ||
      !setlistState?.current_section ||
      !setlistState?.current_song
    ) {
      return 0;
    }

    // Use accurate beat position from transport state
    const currentPositionBeats = transportState.position_beats;
    const section = setlistState.current_section;

    const sectionStartBeats = section.start_position_beats;
    const sectionEndBeats = section.end_position_beats;

    if (
      currentPositionBeats < sectionStartBeats ||
      currentPositionBeats > sectionEndBeats
    ) {
      return 0;
    }

    const sectionDurationBeats = sectionEndBeats - sectionStartBeats;
    const sectionProgress =
      (currentPositionBeats - sectionStartBeats) / sectionDurationBeats;

    return Math.max(0, Math.min(100, sectionProgress * 100));
  };

  // Get current section color for section progress bar
  const getCurrentSectionColor = (): string => {
    if (setlistState?.current_section?.color) {
      const color = setlistState.current_section.color;
      const r = (color >> 16) & 0xff;
      const g = (color >> 8) & 0xff;
      const b = color & 0xff;
      return `rgb(${r}, ${g}, ${b})`;
    }
    return "var(--ableset-color-default-500)";
  };

  // Get tempo and time signature changes for the current song
  const getTempoTimeSignatureChanges = (): Array<{
    change: TempoChangeInfo;
    positionPercent: number;
    label: string;
    isOnSectionBoundary: boolean;
  }> => {
    if (!setlistState?.current_song?.tempo_changes) {
      return [];
    }

    const song = setlistState.current_song;
    // Use beats for positioning to align with measure-based sections
    const songDurationBeats =
      song.end_position_beats - song.start_position_beats;

    console.log("Song beat range:", {
      start: song.start_position_beats,
      end: song.end_position_beats,
      duration: songDurationBeats,
    });

    // Sort changes by position to track previous values
    const sortedChanges = [...song.tempo_changes].sort(
      (a, b) => a.position_beats - b.position_beats
    );

    console.log(
      "Tempo changes raw data:",
      sortedChanges.map((c) => ({
        position_beats: c.position_beats,
        position_seconds: c.position_seconds,
        tempo: c.tempo,
        time_sig: `${c.time_signature_numerator}/${c.time_signature_denominator}`,
      }))
    );

    let previousTempo: number | null = null;
    let previousTimeSignature: string | null = null;

    return sortedChanges
      .map((change) => {
        // Calculate position as percentage of song duration in beats (measure-based)
        const relativePositionBeats =
          change.position_beats - song.start_position_beats;
        let positionPercent = (relativePositionBeats / songDurationBeats) * 100;

        // Check if this marker coincides with a section boundary
        const tolerance = 0.1; // Small tolerance for floating point comparison
        let isOnSectionBoundary = false;

        // Check if the marker position matches any section start position (except the first section)
        for (let i = 1; i < song.sections.length; i++) {
          const sectionStartBeats = song.sections[i].start_position_beats;
          const markerBeats = change.position_beats;

          if (Math.abs(markerBeats - sectionStartBeats) <= tolerance) {
            isOnSectionBoundary = true;

            // Calculate the cumulative width of sections up to this point
            const totalMeasures = song.sections.reduce(
              (total, section) => total + section.length_measures,
              0
            );
            let cumulativeWidth = 0;

            for (let j = 0; j < i; j++) {
              cumulativeWidth +=
                (song.sections[j].length_measures / totalMeasures) * 100;
            }

            // Position the marker at the gap (end of previous section)
            positionPercent = cumulativeWidth;
            break;
          }
        }

        console.log("Processing change:", {
          position_beats: change.position_beats,
          song_start_beats: song.start_position_beats,
          relative_beats: relativePositionBeats,
          position_percent: positionPercent,
          is_on_section_boundary: isOnSectionBoundary,
        });

        // Check what actually changed
        const currentTempo = change.tempo > 0 ? change.tempo : null;
        const currentTimeSignature =
          change.time_signature_numerator > 0 &&
          change.time_signature_denominator > 0
            ? `${change.time_signature_numerator}/${change.time_signature_denominator}`
            : null;

        const tempoChanged =
          currentTempo !== null && currentTempo !== previousTempo;
        const timeSignatureChanged =
          currentTimeSignature !== null &&
          currentTimeSignature !== previousTimeSignature;

        // Create label based on what actually changed
        let label = "";
        const labelParts: string[] = [];

        if (tempoChanged) {
          labelParts.push(`${Math.round(currentTempo!)} BPM`);
        }

        if (timeSignatureChanged) {
          labelParts.push(currentTimeSignature!);
        }

        label = labelParts.join(", ");

        // Update previous values for next iteration
        if (currentTempo !== null) {
          previousTempo = currentTempo;
        }
        if (currentTimeSignature !== null) {
          previousTimeSignature = currentTimeSignature;
        }

        return {
          change,
          positionPercent: Math.max(0, Math.min(100, positionPercent)),
          label,
          isOnSectionBoundary,
        };
      })
      .filter((item) => item.label !== ""); // Only include items with labels (actual changes)
  };

  const sectionWidths = getSectionWidths();
  const songProgress = getSongProgress();
  const sectionProgress = getSectionProgress();
  const tempoTimeSignatureChanges = getTempoTimeSignatureChanges();

  // Show connection status if not connected
  if (!connected) {
    return (
      <div className={`w-full ${className}`}>
        <div className="h-[8vmin] rounded-2xl flex items-center justify-center bg-gray-800 text-gray-400">
          <span>Connecting to REAPER...</span>
        </div>
      </div>
    );
  }

  if (!setlistState?.current_song || sectionWidths.length === 0) {
    return null;
  }

  return (
    <div className={`w-full ${className}`}>
      {/* Tempo and Time Signature Change Labels */}
      {tempoTimeSignatureChanges.length > 0 && (
        <div className="relative mb-2 h-6">
          {tempoTimeSignatureChanges.map((item, index) => (
            <div
              key={index}
              className="absolute transform -translate-x-1/2"
              style={{
                left: `${item.positionPercent}%`,
                top: 0,
              }}
            >
              <div
                className="text-xs font-medium text-center whitespace-nowrap px-1 py-0.5 rounded"
                style={{
                  color: "var(--ableset-color-text)",
                  backgroundColor: "var(--ableset-color-background-light)",
                  border: "1px solid var(--ableset-color-border)",
                }}
              >
                {item.label}
              </div>
            </div>
          ))}
        </div>
      )}

      {/* Song Progress Bar */}
      <div
        className="h-[8vmin] rounded-2xl overflow-hidden relative"
        style={{
          backgroundColor: "var(--ableset-color-background-light)",
        }}
      >
        {/* Section segments */}
        <div className="flex h-full">
          {sectionWidths.map(
            ({ section, widthPercent, isActive, isQueued }, index) => (
              <div
                key={section.index}
                className={`h-full relative cursor-pointer transition-all duration-200 ease-out hover:scale-y-101 group ${
                  isQueued ? "animate-pulse" : ""
                }`}
                style={{
                  width: `${widthPercent}%`,
                  backgroundColor: getSectionColor(section, isActive, isQueued),
                  borderRight:
                    index < sectionWidths.length - 1
                      ? "2px solid var(--ableset-color-background-deep)"
                      : "none",
                  ...(isQueued && {
                    animation:
                      "flash 0.8s ease-in-out infinite, pulse 1.2s cubic-bezier(0.4, 0, 0.6, 1) infinite",
                  }),
                }}
                title={`${section.name} (${section.length_measures} measures)${
                  isQueued ? " - MANUALLY QUEUED" : ""
                } - Click to jump`}
                onClick={() => handleSectionClick(section)}
              >
                {/* Hover overlay - positioned to not interfere with progress gradient */}
                <div
                  className={`absolute inset-0 pointer-events-none transition-opacity duration-200 ${
                    isQueued
                      ? "group-hover:opacity-100 opacity-0"
                      : "group-hover:opacity-100 opacity-0"
                  }`}
                  style={{
                    background: isQueued
                      ? "rgba(255, 255, 255, 0.15)"
                      : "rgba(255, 255, 255, 0.05)",
                    zIndex: 1,
                  }}
                />
              </div>
            )
          )}
        </div>

        {/* Tempo and Time Signature Change Markers */}
        {tempoTimeSignatureChanges.map((item, index) => (
          <div
            key={index}
            className="absolute top-0 bottom-0 pointer-events-none"
            style={{
              left: `${item.positionPercent}%`,
              width: "2px",
              backgroundColor: "var(--ableset-color-text)",
              opacity: 0.7,
              zIndex: 3,
              transform: item.isOnSectionBoundary ? "translateX(-1px)" : "none", // Center in the gap
            }}
          />
        ))}

        {/* Overall progress overlay - higher z-index to appear above hover effects */}
        <div
          className="absolute top-0 left-0 h-full pointer-events-none"
          style={{
            width: `${songProgress}%`,
            background:
              "linear-gradient(90deg, rgba(255,255,255,0) 0%, rgba(255,255,255,0.15) 50%, rgba(255,255,255,0.3) 85%, rgba(255,255,255,0.4) 100%)",
            zIndex: 2,
          }}
        />
      </div>

      {/* Section Progress Bar - positioned between main bar and labels */}
      <div className="mt-4">
        <div
          className="h-2 rounded-xl overflow-hidden relative"
          style={{
            backgroundColor: "var(--ableset-color-background-light)",
          }}
        >
          {/* Progress fill - only show if there's a current section */}
          {setlistState?.current_section && (
            <div
              className={`h-full ${
                shouldAnimate ? "transition-all duration-200 ease-out" : ""
              }`}
              style={{
                width: `${sectionProgress}%`,
                backgroundColor: getCurrentSectionColor(),
              }}
            />
          )}
        </div>
      </div>

      {/* Section labels */}
      <div className="flex mt-4">
        {sectionWidths.map(({ section, widthPercent, isActive, isQueued }) => (
          <div
            key={section.index}
            className={`text-sm text-center font-medium cursor-pointer hover:opacity-80 transition-opacity ${
              isQueued ? "animate-pulse font-bold" : ""
            }`}
            style={{
              width: `${widthPercent}%`,
              color: section.color
                ? (() => {
                    const r = (section.color >> 16) & 0xff;
                    const g = (section.color >> 8) & 0xff;
                    const b = section.color & 0xff;
                    return `rgb(${r}, ${g}, ${b})`;
                  })()
                : isActive
                ? "var(--ableset-color-default-500)"
                : isQueued
                ? "var(--ableset-color-default-300)"
                : "var(--ableset-color-text)",
              opacity: isActive || isQueued ? 1 : 0.6,
            }}
            title={`Click to jump to ${section.name}${
              isQueued ? " (MANUALLY QUEUED)" : ""
            }`}
            onClick={() => handleSectionClick(section)}
          >
            {isQueued ? `▶ ${section.name}` : section.name}
          </div>
        ))}
      </div>
    </div>
  );
};
