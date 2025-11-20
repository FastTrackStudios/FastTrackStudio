import React, { useMemo } from 'react';
import { Song, Section, TransportStates } from '@/atoms/types';
import { StatusCards } from './StatusCards';

// Color palette based on CLI colors
const COLOR_PALETTE = [
  { bright: '#ff3636', muted: '#af3333' }, // Red
  { bright: '#f66c03', muted: '#a95131' }, // Orange
  { bright: '#99724b', muted: '#724f41' }, // Amber
  { bright: '#fff034', muted: '#dbc300' }, // Yellow
  { bright: '#87ff67', muted: '#85961f' }, // Lime
  { bright: '#3dc300', muted: '#539f31' }, // Green
  { bright: '#00bfaf', muted: '#0a9c8e' }, // Emerald
  { bright: '#19e9ff', muted: '#236384' }, // Teal
  { bright: '#10a4ee', muted: '#1a2f96' }, // Sky
  { bright: '#007dc0', muted: '#2f52a2' }, // Blue
  { bright: '#886ce4', muted: '#624bad' }, // Violet
  { bright: '#b677c6', muted: '#a34bad' }, // Purple
  { bright: '#ff39d4', muted: '#cc2e6e' }, // Pink
];

// Helper to convert packed RGB u32 to hex color string
function colorFromMetadata(colorStr: string | undefined, variant: 'bright' | 'muted'): string {
  if (!colorStr) {
    return '#808080'; // Default gray if no color
  }
  
  // Parse as unsigned integer (u32)
  const colorValue = parseInt(colorStr, 10);
  if (isNaN(colorValue) || colorValue < 0) {
    return '#808080'; // Default gray if invalid
  }
  
  // Extract RGB from packed format: (r << 16) | (g << 8) | b
  // Use unsigned right shift (>>>) to ensure we get unsigned values
  const r = (colorValue >>> 16) & 0xFF;
  const g = (colorValue >>> 8) & 0xFF;
  const b = colorValue & 0xFF;
  
  // Convert to hex
  const hex = `#${[r, g, b].map(x => x.toString(16).padStart(2, '0')).join('')}`;
  
  // For muted variant, reduce saturation/brightness
  if (variant === 'muted') {
    // Simplified: reduce brightness by 30%
    const mutedR = Math.floor(r * 0.7);
    const mutedG = Math.floor(g * 0.7);
    const mutedB = Math.floor(b * 0.7);
    return `#${[mutedR, mutedG, mutedB].map(x => x.toString(16).padStart(2, '0')).join('')}`;
  }
  
  return hex;
}

export function getSongColor(song: Song | null, index: number, variant: 'bright' | 'muted' = 'bright'): string {
  // Get color from song_region_start_marker first (this preserves the song region color)
  if (song?.song_region_start_marker?.color != null && song.song_region_start_marker.color !== 0) {
    return colorFromMetadata(song.song_region_start_marker.color.toString(), variant);
  }
  // Fallback to start_marker if available
  if (song?.start_marker?.color != null && song.start_marker.color !== 0) {
    return colorFromMetadata(song.start_marker.color.toString(), variant);
  }
  // Fallback to palette if no color in marker
  const paletteIdx = index % COLOR_PALETTE.length;
  const paletteEntry = COLOR_PALETTE[paletteIdx];
  if (!paletteEntry) {
    return variant === 'bright' ? '#808080' : '#606060'; // Default gray
  }
  return paletteEntry[variant];
}

function getSectionColor(section: Section, songIdx: number, sectionIdx: number, variant: 'bright' | 'muted' = 'bright'): string {
  // Get color directly from section struct
  if (section.color != null && section.color !== 0) {
    const color = colorFromMetadata(section.color.toString(), variant);
    // Debug log to verify colors are being read
    if (process.env.NODE_ENV === 'development') {
      console.log(`Section "${section.name}" color: ${section.color} -> ${color}`);
    }
    return color;
  }
  // Fallback to palette if no color
  const paletteIdx = (songIdx * 10 + sectionIdx) % COLOR_PALETTE.length;
  const paletteEntry = COLOR_PALETTE[paletteIdx];
  if (!paletteEntry) {
    return variant === 'bright' ? '#808080' : '#606060'; // Default gray
  }
  return paletteEntry[variant];
}

interface ProgressBarProps {
  song: Song | null;
  songIndex: number;
  transportStates: TransportStates;
  onSectionClick?: (projectName: string, songName: string, sectionName: string) => void;
}

// Helper to get project name from song metadata
const getProjectName = (song: Song): string | null => {
  return song.metadata?.project_name || song.metadata?.Project || song.metadata?.project || null;
};


export const ProgressBar: React.FC<ProgressBarProps> = ({ song, songIndex, transportStates, onSectionClick }) => {
  // Get transport state for this song's project
  const transport = useMemo(() => {
    if (!song) return null;
    const projectName = getProjectName(song);
    if (!projectName) return null;
    return transportStates.get(projectName) || null;
  }, [song, transportStates]);

  // Calculate section widths and info based on musical positions (measures)
  const sectionWidths = useMemo(() => {
    if (!song || !transport || song.sections.length === 0) return [];

    // Helper function to convert musical position to total measures (including fractional beats/subdivisions)
    // subdivision is 0-999, where 1000 = full beat
    const musicalPositionToMeasures = (pos: { measure?: number; beat?: number; subdivision?: number; tick?: number } | null | undefined, beatsPerMeasure: number = 4): number => {
      if (!pos || pos.measure === undefined) return 0;
      const measure = pos.measure ?? 0;
      const beat = pos.beat ?? 0;
      // Use subdivision if available (0-999), otherwise fallback to tick (0-960)
      const subdivision = pos.subdivision ?? pos.tick ?? 0;
      // Subdivision is in thousandths (0-999), so divide by 1000 to get fractional beats
      const fractionalBeat = subdivision / 1000.0;
      return measure + ((beat + fractionalBeat) / beatsPerMeasure);
    };

    // Assume 4/4 time signature (4 beats per measure) - TODO: get from song metadata
    const beatsPerMeasure = 4;

    // Calculate absolute measure positions for each section
    const sectionsWithMeasures = song.sections.map((section, idx) => {
      // Try to get length_measures from metadata (calculated on Rust side)
      let lengthMeasures: number | null = null;
      if (section.metadata?.length_measures) {
        const parsed = parseFloat(section.metadata.length_measures);
        if (!isNaN(parsed) && parsed > 0) {
          lengthMeasures = parsed;
        }
      }
      
      // Use musical_position from start_position and end_position (musical_position field is redundant)
      const startMusical = section.start_position?.musical_position;
      const endMusical = section.end_position?.musical_position;
      
      const startMeasures = musicalPositionToMeasures(startMusical, beatsPerMeasure);
      const endMeasures = musicalPositionToMeasures(endMusical, beatsPerMeasure);
      
      // Use Rust-calculated length_measures if available, otherwise calculate from positions
      if (lengthMeasures === null) {
        lengthMeasures = Math.max(0, endMeasures - startMeasures);
      }
      
      return {
        section,
        name: section.name || section.section_type,
        start: section.start_position?.time?.seconds ?? 0,
        end: section.end_position?.time?.seconds ?? 0,
        startMeasures,
        endMeasures,
        lengthMeasures,
        originalIdx: idx,
        displayIdx: idx,
      };
    });

    // Find the first and last sections to get song boundaries
    const sortedSections = [...sectionsWithMeasures].sort((a, b) => a.startMeasures - b.startMeasures);
    const firstSection = sortedSections[0];
    const lastSection = sortedSections[sortedSections.length - 1];
    
    if (!firstSection || !lastSection) {
      // Fallback to time-based calculation
      const sectionsWithIndices = song.sections.map((section, idx) => ({
        section,
        name: section.name || section.section_type,
        start: section.start_position?.time?.seconds ?? 0,
        end: section.end_position?.time?.seconds ?? 0,
        originalIdx: idx,
        displayIdx: idx,
      }));

      const firstSectionTime = sectionsWithIndices[0];
      const lastSectionTime = sectionsWithIndices[sectionsWithIndices.length - 1];
      
      const calculatedSongStart = firstSectionTime ? firstSectionTime.start : 
                                   song.song_region_start_marker?.position.time.seconds ?? 
                                   song.start_marker?.position.time.seconds ?? 0;
      const calculatedSongEnd = lastSectionTime ? lastSectionTime.end :
                                song.song_region_end_marker?.position.time.seconds ?? 
                                song.end_marker?.position.time.seconds ??
                                song.render_end_marker?.position.time.seconds ??
                                song.song_end_marker?.position.time.seconds ?? calculatedSongStart;
      const songLength = calculatedSongEnd - calculatedSongStart;
      
      if (songLength <= 0) return [];

      return sectionsWithIndices.map(({ section, name, start, end, originalIdx, displayIdx }) => {
        const sectionStartRel = Math.max(0, start - calculatedSongStart);
        const sectionEndRel = Math.max(0, end - calculatedSongStart);
        const sectionWidth = ((sectionEndRel - sectionStartRel) / songLength) * 100;
        const sectionLeft = (sectionStartRel / songLength) * 100;

        const sectionColorBright = getSectionColor(section, songIndex, displayIdx, 'bright');
        const sectionColorMuted = getSectionColor(section, songIndex, displayIdx, 'muted');

        return {
          section,
          name,
          start,
          end,
          originalIdx,
          displayIdx,
          widthPercent: sectionWidth,
          leftPercent: sectionLeft,
          brightColor: sectionColorBright,
          mutedColor: sectionColorMuted,
        };
      });
    }

    // Calculate total song length in measures (from first section start to last section end)
    const songStartMeasures = firstSection.startMeasures;
    const songEndMeasures = lastSection.endMeasures;
    const totalSongMeasures = Math.max(0, songEndMeasures - songStartMeasures);
    
    // If measures calculation fails (all zeros or invalid), fallback to time-based
    if (totalSongMeasures <= 0 || !sectionsWithMeasures.some(s => s.lengthMeasures > 0)) {
      // Fallback to time-based calculation
      const sectionsWithIndices = song.sections.map((section, idx) => ({
        section,
        name: section.name || section.section_type,
        start: section.start_position?.time?.seconds ?? 0,
        end: section.end_position?.time?.seconds ?? 0,
        originalIdx: idx,
        displayIdx: idx,
      }));

      const firstSectionTime = sectionsWithIndices[0];
      const lastSectionTime = sectionsWithIndices[sectionsWithIndices.length - 1];
      
      const calculatedSongStart = firstSectionTime ? firstSectionTime.start : 
                                   song.song_region_start_marker?.position.time.seconds ?? 
                                   song.start_marker?.position.time.seconds ?? 0;
      const calculatedSongEnd = lastSectionTime ? lastSectionTime.end :
                                song.song_region_end_marker?.position.time.seconds ?? 
                                song.end_marker?.position.time.seconds ??
                                song.render_end_marker?.position.time.seconds ??
                                song.song_end_marker?.position.time.seconds ?? calculatedSongStart;
      const songLength = calculatedSongEnd - calculatedSongStart;
      
      if (songLength <= 0) return [];

      return sectionsWithIndices.map(({ section, name, start, end, originalIdx, displayIdx }) => {
        const sectionStartRel = Math.max(0, start - calculatedSongStart);
        const sectionEndRel = Math.max(0, end - calculatedSongStart);
        const sectionWidth = ((sectionEndRel - sectionStartRel) / songLength) * 100;
        const sectionLeft = (sectionStartRel / songLength) * 100;

        const sectionColorBright = getSectionColor(section, songIndex, displayIdx, 'bright');
        const sectionColorMuted = getSectionColor(section, songIndex, displayIdx, 'muted');

        return {
          section,
          name,
          start,
          end,
          originalIdx,
          displayIdx,
          widthPercent: sectionWidth,
          leftPercent: sectionLeft,
          brightColor: sectionColorBright,
          mutedColor: sectionColorMuted,
        };
      });
    }

    // Calculate widths and positions based on absolute measure positions relative to song start
    return sectionsWithMeasures.map(({ section, name, start, end, startMeasures, lengthMeasures, originalIdx, displayIdx }) => {
      // Calculate position relative to song start
      const sectionStartRel = Math.max(0, startMeasures - songStartMeasures);
      
      // Width is the section's length as percentage of total song length
      const widthPercent = (lengthMeasures / totalSongMeasures) * 100;
      // Left position is where the section starts relative to song start
      const leftPercent = (sectionStartRel / totalSongMeasures) * 100;

      const sectionColorBright = getSectionColor(section, songIndex, displayIdx, 'bright');
      const sectionColorMuted = getSectionColor(section, songIndex, displayIdx, 'muted');

      return {
        section,
        name,
        start,
        end,
        originalIdx,
        displayIdx,
        widthPercent,
        leftPercent,
        brightColor: sectionColorBright,
        mutedColor: sectionColorMuted,
      };
    });
  }, [song, songIndex, transport]);

  // Get song boundaries once
  const songBoundaries = useMemo(() => {
    if (!song) return null;
    
    // Sort sections by start position to get correct first/last
    const sortedSections = [...song.sections].sort(
      (a, b) => a.start_position.time.seconds - b.start_position.time.seconds
    );
    
    const songStart = song.song_region_start_marker?.position.time.seconds ?? 
                      song.start_marker?.position.time.seconds ?? 
                      (sortedSections.length > 0 ? sortedSections[0].start_position.time.seconds : 0);
    
    // Get end from last section's end position
    const songEnd = song.song_region_end_marker?.position.time.seconds ?? 
                    song.song_end_marker?.position.time.seconds ?? 
                    (sortedSections.length > 0 ? sortedSections[sortedSections.length - 1].end_position.time.seconds : songStart);
    
    return { songStart, songEnd, songLength: songEnd - songStart };
  }, [song]);

  // Calculate overall song progress (0-100) based on actual REAPER position
  const songProgress = useMemo(() => {
    if (!song || !transport || !songBoundaries) return 0;
    
    const { songStart, songLength } = songBoundaries;
    
    if (songLength <= 0) return 0;
    
    // Calculate progress from actual REAPER position
    const currentPos = transport.position;
    const relativePos = currentPos - songStart;
    
    if (relativePos < 0) return 0;
    if (relativePos > songLength) return 100;
    
    return (relativePos / songLength) * 100;
  }, [song, transport, songBoundaries]);

  // Calculate current section progress (0-100) based on actual REAPER position
  const currentSectionProgress = useMemo(() => {
    if (!song || !transport || song.sections.length === 0) return 0;
    
    const currentPos = transport.position;
    
    // Sort sections by start position to ensure correct order
    const sortedSections = [...song.sections].sort(
      (a, b) => a.start_position.time.seconds - b.start_position.time.seconds
    );
    
    // Find which section we're currently in based on position
    for (const section of sortedSections) {
      const sectionStart = section.start_position.time.seconds;
      const sectionEnd = section.end_position.time.seconds;
      
      if (currentPos >= sectionStart && currentPos <= sectionEnd) {
        // We're in this section, calculate progress within it
        const sectionLength = sectionEnd - sectionStart;
        if (sectionLength <= 0) return 0;
        
        const relativePos = currentPos - sectionStart;
        return Math.max(0, Math.min(100, (relativePos / sectionLength) * 100));
      }
    }
    
    return 0;
  }, [song, transport]);

  if (!song) {
    return (
      <div className="h-full flex items-center justify-center text-muted-foreground">
        <p>No song selected</p>
      </div>
    );
  }

  // If no transport data yet, show song info but no progress
  if (!transport) {
    return (
      <div className="h-full flex flex-col p-6">
        <div className="mb-4">
          <h2 className="text-2xl font-bold mb-2">{song.name}</h2>
          <p className="text-muted-foreground">Waiting for transport data...</p>
        </div>
      </div>
    );
  }

  // Get artist from metadata
  const artist = song.metadata?.artist || song.metadata?.Artist || null;

  return (
    <div className="h-full flex flex-col p-4">
      {/* Top section: Song Title */}
      <div className="text-center mb-4">
        <h1
          className="text-3xl font-bold"
          style={{ color: getSongColor(song, songIndex, 'bright') }}
        >
          {song.name}
        </h1>
        {artist && (
          <p className="text-base text-muted-foreground mt-1">{artist}</p>
        )}
      </div>

      {/* Main content: Vertical layout */}
      <div className="flex-1 flex flex-col justify-center space-y-4 min-h-0">
        {/* Progress Bar Section */}
        <div className="flex flex-col space-y-3">
          {/* Main Progress Bar */}
          <div className="relative h-16 rounded-xl overflow-hidden bg-muted/20 border">
            {sectionWidths.length > 0 ? (
              <>
                {/* Section segments - absolute positioning to maintain chronological order */}
                <div className="relative h-full">
                  {sectionWidths.map(({ section, name, start, end, originalIdx, widthPercent, leftPercent, brightColor, mutedColor }, index) => {
                    // Determine if this section is active based on actual position
                    const currentPos = transport.position;
                    const sectionStart = start;
                    const sectionEnd = end;
                    const isActive = currentPos >= sectionStart && currentPos <= sectionEnd;
                    
                    const handleClick = () => {
                      const sectionProjectName = getProjectName(song);
                      if (onSectionClick && sectionProjectName) {
                        const sectionName = section?.name || section?.section_type || name;
                        onSectionClick(sectionProjectName, song.name, sectionName);
                      }
                    };
                    
                    // Check if there's a gap before this section
                    const prevSection = index > 0 ? sectionWidths[index - 1] : null;
                    const hasGapBefore = prevSection && 
                      leftPercent !== undefined && 
                      prevSection.leftPercent !== undefined &&
                      (leftPercent - (prevSection.leftPercent + prevSection.widthPercent)) > 0.1;
                    
                    const sectionName = section?.name || section?.section_type || name;
                    
                    return (
                      <div
                        key={`section-${originalIdx}`}
                        className="absolute h-full cursor-pointer transition-all duration-200 ease-out hover:opacity-90 group"
                        style={{
                          left: `${leftPercent}%`,
                          width: `${widthPercent}%`,
                          backgroundColor: isActive ? brightColor : mutedColor,
                          borderRight: hasGapBefore || index < sectionWidths.length - 1 ? '2px solid rgba(0,0,0,0.2)' : 'none',
                        }}
                        title={`${sectionName} - Click to jump`}
                        onClick={handleClick}
                      >
                        {/* Hover overlay */}
                        <div
                          className="absolute inset-0 pointer-events-none transition-opacity duration-200 opacity-0 group-hover:opacity-100"
                          style={{
                            background: 'rgba(255, 255, 255, 0.1)',
                            zIndex: 1,
                          }}
                        />
                      </div>
                    );
                  })}
                </div>

                {/* Overall progress gradient overlay */}
                {songProgress > 0 && (
                  <div
                    className="absolute top-0 left-0 h-full pointer-events-none transition-all duration-300"
                    style={{
                      width: `${songProgress}%`,
                      background: 'linear-gradient(90deg, rgba(255,255,255,0) 0%, rgba(255,255,255,0.15) 50%, rgba(255,255,255,0.3) 85%, rgba(255,255,255,0.4) 100%)',
                      zIndex: 2,
                    }}
                  />
                )}
              </>
            ) : (
              <div className="h-full flex items-center justify-center text-muted-foreground text-sm">
                No sections available
              </div>
            )}
          </div>

          {/* Section Progress Bar */}
          {currentSectionProgress > 0 && (
            <div className="h-2 rounded-xl overflow-hidden relative bg-muted/20">
              <div
                className="h-full transition-all duration-200 ease-out"
                style={{
                  width: `${currentSectionProgress}%`,
                  backgroundColor: sectionWidths.length > 0 && sectionWidths[0]?.brightColor ? sectionWidths[0].brightColor : '#3b82f6',
                }}
              />
            </div>
          )}

          {/* Section Labels */}
          {sectionWidths.length > 0 && (
            <div className="relative">
              {sectionWidths.map(({ section, name, start, end, originalIdx, widthPercent, leftPercent, brightColor, mutedColor }) => {
                // Determine if this section is active based on actual position
                const currentPos = transport.position;
                const sectionStart = start;
                const sectionEnd = end;
                const isActive = currentPos >= sectionStart && currentPos <= sectionEnd;
                
                const handleClick = () => {
                  const sectionProjectName = getProjectName(song);
                  if (onSectionClick && sectionProjectName) {
                    const sectionName = section?.name || section?.section_type || name;
                    onSectionClick(sectionProjectName, song.name, sectionName);
                  }
                };
                
                const sectionName = section?.name || section?.section_type || name;
                
                return (
                  <div
                    key={`label-section-${originalIdx}`}
                    className="absolute text-sm text-center font-medium cursor-pointer hover:opacity-80 transition-opacity"
                    style={{
                      left: `${leftPercent}%`,
                      width: `${widthPercent}%`,
                      color: isActive ? brightColor : mutedColor,
                      opacity: isActive ? 1 : 0.6,
                    }}
                    title={`Click to jump to ${sectionName}`}
                    onClick={handleClick}
                  >
                    {sectionName}
                  </div>
                );
              })}
            </div>
          )}
        </div>

        {/* Status Cards */}
        <div className="mt-4 flex justify-center">
          <StatusCards song={song} transport={transport} />
        </div>
      </div>
    </div>
  );
};

