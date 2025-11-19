import React, { useState, useMemo } from 'react';
import { ChevronRight, ChevronDown } from 'lucide-react';
import { ScrollArea } from '@/components/ui/scroll-area';
import { Progress } from '@/components/ui/progress';
import { cn } from '@/lib/utils';
import { Song, Section, TransportStates } from '@/atoms/types';

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
    // Convert to HSL, reduce saturation and lightness, then back to RGB
    // Simplified: reduce brightness by 30%
    const mutedR = Math.floor(r * 0.7);
    const mutedG = Math.floor(g * 0.7);
    const mutedB = Math.floor(b * 0.7);
    return `#${[mutedR, mutedG, mutedB].map(x => x.toString(16).padStart(2, '0')).join('')}`;
  }
  
  return hex;
}

function getSongColor(song: Song, variant: 'bright' | 'muted' = 'bright'): string {
  // Debug: Log all available color sources
  if (process.env.NODE_ENV === 'development') {
    console.log(`[Navigator] Song "${song.name}" color sources:`, {
      song_region_start_marker: song.song_region_start_marker?.color ?? null,
      start_marker: song.start_marker?.color ?? null,
      has_song_region_start_marker: !!song.song_region_start_marker,
      has_start_marker: !!song.start_marker,
    });
  }
  
  // Get color from song_region_start_marker first (this preserves the song region color)
  if (song.song_region_start_marker?.color != null && song.song_region_start_marker.color !== 0) {
    if (process.env.NODE_ENV === 'development') {
      console.log(`[Navigator] Song "${song.name}" using region color:`, song.song_region_start_marker.color);
    }
    return colorFromMetadata(song.song_region_start_marker.color.toString(), variant);
  }
  // Fallback to start_marker if available
  if (song.start_marker?.color != null && song.start_marker.color !== 0) {
    if (process.env.NODE_ENV === 'development') {
      console.log(`[Navigator] Song "${song.name}" using start_marker color:`, song.start_marker.color);
    }
    return colorFromMetadata(song.start_marker.color.toString(), variant);
  }
  // Fallback to palette if no color in marker
  if (process.env.NODE_ENV === 'development') {
    console.log(`[Navigator] Song "${song.name}" using palette fallback (no color found)`);
  }
  const paletteIdx = (song.metadata?.index as number) || 0;
  const paletteEntry = COLOR_PALETTE[paletteIdx % COLOR_PALETTE.length];
  if (!paletteEntry) {
    return variant === 'bright' ? '#808080' : '#606060'; // Default gray
  }
  return paletteEntry[variant];
}

function getSectionColor(section: Section, variant: 'bright' | 'muted' = 'bright'): string {
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
  const paletteEntry = COLOR_PALETTE[0];
  if (!paletteEntry) {
    return variant === 'bright' ? '#808080' : '#606060'; // Default gray
  }
  return paletteEntry[variant];
}

interface NavigatorProps {
  songs: Song[];
  transportStates: TransportStates;
  selectedSongIndex?: number;
  selectedSectionIndex?: number | null;
  onSongClick?: (songIndex: number, projectName: string) => void;
  onSectionClick?: (songIndex: number, sectionIndex: number, projectName: string, songName: string, sectionName: string) => void;
}

// Helper to get project name from song metadata
const getProjectName = (song: Song): string | null => {
  return song.metadata?.project_name || song.metadata?.Project || song.metadata?.project || null;
};

export const Navigator: React.FC<NavigatorProps> = ({
  songs,
  transportStates,
  selectedSongIndex,
  selectedSectionIndex,
  onSongClick,
  onSectionClick,
}) => {
  const [expandedSongs, setExpandedSongs] = useState<Set<number>>(new Set());

  const toggleSong = (songIdx: number) => {
    setExpandedSongs((prev) => {
      const next = new Set(prev);
      if (next.has(songIdx)) {
        next.delete(songIdx);
      } else {
        next.add(songIdx);
      }
      return next;
    });
  };

  // Calculate song progress for a specific song (uses pre-calculated progress from backend)
  const getSongProgress = (songIdx: number): number => {
    if (!songs[songIdx]) return 0;

    const song = songs[songIdx];
    const projectName = getProjectName(song);
    if (!projectName) return 0;

    const transport = transportStates.get(projectName);
    if (!transport) return 0;

    // Use pre-calculated progress from backend (0-1)
    // Show progress for active project even if not playing
    return transport.song_progress[song.name] ?? 0;
  };

  // Calculate section progress for a specific section (uses pre-calculated progress from backend)
  const getSectionProgress = (songIdx: number, sectionIdx: number): number => {
    if (!songs[songIdx]) return 0;

    const song = songs[songIdx];
    const section = song.sections[sectionIdx];
    if (!section) return 0;

    const projectName = getProjectName(song);
    if (!projectName) return 0;

    const transport = transportStates.get(projectName);
    if (!transport) return 0;

    // Use pre-calculated progress from backend (0-1)
    // Show progress for active project even if not playing
    const key = `${song.name}:${section.name}`;
    return transport.section_progress[key] ?? 0;
  };

  return (
    <div className="h-full flex flex-col border-r bg-background overflow-hidden">
      <div className="p-4 border-b flex-shrink-0">
        <h2 className="text-lg font-semibold">Navigator</h2>
        <p className="text-sm text-muted-foreground">{songs.length} song{songs.length !== 1 ? 's' : ''}</p>
      </div>
      <ScrollArea className="flex-1 min-h-0">
        <div className="p-2 space-y-1">
          {songs.map((song, songIdx) => {
            const isExpanded = expandedSongs.has(songIdx);
            const isSelected = selectedSongIndex === songIdx;
            const songProgress = getSongProgress(songIdx);
            const songColorBright = getSongColor(song, 'bright');
            const songColorMuted = getSongColor(song, 'muted');

            return (
              <div key={songIdx} className="space-y-1">
                {/* Song Item - Entire card acts as progress bar */}
                <div
                  className={cn(
                    'group relative rounded-md p-2 cursor-pointer transition-all overflow-hidden',
                    isSelected && selectedSectionIndex === null && 'border border-primary/40'
                  )}
                  onClick={(e) => {
                    e.stopPropagation();
                    const projectName = getProjectName(song);
                    if (projectName && onSongClick) {
                      onSongClick(songIdx, projectName);
                    }
                    toggleSong(songIdx);
                  }}
                >
                  {/* Muted background color - always visible but very subtle */}
                  <div
                    className="absolute inset-0"
                    style={{
                      backgroundColor: songColorMuted,
                      opacity: 0.08,
                    }}
                  />
                  
                  {/* Progress fill overlay - fills from left */}
                  <div
                    className="absolute inset-0 transition-all duration-300"
                    style={{
                      width: `${songProgress * 100}%`,
                      backgroundColor: songProgress > 0 ? songColorBright : songColorMuted,
                      opacity: songProgress > 0 ? 0.5 : 0.2,
                    }}
                  />
                  
                  {/* Content on top */}
                  <div className="relative z-10 flex items-center gap-2">
                    {isExpanded ? (
                      <ChevronDown className="h-4 w-4 text-white" />
                    ) : (
                      <ChevronRight className="h-4 w-4 text-white" />
                    )}
                    <span
                      className={cn(
                        'text-sm font-medium flex-1 text-white',
                        isSelected && selectedSectionIndex === null && 'text-primary'
                      )}
                    >
                      {songIdx + 1}. {song.name}
                    </span>
                  </div>
                </div>

                {/* Sections */}
                {isExpanded &&
                  song.sections.map((section, sectionIdx) => {
                    const isSectionSelected =
                      isSelected && selectedSectionIndex === sectionIdx;
                    const sectionProgress = getSectionProgress(songIdx, sectionIdx);
                    const sectionColorBright = getSectionColor(songIdx, sectionIdx, 'bright');
                    const sectionColorMuted = getSectionColor(songIdx, sectionIdx, 'muted');

                    return (
                      <div
                        key={sectionIdx}
                        className={cn(
                          'ml-6 rounded-md p-2 cursor-pointer transition-all overflow-hidden relative',
                          isSectionSelected && 'border border-primary/40'
                        )}
                        onClick={(e) => {
                          e.stopPropagation();
                          const projectName = getProjectName(song);
                          if (projectName && onSectionClick) {
                            onSectionClick(songIdx, sectionIdx, projectName, song.name, section.name || section.section_type);
                          }
                        }}
                      >
                        {/* Muted background color - always visible but very subtle */}
                        <div
                          className="absolute inset-0"
                          style={{
                            backgroundColor: sectionColorMuted,
                            opacity: 0.06,
                          }}
                        />
                        
                        {/* Progress fill overlay - fills from left */}
                        <div
                          className="absolute inset-0 transition-all duration-300"
                          style={{
                            width: `${sectionProgress * 100}%`,
                            backgroundColor: sectionProgress > 0 ? sectionColorBright : sectionColorMuted,
                            opacity: sectionProgress > 0 ? 0.5 : 0.2,
                          }}
                        />
                        
                        {/* Content on top */}
                        <div className="relative z-10 flex items-center gap-2">
                          <span className="text-xs text-white">›</span>
                          <span
                            className={cn(
                              'text-xs flex-1 text-white',
                              isSectionSelected && 'text-primary font-medium'
                            )}
                          >
                            {section.name || section.section_type}
                          </span>
                        </div>
                      </div>
                    );
                  })}
              </div>
            );
          })}
        </div>
      </ScrollArea>
    </div>
  );
};

