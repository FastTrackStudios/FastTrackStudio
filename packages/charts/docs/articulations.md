# Articulations

Articulations modify how notes are played, adding expression and dynamics to your chart.

## Overview

Articulations are added after notes or chords using special symbols or Lilypond commands.

## Articulation Types

### Accent (`->`)

**Effect**: Play the note louder/stronger than surrounding notes

```txt
melody = { c'4-> d8 e8-> f4 g4 }

Verse
G-> C Em-> D
```

**Lilypond**: `\accent` or `->` (shorthand)

**Usage**: Emphasize specific beats or syncopation

### Staccato (`-.`)

**Effect**: Play the note short and detached

```txt
melody = { c'4-. d8-. e8-. f4 g4 }

Chorus
G-. C-. Em-. D-.
```

**Lilypond**: `\staccato` or `-.` (shorthand)

**Usage**: Bouncy, detached feel

### Tenuto (`--`)

**Effect**: Hold the note for its full value (slight emphasis)

```txt
melody = { c'4-- d8 e8 f4-- g4 }

Bridge
G-- C Em D--
```

**Lilypond**: `\tenuto` or `--` (shorthand)

**Usage**: Smooth, sustained notes

### Marcato (`-^`)

**Effect**: Strong accent with separation (louder than accent)

```txt
melody = { c'4-^ d8 e8-^ f4 g4 }

Intro
G-^ C-^ Em-^ D-^
```

**Lilypond**: `\marcato` or `-^` (shorthand)

**Usage**: Very emphasized hits, stabs

### Staccatissimo (`-!`)

**Effect**: Very short, extremely detached (shorter than staccato)

```txt
melody = { c'4-! d8-! e8-! f4 g4 }

Verse
G-! C-! Em-! D-!
```

**Lilypond**: `\staccatissimo` or `-!` (shorthand)

**Usage**: Percussive, very short notes

### Portato (`-_`)

**Effect**: Slightly detached but not as short as staccato (half-staccato)

```txt
melody = { c'4-_ d8 e8-_ f4 g4 }

Chorus
G-_ C-_ Em-_ D-_
```

**Lilypond**: `\portato` or `-_` (shorthand)

**Usage**: Gentle separation between notes

### Espressivo

**Effect**: Play expressively (combination of accent and tenuto)

```txt
melody = { c'4\espressivo d8 e8 f4\espressivo g4 }

Verse
G\espressivo C Em\espressivo D
```

**Lilypond**: `\espressivo` (no shorthand)

**Usage**: Emotional, expressive passages

### Fermata

**Effect**: Hold the note longer than its written value (pause)

```txt
melody = { c'4 d8 e8 f4\fermata g4 }

Outro
G C Em D\fermata
```

**Lilypond**: `\fermata` (no shorthand)

**Usage**: Dramatic pauses, endings

## Using Articulations with Chords

### Single Chord

```txt
Verse
G-> C Em-. D
```

### Multiple Articulations

```txt
Chorus
G->-. C-- Em-^ D\fermata
```

**Note**: You can combine compatible articulations (accent + staccato, etc.)

## Using Articulations with Melodies

### Lilypond Syntax

```txt
melody = { c'4-> d8-. e8-. f4-- g4\fermata }

Verse
G C \melody Em D
```

### Scale Degree Syntax

```txt
melody = { 1_4-> 2_8-. 3-. 4_4-- 5\fermata }

Verse
G C \melody Em D
```

## Articulation Combinations

### Accent + Staccato (`->-.`)

```txt
melody = { c'4->-. d8 e8->-. f4 g4 }
```

**Effect**: Short and accented (punchy stabs)

### Accent + Tenuto (`->--`)

```txt
melody = { c'4->-- d8 e8 f4->-- g4 }
```

**Effect**: Emphasized and sustained

### Marcato + Staccato (`-^-.`)

```txt
melody = { c'4-^-. d8 e8-^-. f4 g4 }
```

**Effect**: Very strong, short hits

## Articulations in Rhythm Patterns

```txt
funk_pattern = { 8->-. 8-. 4-- 8->-. 8-. 4 }

Verse
G r{ 8->-. 8-. 4-- 8->-. 8-. 4 } C
```

**Usage**: Add articulations to rhythm variables for consistent feel

## Common Patterns

### Staccato Eighths (Funky)

```txt
verse_pattern = { 8-. 8-. 8-. 8-. 8-. 8-. 8-. 8-. }

Verse
G \verse_pattern C \verse_pattern
```

### Accented Downbeats

```txt
accent_pattern = { 4-> 4 4 4 }

Verse
G r{ 4-> 4 4 4 } C r{ 4-> 4 4 4 }
```

### Marcato Hits

```txt
stabs = { 8-^ 8-^ r4 8-^ 8-^ r4 }

Intro
G r{ 8-^ 8-^ r4 8-^ 8-^ r4 } C
```

### Fermata on Final Chord

```txt
Outro
G C Em D\fermata
```

## Articulation Reference Table

| Name          | Shorthand | Long Form        | Effect                |
| ------------- | --------- | ---------------- | --------------------- |
| Accent        | `->`      | `\accent`        | Louder, emphasized    |
| Staccato      | `-.`      | `\staccato`      | Short, detached       |
| Tenuto        | `--`      | `\tenuto`        | Full value, sustained |
| Marcato       | `-^`      | `\marcato`       | Very strong accent    |
| Staccatissimo | `-!`      | `\staccatissimo` | Extremely short       |
| Portato       | `-_`      | `\portato`       | Slightly detached     |
| Espressivo    | (none)    | `\espressivo`    | Expressive            |
| Fermata       | (none)    | `\fermata`       | Hold/pause            |

## Position in Syntax

Articulations come **after** the note/chord and **before** the duration:

```txt
// Correct
c'->4    // Note, articulation, duration
G->      // Chord, articulation

// Also correct
c'4->    // Note, duration, articulation (alternative)

// In variables
melody = { c'4-> d8-. e8-. }
```

## Best Practices

1. **Use sparingly**: Too many articulations can clutter the chart
2. **Be consistent**: If you accent downbeats in verse, do it throughout
3. **Match style**: Staccato for funk/disco, tenuto for ballads
4. **Fermata on endings**: Common on final chord
5. **Document intent**: Use text cues to clarify articulation intent

```txt
Verse
@all "Staccato funk feel"
G-. C-. Em-. D-.
```

## Examples by Genre

### Funk

```txt
Verse
G->-. C-. Em-. D->-.
// Accented downbeats, staccato eighth notes
```

### Ballad

```txt
Verse
G-- C-- Em-- D--
// Sustained, legato feel
```

### Rock

```txt
Chorus
G-> C-> Em-> D->
// Driving, accented quarter notes
```

### Reggae

```txt
Verse
G-. r8 C-. r8 Em-. r8 D-. r8
// Staccato on offbeats
```

### Disco

```txt
Verse
G-.-.-. C-.-.-. Em-.-.-. D-.-.-.
// Four staccato hits per bar
```

## Technical Notes

- **Lilypond rendering**: Articulations appear as symbols above/below notes
- **Playback**: MIDI playback may interpret articulations (if supported)
- **Combining**: Most articulations can be combined (e.g., `->-.`)
- **Order**: Shorthand order doesn't matter (`->-.` = `-.->`), but consistency helps readability
- **Long form**: Use `\accent`, `\staccato`, etc., for clarity in complex passages
- **Chord articulations**: Apply to entire chord duration, not individual notes
