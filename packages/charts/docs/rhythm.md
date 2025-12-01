# Rhythm Notation

This document covers advanced rhythm notation for chords, including inline rhythms, rhythm variables, and custom patterns.

## Overview

By default, every chord occupies one bar. Rhythm notation allows you to override this behavior with precise timing control.

## Variable Definition

Store reusable rhythm patterns in variables:

```txt
verse_pattern = { 4 8 8 4 4 }
funk_groove = { 8 8. 16 8 8 4 }
half_time = { 2 2 }
```

### Rhythm Syntax

Rhythms use Lilypond duration values:

- **Durations**: `4` (quarter), `8` (eighth), `16` (sixteenth), `2` (half), `1` (whole)
- **Dotted durations**: `4.` (dotted quarter), `8.` (dotted eighth)
- **Rests**: `r4` (quarter rest), `r8` (eighth rest)
- **Spaces**: `s4` (quarter space - silent but takes time)

### Examples

```txt
// Basic patterns
straight_eighths = { 8 8 8 8 8 8 8 8 }
dotted_pattern = { 4. 8 4. 8 }
syncopated = { 8 8 4 8 8 4 }

// With rests
sparse = { 4 r4 4 r4 }
staccato = { 8 r8 8 r8 8 r8 8 r8 }

// With spaces (tacet)
breaks = { 4 4 s2 }
```

## Inline Rhythms

Apply rhythms directly to chords without creating variables:

```txt
Verse
G r{ 4 8 8 } C rhy{ 4. 8 } Em rhythm{ 2 }

// All of these are equivalent:
G r{ 4 8 8 } D
G rhy{ 4 8 8 } D
G rhythm{ 4 8 8 } D
G \rhythm{ 4 8 8 } D
```

### Parsing Disambiguation

The parser distinguishes between rests and rhythm blocks:

- `r4` = quarter note rest (Lilypond)
- `r{ 4 8 8 }` = inline rhythm definition

Detection is based on `{` appearing directly after `r`, `rhy`, `rhythm`, or `\rhythm`.

### Inline Rhythm Examples

```txt
Intro
Gmaj7 r{ 4. 8 4 4 } Cadd9 r{ 8 8 8 8 2 }

Verse
G r{ 4 4 2 } C r{ 8 8 4 8 8 4 } Em D

// Mixed with default timing
G r{ 4 8 8 } C D Em
// G has custom rhythm, C/D/Em use default (one bar each)
```

## Using Rhythm Variables

Reference stored patterns with `\variable`:

```txt
funk_groove = { 8 8. 16 8 8 4 }
verse_rhythm = { 4. 8 4. 8 }

Verse
G \funk_groove C \verse_rhythm Em D

// Variables can be reused
Chorus
C \funk_groove G \funk_groove Am F
```

## Combining with Chord Memory

Rhythms work seamlessly with chord memory:

```txt
Intro
Gmaj7 r{ 4. 8 4 4 }

Verse
g r{ 8 8 8 8 2 }
// 'g' expands to Gmaj7 with new rhythm
```

## Multipliers

Repeat rhythms using `*`:

```txt
Intro
G r{ 4*4 }     // Four quarter notes
C r{ 8*8 }     // Eight eighth notes
Em r{ s1*4 }   // Four bars of space
```

## Per-Chord Rhythm Syntax

For precise control, attach rhythms directly to chords using `_`:

```txt
Verse
Gmaj7_4. Cadd9_8 Em7_4 D_4
```

This is covered in detail in [Chord Rhythms](./chord-rhythm.md).

## Parallel Rhythm Blocks

Use `<< >>` for complex multi-voice rhythms:

```txt
Intro
<<
r{ 4 4 2 }
G C Em D
>>
```

**Behavior**: The rhythm pattern and chords play in complete parallel, starting simultaneously.

## Rhythm with Slash Notation

Combine rhythm variables with slash notation:

```txt
Verse
G //// r{ 4 8 8 4 4 } C //// D
```

The inline rhythm overrides the slash notation when present.

## Complex Rhythm Patterns

### Syncopation

```txt
syncopated = { 8 8 4 8. 16 4 }

Verse
G \syncopated C \syncopated Em D
```

### Polyrhythms

```txt
pattern_a = { 4 4 4 4 }
pattern_b = { 4. 8 4. 8 }

Bridge
<<
\pattern_a
\pattern_b
G C Em D
>>
```

### Metric Modulation

```txt
Verse
G r{ 4 4 2 }

Chorus
G r{ 8 8 8 8 8 8 8 8 }  // Double time feel
```

## Practical Examples

### Reggae Skank

```txt
reggae = { r8 8 r8 8 r8 8 r8 8 }

Verse
G \reggae C \reggae Em \reggae D \reggae
```

### Bossa Nova

```txt
bossa = { 4 8 8 4 8 8 }

Verse
Gmaj7 \bossa Em7 \bossa Am7 \bossa D7 \bossa
```

### Half-Time Feel

```txt
half_time = { 2 2 }

Bridge
G \half_time C \half_time Em \half_time D \half_time
```

### Funk Stabs

```txt
stabs = { 8 r8 8 r4 8 r4 }

Intro
G \stabs C \stabs Em \stabs D \stabs
```

## Integration with Time Signatures

Rhythm patterns should match the time signature:

```txt
Song Title - Artist Name
120bpm 4/4 #G

four_four = { 4 4 4 4 }      // 4/4 time
six_eight = { 8 8 8 8 8 8 }  // 6/8 time

Verse
G \four_four C \four_four
```

For 6/8 time, dotted notation works well:

```txt
six_eight_dotted = { 4. 4. }

Verse
G \six_eight_dotted C \six_eight_dotted
```

## Best Practices

1. **Name patterns descriptively**: `verse_rhythm`, `chorus_groove`, `bridge_pattern`
2. **Keep patterns bar-aligned**: Ensure durations sum to full bars
3. **Use variables for repetition**: Don't repeat the same inline rhythm
4. **Comment complex patterns**: Explain unusual or intricate rhythms
5. **Test with time signature**: Verify patterns fit the meter

```txt
// Funky syncopated groove - emphasizes offbeats
funk_groove = { 8 8. 16 8 r8 8 8 }

// Sparse ballad feel - lots of space
ballad_sparse = { 2 s1 2 s1 }
```

## Technical Notes

- **Duration persistence**: Not applicable to rhythm variables (each value is independent)
- **Bar boundaries**: Rhythms don't automatically wrap to new bars
- **Rests vs Spaces**: Use `r` for explicit silence, `s` for tacet/empty time
- **Inline priority**: Inline rhythms override default timing and slash notation
- **Parallel timing**: `<< >>` starts all elements simultaneously
