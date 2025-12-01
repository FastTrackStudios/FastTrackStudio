# Song Sections

Song sections organize your chart into distinct parts with automatic numbering, length tracking, and smart section memory.

## Section Types

### Standard Sections

```txt
Intro           // in, IN
Verse           // vs, VS
Chorus          // ch, CH
Bridge          // br, BR
Instrumental    // inst, INST
Outro           // out, OUT
```

**Full name or abbreviation** can be used (case-insensitive).

### Pre and Post Sections

Attach `pre` or `post` before another section type:

```txt
Pre-Chorus      // pre ch, pre chorus
Post-Chorus     // post ch, post chorus
Pre-Verse       // pre vs, pre verse
Post-Bridge     // post br, post bridge
```

**Behavior**: Pre/Post sections inherit the name of the section they attach to and are not numbered.

## Basic Section Definition

### Simple Sections

```txt
Verse
G C Em D

Chorus
C G Am F
```

**Default**: Section length is determined by chord count (one chord = one bar).

### Sections with Explicit Length

```txt
Bridge 8
// 8 bars, length determined by number

Instrumental 4
// 4 bars of instrumental
```

**Behavior**:

- If chords are present, chord count overrides explicit length
- If no chords, explicit length is used

## Section Abbreviations

All sections can be abbreviated:

```txt
in 4         // Intro 4
vs           // Verse
ch           // Chorus
br 8         // Bridge 8
inst 2       // Instrumental 2
out 4        // Outro 4
```

Abbreviations are case-insensitive: `vs`, `VS`, `Vs` all work.

## Auto-Numbering

Sections are automatically numbered when they appear more than once:

```txt
Intro
Verse         // Verse 1 (first occurrence)
Chorus        // Chorus 1 (first occurrence)
Verse         // Verse 2 (second occurrence)
Chorus        // Chorus 2 (second occurrence)
Bridge        // Bridge (only one, so no number)
Outro
```

**Rules**:

- Sections are only numbered if there's more than one of that type
- `Intro`, `Outro`, `Pre`, and `Post` sections are never numbered
- Numbering is automatic—you don't specify the number

### Consecutive Sections (Split Letters)

When the same section type appears consecutively, they share a number with split letters:

```txt
Verse         // Verse 1a
Verse         // Verse 1b
Chorus        // Chorus 1
Verse         // Verse 2
Verse         // Verse 2a
Verse         // Verse 2b
```

**Behavior**:

- First of consecutive = 'a', second = 'b', third = 'c', etc.
- Split letters indicate these sections share the same musical content but are separated in the form

## Section Length

### Length from Chords

```txt
Verse
G C Em D
// 4 bars (4 chords)

Chorus
C G Am F G C
// 6 bars (6 chords)
```

### Explicit Length (No Chords)

```txt
Intro 4
// 4 bars, no specific chords defined

Instrumental 8
// 8 bars of instrumental break
```

### Chord Count Overrides Explicit Length

```txt
Bridge 8
G C Em D
// 4 bars (chord count overrides the "8")
```

## Section Memory and Templates

Sections remember their chord progressions and can be reused:

```txt
Verse
G C Em D

Chorus
C G Am F

Verse
// Automatically uses: G C Em D (copied from first Verse)

Chorus
// Automatically uses: C G Am F (copied from first Chorus)
```

**Behavior**:

- First occurrence of a section type defines its template
- Subsequent occurrences without explicit chords copy the template
- Each section type has its own template

### Overriding Templates

```txt
Verse
G C Em D

Verse
// Uses: G C Em D (from template)

Verse
Am F G C
// Overrides with new progression
```

## Pre and Post Sections

### Pre-Sections

```txt
Verse
G C Em D

Pre-Chorus
Am F G
// Or: pre ch, pre chorus

Chorus
C G Am F
```

**Output**: "Pre-Chorus" (not numbered)

### Post-Sections

```txt
Chorus
C G Am F

Post-Chorus
Em F G
// Or: post ch, post chorus

Bridge
```

**Output**: "Post-Chorus" (not numbered)

### Pre/Post with Different Targets

```txt
Pre-Verse       // Before a verse
Pre-Chorus      // Before a chorus
Post-Chorus     // After a chorus
Post-Bridge     // After a bridge
```

## Full Chart Example

```txt
Song Title - Artist Name
120bpm 4/4 #G

Intro 4
G C Em D

Verse
G C Em D
e d e d

Pre-Chorus
Am F G

Chorus
C G Am F

Verse
// Uses template from first Verse

Pre-Chorus
// Uses template from first Pre-Chorus

Chorus
// Uses template from first Chorus

Bridge 8
Em F G Am

Chorus
// Uses template

Post-Chorus
G Am F C

Outro 4
```

**Output Structure**:

- Intro (no number)
- Verse 1a
- Verse 1b
- Pre-Chorus (no number)
- Chorus 1
- Verse 2
- Pre-Chorus (no number, uses template)
- Chorus 2
- Bridge (no number, only one)
- Chorus 3
- Post-Chorus (no number)
- Outro (no number)

## Section-Specific Chord Memory

Each section type maintains its own chord memory:

```txt
Intro
Gmaj7 C Em D

Verse
G7 C Em D
// G7 becomes the Verse's version of G

Chorus
g c e d
// In Chorus, 'g' uses Gmaj7 (from Intro/global)
// Verse's G7 doesn't affect Chorus

Verse
g c e d
// In Verse, 'g' uses G7 (section-specific memory)
```

**Behavior**:

- Each section type has its own chord memory
- First time a chord is defined in a section, it's stored for that section type
- Sections without their own memory inherit from global memory at that point in time

## Inline Section Markers

For compact charts, sections can be written inline:

```txt
in | vs | ch | vs | ch | br | ch | out
```

This creates the structure without defining chords (templates must be defined elsewhere or chords provided inline).

## Section Properties in Output

Each section generates:

1. **Section marker**: `\mark "VS 1"` (LilyPond format)
2. **Length**: Number of measures (bars)
3. **Number**: Auto-assigned based on occurrences
4. **Split letter**: For consecutive sections ('a', 'b', 'c')
5. **Chord progression**: From explicit definition or template

## Best Practices

1. **Define templates early**: Set up your verse/chorus progressions in the first occurrence
2. **Use abbreviations**: `vs` and `ch` are faster than `Verse` and `Chorus`
3. **Explicit lengths for instrumentals**: Always specify bar count for sections without chords
4. **Pre/Post for transitions**: Use Pre-Chorus/Post-Chorus for distinct transition sections
5. **Let auto-numbering work**: Don't manually number sections—the system handles it

## Technical Notes

- **Section detection**: Parser identifies sections by abbreviation or full name
- **Template storage**: First occurrence of each section type stores its progression
- **Memory inheritance**: New section types inherit latest global chord definitions
- **Split letter assignment**: Based on consecutive same-type sections
- **Length calculation**: Chord count > explicit length > default (1 bar)
- **Pre/Post attachment**: `pre` looks ahead, `post` looks back for target section type
