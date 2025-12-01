# Melody Notation

Melodies can be added to charts using Lilypond syntax, either inline with chords or in parallel.

## Variable Definition

Define melodies using either Lilypond syntax or scale degree numbers and store them in variables:

```txt
melody = { c'4 d8 e8 f4 g4 }
intro_riff = { c'8 e g e c4 r4 }
verse_line = { g'4. a8 b4 c'4 }

// Scale degree syntax
scale_melody = { 1_4 2 3_8 4 5 6 7_2 1 }
simple_line = { 1_4 7 6 5 4 3 2 1 }
```

### Lilypond Syntax Basics

Melodies use standard Lilypond notation in **relative mode** (default):

- **Note names**: `c`, `d`, `e`, `f`, `g`, `a`, `b`
- **Relative mode**: Each note is the nearest pitch to the previous note (within a 4th)
- **Octave jumps**:
  - `'` after a note = jump up an octave (e.g., `c'`)
  - `,` after a note = jump down an octave (e.g., `c,`)
  - Multiple jumps: `c''` (up 2 octaves), `c,,` (down 2 octaves)
- **Durations**: `4` (quarter), `8` (eighth), `2` (half), `1` (whole)
- **Dotted notes**: `4.` (dotted quarter)
- **Rests**: `r4` (quarter rest), `r8` (eighth rest)
- **Accidentals**: `cis` (C#), `bes` (Bb), `fis` (F#)

#### Relative Mode Examples

```txt
melody = { g'4 a8 b8 c'4 b8 a8 }
// Starting from g': a and b are close (up), c' jumps octave, b and a go back down

melody2 = { c4 e g c' g e c, }
// Ascending then descending, c, jumps down at the end
```

#### Simultaneous Notes (Chords)

Use `< >` to play multiple notes at the same time:

```txt
// Lilypond syntax
chords = { <c e g>4 <d f a>8 <e g b>8 <f a c'>2 }
// C major chord (quarter), Dm chord (eighth), Em chord (eighth), F major (half)

// Scale degree syntax
chord_pattern = { <1 3 5>4 <2 4 6>4 <3 5 7>2 }
// Play scale degrees simultaneously based on current key
```

**Note:** The duration is applied to the entire chord, not individual notes.

### Scale Degree Syntax

Melodies can also use scale degree numbers (1-7) based on the current key signature:

- **Scale degrees**: `1`, `2`, `3`, `4`, `5`, `6`, `7`
- **Duration setting**: `1_4` attaches duration to the note
- **Duration persists**: Once set, all following notes use that duration until changed

#### Scale Degree Examples

```txt
#C // Key of C major

melody = { 1_4 7 6 5 4 3 2 1 }
// Produces: C4 B4 A4 G4 F4 E4 D4 C4 (all quarter notes)

melody2 = { 1_8 2 3 4_4 5 1_2 }
// Produces: C8 D8 E8 F4 G4 C2
// Duration changes persist until set again
```

#### With Key Signatures

```txt
#G // Key of G major

melody = { 1_4 2 3 4 5 6 7 1 }
// Produces: G4 A4 B4 C4 D4 E4 F#4 G4

#Em // Key of E minor

melody = { 1_8 2 3 4 5 6 7 1 }
// Produces: E8 F#8 G8 A8 B8 C8 D8 E8
```

#### Mixing Durations

```txt
melody = { 1_4 1 2_8 2 2 2 3_4. 4_8 5_2 }
// C4 C4 D8 D8 D8 D8 E4. F8 G2
```

## Inline Melodies

Insert melodies at specific points in the chord progression using `\variable`:

```txt
Verse
G C \melody Em D
```

**Behavior**: The melody starts when the `C` chord is playing and continues through subsequent chords.

### Inline Melody Definition

You can define melodies inline without creating variables using multiple syntax options:

```txt
Verse
G C m{ g'4 a8 b8 c'4 } Em D

// All of these are equivalent:
G m{ 1_4 2 3 } D
G mel{ 1_4 2 3 } D
G melody{ 1_4 2 3 } D
G \melody{ 1_4 2 3 } D
```

**Parsing:** The parser detects `{` directly after `r`, `m`, `rhy`, `mel`, `rhythm`, `melody`, or `\rhythm`, `\melody`.

**Note:** A standalone `r` followed by a number is a rest (e.g., `r4`), but `r{...}` defines an inline rhythm.

### Multiple Inline Melodies

```txt
Verse
\intro_riff G C \verse_line Em D m{ 1_4 7 6 5 }
```

**Behavior**:

- `intro_riff` plays during the `G` chord
- `verse_line` plays starting at `Em`
- Inline melody `m{ 1_4 7 6 5 }` plays at the end

## Inline Rhythms

Define custom rhythms inline for chords using the same syntax shortcuts:

```txt
Verse
G r{ 4 8 8 } C rhy{ 4. 8 } Em rhythm{ 2 }

// All of these are equivalent:
G r{ 4 8 8 } D
G rhy{ 4 8 8 } D
G rhythm{ 4 8 8 } D
G \rhythm{ 4 8 8 } D
```

**Behavior**: Inline rhythms apply to the chord they follow, overriding default "one chord = one bar" behavior.

### Combining Inline Melodies and Rhythms

```txt
Chorus
C r{ 4 4 2 } m{ 5_4 4 3_2 } G r{ 8 8 8 8 } Am F
```

**Behavior**:

- `C` plays with custom rhythm (quarter, quarter, half)
- Melody plays simultaneously with the `C` chord
- `G` plays with custom rhythm (four eighth notes)
- `Am` and `F` use default rhythm (one bar each)

## Parallel Melodies

Use `<< >>` syntax to make melodies play in complete parallel with the chord progression:

```txt
Verse
<<
\melody
G C Em D
>>
```

**Behavior**: The melody and chord progression start at the exact same time and run in parallel.

### Multiple Parallel Elements

```txt
Chorus
<<
\melody
\harmony
G C Am F
>>
```

**Behavior**: Both `melody` and `harmony` play in parallel with the chord progression, all starting simultaneously.

## Combining Inline and Parallel

You can mix both approaches:

```txt
Bridge
<<
\pad
G C
>>
D \melody Em F
```

**Behavior**:

- `pad` plays in parallel with `G C`
- `melody` starts inline with the `D` chord

## Custom Rhythm Notation

For complex rhythmic patterns, use Lilypond duration syntax directly in parallel:

```txt
Intro
<<
\melody
{
  G4 G8 G8 C4. C8
  Em4 Em8 r8 D4 D4
}
>>
```

**Behavior**: The rhythm pattern in `{ }` defines exact chord timing using Lilypond notation.

### Rhythm Syntax

Inside `{ }` blocks, use chord notation with Lilypond durations:

```txt
Verse
{
  Gmaj7_4 Gmaj7_8 Gmaj7_8 Cadd9_2
  Em7_4. D_8 Gmaj7_2
}
```

## Full Example

```txt
Song Title - Artist Name
120bpm 4/4 #G

// Lilypond syntax melody
melody = { g'4 a8 b8 c'4 b8 a8 }
intro_riff = { e'8 g b g e4 r4 }
harmony = { e'2 d2 }

// Scale degree syntax melody
verse_melody = { 1_4 2 3_8 4 4 5_4 1_2 }
chorus_line = { 5_4 4 3 2 1 7 1_2 }

Intro
<<
\intro_riff
G C Em D
>>

Verse
G C \verse_melody Em D

Chorus
<<
\chorus_line
\harmony
C G Am F
>>

Bridge
{
  G4 C8 C8 Em4 D4
}
```

## Best Practices

1. **Variable Naming**: Use descriptive names (`verse_melody`, `chorus_line`, `intro_riff`)
2. **Octave Consistency**: Choose appropriate octaves for your instrument range
3. **Duration Matching**: Ensure melody durations align with chord progression length
4. **Comments**: Add comments to explain complex patterns

```txt
// Main hook melody - 4 bars
melody = { c'4 d'8 e'8 f'4 g'4 a'2 b'2 c''1 }

// Background pad - sustained whole notes
pad = { c'1 d'1 e'1 f'1 }
```

## Technical Notes

- **Parallel timing**: `<< >>` starts all elements at the same time
- **Inline timing**: `\variable` starts when it appears in the sequence
- **Lilypond compatibility**: All standard Lilypond note/rhythm syntax is supported
- **Variables must be defined** before use in sections
