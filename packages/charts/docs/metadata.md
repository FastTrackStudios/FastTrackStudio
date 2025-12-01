# Song Metadata

Song metadata provides essential information about your chart at the very start of the file.

## Basic Metadata Format

All metadata appears at the beginning of the chart, before any sections:

```txt
Song Title - Artist Name
120bpm 4/4 #G

Intro
G C Em D
```

## Metadata Components

### Title and Artist

```txt
Reckless Love - Cory Asbury
```

**Format**: `Title - Artist`

- **Title**: Song name
- **Artist**: Performer or composer
- **Separator**: Space-dash-space (`-`)

### Tempo

```txt
120bpm
```

**Format**: `<number>bpm`

- **Number**: Beats per minute (e.g., `68`, `120`, `140`)
- **Suffix**: `bpm` (required)

**Examples**:

```txt
68bpm     // Slow ballad
120bpm    // Moderate tempo
180bpm    // Fast rock
```

### Time Signature

```txt
4/4
6/8
3/4
7/8
```

**Format**: `<numerator>/<denominator>`

- **Numerator**: Number of beats per measure
- **Denominator**: Note value that gets one beat

**Common Time Signatures**:

```txt
4/4    // Common time (most rock/pop)
6/8    // Compound duple (ballads, some pop)
3/4    // Waltz time
2/4    // March time
5/4    // Odd meter (Take Five)
7/8    // Odd meter (Money by Pink Floyd)
12/8   // Compound quadruple (slow blues)
```

### Key Signature

```txt
#G      // G major
#Em     // E minor
#F#     // F# major
#Bb     // Bb major
```

**Format**: `#<root>[m]`

- **`#` prefix**: Indicates key signature (required)
- **Root**: Note name (C, D, E, F, G, A, B)
- **Accidentals**: Can include sharps (#) or flats (b) in the root name
- **`m` suffix**: Makes it minor (optional, defaults to major)

See [Key Signatures](./key-signatures.md) for more details.

## Complete Metadata Examples

### Example 1: Rock Song

```txt
Don't Stop Believin' - Journey
120bpm 4/4 #E

Intro 4
E B C#m A
```

**Parsed as**:

- Title: "Don't Stop Believin'"
- Artist: "Journey"
- Tempo: 120 BPM
- Time Signature: 4/4
- Key: E major

### Example 2: Ballad in 6/8

```txt
Hallelujah - Leonard Cohen
68bpm 6/8 #C

Intro 2
C Am C Am
```

**Parsed as**:

- Title: "Hallelujah"
- Artist: "Leonard Cohen"
- Tempo: 68 BPM
- Time Signature: 6/8
- Key: C major

### Example 3: Minor Key

```txt
Stairway to Heaven - Led Zeppelin
82bpm 4/4 #Am

Intro
Am E C D
```

**Parsed as**:

- Title: "Stairway to Heaven"
- Artist: "Led Zeppelin"
- Tempo: 82 BPM
- Time Signature: 4/4
- Key: A minor

### Example 4: Sharp Key

```txt
Mr. Brightside - The Killers
148bpm 4/4 #D

Intro
D A Bm G
```

**Parsed as**:

- Title: "Mr. Brightside"
- Artist: "The Killers"
- Tempo: 148 BPM
- Time Signature: 4/4
- Key: D major

### Example 5: Flat Key

```txt
Wonderwall - Oasis
87bpm 4/4 #Eb

Intro
Eb Bb Cm Ab
```

**Parsed as**:

- Title: "Wonderwall"
- Artist: "Oasis"
- Tempo: 87 BPM
- Time Signature: 4/4
- Key: Eb major

## Metadata Order

Metadata must appear in this specific order:

1. **Title and Artist** (first line)
2. **Tempo, Time Signature, Key** (second line, space-separated)
3. **Sections and chart content** (following lines)

**Correct**:

```txt
Song Title - Artist Name
120bpm 4/4 #G

Intro
```

**Incorrect** (order matters):

```txt
120bpm 4/4 #G
Song Title - Artist Name

Intro
```

## Optional Metadata

You can omit certain metadata if not needed:

### No Artist

```txt
Original Song
120bpm 4/4 #G
```

Use this for original compositions or when artist is not applicable.

### Title Only (No Tempo/Key Info)

```txt
Song Title - Artist Name

Intro
G C Em D
```

Useful for quick sketches or when tempo/key is flexible.

## Metadata in Output

Metadata is used to generate:

1. **Chart Header**: Title and artist appear at the top
2. **Tempo Marking**: `♩ = 120` notation
3. **Time Signature**: Visual time signature
4. **Key Signature**: Accidentals on staff
5. **File Naming**: Generated filename uses title

## Mid-Song Changes

### Tempo Changes

```txt
Song Title - Artist Name
120bpm 4/4 #G

Verse
G C Em D

\tempo 140bpm
Chorus
C G Am F
```

Use `\tempo <number>bpm` for tempo changes mid-song.

### Time Signature Changes

```txt
Verse
G C Em D

\time 3/4
Bridge
Em Am D
```

Use `\time <numerator>/<denominator>` for time signature changes.

### Key Changes

Key changes are covered in [Key Signatures](./key-signatures.md):

```txt
Verse
G C Em D

#D
Chorus
D A Bm G
```

## Best Practices

1. **Always include title and artist**: Helps with organization and identification
2. **Specify tempo for originals**: Essential for band rehearsals
3. **Include key signature**: Helps with transposition and scale degree interpretation
4. **Use standard time signatures**: Stick to common meters unless composition requires odd time
5. **Update metadata if song evolves**: Keep tempo and key accurate to the arrangement

## Common Mistakes

### Missing Space Around Dash

**Wrong**: `Song Title-Artist Name`  
**Right**: `Song Title - Artist Name`

### Wrong Key Format

**Wrong**: `G` (missing # prefix)  
**Right**: `#G`

### Wrong Tempo Format

**Wrong**: `120` (missing bpm)  
**Right**: `120bpm`

### Wrong Order

**Wrong**:

```txt
#G 4/4 120bpm
```

**Right**:

```txt
120bpm 4/4 #G
```

## Technical Notes

- **Metadata parsing**: Only the first two lines are checked for metadata
- **Case sensitivity**: `bpm` must be lowercase, time signature numbers are parsed as-is
- **Whitespace**: Spaces between tempo/time/key are required
- **Defaults**: If metadata is missing, reasonable defaults may be used (4/4 time, C major)
- **Artist with dash**: If artist name contains a dash, use the first dash as separator
  - Example: `Song Title - Artist - Band Name` → Title: "Song Title", Artist: "Artist - Band Name"
