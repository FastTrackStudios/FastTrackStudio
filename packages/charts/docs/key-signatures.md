# Key Signatures

There are many ways to specify a key signature using the `#` symbol in context.

## Basic Key Signatures

Use `#` followed by the key name at the start of the chart or anywhere mid-song to change keys.

### Normal Keys

```txt
#C // C major
#Cm // C minor
#Eb // E♭ major
#F# // F♯ major
#F#m // F♯ minor
#Bbm // B♭ minor
```

### How It Works

The `#` prefix indicates a key signature:

- Only recognized when it starts after whitespace (so `F#` as a chord root won't be confused)
- Handles both sharps and flats (e.g., `#F#`, `#Bb`)
- Minor keys indicated by `m` suffix (e.g., `#Em`, `#Cm`)

### Placement

#### Song Header (Initial Key)

```txt
Reckless Love - Cory Asbury
68bpm 6/8 #E

Intro 4
Emaj7 C#m7 B A
```

#### Mid-Song Key Changes

```txt
vs 4
G C D Em

#D // Change to D major
D A Bm G
```

#### Mid-Phrase Key Changes

Key changes can occur within a chord line:

```txt
vs 4
G C #D D A
// G and C in original key, then changes to D major for D and A chords
```

## Modal Keys

Specify modes by name after the root note:

```txt
#C Ionian // Same as C major
#C Dorian // C Dorian mode
#C Phrygian // C Phrygian mode
#C Lydian // C Lydian mode
#C Mixolydian // C Mixolydian mode
#C Aeolian // Same as C natural minor
#C Locrian // C Locrian mode
```

## Alternative Scales

Support for common alternative scales:

```txt
#C Harmonic Minor // C harmonic minor
#C Melodic Minor // C melodic minor
```

## Custom Scales

Define custom scales using semitone patterns:

```txt
\CustomScale CustomScaleName { 2 2 1 2 2 1 2 }
// Pattern should be in semitones from root
// Example: Major scale = 2 2 1 2 2 2 1
```

## Key Signature Properties Stored

When a key is set, the following properties are tracked:

- **Root note** (e.g., "E", "F#", "Bb")
- **Quality** (major or minor)
- **Time signature** (e.g., 6/8, 4/4)
- **Tempo** (e.g., 68bpm)

## Key Signature Display

The key signature affects how scale degrees and roman numerals are interpreted:

```txt
#G // Set to G major

1 2 3 4 5 6 7 1
// Interpreted as: Gmaj Am Bm Cmaj Dmaj Em F#dim Gmaj

I ii iii IV V vi vii° I
// Same progression with roman numerals
```

### Chromatic Scale Degrees

Chromatic alterations preserve the letter name of the scale degree:

```txt
#C // C major

b5     // Gb (flat 5, not F#)
#4     // F# (sharp 4, not Gb)
bV     // Gb major (flat V as roman numeral)
#IV    // F# major (sharp IV as roman numeral)
```

**Key principle**: The scale degree determines the letter, the accidental modifies it:

- Degree 5 in C = G, so `b5` = **Gb**
- Degree 4 in C = F, so `#4` = **F#**

## Integration with Song Metadata

Key signatures work together with other song metadata:

```txt
Song Title - Artist Name
68bpm 6/8 #E

// This sets:
// - Title: "Song Title"
// - Artist: "Artist Name"
// - Tempo: 68 BPM
// - Time Signature: 6/8
// - Key: E major
```
