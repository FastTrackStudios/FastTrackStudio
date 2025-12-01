# Chord Input

Chord Input should never have to enter any sort of mode attached to it. We need to intelligently decipher between notes and chords and everything else.

**Three equivalent notations** - All produce the same semantic chord:

```txt
Dm7 = 2:m7 = ii7      // D minor 7 in C major
G7 = 5:7 = V7         // G dominant 7 in C major
Gb = b5 = bV          // G flat major in C major
```

## Basic Rules

1. **Only define a chord once** - If you say `Gm7b5#11`, then you should only have to type `g` to repeat the chord
2. **The `!` Symbol** - One time override that will forget all previous assumptions (e.g., `!G7`)
3. **Repeat with `x`** - `x` followed by a number will repeat (e.g., `g c em d x2`)

## Chord Input Formats

Chords can be entered in **three equivalent formats** that produce the same semantic chord while preserving the original notation:

- **Note names** (e.g., `Cmaj7`, `G7sus4`, `Gb`)
- **Scale degrees** (e.g., `1`, `2:m7`, `b5`)
- **Roman numerals** (e.g., `I`, `ii`, `bV`)

All three formats produce identical chord data, but the parser remembers which format you used for proper display.

### Colon Separator

A colon can be used to separate the root from the quality/extension:

```txt
1:maj7    // Scale degree 1 with maj7 extension
I:7sus4   // Roman numeral I with 7sus4
Dm:9      // D minor with 9th extension
```

- Optional for note names and roman numerals
- Helpful for clarity with scale degrees
- Required for secondary dominants with extensions (e.g., `V:7/ii`)

### Capitalization Rules

For Roman Numerals, capitalization determines major/minor quality:

- **Uppercase** = Major (e.g., `I`, `V`, `IV`)
- **Lowercase** = Minor (e.g., `ii`, `iii`, `vi`)
- Explicit quality overrides capitalization: `viimaj` = `VIImaj`

### Examples

```txt
Cmaj7 , Imaj7 , 1:maj7
C:7sus4 , I:7sus4 , 1:7sus4

// Showing the Major Scale
Cmaj7 | Dmin7 | Emin7 | Fmaj7 | Gmaj7 | Amin7 | B0 | Cmaj7 |
Imaj7 | iimin7 | iiimin7 | IVmaj7 | V7 | vimin7 | vii0 | Imaj7 |
1:maj7 | 2:m7 | 3:m7 | 4:maj7 | 5:7 | 6:m7 | 70 | 1:maj7 |
1maj7 | 2m7 | 3m7 | 4maj7 | 57 | 6m7 | 70 | 1maj7 |
// Note: This last syntax is correct but not very readable as it produces numbers like 70,
// which is really a half diminished chord on the seventh scale degree
```

## Scale Degrees and Quality Assumptions

Scale degrees have their qualities assumed based on the key signature.

### Major Key Example

```txt
#C // This puts us in C major

C D E F G A B C // Translated to: Cmaj Dm Em Fmaj Gmaj Am Bdim Cmaj
I ii iii IV V vi vii I // Chord quality provided by capitalization
1 2 3 4 5 6 7 1 // Same as first line: Cmaj Dm Em Fmaj Gmaj Am Bdim Cmaj
```

### Minor Key Example

```txt
#Cm // This puts us into C minor

C D Eb F G Ab Bb C // Translated to: Cmin Ddim Ebmaj Fmin Gmin Abmaj Bbmaj Cmin
i ii III iv v VI VII i // Minor scale with roman numerals
1 2 3 4 5 6 7 1 // Numbers auto-assume the right qualities and root notes
```

## Chromatic Alterations

Scale degrees and roman numerals can be altered with `b` (flat) or `#` (sharp):

```txt
#4     // F# in C major (sharp 4)
b5     // Gb in C major (flat 5)
#IV    // F# major in C major
bV     // Gb major in C major
```

**Important**: The alteration preserves the letter name of the scale degree:

- `b5` in C = **Gb** (not F#), because 5 = G
- `#4` in C = **F#** (not Gb), because 4 = F

## Secondary Dominants & Tonicization

**Roman numerals only** - Use `/` to create secondary dominants:

```txt
V/ii     // V of ii in C major = A major (dominant of D minor)
V:7/ii   // A7 (with extension)
iii/V    // iii in key of V = B minor (iii of G in C major)
ii/IV    // ii in key of IV = G minor (ii of F in C major)
```

**Note**: Scale degrees don't use secondary dominants. Instead, just write the actual degree:

- Instead of `3/5`, write `7m` (the actual scale degree)
- Instead of `5/2`, just write the resulting note directly

## Inversions

### Slash Chords (Note Names & Scale Degrees)

Use `/` to specify a bass note different from the root:

```txt
C/E      // C major with E in bass (1st inversion)
G/B      // G major with B in bass (1st inversion)
G/D      // G major with D in bass (2nd inversion)
G/F      // G dominant 7 with F in bass (3rd inversion, infers G7)
G/F#     // G major 7 with F# in bass (3rd inversion, infers Gmaj7)
```

The parser automatically:

- Detects if the bass note is a chord tone (true inversion)
- Determines the inversion number (1st, 2nd, or 3rd)
- Infers 7th extensions when needed (e.g., `G/F` → `G7`)

### Figured Bass (Roman Numerals)

For roman numerals, use figured bass notation instead of slash chords:

```txt
V7     // Root position (or just V)
V65    // 1st inversion (3rd in bass)
V43    // 2nd inversion (5th in bass)
V42    // 3rd inversion (7th in bass)
V2     // 3rd inversion (shorthand)
```

Figured bass automatically calculates the correct bass note based on the inversion.

## Advanced Features

- **Push chords with `'`** - e.g., `'c` (early) or `c'` (late)
- **One-time override `!`** - Ignore chord memory: `!G7`
- **Repeat with `x`** - Repeat previous chords: `g c em d x2`
- **Notes with @keys** - arp heres (TODO: document further)
- **The `^` key** - Can be used to repeat from the last section (TODO: implement)

## Complete Example

```txt
Reckless Love - Cory Asbury
120bpm 4/4 #C

// Using note names
Intro
Cmaj7 Am7 Fmaj7 G

// Using scale degrees (same progression)
Verse
1:maj7 6:m7 4:maj7 5

// Using roman numerals (same progression)
Chorus
Imaj7 vim7 IVmaj7 V

// Secondary dominants
Bridge
V/vi V7/ii ii V     // E, A7, Dm, G in C major

// Inversions with slash chords
Tag
C/E F/A G/B C       // C major, 1st inversions

// Figured bass with roman numerals
Outro
I V65 I6 IV V7 I    // With inversions
```

## Implementation Notes

The parser maintains three pieces of information for each chord:

1. **Semantic representation** - The actual notes/intervals (e.g., "D", "m7")
2. **Notation type** - How it was written (`NoteName`, `ScaleDegree`, or `RomanNumeral`)
3. **Original spelling** - The exact input string (e.g., "Dm7", "2:m7", or "ii7")

This allows the system to:

- Convert between notations while preserving meaning
- Display chords in the user's preferred format
- Maintain consistency within a chart
