Rules

1. Only define a chord once, if you say Gm7b5#11, then you should only have to type g to repeat the chord
2. The ! Symbol is a one time override that will forget all previous assumptions
3. x followed by a number will repeat
4. Push chords with ‘ so like ‘c or c’
5. / can be used to indicate duration, whatever the 4/4 or 6/8 value is. Like G //// Em // C //
6. Completely driven by URL params
7. You can use Roman numerals. I vi V V/V
8. Notes with @keys arp heres
9. The ^ keys can be used to repeat from the last section

Step 1: Name The Song
Reckless Love - Cory Asbury
68bpm 6/8 #E

Stage 2: Sections
Intro 4
vi V IV I
Verse 8
Chorus 4
Verse
Chorus
Bridge 8
Chorus
Outro 4

Step 2:

Reckless Love - Cory Asbury
68bpm 6/8 #G

Vs 8
g c em d x2

Ch /2
I

# Chord Input

Chord Input should never have to enter any sort of mode attached to it. We need to intelligently decipher between notes and chords and everything else.

- Chords can be entered with notenames, or scale degrees, or roman numerals
- A colon can be used to seperate root chord from quality, this is optional for Letters and Roman Numerals, and recommended for Scale Degrees, but still not required
- For Roman Numerals, Capitalization matters, but is overridden by a specficied quality. so viimaj is the same as VII

```md
    Cmaj7 , Imaj7 , 1:maj7
    C:7sus4 , I:7sus4 , 1:7sus4

    // Showing the Major Scale
    Cmaj7 | Dmin7 | Emin7 | Fmaj7 | Gmaj7 | Amin7 | B0 | Cmaj7 |
    Imaj7 | iimin7 | iiimin7 | IVmaj7 | V7 | vimin7 | vii0 | Imaj7 |
    1:maj7 | 2:m7 | 3:m7 | 4:maj7 | 5:7 | 6:m7 | 70 | 1:maj7 |
    1maj7 | 2m7 | 3m7 | 4maj7 | 57 | 6m7 | 70 | 1maj7 | //This syntax is correct but not very readable as it produced numbers like 70, which is really a half diminished chord on the seventh scale degree
```

## Scale Degrees have their qualities assumed for the key signature

```md
    #C //This puts us in C major

    C D E F G A B C //This is translated to Cmaj Dm Em Fmaj Gmaj Am Bdim C
    I ii iii IV V vi vii I // The Chord quality is provided by capitalization. So no quality assumptions are made
    1 2 3 4 5 6 7 1 // This is the same as the first line, translated to Cmaj Dm Em Fmaj Gmaj Am Bdim C
```

```md
    #Cm // This puts us into C minor
    C D Eb F G Ab Bb C //This is translated to Cmin Ddim Ebmaj Fmin Gmin Amaj BMaj Cmin
    i ii III iv v VI VII i // minor scale with roman numerals
    1 2 3 4 5 6 7 1 // Numbers will auto assume the right qualities and root notes so nothing needs to be specified

```

# Chord Rhythms

  Rhythms is definitley hard to make work without looking super messy. There are three ways to specify rhythms for chords

## Slash Syntax

 This is common for quick rhythm charts. A slash is the denominator of the time signature. So in 4/4, //// is a full measure.

```md
    // Slash Syntax
    1:maj7 //// 4:maj7#11 //// 5 //// 6 // 5 //

```

## Lilypond Rhythm Syntax

This will overwrite the default chord rhythm. Which is a bar by default.

```md
    // Slash Syntax

```

## Barline Rhythms

For quick rhythm charts that don't require any super special rhythms, adding bar lines to a phrase will automatically decide some of these rhythms for you.

```md
    // Barline Rhythm Syntax
    4/4 // Time Signature
    1 4 |6 5| 4 // Is Translated into
    1 //// 4 //// 6 // 5 // 4//
```

# Key Signatures

There are many many ways to specify a key signature. This is using the # symbol in context.

- Normal Keys ( #C, #Cm, #Eb, #F#, etc )
- Modal Keys ( #C Ionian #C Dorian, #C Phrygian, #C Lydian, #C Mixolydian, #C Aeolian, #C Locrian )
- Alternative Keys ( #C Harmonic Minor, #C Melodic Minor )
- Define Custom Scales ( \CustomScale CustomScaleName { 2 2 1 2 2 1 2 }) //should be in semitones

# Melodies
- Advanced Synax
