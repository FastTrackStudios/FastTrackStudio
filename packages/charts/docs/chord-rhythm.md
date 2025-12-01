# Chord Rhythms

Rhythms is definitely hard to make work without looking super messy. There are three ways to specify rhythms for chords.

## 1. Slash Syntax

This is common for quick rhythm charts. A slash represents the denominator of the time signature. So in 4/4, `////` is a full measure.

### Slash Syntax Examples

```txt
// 4/4 time - each / is a quarter note
1:maj7 //// 4:maj7#11 //// 5 //// 6 // 5 //

// In this example:
// - 1:maj7 gets a full bar (////)
// - 4:maj7#11 gets a full bar (////)
// - 5 gets a full bar (////)
// - 6 gets half a bar (//)
// - 5 gets half a bar (//)
```

### Duration Syntax

The `/` symbol can be used to indicate duration based on the time signature value:

- In **4/4**: Each `/` = quarter note, `////` = whole note (full bar)
- In **6/8**: Each `/` = eighth note, `/// ///` = full bar
  - **Dotted notation**: Use `/.` for dotted quarter notes (3 eighth notes)
  - Full bar in 6/8: `/// ///` or `/. /.`

```txt
// 4/4 time
G //// Em // C //
// G plays for 4 beats
// Em plays for 2 beats
// C plays for 2 beats

// 6/8 time
G /. /. Em /// C ///
// G plays for a full bar (two dotted quarters)
// Em plays for half a bar (3 eighth notes)
// C plays for half a bar (3 eighth notes)
```

## 2. Lilypond Rhythm Syntax

Specify exact rhythms using Lilypond notation with the `_` (underscore) suffix. This overrides the default "one chord = one bar" behavior.

### Lilypond Syntax

```txt
ChordName_duration
```

### Lilypond Duration Values

| Notation | Duration         | Note |
| -------- | ---------------- | ---- |
| `_4`     | Quarter note     | ♩    |
| `_8`     | Eighth note      | ♪    |
| `_16`    | Sixteenth note   | ♬    |
| `_2`     | Half note        | 𝅗𝅥   |
| `_1`     | Whole note       | 𝅝    |
| `_4.`    | Dotted quarter   | ♩.   |
| `_8.`    | Dotted eighth    | ♪.   |
| `_16.`   | Dotted sixteenth | ♬.   |

### Special Symbols

| Symbol | Purpose                       | Example                      |
| ------ | ----------------------------- | ---------------------------- |
| `r`    | Rest (silent)                 | `r4.` = dotted quarter rest  |
| `s`    | Space (invisible, takes time) | `s4.` = dotted quarter space |
| `~`    | Tie (extend previous chord)   | `C_4~ s8` = quarter + eighth |

**Note:** `s` assumes the previous chord and extends its duration without re-entering it. Both `r` and `s` can be written with or without the `_` prefix (e.g., `r4` or `r_4`).

### Multipliers

Use `*` to multiply durations:

```txt
s1*8 // 8 bars of space
r2*4 // 4 half-note rests (2 bars)
```

### Ties

Use `~` after a chord to tie it to the next duration:

```txt
Cmaj7_4~ s16 // Quarter note + sixteenth (tied)
G_2~ s4 // Half note + quarter note (tied)
D_4.~ s8~ s8 // Dotted quarter + eighth + eighth (all tied)
```

### Rhythm Repetition

**Note:** Unlike Lilypond, entering just a number (e.g., `4`) is ambiguous—it could mean "the 4 chord" or "a quarter note."

To repeat the previous chord with a new rhythm, use `_` followed by the duration:

```txt
Gmaj7_4 _8 _16      // Gmaj7: quarter, eighth, sixteenth
C_2 _4 _4           // C: half, quarter, quarter
D_4. _8~ s8         // D: dotted quarter, tied eighth + eighth
```

### Examples

```txt
// Specific rhythm pattern
Gmaj7_4. Cadd9_8 Em7_16 Em7_16 D13sus_8. Gmaj7_4

// With rests
G_4 r4 C_4 D_4

// With spaces (tacet but time passes)
G_4 s2 D_4

// Long spaces
s1\*8 // 8 bars of nothing

// Mixed with default (quarter notes where not specified)
G_4 C D_8 D_8 Em

// Dotted rhythms
C_4. G_8 Am_4. F_8

// Complex pattern with rests
Gmaj7_2 r4 Cadd9_4 Em7_8 r8 D_4
```

## 3. Barline Rhythms

For quick rhythm charts that don't require any super special rhythms, adding bar lines to a phrase will automatically decide rhythms for you.

### Barline Examples

```txt
// Barline Rhythm Syntax
4/4 // Time Signature

1 4 | 6 5 | 4
// Is translated into:
1 //// 4 //// | 6 // 5 // | 4 ////

// Explanation:
// - Single chords per measure get full duration
// - Multiple chords per measure split the duration evenly
```

## 4. Push/Pull Notation

Use the `'` (apostrophe) symbol to push or pull chord timing for syncopation and rhythmic variety.

### Push/Pull Syntax

- **Before the chord** (`'C`): Push the chord earlier (anticipation)
- **After the chord** (`C'`): Pull the chord later (delay)

### Push/Pull Duration Values

The number of apostrophes determines the subdivision:

| Notation | Duration       | Effect           |
| -------- | -------------- | ---------------- |
| `'`      | Eighth note    | ♪ subdivision    |
| `''`     | Sixteenth note | ♬ subdivision    |
| `'''`    | 32nd note      | Fine subdivision |

### Push/Pull Examples

```txt
// Anticipation (push back/earlier)
'C // Pushed back an eighth note
''C // Pushed back a sixteenth note
'''C // Pushed back a 32nd note

// Delay (pull/later)
C' // Delayed by an eighth note
C'' // Delayed by a sixteenth note
C''' // Delayed by a 32nd note

// Common usage in a progression
G D 'Em C // Em anticipates by an eighth note
G D Em' C // Em delayed by an eighth note

// Multiple anticipations
''G 'D Em C // G pushed by 16th, D pushed by 8th
```

### Practical Uses

- **Anticipation**: Common in pop/rock when the next chord hits early
- **Delay**: Creates a "lazy" or "laid back" feel
- **Syncopation**: Combine with regular rhythms for rhythmic interest

```txt
// Example: Syncopated progression
C //// 'G //// Em' //// D ////
// C on downbeat, G anticipates, Em delays, D on downbeat
```

## Default Behavior

**Important:** Every chord is one bar by default unless otherwise specified.

```txt
vs
e d e g

// This creates 4 bars (one bar per chord)
```

## Repeat Notation

Use `x` followed by a number to repeat a chord progression:

```txt
g c em d x2
// Plays: g c em d g c em d
```
