# Text Cues

Text cues allow you to add performance instructions and callouts for the band or specific instruments using the `@` symbol.

## Basic Syntax

```txt
@target "Cue message"
```

- **`@`** - Cue marker
- **`target`** - Who the cue is for
- **`"message"`** - The cue text in quotes

## Targets

### All Band

```txt
@all "Intro Groove!"
@all "Build to chorus"
```

### Instruments

```txt
@keys "arps here"
@guitar "Let it ring"
@drums "Half time feel"
@bass "Walk up to chorus"
@vocals "Ad lib"
```

### Groups

```txt
@rhythm "Sparse here" // Bass, drums, rhythm guitar
@melody "Unison line" // Lead instruments
@backline "Drop out" // Bass and drums
```

## Placement

### Section Level

```txt
Verse
@all "Quiet, intimate feel"
G C Em D
```

### Measure Level

```txt
Intro
G @keys "arps here" C Em D
```

### Multiple Cues

```txt
Bridge
@drums "Build intensity"
@keys "Add pad"
G G G G
```

## Full Example

```txt
Song Title - Artist Name
120bpm 4/4 #G

Intro 4
@all "Atmospheric, build tension"
G C Em D

Verse
@drums "Light shuffle"
@keys "Sparse chords"
G C Em D

Chorus
@all "Full band!"
C G Am F

Bridge 8
@all "Half-time feel"

Outro 4
@all "Fade out"
```
