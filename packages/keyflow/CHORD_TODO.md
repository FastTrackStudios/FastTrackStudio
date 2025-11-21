# Chord Implementation TODO

Based on the v1 implementation, here's what we need to add to complete the Chord system:

## ✅ Already Implemented

- [x] Basic quality (Major, Minor, Diminished, Augmented, Power, Suspended)
- [x] Root parsing (note names, scale degrees, roman numerals)
- [x] Duration/rhythm parsing
- [x] Tracing support

## ⬜ Still Needed

### 1. **Seventh Chords**

- [ ] `maj7` - Major 7th (C, E, G, B)
- [ ] `min7` / `m7` - Minor 7th (C, Eb, G, Bb)
- [ ] `dom7` / `7` - Dominant 7th (C, E, G, Bb)
- [ ] `dim7` / `°7` - Diminished 7th (C, Eb, Gb, Bbb)
- [ ] `half_dim7` / `m7b5` / `ø7` - Half-diminished (C, Eb, Gb, Bb)
- [ ] `min_maj7` / `mM7` - Minor-major 7th (C, Eb, G, B)
- [ ] `aug_maj7` / `+M7` - Augmented-major 7th (C, E, G#, B)

### 2. **Sixth Chords**

- [ ] `6` - Major 6th (C, E, G, A)
- [ ] `m6` - Minor 6th (C, Eb, G, A)
- [ ] `6/9` - Major 6/9 (C, E, G, A, D)
- [ ] `m6/9` - Minor 6/9 (C, Eb, G, A, D)

### 3. **Extensions (9th, 11th, 13th)**

- [ ] `9` - Dominant 9th (adds 9th to dom7)
- [ ] `maj9` - Major 9th (adds 9th to maj7)
- [ ] `min9` / `m9` - Minor 9th (adds 9th to min7)
- [ ] `11` - Dominant 11th (adds 11th to 9th)
- [ ] `maj11` - Major 11th
- [ ] `min11` / `m11` - Minor 11th
- [ ] `13` - Dominant 13th (adds 13th to 11th)
- [ ] `maj13` - Major 13th
- [ ] `min13` / `m13` - Minor 13th

### 4. **Altered Extensions**

- [ ] `b9` - Flat 9th
- [ ] `#9` - Sharp 9th
- [ ] `#11` - Sharp 11th (Lydian sound)
- [ ] `b13` - Flat 13th
- [ ] `7b9`, `7#9`, `7#11`, `7b13` - Dominant with alterations
- [ ] `maj7#11` - Major 7th with sharp 11th
- [ ] `min7b5` - Half-diminished

### 5. **Altered Fifth**

- [ ] `b5` - Flat fifth
- [ ] `#5` / `aug` - Sharp fifth (augmented)
- [ ] `7b5` - Dominant 7 flat 5
- [ ] `7#5` - Dominant 7 sharp 5

### 6. **Additions** (add note without implying 7th)

- [ ] `add9` - Major with added 9th (no 7th)
- [ ] `madd9` - Minor with added 9th (no 7th)
- [ ] `add11` - Major with added 11th (no 7th)
- [ ] `add13` - Major with added 13th (no 7th)

### 7. **Omissions**

- [ ] `no3` - Omit the 3rd (power chord-like)
- [ ] `no5` - Omit the 5th
- [ ] Parse omissions like `C7no5`

### 8. **Slash Chords** (Bass Notes)

- [ ] `C/E` - C major with E in the bass
- [ ] `Gmaj7/B` - G major 7 with B in the bass
- [ ] `4/6` - Scale degree 4 with scale degree 6 in bass
- [ ] `IV/vi` - Roman numeral IV with vi in bass

### 9. **Intervals System**

- [ ] Store intervals in the chord
- [ ] `chord_tones()` method - get actual notes
- [ ] `transpose()` method - transpose by interval
- [ ] `voicings()` method - get different voicings

### 10. **Advanced Features**

- [ ] Chord normalization (e.g., `Cmaj7#11` → proper symbol)
- [ ] Chord equivalence (enharmonic, etc.)
- [ ] Chord voicing generation
- [ ] Chord inversion detection
- [ ] Chord scale relationships

## Implementation Strategy

### Phase 1: Extensions (Priority)

Start with the most common ones:

1. Add `SeventhType` enum (maj7, min7, dom7, dim7)
2. Add `Extension` enum (9th, 11th, 13th with alterations)
3. Add `Alteration` enum (b5, #5, b9, #9, #11, b13)
4. Update `Chord` struct to include these fields
5. Update parser to recognize extension syntax

### Phase 2: Additions & Omissions

1. Add `Addition` enum
2. Add `Omission` enum
3. Parse `add9`, `add11`, `no3`, `no5`

### Phase 3: Slash Chords

1. Add `bass: Option<RootNotation>` field
2. Parse `/` followed by bass note
3. Handle bass note resolution with key context

### Phase 4: Intervals & Chord Tones

1. Add `intervals: Vec<Interval>` field
2. Implement `to_intervals()` method
3. Implement `chord_tones()` method
4. Add voicing support

## Example Target Syntax

```rust
// Extensions
"Cmaj7"  -> Major 7th
"Dm9"    -> Minor 9th
"G13"    -> Dominant 13th
"Fmaj9#11" -> Major 9th sharp 11th

// Alterations
"G7b9"   -> Dominant 7 flat 9
"C7#11"  -> Dominant 7 sharp 11
"Am7b5"  -> Half-diminished

// Additions
"Cadd9"  -> C major add 9 (no 7th)
"Dmadd11" -> D minor add 11

// Omissions
"C7no5"  -> C7 without 5th

// Slash chords
"C/E"    -> C major with E bass
"Gmaj7/B" -> G major 7 with B bass
"4/6"    -> Scale degree 4 over 6
```

## Notes

- The v1 implementation uses a typestate pattern with phantom types for compile-time safety
- For v2, we can use a simpler runtime approach with enums
- Focus on parsing first, then add interval/voicing generation
- Keep tracing throughout for debugging complex chords

