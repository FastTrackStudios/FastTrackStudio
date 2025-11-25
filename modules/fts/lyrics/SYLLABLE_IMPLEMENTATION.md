# Syllable Splitting Implementation

## Overview

The lyrics crate includes a built-in syllable counting and splitting module (`src/syllables.rs`) that uses regex-based heuristics to estimate syllables in English words. This is a pure Rust implementation with no external dependencies beyond `regex` and `once_cell`.

## Usage

The syllable counting is automatically used when creating words:

```rust
use lyrics::core::Word;
use lyrics::syllables::syllables_in_word;

// Count syllables in a word
let count = syllables_in_word("hello"); // Returns 2

// Create a word with syllable splitting
let word = Word::from_text("drowning");
println!("Syllables: {:?}", word.syllables);
// Output: syllables split based on estimated count
```

## Syllable Counting Algorithm

The algorithm uses multiple regex patterns to handle various English word patterns:

1. **Vowel groups**: Counts groups of consecutive vowels
2. **Y after consonants**: Handles words like "rowdy", "lowly", "every"
3. **Silent 'e' patterns**: Handles cases like "case", "race", "chase"
4. **Special endings**: Handles "aste", "apse", "ted", "eXe", "elve", "eing"
5. **Contractions**: Handles "couldn't", "would've" patterns

### Examples

- "hello" → 2 syllables
- "read" → 1 syllable
- "chocolate" → 3 syllables
- "beautiful" → 3 syllables
- "polymorphism" → 4 syllables
- "drowning" → 2 syllables

## Syllable Splitting Strategy

The splitting algorithm in `Word::from_text()`:

1. Uses the estimated syllable count from `syllables_in_word()` to guide splitting
2. Groups consecutive vowels together
3. Splits after vowel groups, including following consonants:
   - If 2+ consonants follow: split after first consonant
   - If 1 consonant follows: include with vowel
   - If 0 consonants: split after vowel

## Module Structure

The syllable counting logic is in `src/syllables.rs`:

- `syllables_in_word(s: &str) -> usize` - Count syllables in a single word
- `syllables_in_words(text: &str) -> usize` - Count total syllables in multi-word text

Both functions are exported from the crate root and can be used directly.

## Testing

The module includes comprehensive tests covering:
- Individual word syllable counts (50+ test cases)
- Multi-word text syllable counts
- Edge cases and special patterns

All tests pass and are based on the original syllable counting implementation.

## Future Improvements

For perfect position/time sync (as mentioned in requirements), we'll need to:

1. **Time-based synchronization**: Add precise timestamps to each syllable
2. **MusicalPosition-based sync**: Align syllables with musical beats/measures
3. **Manual adjustment**: Allow fine-tuning of syllable boundaries

These features will be implemented when we add the full synchronization engine.

