# Feature Implementation TODO

## Overview

This document tracks the implementation of new chart parsing features for the keyflow library, based on the "All Along The Watchtower" example.

---

## 1. Smart Repeat Syntax (`x^`)

### Description

Add support for `x^` syntax which automatically calculates the number of repeats needed to fill a section based on:

1. The duration of the phrase (calculated from chord durations)
2. The total section length
3. Auto-repeat factor = section_length / phrase_length

### Example

```
VS 16
6_2 5 4 5 x^
```

- Phrase duration: 2 bars (6_2=2 beats, 5=2 beats, 4=2 beats, 5=2 beats in 4/4)
- Section length: 16 bars
- Repeat count: 16 / 2 = 8
- Equivalent to: `6_2 5 4 5 x8`

### Implementation Tasks

- [ ] Add `x^` token recognition in lexer (`music/keyflow/src/parsing/lexer.rs`)
- [ ] Add `AutoRepeat` variant to Token enum (`music/keyflow/src/parsing/token.rs`)
- [ ] Update chord duration parser to handle `x^` syntax
- [ ] Add method to calculate total phrase duration
- [ ] Add method to calculate required repeats (section_length / phrase_length)
- [ ] Update display output to show calculated repeats
- [ ] Add unit tests for `x^` syntax with various time signatures
- [ ] Add integration test with example from "All Along The Watchtower"

### Edge Cases to Handle

- Non-divisible section lengths (e.g., 15-bar section with 4-bar phrase)
- Empty phrases
- Phrases longer than section length
- Multiple `x^` in same section (should error?)

---

## 2. Custom Commands with Slash Syntax

### Description

Add support for custom commands using `/command_name` syntax to modify the last element (chord, melody, or rhythm).

### 2.1 Fermata Command (`/fermata`)

#### Example

```
END 1
6 /fermata
```

- Adds a fermata symbol to the last chord (6)

#### Implementation Tasks

- [ ] Add `/` token recognition for commands in lexer
- [ ] Add `Command(String)` variant to Token enum
- [ ] Create new `Command` enum type with variants: `Fermata`, `Accent`, etc.
- [ ] Add command parser to recognize `/fermata`
- [ ] Add `commands: Vec<Command>` field to chord/melody/rhythm types
- [ ] Update display logic to show fermata symbol
- [ ] Add unit tests for fermata parsing
- [ ] Add integration test for fermata in chart

### 2.2 Accent Command (`/accent` or `->`)

#### Description

Add accent marking to chords/notes with shorthand `->` syntax.

#### Example

```
5->      # Long form: 5 /accent
4_8 _8   # Multiple hits of same chord
```

#### Implementation Tasks

- [ ] Add `->` token recognition in lexer
- [ ] Add shorthand parsing for `->` as accent
- [ ] Update command parser to recognize `/accent`
- [ ] Add `Accent` variant to Command enum
- [ ] Update display logic to show accent symbol
- [ ] Handle accent on duration extensions (e.g., `5-> _8`)
- [ ] Add unit tests for accent parsing (both syntaxes)
- [ ] Add integration test for accents in chart

### Common Command Infrastructure

- [ ] Design command application system (how commands attach to elements)
- [ ] Ensure commands work with all element types (chord, melody, rhythm)
- [ ] Add validation for command placement
- [ ] Handle multiple commands on same element

---

## 3. Smart Repeats Feature

### Description

Automatically group repeated phrases into 4-bar units with repeat signs when enabled via `/SMART_REPEATS=true` setting.

### Example

```
/SMART_REPEATS=true

VS 16
6_2 5 4 5 x^
```

**Without Smart Repeats:**

```
6_2 5 4 5 x8  # 2-bar phrase repeated 8 times
```

**With Smart Repeats:**

```
6_2 5 4 5 6 5 4 5 x4  # 4-bar phrase repeated 4 times
```

### Implementation Tasks

- [ ] Add settings/config system to chart parser
- [ ] Add `/SETTING_NAME=value` syntax recognition in lexer
- [ ] Parse `/SMART_REPEATS=true/false` directive
- [ ] Add `smart_repeats` flag to chart configuration
- [ ] Implement smart repeat algorithm:
  - [ ] Detect when phrase is less than 4 bars
  - [ ] Calculate how many times to duplicate phrase to reach 4 bars
  - [ ] Only apply when result is divisible by 4
  - [ ] Adjust repeat count accordingly
- [ ] Add method to expand phrases for smart repeats
- [ ] Update display to show expanded phrases with adjusted repeats
- [ ] Add unit tests for smart repeat logic
- [ ] Add integration test with smart repeats enabled/disabled
- [ ] Document behavior when phrase doesn't fit evenly into 4-bar groups

### Edge Cases to Handle

- Phrases that are already 4+ bars
- Phrases that don't divide evenly into 4 bars (e.g., 3-bar phrase)
- Interaction with manual repeat counts
- Interaction with `x^` syntax

---

## 4. Duration Extension Syntax (`_`)

### Description

Ensure proper handling of duration extensions where `_` continues the previous chord/note.

### Example

```
4_8 _8 _8    # Three eighth-note hits of chord 4
S4 s4. 5_8 _8 6_8  # Mix of different chords with extensions
```

### Current Status

- [ ] Verify existing implementation handles `_` correctly
- [ ] Ensure `_` can only follow a chord/note (not start a phrase)
- [ ] Validate that accents work with extensions (e.g., `5-> _8`)
- [ ] Test that duration is correctly accumulated
- [ ] Add clear error messages for invalid `_` usage
- [ ] Add unit tests for various `_` patterns
- [ ] Document `_` syntax in user documentation

---

## 5. Comment Syntax (`;`)

### Description

Support inline comments using `;` prefix.

### Example

```
; (Auto 2 measures of Count In hits on & 4 &)

6_8 6 6 s4 5 5 5 |
4 _8 _8 s4 5-> _8 6
; Hit on & of 4
```

### Implementation Tasks

- [ ] Add `;` comment token recognition in lexer
- [ ] Skip comments during parsing
- [ ] Optionally preserve comments for display
- [ ] Add tests for comments in various positions
- [ ] Handle end-of-line vs full-line comments

---

## 6. Section Label Prefix (`^`)

### Description

Add support for `^` prefix on section labels to indicate special section types (e.g., `^Band-In`).

### Example

```
^Band-In 4
6_8 6 6 s4 s4 5 5 4 |
```

### Implementation Tasks

- [ ] Add `^` recognition before section names
- [ ] Add `section_type` or `prefix` field to Section type
- [ ] Update section parser to handle `^` prefix
- [ ] Update display to show `^` prefix
- [ ] Document meaning of `^` prefix
- [ ] Add tests for prefixed sections

---

## Testing Strategy

### Unit Tests

- [ ] Test each feature independently
- [ ] Test edge cases for each feature
- [ ] Test error conditions and validation

### Integration Tests

- [ ] Create comprehensive test using "All Along The Watchtower"
- [ ] Test feature interactions (e.g., `x^` + smart repeats)
- [ ] Test settings combinations
- [ ] Test full chart parsing with all new features

### Example Files

- [ ] Update `all_along_the_watchtower.rs` as reference example
- [ ] Create additional examples showcasing each feature
- [ ] Add documentation examples

---

## Documentation

- [ ] Update README with new syntax
- [ ] Create syntax reference document
- [ ] Add examples for each feature
- [ ] Document settings/configuration system
- [ ] Add migration guide for existing charts

---

## Priority Order

### Phase 1: Foundation

1. Comment syntax (`;`) - simplest, allows cleaner test files
2. Duration extension verification (`_`)
3. Custom command infrastructure

### Phase 2: Commands

4. Accent command (`->` and `/accent`)
5. Fermata command (`/fermata`)

### Phase 3: Advanced Repeats

6. Smart repeat syntax (`x^`)
7. Settings system (`/SETTING=value`)
8. Smart repeats feature

### Phase 4: Polish

9. Section prefix (`^`)
10. Comprehensive testing
11. Documentation

---

## Notes

- All features should work together seamlessly
- Error messages should be clear and helpful
- Performance should not degrade with new features
- Backward compatibility should be maintained where possible
- Display output should be clean and readable
