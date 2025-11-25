# Missing Features for Complete Song Charting

Based on the "All Along The Watchtower" example and FEATURE_TODO.md, here are the features still needed to chart out an entire song:

## 🚨 Critical Features (Blocking Full Song Charting)

### 1. Custom Section Types
**Status**: ⚠️ **PARTIALLY IMPLEMENTED**

**What's Missing:**
- Custom section names like "Count In", "Down", "GTR SOLO", "END"
- Currently only supports: Intro, Verse, Chorus, Bridge, Outro, Instrumental, Pre/Post

**Example from chart:**
```
Count In 2
S4 s4. 5_8 _8 6_8

Down 8
6 5 4 5 x^

GTR SOLO 16
6 5 4 5 x^

END 1
6 /fermata
```

**Implementation Needed:**
- Allow arbitrary section names that aren't in the standard enum
- Parse custom section types gracefully
- Display custom section names in output
- Handle numbering for custom sections (probably don't number them)

**Priority**: 🔴 **HIGH** - Blocks charting songs with custom sections

---

### 2. Measure Separator (`|`)
**Status**: ❌ **NOT IMPLEMENTED**

**What's Missing:**
- Support for `|` as a measure separator/barline
- Currently `|` is likely being ignored or causing issues

**Example from chart:**
```
6_8 6 6 s4 5 5 5 | 
4 _8 _8 s4 5 _8 6
```

**Implementation Needed:**
- Recognize `|` as a measure boundary marker
- Force a new measure when `|` is encountered
- Useful for visual organization and explicit measure boundaries

**Priority**: 🟡 **MEDIUM** - Nice to have for readability, but not blocking

---

### 3. Smart Repeats Feature (4-bar grouping)
**Status**: ⚠️ **INFRASTRUCTURE EXISTS, ALGORITHM MISSING**

**What's Missing:**
- The `/SMART_REPEATS=true` setting is parsed but doesn't do anything
- Need to implement the algorithm that groups phrases into 4-bar units

**Example:**
```
/SMART_REPEATS=true

VS 16
6 5 4 5 x^
```

**Expected Behavior:**
- Without: `6 5 4 5 x4` (4-bar phrase × 4 = 16 bars)
- With: `6 5 4 5 6 5 4 5 x2` (8-bar phrase × 2 = 16 bars, grouped into 4-bar units)

**Implementation Needed:**
- Detect when phrase is less than 4 bars
- Duplicate phrase to reach 4-bar boundary
- Only apply when result is divisible by 4
- Adjust repeat count accordingly

**Priority**: 🟡 **MEDIUM** - Nice optimization, but not blocking

---

## ✅ Features That Are Working

### Duration Extensions (`_`)
- ✅ `_8`, `_2`, `_4` syntax works
- ✅ Duration extensions like `4 _8 _8` are parsed correctly
- ✅ Chords with explicit durations are handled

### Commands
- ✅ `/fermata` works
- ✅ `->` accent syntax works
- ✅ Commands attach to chords correctly

### Comments
- ✅ `; comment text` is parsed and stripped
- ✅ Comments don't break parsing

### Subsection Prefix
- ✅ `^Band-In` is recognized and parsed
- ✅ Subsection flag is set correctly

### Smart Repeat Syntax (`x^`)
- ✅ Automatically calculates repeat count
- ✅ Works with beat-based calculations
- ✅ Validates divisibility

---

## 🟢 Nice-to-Have Features (Not Blocking)

### 1. Additional Commands
- More musical commands (staccato, legato, etc.)
- Dynamic markings (piano, forte, etc.)
- Articulation marks

### 2. Better Error Messages
- More specific error locations
- Suggestions for fixing common mistakes
- Context-aware error messages

### 3. Export Formats
- MusicXML export
- PDF export
- MIDI export
- JSON/structured data export

### 4. Validation & Linting
- Check for common mistakes
- Warn about unusual patterns
- Suggest optimizations

---

## 📋 Implementation Priority

### Phase 1: Critical (Blocking)
1. **Custom Section Types** - Allow arbitrary section names
2. **Measure Separator (`|`)** - Support explicit measure boundaries

### Phase 2: Important (Enhancement)
3. **Smart Repeats Algorithm** - Implement 4-bar grouping
4. **Better Error Handling** - More helpful error messages

### Phase 3: Polish
5. **Additional Commands** - Expand command system
6. **Export Formats** - Add export capabilities
7. **Documentation** - Complete user guide

---

## 🎯 Quick Wins

### Custom Section Types
This is probably the easiest critical feature to implement:
- Add a `Custom(String)` variant to `SectionType` enum
- Update parser to fall back to custom when standard types don't match
- Handle display and numbering appropriately

### Measure Separator
Also relatively straightforward:
- Detect `|` token in parser
- Force measure boundary when encountered
- Ignore in duration calculations

---

## 📊 Current Status Summary

| Feature | Status | Priority | Effort |
|---------|--------|----------|--------|
| Custom Section Types | ❌ Missing | 🔴 High | Medium |
| Measure Separator (`\|`) | ❌ Missing | 🟡 Medium | Low |
| Smart Repeats Algorithm | ⚠️ Partial | 🟡 Medium | Medium |
| Duration Extensions | ✅ Working | - | - |
| Commands (`/fermata`, `->`) | ✅ Working | - | - |
| Comments (`;`) | ✅ Working | - | - |
| Subsection Prefix (`^`) | ✅ Working | - | - |
| Smart Repeat (`x^`) | ✅ Working | - | - |

**Overall**: ~85% complete for basic charting, ~95% complete with custom sections added.

---

## 💡 Recommendation

To chart out an entire song right now, you need:
1. **Custom Section Types** - This is the main blocker
2. **Measure Separator** - Nice to have for readability

Everything else is either working or a nice-to-have enhancement. The core chord system, parsing, and most features are solid.

