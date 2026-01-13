# Layout Engine Architecture: MuseScore-Accurate Sheet Music Rendering

**Document Version:** 1.0
**Date:** 2026-01-13
**Status:** Phase 0 Complete (Foundation)

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Current State Analysis](#current-state-analysis)
3. [Architecture Design](#architecture-design)
4. [Horizontal Spacing Algorithm](#horizontal-spacing-algorithm)
5. [Implementation Phases](#implementation-phases)
6. [Phase 0: Completion Report](#phase-0-completion-report)
7. [Next Steps](#next-steps)
8. [Reference Materials](#reference-materials)

---

## Executive Summary

### Goals

This document outlines the design and implementation of a professional music engraving system in `packages/engraver/` by porting MuseScore's proven layout engine (~42,000 lines of C++) to idiomatic Rust.

**Primary Objectives:**
- Implement MuseScore's core layout components: `LayoutContext`, `HorizontalSpacing`, `Shape` collision, and `TLayout` factory
- Initial scope: Lead sheet elements (chord symbols, rhythm slashes, text, rehearsal marks)
- Long-term: Support full notation (beams, slurs, lyrics, multi-voice, orchestral scores)
- Quality target: Match MuseScore's professional spacing and positioning accuracy

### Timeline

- **Phase 0** (Weeks 1-2): Foundation - Core types and traits ✅ **COMPLETE**
- **Phase 1** (Weeks 3-6): Horizontal Spacing - Spring-based algorithm
- **Phase 2** (Weeks 7-8): Shape & Collision - Detection system
- **Phase 3** (Weeks 9-13): Element Layouts - Lead sheet elements
- **Phase 4** (Weeks 14-15): System Assembly - Multi-page layout

**Total:** 15 weeks for complete lead sheet rendering with MuseScore accuracy

---

## Current State Analysis

### Exploration Summary

An extensive exploration of the codebase revealed three key areas:

#### 1. Current Sheet Music Renderers

**Keyflow Package** (`packages/keyflow/`)
- **Purpose:** Music chart parsing and data model (NOT a renderer)
- **Capabilities:**
  - Chord symbol parsing (Nashville, Roman numerals, standard notation)
  - Chart structure management (sections, measures, beats)
  - Rhythm slash generation
  - Time signatures and key changes
- **Output:** Structured chart data (no visual rendering)

**Engraver Package** (`packages/engraver/`)
- **Purpose:** WGPU-based music notation renderer
- **Architecture:** Model → Layout → Scene → Renderer (Vello/WGPU)
- **Key Components:**
  - Model System: Score, Measure, Note, Voice
  - Font System: SMuFL (Leland, Bravura)
  - Style System: MStyle with 79 properties (MuseScore-compatible)
  - Renderer: WGPU 26 + Lyon tessellation + glyphon text
- **Status:** Basic primitives working, layout engine is stub (34 lines)
- **Examples:**
  - `music_symbols.rs` - SMuFL glyph rendering
  - `canvas_shapes.rs` - Canvas2D API
  - `canvas_sdf.rs` - SDF rounded rectangles

**FTS-Native Chart Renderer** (`apps/fts-native/src/chart_renderer.rs`)
- **Purpose:** Production lead sheet renderer (3,020 lines)
- **Features:**
  - MuseScore-compatible page layout
  - Chord symbol rendering (ParsedChord system)
  - Rhythm slash rendering (SMuFL glyphs)
  - System layout (4 measures per line)
  - Rehearsal marks (SDF capsules)
- **Status:** Working implementation demonstrating feasibility

#### 2. MuseScore Architecture

**Core Files Analyzed:**
- `horizontalspacing.cpp` (1,906 lines) - Note spacing engine
- `harmonylayout.cpp` (934 lines) - Chord symbol positioning
- `chordlayout.cpp` (3,472 lines) - Note/chord positioning
- `systemlayout.cpp` (3,263 lines) - System assembly
- `measurelayout.cpp` (3,090 lines) - Measure layout
- `beamlayout.cpp` (1,451 lines) - Beam calculations
- `shape.cpp` - Collision detection

**Key Architectural Patterns:**

1. **LayoutContext** - Central orchestrator containing:
   - `LayoutConfiguration`: Style settings, fonts, view mode
   - `DomAccessor`: Read-only score data access
   - `LayoutState`: Current layout progress

2. **HorizontalSpacing** - Spring-based spacing algorithm:
   - Spring system with natural width, stiffness, pre-tension
   - Duration-based stretching (logarithmic scaling)
   - Shape-based collision detection
   - Kerning for compatible elements
   - Justification via spring relaxation

3. **Shape** - Collision detection via horizontal slices:
   - Collection of rectangles representing element bounds
   - Skyline algorithm for vertical spacing
   - Minimum distance calculations

4. **TLayout** - Element-specific layout factory (6,719 lines):
   - Static factory class with 100+ methods
   - One method per element type
   - Handles all positioning logic

5. **Multi-Pass Layout Flow:**
   ```
   ScoreLayout
   ↓
   PageLayout → Collect systems
   ↓
   SystemLayout → Collect measures
   ↓
   MeasureLayout → Create segments, compute spacing
   ↓
   TLayout → Layout individual elements
   ↓
   Autoplace → Collision avoidance
   ```

#### 3. Rendering Tech Stack

**Current Infrastructure:**

- **WGPU 26** - Direct GPU access
- **Lyon 1.0** - Path tessellation (fills and strokes)
- **glyphon** - GPU text rendering (wgpu 26 fork)
- **Vello 0.4** - 2D vector renderer (not actively used yet)
- **kurbo 0.11** - 2D geometry (Rect, Point, Affine)
- **SMuFL fonts** - Leland, Bravura with metadata

**Rendering Approaches:**

1. **Canvas2D API** (`renderer/canvas2d.rs`)
   - HTML Canvas-like API
   - Lyon tessellation
   - Methods: `fill_rect()`, `stroke_circle()`, etc.

2. **SDF Rendering** (`examples/canvas_sdf.rs`)
   - Pixel-perfect rounded rectangles
   - Custom WGSL fragment shader
   - Zoom-aware anti-aliasing

3. **Glyph Tessellation** (`fonts/tessellation.rs`)
   - SMuFL glyphs → triangles (Lyon)
   - OTF/TTF outline parsing (skrifa)
   - World-space or NDC coordinates

**Rendering Pipeline:**
```
Music Notation Data (keyflow::Chart)
↓
Layout System (engraver::layout)
↓
Rendering Primitives
↓
┌────┴────┬──────────┬────────┐
Canvas2D  SDF Rects  Glyphs   Text
(Lyon)    (Shader)   (Lyon)   (glyphon)
↓         ↓          ↓        ↓
WGPU Render Pass
↓
Screen
```

### Existing Foundation ✅

**Complete Systems:**
- **MStyle System** (`packages/engraver/src/style/mod.rs`)
  - 79 MuseScore-compatible properties
  - All spacing properties: `BarNoteDistance`, `MinNoteDistance`, `MeasureSpacing`
  - Typed accessors: `spatium()`, `real()`, `bool()`

- **Model System** (`packages/engraver/src/model/`)
  - Complete Score/Measure/Note/Voice data structures
  - Duration, Pitch, Accidental types
  - Element types: Note, Rest, Chord, Clef, KeySignature, TimeSignature

- **Font System** (`packages/engraver/src/fonts/`)
  - SMuFL font loading (Leland, Bravura)
  - Glyph metadata (bounding boxes, anchors)
  - Tessellation to GPU triangles

- **Renderer** (`packages/engraver/src/renderer/`)
  - WGPU 26 with multiple pipelines
  - Lyon tessellation integration
  - glyphon text rendering
  - SDF shader system

- **Working Example**
  - 3,020-line chart renderer in `apps/fts-native/src/chart_renderer.rs`
  - Demonstrates all rendering techniques
  - MuseScore-compatible page layout

### What's Missing ❌

- **Layout Engine** - Only 34-line stub in `packages/engraver/src/layout/mod.rs`
- **Shape System** - No collision detection geometry
- **Spacing Algorithm** - No spring-based horizontal spacing
- **Element Positioning** - Ad-hoc logic scattered in chart_renderer

---

## Architecture Design

### Module Structure

```
packages/engraver/src/
├── layout/                         # NEW LAYOUT ENGINE
│   ├── mod.rs                      # Public API: layout_score()
│   ├── context.rs                  # LayoutContext orchestrator ✅ COMPLETE
│   ├── shape.rs                    # Shape collision detection ✅ COMPLETE
│   ├── spacing.rs                  # HorizontalSpacing (springs)
│   ├── springs.rs                  # Spring system math
│   ├── segment.rs                  # Segment abstraction
│   ├── autoplace.rs                # Collision avoidance
│   ├── tlayout/                    # Element-specific layouts
│   │   ├── mod.rs                  # Layout trait + dispatch ✅ COMPLETE
│   │   ├── harmony.rs              # Chord symbols
│   │   ├── text.rs                 # Generic text
│   │   ├── measure.rs              # Measures
│   │   ├── chord.rs                # Notes/chords
│   │   └── system.rs               # Systems
│   └── duration_stretch.rs         # Duration-based spacing
```

### Core Types

#### 1. LayoutContext (Orchestrator) ✅ COMPLETE

**Location:** `packages/engraver/src/layout/context.rs` (180 lines)

```rust
pub struct LayoutContext<'score> {
    pub config: LayoutConfiguration,
    pub score: &'score Score,
    pub style: &'score MStyle,
    pub font: &'score SMuFLFont<'score>,
    pub state: RefCell<LayoutState>,  // Interior mutability for state
}
```

**Key Features:**
- Lifetime-bound to score (`'score`) - prevents use-after-free
- Immutable configuration and score access
- RefCell for mutable state tracking
- Helper methods: `spatium()`, `style_distance()`, `style_real()`, `style_bool()`

**Pattern:** Pass as `&LayoutContext` parameter (no global state)

**Tests:** 2 passing tests for configuration and state defaults

#### 2. Shape (Collision Detection) ✅ COMPLETE

**Location:** `packages/engraver/src/layout/shape.rs` (330 lines)

```rust
pub enum Shape {
    Fixed { bbox: Rect, element: Option<ElementId> },
    Composite {
        elements: Cow<'static, [ShapeElement]>,
        bbox_cache: Option<Rect>,
    },
}

impl Shape {
    pub fn min_horizontal_distance(&self, other: &Shape, spatium: f64) -> f64;
    pub fn translate(&self, offset: Point) -> Shape;
}
```

**Key Features:**
- Enum optimization: Fixed variant for common single-rect case
- Cow<> for zero-copy optimization with static shapes
- Horizontal slice-based collision detection
- Methods: `bbox()`, `left()`, `right()`, `top()`, `bottom()`, `add_rect()`

**Pattern:** Use horizontal slices like MuseScore, Cow<> for zero-copy

**Tests:** 9 passing tests covering collision detection, translation, composition

#### 3. Layout Trait (TLayout Equivalent) ✅ COMPLETE

**Location:** `packages/engraver/src/layout/tlayout/mod.rs` (167 lines)

```rust
pub trait Layout {
    fn layout(&self, ctx: &LayoutContext) -> LayoutData;
    fn shape(&self, ctx: &LayoutContext) -> Shape;
    fn natural_width(&self, ctx: &LayoutContext) -> f64;
}

pub struct LayoutData {
    pub position: Point,
    pub bbox: Rect,
    pub shape: Shape,
    pub children: Vec<(ElementId, LayoutData)>,
}
```

**Key Features:**
- Trait-based dispatch instead of 6,719-line TLayout.cpp
- Each element type gets dedicated module (harmony.rs, chord.rs, etc.)
- Enum dispatch compiles to jump table (zero-cost abstraction)
- Hierarchical layout data with children

**Pattern:** Implement for concrete types (Note, Harmony), use enum dispatch via MusicElement

**Tests:** 2 passing tests for LayoutData creation and translation

#### 4. Segment (Spacing Unit) - Phase 1

**Planned Location:** `packages/engraver/src/layout/segment.rs`

```rust
pub struct Segment {
    pub seg_type: SegmentType,  // ChordRest, Clef, KeySig, etc.
    pub ticks: i32,             // Musical position
    pub elements: Vec<Option<SegmentElement>>,
    pub shape: Shape,           // Collision boundary
    pub x: f32,                 // Computed position
    pub width: f32,             // Computed width
}
```

**Pattern:** MuseScore's fundamental spacing unit - all elements at same time

#### 5. Spring (Justification) - Phase 1

**Planned Location:** `packages/engraver/src/layout/springs.rs`

```rust
pub struct Spring {
    pub spring_const: f64,      // Stiffness = 1.0 / natural_width
    pub width: f64,             // Current width
    pub pre_tension: f64,       // Minimum width from duration
    pub segment_index: usize,
}

pub fn stretch_springs_to_width(springs: &mut [Spring], target_width: f64);
```

**Pattern:** Distribute space proportionally based on spring forces

### Unit Conversions ✅ COMPLETE

**Location:** `packages/engraver/src/layout/mod.rs` (130 lines)

```rust
// Newtype wrappers for dimensional safety
pub struct Spatium(pub f64);  // Staff spaces
pub struct Points(pub f64);   // 1/72 inch
pub struct Pixels(pub f64);   // Device units

impl Spatium {
    pub fn to_points(self, base_spatium: f64) -> Points;
    pub fn to_pixels(self, base_spatium: f64, dpi: f64) -> Pixels;
}
```

**Benefits:**
- Prevents unit confusion at compile time
- Clear intent in function signatures
- Automatic conversions via methods

**Tests:** 4 passing tests for unit conversions

### Key Rust Patterns

#### 1. Lifetime Management
```rust
LayoutContext<'score>  // Ties layout to score lifetime
                       // Prevents use-after-free
                       // Enables zero-copy references
```

#### 2. Interior Mutability
```rust
pub state: RefCell<LayoutState>  // Allows mutation through &LayoutContext
                                  // Needed for progress tracking
```

#### 3. Cow<> Optimization
```rust
Composite {
    elements: Cow<'static, [ShapeElement]>,  // Zero-copy for static shapes
}
```

#### 4. Trait Dispatch
```rust
impl Layout for Note { ... }
impl Layout for Harmony { ... }

// Enum dispatch compiles to jump table (zero-cost)
match element {
    MusicElement::Note(n) => n.layout(ctx),
    MusicElement::Harmony(h) => h.layout(ctx),
}
```

#### 5. Newtype Wrappers
```rust
struct Spatium(f64);   // Staff spaces
struct Points(f64);    // 1/72 inch
struct Pixels(f64);    // Device units

// Prevents unit confusion at compile time
```

---

## Horizontal Spacing Algorithm

### Overview

MuseScore's core spacing algorithm (to be ported from `horizontalspacing.cpp` - 1,906 lines):

### Algorithm Phases

#### Phase 1: Segment Creation
- Group elements by tick position
- Create Segment for each time point
- Calculate bounding shapes for each segment

#### Phase 2: Natural Width Calculation
```rust
natural_width = shape_width * duration_stretch_factor
duration_stretch = slope * log2(duration / quarter_note) + 1.0
```

**Key Formula:**
- Logarithmic scaling based on note duration
- Quarter note = baseline (stretch factor = 1.0)
- Whole note (4 quarters) = stretch factor ≈ 4.0
- Eighth note (0.5 quarters) = stretch factor ≈ 0.5

#### Phase 3: Collision Detection Loop
```rust
for each segment i:
    x = max(x_cur, max_over_j(segments[j].x + min_distance(j, i)))
    segments[i].x = x
```

**Key Operations:**
- Check all previous segments for collisions
- Use `Shape::min_horizontal_distance()`
- Apply minimum spacing margins from MStyle

#### Phase 4: Spring Creation
```rust
springs[i] = Spring {
    spring_const: 1.0 / natural_width,
    width: current_width,
    pre_tension: duration_stretch,
}
```

**Spring Physics:**
- Stiffer springs (small natural width) resist more
- Looser springs (large natural width) stretch more
- Pre-tension enforces minimum spacing

#### Phase 5: Justification
```rust
stretch_springs_to_width(springs, target_width);
// Distribute extra space proportionally to spring forces
```

**Distribution:**
- Calculate total force: `Σ(spring.force())`
- Distribute proportionally: `Δwidth = extra_space * (force_i / total_force)`
- Springs with higher force get less stretch

#### Phase 6: Kerning (Optional)
- Compatible elements can overlap (note+accidental, rest+note)
- Context-aware padding rules
- Reduces spacing for compatible pairs

### Data Structures

#### Spring System

```rust
pub struct Spring {
    /// Spring constant (stiffness) = 1.0 / natural_width
    pub spring_const: f64,
    /// Current width of the spring
    pub width: f64,
    /// Pre-tension from duration stretching
    pub pre_tension: f64,
    /// Associated segment
    pub segment_index: usize,
}

impl Spring {
    /// Calculate the force on this spring
    pub fn force(&self) -> f64 {
        self.pre_tension + (self.width * self.spring_const)
    }
}
```

#### Duration Stretch

```rust
pub fn duration_stretch_for_duration(duration: Duration, slope: f64) -> f64 {
    let quarters = duration.quarters();
    let ticks = quarters * 480.0; // MuseScore ticks
    let min_ticks = 480.0;        // Quarter note baseline

    if ticks < min_ticks {
        // Shorter than quarter: linear scale
        return ticks / min_ticks;
    }

    // Logarithmic stretch for longer durations
    slope * (ticks / min_ticks).log2() + 1.0
}
```

#### Collision Detection

```rust
pub fn min_horizontal_distance(
    first: &Segment,
    second: &Segment,
    ctx: &SpacingContext,
) -> f64 {
    // Use shape-based collision detection
    let base_distance = first.shape.min_horizontal_distance(
        &second.shape,
        ctx.spatium,
    );

    // Apply squeeze factor
    let squeezed = base_distance * ctx.squeeze_factor;

    // Add special case padding (lyrics, cross-staff beams)
    let special_padding = compute_special_padding(first, second, ctx);

    (squeezed + special_padding).max(0.0)
}
```

#### Kerning System

```rust
pub enum KerningType {
    None,      // No kerning allowed
    Normal,    // Standard kerning
    Overlap,   // Elements can overlap
}

pub fn compute_kerning(elem1: &MusicElement, elem2: &MusicElement) -> KerningType {
    match (elem1, elem2) {
        (Note(_), Note(n2)) if n2.accidental != Accidental::None => {
            KerningType::Overlap  // Note followed by accidental
        }
        (Rest(_), Note(_)) => KerningType::Normal,
        _ => KerningType::None,
    }
}
```

### Integration with MStyle

The spacing algorithm uses MStyle properties:

- `Sid::MeasureSpacing` (default: 1.5) - Slope for duration stretch
- `Sid::BarNoteDistance` (default: 1.5 sp) - Space after barline
- `Sid::NoteBarDistance` (default: 1.0 sp) - Space before barline
- `Sid::MinNoteDistance` (default: 0.5 sp) - Minimum between notes

### Special Cases

1. **Cross-staff beams** - Need extra spacing
2. **Lyrics vs barlines** - Lyrics need clearance
3. **Grace notes** - Positioned after main spacing
4. **Multi-measure rests** - Custom stretch logic
5. **Large time signatures** - Check against margins

---

## Implementation Phases

### Phase 0: Foundation ✅ COMPLETE

**Duration:** Weeks 1-2
**Goal:** Core types and traits compile
**Status:** ✅ Complete (2026-01-13)

**Deliverables:**
- ✅ `LayoutContext<'score>` in `layout/context.rs` (180 lines)
- ✅ `Layout` trait in `layout/tlayout/mod.rs` (167 lines)
- ✅ `Shape` enum in `layout/shape.rs` (330 lines)
- ✅ Newtype wrappers in `layout/mod.rs` (Spatium, Points, Pixels)
- ✅ Module exports and documentation
- ✅ Unit tests (16 new tests, 61 total passing)

**Success Criteria:** ✅ Code compiles, basic unit tests pass

### Phase 1: Horizontal Spacing (Weeks 3-6)

**Goal:** Port MuseScore's HorizontalSpacing algorithm

#### Sub-phases:

**1a. Springs (Week 3)**
- Implement `Spring` struct in `layout/springs.rs`
- Implement `stretch_springs_to_width()` solver
- Unit tests: verify spring distribution math

**Files:**
- `/packages/engraver/src/layout/springs.rs` (new)

**Tests:**
```rust
#[test]
fn test_spring_solving() {
    let springs = vec![
        Spring::new(10.0, 0.0, 0),
        Spring::new(20.0, 0.0, 1),
    ];
    stretch_springs_to_width(&mut springs, 45.0);
    // Verify proportional distribution
}
```

**1b. Duration Stretch (Week 3)**
- Implement `duration_stretch_for_duration()` in `layout/duration_stretch.rs`
- Logarithmic scaling: `slope * log2(ticks / min_ticks) + 1.0`
- Tests: quarter=1.0, whole=4.0, eighth=0.5

**Files:**
- `/packages/engraver/src/layout/duration_stretch.rs` (new)

**Tests:**
```rust
#[test]
fn test_duration_stretch_quarter_note() {
    assert_eq!(duration_stretch_for_duration(Duration::QUARTER, 1.5), 1.0);
}

#[test]
fn test_duration_stretch_whole_note() {
    let whole = Duration::WHOLE;
    let stretch = duration_stretch_for_duration(whole, 1.5);
    assert!((stretch - 4.0).abs() < 0.1);
}
```

**1c. Shape Distance (Week 4)**
- Expand `Shape::min_horizontal_distance()` with full algorithm
- Implement horizontal slice method
- Tests: overlapping rectangles return correct distances

**Files:**
- `/packages/engraver/src/layout/shape.rs` (expand existing)

**Reference:** `musescore/src/engraving/infrastructure/shape.cpp`

**1d. Segment System (Week 5)**
- Implement `Segment` in `layout/segment.rs`
- Implement `SegmentType` enum (ChordRest, Clef, KeySig, etc.)
- Create segments from Measure/Voice elements
- Tests: correct segment grouping by tick position

**Files:**
- `/packages/engraver/src/layout/segment.rs` (new)

**Data Structures:**
```rust
pub struct Segment {
    pub seg_type: SegmentType,
    pub ticks: i32,
    pub elements: Vec<Option<SegmentElement>>,
    pub shape: Shape,
    pub x: f32,
    pub width: f32,
}

pub enum SegmentType {
    ChordRest,
    Clef,
    KeySig,
    TimeSig,
    StartRepeatBarline,
    Barline,
    EndBarline,
}
```

**1e. Main Algorithm (Week 6)**
- Implement `compute_measure_spacing()` in `layout/spacing.rs`
- Integrate: segments → natural widths → collision → springs → positions
- Tests: verify against MuseScore reference scores

**Files:**
- `/packages/engraver/src/layout/spacing.rs` (new)

**Reference:** `musescore/src/engraving/rendering/score/horizontalspacing.cpp` (1,906 lines)

**Success Criteria:** Beat positions match MuseScore within 0.5 spatium

### Phase 2: Shape & Collision (Weeks 7-8)

**Goal:** Complete collision detection system

**Tasks:**
1. Expand `Shape` with `translate()`, `union()`, `bbox()` methods (already have translate)
2. Implement spatial indexing (R-tree) for fast queries
3. Implement `CollisionDetector` in `layout/autoplace.rs`
4. Add autoplace algorithm for overlapping elements
5. Performance testing: < 100ms for 100-measure score

**Dependencies:**
- Add `rstar = "0.12"` to `Cargo.toml` for R-tree

**Files:**
- `/packages/engraver/src/layout/shape.rs` (complete)
- `/packages/engraver/src/layout/autoplace.rs` (new)

**Reference:** `musescore/src/engraving/infrastructure/shape.cpp`

**Data Structures:**
```rust
pub struct CollisionDetector {
    tree: RTree<ShapeElement>,
}

impl CollisionDetector {
    pub fn intersecting(&self, shape: &Shape) -> Vec<ElementId>;
    pub fn min_vertical_offset(&self, shape: &Shape, x_range: Range<f64>) -> f64;
}
```

**Success Criteria:** No overlapping elements in test scores

### Phase 3: Element Layouts - Lead Sheet (Weeks 9-13)

**Goal:** Implement TLayout equivalents for lead sheet elements

**Priority Order** (by dependency):

**3a. Text Layout (Week 9)**
- Generic text rendering in `layout/tlayout/text.rs`
- Font metrics integration with glyphon
- Bounding box calculation
- Tests: text positioning accuracy

**Files:**
- `/packages/engraver/src/layout/tlayout/text.rs` (new)

**3b. Chord Symbols (Week 10)**
- Port `HarmonyLayout` in `layout/tlayout/harmony.rs`
- Implement `ParsedChord` structure (root, quality, extension, bass)
- MuseScore-accurate horizontal alignment
- Above-staff positioning with collision avoidance
- Tests: match existing chart_renderer output

**Files:**
- `/packages/engraver/src/layout/tlayout/harmony.rs` (new)

**Reference:** `musescore/src/engraving/rendering/score/harmonylayout.cpp` (934 lines)

**3c. Measure Layout (Week 11)**
- Implement `MeasureLayout` in `layout/tlayout/measure.rs`
- Barlines, time signatures, clefs
- Segment organization and spacing integration
- Tests: measure width calculations

**Files:**
- `/packages/engraver/src/layout/tlayout/measure.rs` (new)

**Reference:** `musescore/src/engraving/rendering/score/measurelayout.cpp` (3,090 lines)

**3d. Notes/Chords (Week 12)**
- Basic note positioning in `layout/tlayout/chord.rs`
- Stem direction calculation
- Simple accidental placement (no complex collision yet)
- Notehead selection from SMuFL
- Tests: verify stem lengths

**Files:**
- `/packages/engraver/src/layout/tlayout/chord.rs` (new)

**Reference:** `musescore/src/engraving/rendering/score/chordlayout.cpp` (3,472 lines, simplified)

**3e. Rhythm Slashes (Week 13)**
- Slash notehead positioning
- Alignment with beat positions from spacing algorithm
- Integration with existing rhythm slash rendering
- Tests: visual regression against chart_renderer

**Success Criteria:** Render lead sheets matching MuseScore appearance

### Phase 4: System Assembly (Weeks 14-15)

**Goal:** Multi-system and multi-page layout

**Tasks:**
1. Implement `SystemLayout` in `layout/tlayout/system.rs`
2. System spacing and page breaks
3. Vertical distribution of systems
4. Integration: `layout_score()` orchestrates all passes
5. Incremental relayout support for interactive editing

**Files:**
- `/packages/engraver/src/layout/tlayout/system.rs` (new)
- `/packages/engraver/src/layout/mod.rs` (add `layout_score()`)

**Reference:** `musescore/src/engraving/rendering/score/systemlayout.cpp` (3,263 lines)

**Entry Point:**
```rust
pub fn layout_score(ctx: &LayoutContext) -> SceneGraph {
    let mut builder = LayoutBuilder::new(ctx);
    builder.compute_systems()?;      // Phase 1
    builder.apply_spacing()?;        // Phase 1
    builder.apply_vertical_spacing()?; // Phase 2
    builder.build()                  // Phase 4
}
```

**Success Criteria:** Multi-page scores render correctly with proper system breaks

---

## Phase 0: Completion Report

### Implementation Summary

**Date Completed:** 2026-01-13
**Duration:** Initial implementation session
**Status:** ✅ All Phase 0 objectives met

### Created Files

1. **`packages/engraver/src/layout/context.rs`** (180 lines)
   - `LayoutContext<'score>` with lifetime management
   - `LayoutConfiguration` for immutable settings
   - `LayoutState` for mutable progress tracking
   - Helper methods: `spatium()`, `style_distance()`, `style_real()`, `style_bool()`
   - Tests: `test_layout_configuration_default()`, `test_layout_state_default()`

2. **`packages/engraver/src/layout/tlayout/mod.rs`** (167 lines)
   - `Layout` trait with `layout()`, `shape()`, `natural_width()` methods
   - `LayoutData` struct for computed positions
   - Hierarchical layout with children support
   - Tests: `test_layout_data_new()`, `test_layout_data_translate()`

3. **`packages/engraver/src/layout/shape.rs`** (330 lines)
   - `Shape` enum: Fixed (optimized) and Composite variants
   - `ShapeElement` with rectangle, element reference, ignore flag
   - Methods: `from_rect()`, `translate()`, `bbox()`, `min_horizontal_distance()`
   - Cow<> optimization for zero-copy immutable shapes
   - Tests: 9 comprehensive tests covering all operations

4. **`packages/engraver/src/layout/mod.rs`** (refactored from 34-line stub to 130 lines)
   - Module organization and exports
   - Newtype wrappers: `Spatium`, `Points`, `Pixels`
   - Unit conversion methods with proper DPI handling
   - Comprehensive module documentation
   - Tests: 4 unit conversion tests

### Modified Files

1. **`packages/engraver/src/model/element.rs`**
   - Added `ElementId(usize)` newtype for element tracking
   - Used by layout system for collision detection references

2. **`packages/engraver/src/model/mod.rs`**
   - Exported `ElementId` for public use

### Test Results

**Total Tests:** 61 passing (16 new layout tests)
- `layout::context` - 2 tests
- `layout::shape` - 9 tests
- `layout::tlayout` - 2 tests
- `layout::mod` - 4 tests (unit conversions)

**Compilation:**
- ✅ Zero errors
- ✅ Zero warnings (after lifetime fixes)
- ✅ All doctests pass (5 ignored example blocks)

### Code Quality

**Rust Idioms:**
- ✅ Lifetime management (`LayoutContext<'score>`)
- ✅ Interior mutability (`RefCell<LayoutState>`)
- ✅ Zero-cost abstractions (Cow<>, trait dispatch)
- ✅ Type safety (newtype wrappers)
- ✅ Clear documentation (rustdoc comments)

**Architecture:**
- ✅ Modular design (separate files for each component)
- ✅ MuseScore patterns adapted to Rust
- ✅ Clean separation of concerns
- ✅ Testable components

### Phase 0 Success Criteria Met

✅ **Core types compile** - All new types build without errors
✅ **Basic unit tests pass** - 16 new tests, all passing
✅ **Rust idioms in place** - Lifetimes, newtypes, Cow<>
✅ **MuseScore patterns established** - Context, Shape, Layout trait
✅ **Foundation ready** - Prepared for Phase 1 (Horizontal Spacing)

### Lines of Code

| Component | Lines | Status |
|-----------|-------|--------|
| `layout/context.rs` | 180 | ✅ Complete |
| `layout/shape.rs` | 330 | ✅ Complete |
| `layout/tlayout/mod.rs` | 167 | ✅ Complete |
| `layout/mod.rs` | 130 | ✅ Complete |
| **Total New Code** | **807** | **✅ Complete** |

### Documentation

- ✅ Module-level documentation (`//!`)
- ✅ Struct/trait documentation (`///`)
- ✅ Example code blocks (ignored for now, will work when integrated)
- ✅ Inline comments for complex logic
- ✅ Architecture diagrams in this document

---

## Next Steps

### Immediate: Phase 1 Implementation

**Phase 1a: Springs (Week 3)**

**Priority:** HIGH
**Estimated Effort:** 2-3 days

**Tasks:**
1. Create `packages/engraver/src/layout/springs.rs`
2. Implement `Spring` struct with `spring_const`, `width`, `pre_tension`
3. Implement `stretch_springs_to_width()` solver
4. Add unit tests for spring distribution math
5. Verify against MuseScore algorithm

**Acceptance Criteria:**
- Springs distribute space proportionally to forces
- Tests verify equal springs get equal stretch
- Tests verify stiffer springs resist more

**Phase 1b: Duration Stretch (Week 3)**

**Priority:** HIGH
**Estimated Effort:** 2 days

**Tasks:**
1. Create `packages/engraver/src/layout/duration_stretch.rs`
2. Implement logarithmic duration scaling
3. Add tests for all duration types
4. Verify quarter note baseline = 1.0

**Acceptance Criteria:**
- Quarter note stretch factor = 1.0
- Whole note stretch factor ≈ 4.0
- Eighth note stretch factor ≈ 0.5

### Integration Strategy

**Existing Model Integration:**

1. **Phases 0-3:** Keep existing `MeasureLayout` types in `model/measure_layout.rs`
2. **Phase 4:** Gradually migrate to new layout system
3. Add feature flag `use_new_layout` for A/B testing
4. Deprecate old types once feature-complete

**Chart Renderer Migration:**

1. **Parallel Development:** Keep `chart_renderer.rs` working during layout engine build
2. **Phase 3 Complete:** Add `ChartRenderer::render_with_new_layout()`
3. **Testing:** A/B test old vs new rendering
4. **Phase 4:** Remove old layout code, use new engine exclusively

**Renderer Integration:**

1. New layout produces `LayoutData` with positions
2. Renderer consumes `LayoutData` → Vello/WGPU primitives
3. Same rendering pipeline (Lyon tessellation, glyphon text)
4. Zero changes to rendering infrastructure

### Dependencies to Add

**Phase 1:** None (all dependencies already present)

**Phase 2:**
```toml
[dependencies]
rstar = "0.12"         # NEW: R-tree spatial indexing
nalgebra = "0.33"      # NEW: Linear algebra (spring solver)
```

**Already Available:**
- `kurbo = "0.11"` - Rect, Point, Affine
- `vello = "0.4"` - Rendering
- `smufl = "0.2"` - SMuFL metadata
- `lyon = "1.0"` - Tessellation
- `glyphon` - Text rendering
- `wgpu = "26"` - GPU access

### Performance Targets

- **Layout Time:** < 100ms for 100-measure lead sheet
- **Memory:** < 10MB overhead vs model size
- **Incremental Relayout:** < 10ms for single-measure edit (Phase 5+)

### Success Metrics

**Phase Completion Criteria:**
- **Phase 0:** ✅ Types compile, basic tests pass
- **Phase 1:** Measure spacing matches MuseScore within 0.5 spatium
- **Phase 2:** Zero collisions in test suite (100 test scores)
- **Phase 3:** Visual regression tests pass (95%+ pixel match)
- **Phase 4:** Multi-page layout renders correctly

**Quality Gates:**
- All unit tests pass
- Integration tests verify spacing accuracy
- Visual regression tests show < 5% diff vs MuseScore
- Performance benchmarks meet targets
- Code review by maintainer

---

## Reference Materials

### MuseScore Source Files

**Critical Files** (in `/libs/reference/sheet-music/musescore/src/engraving/`):

1. **`rendering/score/horizontalspacing.cpp`** (1,906 lines)
   - Main spacing algorithm
   - Spring system implementation
   - Collision detection integration

2. **`rendering/score/harmonylayout.cpp`** (934 lines)
   - Chord symbol positioning
   - Text alignment algorithms
   - Fretboard diagram integration

3. **`rendering/score/chordlayout.cpp`** (3,472 lines)
   - Note positioning
   - Stem direction and length
   - Accidental collision detection
   - Dot placement

4. **`rendering/score/measurelayout.cpp`** (3,090 lines)
   - Segment creation
   - Measure width calculation
   - Barline and clef spacing

5. **`rendering/score/systemlayout.cpp`** (3,263 lines)
   - System assembly
   - Vertical spacing
   - Page breaks
   - Autoplace integration

6. **`infrastructure/shape.cpp`**
   - Shape collision detection
   - Horizontal slice algorithm
   - Skyline computation

### Internal Documentation

1. **`/docs/CHART_RENDERING_ARCHITECTURE.md`**
   - Existing chart renderer documentation
   - Rhythm slash system design
   - MuseScore positioning algorithms
   - Coordinate systems

2. **`/docs/LAYOUT_ENGINE_ARCHITECTURE.md`** (this document)
   - Complete layout engine design
   - Implementation phases
   - Progress tracking

### Key Papers and Resources

1. **MuseScore Design Documentation**
   - https://musescore.org/en/handbook/3/layout-and-formatting
   - Engraving standards and conventions

2. **SMuFL Specification**
   - https://w3c.github.io/smufl/
   - Standard Music Font Layout
   - Glyph metadata and anchors

3. **Music Notation Best Practices**
   - Elaine Gould: "Behind Bars"
   - Ted Ross: "The Art of Music Engraving"

### Code Examples

**Current Working Examples:**

1. **`packages/engraver/examples/music_symbols.rs`**
   - SMuFL glyph rendering
   - MuseScore-style page layout
   - System spacing demonstration

2. **`packages/engraver/examples/canvas_shapes.rs`**
   - Canvas2D API usage
   - Shape tessellation
   - Zoom and pan controls

3. **`packages/engraver/examples/canvas_sdf.rs`**
   - SDF rounded rectangles
   - Text rendering with glyphon
   - Coordinate system alignment

4. **`apps/fts-native/src/chart_renderer.rs`** (3,020 lines)
   - Production chart rendering
   - Complete integration example
   - All rendering techniques

### Testing Resources

**Visual Regression Testing:**
- Test scores from MuseScore examples
- Render with both old and new systems
- Pixel-diff comparison tools
- 95%+ match threshold

**Unit Test Patterns:**
```rust
#[test]
fn test_spring_solving() {
    let springs = vec![Spring::new(10.0, 0.0, 0), ...];
    stretch_springs_to_width(&mut springs, 30.0);
    assert_eq!(springs[0].width, 15.0);
}

#[test]
fn test_simple_measure_spacing() {
    let score = test_fixtures::four_quarter_notes();
    let layout = layout_score(&score, &MStyle::new(), &test_font());
    let beats = layout.measure(0).beat_positions();
    assert_approx_eq!(beats[1] - beats[0], beats[2] - beats[1], 0.1);
}
```

---

## Appendix: Risk Mitigation

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Borrowing conflicts in LayoutContext | Medium | High | Use RefCell for state, extensive testing |
| Performance degradation vs C++ | Low | Medium | Profile early, use zero-cost abstractions |
| MuseScore algorithm incompleteness | Medium | Medium | Reference implementation available, iterative refinement |
| Complex lifetimes unmanageable | Low | High | Start simple (lead sheets), add complexity gradually |
| Scope creep (full notation is 10x larger) | High | High | Strict phase gating, lead sheets MUST work before expanding |

## Appendix: Future Expansion

### Phase 5: Full Notation Elements (Future)
- Beams (BeamLayout.cpp - 1,200 lines)
- Slurs/Ties (SlurTieLayout.cpp - 2,500 lines)
- Lyrics (LyricsLayout)
- Multi-voice (VoiceLayout)

### Phase 6: Orchestral Features (Future)
- Multiple staves per system
- Brackets/braces
- Part extraction
- Tablature

### Phase 7: Interactive Features (Future)
- Real-time layout updates during editing
- Custom spacing policies
- Style import/export (MusicXML, MuseScore .mss)
- MIDI playback integration

---

**Document End**

*This architecture document will be updated as implementation progresses through each phase.*
