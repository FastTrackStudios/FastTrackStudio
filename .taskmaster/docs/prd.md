<context>
# Overview

The keyflow engraver currently uses an estimation-then-fix approach for chart layout that causes several problems: chords are measured multiple times, estimates don't match reality, and post-hoc collision fixes break barline positioning. This PRD describes a refactor to a proper multi-pass layout system (Measure → Layout → Paint) similar to TeX and web browser layout engines.

**Problem Statement:**
- Same chord symbols measured 3+ times during layout
- Estimation functions (`estimate_measure_content_weight()`, `estimate_chord_collision_penalty()`) produce inaccurate results
- Post-render collision fixes don't move barlines, causing visual inconsistencies
- Complex interdependencies between estimation functions make the code hard to maintain

**Solution:**
Replace estimation with a deterministic three-pass pipeline:
1. **MEASURE** - Get actual sizes of all elements (with caching)
2. **LAYOUT** - Compute positions using real sizes
3. **PAINT** - Render with final positions

**Target Users:**
- Developers working on the keyflow engraver
- Musicians viewing lead sheets rendered by FastTrackStudio

**Value:**
- Correct layout without post-hoc fixes
- Better performance through caching (each chord measured once)
- Simpler, more maintainable code (~200 lines of estimation code removed)
- Barlines that move correctly when measures need more space

# Core Features

## 1. Measurement Cache
**What it does:** Caches element measurements keyed by (symbol, font_size) tuples so identical chords are only measured once.

**Why it's important:** Current code measures the same chord multiple times (weight estimation, minimum width calculation, collision checking). Caching eliminates redundant work.

**How it works:**
- HashMap stores `(String, OrderedFloat<f64>) → f64` for chord widths
- HashMap stores `HarmonyKey → HarmonyLayoutData` for full harmony layouts
- Cache is session-scoped (created fresh for each `layout_chart()` call, then dropped)

## 2. Pre-measurement Pass
**What it does:** Before any layout calculations, measures all elements in the chart to get exact dimensions.

**Why it's important:** Provides real measurements instead of estimates. The layout pass can then make correct decisions about spacing.

**How it works:**
- `measure_chart()` iterates all sections and measures
- For each measure, calculates actual `chord_widths`, `min_width`, and `segment_count`
- Returns `ChartMeasurements` struct containing all measurement data

## 3. Layout Pass with Real Data
**What it does:** Computes element positions using the actual measurements from Pass 1.

**Why it's important:** Eliminates the need for estimation functions. Positions are correct the first time.

**How it works:**
- Uses `measurements.measures[i].min_width` instead of `estimate_measure_content_weight()`
- Distribution algorithm respects real minimum widths
- No collision-fixing needed because spacing is correct from the start

## 4. Simplified Paint Pass
**What it does:** Renders elements at their computed positions without any adjustment.

**Why it's important:** Current renderer has complex collision-fixing code that can be removed.

**How it works:**
- Takes layout data with final positions
- Renders directly without post-processing
- Uses cached harmony layouts from Pass 1 for efficiency

# User Experience

**User Personas:**
- Keyflow developer: Wants clean, maintainable layout code
- Musician: Wants lead sheets that look professional with correct spacing

**Key User Flows:**
1. Parse chart text → Generate chart model → Layout chart → Render to screen
2. Edit chart text → Re-layout affected sections → Update display

**UI/UX Considerations:**
- Visual output should match or improve on current quality
- No user-facing API changes (internal refactor)
- Performance should be equal or better
</context>
<PRD>
# Technical Architecture

## System Components

### New Files
1. **`packages/keyflow/src/engraver/layout/chart/measure_pass.rs`**
   - `MeasurementCache` struct with chord_widths and harmony_layouts HashMaps
   - `measure_chord()` - measure single chord, return cached value if available
   - `get_harmony_layout()` - get full harmony layout, render if not cached
   - `measure_chart()` - pre-measure all elements in chart
   - `measure_measure()` - measure single measure contents

2. **`packages/keyflow/src/engraver/layout/chart/layout_pass.rs`** (optional, may inline)
   - Layout computation logic extracted from current code
   - Uses `ChartMeasurements` instead of estimation functions

### Modified Files
1. **`packages/keyflow/src/engraver/layout/chart/mod.rs`**
   - `ChartLayoutEngine::layout_chart()` orchestrates three passes
   - Remove calls to estimation functions
   - Add measurement cache creation and passing

2. **`packages/keyflow/src/engraver/layout/chart/chord_renderer.rs`**
   - Remove collision-fixing code
   - Simplify `render_chord_symbols()` to just render at given positions

3. **`packages/keyflow/src/engraver/layout/chart/measure_layout.rs`**
   - Use real minimums from measurements instead of computed estimates

## Data Models

```rust
/// Cache for measured element sizes (session-scoped)
pub struct MeasurementCache {
    chord_widths: HashMap<(String, OrderedFloat<f64>), f64>,
    harmony_layouts: HashMap<HarmonyKey, HarmonyLayoutData>,
}

/// Measurement data for entire chart
pub struct ChartMeasurements {
    pub measures: Vec<MeasureMeasurements>,
}

/// Measurement data for a single measure
pub struct MeasureMeasurements {
    pub chord_widths: Vec<f64>,
    pub min_width: f64,
    pub segment_count: usize,
}

/// Key for harmony layout cache
pub struct HarmonyKey {
    pub symbol: String,
    pub font_size: OrderedFloat<f64>,
    pub style_hash: u64,
}
```

## APIs and Integrations
- Internal refactor only, no public API changes
- `ChartLayoutEngine::layout_chart()` signature unchanged
- All changes behind the `engraver` feature flag

## Infrastructure Requirements
- No new dependencies needed
- `ordered-float` crate already available in workspace

# Development Roadmap

## Phase 1: Create Measurement Infrastructure
**Scope:** Create the measurement cache and measurement pass without changing layout behavior

- [x] Create `measure_pass.rs` with `MeasurementCache` struct
- [ ] Implement `measure_chord()` with caching logic
- [ ] Implement `HarmonyKey` and `get_harmony_layout()`
- [ ] Implement `MeasureMeasurements` and `measure_measure()`
- [ ] Implement `ChartMeasurements` and `measure_chart()`
- [ ] Add unit tests for cache hit/miss behavior
- [ ] Verify: `cargo check -p keyflow --features engraver`

## Phase 2: Integrate Measurement Pass into Layout
**Scope:** Wire up the measurement pass but keep using estimation functions (parallel operation)

- [ ] Modify `ChartLayoutEngine::layout_chart()` to create cache and call `measure_chart()`
- [ ] Pass `ChartMeasurements` through to layout functions
- [ ] Add logging to compare measured values vs estimated values
- [ ] Verify measurements match expectations
- [ ] Verify: `cargo test -p keyflow --features engraver`

## Phase 3: Replace Estimation with Real Measurements
**Scope:** Switch layout to use real measurements instead of estimates

- [ ] Replace `estimate_measure_content_weight()` usage with `measurements.measures[i].segment_count`
- [ ] Replace `compute_minimum_measure_width()` usage with `measurements.measures[i].min_width`
- [ ] Replace `compute_chord_min_widths()` usage with `measurements.measures[i].chord_widths`
- [ ] Update `distribute_measure_widths_with_mins()` to use real minimums
- [ ] Verify: Run `debug-keyflow` and compare visual output

## Phase 4: Remove Collision Fixing
**Scope:** Simplify chord renderer since layout is now correct

- [ ] Remove `detect_collisions()` call from chord renderer
- [ ] Remove `apply_collision_fixes()` and related functions
- [ ] Simplify `render_chord_symbols()` to render at given positions
- [ ] Verify: Visual regression test with existing charts

## Phase 5: Cleanup and Optimization
**Scope:** Remove dead code and optimize

- [ ] Delete or deprecate `estimate_measure_content_weight()`
- [ ] Delete or deprecate `estimate_chord_collision_penalty()`
- [ ] Delete or deprecate `compute_chord_font_scale()`
- [ ] Add documentation for multi-pass architecture
- [ ] Performance benchmark: measure cache hit rate
- [ ] Verify: `cargo clippy -p keyflow --features engraver`

# Logical Dependency Chain

```
Phase 1: Measurement Infrastructure
    ↓ (provides MeasurementCache, measure_chart())
Phase 2: Integration
    ↓ (measurements available alongside estimates)
Phase 3: Replace Estimation
    ↓ (layout uses real measurements)
Phase 4: Remove Collision Fixing
    ↓ (renderer simplified)
Phase 5: Cleanup
```

**Foundation (Phase 1):** Must be built first - all other phases depend on having the measurement cache and pre-measurement pass working.

**Quick Win (Phase 2):** Running both systems in parallel lets us validate measurements match reality before committing to the switch.

**Core Change (Phase 3):** The main refactor - swapping estimates for real data. This is where layout correctness improves.

**Simplification (Phase 4):** Only possible after Phase 3 - collision fixing becomes unnecessary when layout is correct.

**Polish (Phase 5):** Cleanup old code only after everything works.

# Risks and Mitigations

## Technical Challenges

**Risk:** Measurement cache key might not capture all factors affecting layout
**Mitigation:** `HarmonyKey` includes symbol, font_size, and style_hash. Add more factors if edge cases discovered.

**Risk:** Different code paths might measure with different parameters
**Mitigation:** Centralize all measurements through `MeasurementCache` methods.

**Risk:** Cache memory usage for large charts
**Mitigation:** Cache is session-scoped and dropped after each layout. Unique chords in a chart are typically < 100.

## MVP Definition

**Minimum Viable:** Phases 1-3 complete
- Measurement cache working
- Layout using real measurements
- Visual output correct

**Can defer:**
- Phase 4 (collision removal) - code still works, just has dead paths
- Phase 5 (cleanup) - can be done incrementally

## Resource Constraints

**Constraint:** Must maintain backward compatibility
**Approach:** Internal refactor only, public API unchanged

**Constraint:** Feature-gated code
**Approach:** All changes under `engraver` feature flag

# Appendix

## Current Estimation Functions to Replace

| Function | Current Location | Replacement |
|----------|------------------|-------------|
| `estimate_measure_content_weight()` | mod.rs | `measurements.segment_count` |
| `estimate_chord_collision_penalty()` | mod.rs | Not needed (use real widths) |
| `compute_chord_font_scale()` | mod.rs | Not needed (layout ensures fit) |
| `compute_chord_min_widths()` | chord_renderer.rs | `measurements.chord_widths` |
| `compute_minimum_measure_width()` | measure_layout.rs | `measurements.min_width` |

## Performance Expectations

**Current:**
- `horizontal_advance()` called ~3x per chord
- Same chord measured repeatedly across measures
- Layout then re-measured during render

**After Refactor:**
- `horizontal_advance()` called 1x per unique (chord, font_size)
- Cache hit for repeated chords (common in music)
- No re-measurement during render

## Verification Commands

```bash
# Check compilation
cargo check -p keyflow --features engraver

# Run tests
cargo test -p keyflow --features engraver

# Visual test
cargo run --bin debug-keyflow --features engraver

# Lint
cargo clippy -p keyflow --features engraver
```

## Reference

- Original plan: `~/.claude/plans/gentle-snacking-nygaard.md`
- Layout architecture doc: `docs/LAYOUT_ENGINE_ARCHITECTURE.md`
- Chart rendering doc: `docs/CHART_RENDERING_ARCHITECTURE.md`
</PRD>
