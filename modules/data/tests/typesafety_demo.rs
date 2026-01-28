//! Type-safety runtime proofs — values that the type system constrains.
//!
//! Compile-time proofs (`compile_fail` doctests) live in `src/lib.rs` where
//! `rustdoc` can verify them. This file covers the runtime side: clamped
//! values, structural invariants, enum exhaustiveness, and format-driven
//! parameter mapping.
//!
//! Run with: `cargo test -p data --test typesafety_demo`

// ─────────────────────────────────────────────────────────────────────────────
// 1. NORMALIZED VALUES — out-of-range is impossible
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn normalized_clamps_at_construction() {
    use data::normalized::NormalizedF64;

    // Values are silently clamped — no way to hold 1.5 or -0.3
    let over = NormalizedF64::new(1.5);
    assert_eq!(over.get(), 1.0);

    let under = NormalizedF64::new(-0.3);
    assert_eq!(under.get(), 0.0);

    // Valid value passes through
    let ok = NormalizedF64::new(0.7);
    assert!((ok.get() - 0.7).abs() < f64::EPSILON);
}

#[test]
fn rating_cannot_exceed_five() {
    use data::normalized::Rating;

    let r = Rating::new(99);
    assert_eq!(r.get(), 5); // clamped
}

#[test]
fn midi_note_cannot_exceed_127() {
    use data::normalized::MidiNote;

    let n = MidiNote::new(200);
    assert_eq!(n.get(), 127); // clamped
}

// ─────────────────────────────────────────────────────────────────────────────
// 2. NON-EMPTY COLLECTIONS — emptiness is unrepresentable
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn non_empty_vec_rejects_empty() {
    use data::non_empty::NonEmptyVec;

    let result = NonEmptyVec::<i32>::from_vec(vec![]);
    assert!(result.is_none()); // can't construct empty

    // But a non-empty vec works:
    let v = NonEmptyVec::from_vec(vec![1, 2, 3]).unwrap();
    assert_eq!(v.len(), 3);
    assert_eq!(*v.first(), 1);
}

#[test]
fn section_always_has_a_layer() {
    use data::layer::Layer;
    use data::section::Section;

    // Section::new requires a primary layer — there is no way to create
    // a section with zero layers.
    let layer = Layer::new("Main", 0);
    let section = Section::new("Lead", layer);
    assert_eq!(section.layers.len(), 1);
}

// ─────────────────────────────────────────────────────────────────────────────
// 3. ENUM EXHAUSTIVENESS — ModuleType, BlockType, etc.
// ─────────────────────────────────────────────────────────────────────────────

/// Adding a new variant to ModuleType forces every match to be updated.
/// This match covers ALL variants — if one is added, this test fails to compile.
#[test]
fn module_type_match_is_exhaustive() {
    use data::module::ModuleType;

    let t = ModuleType::Drive;
    let name = match t {
        ModuleType::Rescue => "Rescue",
        ModuleType::Correction => "Correction",
        ModuleType::Tonal => "Tonal",
        ModuleType::VocalModulation => "Vocal Modulation",
        ModuleType::Sends => "Sends",
        ModuleType::Source => "Source",
        ModuleType::Eq => "EQ",
        ModuleType::Dynamics => "Dynamics",
        ModuleType::Special => "Special",
        ModuleType::Drive => "Drive",
        ModuleType::PreFx => "Pre FX",
        ModuleType::Volume => "Volume",
        ModuleType::Amp => "Amp",
        ModuleType::Cabinet => "Cabinet",
        ModuleType::PostEq => "Post EQ",
        ModuleType::Modulation => "Modulation",
        ModuleType::PostFx => "Post FX",
        ModuleType::Transient => "Transient",
    };
    assert_eq!(name, "Drive");
}

// ─────────────────────────────────────────────────────────────────────────────
// 4. MODULE OVERRIDE TYPE SAFETY — enum variants enforce valid override kinds
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn module_override_variants_are_distinct() {
    use data::id::ModulePresetId;
    use data::module::ModuleType;
    use data::module_preset::{ModuleOverride, ModuleOverrideType};

    let swap = ModuleOverride::swap_preset(ModuleType::Amp, ModulePresetId::new(), None);
    let disable = ModuleOverride::disable(ModuleType::Amp);

    // Pattern matching proves the enum is doing its job:
    assert!(matches!(swap.override_type, ModuleOverrideType::SwapPreset { .. }));
    assert!(matches!(disable.override_type, ModuleOverrideType::Disable));
}

// ─────────────────────────────────────────────────────────────────────────────
// 5. GLOBAL MODULE LOCK — the lock flag is part of the type
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn global_override_lock_semantics() {
    use data::id::ModulePresetId;
    use data::module::ModuleType;
    use data::module_preset::GlobalModuleOverride;
    use data::rig::{InstrumentType, Rig};

    let mut rig = Rig::new("Test", InstrumentType::Guitar);
    let preset_id = ModulePresetId::new();

    // Add a locked global override for Amp
    rig.add_global_override(GlobalModuleOverride::locked(ModuleType::Amp, preset_id));

    // Amp is locked, Drive is not
    assert!(rig.is_module_locked(&ModuleType::Amp));
    assert!(!rig.is_module_locked(&ModuleType::Drive));

    // Unlock it
    rig.global_module_overrides[0].unlock();
    assert!(!rig.is_module_locked(&ModuleType::Amp));
}

// ─────────────────────────────────────────────────────────────────────────────
// 6. PARAMETER FORMAT — format-specific mapping prevents misinterpretation
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn param_format_roundtrip_is_format_specific() {
    use data::parameter::ParamFormat;

    // A Percent format normalizes 0-100 -> 0.0-1.0
    let pct = ParamFormat::Percent;
    let norm = pct.normalize(75.0);
    assert!((norm - 0.75).abs() < 0.001);
    assert!((pct.denormalize(norm) - 75.0).abs() < 0.001);

    // A Frequency format uses skewed scaling — the same normalized value
    // maps to a completely different display value
    let freq = ParamFormat::Frequency {
        min: 20.0,
        max: 20_000.0,
        skew_factor: 1.0,
    };
    let freq_norm = freq.normalize(1_000.0);
    let freq_back = freq.denormalize(freq_norm);
    assert!((freq_back - 1_000.0).abs() < 1.0);

    // The enum variant you choose determines the mapping curve. Using
    // Percent normalization on a frequency value gives wrong results,
    // so the format type drives correctness.
    let wrong = pct.denormalize(freq_norm);
    assert!((wrong - 1_000.0).abs() > 100.0); // clearly wrong if misused
}

// ─────────────────────────────────────────────────────────────────────────────
// 7. CATEGORY FALLBACK — structural guarantee via enum nesting
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn category_fallback_chain_is_type_driven() {
    use data::category::{BaseTone, Genre, PresetCategory};

    let specific = PresetCategory::Genre {
        base_tone: BaseTone::Lead,
        genre: Genre::Blues,
    };

    // Fallback chain: Genre(Lead,Blues) -> Generic(Lead) -> None
    let fallback = specific.fallback().unwrap();
    assert!(matches!(
        fallback,
        PresetCategory::Generic {
            base_tone: BaseTone::Lead
        }
    ));

    let root = fallback.fallback();
    assert!(root.is_none()); // Generic is the root — no further fallback
}

// ─────────────────────────────────────────────────────────────────────────────
// 8. TYPESTATE SELECTION — runtime proof of the resolve/unresolve cycle
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn active_preset_resolve_unresolve_cycle() {
    use data::category::{BaseTone, PresetCategory};
    use data::preset::{Preset, Snapshot};
    use data::selection::ActivePreset;

    let mut preset = Preset::new(
        "Test",
        PresetCategory::Generic {
            base_tone: BaseTone::Clean,
        },
    );
    let snap = Snapshot::new("Verse");
    let snap_id = snap.id;
    preset.add_snapshot(snap);

    // Unresolved -> Resolved
    let unresolved = ActivePreset::new(preset);
    assert_eq!(unresolved.available_snapshots().len(), 1);
    // unresolved.snapshot() would be a compile error (tested in lib.rs doctests)

    let resolved = unresolved.resolve(snap_id).unwrap();
    assert_eq!(resolved.snapshot().name, "Verse");
    assert_eq!(resolved.snapshot_id(), snap_id);

    // Resolved -> Unresolved (drops snapshot access)
    let back = resolved.unresolve();
    assert_eq!(back.available_snapshots().len(), 1);
    // back.snapshot() would again be a compile error
}

#[test]
fn active_preset_resolve_invalid_id_returns_none() {
    use data::category::{BaseTone, PresetCategory};
    use data::id::SnapshotId;
    use data::preset::Preset;
    use data::selection::ActivePreset;

    let preset = Preset::new(
        "Empty",
        PresetCategory::Generic {
            base_tone: BaseTone::Clean,
        },
    );

    let sel = ActivePreset::new(preset);
    // No snapshots exist — resolve returns None, not a panic
    assert!(sel.resolve(SnapshotId::new()).is_none());
}

// ─────────────────────────────────────────────────────────────────────────────
// 9. BUILDER CORRECT USAGE — compiles and produces correct output
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn builder_typestate_happy_path() {
    use data::category::{BaseTone, PresetCategory};
    use data::normalized::Rating;
    use data::preset::builder::PresetBuilder;

    let preset = PresetBuilder::new()
        .name("Blues Clean")
        .category(PresetCategory::Generic {
            base_tone: BaseTone::Clean,
        })
        .rating(Rating::new(4))
        .notes("Warm and articulate")
        .build();

    assert_eq!(preset.name, "Blues Clean");
    assert_eq!(preset.rating, Rating::new(4));
    assert_eq!(preset.notes.as_deref(), Some("Warm and articulate"));
}
