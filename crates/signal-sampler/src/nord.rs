//! The full **Nord Stage 4** signal routing, built as a placeholder composition
//! tree ([`crate::rig_node`]). Every block is a placeholder (no DSP yet) — this
//! locks the *routing* per `docs/nord-stage-4-signal-routing.md`; the `Native`
//! DSP for each block type gets implemented one at a time afterwards.
//!
//! Shape:
//! ```text
//! Preset "Nord Stage"
//! ├─ Engines (parallel)
//! │  ├─ Engine Organ   → Voices(parallel: A,B) → shared Organ FX
//! │  ├─ Engine Keys    → Voices(parallel: A,B)  each: source → FX
//! │  └─ Engine Synth   → Voices(parallel: A,B,C) each: osc→filter→amp → FX
//! └─ Global → Rotary (one shared, last)
//! ```
//! Per-layer FX = the 6 stages: Mod1 · Mod2 · Delay · Amp/EQ · Comp · Reverb
//! (Comp is a bare block; the rest are modules). Synth layers carry the
//! modulators (3 envelopes, LFO, vibrato, arp).

use signal_proto::block::BlockType;

use crate::rig_node::{Container, RigNode};

const ROTARY: &str = "Rotary";

// ── Per-layer FX chain (the 6 stages) ────────────────────────────────────────

/// Mod 1 — modulation family (one mode active): A-Pan / Trem / RM / A-Wah / Wah
/// / Pump. Placeholder = a Tremolo block.
fn mod1() -> Container {
    Container::module("Mod 1").block(BlockType::Trem, "Tremolo")
}

/// Mod 2 — modulation family: Phaser / Flanger / Vibe / Chorus / Ensemble / Spin.
fn mod2() -> Container {
    Container::module("Mod 2").block(BlockType::Chorus, "Chorus")
}

/// Delay — a multi-block module: the delay + a feedback-FX sub-module + a
/// feedback filter (nested modules).
fn delay() -> Container {
    Container::module("Delay")
        .block(BlockType::Delay, "Delay")
        .add(Container::module("Feedback FX").block(BlockType::Chorus, "FB Mod"))
        .block(BlockType::Filter, "FB Filter")
}

/// Amp Sim / EQ — amp model + 3-band EQ + resonant LP24/HP24 filter (+ Drive).
/// Carries the `To Rotary` send (any layer can route to the global Rotary).
fn amp_eq() -> Container {
    Container::module("Amp/EQ")
        .block(BlockType::Amp, "Amp Model")
        .block(BlockType::Eq, "3-Band EQ")
        .block(BlockType::Filter, "LP24/HP24")
        .send(ROTARY, "To Rotary")
}

/// Reverb — swappable algorithm (Spring..Cath) + variation/chorale + bright/dark.
fn reverb() -> Container {
    Container::module("Reverb").block(BlockType::Reverb, "Reverb")
}

/// The ordered per-layer FX tail: Mod1 → Mod2 → Delay → Amp/EQ → Comp(block) →
/// Reverb. (Rotary is reached by the Amp/EQ send, not part of the chain.)
fn fx_chain() -> Vec<RigNode> {
    vec![
        mod1().into(),
        mod2().into(),
        delay().into(),
        amp_eq().into(),
        RigNode::Block(crate::rig::RigBlock::of_type(BlockType::Compressor).named("Comp")),
        reverb().into(),
    ]
}

// ── Engines ──────────────────────────────────────────────────────────────────

/// Organ — 2 layers (Tonewheel source each) that **share one FX chain** living
/// at the engine level (the flexible-tree case). Routes to Rotary by default.
fn organ_engine() -> Container {
    Container::engine("Organ")
        // engine-level menu (B3 system params)
        .param("tonewheel_mode", "Vintage 1")
        .param("click_level", "Normal")
        .param("trigger_point", "High")
        .add(
            Container::parallel("Organ Voices")
                .add(organ_layer("Organ A"))
                .add(organ_layer("Organ B")),
        )
        .add(Container::module("Organ FX").extend(fx_chain()))
}

fn organ_layer(name: &str) -> Container {
    Container::layer(name)
        .param("model", "B3") // B3 | B3Bass | Vox | Farfisa | Pipe1 | Pipe2
        .param("level", "0 dB")
        .param("octave", "0")
        .param("vc", "Off") // V1..V3 | C1..C3 | Off
        .param("percussion", "Off")
        .add(Container::module("Organ Source").block(BlockType::Tonewheel, name))
}

/// Keys (piano) — 2 layers, each a self-contained Sampler source → its own FX.
fn keys_engine() -> Container {
    Container::engine("Keys").add(
        Container::parallel("Keys Voices")
            .add(keys_layer("Keys A"))
            .add(keys_layer("Keys B")),
    )
}

fn keys_layer(name: &str) -> Container {
    Container::layer(name)
        .param("level", "0 dB")
        .param("octave", "0")
        .add(Container::module("Piano Source").block(BlockType::Sampler, "Piano"))
        .extend(fx_chain())
}

/// Synth — 3 layers, each the full voice (Osc → Filter → Amp) + FX, with the
/// control-rate modulators attached.
fn synth_engine() -> Container {
    Container::engine("Synth").add(
        Container::parallel("Synth Voices")
            .add(synth_layer("Synth A"))
            .add(synth_layer("Synth B"))
            .add(synth_layer("Synth C")),
    )
}

fn synth_layer(name: &str) -> Container {
    Container::layer(name)
        // per-layer settings (not blocks)
        .param("level", "0 dB")
        .param("octave", "0")
        .param("voice_mode", "Poly") // Poly | Mono | Legato
        .param("unison", "Off") // Off | 1 | 2 | 3
        .param("glide", "0")
        // voice: oscillator → filter → amp
        .add(
            Container::module("Osc")
                .block(BlockType::Oscillator, "Oscillator")
                .block(BlockType::Unison, "Unison"),
        )
        .add(Container::module("Filter").block(BlockType::Filter, "Filter"))
        .add(Container::module("Amp").block(BlockType::Amp, "Amp"))
        // per-layer FX
        .extend(fx_chain())
        // control-rate modulators (routing axis — drive params, not audio)
        .modulator(BlockType::Envelope, "Osc Env")
        .modulator(BlockType::Envelope, "Filter Env")
        .modulator(BlockType::Envelope, "Amp Env")
        .modulator(BlockType::Lfo, "LFO")
        .modulator(BlockType::Lfo, "Vibrato")
        .modulator(BlockType::Arpeggiator, "Arp")
}

// ── The Program ──────────────────────────────────────────────────────────────

/// The complete Nord Stage 4 program as a placeholder routing tree. Inspect with
/// [`Container::dump`].
pub fn nord_stage_preset() -> Container {
    Container::preset("Nord Stage")
        .add(
            Container::parallel("Engines")
                .add(organ_engine())
                .add(keys_engine())
                .add(synth_engine()),
        )
        // Global tail. The Global-mode Delay/Comp/Reverb instances live here when
        // a layer promotes that effect to Global (Shift+On); the single shared
        // Rotary is always last. All disabled by default (per-layer FX is the norm).
        .add(
            Container::module("Global")
                .param("delay_scope", "PerLayer") // PerLayer | Global
                .param("comp_scope", "PerLayer")
                .param("reverb_scope", "PerLayer")
                .add(
                    Container::module("Global Delay")
                        .param("enabled", "false")
                        .block(BlockType::Delay, "Global Delay"),
                )
                .add(
                    Container::module("Global Comp")
                        .param("enabled", "false")
                        .block(BlockType::Compressor, "Global Comp"),
                )
                .add(
                    Container::module("Global Reverb")
                        .param("enabled", "false")
                        .block(BlockType::Reverb, "Global Reverb"),
                )
                .block(BlockType::Rotary, ROTARY),
        )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rig_node::Role;

    #[test]
    fn nord_stage_has_the_full_layer_topology() {
        let p = nord_stage_preset();
        // 3 engines, 7 sound layers.
        assert_eq!(p.of_role(Role::Engine).len(), 3);
        assert_eq!(p.of_role(Role::Layer).len(), 7);
        assert!(p.find("Organ").is_some());
        assert!(p.find("Keys").is_some());
        assert!(p.find("Synth").is_some());
        assert!(p.find("Synth C").is_some());
    }

    #[test]
    fn sources_and_global_rotary_present_as_placeholders() {
        let p = nord_stage_preset();
        let types: Vec<&str> = p.blocks().iter().map(|b| b.block_type_tag()).collect();
        assert!(types.contains(&"tonewheel"), "organ source");
        assert!(types.contains(&"sampler"), "piano source");
        assert!(types.contains(&"oscillator"), "synth source");
        assert!(types.contains(&"rotary"), "global rotary");
        // Nothing is implemented yet — every block is a placeholder.
        assert!(p.blocks().iter().all(|b| !b.has_backend()));
    }

    #[test]
    fn organ_fx_is_shared_at_engine_level() {
        let p = nord_stage_preset();
        let organ = p.find("Organ").unwrap();
        // The Organ engine holds its parallel Voices AND the shared FX module.
        assert!(organ.find("Organ FX").is_some());
        assert!(organ.find("Organ Voices").is_some());
        // Keys/Synth engines have NO engine-level FX module (FX is per-layer).
        assert!(p.find("Keys").unwrap().find("Organ FX").is_none());
    }

    #[test]
    fn synth_layers_carry_modulators_and_to_rotary_sends() {
        let p = nord_stage_preset();
        let synth = p.find("Synth").unwrap();
        // 3 envelopes + 2 LFOs + 1 arp per synth layer × 3 layers = 18.
        assert_eq!(synth.modulators_recursive().len(), 18);
        // Every layer's Amp/EQ has a To-Rotary send; many across the tree.
        let to_rotary = p
            .sends_recursive()
            .into_iter()
            .filter(|(_, s)| s.target == "Rotary")
            .count();
        assert!(to_rotary >= 6, "each FX chain routes To Rotary, got {to_rotary}");
    }

    #[test]
    fn dump_renders_the_routing() {
        let p = nord_stage_preset();
        let dump = p.dump();
        assert!(dump.contains("Preset \"Nord Stage\""));
        assert!(dump.contains("Engine \"Organ\""));
        assert!(dump.contains("Block tonewheel \"Organ A\" (placeholder)"));
        assert!(dump.contains("To Rotary→Rotary"));
        // Uncomment to eyeball the full routing:
        // println!("{dump}");
    }
}
