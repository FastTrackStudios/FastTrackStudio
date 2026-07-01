//! Tree-render runtime for the composition tree ([`crate::rig_node`]).
//!
//! Compiles a [`Container`] into a [`RenderNode`] tree of boxed processors, then
//! renders audio by walking it: a **Serial** container chains its children
//! (output of N → input of N+1), a **Parallel** container sums them. A leaf
//! **Block** becomes its audio backend — a NAM/IR/plugin, or a built-in `Native`
//! DSP (currently just the [`NativeOscillator`]) — or, when the block is still a
//! placeholder, a **pass-through** (audio flows through untouched). So the same
//! routing renders correctly today (placeholders = silence/thru) and gains sound
//! as each block type's DSP is implemented.
//!
//! Headless / offline: `process` allocates scratch, so it's for tests and bring-up
//! rather than the realtime callback (which will pre-allocate). MIDI events are
//! delivered to every node; sources consume note on/off, effects ignore them.

use signal_plugin_host::{PluginEvents, PluginInstance, PluginMidiEvent};
use signal_proto::block::BlockType;

use crate::native::{NativeAmp, NativeFilter};
use crate::native_osc::NativeOscillator;
use crate::rig::{build_block, RigBlock};
use crate::rig_node::{Combine, Container, RigNode, Zone};

/// Build the audio backend for one block at `sample_rate`, or `None` when the
/// block is a placeholder or an unimplemented `Native` type (→ pass-through).
pub fn build_node_backend(block: &RigBlock, sample_rate: u32) -> Option<Box<dyn PluginInstance>> {
    if !block.has_backend() {
        return None;
    }
    if block.is_native() {
        // Built-in DSP dispatch by block type. Grows one type at a time.
        match block.block_type {
            BlockType::Oscillator => Some(Box::new(NativeOscillator::new(sample_rate))),
            BlockType::Filter => Some(Box::new(NativeFilter::new(sample_rate))),
            BlockType::Amp => Some(Box::new(NativeAmp::new(sample_rate))),
            _ => None,
        }
    } else {
        // NAM / IR / hosted plugin via the shared loader.
        build_block(block, sample_rate)
            .map_err(|e| tracing::warn!(error = %e, "node_render: backend build failed"))
            .ok()
            .map(|(boxed, _, _, _)| boxed)
    }
}

/// A compiled, renderable node mirroring the container tree.
pub enum RenderNode {
    /// A leaf processor, or `None` for a placeholder (pass-through).
    Leaf(Option<Box<dyn PluginInstance>>),
    /// Children chained in order.
    Serial(Vec<RenderNode>),
    /// Children summed.
    Parallel(Vec<RenderNode>),
    /// A keyboard-routed subtree: incoming MIDI is filtered + velocity-scaled by
    /// the [`Zone`] (key split + velocity crossfade) before reaching `inner`.
    /// This is the central-MIDI-input router, expressed in the render tree.
    Zoned {
        zone: Zone,
        inner: Box<RenderNode>,
    },
}

impl RenderNode {
    /// Compile a container subtree into a render tree at `sample_rate`. A
    /// container with a non-full [`Zone`] is wrapped in [`RenderNode::Zoned`] so
    /// only its in-window notes reach it.
    pub fn compile(container: &Container, sample_rate: u32) -> RenderNode {
        let kids = container
            .children
            .iter()
            .map(|n| Self::compile_node(n, sample_rate))
            .collect();
        let base = match container.combine {
            Combine::Serial => RenderNode::Serial(kids),
            Combine::Parallel => RenderNode::Parallel(kids),
        };
        if container.zone.is_full() {
            base
        } else {
            RenderNode::Zoned {
                zone: container.zone,
                inner: Box::new(base),
            }
        }
    }

    fn compile_node(node: &RigNode, sample_rate: u32) -> RenderNode {
        match node {
            RigNode::Block { block: b } => RenderNode::Leaf(build_node_backend(b, sample_rate)),
            RigNode::Container { container: c } => Self::compile(c, sample_rate),
        }
    }

    /// Prepare every leaf processor for `sample_rate` / `block_size`.
    pub fn prepare(&mut self, sample_rate: f64, block_size: u32) {
        match self {
            RenderNode::Leaf(Some(inst)) => {
                let _ = inst.prepare(sample_rate, block_size);
            }
            RenderNode::Leaf(None) => {}
            RenderNode::Serial(v) | RenderNode::Parallel(v) => {
                v.iter_mut().for_each(|n| n.prepare(sample_rate, block_size));
            }
            RenderNode::Zoned { inner, .. } => inner.prepare(sample_rate, block_size),
        }
    }

    /// Count the leaf processors that actually have a backend (for tests/metering).
    pub fn live_leaves(&self) -> usize {
        match self {
            RenderNode::Leaf(opt) => opt.is_some() as usize,
            RenderNode::Serial(v) | RenderNode::Parallel(v) => {
                v.iter().map(|n| n.live_leaves()).sum()
            }
            RenderNode::Zoned { inner, .. } => inner.live_leaves(),
        }
    }

    /// Render one block. `out` is overwritten with this node's output.
    pub fn process(
        &mut self,
        in_l: &[f32],
        in_r: &[f32],
        out_l: &mut [f32],
        out_r: &mut [f32],
        events: &PluginEvents<'_>,
    ) {
        let frames = out_l.len().min(out_r.len());
        match self {
            RenderNode::Leaf(Some(inst)) => {
                let _ = inst.process_block(in_l, in_r, out_l, out_r, events);
            }
            RenderNode::Leaf(None) => copy_in(in_l, in_r, out_l, out_r, frames),
            RenderNode::Serial(nodes) => {
                if nodes.is_empty() {
                    return copy_in(in_l, in_r, out_l, out_r, frames);
                }
                // Ping-pong the signal through each child.
                let mut cur_l = in_l[..frames].to_vec();
                let mut cur_r = in_r[..frames].to_vec();
                let mut nxt_l = vec![0.0f32; frames];
                let mut nxt_r = vec![0.0f32; frames];
                for node in nodes.iter_mut() {
                    nxt_l.iter_mut().for_each(|x| *x = 0.0);
                    nxt_r.iter_mut().for_each(|x| *x = 0.0);
                    node.process(&cur_l, &cur_r, &mut nxt_l, &mut nxt_r, events);
                    std::mem::swap(&mut cur_l, &mut nxt_l);
                    std::mem::swap(&mut cur_r, &mut nxt_r);
                }
                out_l[..frames].copy_from_slice(&cur_l[..frames]);
                out_r[..frames].copy_from_slice(&cur_r[..frames]);
            }
            RenderNode::Parallel(nodes) => {
                out_l[..frames].iter_mut().for_each(|x| *x = 0.0);
                out_r[..frames].iter_mut().for_each(|x| *x = 0.0);
                let mut tl = vec![0.0f32; frames];
                let mut tr = vec![0.0f32; frames];
                for node in nodes.iter_mut() {
                    tl.iter_mut().for_each(|x| *x = 0.0);
                    tr.iter_mut().for_each(|x| *x = 0.0);
                    node.process(in_l, in_r, &mut tl, &mut tr, events);
                    for f in 0..frames {
                        out_l[f] += tl[f];
                        out_r[f] += tr[f];
                    }
                }
            }
            RenderNode::Zoned { zone, inner } => {
                // Central-MIDI-input routing: keep only notes in this zone's
                // window, scaling each NoteOn's velocity by the crossfade gain.
                // Note-offs and CC pass through (so held notes always release).
                let filtered = filter_events_by_zone(*zone, events);
                let fe = PluginEvents {
                    params: events.params,
                    midi: &filtered,
                    note_expressions: events.note_expressions,
                };
                inner.process(in_l, in_r, out_l, out_r, &fe);
            }
        }
    }

    /// Convenience: render `frames` of output from silence + the given MIDI.
    pub fn render(&mut self, out_l: &mut [f32], out_r: &mut [f32], midi: &PluginEvents<'_>) {
        let frames = out_l.len().min(out_r.len());
        let silence = vec![0.0f32; frames];
        self.process(&silence, &silence, out_l, out_r, midi);
    }
}

/// Apply a [`Zone`] to a MIDI stream: drop NoteOns outside the window, scale
/// the rest by the crossfade gain (velocity × gain), and pass everything else
/// (NoteOff / CC / …) through unchanged so releases always land.
fn filter_events_by_zone(zone: Zone, events: &PluginEvents<'_>) -> Vec<PluginMidiEvent> {
    use daw::service::MidiMessage;
    let mut out = Vec::with_capacity(events.midi.len());
    for ev in events.midi {
        match ev.message {
            MidiMessage::NoteOn {
                channel,
                note,
                velocity,
            } => {
                let gain = zone.note_gain(note, velocity);
                if gain > 0.0 {
                    let scaled = ((velocity as f32 * gain).round() as u8).clamp(1, 127);
                    out.push(PluginMidiEvent {
                        offset: ev.offset,
                        message: MidiMessage::note_on(channel, note, scaled),
                    });
                }
            }
            _ => out.push(ev.clone()),
        }
    }
    out
}

fn copy_in(in_l: &[f32], in_r: &[f32], out_l: &mut [f32], out_r: &mut [f32], frames: usize) {
    let n = frames.min(in_l.len()).min(in_r.len());
    out_l[..n].copy_from_slice(&in_l[..n]);
    out_r[..n].copy_from_slice(&in_r[..n]);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rig_node::Container;
    use signal_plugin_host::PluginMidiEvent;

    fn note_on(note: u8, vel: u8) -> PluginMidiEvent {
        PluginMidiEvent {
            offset: 0,
            message: daw::service::MidiMessage::note_on(0, note, vel),
        }
    }

    fn rms(buf: &[f32]) -> f32 {
        (buf.iter().map(|s| s * s).sum::<f32>() / buf.len().max(1) as f32).sqrt()
    }

    #[test]
    fn oscillator_block_renders_through_a_serial_module() {
        // Module [serial] { Oscillator (native), Filter (native SVF) }
        let tree = Container::module("Osc")
            .block(BlockType::Oscillator, "Osc")
            .block(BlockType::Filter, "Filter");
        let mut rn = RenderNode::compile(&tree, 48_000);
        rn.prepare(48_000.0, 256);
        assert_eq!(rn.live_leaves(), 2, "oscillator + filter both have backends");

        let (mut l, mut r) = (vec![0.0; 256], vec![0.0; 256]);
        let midi = [note_on(69, 110)];
        let ev = PluginEvents {
            params: &[],
            midi: &midi,
            note_expressions: &[],
        };
        rn.render(&mut l, &mut r, &ev);
        assert!(rms(&l) > 1e-3, "audio flows osc → filter (default open) → out");
    }

    #[test]
    fn parallel_sums_two_oscillators() {
        let tree = Container::parallel("Voices")
            .add(Container::layer("A").block(BlockType::Oscillator, "Osc"))
            .add(Container::layer("B").block(BlockType::Oscillator, "Osc"));
        let mut rn = RenderNode::compile(&tree, 48_000);
        rn.prepare(48_000.0, 256);
        assert_eq!(rn.live_leaves(), 2);

        let (mut l, mut r) = (vec![0.0; 256], vec![0.0; 256]);
        let midi = [note_on(60, 100)];
        let ev = PluginEvents {
            params: &[],
            midi: &midi,
            note_expressions: &[],
        };
        rn.render(&mut l, &mut r, &ev);
        assert!(rms(&l) > 1e-3);
    }

    fn render_note(rn: &mut RenderNode, note: u8, vel: u8) -> f32 {
        let (mut l, mut r) = (vec![0.0; 256], vec![0.0; 256]);
        let midi = [note_on(note, vel)];
        let ev = PluginEvents {
            params: &[],
            midi: &midi,
            note_expressions: &[],
        };
        rn.render(&mut l, &mut r, &ev);
        rms(&l)
    }

    #[test]
    fn key_split_routes_notes_to_their_zone() {
        // Low osc keys 0-50, High osc keys 70-127 — with a silent gap 51..69.
        let split = Container::parallel("Split")
            .add(Container::layer("Low").keys(0, 50).block(BlockType::Oscillator, "Osc"))
            .add(Container::layer("High").keys(70, 127).block(BlockType::Oscillator, "Osc"));

        // A note in the low zone sounds.
        let mut rn = RenderNode::compile(&split, 48_000);
        rn.prepare(48_000.0, 256);
        assert!(render_note(&mut rn, 40, 100) > 1e-3, "low zone note sounds");

        // A note in the gap reaches no layer → silence.
        let mut rn = RenderNode::compile(&split, 48_000);
        rn.prepare(48_000.0, 256);
        assert!(render_note(&mut rn, 60, 100) < 1e-5, "gap note is silent");
    }

    #[test]
    fn velocity_window_gates_notes() {
        let loud = Container::layer("Loud")
            .velocity(64, 127)
            .block(BlockType::Oscillator, "Osc");

        let mut rn = RenderNode::compile(&loud, 48_000);
        rn.prepare(48_000.0, 256);
        assert!(render_note(&mut rn, 60, 110) > 1e-3, "hard hit sounds");

        let mut rn = RenderNode::compile(&loud, 48_000);
        rn.prepare(48_000.0, 256);
        assert!(render_note(&mut rn, 60, 30) < 1e-5, "soft hit is below the window");
    }

    /// A Sampler block (BlockImpl::Sample) plays a real library through the
    /// render tree — the keys/piano/drums/orchestral loading path.
    #[test]
    fn sampler_block_plays_a_library() {
        // Fixture: one 220 Hz sine zone covering the whole keyboard.
        let dir = std::env::temp_dir().join(format!(
            "signal-sampler-block-test-{}",
            std::process::id()
        ));
        std::fs::create_dir_all(&dir).unwrap();
        let wav = dir.join("note.wav");
        let spec = hound::WavSpec {
            channels: 1,
            sample_rate: 48_000,
            bits_per_sample: 32,
            sample_format: hound::SampleFormat::Float,
        };
        let mut w = hound::WavWriter::create(&wav, spec).unwrap();
        for i in 0..48_000 {
            let t = i as f32 / 48_000.0;
            w.write_sample((core::f32::consts::TAU * 220.0 * t).sin() * 0.8)
                .unwrap();
        }
        w.finalize().unwrap();
        let styx = "\
name TestZoneLib
zones (
    { file note.wav, key_min 0, key_max 127, root_key 60, vel_min 0, vel_max 127 }
)
";
        let spec_path = dir.join("lib.styx");
        std::fs::write(&spec_path, styx).unwrap();

        // Layer { Sampler(lib) } — samples_root defaults to the spec's dir.
        let tree = Container::layer("Keys")
            .sample_block("Piano", spec_path.to_string_lossy().to_string());
        let mut rn = RenderNode::compile(&tree, 48_000);
        rn.prepare(48_000.0, 512);
        assert_eq!(rn.live_leaves(), 1, "sampler block has a backend");

        let (mut l, mut r) = (vec![0.0; 512], vec![0.0; 512]);
        let midi = [note_on(60, 100)];
        // The cache fills on a background thread — keep (re)triggering the
        // note until the sample is decoded and audible.
        let mut heard = 0.0f32;
        for _ in 0..200 {
            let ev = PluginEvents {
                params: &[],
                midi: &midi,
                note_expressions: &[],
            };
            rn.render(&mut l, &mut r, &ev);
            heard = heard.max(rms(&l));
            if heard > 1e-3 {
                break;
            }
            std::thread::sleep(std::time::Duration::from_millis(5));
        }
        std::fs::remove_dir_all(&dir).ok();
        assert!(heard > 1e-3, "sampler block should be audible, rms={heard}");
    }

    /// Machine-local: the real Keyscape LA Custom C7 Grand extraction loads
    /// through a Sampler block and makes sound. Run explicitly:
    /// `cargo test -p signal-sampler --lib keyscape -- --ignored`
    #[test]
    #[ignore = "requires the local Keyscape extraction on AudioHaven"]
    fn keyscape_c7_grand_loads_and_sounds() {
        let spec =
            "/run/media/AudioHaven/Sampled/Keys/Keyscape/LA Custom C7 Grand/library.styx";
        if !std::path::Path::new(spec).exists() {
            eprintln!("skipping: {spec} not present");
            return;
        }
        let tree = Container::layer("Keys").sample_block("C7 Grand", spec);
        let mut rn = RenderNode::compile(&tree, 48_000);
        rn.prepare(48_000.0, 512);
        assert_eq!(rn.live_leaves(), 1, "keyscape sampler block builds");

        let (mut l, mut r) = (vec![0.0; 512], vec![0.0; 512]);
        let midi = [note_on(60, 100)];
        let mut heard = 0.0f32;
        for _ in 0..600 {
            let ev = PluginEvents {
                params: &[],
                midi: &midi,
                note_expressions: &[],
            };
            rn.render(&mut l, &mut r, &ev);
            heard = heard.max(rms(&l));
            if heard > 1e-3 {
                break;
            }
            std::thread::sleep(std::time::Duration::from_millis(10));
        }
        assert!(heard > 1e-3, "C7 grand should be audible, rms={heard}");
    }

    #[test]
    fn full_nord_preset_renders_synth_oscillators() {
        // The whole Nord routing: the 3 synth Oscillators plus every native
        // Filter/Amp are live DSP; the rest are placeholder pass-throughs. A
        // held note must reach the output through the entire tree (synth
        // voices → engines-sum → global thru).
        let preset = crate::nord::nord_stage_preset();
        let mut rn = RenderNode::compile(&preset, 48_000);
        rn.prepare(48_000.0, 512);
        assert_eq!(
            rn.live_leaves(),
            27,
            "3 oscillators + the tree's native Filter/Amp blocks are live"
        );

        let (mut l, mut r) = (vec![0.0; 512], vec![0.0; 512]);
        let midi = [note_on(64, 100)];
        let ev = PluginEvents {
            params: &[],
            midi: &midi,
            note_expressions: &[],
        };
        rn.render(&mut l, &mut r, &ev);
        assert!(
            rms(&l) > 1e-3,
            "the synth layers must be audible through the full Nord tree"
        );
    }
}
