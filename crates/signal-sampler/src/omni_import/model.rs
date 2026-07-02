//! The parsed **patch model** (`OmniPatch` / `OmniLayer`) and the AmberPart
//! element walk that fills it.

use super::xml::XmlNode;
use super::parse_xml;

// ── Patch model ──────────────────────────────────────────────────────────────

/// One layer extracted from a patch (a `VOICE` + its `MULTISAMPLE`).
#[derive(Debug, Clone, Default)]
pub struct OmniLayer {
    /// Soundsource name from `MS_IM_0 name=` (empty ⇒ synth mode / none).
    pub soundsource: String,
    /// Soundsource library from `MS_IM_0 library=`.
    pub ss_library: String,
    /// `FILTER NameStr=` display name (e.g. "LPF UVI 3").
    pub filter_name: String,
    /// `FILTER para=` ≠ 0 ⇒ the two filters run in parallel.
    pub filter_parallel: bool,
    /// `FILTER act=` ≠ 0 ⇒ the filter section is engaged.
    pub filter_active: bool,
    /// Normalized filter cutoff / resonance (`freq` / `res`).
    pub filter_freq: f32,
    pub filter_res: f32,
    /// `OSC level` (normalized).
    pub level: f32,
    /// Unison: voice count (1..8), detune 0..1, width 0..1, plus the
    /// octave / analog / drift mode amounts (0..1).
    pub unison_count: u32,
    pub unison_detune: f32,
    pub unison_width: f32,
    pub unison_octave: f32,
    pub unison_analog: f32,
    pub unison_drift: f32,
    /// FM modulator waveform morph 0..1 (`OSC fmwf`).
    pub fm_shape: f32,
    /// Amplitude AHDSR `(attack_s, decay_s, sustain, release_s)`.
    pub amp_env: Option<(f32, f32, f32, f32)>,
    /// Filter AHDSR `(attack_s, decay_s, sustain, release_s)`.
    pub filter_env: Option<(f32, f32, f32, f32)>,
    /// Filter-envelope → cutoff depth (signed; `FILTER envdpth`, inverted by
    /// `envdpthinv`).
    pub filter_env_depth: f32,
    /// Filter 2, when engaged (`act2`): `(freq, res)` normalized.
    pub filter2: Option<(f32, f32)>,
    /// FM depth 0..1 (`OSC fm`).
    pub fm_depth: f32,
    /// Ring/AM mix 0..1 (`OSC am`).
    pub ring_mix: f32,
    /// Active Harmonia voices: (level, interval semitones, pan −1..1, shape).
    pub harmonia: Vec<(f32, f32, f32, f32)>,
    /// Waveshaper when engaged: (drive, crush, reduce, mix).
    pub shaper: Option<(f32, f32, f32, f32)>,
    /// Dual Frequency Shifter when engaged: (hz_a, mix_a, hz_b, mix_b, parallel).
    pub dfs: Option<(f32, f32, f32, f32, bool)>,
    /// Layer FX rack: the four `EFFMODULE Type=` names ("No Effect" ⇒ empty).
    pub fx: Vec<String>,
}

/// One mod-matrix route (`sourceN` → `targetN`).
#[derive(Debug, Clone)]
pub struct OmniModRoute {
    pub source: String,
    pub target: String,
    pub depth: f32,
}

/// A parsed `.prt_omn` patch.
#[derive(Debug, Clone, Default)]
pub struct OmniPatch {
    pub name: String,
    pub library: String,
    /// Browser tags from `ENTRYDESCR ATTRIB_VALUE_DATA` (`key=value` pairs).
    pub tags: Vec<(String, String)>,
    pub layers: Vec<OmniLayer>,
    /// Common FX rack module names.
    pub common_fx: Vec<String>,
    /// Aux FX rack module names.
    pub aux_fx: Vec<String>,
    pub mod_routes: Vec<OmniModRoute>,
    /// Part LFOs from `LFO_SET`: `(rate 0..1, type 0..1, synced, retrigger)`.
    pub lfos: Vec<(f32, f32, bool, bool)>,
    pub arp_on: bool,
    /// Arp pattern from `ARPSEQ2`: `(on, velocity, gate 0..1)` per step.
    pub arp_steps: Vec<(bool, u8, f32)>,
    /// Step length in beats (from tick spacing vs `TICKSPERQUARTER`).
    pub arp_step_beats: f32,
}

fn rack_types(rack: &XmlNode) -> Vec<String> {
    rack.children_tagged("EFFMODULE")
        .map(|m| m.attr("Type").unwrap_or("").to_string())
        .collect()
}

/// Coarse filter classification from the factory preset name (`NameStr`) —
/// mode + pole count. The real algorithm enum (`type1`) is undecoded; the
/// names cover the dominant families ("Classic LPF 4-pole", "HPF Juicy
/// 12db", "Bandpass", "Notch", …). Defaults: LP 12 dB.
/// Classification including the engine character: the saturating families
/// (Juicy / Moogie / OB / Jupiter / FATBOY / Sauce / Beefy / Warm / Power /
/// French / Brit) map onto the ladder engine.
pub(crate) fn classify_filter_full(name: &str) -> (&'static str, u32, &'static str) {
    let (mode, poles) = classify_filter_inner(name);
    let k = name.to_ascii_lowercase();
    let saturating = [
        "juicy", "moogie", "fatboy", "ob ", "jupiter", "sauce", "beefy", "warm", "power",
        "french", "brit",
    ]
    .iter()
    .any(|f| k.contains(f));
    let character = if saturating && mode == "lowpass" {
        "ladder"
    } else {
        "clean"
    };
    (mode, poles, character)
}

fn classify_filter_inner(name: &str) -> (&'static str, u32) {
    let k = name.to_ascii_lowercase();
    let mode = if k.contains("hpf") || k.contains("hipass") || k.contains("high") {
        "highpass"
    } else if k.contains("bpf") || k.contains("bandpass") {
        "bandpass"
    } else if k.contains("notch") {
        "notch"
    } else {
        "lowpass"
    };
    // "<N>-pole" wins; else "<N>db" → N/6 poles.
    let mut poles = 2u32;
    for (pat, scale) in [("-pole", 1u32), ("db", 6u32)] {
        if let Some(pos) = k.find(pat) {
            let digits: String = k[..pos]
                .chars()
                .rev()
                .take_while(|c| c.is_ascii_digit())
                .collect::<Vec<_>>()
                .into_iter()
                .rev()
                .collect();
            if let Ok(n) = digits.parse::<u32>() {
                if n >= scale {
                    poles = (n / scale).clamp(1, 8);
                    break;
                }
            }
        }
    }
    (mode, poles)
}

/// Normalized envelope time → seconds. CALIBRATE: the exact Omnisphere
/// mapping is unverified; a cubic curve into a 10 s range is perceptually
/// plausible (0.5 → 1.25 s) until the A/B harness measures it.
pub(crate) fn env_seconds(v: f32) -> f32 {
    v.clamp(0.0, 1.0).powi(3) * 10.0
}

/// Parse an `AENVPARAMS`/`FENVPARAMS` element into `(a, d, s, r)`.
fn parse_env(e: &XmlNode) -> Option<(f32, f32, f32, f32)> {
    if e.num("onOff").unwrap_or(1.0) == 0.0 {
        return None;
    }
    Some((
        env_seconds(e.num("attk").unwrap_or(0.0)),
        env_seconds(e.num("decy").unwrap_or(0.0)),
        e.num("sust").unwrap_or(1.0).clamp(0.0, 1.0),
        env_seconds(e.num("rels").unwrap_or(0.0)),
    ))
}

/// Parse a `.prt_omn` document into an [`OmniPatch`].
pub fn parse_patch(xml: &str) -> Result<OmniPatch, String> {
    let root = parse_xml(xml)?;
    parse_patch_node(&root)
}

/// Parse one part from any node containing a `SYNTHENG` (a patch document
/// root, or one `SynthEngine` inside a Multi).
pub(crate) fn parse_patch_node(root: &XmlNode) -> Result<OmniPatch, String> {
    let engine = root
        .find("SYNTHENG")
        .ok_or("no SYNTHENG element (not an Omnisphere patch?)")?;

    let mut patch = OmniPatch::default();

    if let Some(descr) = engine.child("ENTRYDESCR") {
        patch.name = descr.attr("name").unwrap_or("").to_string();
        patch.library = descr.attr("library").unwrap_or("").to_string();
        if let Some(tags) = descr.attr("ATTRIB_VALUE_DATA") {
            patch.tags = tags
                .split(';')
                .filter_map(|kv| {
                    let (k, v) = kv.split_once('=')?;
                    Some((k.trim().to_string(), v.trim().to_string()))
                })
                .collect();
        }
    }

    // Layers: VOICE[i] pairs with MULTISAMPLE[i].
    let voices: Vec<&XmlNode> = engine.children_tagged("VOICE").collect();
    let multis: Vec<&XmlNode> = engine.children_tagged("MULTISAMPLE").collect();
    for (i, voice) in voices.iter().enumerate() {
        let mut layer = OmniLayer::default();
        if let Some(ms) = multis.get(i).and_then(|m| m.child("MS_IM_0")) {
            layer.soundsource = ms.attr("name").unwrap_or("").to_string();
            layer.ss_library = ms.attr("library").unwrap_or("").to_string();
        }
        if let Some(f) = voice.child("FILTER") {
            layer.filter_name = f.attr("NameStr").unwrap_or("").to_string();
            layer.filter_parallel = f.num("para").unwrap_or(0.0) != 0.0;
            layer.filter_active = f.num("act").unwrap_or(0.0) != 0.0;
            layer.filter_freq = f.num("freq").unwrap_or(0.5);
            layer.filter_res = f.num("res").unwrap_or(0.0);
            let depth = f.num("envdpth").unwrap_or(0.0).clamp(0.0, 1.0);
            let inv = f.num("envdpthinv").unwrap_or(0.0) != 0.0;
            layer.filter_env_depth = if inv { -depth } else { depth };
            if f.num("act2").unwrap_or(0.0) != 0.0 {
                layer.filter2 = Some((
                    f.num("freq2").unwrap_or(0.5).clamp(0.0, 1.0),
                    f.num("res2").unwrap_or(0.0).clamp(0.0, 1.0),
                ));
            }
        }
        layer.amp_env = voice.child("AENVPARAMS").and_then(parse_env);
        layer.filter_env = voice.child("FENVPARAMS").and_then(parse_env);
        if let Some(osc) = voice.child("OSC") {
            layer.level = osc.num("level").unwrap_or(0.5);
            layer.fm_depth = osc.num("fm").unwrap_or(0.0).clamp(0.0, 1.0);
            layer.fm_shape = osc.num("fmwf").unwrap_or(0.0).clamp(0.0, 1.0);
            layer.ring_mix = osc.num("am").unwrap_or(0.0).clamp(0.0, 1.0);
            // Unison: the newer UNI element wins; older patches carry the
            // uns*/u* attrs directly on OSC.
            let (on, cnt, dpth, wdth) = match osc.find("UNI") {
                Some(uni) => (
                    uni.num("umix").unwrap_or(1.0) > 0.0,
                    uni.num("ucnt").unwrap_or(0.0),
                    uni.num("udpth").unwrap_or(0.1),
                    uni.num("uwdth").unwrap_or(0.7),
                ),
                None => (
                    osc.num("unsOn").unwrap_or(0.0) > 0.0,
                    osc.num("ucnt").unwrap_or(0.0),
                    osc.num("udpth").unwrap_or(0.1),
                    osc.num("uwdth").unwrap_or(0.7),
                ),
            };
            if on {
                layer.unison_count = 1 + (cnt.clamp(0.0, 1.0) * 7.0).round() as u32;
                layer.unison_detune = dpth.clamp(0.0, 1.0);
                layer.unison_width = wdth.clamp(0.0, 1.0);
                let (src_oct, src_analg, src_drft) = match osc.find("UNI") {
                    Some(uni) => (uni.num("uoct"), uni.num("uanalg"), uni.num("udrft")),
                    None => (osc.num("uoct"), osc.num("uanalg"), osc.num("udrft")),
                };
                layer.unison_octave = src_oct.unwrap_or(0.0).clamp(0.0, 1.0);
                layer.unison_analog = src_analg.unwrap_or(0.0).clamp(0.0, 1.0);
                layer.unison_drift = src_drft.unwrap_or(0.0).clamp(0.0, 1.0);
            } else {
                layer.unison_count = 1;
            }
            // Harmonia: gated by OSC hrmOn, scaled by hrmLv.
            let hrm_on = osc.num("hrmOn").unwrap_or(1.0) > 0.0;
            let hrm_lv = osc.num("hrmLv").unwrap_or(1.0).clamp(0.0, 1.0);
            if hrm_on {
                if let Some(h) = osc.find("HARM") {
                    for i in 1..=4 {
                        let act = h.num(&format!("Act{i}")).unwrap_or(0.0) > 0.0;
                        let level = h.num(&format!("lvl{i}")).unwrap_or(0.0) * hrm_lv;
                        if act && level > 0.0 {
                            // smi normalized 0..1 → ±24 semitones; pan 0..1 → ±1.
                            let smi = (h.num(&format!("smi{i}")).unwrap_or(0.5) - 0.5) * 48.0;
                            let pan = h.num(&format!("pan{i}")).unwrap_or(0.5) * 2.0 - 1.0;
                            let shape = h.num(&format!("wfm{i}")).unwrap_or(0.0).clamp(0.0, 1.0);
                            layer.harmonia.push((level.clamp(0.0, 1.0), smi.round(), pan, shape));
                        }
                    }
                }
            }
        }
        if let Some(dfs) = voice.find("DFS") {
            if dfs.num("on").unwrap_or(0.0) != 0.0 {
                // freq normalized 0.5 = no shift → ±2 kHz (CALIBRATE);
                // inv flips the direction.
                let hz = |f: Option<f32>, inv: bool| {
                    let v = (f.unwrap_or(0.5) - 0.5) * 4000.0;
                    if inv {
                        -v
                    } else {
                        v
                    }
                };
                layer.dfs = Some((
                    hz(dfs.num("freqA"), dfs.num("invA").unwrap_or(0.0) != 0.0),
                    dfs.num("mixA").unwrap_or(0.5).clamp(0.0, 1.0),
                    hz(dfs.num("freqB"), dfs.num("invB").unwrap_or(0.0) != 0.0),
                    dfs.num("mixB").unwrap_or(0.5).clamp(0.0, 1.0),
                    dfs.num("parl").unwrap_or(0.0) != 0.0,
                ));
            }
        }
        if let Some(ws) = voice.child("WAVESHAPER") {
            if ws.num("act").unwrap_or(0.0) > 0.0 {
                let drive = ws.num("dpth").unwrap_or(0.0).clamp(0.0, 1.0);
                let crush = ws.num("bc").unwrap_or(0.0).clamp(0.0, 1.0);
                let reduce = ws.num("srrdc").unwrap_or(0.0).clamp(0.0, 1.0);
                let mix = ws.num("mix").unwrap_or(1.0).clamp(0.0, 1.0);
                if drive > 0.0 || crush > 0.0 || reduce > 0.0 {
                    layer.shaper = Some((drive, crush, reduce, mix));
                }
            }
        }
        if let Some(rack) = voice.child("EFFRACK") {
            layer.fx = rack_types(rack);
        }
        patch.layers.push(layer);
    }

    // Part racks: the SYNTHENG-level EFFRACK is the Common rack.
    if let Some(rack) = engine.child("EFFRACK") {
        patch.common_fx = rack_types(rack);
    }
    if let Some(rack) = engine.child("AUXEFFRACK") {
        patch.aux_fx = rack_types(rack);
    }

    // Mod matrix: flat sourceN/targetN attribute pairs.
    if let Some(matrix) = engine.child("MOD_MATRIX") {
        for n in 0..64 {
            let (Some(source), Some(target)) = (
                matrix.attr(&format!("source{n}")),
                matrix.attr(&format!("target{n}")),
            ) else {
                break;
            };
            if source.is_empty() && target.is_empty() {
                continue;
            }
            patch.mod_routes.push(OmniModRoute {
                source: source.to_string(),
                target: target.to_string(),
                depth: matrix.num(&format!("hi{n}")).unwrap_or(0.0),
            });
        }
    }

    if let Some(set) = engine.child("LFO_SET") {
        for lfo in set.children_tagged("LFO") {
            patch.lfos.push((
                lfo.num("rate").unwrap_or(0.25).clamp(0.0, 1.0),
                lfo.num("type").unwrap_or(0.0).clamp(0.0, 1.0),
                lfo.num("sync").unwrap_or(0.0) != 0.0,
                lfo.num("resettr").unwrap_or(0.0) != 0.0,
            ));
        }
    }

    if let Some(arp) = root.find("ARP") {
        patch.arp_on = arp.num("ArpOnOff").unwrap_or(0.0) != 0.0;
        if let Some(seq) = arp.find("ARPSEQ2") {
            let tpq = seq.num("TICKSPERQUARTER").unwrap_or(1200.0).max(1.0);
            let mut raw: Vec<(f32, f32, u8)> = seq
                .children_tagged("SLICESEQSTEP")
                .map(|s| {
                    (
                        s.num("BEGIN").unwrap_or(0.0),
                        s.num("END").unwrap_or(0.0),
                        s.num("VEL").unwrap_or(0.0) as u8,
                    )
                })
                .collect();
            raw.sort_by(|a, b| a.0.total_cmp(&b.0));
            // Step length = the spacing between step starts (1/16 = TPQ/4).
            let step_ticks = raw
                .windows(2)
                .map(|w| w[1].0 - w[0].0)
                .find(|d| *d > 0.0)
                .unwrap_or(tpq / 4.0);
            patch.arp_step_beats = step_ticks / tpq;
            patch.arp_steps = raw
                .iter()
                .map(|(b, e, v)| {
                    let gate = ((e - b) / step_ticks).clamp(0.05, 1.0);
                    (*v > 0, (*v).max(1), gate)
                })
                .collect();
        }
    }

    Ok(patch)
}
