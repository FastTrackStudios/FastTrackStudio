//! **Omnisphere patch import** — parse Spectrasonics `.prt_omn` patch files
//! (the "AmberPart" XML dialect) and map them onto the composition tree from
//! [`crate::omni`], realizing Soundsource blocks against the local
//! soundsource extraction by name.
//!
//! Format notes (reverse-engineered from the Settings Library):
//! - Plain XML, no declaration; root `<AmberPart><SynthEngine>…`.
//! - Numeric attributes are IEEE-754 `f32` bit patterns as 8 hex digits
//!   (`3f800000` = 1.0); small integers are written as decimal.
//! - `SYNTHENG` holds `VOICE` (one per layer: FILTER, WAVESHAPER, OSC + HARM,
//!   AENV/FENV + params, a 4-slot `EFFRACK`), a parallel `MULTISAMPLE` list
//!   (its `MS_IM_0 name= library=` names the layer's soundsource), the
//!   common `EFFRACK` + `AUXEFFRACK`, `LFO_SET` (6 LFOs), 6 `MODENV`s and
//!   the flat-attribute `MOD_MATRIX` (`source0`/`target0`/…).
//! - `ENTRYDESCR` carries the patch name, library and the browser tag string
//!   (`Author=…;Genre=…;Mood=…`).

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use signal_proto::block::BlockType;

use crate::rig::RigBlock;
use crate::rig_node::Container;

// ── Minimal XML ──────────────────────────────────────────────────────────────

/// One parsed element. The dialect uses no text content — only nested
/// elements and attributes — so text is ignored.
#[derive(Debug, Clone)]
pub struct XmlNode {
    pub tag: String,
    pub attrs: Vec<(String, String)>,
    pub children: Vec<XmlNode>,
}

impl XmlNode {
    pub fn attr(&self, name: &str) -> Option<&str> {
        self.attrs
            .iter()
            .find(|(k, _)| k == name)
            .map(|(_, v)| v.as_str())
    }

    /// Decode a numeric attribute (hex-bits float or decimal integer).
    pub fn num(&self, name: &str) -> Option<f32> {
        self.attr(name).map(omni_num)
    }

    pub fn child(&self, tag: &str) -> Option<&XmlNode> {
        self.children.iter().find(|c| c.tag == tag)
    }

    pub fn children_tagged<'a>(&'a self, tag: &'a str) -> impl Iterator<Item = &'a XmlNode> {
        self.children.iter().filter(move |c| c.tag == tag)
    }

    /// Depth-first search for the first element with `tag`.
    pub fn find(&self, tag: &str) -> Option<&XmlNode> {
        if self.tag == tag {
            return Some(self);
        }
        self.children.iter().find_map(|c| c.find(tag))
    }
}

/// Decode an attribute value: 8 hex digits → `f32` from bits; otherwise a
/// plain decimal number; otherwise 0.
pub fn omni_num(s: &str) -> f32 {
    let t = s.trim();
    if t.len() == 8 && t.bytes().all(|b| b.is_ascii_hexdigit()) {
        if let Ok(bits) = u32::from_str_radix(t, 16) {
            let f = f32::from_bits(bits);
            if f.is_finite() {
                return f;
            }
        }
    }
    t.parse::<f32>().unwrap_or(0.0)
}

fn decode_entities(s: &str) -> String {
    if !s.contains('&') {
        return s.to_string();
    }
    let mut out = String::with_capacity(s.len());
    let mut rest = s;
    while let Some(pos) = rest.find('&') {
        out.push_str(&rest[..pos]);
        rest = &rest[pos..];
        let end = match rest.find(';') {
            Some(e) if e <= 12 => e,
            _ => {
                out.push('&');
                rest = &rest[1..];
                continue;
            }
        };
        let ent = &rest[1..end];
        match ent {
            "amp" => out.push('&'),
            "lt" => out.push('<'),
            "gt" => out.push('>'),
            "quot" => out.push('"'),
            "apos" => out.push('\''),
            _ if ent.starts_with('#') => {
                let n = if let Some(hex) = ent.strip_prefix("#x") {
                    u32::from_str_radix(hex, 16).ok()
                } else {
                    ent[1..].parse::<u32>().ok()
                };
                match n.and_then(char::from_u32) {
                    Some(c) => out.push(c),
                    None => out.push_str(&rest[..=end]),
                }
            }
            _ => out.push_str(&rest[..=end]),
        }
        rest = &rest[end + 1..];
    }
    out.push_str(rest);
    out
}

/// Parse the AmberPart XML dialect (elements + attributes only; comments,
/// PIs and text content are skipped).
pub fn parse_xml(input: &str) -> Result<XmlNode, String> {
    let b = input.as_bytes();
    let mut i = 0usize;
    let mut stack: Vec<XmlNode> = Vec::new();
    let mut root: Option<XmlNode> = None;

    fn skip_ws(b: &[u8], i: &mut usize) {
        while *i < b.len() && b[*i].is_ascii_whitespace() {
            *i += 1;
        }
    }

    while i < b.len() {
        // Find the next tag; ignore any stray text between elements.
        match b[i..].iter().position(|&c| c == b'<') {
            Some(off) => i += off,
            None => break,
        }
        if b[i..].starts_with(b"<?") || b[i..].starts_with(b"<!--") {
            let close: &[u8] = if b[i..].starts_with(b"<?") { b"?>" } else { b"-->" };
            match b[i..]
                .windows(close.len())
                .position(|w| w == close)
            {
                Some(off) => {
                    i += off + close.len();
                    continue;
                }
                None => return Err("unterminated <? or <!--".into()),
            }
        }
        if b[i..].starts_with(b"</") {
            // Closing tag: pop.
            let end = b[i..]
                .iter()
                .position(|&c| c == b'>')
                .ok_or("unterminated close tag")?;
            i += end + 1;
            let done = stack.pop().ok_or("unbalanced close tag")?;
            match stack.last_mut() {
                Some(parent) => parent.children.push(done),
                None => {
                    root = Some(done);
                    break;
                }
            }
            continue;
        }
        // Opening tag.
        i += 1;
        let start = i;
        while i < b.len() && !b[i].is_ascii_whitespace() && b[i] != b'>' && b[i] != b'/' {
            i += 1;
        }
        let tag = std::str::from_utf8(&b[start..i])
            .map_err(|_| "bad utf8 in tag")?
            .to_string();
        let mut node = XmlNode {
            tag,
            attrs: Vec::new(),
            children: Vec::new(),
        };
        // Attributes.
        loop {
            skip_ws(b, &mut i);
            if i >= b.len() {
                return Err("unterminated tag".into());
            }
            if b[i] == b'>' {
                i += 1;
                stack.push(node);
                break;
            }
            if b[i] == b'/' {
                // Self-closing.
                i += 1;
                if i < b.len() && b[i] == b'>' {
                    i += 1;
                }
                match stack.last_mut() {
                    Some(parent) => parent.children.push(node),
                    None => root = Some(node),
                }
                break;
            }
            let astart = i;
            while i < b.len() && b[i] != b'=' && !b[i].is_ascii_whitespace() {
                i += 1;
            }
            let name = std::str::from_utf8(&b[astart..i])
                .map_err(|_| "bad utf8 in attr")?
                .to_string();
            skip_ws(b, &mut i);
            if i < b.len() && b[i] == b'=' {
                i += 1;
                skip_ws(b, &mut i);
                if i >= b.len() || b[i] != b'"' {
                    return Err(format!("attr {name} missing quote"));
                }
                i += 1;
                let vstart = i;
                while i < b.len() && b[i] != b'"' {
                    i += 1;
                }
                let value = std::str::from_utf8(&b[vstart..i])
                    .map_err(|_| "bad utf8 in attr value")?;
                i += 1; // closing quote
                node.attrs.push((name, decode_entities(value)));
            } else {
                node.attrs.push((name, String::new()));
            }
        }
        if root.is_some() {
            break;
        }
    }
    root.or_else(|| stack.pop()).ok_or_else(|| "no root element".into())
}

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
fn classify_filter(name: &str) -> (&'static str, u32) {
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
fn env_seconds(v: f32) -> f32 {
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
    }

    Ok(patch)
}

// ── Soundsource index ────────────────────────────────────────────────────────

/// Name → spec-path index over the local soundsource extraction. Multisample
/// sources are `<Name>/library.styx` dirs; one-shots are flat `<Name>.styx`.
#[derive(Debug, Default)]
pub struct SoundsourceIndex {
    by_name: HashMap<String, PathBuf>,
}

impl SoundsourceIndex {
    /// Walk `root` (e.g. `…/Omnisphere`) up to a few levels, collecting every
    /// soundsource spec keyed by lower-cased name.
    pub fn scan(root: &Path) -> Self {
        let mut idx = Self::default();
        idx.scan_dir(root, 0);
        idx
    }

    /// Scan the default extraction root (`FTS_OMNISPHERE_ROOT` override).
    pub fn scan_default() -> Self {
        let root = std::env::var("FTS_OMNISPHERE_ROOT")
            .unwrap_or_else(|_| crate::omni::OMNISPHERE_ROOT.into());
        Self::scan(Path::new(&root))
    }

    fn scan_dir(&mut self, dir: &Path, depth: usize) {
        if depth > 4 {
            return;
        }
        let Ok(entries) = std::fs::read_dir(dir) else {
            return;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                // A multisample soundsource dir: <Name>/library.styx.
                let lib = path.join("library.styx");
                if lib.exists() {
                    if let Some(name) = path.file_name().and_then(|s| s.to_str()) {
                        self.by_name.insert(name.to_lowercase(), lib);
                    }
                } else {
                    self.scan_dir(&path, depth + 1);
                }
            } else if path.extension().is_some_and(|e| e == "styx")
                && path.file_name().is_some_and(|f| f != "library.styx")
            {
                // A flat one-shot: <Name>.styx beside its FLAC.
                if let Some(stem) = path.file_stem().and_then(|s| s.to_str()) {
                    self.by_name.insert(stem.to_lowercase(), path.clone());
                }
            }
        }
    }

    pub fn len(&self) -> usize {
        self.by_name.len()
    }

    pub fn is_empty(&self) -> bool {
        self.by_name.is_empty()
    }

    /// Look a soundsource up by its patch name (case-insensitive).
    pub fn find(&self, name: &str) -> Option<&Path> {
        self.by_name.get(&name.to_lowercase()).map(|p| p.as_path())
    }
}

// ── Patch → composition tree ─────────────────────────────────────────────────

const LAYER_NAMES: [&str; 4] = ["Layer A", "Layer B", "Layer C", "Layer D"];

fn fx_rack_from(name: &str, types: &[String]) -> Container {
    let mut rack = Container::module(name);
    for slot in 0..4 {
        let label = types
            .get(slot)
            .map(|s| s.as_str())
            .filter(|s| !s.is_empty() && *s != "No Effect");
        rack = match label {
            Some(fx) => rack.block(BlockType::Custom, fx),
            None => rack.block(BlockType::Custom, format!("{name} Slot {}", slot + 1)),
        };
    }
    rack
}

/// Translate one Omnisphere mod-matrix route into our route model, when the
/// target is something the runtime drives today.
///
/// Returns `(layer_index, source, target, depth)` — `layer_index` scopes the
/// route to a layer (`A freq` targets Layer A's filter); part-wide routes use
/// the layer the target names.
fn translate_route(
    route: &OmniModRoute,
    filter_labels: &[String],
) -> Option<(usize, String, String, f32)> {
    // Targets: "<L> freq" / "<L> res" where <L> is A..D → the layer's Filter 1.
    let (layer_letter, param) = route.target.split_once(' ')?;
    let layer_idx = match layer_letter {
        "A" => 0,
        "B" => 1,
        "C" => 2,
        "D" => 3,
        _ => return None,
    };
    // Pitch targets ride the synth oscillator's tune param; freq/res ride
    // the layer's Filter 1.
    let (block, param, scale): (&str, &str, f32) = match param {
        "freq" => (filter_labels.get(layer_idx)?.as_str(), "cutoff", 1.0),
        "res" => (filter_labels.get(layer_idx)?.as_str(), "resonance", 1.0),
        "tune" => ("Synth Osc", "tune", 1.0),
        // tuneFine is ±1 semitone on a ±24 semitone param.
        "tuneFine" => ("Synth Osc", "tune", 1.0 / 24.0),
        // Osc amp tremolo → the layer's Amp gain.
        "atrm" => ("Amp", "gain", 1.0),
        // PWM depth → the square's pulse width (Symmetry axis).
        "pdepth" => ("Synth Osc", "symmetry", 1.0),
        // Harmonia mix.
        "Harmmix" => ("Synth Osc", "harm_mix", 1.0),
        _ => return None, // hrdsnc/mogrify/timbre/LFO-param/E1P0/… — later
    };
    // Sources: MIDI performance names map directly; Omnisphere modulator
    // names map onto the modulator blocks our tree attaches.
    let source = match route.source.as_str() {
        "Wheel" => "Wheel".to_string(),
        "Velo" => "Velocity".to_string(),
        "After" => "Aftertouch".to_string(),
        "Bender" => "Bender".to_string(),
        "Key" => "Key".to_string(),
        "Alt" => "Alt".to_string(),
        "Constant" | "Bias1" | "Bias2" => "Constant".to_string(),
        "Random" | "Random2" | "Random Unipolar" => "Random".to_string(),
        "MPEv" => "MPEPressure".to_string(),
        "MPE3" => "MPETimbre".to_string(),
        s if s.starts_with("LFO") => format!("LFO {}", &s[3..]),
        s if s.ends_with("FENV") => "Filter Env".to_string(),
        s if s.starts_with("ModEnv") => "Mod Env".to_string(),
        _ => return None,
    };
    Some((
        layer_idx,
        source,
        format!("{block}.{param}"),
        route.depth * scale,
    ))
}

/// Map a parsed patch onto the Omnisphere composition tree, realizing each
/// layer's Soundsource block against `index` (unmatched names stay
/// placeholders — the structure still routes).
pub fn patch_to_container(patch: &OmniPatch, index: &SoundsourceIndex) -> Container {
    // Filter block labels per layer (route targets reference them by name).
    let filter_labels: Vec<String> = patch
        .layers
        .iter()
        .take(4)
        .map(|l| {
            if l.filter_name.is_empty() {
                "Filter 1".to_string()
            } else {
                l.filter_name.clone()
            }
        })
        .collect();
    // Live routes bucketed per layer; the rest stay inspectable params.
    let mut layer_routes: Vec<Vec<(String, String, f32)>> = vec![Vec::new(); 4];
    for route in &patch.mod_routes {
        if let Some((idx, source, target, depth)) = translate_route(route, &filter_labels) {
            layer_routes[idx].push((source, target, depth));
        }
    }

    let mut quadzone = Container::parallel("Quadzone").param("mode", "Fader");
    for (i, layer) in patch.layers.iter().take(4).enumerate() {
        let name = LAYER_NAMES[i];

        let mut osc = Container::module("Oscillator");
        osc = if layer.soundsource.is_empty() {
            // Synth mode: the wavetable voice carries the whole oscillator
            // stack (unison / harmonia / FM / ring) as build params.
            let mut wt = RigBlock::of_type(BlockType::Wavetable).named("Synth Osc");
            if layer.unison_count > 1 {
                wt = wt
                    .with_param("unison_voices", layer.unison_count.to_string())
                    .with_param("unison_detune", format!("{:.4}", layer.unison_detune))
                    .with_param("unison_width", format!("{:.4}", layer.unison_width));
                if layer.unison_octave > 0.0 {
                    wt = wt.with_param("unison_octave", format!("{:.4}", layer.unison_octave));
                }
                if layer.unison_analog > 0.0 {
                    wt = wt.with_param("unison_analog", format!("{:.4}", layer.unison_analog));
                }
                if layer.unison_drift > 0.0 {
                    wt = wt.with_param("unison_drift", format!("{:.4}", layer.unison_drift));
                }
            }
            if let Some((a, d, s, r)) = layer.amp_env {
                wt = wt
                    .with_param("amp_attack", format!("{a:.4}"))
                    .with_param("amp_decay", format!("{d:.4}"))
                    .with_param("amp_sustain", format!("{s:.4}"))
                    .with_param("amp_release", format!("{r:.4}"));
            }
            if layer.fm_depth > 0.0 {
                wt = wt
                    .with_param("fm_depth", format!("{:.4}", layer.fm_depth))
                    .with_param("fm_shape", format!("{:.4}", layer.fm_shape));
            }
            if layer.ring_mix > 0.0 {
                wt = wt.with_param("ring_mix", format!("{:.4}", layer.ring_mix));
            }
            for (i, (level, smi, pan, shape)) in layer.harmonia.iter().take(4).enumerate() {
                let n = i + 1;
                wt = wt
                    .with_param(format!("harm{n}_level"), format!("{level:.4}"))
                    .with_param(format!("harm{n}_interval"), format!("{smi:.1}"))
                    .with_param(format!("harm{n}_pan"), format!("{pan:.4}"))
                    .with_param(format!("harm{n}_shape"), format!("{shape:.4}"));
            }
            osc.add(wt)
        } else {
            match index.find(&layer.soundsource) {
                Some(spec) => {
                    // Sample mode: unison + amp attack/release ride the
                    // Sampler block (the engine handles them at trigger time;
                    // decay/sustain need a full per-voice ADSR — pending).
                    let mut sb = RigBlock::sample_lib(spec.to_string_lossy().to_string())
                        .named(&layer.soundsource);
                    if layer.unison_count > 1 {
                        sb = sb
                            .with_param("unison_voices", layer.unison_count.to_string())
                            .with_param("unison_detune", format!("{:.4}", layer.unison_detune))
                            .with_param("unison_width", format!("{:.4}", layer.unison_width));
                    }
                    if let Some((a, _d, _s, r)) = layer.amp_env {
                        sb = sb
                            .with_param("amp_attack", format!("{a:.4}"))
                            .with_param("amp_release", format!("{r:.4}"));
                    }
                    osc.add(sb)
                }
                None => {
                    tracing::warn!(
                        soundsource = %layer.soundsource,
                        library = %layer.ss_library,
                        "omni import: soundsource not in the local extraction — placeholder"
                    );
                    osc.block(BlockType::Sampler, &layer.soundsource)
                }
            }
        };
        let mut shaper_block = RigBlock::of_type(BlockType::Waveshaper).named("Waveshaper");
        if let Some((drive, crush, reduce, mix)) = layer.shaper {
            shaper_block = shaper_block
                .with_param("drive", format!("{drive:.4}"))
                .with_param("crush", format!("{crush:.4}"))
                .with_param("reduce", format!("{reduce:.4}"))
                .with_param("mix", format!("{mix:.4}"));
        }
        let osc = osc
            .block(BlockType::Unison, "Unison")
            .block(BlockType::Harmonic, "Harmonia")
            .block(BlockType::FmOperator, "FM")
            .block(BlockType::RingModulator, "Ring Mod")
            .add(shaper_block)
            .block(BlockType::Granular, "Granular");

        let filter_label = filter_labels[i].clone();
        let mut built = Container::layer(name)
                .param("level", format!("{:.3}", layer.level))
                .param(
                    "filter_routing",
                    if layer.filter_parallel { "Parallel" } else { "Series" },
                )
                .param("filter_freq", format!("{:.3}", layer.filter_freq))
                .param("filter_res", format!("{:.3}", layer.filter_res))
                .add(osc)
                .add({
                    // Filter 1 carries the imported cutoff/resonance when the
                    // section is engaged, plus a coarse mode/poles algorithm
                    // classified from the factory preset name.
                    let mut f1 = RigBlock::of_type(BlockType::Filter).named(filter_label.clone());
                    if layer.filter_active {
                        let (mode, poles) = classify_filter(&layer.filter_name);
                        f1 = f1
                            .with_param("cutoff", format!("{:.4}", layer.filter_freq))
                            .with_param("resonance", format!("{:.4}", layer.filter_res))
                            .with_param("mode", mode)
                            .with_param("poles", poles.to_string());
                    }
                    let mut f2 = RigBlock::of_type(BlockType::Filter).named("Filter 2");
                    if let Some((freq, res)) = layer.filter2 {
                        if layer.filter_active {
                            f2 = f2
                                .with_param("cutoff", format!("{freq:.4}"))
                                .with_param("resonance", format!("{res:.4}"));
                        }
                    }
                    // SERIES chains the filters; PARALLEL sums them.
                    let filters = if layer.filter_parallel {
                        Container::parallel("Filters")
                    } else {
                        Container::module("Filters")
                    };
                    filters.add(f1).add(f2)
                })
                .add(Container::module("Amp").block(BlockType::Amp, "Amp"))
                .add(fx_rack_from("Layer FX", &layer.fx))
                .send("Aux Rack", "To Aux")
                .modulator(BlockType::Envelope, "Amp Env")
                .modulator_block({
                    // The filter envelope carries its imported ADSR so the
                    // mod engine gates/sweeps with the patch's own shape.
                    let mut fe = RigBlock::of_type(BlockType::Envelope).named("Filter Env");
                    if let Some((a, d, s, r)) = layer.filter_env {
                        fe = fe
                            .with_param("attack", format!("{a:.4}"))
                            .with_param("decay", format!("{d:.4}"))
                            .with_param("sustain", format!("{s:.4}"))
                            .with_param("release", format!("{r:.4}"));
                    }
                    fe
                })
                .modulator(BlockType::MultisegEnvelope, "Mod Env");
        // The filter section's own envelope depth (independent of matrix rows).
        if layer.filter_active && layer.filter_env_depth != 0.0 {
            built = built.route(
                "Filter Env",
                format!("{}.cutoff", filter_labels[i]),
                layer.filter_env_depth,
            );
        }
        for (source, target, depth) in layer_routes[i].drain(..) {
            built = built.route(source, target, depth);
        }
        quadzone = quadzone.add(built);
    }

    let title = if patch.name.is_empty() {
        "Omnisphere Patch".to_string()
    } else {
        patch.name.clone()
    };
    let mut preset = Container::preset(title)
        .add(quadzone)
        .add(fx_rack_from("Common FX", &patch.common_fx))
        .add(fx_rack_from("Aux Rack", &patch.aux_fx))
        .modulator(BlockType::ModMatrix, "Mod Matrix");
    for n in 1..=8usize {
        let mut lfo = RigBlock::of_type(BlockType::Lfo).named(format!("LFO {n}"));
        if let Some((rate, ty, sync, retrig)) = patch.lfos.get(n - 1) {
            // Normalized rate → Hz (exp sweep 0.05..20; CALIBRATE) and
            // normalized type → wave index 0..4 (4 = S&H).
            lfo = lfo
                .with_param("rate", format!("{:.4}", 0.05 * 400f32.powf(*rate)))
                .with_param("wave", format!("{}", (ty * 4.0).round() as u32));
            if *sync {
                // Tempo-synced: rate index → beats/cycle (CALIBRATE).
                let beats = [4.0, 2.0, 1.0, 0.5, 0.25, 0.125][(rate * 5.0).round() as usize];
                lfo = lfo.with_param("sync_beats", format!("{beats}"));
            }
            if *retrig {
                lfo = lfo.with_param("retrigger", "1");
            }
        }
        preset = preset.modulator_block(lfo);
    }
    if patch.arp_on {
        preset = preset.modulator(BlockType::Arpeggiator, "Arp");
    }
    // Carry the browser tags + mod routes as preset params (inspectable in
    // dumps and the TUI; the mod routes become live once the ModMatrix
    // runtime lands).
    for (k, v) in &patch.tags {
        preset = preset.param(format!("tag:{k}"), v.clone());
    }
    for (i, route) in patch.mod_routes.iter().enumerate() {
        preset = preset.param(
            format!("mod{i}"),
            format!("{} -> {} @ {:.3}", route.source, route.target, route.depth),
        );
    }
    preset
}

/// Convenience: read + parse + map a `.prt_omn` file.
pub fn load_patch_file(path: &Path, index: &SoundsourceIndex) -> Result<Container, String> {
    let xml = std::fs::read_to_string(path).map_err(|e| format!("read {path:?}: {e}"))?;
    let patch = parse_patch(&xml)?;
    Ok(patch_to_container(&patch, index))
}

#[cfg(test)]
mod tests {
    use super::*;

    const MINI_PATCH: &str = r#"<AmberPart >
<SynthEngine >
<ARP ArpOnOff="3f800000" >
</ARP>
<SYNTHENG >
<ENTRYDESCR name="Test Patch" library="Test Library" ATTRIB_VALUE_DATA="Author=Cody;Mood=Fun" >
</ENTRYDESCR>
<MOD_MATRIX source0="Layer A FENV" target0="A freq" hi0="3f000000" >
</MOD_MATRIX>
<VOICE >
<FILTER NameStr="LPF Test" act="3f800000" para="0" freq="3f000000" res="3e800000" envdpth="3f000000" envdpthinv="0" >
</FILTER>
<AENVPARAMS onOff="3f800000" attk="0" decy="0" sust="3f800000" rels="3f000000" >
</AENVPARAMS>
<FENVPARAMS onOff="3f800000" attk="3e4ccccd" decy="3f000000" sust="3f000000" rels="3f000000" >
</FENVPARAMS>
<OSC level="3f400000" fm="3e800000" am="0" hrmOn="3f800000" hrmLv="3f800000" >
<UNI umix="3f800000" ucnt="3f800000" udpth="3e4ccccd" uwdth="3f800000" >
</UNI>
<HARM Act1="3f800000" lvl1="3f000000" smi1="3f4aaaab" pan1="3f000000" wfm1="0" >
</HARM>
</OSC>
<WAVESHAPER act="3f800000" dpth="3f000000" bc="0" srrdc="0" mix="3f800000" >
</WAVESHAPER>
<EFFRACK >
<EFFMODULE Type="Chorus Echo" Active="3f800000" >
</EFFMODULE>
<EFFMODULE Type="No Effect" Active="0" >
</EFFMODULE>
</EFFRACK>
</VOICE>
<MULTISAMPLE >
<MS_IM_0 name="My Source" library="Core Library" >
</MS_IM_0>
</MULTISAMPLE>
<EFFRACK >
<EFFMODULE Type="PRO-Verb" Active="3f800000" >
</EFFMODULE>
</EFFRACK>
<AUXEFFRACK >
</AUXEFFRACK>
</SYNTHENG>
</SynthEngine>
</AmberPart>
"#;

    #[test]
    fn filter_names_classify() {
        assert_eq!(classify_filter("Classic LPF 4-pole"), ("lowpass", 4));
        assert_eq!(classify_filter("Basic 12db Lowpass"), ("lowpass", 2));
        assert_eq!(classify_filter("HPF Juicy 24db"), ("highpass", 4));
        assert_eq!(classify_filter("Bandpass Juicy 12db"), ("bandpass", 2));
        assert_eq!(classify_filter("Notch Filter"), ("notch", 2));
        assert_eq!(classify_filter("Classic LPF 8-pole"), ("lowpass", 8));
        assert_eq!(classify_filter("untitled"), ("lowpass", 2));
    }

    #[test]
    fn hex_floats_decode() {
        assert_eq!(omni_num("3f800000"), 1.0);
        assert_eq!(omni_num("3f000000"), 0.5);
        assert_eq!(omni_num("0"), 0.0);
        assert_eq!(omni_num("1200"), 1200.0);
    }

    #[test]
    fn parses_the_mini_patch() {
        let p = parse_patch(MINI_PATCH).unwrap();
        assert_eq!(p.name, "Test Patch");
        assert_eq!(p.library, "Test Library");
        assert_eq!(p.tags, vec![
            ("Author".to_string(), "Cody".to_string()),
            ("Mood".to_string(), "Fun".to_string())
        ]);
        assert!(p.arp_on);
        assert_eq!(p.layers.len(), 1);
        let l = &p.layers[0];
        assert_eq!(l.soundsource, "My Source");
        assert_eq!(l.filter_name, "LPF Test");
        assert_eq!(l.filter_freq, 0.5);
        assert_eq!(l.level, 0.75);
        assert_eq!(l.fx[0], "Chorus Echo");
        assert_eq!(p.common_fx[0], "PRO-Verb");
        // Oscillator stack: 8-voice unison at 20 cents, FM 0.25, one
        // harmonia voice +14 semitones at half level, drive-0.5 waveshaper.
        assert_eq!(l.unison_count, 8);
        assert!((l.unison_detune - 0.2).abs() < 1e-3);
        assert!((l.fm_depth - 0.25).abs() < 1e-6);
        assert_eq!(l.harmonia.len(), 1);
        let (level, smi, pan, _shape) = l.harmonia[0];
        assert!((level - 0.5).abs() < 1e-3);
        assert_eq!(smi, 14.0);
        assert!(pan.abs() < 1e-3);
        assert_eq!(l.shaper, Some((0.5, 0.0, 0.0, 1.0)));
        // Envelopes: amp release 0.5³·10 = 1.25 s; filter env present with
        // its own cutoff depth of +0.5.
        let (aa, _ad, asus, ar) = l.amp_env.expect("amp env");
        assert_eq!(aa, 0.0);
        assert!((asus - 1.0).abs() < 1e-6);
        assert!((ar - 1.25).abs() < 1e-3);
        assert!(l.filter_env.is_some());
        assert!((l.filter_env_depth - 0.5).abs() < 1e-6);
        assert_eq!(p.mod_routes.len(), 1);
        assert_eq!(p.mod_routes[0].source, "Layer A FENV");
        assert_eq!(p.mod_routes[0].target, "A freq");
    }

    #[test]
    fn maps_to_a_container() {
        let p = parse_patch(MINI_PATCH).unwrap();
        let tree = patch_to_container(&p, &SoundsourceIndex::default());
        assert_eq!(tree.name, "Test Patch");
        let layer = tree.find("Layer A").expect("layer A");
        // Unmatched soundsource stays a placeholder block with the name.
        let names: Vec<_> = layer
            .find("Oscillator")
            .unwrap()
            .blocks()
            .iter()
            .map(|b| b.display_name())
            .collect();
        assert!(names.iter().any(|n| n == "My Source"));
        // The layer FX slot carries the effect name.
        let fx: Vec<_> = layer
            .find("Layer FX")
            .unwrap()
            .blocks()
            .iter()
            .map(|b| b.display_name())
            .collect();
        assert_eq!(fx[0], "Chorus Echo");
        // Tags + mod routes survive as params.
        assert!(tree.params.iter().any(|p| p.name == "tag:Author"));
        assert!(tree.params.iter().any(|p| p.name == "mod0"));
        // Two live routes on Layer A: the filter section's own envdpth route
        // plus the matrix row — both "Filter Env" → cutoff at 0.5.
        assert_eq!(layer.mod_routes.len(), 2);
        for r in &layer.mod_routes {
            assert_eq!(r.source, "Filter Env");
            assert_eq!(r.target, "LPF Test.cutoff");
            assert!((r.depth - 0.5).abs() < 1e-6);
        }
        // The filter envelope modulator carries its imported ADSR.
        let fe = layer
            .modulators
            .iter()
            .find(|m| m.display_name() == "Filter Env")
            .expect("filter env modulator");
        assert!((fe.param_f32("sustain").unwrap() - 0.5).abs() < 1e-6);
        assert!(fe.param_f32("attack").unwrap() > 0.0);
        // The engaged filter carries the imported cutoff/res as build params.
        let f1 = layer
            .find("Filters")
            .unwrap()
            .blocks()
            .into_iter()
            .find(|b| b.display_name() == "LPF Test")
            .expect("filter 1");
        assert_eq!(f1.param_f32("cutoff"), Some(0.5));
        assert_eq!(f1.param_f32("resonance"), Some(0.25));
        // Renders (placeholder-safe).
        let mut rn = crate::node_render::RenderNode::compile(&tree, 48_000);
        rn.prepare(48_000.0, 256);
        assert!(rn.live_leaves() >= 3, "native filters/amp are live");
    }

    /// Machine-local: import one of the user's own patches from the synced
    /// voyager Settings Library, realize its soundsources against the local
    /// extraction, and render it audibly.
    /// `cargo test -p signal-sampler --lib voyager_patch -- --ignored`
    #[test]
    #[ignore = "requires the voyager patch sync + soundsource extraction"]
    fn voyager_patch_imports_and_sounds() {
        use signal_plugin_host::{PluginEvents, PluginMidiEvent};
        let root = Path::new(
            "/run/media/AudioHaven/Sampled/Synth/Spectrasonics-Patches/Omnisphere-Voyager/Settings Library/Patches",
        );
        if !root.exists() {
            eprintln!("skipping: {root:?} not present");
            return;
        }
        let index = SoundsourceIndex::scan_default();
        assert!(!index.is_empty(), "soundsource extraction indexed");
        // First SAMPLE-mode patch whose soundsource resolves against the
        // extraction (pure synth-mode patches are placeholder-silent until
        // the Wavetable DSP lands).
        let mut patch_path = None;
        let mut stack = vec![root.to_path_buf()];
        'outer: while let Some(dir) = stack.pop() {
            let Ok(entries) = std::fs::read_dir(&dir) else { continue };
            let mut entries: Vec<_> = entries.flatten().map(|e| e.path()).collect();
            entries.sort();
            for p in entries {
                if p.is_dir() {
                    stack.push(p);
                } else if p.extension().is_some_and(|x| x == "prt_omn") {
                    let xml = std::fs::read_to_string(&p).unwrap_or_default();
                    if let Ok(parsed) = parse_patch(&xml) {
                        if parsed
                            .layers
                            .iter()
                            .any(|l| !l.soundsource.is_empty() && index.find(&l.soundsource).is_some())
                        {
                            patch_path = Some(p);
                            break 'outer;
                        }
                    }
                }
            }
        }
        let patch_path = patch_path.expect("a sample-mode .prt_omn with a matched soundsource");
        let tree = load_patch_file(&patch_path, &index).expect("import");
        eprintln!("imported {:?} as {:?}", patch_path.file_name(), tree.name);

        let mut rn = crate::node_render::RenderNode::compile(&tree, 48_000);
        rn.prepare(48_000.0, 512);
        let (mut l, mut r) = (vec![0.0; 512], vec![0.0; 512]);
        let midi = [PluginMidiEvent {
            offset: 0,
            message: daw::service::MidiMessage::note_on(0, 60, 100),
        }];
        let mut heard = 0.0f32;
        for _ in 0..600 {
            let ev = PluginEvents {
                params: &[],
                midi: &midi,
                note_expressions: &[],
            };
            rn.render(&mut l, &mut r, &ev);
            let rms = (l.iter().map(|s| s * s).sum::<f32>() / l.len() as f32).sqrt();
            heard = heard.max(rms);
            if heard > 1e-3 {
                break;
            }
            std::thread::sleep(std::time::Duration::from_millis(10));
        }
        assert!(heard > 1e-3, "imported patch should be audible, rms={heard}");
    }

    /// Machine-local: a pure SYNTH-mode patch (no soundsources) now sounds
    /// via the native wavetable oscillator.
    /// `cargo test -p signal-sampler --lib synth_mode_patch -- --ignored`
    #[test]
    #[ignore = "requires the voyager patch sync"]
    fn synth_mode_patch_sounds() {
        use signal_plugin_host::{PluginEvents, PluginMidiEvent};
        let path = Path::new(
            "/run/media/AudioHaven/Sampled/Synth/Spectrasonics-Patches/Omnisphere-Voyager/Settings Library/Patches/User/My Category/1975 Attempt.prt_omn",
        );
        if !path.exists() {
            eprintln!("skipping: {path:?} not present");
            return;
        }
        let tree = load_patch_file(path, &SoundsourceIndex::default()).expect("import");
        let mut rn = crate::node_render::RenderNode::compile(&tree, 48_000);
        rn.prepare(48_000.0, 512);
        let (mut l, mut r) = (vec![0.0; 512], vec![0.0; 512]);
        let midi = [PluginMidiEvent {
            offset: 0,
            message: daw::service::MidiMessage::note_on(0, 60, 100),
        }];
        let mut heard = 0.0f32;
        for b in 0..8 {
            let ev = PluginEvents {
                params: &[],
                midi: if b == 0 { &midi } else { &[] },
                note_expressions: &[],
            };
            rn.render(&mut l, &mut r, &ev);
            heard = heard.max((l.iter().map(|s| s * s).sum::<f32>() / 512.0).sqrt());
        }
        assert!(heard > 1e-3, "synth-mode patch audible, rms={heard}");
    }

    /// Machine-local: parse every factory patch in the on-disk Settings
    /// Library without errors (format coverage sweep).
    /// `cargo test -p signal-sampler --lib factory_patches -- --ignored`
    #[test]
    #[ignore = "requires the local Spectrasonics patch library"]
    fn factory_patches_all_parse() {
        let root = Path::new(
            "/run/media/AudioHaven/Sampled/Synth/Spectrasonics-Patches/Omnisphere/Settings Library/Patches",
        );
        if !root.exists() {
            eprintln!("skipping: {root:?} not present");
            return;
        }
        let mut total = 0usize;
        let mut failed = Vec::new();
        let mut stack = vec![root.to_path_buf()];
        while let Some(dir) = stack.pop() {
            let Ok(entries) = std::fs::read_dir(&dir) else {
                continue;
            };
            for e in entries.flatten() {
                let p = e.path();
                if p.is_dir() {
                    stack.push(p);
                } else if p.extension().is_some_and(|x| x == "prt_omn") {
                    total += 1;
                    let xml = std::fs::read_to_string(&p).unwrap_or_default();
                    if let Err(err) = parse_patch(&xml) {
                        failed.push(format!("{p:?}: {err}"));
                    }
                }
            }
        }
        eprintln!("parsed {total} patches, {} failures", failed.len());
        for f in failed.iter().take(10) {
            eprintln!("  {f}");
        }
        assert!(total > 1000, "expected a big factory library, got {total}");
        assert!(
            failed.is_empty(),
            "{} of {total} factory patches failed to parse",
            failed.len()
        );
    }
}
