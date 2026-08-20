//! Generate a Gig Performer `.gig` with one DecentSampler rackspace per pack.
//!
//! A `.gig` is XML, not the binary chunk format its extension suggests. What is
//! opaque is the plugin state, which nests four layers deep:
//!
//! ```text
//! PROCESSORSTATEZ = juce_b64( zlib( outer ) )
//! outer           = "VC2!" u32(len) <VST3PluginState>…</VST3PluginState> NUL
//! IComponent body = juce_b64( inner )
//! inner           = "VC2!" u32(len) <DecentSampler …/> NUL JUCEPrivateData
//! ```
//!
//! Both `u32` lengths count the XML **without** its terminating NUL, and
//! DecentSampler embeds the *entire* `.dspreset` inline — so a rackspace is
//! self-contained apart from the audio files.
//!
//! Rather than synthesise a `.gig` from nothing, an existing one is used as a
//! template: its single rackspace is cloned per patch with a fresh name, a fresh
//! uid, and the plugin state rebuilt around that patch's preset. Everything
//! else — busses, connections, the MIDI-in processor, the global rackspace —
//! is whatever Gig Performer itself wrote, which is the only way to be sure it
//! is right.

use std::path::{Path, PathBuf};

use eyre::{Context, Result, bail, eyre};

/// JUCE's `MemoryBlock` base64: a `<byte-count>.` prefix, this 64-character
/// alphabet, and **LSB-first** bit packing (standard base64 is MSB-first).
///
/// Derived from a real file rather than from memory: the encoded data used
/// exactly 64 distinct characters at a 1.333 ratio, ending in `+` with no `/`.
const TABLE: &[u8; 64] = b".ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+";

fn table_index(c: u8) -> Option<u8> {
    TABLE.iter().position(|&t| t == c).map(|i| i as u8)
}

/// Decode a `<len>.<data>` JUCE base64 string.
pub fn juce_decode(s: &str) -> Result<Vec<u8>> {
    let (len, body) = s
        .split_once('.')
        .ok_or_else(|| eyre!("no '.' length prefix in JUCE base64"))?;
    let size: usize = len
        .trim()
        .parse()
        .wrap_err("JUCE base64 length prefix is not a number")?;
    let mut out = vec![0u8; size];
    for (i, c) in body.bytes().enumerate() {
        let Some(v) = table_index(c) else { continue };
        let bit = i * 6;
        let (idx, off) = (bit >> 3, bit & 7);
        if idx < size {
            out[idx] |= (v << off) & 0xFF;
        }
        if off > 2 && idx + 1 < size {
            out[idx + 1] |= v >> (8 - off);
        }
    }
    Ok(out)
}

/// Encode bytes as `<len>.<data>`.
pub fn juce_encode(data: &[u8]) -> String {
    let size = data.len();
    let num_chars = ((size << 3) + 5) / 6;
    let byte = |i: usize| -> u16 { if i < size { data[i] as u16 } else { 0 } };
    let mut s = format!("{size}.");
    s.reserve(num_chars);
    for i in 0..num_chars {
        let bit = i * 6;
        let (idx, off) = (bit >> 3, bit & 7);
        let c = if off > 2 {
            (byte(idx) >> off) | (byte(idx + 1) << (8 - off))
        } else {
            byte(idx) >> off
        };
        s.push(TABLE[(c & 0x3F) as usize] as char);
    }
    s
}

const MAGIC: &[u8; 4] = b"VC2!";

/// Split a `VC2!` container into its XML and whatever trails it.
fn vc2_unwrap(blob: &[u8]) -> Result<(String, Vec<u8>)> {
    if blob.len() < 8 || &blob[..4] != MAGIC {
        bail!("not a VC2! container");
    }
    let n = u32::from_le_bytes([blob[4], blob[5], blob[6], blob[7]]) as usize;
    if 8 + n > blob.len() {
        bail!("VC2! length {n} overruns the {} byte blob", blob.len());
    }
    let xml = String::from_utf8(blob[8..8 + n].to_vec()).wrap_err("VC2! payload is not UTF-8")?;
    Ok((xml, blob[8 + n..].to_vec()))
}

/// Build a `VC2!` container. `tail` carries the NUL and any private data.
fn vc2_wrap(xml: &str, tail: &[u8]) -> Vec<u8> {
    let raw = xml.as_bytes();
    let mut out = Vec::with_capacity(8 + raw.len() + tail.len());
    out.extend_from_slice(MAGIC);
    out.extend_from_slice(&(raw.len() as u32).to_le_bytes());
    out.extend_from_slice(raw);
    out.extend_from_slice(tail);
    out
}

/// The parts of a DecentSampler plugin state that a rackspace needs.
///
/// `outer_template` keeps the `<VST3PluginState>` wrapper verbatim with a `{}`
/// where the inner blob goes, so nothing outside the preset is invented.
pub struct StateParts {
    pub decent_xml: String,
    pub inner_tail: Vec<u8>,
    pub outer_template: String,
    pub outer_tail: Vec<u8>,
}

/// Unpack a `PROCESSORSTATEZ` blob into its parts.
pub fn decode_state(z: &str) -> Result<StateParts> {
    use std::io::Read as _;
    let mut outer = Vec::new();
    flate2::read::ZlibDecoder::new(&juce_decode(z)?[..])
        .read_to_end(&mut outer)
        .wrap_err("inflate PROCESSORSTATEZ")?;
    let (outer_xml, outer_tail) = vc2_unwrap(&outer)?;

    let open = outer_xml
        .find("<IComponent>")
        .ok_or_else(|| eyre!("no <IComponent> in the plugin state"))?
        + "<IComponent>".len();
    let close = outer_xml[open..]
        .find("</IComponent>")
        .ok_or_else(|| eyre!("unterminated <IComponent>"))?
        + open;

    let (decent_xml, inner_tail) = vc2_unwrap(&juce_decode(outer_xml[open..close].trim())?)?;
    let outer_template = format!("{}{{}}{}", &outer_xml[..open], &outer_xml[close..]);
    Ok(StateParts {
        decent_xml,
        inner_tail,
        outer_template,
        outer_tail,
    })
}

/// Repack a state, substituting a different DecentSampler preset.
pub fn encode_state(parts: &StateParts, decent_xml: &str) -> Result<String> {
    use std::io::Write as _;
    let inner = juce_encode(&vc2_wrap(decent_xml, &parts.inner_tail));
    let outer_xml = parts.outer_template.replacen("{}", &inner, 1);

    // Level 9: what Gig Performer writes, and verified byte-identical on a real
    // file, so a regenerated gig is indistinguishable from a hand-saved one.
    let mut z = flate2::write::ZlibEncoder::new(Vec::new(), flate2::Compression::best());
    z.write_all(&vc2_wrap(&outer_xml, &parts.outer_tail))
        .wrap_err("deflate plugin state")?;
    Ok(juce_encode(&z.finish().wrap_err("finish deflate")?))
}

/// Attributes DecentSampler writes into the preset when it loads one.
///
/// `_libraryBookmark` is deliberately **not** carried over. It is a macOS
/// security-scoped bookmark naming one specific file, so copying the template's
/// would aim every rackspace at whichever patch the template happened to hold.
/// Omitting it leaves `_samplePath` and `_libraryUrl` to do the resolving.
const CARRIED_ATTRS: &[&str] = &[
    "_tuningA69Frequency",
    "_velocityPreprocessorOutLow",
    "_velocityPreprocessorOutHigh",
    "_velocityPreprocessorDrive",
    "_velocityPreprocessorCompression",
    "_velocityPreprocessorRandom",
    "_mpeTimbreSensitivity",
    "_mpeTimbreMin",
    "_mpeTimbreMax",
    "_mpePressureSensitivity",
    "_mpePressureMin",
    "_mpePressureMax",
];

/// Reverse [`xml_escape`].
///
/// Attribute values must be unescaped when read, or an entity survives into a
/// filename lookup (`F.Horns&amp;Bones.dspreset` names no file) and any value
/// carried from the template gets escaped a second time on the way out.
fn xml_unescape(s: &str) -> String {
    // `&amp;` last: unescaping it first would corrupt `&amp;lt;` into `<`.
    s.replace("&quot;", "\"")
        .replace("&gt;", ">")
        .replace("&lt;", "<")
        .replace("&amp;", "&")
}

fn xml_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

/// Read `<Tag attr="value" …>` attributes from the first matching element.
fn attrs_of(xml: &str, tag: &str) -> Vec<(String, String)> {
    let Some(start) = xml.find(&format!("<{tag}")) else {
        return Vec::new();
    };
    let Some(end) = xml[start..].find('>') else {
        return Vec::new();
    };
    let inside = &xml[start + tag.len() + 1..start + end];
    let mut out = Vec::new();
    let mut rest = inside;
    while let Some(eq) = rest.find("=\"") {
        let name = rest[..eq].trim().to_string();
        let after = &rest[eq + 2..];
        let Some(q) = after.find('"') else { break };
        if !name.is_empty() {
            out.push((name, xml_unescape(&after[..q])));
        }
        rest = &after[q + 1..];
    }
    out
}

/// Build the embedded preset for one patch: its `.dspreset`, plus the
/// attributes DecentSampler needs to resolve the samples.
fn build_decent_xml(preset: &Path, folder: &str, template: &StateParts) -> Result<String> {
    let raw = std::fs::read_to_string(preset)
        .wrap_err_with(|| format!("read {}", preset.display()))?;
    let open = raw
        .find("<DecentSampler")
        .ok_or_else(|| eyre!("no <DecentSampler> root in {}", preset.display()))?;
    let close = raw[open..]
        .find('>')
        .ok_or_else(|| eyre!("unterminated <DecentSampler> in {}", preset.display()))?
        + open;
    let body = raw[close + 1..].trim();

    let stem = preset
        .file_stem()
        .and_then(|s| s.to_str())
        .ok_or_else(|| eyre!("odd preset filename {}", preset.display()))?;
    // `{instrumentLibraryRoot}` is DecentSampler's own variable for its Sample
    // Libraries folder, so the gig stays portable across machines.
    let url = format!("file://{{instrumentLibraryRoot}}/{folder}/{stem}.dspreset");

    let mut attrs = vec![
        ("minVersion".to_string(), "1.0.0".to_string()),
        (
            "_samplePath".to_string(),
            format!("{{instrumentLibraryRoot}}/{folder}"),
        ),
        ("_presetName".to_string(), stem.to_string()),
        ("_libraryUrl".to_string(), url.clone()),
        ("_libraryCanonicalUrl".to_string(), url),
        ("_sampleLibraryId".to_string(), "-1".to_string()),
    ];
    let from_template = attrs_of(&template.decent_xml, "DecentSampler");
    for key in CARRIED_ATTRS {
        if let Some((_, v)) = from_template.iter().find(|(k, _)| k == key) {
            attrs.push((key.to_string(), v.clone()));
        }
    }

    let rendered = attrs
        .iter()
        .map(|(k, v)| format!("{k}=\"{}\"", xml_escape(v)))
        .collect::<Vec<_>>()
        .join(" ");
    Ok(format!("<DecentSampler {rendered}>{body}"))
}

/// One patch to give a rackspace.
pub struct GigPatch {
    /// Set-list slot, used only for ordering.
    pub slot: u8,
    /// Display name, as it should appear in Gig Performer.
    pub name: String,
    /// Folder under the samples root (and under DecentSampler's library root).
    pub folder: String,
    /// The `.dspreset` to embed.
    pub preset: PathBuf,
}

/// What an export produced.
#[derive(Debug, Default)]
pub struct GigReport {
    pub written: PathBuf,
    pub rackspaces: Vec<String>,
    /// Patches left out, with the reason.
    pub skipped: Vec<(String, String)>,
}

/// Replace the first `attr="…"` inside the opening tag starting at `tag_start`.
fn set_attr(block: &str, tag_start: usize, attr: &str, value: &str) -> String {
    let Some(rel_end) = block[tag_start..].find('>') else {
        return block.to_string();
    };
    let tag_end = tag_start + rel_end;
    let needle = format!("{attr}=\"");
    let Some(rel) = block[tag_start..tag_end].find(&needle) else {
        return block.to_string();
    };
    let vstart = tag_start + rel + needle.len();
    let Some(vlen) = block[vstart..tag_end].find('"') else {
        return block.to_string();
    };
    format!(
        "{}{}{}",
        &block[..vstart],
        xml_escape(value),
        &block[vstart + vlen..]
    )
}

/// Write a `.gig` with one rackspace per patch, cloned from `example`.
pub fn export(example: &Path, patches: &[GigPatch], prefix: &str, out: &Path) -> Result<GigReport> {
    let gig = std::fs::read_to_string(example)
        .wrap_err_with(|| format!("read {}", example.display()))?;

    let rs_start = gig
        .find("<RACKSPACE")
        .ok_or_else(|| eyre!("no <RACKSPACE> in {}", example.display()))?;
    let rs_end = gig
        .find("</RACKSPACE>")
        .ok_or_else(|| eyre!("unterminated <RACKSPACE> in {}", example.display()))?
        + "</RACKSPACE>".len();
    let template_block = &gig[rs_start..rs_end];

    // The DecentSampler processor's state within that rackspace.
    let ds = template_block
        .find(r#"prop_str_nodeName="DecentSampler""#)
        .ok_or_else(|| eyre!("the template rackspace has no DecentSampler plugin"))?;
    let zopen = template_block[ds..]
        .find("<PROCESSORSTATEZ>")
        .ok_or_else(|| eyre!("no PROCESSORSTATEZ for DecentSampler"))?
        + ds
        + "<PROCESSORSTATEZ>".len();
    let zclose = template_block[zopen..]
        .find("</PROCESSORSTATEZ>")
        .ok_or_else(|| eyre!("unterminated PROCESSORSTATEZ"))?
        + zopen;
    let parts = decode_state(template_block[zopen..zclose].trim())?;

    let mut report = GigReport {
        written: out.to_path_buf(),
        ..Default::default()
    };
    let mut blocks = String::new();

    let mut ordered: Vec<&GigPatch> = patches.iter().collect();
    // Set-list order: the order they are played, not the order they were listed.
    ordered.sort_by_key(|p| p.slot);

    for p in ordered {
        if !p.preset.is_file() {
            report
                .skipped
                .push((p.name.clone(), "no .dspreset".into()));
            continue;
        }
        let decent = build_decent_xml(&p.preset, &p.folder, &parts)?;
        let state = encode_state(&parts, &decent)?;

        let mut block = String::with_capacity(template_block.len() + state.len());
        block.push_str(&template_block[..zopen]);
        block.push_str(&state);
        block.push_str(&template_block[zclose..]);

        let label = format!("{prefix}{}", p.name);
        block = set_attr(&block, 0, "name", &label);
        block = set_attr(&block, 0, "uid", &uuid::Uuid::new_v4().simple().to_string());

        blocks.push_str(&block);
        blocks.push('\n');
        report.rackspaces.push(label);
    }

    if report.rackspaces.is_empty() {
        bail!("no patches had a .dspreset to embed");
    }

    let mut out_xml = String::with_capacity(gig.len() + blocks.len());
    out_xml.push_str(&gig[..rs_start]);
    out_xml.push_str(&blocks);
    out_xml.push_str(&gig[rs_end..]);
    std::fs::write(out, out_xml).wrap_err_with(|| format!("write {}", out.display()))?;
    Ok(report)
}

/// Re-read a written `.gig` and check every rackspace decodes to the preset it
/// should.
///
/// Worth doing every time: a state can decode cleanly and still carry the wrong
/// preset, which is precisely what copying `_libraryBookmark` would have caused.
/// Only comparing against the *source* `.dspreset` catches that.
pub fn verify(path: &Path, packs_root: &Path) -> Result<Vec<(String, usize, bool)>> {
    let gig = std::fs::read_to_string(path).wrap_err_with(|| format!("read {}", path.display()))?;
    let mut out = Vec::new();
    let mut rest = gig.as_str();
    while let Some(i) = rest.find(r#"prop_str_nodeName="DecentSampler""#) {
        let Some(zo) = rest[i..].find("<PROCESSORSTATEZ>") else {
            break;
        };
        let zopen = i + zo + "<PROCESSORSTATEZ>".len();
        let Some(zc) = rest[zopen..].find("</PROCESSORSTATEZ>") else {
            break;
        };
        let zclose = zopen + zc;

        let parts = decode_state(rest[zopen..zclose].trim())?;
        let attrs = attrs_of(&parts.decent_xml, "DecentSampler");
        let get = |k: &str| {
            attrs
                .iter()
                .find(|(a, _)| a == k)
                .map(|(_, v)| v.clone())
                .unwrap_or_default()
        };
        let name = get("_presetName");
        let folder = get("_samplePath")
            .rsplit('/')
            .next()
            .unwrap_or_default()
            .to_string();
        let zones = parts.decent_xml.matches("<sample ").count();

        let src = packs_root.join(&folder).join(format!("{name}.dspreset"));
        let matches = std::fs::read_to_string(&src)
            .map(|s| s.matches("<sample ").count() == zones && zones > 0)
            .unwrap_or(false);
        out.push((name, zones, matches));
        rest = &rest[zclose..];
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn juce_base64_round_trips_arbitrary_bytes() {
        for len in [0usize, 1, 2, 3, 7, 8, 64, 255, 1000] {
            let data: Vec<u8> = (0..len).map(|i| (i * 37 + 11) as u8).collect();
            let enc = juce_encode(&data);
            assert!(enc.starts_with(&format!("{len}.")), "missing length prefix");
            assert_eq!(juce_decode(&enc).unwrap(), data, "round trip failed at {len}");
        }
    }

    #[test]
    fn juce_base64_uses_the_expected_alphabet() {
        // 64 characters, starting with '.', ending with '+', no '/'. Getting
        // this wrong yields a file Gig Performer silently refuses.
        assert_eq!(TABLE.len(), 64);
        assert_eq!(TABLE[0], b'.');
        assert_eq!(TABLE[63], b'+');
        assert!(!TABLE.contains(&b'/'));
    }

    #[test]
    fn juce_base64_is_lsb_first_not_standard_base64() {
        // A single 0x01 byte packs into the LOW bits of the first character, so
        // index 1 ('A'). Standard base64 would put it in the high bits.
        assert_eq!(juce_encode(&[0x01]), "1.A.");
    }

    #[test]
    fn vc2_round_trips_and_length_excludes_the_nul() {
        let xml = "<Hello world=\"1\"/>";
        let tail = b"\x00PRIVATE".to_vec();
        let blob = vc2_wrap(xml, &tail);
        assert_eq!(&blob[..4], b"VC2!");
        let n = u32::from_le_bytes([blob[4], blob[5], blob[6], blob[7]]) as usize;
        assert_eq!(n, xml.len(), "length must not count the trailing NUL");
        let (got_xml, got_tail) = vc2_unwrap(&blob).unwrap();
        assert_eq!(got_xml, xml);
        assert_eq!(got_tail, tail);
    }

    #[test]
    fn vc2_rejects_a_length_that_overruns() {
        let mut blob = vc2_wrap("<a/>", b"\x00");
        blob[4] = 0xFF; // absurd length
        assert!(vc2_unwrap(&blob).is_err());
    }

    #[test]
    fn vc2_rejects_a_bad_magic() {
        assert!(vc2_unwrap(b"NOPE\x00\x00\x00\x00").is_err());
    }

    #[test]
    fn attrs_are_read_from_the_opening_tag() {
        let xml = r#"<DecentSampler minVersion="1.0.0" _presetName="A B" _n="2"><ui/></DecentSampler>"#;
        let a = attrs_of(xml, "DecentSampler");
        assert_eq!(a.len(), 3);
        assert_eq!(a[1], ("_presetName".into(), "A B".into()));
    }

    #[test]
    fn attribute_values_are_unescaped_when_read() {
        // Names like "Orchestra & Timpani" are escaped in the XML. Reading them
        // back raw makes the preset filename lookup fail, and re-escaping an
        // already-escaped value corrupts it.
        let xml = r#"<DecentSampler _presetName="Orchestra &amp; Timpani"/>"#;
        let a = attrs_of(xml, "DecentSampler");
        assert_eq!(a[0].1, "Orchestra & Timpani");
    }

    #[test]
    fn escaping_round_trips() {
        for s in ["plain", "A & B", "a<b>c", "quote\"d", "&amp; literal"] {
            assert_eq!(xml_unescape(&xml_escape(s)), s, "failed on {s:?}");
        }
    }

    #[test]
    fn set_attr_replaces_only_within_the_opening_tag() {
        let block = r#"<RACKSPACE name="Old" uid="x"><PRESET name="Default"/></RACKSPACE>"#;
        let got = set_attr(block, 0, "name", "New");
        assert!(got.starts_with(r#"<RACKSPACE name="New""#));
        assert!(
            got.contains(r#"<PRESET name="Default"/>"#),
            "must not touch nested elements: {got}"
        );
    }

    #[test]
    fn a_state_survives_decode_then_encode() {
        // Build a state the way Gig Performer would, then round-trip it.
        let decent = r#"<DecentSampler minVersion="1.0.0"><groups/></DecentSampler>"#;
        let parts = StateParts {
            decent_xml: decent.to_string(),
            inner_tail: b"\x00JUCEPrivateData".to_vec(),
            outer_template: "<VST3PluginState><IComponent>{}</IComponent></VST3PluginState>"
                .to_string(),
            outer_tail: b"\x00".to_vec(),
        };
        let z = encode_state(&parts, decent).unwrap();
        let back = decode_state(&z).unwrap();
        assert_eq!(back.decent_xml, decent);
        assert_eq!(back.inner_tail, parts.inner_tail);
        assert_eq!(back.outer_tail, parts.outer_tail);
        assert_eq!(back.outer_template, parts.outer_template);
    }
}
