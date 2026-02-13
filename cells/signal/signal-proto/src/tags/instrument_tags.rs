//! Instrument-specific tag hierarchies.
//!
//! Each instrument type gets its own set of tags beyond the defaults.
//! These are added on top of `TagRegistry::with_defaults()`.

use super::{Tag, TagCategory, TagRegistry};
use crate::rig::InstrumentType;

/// Register guitar-specific tags into a registry.
pub fn guitar_tags(registry: &mut TagRegistry) {
    // Character extensions (beyond defaults)
    registry.add(Tag::new("Chimey", TagCategory::Character));
    registry.add(Tag::new("Heavy", TagCategory::Character));
    registry.add(Tag::new("Glassy", TagCategory::Character));
    registry.add(Tag::new("Thick", TagCategory::Character));
    registry.add(Tag::new("Compressed", TagCategory::Character));
    registry.add(Tag::new("Dynamic", TagCategory::Character));

    // Gear tags
    registry.add(Tag::new("Marshall", TagCategory::Gear));
    registry.add(Tag::new("Fender", TagCategory::Gear));
    registry.add(Tag::new("Vox", TagCategory::Gear));
    registry.add(Tag::new("Mesa", TagCategory::Gear));
    registry.add(Tag::new("5150", TagCategory::Gear));
    registry.add(Tag::new("Klon", TagCategory::Gear));
    registry.add(Tag::new("Tube Screamer", TagCategory::Gear));

    // Plugin: Manufacturer > Plugin name (hierarchical)
    let ndsp = Tag::new("Neural DSP", TagCategory::Plugin);
    let ndsp_id = ndsp.id;
    registry.add(ndsp);
    registry.add(Tag::new("Archetype Cory Wong", TagCategory::Plugin).with_parent(ndsp_id));
    registry.add(Tag::new("Archetype Gojira", TagCategory::Plugin).with_parent(ndsp_id));
    registry.add(Tag::new("Archetype Plini", TagCategory::Plugin).with_parent(ndsp_id));
    registry.add(Tag::new("Archetype Petrucci", TagCategory::Plugin).with_parent(ndsp_id));

    let line6 = Tag::new("Line 6", TagCategory::Plugin);
    let line6_id = line6.id;
    registry.add(line6);
    registry.add(Tag::new("Helix Native", TagCategory::Plugin).with_parent(line6_id));
}

/// Register bass-specific tags into a registry.
pub fn bass_tags(registry: &mut TagRegistry) {
    // Instrument tags
    registry.add(Tag::new("Electric Bass", TagCategory::Instrument));
    registry.add(Tag::new("Synth Bass", TagCategory::Instrument));
    registry.add(Tag::new("Upright Bass", TagCategory::Instrument));

    // Character extensions
    registry.add(Tag::new("Deep", TagCategory::Character));
    registry.add(Tag::new("Punchy", TagCategory::Character));
    registry.add(Tag::new("Round", TagCategory::Character));
    registry.add(Tag::new("Growly", TagCategory::Character));
    registry.add(Tag::new("Sub", TagCategory::Character));
    registry.add(Tag::new("Tight", TagCategory::Character));

    // Plugin hierarchy
    let ndsp = Tag::new("Neural DSP", TagCategory::Plugin);
    let ndsp_id = ndsp.id;
    registry.add(ndsp);
    registry.add(Tag::new("Parallax", TagCategory::Plugin).with_parent(ndsp_id));

    let darkglass = Tag::new("Darkglass", TagCategory::Plugin);
    registry.add(darkglass);

    let ampeg = Tag::new("Ampeg", TagCategory::Plugin);
    let ampeg_id = ampeg.id;
    registry.add(ampeg);
    registry.add(Tag::new("SVT", TagCategory::Plugin).with_parent(ampeg_id));
}

/// Register keys-specific tags into a registry.
pub fn keys_tags(registry: &mut TagRegistry) {
    // Instrument hierarchy (Piano > Grand, Piano > Upright, etc.)
    let piano = Tag::new("Piano", TagCategory::Instrument);
    let piano_id = piano.id;
    registry.add(piano);
    registry.add(Tag::new("Grand", TagCategory::Instrument).with_parent(piano_id));
    registry.add(Tag::new("Upright", TagCategory::Instrument).with_parent(piano_id));

    let electric = Tag::new("Electric", TagCategory::Instrument);
    let electric_id = electric.id;
    registry.add(electric);
    registry.add(Tag::new("Rhodes", TagCategory::Instrument).with_parent(electric_id));
    registry.add(Tag::new("Wurli", TagCategory::Instrument).with_parent(electric_id));

    let organ = Tag::new("Organ", TagCategory::Instrument);
    let organ_id = organ.id;
    registry.add(organ);
    registry.add(Tag::new("B3", TagCategory::Instrument).with_parent(organ_id));
    registry.add(Tag::new("Farfisa", TagCategory::Instrument).with_parent(organ_id));
    registry.add(Tag::new("Pipe", TagCategory::Instrument).with_parent(organ_id));
    registry.add(Tag::new("Vox Organ", TagCategory::Instrument).with_parent(organ_id));

    registry.add(Tag::new("Pad", TagCategory::Instrument));
    registry.add(Tag::new("Lead", TagCategory::Instrument));
    registry.add(Tag::new("Arp", TagCategory::Instrument));
    registry.add(Tag::new("Chords", TagCategory::Instrument));

    // Plugin hierarchy: Manufacturer > Plugin name
    let nord = Tag::new("Nord", TagCategory::Plugin);
    let nord_id = nord.id;
    registry.add(nord);
    registry.add(Tag::new("Nord Stage", TagCategory::Plugin).with_parent(nord_id));

    let spectrasonics = Tag::new("Spectrasonics", TagCategory::Plugin);
    let spec_id = spectrasonics.id;
    registry.add(spectrasonics);
    registry.add(Tag::new("Keyscape", TagCategory::Plugin).with_parent(spec_id));
    registry.add(Tag::new("Omnisphere", TagCategory::Plugin).with_parent(spec_id));

    let ni = Tag::new("Native Instruments", TagCategory::Plugin);
    let ni_id = ni.id;
    registry.add(ni);
    registry.add(Tag::new("Kontakt", TagCategory::Plugin).with_parent(ni_id));

    let arturia = Tag::new("Arturia", TagCategory::Plugin);
    let arturia_id = arturia.id;
    registry.add(arturia);
    registry.add(Tag::new("V Collection", TagCategory::Plugin).with_parent(arturia_id));
}

/// Register vocal-specific tags into a registry.
pub fn vocal_tags(registry: &mut TagRegistry) {
    // Instrument tags
    registry.add(Tag::new("Lead Vocal", TagCategory::Instrument));
    registry.add(Tag::new("Harmony", TagCategory::Instrument));
    registry.add(Tag::new("Background Vocals", TagCategory::Instrument));

    // Character extensions
    registry.add(Tag::new("Airy", TagCategory::Character));
    registry.add(Tag::new("Processed", TagCategory::Character));
    registry.add(Tag::new("Dry", TagCategory::Character));
    registry.add(Tag::new("Wet", TagCategory::Character));
}

/// Create a registry pre-populated for a specific instrument type.
///
/// Starts from `TagRegistry::with_defaults()` and adds instrument-specific tags.
pub fn registry_for_instrument(it: &InstrumentType) -> TagRegistry {
    let mut registry = TagRegistry::with_defaults();
    match it {
        InstrumentType::Guitar => guitar_tags(&mut registry),
        InstrumentType::Keys => keys_tags(&mut registry),
        InstrumentType::Bass => bass_tags(&mut registry),
        InstrumentType::Vocals => vocal_tags(&mut registry),
        InstrumentType::Drums | InstrumentType::Synth | InstrumentType::Custom(_) => {
            // No additional tags for these yet
        }
    }
    registry
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn guitar_registry_has_gear_tags() {
        let registry = registry_for_instrument(&InstrumentType::Guitar);
        assert!(registry.find_by_name("Marshall").is_some());
        assert!(registry.find_by_name("Klon").is_some());
    }

    #[test]
    fn guitar_registry_has_plugin_hierarchy() {
        let registry = registry_for_instrument(&InstrumentType::Guitar);
        let ndsp = registry.find_by_name("Neural DSP").unwrap();
        let children = registry.children(ndsp.id);
        assert!(children.len() >= 3);
    }

    #[test]
    fn keys_registry_has_instrument_hierarchy() {
        let registry = registry_for_instrument(&InstrumentType::Keys);
        let piano = registry.find_by_name("Piano").unwrap();
        let children = registry.children(piano.id);
        assert_eq!(children.len(), 2); // Grand, Upright
    }

    #[test]
    fn keys_registry_has_organ_hierarchy() {
        let registry = registry_for_instrument(&InstrumentType::Keys);
        let organ = registry.find_by_name("Organ").unwrap();
        let children = registry.children(organ.id);
        assert_eq!(children.len(), 4); // B3, Farfisa, Pipe, Vox Organ
    }

    #[test]
    fn bass_registry_has_instrument_tags() {
        let registry = registry_for_instrument(&InstrumentType::Bass);
        assert!(registry.find_by_name("Electric Bass").is_some());
        assert!(registry.find_by_name("Synth Bass").is_some());
    }

    #[test]
    fn bass_registry_has_character_extensions() {
        let registry = registry_for_instrument(&InstrumentType::Bass);
        assert!(registry.find_by_name("Punchy").is_some());
        assert!(registry.find_by_name("Growly").is_some());
    }

    #[test]
    fn vocal_registry_has_instrument_tags() {
        let registry = registry_for_instrument(&InstrumentType::Vocals);
        assert!(registry.find_by_name("Lead Vocal").is_some());
        assert!(registry.find_by_name("Harmony").is_some());
    }

    #[test]
    fn default_registry_includes_base_defaults() {
        let registry = registry_for_instrument(&InstrumentType::Guitar);
        // Should still have the base defaults
        assert!(registry.find_by_name("Clean").is_some());
        assert!(registry.find_by_name("Blues").is_some());
    }

    #[test]
    fn unknown_instrument_gets_defaults_only() {
        let registry = registry_for_instrument(&InstrumentType::Drums);
        // Has defaults but no instrument-specific additions
        assert!(registry.find_by_name("Clean").is_some());
        assert!(registry.find_by_name("Marshall").is_none());
    }
}
