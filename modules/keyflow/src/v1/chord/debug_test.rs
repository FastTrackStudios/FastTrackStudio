use crate::chord::chord::Chord;
use crate::primitives::note::Note;

pub fn debug_chord_structures() {
    println!("=== DEBUGGING CHORD STRUCTURES ===");
    
    // Test basic chords
    let c_maj = Chord::maj().with_root(Note::c());
    let c_maj7 = Chord::maj7().with_root(Note::c());
    let c_dom7 = Chord::dom7().with_root(Note::c());
    let c_min7 = Chord::min7().with_root(Note::c());
    let c_dim = Chord::dim().with_root(Note::c());
    let c_aug = Chord::aug().with_root(Note::c());
    
    println!("\n--- Basic Chords ---");
    println!("C major: {:?}", c_maj);
    println!("C major7: {:?}", c_maj7);
    println!("C dom7: {:?}", c_dom7);
    println!("C min7: {:?}", c_min7);
    println!("C dim: {:?}", c_dim);
    println!("C aug: {:?}", c_aug);
    
    // Test extensions
    let c_maj9 = Chord::maj().ninth().with_root(Note::c());
    let c_dom11 = Chord::dom().eleventh().with_root(Note::c());
    let c_dom13 = Chord::dom().thirteenth().with_root(Note::c());
    
    println!("\n--- Extended Chords ---");
    println!("C maj9: {:?}", c_maj9);
    println!("C dom11: {:?}", c_dom11);
    println!("C dom13: {:?}", c_dom13);
    
    // Test alterations
    let c_dom_flat5 = Chord::dom().flat_5().with_root(Note::c());
    let c_dom_sharp5 = Chord::dom().sharp_5().with_root(Note::c());
    let c_dom_flat9 = Chord::dom().flat_9().with_root(Note::c());
    let c_dom_sharp11 = Chord::dom().sharp_11().with_root(Note::c());
    let c_dom_alt = Chord::dom().alt().with_root(Note::c());
    
    println!("\n--- Altered Chords ---");
    println!("C dom b5: {:?}", c_dom_flat5);
    println!("C dom #5: {:?}", c_dom_sharp5);
    println!("C dom b9: {:?}", c_dom_flat9);
    println!("C dom #11: {:?}", c_dom_sharp11);
    println!("C dom alt: {:?}", c_dom_alt);
    
    // Test suspended chords
    let c_sus4 = Chord::maj().sus4().with_root(Note::c());
    let c_sus2 = Chord::maj().sus2().with_root(Note::c());
    let c_7sus4 = Chord::dom().sus4().with_root(Note::c());
    
    println!("\n--- Suspended Chords ---");
    println!("C sus4: {:?}", c_sus4);
    println!("C sus2: {:?}", c_sus2);
    println!("C 7sus4: {:?}", c_7sus4);
    
    // Test slash chords
    let c_maj_over_e = Chord::maj().slash_bass(Note::e()).with_root(Note::c());
    let c_maj7_over_f = Chord::maj7().slash_bass(Note::f()).with_root(Note::c());
    
    println!("\n--- Slash Chords ---");
    println!("C/E: {:?}", c_maj_over_e);
    println!("C Maj7/F: {:?}", c_maj7_over_f);
    
    // Test omitted intervals
    let c_maj_no3 = Chord::maj().omit_3().with_root(Note::c());
    let c_maj_no5 = Chord::maj().omit_5().with_root(Note::c());
    let c_maj7_no3 = Chord::maj7().omit_3().with_root(Note::c());
    
    println!("\n--- Omitted Intervals ---");
    println!("C maj no3: {:?}", c_maj_no3);
    println!("C maj no5: {:?}", c_maj_no5);
    println!("C maj7 no3: {:?}", c_maj7_no3);
    
    // Test complex chords
    let c_complex = Chord::dom()
        .thirteenth()
        .flat_5()
        .sharp_11()
        .slash_bass(Note::e())
        .with_root(Note::c());
    
    println!("\n--- Complex Chord ---");
    println!("C dom13 b5 #11/E: {:?}", c_complex);
    
    // Print interval information
    println!("\n--- Interval Analysis ---");
    println!("C maj intervals: {:?}", c_maj.intervals);
    println!("C maj7 intervals: {:?}", c_maj7.intervals);
    println!("C dom7 intervals: {:?}", c_dom7.intervals);
    println!("C dom alt intervals: {:?}", c_dom_alt.intervals);
    println!("C sus4 intervals: {:?}", c_sus4.intervals);
    
    // Print chord tones
    println!("\n--- Chord Tones ---");
    println!("C maj chord tones: {:?}", c_maj.chord_tones());
    println!("C maj7 chord tones: {:?}", c_maj7.chord_tones());
    println!("C dom7 chord tones: {:?}", c_dom7.chord_tones());
    println!("C dom alt chord tones: {:?}", c_dom_alt.chord_tones());
    
    // Print colored output
    println!("\n--- Colored Output ---");
    println!("C major: {}", c_maj.to_colored_string());
    println!("C major7: {}", c_maj7.to_colored_string());
    println!("C dom7: {}", c_dom7.to_colored_string());
    println!("C min7: {}", c_min7.to_colored_string());
    println!("C dim: {}", c_dim.to_colored_string());
    println!("C aug: {}", c_aug.to_colored_string());
    println!("C maj9: {}", c_maj9.to_colored_string());
    println!("C dom11: {}", c_dom11.to_colored_string());
    println!("C dom13: {}", c_dom13.to_colored_string());
    println!("C dom b5: {}", c_dom_flat5.to_colored_string());
    println!("C dom #5: {}", c_dom_sharp5.to_colored_string());
    println!("C dom b9: {}", c_dom_flat9.to_colored_string());
    println!("C dom #11: {}", c_dom_sharp11.to_colored_string());
    println!("C dom alt: {}", c_dom_alt.to_colored_string());
    println!("C sus4: {}", c_sus4.to_colored_string());
    println!("C sus2: {}", c_sus2.to_colored_string());
    println!("C 7sus4: {}", c_7sus4.to_colored_string());
    println!("C/E: {}", c_maj_over_e.to_colored_string());
    println!("C Maj7/F: {}", c_maj7_over_f.to_colored_string());
    println!("C maj no3: {}", c_maj_no3.to_colored_string());
    println!("C maj no5: {}", c_maj_no5.to_colored_string());
    println!("C maj7 no3: {}", c_maj7_no3.to_colored_string());
    println!("C dom13 b5 #11/E: {}", c_complex.to_colored_string());
    
    // Test 6th chords
    let c_maj6 = Chord::maj6().with_root(Note::c());
    let c_min6 = Chord::min6().with_root(Note::c());
    let c_maj6_9 = Chord::maj6_9().with_root(Note::c());
    let c_min6_9 = Chord::min6_9().with_root(Note::c());
    println!("C maj6: {}", c_maj6.to_colored_string());
    println!("C min6: {}", c_min6.to_colored_string());
    println!("C maj6/9: {}", c_maj6_9.to_colored_string());
    println!("C min6/9: {}", c_min6_9.to_colored_string());
    
    // Test suspended chord with add6
    // Note: We can't do sus4() on a maj6 chord because it's musically invalid
    // Instead, we would need to add an add6() method to suspended chords
}
