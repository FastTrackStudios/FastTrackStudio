use keyflow::chart::Chart;
use keyflow::sections::SectionType;

/// Test 1: Basic chord memory with explicit definitions
/// Tests that:
/// - Chords with explicit quality are remembered in both global and section memory
/// - Just root notes recall from memory hierarchy: section → global → key inference
/// - Template recall works for repeated sections
#[test]
fn test_chord_memory_explicit_definitions() {
    let input = r#"Chord Memory Test - Demo
120bpm 4/4 #G

intro 2
g c
vs 4
Gmaj13 C9 Gmaj13 C9
ch 4
e d Gmaj6 C5
vs
br 2
G C
"#;

    let chart = Chart::parse(input).unwrap();
    
    // Test metadata
    assert_eq!(chart.metadata.title, Some("Chord Memory Test".to_string()));
    assert_eq!(chart.metadata.artist, Some("Demo".to_string()));
    
    // Test sections
    assert_eq!(chart.sections.len(), 5);
    
    // Test Intro section - should infer from key (G major: I=G, IV=C)
    let intro_section = &chart.sections[0];
    assert_eq!(intro_section.section.section_type, SectionType::Intro);
    assert_eq!(intro_section.measures.len(), 2);
    
    // Intro: g c = G C (inferred from G major scale, triads)
    let intro_chord1 = &intro_section.measures[0].chords[0];
    assert_eq!(format!("{}", intro_chord1.root), "G");
    assert_eq!(intro_chord1.full_symbol, "G");
    
    let intro_chord2 = &intro_section.measures[1].chords[0];
    assert_eq!(format!("{}", intro_chord2.root), "C");
    assert_eq!(intro_chord2.full_symbol, "C");
    
    // Test Verse 1 section - explicit qualities update global memory
    let verse1_section = &chart.sections[1];
    assert_eq!(verse1_section.section.section_type, SectionType::Verse);
    assert_eq!(verse1_section.measures.len(), 4);
    
    // Verse 1: Gmaj13 C9 Gmaj13 C9 (explicit definitions)
    let verse1_chord1 = &verse1_section.measures[0].chords[0];
    assert_eq!(format!("{}", verse1_chord1.root), "G");
    assert_eq!(verse1_chord1.full_symbol, "Gmaj13");
    
    let verse1_chord2 = &verse1_section.measures[1].chords[0];
    assert_eq!(format!("{}", verse1_chord2.root), "C");
    assert_eq!(verse1_chord2.full_symbol, "C9");
    
    let verse1_chord3 = &verse1_section.measures[2].chords[0];
    assert_eq!(format!("{}", verse1_chord3.root), "G");
    assert_eq!(verse1_chord3.full_symbol, "Gmaj13");
    
    let verse1_chord4 = &verse1_section.measures[3].chords[0];
    assert_eq!(format!("{}", verse1_chord4.root), "C");
    assert_eq!(verse1_chord4.full_symbol, "C9");
    
    // Test Chorus section - clears section memory, infers e/d from key, then sets Gmaj6/C5
    let chorus_section = &chart.sections[2];
    assert_eq!(chorus_section.section.section_type, SectionType::Chorus);
    assert_eq!(chorus_section.measures.len(), 4);
    
    // Chorus: e d Gmaj6 C5
    // e and d should infer from key: vi=Em, V=D
    // Gmaj6 and C5 are explicit and update global memory
    let chorus_chord1 = &chorus_section.measures[0].chords[0];
    assert_eq!(format!("{}", chorus_chord1.root), "E");
    assert_eq!(chorus_chord1.full_symbol, "Em");
    
    let chorus_chord2 = &chorus_section.measures[1].chords[0];
    assert_eq!(format!("{}", chorus_chord2.root), "D");
    assert_eq!(chorus_chord2.full_symbol, "D");
    
    let chorus_chord3 = &chorus_section.measures[2].chords[0];
    assert_eq!(format!("{}", chorus_chord3.root), "G");
    assert_eq!(chorus_chord3.full_symbol, "Gmaj6");
    
    let chorus_chord4 = &chorus_section.measures[3].chords[0];
    assert_eq!(format!("{}", chorus_chord4.root), "C");
    assert_eq!(chorus_chord4.full_symbol, "C5");
    
    // Test Verse 2 section - uses template recall from Verse 1
    let verse2_section = &chart.sections[3];
    assert_eq!(verse2_section.section.section_type, SectionType::Verse);
    assert_eq!(verse2_section.measures.len(), 4);
    
    // Verse 2: template recall from Verse 1 (Gmaj13, C9, Gmaj13, C9)
    let verse2_chord1 = &verse2_section.measures[0].chords[0];
    assert_eq!(format!("{}", verse2_chord1.root), "G");
    assert_eq!(verse2_chord1.full_symbol, "Gmaj13");
    
    let verse2_chord2 = &verse2_section.measures[1].chords[0];
    assert_eq!(format!("{}", verse2_chord2.root), "C");
    assert_eq!(verse2_chord2.full_symbol, "C9");
    
    // Test Bridge section - uses global memory (Gmaj6, C5 from Chorus)
    let bridge_section = &chart.sections[4];
    assert_eq!(bridge_section.section.section_type, SectionType::Bridge);
    assert_eq!(bridge_section.measures.len(), 2);
    
    // Bridge: G C (uses global memory from Chorus: Gmaj6, C5)
    let bridge_chord1 = &bridge_section.measures[0].chords[0];
    assert_eq!(format!("{}", bridge_chord1.root), "G");
    assert_eq!(bridge_chord1.full_symbol, "Gmaj6");
    
    let bridge_chord2 = &bridge_section.measures[1].chords[0];
    assert_eq!(format!("{}", bridge_chord2.root), "C");
    assert_eq!(bridge_chord2.full_symbol, "C5");
}

/// Test 2: One-time overrides with ! prefix
/// Tests that:
/// - !chord uses the quality but doesn't update memory
/// - Subsequent chords recall the original memory
#[test]
fn test_chord_memory_one_time_overrides() {
    let input = r#"Chord Override Test - Demo
120bpm 4/4 #G

intro 2
Gmaj7 Gmaj7
vs 2
!G7 G
"#;

    let chart = Chart::parse(input).unwrap();
    
    // Test metadata
    assert_eq!(chart.metadata.title, Some("Chord Override Test".to_string()));
    assert_eq!(chart.metadata.artist, Some("Demo".to_string()));
    
    // Test sections
    assert_eq!(chart.sections.len(), 2);
    
    // Test Intro section - sets Gmaj7 in memory
    let intro_section = &chart.sections[0];
    assert_eq!(intro_section.section.section_type, SectionType::Intro);
    assert_eq!(intro_section.measures.len(), 2);
    
    let intro_chord1 = &intro_section.measures[0].chords[0];
    assert_eq!(intro_chord1.full_symbol, "Gmaj7");
    
    // Test Verse section - first chord is override, second recalls original
    let verse_section = &chart.sections[1];
    assert_eq!(verse_section.section.section_type, SectionType::Verse);
    assert_eq!(verse_section.measures.len(), 2);
    
    // First chord: !G7 - uses G7 but doesn't update memory
    let verse_chord1 = &verse_section.measures[0].chords[0];
    assert_eq!(verse_chord1.full_symbol, "G7");
    
    // Second chord: G - recalls original Gmaj7 from global memory
    let verse_chord2 = &verse_section.measures[1].chords[0];
    assert_eq!(verse_chord2.full_symbol, "Gmaj7");
}

/// Test 3: Section-specific memory priority
/// Tests that:
/// - Each section type has its own memory
/// - Section memory takes priority over global memory
/// - Global memory is copied to section memory when accessed
#[test]
fn test_chord_memory_section_priority() {
    let input = r#"Section Priority Test - Demo
120bpm 4/4 #G

intro 2
Gmaj7 Cmaj7
vs 2
Gmaj13 C9
ch 2
Gmaj6 C5
vs
br 2
G C
"#;

    let chart = Chart::parse(input).unwrap();
    
    // Test sections
    assert_eq!(chart.sections.len(), 5);
    
    // Intro: Gmaj7 Cmaj7 (sets global memory)
    let intro_section = &chart.sections[0];
    let intro_chord1 = &intro_section.measures[0].chords[0];
    assert_eq!(intro_chord1.full_symbol, "Gmaj7");
    
    // Verse 1: Gmaj13 C9 (updates global memory)
    let verse1_section = &chart.sections[1];
    let verse1_chord1 = &verse1_section.measures[0].chords[0];
    assert_eq!(verse1_chord1.full_symbol, "Gmaj13");
    
    // Chorus: Gmaj6 C5 (updates global memory again)
    let chorus_section = &chart.sections[2];
    let chorus_chord1 = &chorus_section.measures[0].chords[0];
    assert_eq!(chorus_chord1.full_symbol, "Gmaj6");
    
    // Verse 2: uses template from Verse 1 (Gmaj13, C9)
    let verse2_section = &chart.sections[3];
    let verse2_chord1 = &verse2_section.measures[0].chords[0];
    assert_eq!(verse2_chord1.full_symbol, "Gmaj13");
    
    // Bridge: G C uses global memory (Gmaj6, C5 from Chorus)
    let bridge_section = &chart.sections[4];
    let bridge_chord1 = &bridge_section.measures[0].chords[0];
    assert_eq!(bridge_chord1.full_symbol, "Gmaj6");
}

/// Test 4: Complex memory scenario with multiple updates
/// Tests various combinations of explicit chords, recalls, and key inference
#[test]
fn test_chord_memory_complex_scenario() {
    let input = r#"Complex Memory Test - Demo
120bpm 4/4 #G

intro 2
g c
vs 2
Gmaj13 C9
ch 2
e d
vs
br 2
G C
outro 2
!Gmaj7 !Cmaj7
"#;

    let chart = Chart::parse(input).unwrap();
    
    assert_eq!(chart.sections.len(), 6);
    
    // Intro: g c infers from key = G, C (triads)
    let intro_section = &chart.sections[0];
    let intro_chord1 = &intro_section.measures[0].chords[0];
    assert_eq!(intro_chord1.full_symbol, "G");
    let intro_chord2 = &intro_section.measures[1].chords[0];
    assert_eq!(intro_chord2.full_symbol, "C");
    
    // Verse 1: Gmaj13 C9 (updates global memory)
    let verse1_section = &chart.sections[1];
    let verse1_chord1 = &verse1_section.measures[0].chords[0];
    assert_eq!(verse1_chord1.full_symbol, "Gmaj13");
    
    // Chorus: e d infers from key = Em, D
    let chorus_section = &chart.sections[2];
    let chorus_chord1 = &chorus_section.measures[0].chords[0];
    assert_eq!(chorus_chord1.full_symbol, "Em");
    let chorus_chord2 = &chorus_section.measures[1].chords[0];
    assert_eq!(chorus_chord2.full_symbol, "D");
    
    // Verse 2: uses template from Verse 1 (Gmaj13, C9)
    let verse2_section = &chart.sections[3];
    let verse2_chord1 = &verse2_section.measures[0].chords[0];
    assert_eq!(verse2_chord1.full_symbol, "Gmaj13");
    
    // Bridge: G C uses global memory (Gmaj13, C9 from Verse)
    let bridge_section = &chart.sections[4];
    let bridge_chord1 = &bridge_section.measures[0].chords[0];
    assert_eq!(bridge_chord1.full_symbol, "Gmaj13");
    let bridge_chord2 = &bridge_section.measures[1].chords[0];
    assert_eq!(bridge_chord2.full_symbol, "C9");
    
    // Outro: !Gmaj7 !Cmaj7 (one-time overrides, don't change memory)
    let outro_section = &chart.sections[5];
    let outro_chord1 = &outro_section.measures[0].chords[0];
    assert_eq!(outro_chord1.full_symbol, "Gmaj7");
    let outro_chord2 = &outro_section.measures[1].chords[0];
    assert_eq!(outro_chord2.full_symbol, "Cmaj7");
}

/// Test 5: Template recall for repeated sections
/// Tests that:
/// - Sections without content recall templates from previous definitions
/// - Templates preserve chord progressions
#[test]
fn test_template_recall() {
    let input = r#"Template Test - Demo
120bpm 4/4 #G

vs 4
Gmaj13 C9 Em7 D7
vs
"#;

    let chart = Chart::parse(input).unwrap();
    
    assert_eq!(chart.sections.len(), 2);
    
    // Verse 1: sets the template
    let verse1_section = &chart.sections[0];
    assert_eq!(verse1_section.measures.len(), 4);
    
    // Verse 2: recalls template from Verse 1
    let verse2_section = &chart.sections[1];
    assert_eq!(verse2_section.measures.len(), 4);
    
    // Verify template was recalled
    let verse2_chord1 = &verse2_section.measures[0].chords[0];
    assert_eq!(verse2_chord1.full_symbol, "Gmaj13");
    
    let verse2_chord2 = &verse2_section.measures[1].chords[0];
    assert_eq!(verse2_chord2.full_symbol, "C9");
}

/// Test 6: Global memory with explicit redefinition
/// Tests that:
/// - When a section is explicitly redefined with just roots, it uses global memory
/// - Global memory is copied to the section's memory
#[test]
fn test_global_memory_with_explicit_redefinition() {
    let input = r#"Global Memory Test - Demo
120bpm 4/4 #G

vs
g
ch
Gmaj6
vs
g
"#;

    let chart = Chart::parse(input).unwrap();
    
    assert_eq!(chart.sections.len(), 3);
    
    // Verse 1: g infers from key = G
    let verse1_section = &chart.sections[0];
    let verse1_chord1 = &verse1_section.measures[0].chords[0];
    assert_eq!(verse1_chord1.full_symbol, "G");
    
    // Chorus: Gmaj6 updates global memory
    let chorus_section = &chart.sections[1];
    let chorus_chord1 = &chorus_section.measures[0].chords[0];
    assert_eq!(chorus_chord1.full_symbol, "Gmaj6");
    
    // Verse 2: g explicitly redefined, uses global memory = Gmaj6
    let verse2_section = &chart.sections[2];
    let verse2_chord1 = &verse2_section.measures[0].chords[0];
    assert_eq!(verse2_chord1.full_symbol, "Gmaj6");
}

/// Test 7: Template length and inline chord notation
/// Tests that:
/// - Sections can have inline chords after measure count
/// - Templates preserve the correct length
#[test]
fn test_template_length_and_inline_chords() {
    let input = r#"Template Length Test - Demo
120bpm 4/4 #G

vs 4
Gmaj7 Cmaj7 Dmin7 G7
vs
"#;

    let chart = Chart::parse(input).unwrap();
    
    assert_eq!(chart.sections.len(), 2);
    
    // Verse 1: 4 measures
    let verse1_section = &chart.sections[0];
    assert_eq!(verse1_section.measures.len(), 4);
    
    // Verse 2: recalls template with same length (4 measures)
    let verse2_section = &chart.sections[1];
    assert_eq!(verse2_section.measures.len(), 4);
    
    // Verify chords were recalled correctly
    let verse2_chord1 = &verse2_section.measures[0].chords[0];
    assert_eq!(verse2_chord1.full_symbol, "Gmaj7");
}

/// Test 8: Key-based chord quality inference
/// Tests that:
/// - Chords without explicit quality infer from the current key
/// - Inference works for note names, scale degrees, and Roman numerals
#[test]
fn test_key_based_quality_inference() {
    let input = r#"Key Inference Test - Demo
120bpm 4/4 #G

intro 4
g c e d
vs 4
1 4 6 5
ch 4
I IV vi V
"#;

    let chart = Chart::parse(input).unwrap();
    
    assert_eq!(chart.sections.len(), 3);
    
    // Intro: note names (lowercase) infer from G major
    let intro_section = &chart.sections[0];
    assert_eq!(intro_section.measures[0].chords[0].full_symbol, "G");    // I = major
    assert_eq!(intro_section.measures[1].chords[0].full_symbol, "C");    // IV = major
    assert_eq!(intro_section.measures[2].chords[0].full_symbol, "Em");   // vi = minor
    assert_eq!(intro_section.measures[3].chords[0].full_symbol, "D");    // V = major
    
    // Verse: scale degrees infer from G major
    let verse_section = &chart.sections[1];
    assert_eq!(verse_section.measures[0].chords[0].full_symbol, "1");    // 1 (quality implied by key)
    assert_eq!(verse_section.measures[1].chords[0].full_symbol, "4");    // 4 (quality implied by key)
    assert_eq!(verse_section.measures[2].chords[0].full_symbol, "6");    // 6 (quality implied by key)
    assert_eq!(verse_section.measures[3].chords[0].full_symbol, "5");    // 5 (quality implied by key)
    
    // Chorus: Roman numerals infer from G major
    let chorus_section = &chart.sections[2];
    assert_eq!(chorus_section.measures[0].chords[0].full_symbol, "I");   // I (quality implied by key)
    assert_eq!(chorus_section.measures[1].chords[0].full_symbol, "IV");  // IV (quality implied by key)
    assert_eq!(chorus_section.measures[2].chords[0].full_symbol, "vi");  // vi (quality implied by key)
    assert_eq!(chorus_section.measures[3].chords[0].full_symbol, "V");   // V (quality implied by key)
}
