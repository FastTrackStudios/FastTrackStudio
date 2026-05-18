fn main() -> Result<(), Box<dyn std::error::Error>> {
    let s = dawfile_protools::read_session(std::env::args().nth(1).unwrap().as_str(), 48000)?;
    println!("=== {} edit groups ===", s.edit_groups.len());
    for (i, g) in s.edit_groups.iter().enumerate() {
        println!("  [{i:>3}] color={:>5?} name={:?}", g.color, g.name);
    }
    println!("=== {} stem mappings ===", s.stem_mappings.len());
    for (i, name) in s.stem_mappings.iter().enumerate() {
        println!("  [{i:>3}] {:?}", name);
    }
    Ok(())
}
