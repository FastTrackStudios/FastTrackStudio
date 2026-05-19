//! Convert a Pro Tools session to a REAPER project file.
//!
//! Usage: `cargo run -p daw-reaper --example pt_to_rpp -- <input.ptx> [output.rpp]`

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut args = std::env::args().skip(1);
    let input = args
        .next()
        .ok_or("usage: pt_to_rpp <input.ptx> [output.rpp]")?;
    let output = args.next().unwrap_or_else(|| {
        let stem = std::path::Path::new(&input)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("session");
        format!("{stem}.rpp")
    });

    let rpp = daw_reaper::project_import::protools_to_rpp(&input)?;
    std::fs::write(&output, &rpp)?;
    eprintln!("wrote {} ({} bytes)", output, rpp.len());
    Ok(())
}
