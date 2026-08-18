//! Resolve display text to normalized parameter values.
//! Used to find Pro-C 3's internal normalized values for specific ms times.

use anyhow::Result;
use fts_analyzer::host::LoadedPlugin;

pub fn run_resolve(
    plugin_path: &str,
    param_id: u32,
    texts: &[String],
    sample_rate: f64,
    block_size: u32,
) -> Result<()> {
    let mut plugin = LoadedPlugin::load(plugin_path.as_ref(), 0, sample_rate, block_size)?;

    eprintln!("Resolving param {} values:\n", param_id);
    println!("text,value");

    for text in texts {
        match plugin.text_to_value(param_id, text) {
            Some(val) => {
                let back = plugin.value_to_text(param_id, val).unwrap_or_default();
                println!("{},{:.10},{}", text, val, back);
            }
            None => eprintln!("  '{}' → NOT RECOGNIZED", text),
        }
    }

    Ok(())
}
