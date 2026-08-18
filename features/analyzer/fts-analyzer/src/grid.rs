use crate::config::{BucketStrategy, MeasureConfig, ParameterBucket};
use crate::host::LoadedPlugin;

/// A single point in the parameter grid: one set of parameter values + one input gain.
#[derive(Debug, Clone)]
pub struct GridPoint {
    /// Index of this point in the full grid.
    pub index: usize,
    /// Total number of points in the grid.
    pub total: usize,
    /// Input gain in dB.
    pub input_gain_db: f32,
    /// Parameter name → value pairs for this point.
    pub params: Vec<(String, f64)>,
}

/// Resolved parameter: a parameter name paired with its CLAP parameter ID.
#[derive(Debug, Clone)]
pub struct ResolvedParam {
    pub name: String,
    pub id: u32,
    pub values: Vec<f64>,
}

/// Generate all grid points as the cartesian product of resolved parameter values
/// and input gain levels.
pub fn generate_grid(config: &MeasureConfig, resolved: &[ResolvedParam]) -> Vec<GridPoint> {
    let param_values: Vec<(String, Vec<f64>)> = resolved
        .iter()
        .map(|r| (r.name.clone(), r.values.clone()))
        .collect();

    let gain_levels = if config.input_gain_buckets_db.is_empty() {
        vec![0.0f32]
    } else {
        config.input_gain_buckets_db.clone()
    };

    // Build cartesian product of all parameter values
    let mut param_combos: Vec<Vec<(String, f64)>> = vec![vec![]];
    for (name, values) in &param_values {
        let mut new_combos = Vec::new();
        for combo in &param_combos {
            for &val in values {
                let mut extended = combo.clone();
                extended.push((name.clone(), val));
                new_combos.push(extended);
            }
        }
        param_combos = new_combos;
    }

    // Cross with gain levels
    let total = param_combos.len() * gain_levels.len();
    let mut points = Vec::with_capacity(total);
    let mut index = 0;

    for &gain_db in &gain_levels {
        for combo in &param_combos {
            points.push(GridPoint {
                index,
                total,
                input_gain_db: gain_db,
                params: combo.clone(),
            });
            index += 1;
        }
    }

    points
}

/// Resolve parameter names to CLAP parameter IDs using the plugin's parameter list.
/// For `DisplayText` buckets, uses the plugin's `text_to_value` to convert display
/// strings to native parameter values.
pub fn resolve_params(
    buckets: &[ParameterBucket],
    plugin_params: &[crate::host::ParamInfo],
    plugin: &mut LoadedPlugin,
) -> anyhow::Result<Vec<ResolvedParam>> {
    buckets
        .iter()
        .map(|b| {
            let info = plugin_params
                .iter()
                .find(|p| p.name == b.param_name)
                .ok_or_else(|| {
                    let available: Vec<&str> =
                        plugin_params.iter().map(|p| p.name.as_str()).collect();
                    anyhow::anyhow!(
                        "parameter '{}' not found in plugin. Available: {:?}",
                        b.param_name,
                        available
                    )
                })?;

            let values = if b.strategy == BucketStrategy::DisplayText {
                b.text_values
                    .iter()
                    .map(|text| {
                        plugin.text_to_value(info.id, text).ok_or_else(|| {
                            anyhow::anyhow!(
                                "plugin could not parse '{}' for parameter '{}'",
                                text,
                                b.param_name
                            )
                        })
                    })
                    .collect::<anyhow::Result<Vec<f64>>>()?
            } else {
                b.generate_values()
            };

            Ok(ResolvedParam {
                name: b.param_name.clone(),
                id: info.id,
                values,
            })
        })
        .collect()
}
